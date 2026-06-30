# =============================================================================
# 1_clean_findex.R
# Step 3 — Full cleaning pipeline for the harmonised Findex UEMOA data.
#
# Operations (in order):
#   1. Deduplication
#   2. Recode Gallup binary vars (1=yes→1 / 2=no→0 / 3=dk→NA / 4=ref→NA)
#   3. Recode categorical variables (sexe, education, quintile, actif, fin25)
#   4. Recompute epargne_totale after proper recoding
#   5. Drop useless columns (milieu: 100% NA in Findex 2017)
#   6. Whitespace normalisation
#   7. Numeric bounds (age, wgt)
#   8. Cross-variable consistency checks + flagging
#   9. NA imputation
#
# To adapt for a new survey edition or new countries:
#   → modify only the CLEANING_PARAMS list below.
#
# Reads:  data/intermediates/findex_renamed.csv
# Writes: data/intermediates/findex_clean.csv
# =============================================================================

rm(list = ls()); gc()

pacman::p_load(dplyr, readr, stringr, forcats, data.table, glue, purrr, tidyr, here)

source(here("config.R"))
source(here("utils.R"))

# =============================================================================
# CLEANING PARAMETERS — adapt this section for a new survey/edition
# =============================================================================

# ── List of Gallup-coded binary variables (1=yes, 2=no, 3=dk, 4=ref) ──────
GALLUP_BINARY_VARS <- c(
  # Access & usage of accounts
  "fin2_carte_debit", "fin3_carte_nom_propre", "fin4_carte_utilisee_12m",
  "fin5_mobile_acces_compte", "fin6_mobile_solde",
  "fin7_carte_credit", "fin8_carte_credit_utilisee",
  "fin9_depot_12m", "fin10_retrait_12m",
  # Barriers to account ownership
  "fin11a_trop_loin", "fin11b_trop_cher", "fin11c_docs_manquants",
  "fin11d_manque_confiance", "fin11e_religion", "fin11f_manque_argent",
  "fin11g_famille_a_compte", "fin11h_pas_besoin",
  # Digital payments
  "fin14a_paiement_enligne", "fin14b_achat_enligne",
  # Savings instruments
  "fin15_epargne_agri_biz", "fin16_epargne_retraite",
  "fin17a_epargne_inst_fin", "fin17b_epargne_tontine",
  # Credit
  "fin19_pret_immo", "fin20_emprunt_sante", "fin21_emprunt_agri_biz",
  "fin22a_emprunt_inst_fin", "fin22b_emprunt_famille", "fin22c_emprunt_tontine",
  # Emergency resilience
  "fin24_fonds_urgence",
  # Domestic remittances (sent)
  "fin26_envoi_transfert", "fin27a_envoi_via_fi", "fin27b_envoi_via_mobile",
  "fin27c1_envoi_especes", "fin27c2_envoi_mto",
  # Domestic remittances (received)
  "fin28_recep_transfert", "fin29a_recep_via_fi", "fin29b_recep_via_mobile",
  "fin29c1_recep_especes", "fin29c2_recep_mto",
  # Utility bill payments
  "fin30_paie_factures", "fin31a_factures_compte", "fin31b_factures_mobile",
  "fin31c_factures_especes",
  # Wage payments
  "fin32_salaire_12m", "fin33_secteur_public",
  "fin34a_salaire_compte", "fin34b_salaire_mobile",
  "fin34c1_salaire_especes", "fin34c2_salaire_carte",
  # Government transfers
  "fin37_transferts_etat", "fin38_pension_etat",
  "fin39a_etat_compte", "fin39b_etat_mobile",
  "fin39c1_etat_especes", "fin39c2_etat_carte",
  # Agricultural payments
  "fin42_paie_agri", "fin43a_agri_compte", "fin43b_agri_mobile",
  "fin43c1_agri_especes", "fin43c2_agri_carte",
  # Self-employment payments
  "fin46_paie_auto_emploi", "fin47a_autoemploi_compte", "fin47b_autoemploi_mobile",
  "fin47c1_autoemploi_especes", "fin47c2_autoemploi_carte",
  # Other
  "fin48_id_nat", "mobileowner"
)

# ── Savings variables used to recompute epargne_totale ─────────────────────
# (after Gallup recoding — all become 0/1)
SAVINGS_VARS <- c(
  "fin15_epargne_agri_biz", "fin16_epargne_retraite",
  "fin17a_epargne_inst_fin", "fin17b_epargne_tontine"
)

# ── Categorical recoding maps ────────────────────────────────────────────────
# Format: named vector where names = original codes (as character), values = new labels
CATEG_RECODE_MAP <- list(

  # Gallup: 1 = Male, 2 = Female
  sexe = c("1" = "Masculin", "2" = "Feminin"),

  # Gallup: 1 = primary or less, 2 = secondary, 3 = tertiary, 4 = dk, 5 = ref
  education = c(
    "1" = "Primaire ou moins",
    "2" = "Secondaire",
    "3" = "Superieur",
    "4" = NA_character_,    # don't know
    "5" = NA_character_     # refused
  ),

  # Gallup: 1 = Poorest 20%, 2 = Second 20%, 3 = Middle 20%, 4 = Fourth 20%, 5 = Richest 20%
  quintile_revenu = c(
    "1" = "20% les plus pauvres",
    "2" = "2e quintile",
    "3" = "3e quintile (median)",
    "4" = "4e quintile",
    "5" = "20% les plus riches"
  ),

  # Gallup: 1 = in workforce, 0 = out of workforce (already binary from colleague)
  # kept for clarity in case some files differ
  actif = c("1" = "Actif", "0" = "Inactif"),

  # fin25: source of emergency funds
  fin25_source_urgence = c(
    "1" = "Salaire ou travail",
    "2" = "Famille ou amis",
    "3" = "Epargne",
    "4" = "Vente d actifs",
    "5" = "Emprunt (banque ou employeur)",
    "6" = "Autre source",
    "7" = "Ne sait pas",
    "8" = "Refuse"
  )
)

# ── Main CLEANING_PARAMS ─────────────────────────────────────────────────────
CLEANING_PARAMS <- list(

  # Primary key for deduplication
  id_vars = c("code_pays", "wpid_random"),

  # Numeric domain bounds: values outside are set to NA
  numeric_bounds = list(
    age = c(15, 99),    # Findex targets adults 15+; 99 = plausible upper bound
    wgt = c(0.01, Inf)  # Sampling weight must be strictly positive
  ),

  # Numeric imputation after bounding
  numeric_impute = list(
    age = "mean"   # 56 NAs — impute by survey mean (acceptable for age)
  ),

  # Categorical imputation (none by default for Findex: structural NAs from filter questions)
  categ_impute = list(),

  # Columns to drop (milieu = 100% NA — not available in Findex 2017 public microdata)
  # NOTE: The Gallup World Poll records rural/urban but the Findex microdata release
  #       does not include it. This is a confirmed limitation of the dataset.
  vars_to_drop = c("milieu"),

  # Cross-variable consistency rules
  # Each rule evaluated on the cleaned df; action = "na" or "flag"
  consistency_rules = list(

    # 1. Having a debit card requires having an account
    list(
      label     = "Carte debit sans compte",
      condition = "!is.na(fin2_carte_debit) & fin2_carte_debit == 1 & account == 0",
      target    = "fin2_carte_debit",
      action    = "flag"      # flag only — may reflect joint accounts
    ),

    # 2. Using a debit card requires having one
    list(
      label     = "Utilisation carte debit sans carte debit",
      condition = "!is.na(fin4_carte_utilisee_12m) & fin4_carte_utilisee_12m == 1 &
                   !is.na(fin2_carte_debit) & fin2_carte_debit == 0",
      target    = "fin4_carte_utilisee_12m",
      action    = "na"
    ),

    # 3. Using credit card requires having one
    list(
      label     = "Utilisation carte credit sans carte credit",
      condition = "!is.na(fin8_carte_credit_utilisee) & fin8_carte_credit_utilisee == 1 &
                   !is.na(fin7_carte_credit) & fin7_carte_credit == 0",
      target    = "fin8_carte_credit_utilisee",
      action    = "na"
    ),

    # 4. Mobile internet access requires a mobile phone
    list(
      label     = "Acces mobile sans telephone",
      condition = "!is.na(fin5_mobile_acces_compte) & fin5_mobile_acces_compte == 1 &
                   !is.na(mobileowner) & mobileowner == 0",
      target    = "fin5_mobile_acces_compte",
      action    = "flag"
    ),

    # 5. Mobile transfer requires mobile phone
    list(
      label     = "Transfert mobile sans telephone",
      condition = "!is.na(fin27b_envoi_via_mobile) & fin27b_envoi_via_mobile == 1 &
                   !is.na(mobileowner) & mobileowner == 0",
      target    = "fin27b_envoi_via_mobile",
      action    = "flag"
    ),

    # 6. Account deposit/withdrawal requires an account
    list(
      label     = "Depot sans compte",
      condition = "!is.na(fin9_depot_12m) & fin9_depot_12m == 1 & account == 0",
      target    = "fin9_depot_12m",
      action    = "flag"
    ),
    list(
      label     = "Retrait sans compte",
      condition = "!is.na(fin10_retrait_12m) & fin10_retrait_12m == 1 & account == 0",
      target    = "fin10_retrait_12m",
      action    = "flag"
    ),

    # 7. saved=1 but no savings instrument declared (fin15, 16, 17a, 17b all 0)
    list(
      label     = "Epargnant sans instrument epargne identifie",
      condition = "saved == 1 &
                   (is.na(fin15_epargne_agri_biz)  | fin15_epargne_agri_biz  == 0) &
                   (is.na(fin16_epargne_retraite)   | fin16_epargne_retraite   == 0) &
                   (is.na(fin17a_epargne_inst_fin)  | fin17a_epargne_inst_fin  == 0) &
                   (is.na(fin17b_epargne_tontine)   | fin17b_epargne_tontine   == 0)",
      target    = "saved",
      action    = "flag"   # flag only: may have saved via other channels not captured
    ),

    # 8. Age > 95 (improbable but not impossible)
    list(
      label     = "Age superieur a 95 ans",
      condition = "!is.na(age) & age > 95",
      target    = "age",
      action    = "flag"
    )
  )
)

# =============================================================================
# PIPELINE EXECUTION
# =============================================================================

message(strrep("=", 60))
message("FINDEX CLEANING PIPELINE")
message(strrep("=", 60))

# ── 1. Load data ──────────────────────────────────────────────────────────────
message("\n[1/7] Loading data...")
df <- read_csv(
  file.path(FINDEX_INTERMEDIATE_PATH, "findex_renamed.csv"),
  show_col_types = FALSE
)
message(glue("  Input: {nrow(df)} rows x {ncol(df)} columns ({n_distinct(df$code_pays)} countries)"))

# ── 2. Recode Gallup binary variables ─────────────────────────────────────────
message("\n[2/7] Recoding Gallup binary variables (1→1 / 2→0 / 3,4→NA)...")
df <- recode_gallup_binary(df, GALLUP_BINARY_VARS)

# ── 3. Recode categorical variables ──────────────────────────────────────────
message("\n[3/7] Recoding categorical variables...")
df <- recode_gallup_categ(df, CATEG_RECODE_MAP)

# ── 4. Recompute epargne_totale ───────────────────────────────────────────────
message("\n[4/7] Recomputing epargne_totale from savings instruments...")
df <- compute_epargne_totale(df, SAVINGS_VARS)

# ── 5-9. Standard cleaning pipeline (dedup, drop, normalise, bounds, rules, impute)
message("\n[5-9/7] Standard cleaning pipeline...")
df_clean <- run_cleaning_pipeline(
  df       = df,
  params   = CLEANING_PARAMS,
  key_cols = CLEANING_PARAMS$id_vars,
  label    = "Findex UEMOA"
)

# ── Report missing values summary ─────────────────────────────────────────────
message("\n── Missing values summary (post-cleaning) ──")
na_summary <- df_clean %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  filter(pct_na > 0) %>%
  arrange(desc(pct_na))

if (nrow(na_summary) > 0) {
  cat(sprintf("  %-45s %8s\n", "Variable", "% NA"))
  cat(sprintf("  %s\n", strrep("-", 55)))
  for (i in seq_len(min(20, nrow(na_summary)))) {
    cat(sprintf("  %-45s %7.1f%%\n",
                na_summary$variable[i], na_summary$pct_na[i]))
  }
  if (nrow(na_summary) > 20)
    message(glue("  ... and {nrow(na_summary) - 20} more variables with missing values"))
} else {
  message("  No missing values.")
}

# ── Save intermediate ──────────────────────────────────────────────────────────
message("\n── Saving cleaned intermediate...")
fwrite(df_clean, file.path(FINDEX_INTERMEDIATE_PATH, "findex_clean.csv"))
message(glue("  Saved: data/intermediates/findex_clean.csv"))
message(glue("  Final: {nrow(df_clean)} rows x {ncol(df_clean)} columns"))
message(glue("  Countries: {paste(sort(unique(df_clean$code_pays)), collapse=', ')}"))
