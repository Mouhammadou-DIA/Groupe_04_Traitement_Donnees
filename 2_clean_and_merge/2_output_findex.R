# =============================================================================
# 2_output_findex.R
# Step 4 — Produce the final consolidated output table.
#
# Operations:
#   1. Load cleaned data
#   2. Enforce final column ordering (id | socio-demo | financial | savings | flags)
#   3. Validate: row count, country coverage, no duplicate IDs
#   4. Save final output
#
# Reads:  data/intermediates/findex_clean.csv
# Writes: data/output/findex_uemoa_clean.csv
# =============================================================================

rm(list = ls()); gc()

pacman::p_load(dplyr, readr, data.table, glue, here)

source(here("config.R"))

# ── Load cleaned data ────────────────────────────────────────────────────────
message(strrep("=", 60))
message("FINDEX OUTPUT — Final consolidation")
message(strrep("=", 60))

df <- read_csv(
  file.path(FINDEX_INTERMEDIATE_PATH, "findex_clean.csv"),
  show_col_types = FALSE
)

# ── Final column ordering ────────────────────────────────────────────────────
# 1. Identifiers & design variables
id_cols <- c("code_pays", "pays", "annee_enquete", "wpid_random", "wgt")

# 2. Socio-demographic
demo_cols <- c("sexe", "age", "education", "quintile_revenu", "actif")

# 3. Account access & digital
account_cols <- c(
  "account", "account_fin", "account_mob",
  "mobileowner", "fin48_id_nat",
  "fin2_carte_debit", "fin3_carte_nom_propre", "fin4_carte_utilisee_12m",
  "fin5_mobile_acces_compte", "fin6_mobile_solde",
  "fin7_carte_credit", "fin8_carte_credit_utilisee",
  "fin9_depot_12m", "fin10_retrait_12m",
  "fin14a_paiement_enligne", "fin14b_achat_enligne"
)

# 4. Barriers to account
barrier_cols <- paste0("fin11", c("a","b","c","d","e","f","g","h"), "_",
                       c("trop_loin","trop_cher","docs_manquants","manque_confiance",
                         "religion","manque_argent","famille_a_compte","pas_besoin"))

# 5. Savings (all savings variables + total)
savings_cols <- c(
  "saved",
  "fin15_epargne_agri_biz", "fin16_epargne_retraite",
  "fin17a_epargne_inst_fin", "fin17b_epargne_tontine",
  "epargne_totale"
)

# 6. Credit & resilience
credit_cols <- c(
  "borrowed",
  "fin19_pret_immo", "fin20_emprunt_sante", "fin21_emprunt_agri_biz",
  "fin22a_emprunt_inst_fin", "fin22b_emprunt_famille", "fin22c_emprunt_tontine",
  "fin24_fonds_urgence", "fin25_source_urgence"
)

# 7. Payments & transfers
payment_cols <- c(
  "fin26_envoi_transfert","fin27a_envoi_via_fi","fin27b_envoi_via_mobile",
  "fin27c1_envoi_especes","fin27c2_envoi_mto",
  "fin28_recep_transfert","fin29a_recep_via_fi","fin29b_recep_via_mobile",
  "fin29c1_recep_especes","fin29c2_recep_mto",
  "fin30_paie_factures","fin31a_factures_compte","fin31b_factures_mobile","fin31c_factures_especes",
  "fin32_salaire_12m","fin33_secteur_public",
  "fin34a_salaire_compte","fin34b_salaire_mobile","fin34c1_salaire_especes","fin34c2_salaire_carte",
  "fin37_transferts_etat","fin38_pension_etat",
  "fin39a_etat_compte","fin39b_etat_mobile","fin39c1_etat_especes","fin39c2_etat_carte",
  "fin42_paie_agri","fin43a_agri_compte","fin43b_agri_mobile","fin43c1_agri_especes","fin43c2_agri_carte",
  "fin46_paie_auto_emploi","fin47a_autoemploi_compte","fin47b_autoemploi_mobile",
  "fin47c1_autoemploi_especes","fin47c2_autoemploi_carte",
  "receive_wages","receive_transfers","receive_pension","receive_agriculture",
  "pay_utilities","remittances","pay_onlne","pay_cash"
)

# 8. Flag columns (created by consistency rules)
flag_cols <- sort(names(df)[startsWith(names(df), "flag_")])

# Build final ordered column list (intersect = safe if some vars absent)
final_col_order <- c(
  intersect(id_cols,      names(df)),
  intersect(demo_cols,    names(df)),
  intersect(account_cols, names(df)),
  intersect(barrier_cols, names(df)),
  intersect(savings_cols, names(df)),
  intersect(credit_cols,  names(df)),
  intersect(payment_cols, names(df)),
  intersect(flag_cols,    names(df))
)

# Add any remaining columns not yet ordered
remaining <- setdiff(names(df), final_col_order)
if (length(remaining) > 0) {
  message(glue("  [output] {length(remaining)} unclassified column(s) appended: ",
               "{paste(remaining, collapse=', ')}"))
  final_col_order <- c(final_col_order, remaining)
}

df_out <- df %>% select(all_of(final_col_order))

# ── Validation checks ────────────────────────────────────────────────────────
message("\n── Validation checks ──")

# Check expected countries
expected_countries <- c("BEN", "BFA", "CIV", "MLI", "NER", "SEN", "TGO")
present_countries  <- sort(unique(df_out$code_pays))
missing_countries  <- setdiff(expected_countries, present_countries)
extra_countries    <- setdiff(present_countries, expected_countries)

message(glue("  Countries present ({length(present_countries)}): {paste(present_countries, collapse=', ')}"))
if (length(missing_countries) > 0)
  message(glue("  ⚠ Missing: {paste(missing_countries, collapse=', ')}"))
if (length(extra_countries) > 0)
  message(glue("  ⚠ Extra:   {paste(extra_countries,   collapse=', ')}"))

# Check expected row count (1000 per country)
row_check <- df_out %>%
  count(code_pays) %>%
  mutate(expected = 1000, ok = (n == expected))
for (i in seq_len(nrow(row_check))) {
  status <- if (row_check$ok[i]) "OK" else "⚠"
  message(glue("  {status} {row_check$code_pays[i]}: {row_check$n[i]} rows"))
}

# Check no duplicate IDs
n_dup <- sum(duplicated(paste(df_out$code_pays, df_out$wpid_random)))
if (n_dup > 0) {
  message(glue("  ⚠ {n_dup} duplicate (code_pays, wpid_random) pairs"))
} else {
  message("  No duplicate IDs (code_pays x wpid_random)")
}

# Flag summary
if (length(flag_cols) > 0) {
  message("\n── Flags summary ──")
  for (fc in flag_cols) {
    if (fc %in% names(df_out)) {
      n <- sum(df_out[[fc]], na.rm = TRUE)
      message(glue("  {fc}: {n} flagged rows ({round(n/nrow(df_out)*100,2)}%)"))
    }
  }
}

# ── Save final output ────────────────────────────────────────────────────────
message("\n── Saving final output...")
fwrite(df_out, file.path(FINDEX_OUTPUT_PATH, FINDEX_OUTPUT_FILE))

message(strrep("=", 60))
message(glue("FINAL OUTPUT: data/output/{FINDEX_OUTPUT_FILE}"))
message(glue("  {nrow(df_out)} rows x {ncol(df_out)} columns"))
message(glue("  Countries: {paste(sort(unique(df_out$code_pays)), collapse=', ')}"))
message(strrep("=", 60))
