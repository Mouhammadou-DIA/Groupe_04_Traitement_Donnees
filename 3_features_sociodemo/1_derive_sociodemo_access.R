# =============================================================================
# 3_features_sociodemo / 1_derive_sociodemo_access.R
#
# Etape de variables derivees (socio-demographiques & acces).
# Se branche EN AVAL du nettoyage : lit la sortie nettoyee du pipeline
# (data/output/findex_uemoa_clean.csv) et ajoute des variables d'analyse.
#
# Reutilise le cadre du groupe (config.R, utils.R, here()).
# Lancement : Rscript --vanilla 3_features_sociodemo/1_derive_sociodemo_access.R
#             ou via run_all.R (etape "features").
# =============================================================================

pacman::p_load(here, readr, dplyr, data.table, glue)

source(here::here("config.R"))
source(here::here("utils.R"))

message(strrep("=", 60))
message("VARIABLES DERIVEES — socio-demo & acces")
message(strrep("=", 60))

# --- Lecture de la sortie nettoyee ------------------------------------------
in_path <- here::here(FINDEX_OUTPUT_PATH, FINDEX_OUTPUT_FILE)
df <- read_csv(in_path, show_col_types = FALSE,
               locale = locale(encoding = "UTF-8"))
message(glue("  Entree : {FINDEX_OUTPUT_FILE} ({nrow(df)} lignes x {ncol(df)} colonnes)"))

num <- function(x) suppressWarnings(as.numeric(x))

# --- Construction des variables derivees ------------------------------------
df <- df %>%
  mutate(
    # Tranches d'age decennales
    groupe_age = cut(
      num(age),
      breaks = c(15, 25, 35, 45, 55, 65, Inf),
      labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
      right  = FALSE
    ),
    # Jeune (15-24 ans) : 1 = oui, 0 = non
    jeune_15_24 = if_else(num(age) < 25, 1L, 0L),
    # Detention d'au moins une carte (debit OU credit)
    detention_carte = case_when(
      num(fin2_carte_debit) == 1 | num(fin7_carte_credit) == 1 ~ 1L,
      num(fin2_carte_debit) == 0 & num(fin7_carte_credit) == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    # Indicateur synthetique d'inclusion financiere (= possede un compte)
    inclusion_financiere = num(account)
  )

message(glue("  + 4 variable(s) derivee(s) : groupe_age, jeune_15_24, ",
             "detention_carte, inclusion_financiere"))

# --- Controle qualite rapide ------------------------------------------------
message("\n  Repartition groupe_age :")
print(table(df$groupe_age, useNA = "ifany"))

message("\n  Taux d'inclusion financiere par pays (%) :")
incl <- df %>%
  group_by(pays) %>%
  summarise(taux_inclusion = round(mean(inclusion_financiere, na.rm = TRUE) * 100, 1),
            .groups = "drop")
print(as.data.frame(incl), row.names = FALSE)

message(glue("\n  Detention de carte : {sum(df$detention_carte == 1, na.rm = TRUE)} ",
            "detenteur(s) ; {sum(is.na(df$detention_carte))} NA"))

# --- Export -----------------------------------------------------------------
out_path <- here::here(FINDEX_OUTPUT_PATH, FINDEX_FEATURES_FILE)
fwrite(df, out_path, bom = TRUE)
message(glue("\n  Sortie : {FINDEX_FEATURES_FILE} ({nrow(df)} lignes x {ncol(df)} colonnes)"))
message("  Etape variables derivees : terminee.")
