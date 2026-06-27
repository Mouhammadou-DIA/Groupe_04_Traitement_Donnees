# =============================================================================
# run_all.R — Pipeline orchestrator — Global Findex UEMOA
#
# Usage (depuis la racine du projet) :
#   Rscript run_all.R           # execute tout le pipeline
#   Rscript run_all.R step1     # etape 1 seulement (dictionnaire initial)
#   Rscript run_all.R step2     # etape 2 seulement (application dictionnaire)
#   Rscript run_all.R step3     # etape 3 seulement (nettoyage + output)
#   Rscript run_all.R qaqc      # rapport QAQC seulement
# =============================================================================

pacman::p_load(stringr, here, glue)

# ── Determine this script's directory as the project root ─────────────────
# Works whether run via Rscript, source(), or interactively.
.get_script_dir <- function() {
  # Rscript: --file= argument
  args <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", args, value = TRUE)
  if (length(f) > 0) return(normalizePath(dirname(sub("^--file=", "", f)), winslash = "/"))
  # source(): sys.frame
  for (i in seq_len(sys.nframe())) {
    fn <- sys.frame(i)$ofile
    if (!is.null(fn)) return(normalizePath(dirname(fn), winslash = "/"))
  }
  # Fallback: working directory
  normalizePath(getwd(), winslash = "/")
}

PROJECT_ROOT <- .get_script_dir()

# Create .here marker so here() resolves correctly for sub-scripts too
here_marker <- file.path(PROJECT_ROOT, ".here")
if (!file.exists(here_marker)) file.create(here_marker)

# Re-initialise here() to use our project root
here::i_am(".here")

args   <- commandArgs(trailingOnly = TRUE)
target <- if (length(args) > 0) tolower(args[1]) else "all"

message(glue("Project root: {PROJECT_ROOT}"))

# =============================================================================
# REGISTRE DES SCRIPTS
# Pour ajouter une nouvelle etape : ajouter une entree dans PIPELINE_SCRIPTS.
# =============================================================================

PIPELINE_SCRIPTS <- list(

  list(step = "step1",
       path = here("1_data_exploration/1_get_initial_dict"),
       desc = "Generation du dictionnaire initial"),

  list(step = "step2",
       path = here("1_data_exploration/2_select_and_label"),
       desc = "Application du dictionnaire (renommage, labels, types)"),

  list(step = "step3",
       path = here("2_clean_and_merge"),
       desc = "Nettoyage complet et output consolide"),

  list(step = "qaqc",
       path = here("9_qaqc/1_survey_data_qaqc"),
       desc = "Rapport QAQC")
)

# =============================================================================
# COLLECTE DES SCRIPTS
# =============================================================================

collect_scripts <- function(target) {
  if (target == "all") {
    entries <- PIPELINE_SCRIPTS
  } else {
    entries <- Filter(function(e) e$step == target, PIPELINE_SCRIPTS)
    if (length(entries) == 0)
      stop(glue("Etape inconnue: '{target}'. Options: all, step1, step2, step3, qaqc"))
  }
  scripts <- character(0)
  for (e in entries) {
    found   <- sort(list.files(e$path, pattern = "\\.R$", full.names = TRUE))
    # Exclure les scripts EHCVM (hh, ind, merge)
    found   <- found[!grepl("hh_dict|ind_dict|apply_hh|apply_ind|clean_hh|merge_hh|qaqc_report\\.R$", found)]
    scripts <- c(scripts, found)
  }
  scripts
}

scripts_to_run <- collect_scripts(target)

if (length(scripts_to_run) == 0)
  stop(glue("Aucun script trouve pour '{target}'"))

# =============================================================================
# EXECUTION
# =============================================================================

message(strrep("=", 60))
message(glue("PIPELINE FINDEX UEMOA | Etape: {toupper(target)} | {length(scripts_to_run)} script(s)"))
message(strrep("=", 60))

for (s in scripts_to_run) {
  message(glue("\n>> {basename(s)}"))
  result <- system2("Rscript", c("--vanilla", shQuote(s)))
  if (result != 0)
    stop(glue("Echec (code {result}): {basename(s)}"))
  message(glue("   OK: {basename(s)}"))
}

message(paste0("\n", strrep("=", 60)))
message(glue("Pipeline termine — {length(scripts_to_run)} script(s) executes"))
message(strrep("=", 60))
