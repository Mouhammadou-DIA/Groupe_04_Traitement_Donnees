# =============================================================================
# config.R — Centralised path configuration
# Modify only this file to adapt the pipeline to a different project.
# =============================================================================

# ── Auxiliary files (dictionaries) ────────────────────────────────────────
AUX_FILE_PATH  <- "data/aux_file"

# ── Findex pipeline paths ──────────────────────────────────────────────────
FINDEX_INPUT_PATH        <- "data/input"
FINDEX_INTERMEDIATE_PATH <- "data/intermediates"
FINDEX_OUTPUT_PATH       <- "data/output"
FINDEX_OUTPUT_QAQC_PATH  <- "data/output_qaqc"

# ── Input file name (change to switch edition/survey) ─────────────────────
FINDEX_INPUT_FILE  <- "findex_uemoa_harmonise.csv"

# ── Output file names ──────────────────────────────────────────────────────
FINDEX_OUTPUT_FILE        <- "findex_uemoa_clean.csv"
FINDEX_OUTPUT_QAQC_REPORT <- "qaqc_findex_report.html"
