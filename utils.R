# =============================================================================
# utils.R — Reusable pipeline functions for the Findex UEMOA pipeline
# =============================================================================

pacman::p_load(dplyr, stringr, forcats, glue, purrr)

# =============================================================================
# SECTION 1 — DICTIONARY HELPERS
# =============================================================================

#' Apply a variable dictionary to a data.frame
#' (renaming, relabelling, type coercion, variable selection)
apply_var_dictionary <- function(df, dict) {

  stopifnot(all(c("var_orig", "var_new", "type_new", "keep") %in% names(dict)))

  dict <- dict %>% filter(var_orig %in% names(df))

  rename_map <- setNames(dict$var_orig, dict$var_new)
  df <- df %>% rename(!!!rename_map)

  label_map <- setNames(dict$label_new, dict$var_new)
  for (v in intersect(names(df), names(label_map))) {
    attr(df[[v]], "label") <- label_map[[v]]
  }

  for (i in seq_len(nrow(dict))) {
    v <- dict$var_new[i]
    t <- dict$type_new[i]
    if (!v %in% names(df)) next
    if      (t == "factor")    df[[v]] <- haven::as_factor(df[[v]], levels = "labels")
    else if (t == "numeric")   df[[v]] <- as.numeric(df[[v]])
    else if (t == "character") df[[v]] <- as.character(df[[v]])
  }

  vars_keep <- dict %>% filter(tolower(keep) == "yes") %>% pull(var_new)
  df %>% select(any_of(vars_keep))
}

#' Apply a modality dictionary to a data.frame (label harmonisation)
apply_modality_dictionary <- function(df, dict) {
  vars_to_recode <- intersect(names(df), unique(dict$var_name))
  df %>%
    mutate(across(
      all_of(vars_to_recode),
      ~ {
        if (!is.factor(.x)) return(.x)
        d <- dict %>% filter(var_name == cur_column())
        if (nrow(d) == 0) return(.x)
        fct_relabel(.x, function(lvls) {
          idx <- match(lvls, d$label_init)
          ifelse(is.na(idx), lvls, d$label_new[idx])
        })
      }
    ))
}

# =============================================================================
# SECTION 2 — FINDEX-SPECIFIC RECODING
# =============================================================================

#' Recode Gallup-coded binary variables (1=yes, 2=no, 3=dk, 4=ref) -> 1/0/NA/NA
#' Scalable: pass a vector of variable names; skips those absent from df.
recode_gallup_binary <- function(df, vars) {
  vars_present <- intersect(names(df), vars)
  n_skipped    <- length(vars) - length(vars_present)
  if (n_skipped > 0)
    message(glue("  [recode_gallup] {n_skipped} variable(s) absent from df — skipped"))

  df <- df %>%
    mutate(across(
      all_of(vars_present),
      ~ case_when(
          .x == 1 ~ 1L,
          .x == 2 ~ 0L,
          .x == 3 ~ NA_integer_,
          .x == 4 ~ NA_integer_,
          TRUE    ~ NA_integer_
        )
    ))

  message(glue("  [recode_gallup] {length(vars_present)} variable(s) recoded -> 0/1/NA"))
  df
}

#' Recode categorical Gallup variables with explicit label mapping
#' recode_map: named list where each element is a named character vector
#'             names = original codes (as character), values = new labels
recode_gallup_categ <- function(df, recode_map) {
  for (v in names(recode_map)) {
    if (!v %in% names(df)) next
    mapping  <- recode_map[[v]]
    n_before <- sum(!is.na(df[[v]]))
    df[[v]]  <- mapping[as.character(df[[v]])]
    n_after  <- sum(!is.na(df[[v]]))
    message(glue("  [recode_categ] {v} recoded — {n_before - n_after} new NA(s) (dk/ref)"))
  }
  df
}

#' Recompute epargne_totale as sum of binary savings instruments after recoding
#' savings_vars: character vector of savings instrument variables (must be 0/1)
compute_epargne_totale <- function(df, savings_vars) {
  vars_present <- intersect(names(df), savings_vars)
  if (length(vars_present) == 0) {
    message("  [epargne_totale] No savings variables found — skipped")
    return(df)
  }
  df <- df %>%
    mutate(epargne_totale = rowSums(across(all_of(vars_present)), na.rm = TRUE))
  message(glue("  [epargne_totale] Recomputed from {length(vars_present)} savings variable(s)"))
  message(glue("  [epargne_totale] Distribution: {paste(names(table(df$epargne_totale)),
                                                         table(df$epargne_totale), sep='=',
                                                         collapse=' | ')}"))
  df
}

# =============================================================================
# SECTION 3 — GENERIC CLEANING FUNCTIONS
# =============================================================================

#' Remove constant columns and those listed in vars_to_drop
drop_vars <- function(df, params) {
  to_drop       <- intersect(names(df), params$vars_to_drop)
  constant_cols <- names(df)[sapply(df, function(x) length(unique(na.omit(x))) == 1)]
  constant_cols <- setdiff(constant_cols, params$id_vars)
  all_drop      <- unique(c(to_drop, constant_cols))
  if (length(all_drop) > 0)
    message(glue("  [drop_vars] Removing {length(all_drop)} column(s): {paste(all_drop, collapse=', ')}"))
  df %>% select(-any_of(all_drop))
}

#' Apply domain bounds to numeric variables; out-of-bounds -> NA
bound_numeric <- function(df, params) {
  bounds <- params$numeric_bounds
  vars   <- intersect(names(df), names(bounds))
  for (v in vars) {
    lo <- bounds[[v]][1]; hi <- bounds[[v]][2]
    n_before <- sum(!is.na(df[[v]]))
    df[[v]]  <- if_else(!is.na(df[[v]]) & (df[[v]] < lo | df[[v]] > hi),
                        NA_real_, as.numeric(df[[v]]))
    n_out <- n_before - sum(!is.na(df[[v]]))
    if (n_out > 0)
      message(glue("  [bound_numeric] {v}: {n_out} value(s) outside [{lo}, {hi}] -> NA"))
  }
  df
}

#' Impute NA for numeric variables
impute_numeric <- function(df, params) {
  strategies <- params$numeric_impute
  vars       <- intersect(names(df), names(strategies))
  for (v in vars) {
    strat  <- strategies[[v]]
    n_na   <- sum(is.na(df[[v]]))
    if (n_na == 0 || strat == "none") next
    fill_val <- switch(strat,
      "median" = median(df[[v]], na.rm = TRUE),
      "mean"   = mean(df[[v]],   na.rm = TRUE),
      "zero"   = 0,
      NA_real_
    )
    df[[v]] <- if_else(is.na(df[[v]]), fill_val, df[[v]])
    message(glue("  [impute_numeric] {v}: {n_na} NA imputed with {strat} ({round(fill_val, 2)})"))
  }
  df
}

#' Impute NA for categorical variables
impute_categ <- function(df, params) {
  strategies <- params$categ_impute
  vars       <- intersect(names(df), names(strategies))
  for (v in vars) {
    strat  <- strategies[[v]]
    n_na   <- sum(is.na(df[[v]]))
    if (n_na == 0 || strat == "none") next
    fill_val <- if (strat == "mode") {
      names(sort(table(df[[v]]), decreasing = TRUE))[1]
    } else strat
    df[[v]] <- if_else(is.na(df[[v]]), fill_val, df[[v]])
    message(glue("  [impute_categ] {v}: {n_na} NA imputed with '{fill_val}'"))
  }
  df
}

#' Apply cross-variable consistency rules
#' action = "na" : replace target with NA
#' action = "flag": create a boolean flag_* column
apply_consistency_rules <- function(df, params) {
  rules <- params$consistency_rules
  for (rule in rules) {
    mask <- tryCatch(
      eval(parse(text = rule$condition), envir = df),
      error = function(e) {
        message(glue("  [consistency] Rule '{rule$label}': error -> {e$message}"))
        return(rep(FALSE, nrow(df)))
      }
    )
    n_flagged <- sum(mask, na.rm = TRUE)
    if (n_flagged == 0) next
    if (rule$action == "na") {
      df[[rule$target]][mask] <- NA
      message(glue("  [consistency] '{rule$label}': {n_flagged} row(s) -> NA on '{rule$target}'"))
    } else if (rule$action == "flag") {
      flag_col <- paste0("flag_", rule$target)
      df[[flag_col]] <- if (flag_col %in% names(df)) df[[flag_col]] | mask else mask
      message(glue("  [consistency] '{rule$label}': {n_flagged} row(s) flagged -> '{flag_col}'"))
    }
  }
  df
}

#' Normalise character columns (trim + squish whitespace)
normalize_categ <- function(df) {
  char_cols <- names(df)[sapply(df, is.character)]
  df <- df %>% mutate(across(all_of(char_cols), ~ str_squish(str_trim(.x))))
  message(glue("  [normalize_categ] Whitespace normalised on {length(char_cols)} column(s)"))
  df
}

#' Remove exact duplicate rows based on primary key
dedup <- function(df, key_cols) {
  n_before <- nrow(df)
  df       <- df %>% distinct(across(all_of(key_cols)), .keep_all = TRUE)
  n_dup    <- n_before - nrow(df)
  if (n_dup > 0)
    message(glue("  [dedup] {n_dup} duplicate(s) removed on ({paste(key_cols, collapse=', ')})"))
  else
    message(glue("  [dedup] No duplicates on ({paste(key_cols, collapse=', ')})"))
  df
}

#' Full cleaning pipeline — orchestrates all steps in order
run_cleaning_pipeline <- function(df, params, key_cols, label = "") {
  message(paste0("\n", strrep("=", 60)))
  message(glue("Cleaning: {label}  ({nrow(df)} rows x {ncol(df)} columns)"))
  message(strrep("=", 60))
  df <- df %>%
    dedup(key_cols)                  %>%
    drop_vars(params)                %>%
    normalize_categ()                %>%
    bound_numeric(params)            %>%
    apply_consistency_rules(params)  %>%
    impute_numeric(params)           %>%
    impute_categ(params)
  message(glue("\n  Final result: {nrow(df)} rows x {ncol(df)} columns"))
  df
}
