# Combine Functions — build derived datasets from multiple processed .rds files
#
# These functions support combine_widget.R, which lets users construct new
# datasets (e.g. Test Positivity Rate) by combining and transforming variables
# from existing processed datasets.

# Flag columns that can be inherited from source datasets (Phase 1 inheritance)
.source_flag_cols <- c("over_max", "mad15", "mad10", "seasonal5", "seasonal3")

# ---------------------------------------------------------------------------
# Phase 2 outlier scan helper
# ---------------------------------------------------------------------------

#' Scan one series for outliers, respecting the severity hierarchy
#'
#' Called by [build_combined_dataset()] via `data.table` `by` grouping.
#' Phase 1 (inherited) flags are the base; Phase 2 adds any new detections and
#' OR-es them in.  Hierarchy: over_max stops all scanning; mad15 stops mad10;
#' mad10 stops seasonal; seasonal5 stops seasonal3.
#'
#' Uses `smallThreshold = 0` so ratio values in \[0, 1\] are not silently skipped
#' (the default threshold of 50 would skip every ratio series).
#'
#' @param original Numeric vector (one orgUnit × data series).
#' @param over_max,mad15,mad10,seasonal5,seasonal3 Logical vectors —
#'   Phase 1 inherited flags for the same series.
#' @return Named list of length 4: mad15, mad10, seasonal5, seasonal3 (in that
#'   order, matching the LHS of the `data.table` `:=` assignment).
#' @noRd
.scan_series_outliers <- function(original, over_max,
                                  mad15, mad10, seasonal5, seasonal3) {
  x   <- as.numeric(original)
  om  <- as.logical(over_max);   om[is.na(om)]   <- FALSE
  m15 <- as.logical(mad15);      m15[is.na(m15)] <- FALSE
  m10 <- as.logical(mad10);      m10[is.na(m10)] <- FALSE
  s5  <- as.logical(seasonal5);  s5[is.na(s5)]   <- FALSE
  s3  <- as.logical(seasonal3);  s3[is.na(s3)]   <- FALSE

  # mad15: scan values not already over_max
  not_flagged <- !om & !is.na(x)
  if (any(not_flagged)) {
    x_scan <- x; x_scan[!not_flagged] <- NA_real_
    new15 <- tryCatch(
      extremely_mad(x_scan, deviation = 15, smallThreshold = 0, logical = TRUE),
      error = function(e) rep(FALSE, length(x))
    )
    new15[is.na(new15)] <- FALSE
    m15 <- m15 | new15
  }

  # mad10: scan values not over_max and not mad15
  not_flagged <- !om & !m15 & !is.na(x)
  if (any(not_flagged)) {
    x_scan <- x; x_scan[!not_flagged] <- NA_real_
    new10 <- tryCatch(
      extremely_mad(x_scan, deviation = 10, smallThreshold = 0, logical = TRUE),
      error = function(e) rep(FALSE, length(x))
    )
    new10[is.na(new10)] <- FALSE
    m10 <- m10 | new10
  }

  # seasonal5: scan values not over_max, mad15, or mad10
  not_flagged <- !om & !m15 & !m10 & !is.na(x)
  if (any(not_flagged)) {
    x_scan <- x; x_scan[!not_flagged] <- NA_real_
    news5 <- tryCatch(
      unseasonal(x_scan, smallThreshold = 0, deviation = 5, logical = TRUE),
      error = function(e) rep(FALSE, length(x))
    )
    news5[is.na(news5)] <- FALSE
    s5 <- s5 | news5
  }

  # seasonal3: scan values not over_max, mad15, mad10, or seasonal5
  not_flagged <- !om & !m15 & !m10 & !s5 & !is.na(x)
  if (any(not_flagged)) {
    x_scan <- x; x_scan[!not_flagged] <- NA_real_
    news3 <- tryCatch(
      unseasonal(x_scan, smallThreshold = 0, deviation = 3, logical = TRUE),
      error = function(e) rep(FALSE, length(x))
    )
    news3[is.na(news3)] <- FALSE
    s3 <- s3 | news3
  }

  # Order matches c("mad15","mad10","seasonal5","seasonal3") in the := call
  list(m15, m10, s5, s3)
}

# ---------------------------------------------------------------------------
# Metadata helpers
# ---------------------------------------------------------------------------

#' Read metadata from a processed dataset file
#'
#' Returns unique `data` values and available cleaning levels without loading
#' the full dataset.  Called when populating the combine widget's source-file
#' pickers.
#'
#' @param file_path Full path to a processed `.rds` dataset file.
#' @return Named list: `data_values`, `cleaning_levels`, `period_type`, `n_rows`.
#' @export
read_combine_meta <- function(file_path) {
  d <- readRDS(file_path)

  outlier_order <- c("key_entry_error", "over_max", "mad15", "mad10", "seasonal5", "seasonal3")
  available     <- intersect(outlier_order, names(d))

  list(
    data_values     = sort(unique(as.character(d$data[!is.na(d$data)]))),
    cleaning_levels = available,
    period_type     = if ("Week" %in% names(d)) "Week" else "Month",
    n_rows          = nrow(d)
  )
}

# ---------------------------------------------------------------------------
# Cleaning
# ---------------------------------------------------------------------------

#' Apply outlier cleaning at a specified cumulative severity level
#'
#' Replaces values flagged as outliers — at or below the chosen severity — with
#' the modelled `expected` value (or `NA` when `expected` is unavailable).
#'
#' Severity order (most → least severe):
#' `key_entry_error` > `over_max` > `mad15` > `mad10` > `seasonal5` > `seasonal3`
#'
#' "Use mad15" means: only replace values flagged by key_entry_error, over_max,
#' or mad15.  "Use seasonal3" replaces everything detected at any level.
#'
#' @param d data.frame or data.table with `original`, optional `expected`, and
#'   outlier flag columns.
#' @param cleaning_level One of `"none"`, `"mad15"`, `"mad10"`, `"seasonal5"`,
#'   `"seasonal3"`.
#' @return Numeric vector of cleaned values, same length as `nrow(d)`.
#' @export
apply_combine_cleaning <- function(d, cleaning_level = "seasonal3") {
  original <- as.numeric(d$original)

  if (cleaning_level == "none" || !"original" %in% names(d)) return(original)

  level_order <- c("key_entry_error", "over_max", "mad15", "mad10", "seasonal5", "seasonal3")

  if (!cleaning_level %in% level_order) {
    warning("apply_combine_cleaning: unknown level '", cleaning_level, "', returning original")
    return(original)
  }

  flag_cols <- intersect(
    level_order[seq_len(match(cleaning_level, level_order))],
    names(d)
  )

  if (length(flag_cols) == 0) return(original)

  is_outlier <- Reduce(`|`, lapply(flag_cols, function(col) {
    v <- d[[col]]
    !is.na(v) & as.logical(v)
  }))

  if (!any(is_outlier, na.rm = TRUE)) return(original)

  cleaned <- original
  if ("expected" %in% names(d)) {
    cleaned[is_outlier] <- as.numeric(d$expected[is_outlier])
  } else {
    cleaned[is_outlier] <- NA_real_
  }
  cleaned
}

# ---------------------------------------------------------------------------
# Step execution
# ---------------------------------------------------------------------------

#' Execute an Include step
#' @noRd
execute_include_step <- function(step, data_folder, period_col, verbose = FALSE) {
  cat("  [Include]", step$output_name, "← file:", step$source_file,
      "| values:", paste(step$data_values, collapse = ", "),
      "| cleaning:", step$cleaning_level, "\n")

  fp <- paste0(data_folder, step$source_file)
  if (!file.exists(fp)) stop("File not found: ", fp)

  d <- readRDS(fp)
  if (!is.data.table(d)) setDT(d)

  d <- d[data %chin% step$data_values]
  if (nrow(d) == 0) {
    warning("execute_include_step: no rows matched for: ",
            paste(step$data_values, collapse = ", "))
    return(data.table())
  }
  cat("  ", nrow(d), "rows after filter\n")

  cleaned <- apply_combine_cleaning(d, step$cleaning_level)

  out_name <- step$output_name
  if (!is.null(out_name) && nchar(trimws(out_name)) > 0 && length(step$data_values) == 1) {
    d[, data := out_name]
  }

  # Build output — carry Phase 1 flag columns from source data
  out <- d[, .(orgUnit, data, original = cleaned)]
  set(out, j = period_col, value = d[[period_col]])

  flag_cols_avail <- intersect(.source_flag_cols, names(d))
  for (fc in flag_cols_avail) {
    set(out, j = fc, value = as.logical(d[[fc]]))
  }

  out
}

#' Execute a Ratio step
#' @noRd
execute_ratio_step <- function(step, data_folder, period_col, verbose = FALSE) {

  load_side <- function(spec, label) {
    cat("  [Ratio]", label, "← file:", spec$source_file,
        "| values:", paste(spec$data_values, collapse = ", "),
        "| cleaning:", spec$cleaning_level, "\n")

    fp <- paste0(data_folder, spec$source_file)
    if (!file.exists(fp)) stop("File not found: ", fp)

    d <- readRDS(fp)
    if (!is.data.table(d)) setDT(d)
    d <- d[data %chin% spec$data_values]
    if (nrow(d) == 0) {
      warning("No rows matched for ", label, ": ",
              paste(spec$data_values, collapse = ", "))
      return(data.table())
    }
    cat("  ", nrow(d), "rows after filter\n")

    cleaned <- apply_combine_cleaning(d, spec$cleaning_level)

    # Build output — carry Phase 1 flag columns from source data
    out <- d[, .(orgUnit)]
    set(out, j = period_col, value = d[[period_col]])
    out[, val := cleaned]

    flag_cols_avail <- intersect(.source_flag_cols, names(d))
    for (fc in flag_cols_avail) {
      set(out, j = fc, value = as.logical(d[[fc]]))
    }

    out
  }

  num <- load_side(step$numerator,   paste("numerator  →", step$output_name))
  den <- load_side(step$denominator, paste("denominator →", step$output_name))

  if (nrow(num) == 0 || nrow(den) == 0) {
    warning("execute_ratio_step '", step$output_name, "': empty numerator or denominator")
    return(data.table())
  }

  by_cols <- c("orgUnit", period_col)

  # Aggregate per orgUnit × period: sum values, OR flag columns
  flag_cols_n <- intersect(.source_flag_cols, names(num))
  if (length(flag_cols_n) > 0) {
    num <- num[, c(.(val = sum(val, na.rm = FALSE)),
                   lapply(.SD, any, na.rm = TRUE)),
               .SDcols = flag_cols_n, by = by_cols]
  } else {
    num <- num[, .(val = sum(val, na.rm = FALSE)), by = by_cols]
  }

  flag_cols_d <- intersect(.source_flag_cols, names(den))
  if (length(flag_cols_d) > 0) {
    den <- den[, c(.(val = sum(val, na.rm = FALSE)),
                   lapply(.SD, any, na.rm = TRUE)),
               .SDcols = flag_cols_d, by = by_cols]
  } else {
    den <- den[, .(val = sum(val, na.rm = FALSE)), by = by_cols]
  }

  cat("  num rows:", nrow(num), " | den rows:", nrow(den), "\n")

  # Full outer join: keeps rows where only numerator OR only denominator exists.
  # Those rows produce missing_numerator / missing_denominator flags.
  joined <- merge(num, den, by = by_cols, suffixes = c("_n", "_d"), all = TRUE)
  cat("  joined rows:", nrow(joined), "\n")

  # Combine flag columns from numerator and denominator sides (OR)
  for (fc in .source_flag_cols) {
    cn_n <- paste0(fc, "_n"); cn_d <- paste0(fc, "_d")
    has_n <- cn_n %in% names(joined); has_d <- cn_d %in% names(joined)
    if (has_n || has_d) {
      fn_val <- if (has_n) as.logical(joined[[cn_n]]) else rep(FALSE, nrow(joined))
      fd_val <- if (has_d) as.logical(joined[[cn_d]]) else rep(FALSE, nrow(joined))
      combined_flag <- fn_val | fd_val
      combined_flag[is.na(combined_flag)] <- FALSE
      set(joined, j = fc, value = combined_flag)
      if (has_n) set(joined, j = cn_n, value = NULL)
      if (has_d) set(joined, j = cn_d, value = NULL)
    }
  }

  joined[, missing_numerator   := is.na(val_n)]
  joined[, missing_denominator := is.na(val_d)]

  joined[, original := fcase(
    is.na(val_n) | is.na(val_d), NA_real_,
    val_d == 0,                  NA_real_,
    val_n == 0,                  0,
    default = val_n / val_d
  )]
  cat("  non-NA ratio values:", sum(!is.na(joined$original)), "\n")
  cat("  missing_numerator:", sum(joined$missing_numerator), "\n")
  cat("  missing_denominator:", sum(joined$missing_denominator), "\n")

  joined[, data := step$output_name]

  # Build output — always include numerator/denominator for ratio-aware aggregation,
  # and carry Phase 1 inherited flags
  flag_cols_final <- intersect(.source_flag_cols, names(joined))

  base_out <- joined[, .(orgUnit, data,
                          numerator           = val_n,
                          denominator         = val_d,
                          missing_numerator,
                          missing_denominator,
                          original,
                          .p                  = get(period_col))] |>
    setnames(".p", period_col)

  for (fc in flag_cols_final) {
    set(base_out, j = fc, value = joined[[fc]])
  }

  base_out
}

# ---------------------------------------------------------------------------
# Pipeline
# ---------------------------------------------------------------------------

#' Build a combined dataset from a list of step definitions
#'
#' Executes each step in sequence and binds results into a single
#' processed-format dataset, joining the org unit hierarchy from `ousTree`.
#' The output is compatible with the standard MG2 pipeline
#' (DQA → Reporting → Outliers → Evaluation).
#'
#' Outlier flags are populated in two phases:
#' \enumerate{
#'   \item \strong{Inheritance} — each step carries flag columns from its
#'     source dataset(s).  For Ratio steps, flags from numerator and
#'     denominator are combined with OR per orgUnit × period.
#'   \item \strong{Direct scan} — [extremely_mad()] and [unseasonal()] are
#'     applied to the combined values using `smallThreshold = 0` (the default
#'     of 50 would skip all ratio series whose values are in \[0, 1\]).
#'     Results are OR-ed with Phase 1 flags: an inherited `TRUE` is never
#'     cleared.  The scan respects the severity hierarchy — once a more severe
#'     flag is set, less severe algorithms are skipped for that value.
#' }
#'
#' @param steps List of step definition lists (from a `Combinations_*.rds` file).
#' @param data_folder Directory path (trailing slash).
#' @param ousTree data.frame with org unit hierarchy (from `metadata_widget`).
#' @param formula_name Character; stored as `Formula.Name` in the output.
#' @param verbose Logical; print progress messages (always prints to console).
#' @param .progress Optional function `(value, message, detail = "")` called at
#'   key stages.  `value` is 0–1.  Pass `shiny::setProgress` (or a wrapper)
#'   when calling from a Shiny `withProgress()` context.
#' @return data.table in standard processed-dataset format.
#' @export
build_combined_dataset <- function(steps, data_folder, ousTree = NULL,
                                   formula_name = "Combined", verbose = TRUE,
                                   .progress = NULL) {
  if (length(steps) == 0) stop("build_combined_dataset: no steps defined")

  # Progress reporting helper — no-op when .progress is NULL
  rp <- function(value, message, detail = "") {
    if (is.function(.progress)) .progress(value = value, message = message, detail = detail)
  }

  cat("\n* build_combined_dataset:", formula_name, "\n")
  cat("- data_folder:", data_folder, "\n")
  cat("- steps:", length(steps), "\n")

  rp(0.02, "Detecting period type")

  # Determine period type from the first referenced source file
  period_col <- "Month"
  for (s in steps) {
    src <- if (s$operation == "Ratio") s$numerator$source_file else s$source_file
    if (!is.null(src)) {
      fp <- paste0(data_folder, src)
      if (file.exists(fp)) {
        tmp        <- readRDS(fp)
        period_col <- if ("Week" %in% names(tmp)) "Week" else "Month"
        rm(tmp)
        cat("- period type:", period_col, "\n")
        break
      }
    }
  }

  results <- vector("list", length(steps))
  n_steps <- length(steps)

  for (i in seq_along(steps)) {
    s <- steps[[i]]
    cat("\n- step", i, "[", s$operation, "] →", s$output_name, "\n")
    rp(0.05 + (i - 1) / n_steps * 0.50,
       paste0("Step ", i, " of ", n_steps, ": ", s$output_name),
       s$operation)

    rows <- tryCatch({
      r <- switch(s$operation,
        Include = execute_include_step(s, data_folder, period_col, verbose),
        Ratio   = execute_ratio_step(s,   data_folder, period_col, verbose),
        {
          warning("Unknown operation '", s$operation, "'"); NULL
        }
      )
      r
    },
    error = function(e) {
      cat("  ERROR:", conditionMessage(e), "\n"); NULL
    })

    if (!is.null(rows) && nrow(rows) > 0) {
      cat("  step", i, "produced", nrow(rows), "rows\n")
      results[[i]] <- rows
    } else {
      cat("  step", i, "produced NO rows\n")
    }
  }

  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) stop("build_combined_dataset: all steps produced empty output")

  rp(0.58, "Binding results", paste(length(results), "step(s)"))
  cat("\n- binding", length(results), "result(s)\n")
  combined <- rbindlist(results, fill = TRUE)

  combined[, `:=`(
    data.id       = data,
    value         = !is.na(original),
    dataCol       = original,
    effectiveLeaf = TRUE,
    Formula.Name  = formula_name
  )]

  # key_entry_error: not applicable for derived data (always FALSE)
  combined[, key_entry_error := FALSE]

  # Phase 1 flags are already in `combined` (inherited from source datasets via
  # execute_*_step).  Only initialize columns that are missing; coerce NA→FALSE
  # for any NAs introduced by rbindlist(fill=TRUE) across steps with different
  # source flag coverage.
  for (fc in .source_flag_cols) {
    if (fc %in% names(combined)) {
      combined[is.na(get(fc)), (fc) := FALSE]
    } else {
      combined[, (fc) := FALSE]
    }
  }

  # missing_numerator / missing_denominator come from execute_ratio_step.
  # Include steps don't produce them; fill NAs introduced by rbindlist(fill=TRUE).
  if (!"missing_numerator" %in% names(combined))
    combined[, missing_numerator := FALSE]
  if (!"missing_denominator" %in% names(combined))
    combined[, missing_denominator := FALSE]
  combined[is.na(missing_numerator),   missing_numerator   := FALSE]
  combined[is.na(missing_denominator), missing_denominator := FALSE]

  # Apply per-step max_value to flag over_max (OR with Phase 1 inherited flags)
  for (s in steps) {
    mv <- if (!is.null(s$max_value) && !is.na(s$max_value)) as.numeric(s$max_value) else NA_real_
    if (!is.na(mv)) {
      combined[data == s$output_name & !is.na(original) & as.numeric(original) > mv,
               over_max := TRUE]
      n_over <- sum(combined[data == s$output_name]$over_max, na.rm = TRUE)
      cat("- over_max: '", s$output_name, "' max =", mv, "→", n_over, "flagged\n")
    }
  }

  # Phase 2: direct outlier scan on combined values.
  # Respects hierarchy: over_max → stop; mad15 → skip mad10; mad10 → skip seasonal.
  # Uses smallThreshold=0 because ratio values in [0,1] would all be skipped by
  # the default threshold of 50 used in mad_outliers()/seasonal_outliers().
  # New flags are OR-ed with Phase 1 inherited flags — once TRUE, stays TRUE.
  n_series <- nrow(unique(combined[, .(orgUnit, data)]))
  cat("- Phase 2: scanning combined values for outliers (smallThreshold=0)\n")
  cat("  ", n_series, "series to scan\n")
  rp(0.65, "Scanning for outliers", paste(format(n_series, big.mark = ","), "series"))

  combined[, c("mad15", "mad10", "seasonal5", "seasonal3") :=
    .scan_series_outliers(original, over_max, mad15, mad10, seasonal5, seasonal3),
    by = .(orgUnit, data)]

  n_m15 <- sum(combined$mad15,     na.rm = TRUE)
  n_m10 <- sum(combined$mad10,     na.rm = TRUE)
  n_s5  <- sum(combined$seasonal5, na.rm = TRUE)
  n_s3  <- sum(combined$seasonal3, na.rm = TRUE)
  cat("- Phase 2 complete: mad15=", n_m15, " mad10=", n_m10,
      " seasonal5=", n_s5, " seasonal3=", n_s3, "\n")
  rp(0.90, "Outlier scan complete",
     paste0("mad15=", n_m15, "  mad10=", n_m10,
            "  seasonal5=", n_s5, "  seasonal3=", n_s3))

  if (!is.null(ousTree) && nrow(ousTree) > 0 && "orgUnit" %in% names(ousTree)) {
    rp(0.93, "Joining org unit hierarchy",
       paste(nrow(ousTree), "org units"))
    cat("- joining ousTree (", nrow(ousTree), "org units)\n")
    ou_dt    <- as.data.table(ousTree)
    combined <- merge(combined, ou_dt, by = "orgUnit", all.x = TRUE)
  }

  cat("- done:", nrow(combined), "rows |",
      length(unique(combined$data)), "variable(s):",
      paste(sort(unique(combined$data)), collapse = ", "), "\n")
  rp(1.0, "Complete",
     paste0(format(nrow(combined), big.mark = ","), " rows  |  ",
            paste(sort(unique(combined$data)), collapse = ", ")))

  combined
}

# ---------------------------------------------------------------------------
# Formula file registration
# ---------------------------------------------------------------------------

#' Add a formula name to an existing formula xlsx file
#'
#' Appends a row with the given `formula_name` to the "Formula" sheet of the
#' specified xlsx so the combined dataset is discoverable by `data_widget`.
#' Does nothing if the name already exists.
#'
#' @param formula_name Character; the combination name (e.g. `"TPR"`).
#' @param formula_file Full path to a `Formulas_*.xlsx` file.
#' @return Invisible `TRUE` on success.
#' @export
register_combined_formula <- function(formula_name, formula_file) {
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("Package 'openxlsx' is required to update the formula file.")

  wb      <- openxlsx::loadWorkbook(formula_file)
  current <- openxlsx::readWorkbook(wb, sheet = "Formula", colNames = TRUE)

  if (formula_name %in% current$Formula.Name) {
    message("register_combined_formula: '", formula_name,
            "' already present — no change made")
    return(invisible(TRUE))
  }

  # Build a new row with NAs for all other columns
  new_row <- as.data.frame(
    setNames(lapply(names(current), function(col) {
      if (col == "Formula.Name") formula_name else NA
    }), names(current)),
    stringsAsFactors = FALSE
  )

  updated <- rbind(current, new_row)

  # If the sheet contains an Excel Table object, remove it first — writeData
  # cannot overwrite table headers.  The data is re-written as plain cells.
  tbls <- openxlsx::getTables(wb, sheet = "Formula")
  for (tbl in tbls) {
    openxlsx::removeTable(wb, sheet = "Formula", table = tbl)
  }

  openxlsx::writeData(wb, sheet = "Formula", x = updated, startRow = 1,
                      startCol = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, formula_file, overwrite = TRUE)
  cat("- registered '", formula_name, "' in", basename(formula_file), "\n")
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Definition file helpers
# ---------------------------------------------------------------------------

#' Suggest an output filename for a combined dataset
#'
#' @param combo_name User-supplied combination name (e.g. `"TPR"`).
#' @return Suggested base filename (no directory prefix).
#' @export
suggest_combine_filename <- function(combo_name) {
  # Only replace characters that are illegal in macOS/Windows filenames.
  # Keep spaces, >=, accented chars, etc. so data_widget can match the file
  # by formula name with grepl(formula_name, filename, fixed = TRUE).
  base <- gsub("[/:\\\\]", "_", trimws(combo_name))
  if (nchar(base) == 0) base <- "Combined"
  paste0(base, "_", format(Sys.Date(), "%Y-%m-%d"), ".rds")
}

#' Save a combination definition to a companion RDS file
#'
#' @param definition List with `steps`, `combo_name`, and optional `formula_file`.
#' @param file_path Full path for the companion file.
#' @export
save_combine_definition <- function(definition, file_path) {
  definition$modified <- Sys.time()
  if (is.null(definition$created)) definition$created <- Sys.time()
  saveRDS(definition, file_path)
  invisible(file_path)
}

#' Load a combination definition from a companion RDS file
#'
#' @param file_path Full path to a `Combinations_*.rds` file.
#' @return Definition list.
#' @export
load_combine_definition <- function(file_path) {
  readRDS(file_path)
}
