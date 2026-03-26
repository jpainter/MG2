# Metadata utilities
# Helpers for cleaning and preparing metadata data frames before
# export to Excel (openxlsx). Particularly useful for DHIS2 metadata
# which often contains non-printable Unicode characters or very long strings.

# Regex matching control characters and zero-width/invisible Unicode that
# cause problems in Excel exports.
.invalid_regex <- paste0(
  "(?:[\\p{Cc}&&[^\\t\\n\\r]]",
  "|[\\x{007F}-\\x{009F}]",
  "|\\u00AD|\\u200B|\\u200C|\\u200D|\\u2060|\\uFEFF)"
)

#' Remove Invalid Unicode Characters from a Data Frame
#'
#' Strips control characters and invisible Unicode code points from all
#' character columns in a data frame (or data.table). Operates in-place on
#' data.table objects; returns a modified copy for data frames.
#'
#' These characters appear in DHIS2 metadata names and descriptions and cause
#' Excel exports to fail or corrupt.
#'
#' @param df A data frame or data.table.
#'
#' @return The input `df` with invalid characters removed from character
#'   columns. Returns invisibly.
#' @export
#'
#' @examples
#' df <- data.frame(name = c("Good\u200Bname", "Normal"), value = 1:2)
#' clean_invalid_characters(df)
clean_invalid_characters <- function(df) {
  DT <- data.table::as.data.table(df)
  char_cols <- names(DT)[vapply(DT, is.character, logical(1))]

  if (!length(char_cols)) return(invisible(DT))

  for (col in char_cols) {
    x       <- DT[[col]]
    bad     <- stringi::stri_detect_regex(x, .invalid_regex)
    bad_idx <- which(bad)   # which() drops NAs, giving integer indices only
    if (length(bad_idx) > 0) {
      x[bad_idx] <- stringi::stri_replace_all_regex(x[bad_idx], .invalid_regex, "")
      data.table::set(DT, i = bad_idx, j = col, value = x[bad_idx])
    }
  }

  invisible(DT)
}

#' Truncate Long Strings in a Data Frame
#'
#' Shortens character values that exceed `limit` characters. Truncated values
#' get a `suffix` appended to signal that the string was cut. Useful before
#' writing to Excel, which has a 32,767 character cell limit.
#'
#' @param df A data frame or data.table.
#' @param limit Integer. Maximum number of characters to keep (default: 500).
#' @param suffix Character. Text appended to truncated values
#'   (default: `" \u2026[TRUNC]"`). Set to `NULL` or `""` to omit.
#'
#' @return The input `df` with long strings truncated. Returns invisibly.
#' @export
#'
#' @examples
#' df <- data.frame(x = paste(rep("word", 200), collapse = " "))
#' result <- truncate_df_chars(df, limit = 50)
truncate_df_chars <- function(df, limit = 500L, suffix = " \u2026[TRUNC]") {
  DT        <- data.table::as.data.table(df)
  char_cols <- names(DT)[vapply(DT, is.character, logical(1))]

  if (!length(char_cols)) {
    message("No character columns to truncate.")
    return(invisible(DT))
  }

  suffix     <- if (is.null(suffix)) "" else as.character(suffix[1L])
  suffix_len <- stringi::stri_length(suffix)
  keep       <- max(0L, as.integer(limit) - suffix_len)

  for (col in char_cols) {
    x    <- DT[[col]]
    n    <- stringi::stri_length(x)
    over <- !is.na(n) & n > limit

    if (any(over)) {
      head_part <- if (keep > 0L) stringi::stri_sub(x[over], 1L, keep) else rep("", sum(over))
      truncated <- if (suffix_len > 0L) paste0(head_part, suffix) else head_part
      data.table::set(DT, i = which(over), j = col, value = as.character(truncated))
    }
  }

  invisible(DT)
}

#' Fetch All Validation Rules from DHIS2
#'
#' Downloads every validation rule from a DHIS2 instance, handling paged
#' responses automatically, and translates UID references in the left- and
#' right-side expressions to human-readable names.
#'
#' @param baseurl Character. Base URL of the DHIS2 instance (with trailing `/`).
#' @param username Character. DHIS2 username.
#' @param password Character. DHIS2 password.
#' @param id_names A data frame with columns `id` and `name` used to translate
#'   UID references in expressions. Typically a combined table of data elements,
#'   indicators, and category option combos. Pass `NULL` to skip translation.
#'
#' @return A tibble with one row per validation rule and columns:
#'   `id`, `name`, `description`, `importance`, `operator`, `ruleType`,
#'   `periodType`, `leftSide_description`, `leftSide_expression` (translated),
#'   `rightSide_description`, `rightSide_expression` (translated),
#'   `leftSide_expression_raw`, `rightSide_expression_raw` (original UIDs,
#'   used internally for element-to-rule matching).
#' @export
fetch_validation_rules <- function(baseurl, username, password, id_names = NULL) {

  fields <- paste(
    "id", "name", "description", "importance", "operator",
    "ruleType", "periodType",
    "leftSide[expression,description]",
    "rightSide[expression,description]",
    sep = ","
  )

  page_size <- 100L
  page      <- 1L
  all_rules <- list()

  repeat {
    url <- paste0(
      baseurl,
      "api/validationRules.json?fields=", fields,
      "&pageSize=", page_size,
      "&page=",     page
    )

    resp <- dhis2_get(source_url = url, username = username, password = password)

    if (is.null(resp)) {
      message("fetch_validation_rules: no response from server")
      break
    }

    rules_page <- resp$validationRules
    if (is.null(rules_page) || (is.data.frame(rules_page) && nrow(rules_page) == 0)) break

    all_rules[[length(all_rules) + 1L]] <- rules_page

    total_pages <- resp$pager$pageCount
    cat("\n - validation rules page", page, "of", total_pages)
    if (is.null(total_pages) || page >= total_pages) break
    page <- page + 1L
  }

  empty_tbl <- tibble::tibble(
    id = character(), name = character(), description = character(),
    importance = character(), operator = character(),
    ruleType = character(), periodType = character(),
    leftSide_description = character(), leftSide_expression = character(),
    rightSide_description = character(), rightSide_expression = character(),
    leftSide_expression_raw = character(), rightSide_expression_raw = character()
  )

  if (length(all_rules) == 0) return(empty_tbl)

  combined <- dplyr::bind_rows(all_rules)

  # leftSide / rightSide come as nested data-frame columns from jsonlite::fromJSON
  ls_expr <- if (!is.null(combined$leftSide))  combined$leftSide$expression  else rep(NA_character_, nrow(combined))
  ls_desc <- if (!is.null(combined$leftSide))  combined$leftSide$description else rep(NA_character_, nrow(combined))
  rs_expr <- if (!is.null(combined$rightSide)) combined$rightSide$expression else rep(NA_character_, nrow(combined))
  rs_desc <- if (!is.null(combined$rightSide)) combined$rightSide$description else rep(NA_character_, nrow(combined))

  # Translate UID references to human-readable names if lookup provided
  if (!is.null(id_names) && nrow(id_names) > 0) {
    ls_expr_tr <- vapply(ls_expr, .translate_vr_expression, character(1L), lookup = id_names)
    rs_expr_tr <- vapply(rs_expr, .translate_vr_expression, character(1L), lookup = id_names)
  } else {
    ls_expr_tr <- ls_expr
    rs_expr_tr <- rs_expr
  }

  tibble::tibble(
    id                       = combined$id,
    name                     = combined$name,
    description              = if ("description" %in% names(combined)) combined$description else NA_character_,
    importance               = if ("importance"  %in% names(combined)) combined$importance  else NA_character_,
    operator                 = if ("operator"    %in% names(combined)) combined$operator    else NA_character_,
    ruleType                 = if ("ruleType"    %in% names(combined)) combined$ruleType    else NA_character_,
    periodType               = if ("periodType"  %in% names(combined)) combined$periodType  else NA_character_,
    leftSide_description     = ls_desc,
    leftSide_expression      = ls_expr_tr,
    rightSide_description    = rs_desc,
    rightSide_expression     = rs_expr_tr,
    leftSide_expression_raw  = ls_expr,
    rightSide_expression_raw = rs_expr
  )
}

# Internal: replace DHIS2 UID references in an expression string with
# human-readable names. Handles #{uid}, #{uid.uid}, and I{uid} patterns.
# DHIS2 UIDs are 11-character alphanumeric strings starting with a letter.
.translate_vr_expression <- function(expr, lookup) {
  if (is.null(expr) || is.na(expr) || !nzchar(expr)) return(as.character(expr))
  uid_pattern <- "[A-Za-z][A-Za-z0-9]{10}"
  uids <- unique(unlist(regmatches(expr, gregexpr(uid_pattern, expr))))
  if (length(uids) == 0) return(expr)
  result <- expr
  for (uid in uids) {
    idx <- match(uid, lookup$id)
    if (!is.na(idx)) {
      result <- gsub(uid, paste0("[", lookup$name[[idx]], "]"), result, fixed = TRUE)
    }
  }
  result
}
