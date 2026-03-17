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
#'   (default: `" …[TRUNC]"`). Set to `NULL` or `""` to omit.
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
