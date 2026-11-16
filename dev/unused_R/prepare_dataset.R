# Moved from R/prepare_dataset.R on 2026-11-16: unreachable from the
# package API, the Shiny app, vignettes or tests (call-graph check).
# Not part of the package build (dev/ is .Rbuildignore'd).

#' Match Downloaded Data to Formula Elements (simple match)
#'
#' @noRd
translate_dataset <- function(data, formula_elements, .verbose = FALSE) {
  if (.verbose) message("translate_dataset:")

  if (any(is.na(data$categoryOptionCombo))) {
    element_match <- match(
      paste(data$dataElement, data$categoryOptionCombo),
      paste(
        formula_elements$dataElement.id,
        ifelse(
          is.na(formula_elements$n_categoryOptions),
          NA,
          formula_elements$categoryOptionCombo.ids
        )
      )
    )
  } else {
    element_match <- match(
      paste(data$dataElement, data$categoryOptionCombo),
      paste(formula_elements$dataElement.id, formula_elements$categoryOptionCombo.ids)
    )
  }

  formula_data <- formula_elements[element_match, ] |>
    dplyr::select(-dataElement.id, -categoryOptionCombo.ids)

  data |>
    dplyr::rename(
      dataElement.id          = dataElement,
      categoryOptionCombo.ids = categoryOptionCombo
    ) |>
    dplyr::bind_cols(formula_data)
}

