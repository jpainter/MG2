# Moved from R/dqa_functions.R on 2026-11-16: unreachable from the
# package API, the Shiny app, vignettes or tests (call-graph check).
# Not part of the package build (dev/ is .Rbuildignore'd).

#' Extract Unique Org Units from DQA Data
#' @noRd
data_ous <- function(dqa_data) {
  dqa_data |>
    tibble::as_tibble() |>
    dplyr::ungroup() |>
    dplyr::count(orgUnit, orgUnitName, level) |>
    dplyr::select(-n)
}



#' @rdname dqa_swape
#' @noRd
dqa_mase <- dqa_swape   # backwards-compatible alias

#' @rdname dqa_swape_plot
#' @noRd
dqa_mase_plot <- dqa_swape_plot   # backwards-compatible alias
