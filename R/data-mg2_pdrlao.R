#' MG2 PDR Lao Data: Formula Elements
#'
#' Formula elements table for the PDR Lao malaria dataset shipped with MG2.
#' The format matches the `updated_formula_elements$df` reactive produced by
#' the Formula widget: one row per data element x category option combo,
#' with human-readable names and UIDs.
#'
#' @format A data frame with the same columns as `mg2_demo_formula`:
#' `Formula.Name`, `role`, `dataElement`, `Categories`, `dataElement.id`,
#' `categoryOptionCombo.ids`, `n_categoryOptions`, `categoryCombo`,
#' `categoryCombo.id`, `dataSet`, `dataSet.ids`, `dataElementGroup`,
#' `dataElementGroups.id`, `displayName`, `displayShortName`, `shortName`,
#' `periodType`, `zeroIsSignificant`.
#'
#' @seealso [mg2_pdrlao_setup()], which downloads the matching metadata and
#'   processed dataset.
#' @examples
#' data(mg2_pdrlao_formula)
#' head(mg2_pdrlao_formula[, c("dataElement", "Categories")])
"mg2_pdrlao_formula"

