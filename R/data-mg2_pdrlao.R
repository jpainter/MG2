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
#' @seealso `mg2_pdrlao_meta`, `mg2_pdrlao_processed`, [mg2_pdrlao_setup()]
#' @examples
#' data(mg2_pdrlao_formula)
#' \dontrun{
#' mg2_pdrlao_formula[, c("dataElement", "Categories")]
#' }
"mg2_pdrlao_formula"


#' MG2 PDR Lao Data: Metadata
#'
#' Metadata list for the PDR Lao DHIS2 instance, structured to match the
#' `metadata_YYYY-MM-DD.rds` files saved by the Metadata widget.
#'
#' @format A named list with the same structure as `mg2_demo_meta`:
#' `systemInfo`, `meta_variables`, `orgUnitLevels`, `orgUnits`,
#' `dataElementDictionary`, `indicatorDictionary`, `dataSets.`, `dataSets`,
#' `categories`, `dataElementGroups`, `ousTree`, `geoFeatures`,
#' `validationRules`, `resources`.
#'
#' @seealso `mg2_pdrlao_formula`, `mg2_pdrlao_processed`, [mg2_pdrlao_setup()]
#' @examples
#' data(mg2_pdrlao_meta)
#' names(mg2_pdrlao_meta)
"mg2_pdrlao_meta"


#' MG2 PDR Lao Data: Processed Dataset
#'
#' PDR Lao malaria data pre-processed through [data_1()]: a `tsibble` with
#' org unit hierarchy joined, data element labels resolved, and time index
#' built. Covers approximately 57 months (Aug 2021 to May 2026) across
#' 82 data elements.
#'
#' Shipped pre-built so that [mg2_pdrlao_setup()] can write it to disk
#' instantly without re-running the pipeline.
#'
#' @format A `tsibble` with the same columns as `mg2_demo_processed`:
#' `orgUnit`, `dataElement`, `dataSet`, `orgUnitName`, `level`,
#' admin-level columns, `effectiveLeaf`, `Month`, `data`, `data.id`,
#' `original`, `value`.
#'
#' @seealso `mg2_pdrlao_formula`, `mg2_pdrlao_meta`, [mg2_pdrlao_setup()],
#'   [data_1()]
#' @examples
#' data(mg2_pdrlao_processed)
#' \dontrun{
#' tsibble::n_keys(mg2_pdrlao_processed)
#' }
"mg2_pdrlao_processed"
