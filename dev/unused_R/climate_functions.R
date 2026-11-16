# Moved from R/climate_functions.R on 2026-11-16: unreachable from the
# package API, the Shiny app, vignettes or tests (call-graph check).
# Not part of the package build (dev/ is .Rbuildignore'd).

#' African country ISO-3 codes for GADM / CHIRPS analysis
#'
#' @return Named character vector mapping country name to ISO-3 code.
#' @noRd
chirps_country_options <- function() {
  c(
    "Angola"                          = "AGO",
    "Benin"                           = "BEN",
    "Botswana"                        = "BWA",
    "Burkina Faso"                    = "BFA",
    "Burundi"                         = "BDI",
    "Cameroon"                        = "CMR",
    "Central African Republic"        = "CAF",
    "Chad"                            = "TCD",
    "Democratic Republic of the Congo"= "COD",
    "Equatorial Guinea"               = "GNQ",
    "Ethiopia"                        = "ETH",
    "Gabon"                           = "GAB",
    "Gambia"                          = "GMB",
    "Ghana"                           = "GHA",
    "Guinea"                          = "GIN",
    "Guinea-Bissau"                   = "GNB",
    "Ivory Coast"                     = "CIV",
    "Kenya"                           = "KEN",
    "Liberia"                         = "LBR",
    "Madagascar"                      = "MDG",
    "Malawi"                          = "MWI",
    "Mali"                            = "MLI",
    "Mauritania"                      = "MRT",
    "Mozambique"                      = "MOZ",
    "Namibia"                         = "NAM",
    "Niger"                           = "NER",
    "Nigeria"                         = "NGA",
    "Republic of the Congo"           = "COG",
    "Rwanda"                          = "RWA",
    "Senegal"                         = "SEN",
    "Sierra Leone"                    = "SLE",
    "South Africa"                    = "ZAF",
    "South Sudan"                     = "SSD",
    "Sudan"                           = "SDN",
    "Tanzania"                        = "TZA",
    "Togo"                            = "TGO",
    "Uganda"                          = "UGA",
    "Zambia"                          = "ZMB",
    "Zimbabwe"                        = "ZWE"
  )
}

#' Download and cache a GADM administrative boundary shapefile
#'
#' Shapefiles are cached as RDS files under `cache_dir` so subsequent calls for
#' the same country/level return immediately without re-downloading.
#'
#' @param country_code ISO-3 code (e.g. `"SLE"`).
#' @param admin_level  Integer 0–4 (0 = country, 1 = regions, …).
#' @param cache_dir    Directory for caching downloaded files.
#' @return An `sf` polygon object.
#' @importFrom httr GET content status_code timeout
#' @importFrom sf st_read st_crs<-
chirps_download_gadm <- function(country_code, admin_level,
                                  cache_dir = tempdir()) {
  if (!requireNamespace("sf",   quietly = TRUE)) stop("Package 'sf' is required.")
  if (!requireNamespace("httr", quietly = TRUE)) stop("Package 'httr' is required.")

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_file <- file.path(cache_dir,
                          paste0("gadm_", country_code, "_", admin_level, ".rds"))
  if (file.exists(cache_file)) return(readRDS(cache_file))

  url <- paste0(
    "https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_",
    country_code, "_shp.zip"
  )

  resp <- httr::GET(url, httr::timeout(180))
  if (httr::status_code(resp) != 200)
    stop("Failed to download GADM shapefile for ", country_code,
         " (HTTP ", httr::status_code(resp), ").")

  tmp_zip <- tempfile(fileext = ".zip")
  writeBin(httr::content(resp, "raw"), tmp_zip)
  on.exit(unlink(tmp_zip), add = TRUE)

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  utils::unzip(tmp_zip, exdir = tmp_dir)

  shp_name <- paste0("gadm41_", country_code, "_", admin_level, ".shp")
  shp_path <- file.path(tmp_dir, shp_name)

  if (!file.exists(shp_path)) {
    available <- list.files(tmp_dir, pattern = "\\.shp$")
    stop("Admin level ", admin_level, " not found for ", country_code,
         ". Available files: ", paste(available, collapse = ", "))
  }

  gdf <- sf::st_read(shp_path, quiet = TRUE)
  if (is.na(sf::st_crs(gdf))) sf::st_crs(gdf) <- 4326

  saveRDS(gdf, cache_file)
  gdf
}

