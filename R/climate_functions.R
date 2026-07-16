# climate_functions.R — CHIRPS satellite rainfall analysis functions for MG2
#
# Inspired by the Python/Streamlit CHIRPS application by Mohamed Sillah Kanu
# (Informatics Consultancy Firm – Sierra Leone; Research Data Analyst Associate,
# Northwestern University).
# Original source: https://github.com/mohamedsillahkanu/blank-app
# Original license: Apache License 2.0
# Copyright: Mohamed Sillah Kanu
#
# This file is a complete rewrite in R (not a translation of the Python source).
# Concepts borrowed from the original work:
#   - Bounding-box–limited raster download (crop to area of interest, not full continent)
#   - Zonal statistics pattern: crop raster to boundary extent, then extract per-polygon means
#   - UI structure: admin-level selector, year/month pickers, tabbed map/stats/download outputs
#
# Modifications and additions:
#   - DHIS2 org unit polygons replace GADM as the primary boundary source
#   - Auto-selection of Africa vs global CHIRPS dataset based on bounding box
#   - Centroid-based fallback extraction for polygons smaller than one CHIRPS cell
#   - Atomic TIF caching (write to .tmp sentinel, rename on success)
#   - on_progress / on_error callbacks for Shiny integration
#
# This file is part of MG2, licensed under GPL-3, which is compatible with Apache-2.0.
# Full Apache-2.0 license text: https://www.apache.org/licenses/LICENSE-2.0
#
# Requires: sf, terra, httr, openxlsx, ggplot2, leaflet (all in Suggests)

# ── Country list ──────────────────────────────────────────────────────────────

#' African country ISO-3 codes for GADM / CHIRPS analysis
#'
#' @return Named character vector mapping country name to ISO-3 code.
#' @export
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

# ── Date validation ───────────────────────────────────────────────────────────

#' Check whether CHIRPS data is available for a given year/month
#'
#' CHIRPS monthly data begins January 1981 and has a ~2-month publication delay.
#'
#' @param year  Integer year.
#' @param month Integer month (1–12).
#' @return List with `valid` (logical) and `message` (character).
chirps_validate_date <- function(year, month) {
  now   <- Sys.Date()
  cy    <- as.integer(format(now, "%Y"))
  cm    <- as.integer(format(now, "%m"))
  if (year < 1981)
    return(list(valid = FALSE, message = "CHIRPS data starts from January 1981."))
  if (year > cy || (year == cy && month > cm - 2))
    return(list(valid = FALSE,
                message = paste0("CHIRPS data has a ~2-month delay. ",
                                 "The selected month is not yet available.")))
  list(valid = TRUE, message = "")
}

# ── Boundary loading ──────────────────────────────────────────────────────────

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

#' Load a custom shapefile from uploaded component file paths
#'
#' Copies the uploaded files to a temporary directory with a consistent base
#' name so that `sf::st_read()` can locate all components. Assumes WGS84 if no
#' CRS is detected and no `.prj` file is supplied.
#'
#' @param shp_path Path to `.shp` file.
#' @param shx_path Path to `.shx` file.
#' @param dbf_path Path to `.dbf` file.
#' @param prj_path Optional path to `.prj` projection file.
#' @return An `sf` polygon object.
#' @importFrom sf st_read st_crs<-
chirps_load_custom_shapefile <- function(shp_path, shx_path, dbf_path,
                                          prj_path = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file.copy(shp_path, file.path(tmp_dir, "upload.shp"))
  file.copy(shx_path, file.path(tmp_dir, "upload.shx"))
  file.copy(dbf_path, file.path(tmp_dir, "upload.dbf"))
  if (!is.null(prj_path) && file.exists(prj_path))
    file.copy(prj_path, file.path(tmp_dir, "upload.prj"))

  gdf <- sf::st_read(file.path(tmp_dir, "upload.shp"), quiet = TRUE)

  if (is.na(sf::st_crs(gdf))) {
    warning("No CRS detected in shapefile. Assuming WGS84 (EPSG:4326).")
    sf::st_crs(gdf) <- 4326
  }
  gdf
}

# ── CHIRPS raster download ────────────────────────────────────────────────────

# Build a short string from a bounding box suitable for use in a filename.
# Coordinates are rounded to 1 decimal place (≈ 11 km) so nearby analyses
# share the same cached crop.
.chirps_bbox_id <- function(bbox) {
  sprintf("x%.1f_%.1f_y%.1f_%.1f",
          floor(bbox[["xmin"]] * 10) / 10,
          ceiling(bbox[["xmax"]] * 10) / 10,
          floor(bbox[["ymin"]] * 10) / 10,
          ceiling(bbox[["ymax"]] * 10) / 10)
}

# Generate N visually distinct qualitative colours for year-fill bars.
# Uses a curated 12-colour base; interpolates via colorRampPalette for N > 12.
.chirps_year_palette <- function(n) {
  base <- c(
    "#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00",
    "#A65628","#F781BF","#666666","#1B9E77","#D95F02",
    "#7570B3","#E7298A"
  )
  if (n <= length(base)) return(base[seq_len(n)])
  grDevices::colorRampPalette(base)(n)
}

# TRUE when the bounding box lies entirely within the CHIRPS Africa domain.
# Africa-only TIFs (~3-5 MB) are preferred over the global ones (~20 MB).
.chirps_in_africa <- function(bbox) {
  bbox[["xmin"]] >= -30 && bbox[["xmax"]] <= 65 &&
  bbox[["ymin"]] >= -40 && bbox[["ymax"]] <= 40
}

#' Download and cache a bbox-cropped CHIRPS monthly GeoTIFF
#'
#' The full continental/global TIF is downloaded to a **temporary** file,
#' cropped immediately to `bbox` (plus a 0.2° buffer), and only the small
#' cropped version is written to `cache_dir`.  This mirrors the windowed-read
#' approach used by the original Python app (`rasterio.mask.mask(crop=True)`)
#' while avoiding permanent storage of the large continental raster.
#'
#' When `bbox` is `NULL` the full Africa TIF is cached (legacy/standalone use).
#'
#' The function auto-selects the Africa-only dataset when the bbox falls within
#' Africa (~3-5 MB download) and the global dataset otherwise (~20 MB), so it
#' works for any country, including non-African DHIS2 instances (e.g. PDR Lao).
#'
#' @param year      Integer year (>= 1981).
#' @param month     Integer month (1–12).
#' @param bbox      Named numeric vector `c(xmin=, ymin=, xmax=, ymax=)` in
#'                  WGS84 degrees, or `NULL` to cache the full Africa TIF.
#' @param cache_dir Directory for caching downloaded files.
#' @return Path to the cached `.tif` file (cropped when `bbox` is supplied).
#' @importFrom httr GET content status_code timeout
chirps_download_tif <- function(year, month, bbox = NULL, cache_dir = tempdir()) {
  v <- chirps_validate_date(year, month)
  if (!v$valid) stop(v$message)

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  base_name <- sprintf("chirps-v2.0.%d.%02d", year, month)

  # Cache key — includes bbox so different regions don't share the same file
  if (!is.null(bbox)) {
    tif_name <- paste0(base_name, "_", .chirps_bbox_id(bbox), ".tif")
  } else {
    tif_name <- paste0(base_name, ".tif")
  }
  tif_path <- file.path(cache_dir, tif_name)
  if (file.exists(tif_path)) return(tif_path)

  # Choose dataset: Africa (~3-5 MB) when bbox is within Africa, global (~20 MB) otherwise
  if (!is.null(bbox) && !.chirps_in_africa(bbox)) {
    gz_url <- paste0("https://data.chc.ucsb.edu/products/CHIRPS-2.0/",
                     "global_monthly/tifs/", base_name, ".tif.gz")
    max_bytes <- 500e6   # global TIF can be ~100 MB uncompressed
  } else {
    gz_url <- paste0("https://data.chc.ucsb.edu/products/CHIRPS-2.0/",
                     "africa_monthly/tifs/", base_name, ".tif.gz")
    max_bytes <- 300e6
  }

  resp <- httr::GET(gz_url, httr::timeout(300))
  if (httr::status_code(resp) != 200)
    stop("CHIRPS download failed for ", sprintf("%d-%02d", year, month),
         " (HTTP ", httr::status_code(resp), ").")

  # Decompress to a temp TIF — the large continental file is NEVER cached
  tmp_gz  <- tempfile(fileext = ".tif.gz")
  tmp_tif <- tempfile(fileext = ".tif")
  tif_tmp <- paste0(tif_path, ".tmp")    # atomic write sentinel
  on.exit({
    unlink(tmp_gz,  force = TRUE)
    unlink(tmp_tif, force = TRUE)
    unlink(tif_tmp, force = TRUE)
  }, add = TRUE)

  writeBin(httr::content(resp, "raw"), tmp_gz)

  gz_con    <- gzfile(tmp_gz, "rb")
  raw_bytes <- readBin(gz_con, what = "raw", n = max_bytes)
  close(gz_con)

  if (length(raw_bytes) == 0)
    stop("Decompression produced empty output for ",
         sprintf("%d-%02d", year, month), ". The download may be corrupt.")

  writeBin(raw_bytes, tmp_tif)

  if (!is.null(bbox)) {
    # Crop to bbox + buffer and save only the small region
    if (!requireNamespace("terra", quietly = TRUE))
      stop("Package 'terra' is required.")
    buf    <- 0.2
    r_full <- terra::rast(tmp_tif)
    ext    <- terra::ext(
      bbox[["xmin"]] - buf, bbox[["xmax"]] + buf,
      bbox[["ymin"]] - buf, bbox[["ymax"]] + buf
    )
    r_crop <- terra::crop(r_full, ext)
    terra::writeRaster(r_crop, tif_tmp, filetype = "GTiff", overwrite = TRUE)
  } else {
    file.copy(tmp_tif, tif_tmp)
  }

  file.rename(tif_tmp, tif_path)   # atomic: only appears on disk when complete
  tif_path
}

# ── Raster extraction ─────────────────────────────────────────────────────────

#' Extract mean CHIRPS rainfall for each polygon in an sf object
#'
#' Mirrors Python's `rasterio.mask.mask(src, [geom], crop=True)`: the raster
#' is first cropped to the sf bounding box so that only the relevant portion
#' is held in memory, then per-polygon zonal statistics are computed.
#'
#' @param sf_obj   An `sf` polygon object.
#' @param tif_path Path to a CHIRPS monthly GeoTIFF (may already be cropped).
#' @return `sf_obj` with added columns `mean_rain` (mm) and `valid_pixels`.
#' @importFrom sf st_transform st_bbox
chirps_extract_rainfall <- function(sf_obj, tif_path) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required for raster extraction.")
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Package 'sf' is required.")

  r <- terra::rast(tif_path)

  # Reproject sf to match raster CRS
  rast_crs  <- terra::crs(r)
  sf_reproj <- sf::st_transform(sf_obj, crs = rast_crs)

  # Crop raster to sf extent — mirrors rasterio crop=True; no-op if TIF is
  # already cropped by chirps_download_tif(), ensures only needed pixels are
  # loaded for extraction.
  bbox_r <- sf::st_bbox(sf_reproj)
  r      <- terra::crop(r, terra::ext(
    bbox_r[["xmin"]], bbox_r[["xmax"]],
    bbox_r[["ymin"]], bbox_r[["ymax"]]
  ))

  # Mask CHIRPS nodata value (-9999) AFTER crop (smaller array → faster)
  r[r < -9000] <- NA

  vect_obj <- terra::vect(sf_reproj)

  # Per-polygon mean and valid pixel count.
  # Use column-position index (not name) because terra names the extract column
  # after the raster layer — e.g. "chirps-v2.0.2005.01" — and R's data.frame
  # rules may mangle the hyphens to dots.
  mean_df  <- terra::extract(r, vect_obj, fun = function(x) mean(x, na.rm = TRUE),
                              bind = FALSE)
  count_df <- terra::extract(r, vect_obj, fun = function(x) sum(!is.na(x)),
                              bind = FALSE)

  mean_col  <- setdiff(names(mean_df),  "ID")[1]
  count_col <- setdiff(names(count_df), "ID")[1]
  sf_obj$mean_rain    <- mean_df[[mean_col]]
  sf_obj$valid_pixels <- as.integer(count_df[[count_col]])

  # Fallback for polygons smaller than one CHIRPS cell (valid_pixels == 0):
  # extract at the polygon centroid instead. Flagged as fill_method = "centroid".
  na_idx <- which(is.na(sf_obj$mean_rain) | sf_obj$valid_pixels == 0)
  sf_obj$fill_method <- "zonal"
  if (length(na_idx) > 0) {
    centroids   <- sf::st_centroid(sf_reproj[na_idx, ])
    cent_vect   <- terra::vect(centroids)
    cent_vals   <- terra::extract(r, cent_vect)
    cent_col    <- setdiff(names(cent_vals), "ID")[1]
    sf_obj$mean_rain[na_idx]    <- cent_vals[[cent_col]]
    sf_obj$valid_pixels[na_idx] <- 0L   # keep 0 so downstream knows
    sf_obj$fill_method[na_idx]  <- "centroid"
  }

  # Always return WGS84 so leaflet and downstream consumers don't need to reproject
  sf::st_transform(sf_obj, 4326)
}

# ── Multi-month processing ────────────────────────────────────────────────────

#' Process CHIRPS rainfall for multiple months
#'
#' Computes the WGS84 bounding box of `sf_obj` once and passes it to
#' `chirps_download_tif()` so that only the cropped region is downloaded and
#' cached — not the full continental raster.
#'
#' @param sf_obj      An `sf` polygon object.
#' @param year        Integer year.
#' @param months      Integer vector of month numbers (1–12).
#' @param cache_dir   Cache directory for downloaded CHIRPS files.
#' @param on_progress Optional `function(i, n, msg)` called before each month.
#' @param on_error    Optional `function(msg)` called when a month fails.
#' @return Named list of `sf` objects, one per successfully processed month
#'   (names are three-letter month abbreviations, e.g. `"Jan"`, `"Feb"`).
chirps_process_months <- function(sf_obj, year, months,
                                   cache_dir   = tempdir(),
                                   on_progress = NULL,
                                   on_error    = NULL) {
  mon_names <- c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")

  # Compute WGS84 bbox once — passed to every monthly download so only the
  # area of interest is fetched and cached (not the full continental raster).
  bbox_wgs84 <- tryCatch({
    bb <- sf::st_bbox(sf::st_transform(sf_obj, 4326))
    c(xmin = unname(bb["xmin"]), ymin = unname(bb["ymin"]),
      xmax = unname(bb["xmax"]), ymax = unname(bb["ymax"]))
  }, error = function(e) {
    warning("Could not compute bounding box; downloading full Africa TIF: ",
            conditionMessage(e))
    NULL
  })

  results <- list()
  n <- length(months)

  for (i in seq_along(months)) {
    m <- months[i]

    # Check TIF cache to show informative progress ("Loading" vs "Downloading")
    tif_cached <- tryCatch({
      base_name <- sprintf("chirps-v2.0.%d.%02d", year, m)
      tif_id    <- if (!is.null(bbox_wgs84))
                     paste0(base_name, "_", .chirps_bbox_id(bbox_wgs84), ".tif")
                   else
                     paste0(base_name, ".tif")
      file.exists(file.path(cache_dir, tif_id))
    }, error = function(e) FALSE)

    if (!is.null(on_progress))
      on_progress(i, n,
                  paste(if (tif_cached) "Extracting" else "Downloading",
                        mon_names[m], year))

    tryCatch({
      tif_path <- chirps_download_tif(year, m,
                                       bbox      = bbox_wgs84,
                                       cache_dir = cache_dir)
      sf_out   <- chirps_extract_rainfall(sf_obj, tif_path)
      results[[mon_names[m]]] <- sf_out
    }, error = function(e) {
      msg <- paste0("Skipped ", mon_names[m], " ", year, ": ", conditionMessage(e))
      message(msg)
      if (!is.null(on_error)) on_error(msg)
    })
  }

  Filter(Negate(is.null), results)
}

#' Build a cache filename for a CHIRPS results RDS file
#'
#' The key encodes year, months, admin level, and bounding box so that
#' different queries never share the same cached result.
#'
#' @param sf_obj    An `sf` polygon object (used to compute the bbox).
#' @param year      Integer year.
#' @param months    Integer vector of months (1–12).
#' @param level_str Character identifier for the admin level (e.g. `"2"` or
#'   `"custom"`).
#' @return A filename string (not a full path) ending in `.rds`.
chirps_results_cache_key <- function(sf_obj, year, months, level_str = "custom") {
  bbox <- tryCatch({
    bb <- sf::st_bbox(sf::st_transform(sf_obj, 4326))
    c(xmin = unname(bb["xmin"]), ymin = unname(bb["ymin"]),
      xmax = unname(bb["xmax"]), ymax = unname(bb["ymax"]))
  }, error = function(e) NULL)

  bbox_part  <- if (!is.null(bbox)) .chirps_bbox_id(bbox) else "nobbox"
  months_str <- paste(sort(as.integer(months)), collapse = "-")
  sprintf("results_%d_%s_lvl%s_%s.rds", year, months_str, level_str, bbox_part)
}

# ── Visualisation ─────────────────────────────────────────────────────────────

#' Build a ggplot2 faceted choropleth of monthly CHIRPS rainfall
#'
#' @param results_list Named list from `chirps_process_months()`.
#' @param title_prefix Character prefix for the plot title (e.g. country name).
#' @param year         Integer year shown in the title.
#' @param palette      Colour palette: `"Blues"`, `"YlOrRd"` (ColorBrewer) or
#'                     `"viridis"`, `"plasma"` (viridis).
#' @return A `ggplot` object.
#' @importFrom ggplot2 ggplot aes geom_sf facet_wrap scale_fill_distiller
#'   scale_fill_viridis_c theme_void labs theme element_text
chirps_make_map <- function(results_list, title_prefix = "", year,
                             palette = "Blues") {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for map rendering.")

  dfs <- lapply(names(results_list), function(mon) {
    d <- results_list[[mon]]
    d$month_label <- mon
    d
  })
  combined <- do.call(rbind, dfs)
  combined$month_label <- factor(combined$month_label, levels = names(results_list))

  all_vals <- combined$mean_rain[!is.na(combined$mean_rain)]
  if (length(all_vals) == 0) { vmin <- 0; vmax <- 100 } else {
    vmin <- stats::quantile(all_vals, 0.05)
    vmax <- stats::quantile(all_vals, 0.95)
  }

  viridis_options <- c("viridis" = "D", "plasma" = "C",
                       "inferno" = "B", "magma"  = "A", "cividis" = "E")

  p <- ggplot2::ggplot(combined) +
    ggplot2::geom_sf(ggplot2::aes(fill = mean_rain),
                     color = "white", linewidth = 0.3) +
    {
      # Outline centroid-imputed polygons in orange so they're visually distinct
      if ("fill_method" %in% names(combined)) {
        cent <- combined[!is.na(combined$fill_method) &
                           combined$fill_method == "centroid", ]
        if (nrow(cent) > 0)
          ggplot2::geom_sf(data = cent, fill = NA,
                           color = "orange", linewidth = 0.6)
      }
    } +
    ggplot2::facet_wrap(~ month_label) +
    ggplot2::labs(
      title = if (nzchar(title_prefix))
                paste0(title_prefix, " ", year, " — Monthly Rainfall (CHIRPS)")
              else
                paste0(year, " Monthly Rainfall (CHIRPS)"),
      fill  = "Rainfall\n(mm)"
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "right",
      strip.text      = ggplot2::element_text(face = "bold", size = 9)
    )

  if (palette %in% names(viridis_options)) {
    p <- p + ggplot2::scale_fill_viridis_c(
      option   = viridis_options[[palette]],
      na.value = "grey80",
      limits   = c(vmin, vmax),
      oob      = scales::squish
    )
  } else {
    p <- p + ggplot2::scale_fill_distiller(
      palette   = palette,
      direction = 1,
      na.value  = "grey80",
      limits    = c(vmin, vmax),
      oob       = scales::squish
    )
  }

  p
}

#' Build a ggplot2 bar chart of monthly CHIRPS rainfall
#'
#' Produces a bar chart with mean rainfall per month (averaged across all
#' polygons in `results_list`). Error bars show ±1 SD across polygons.
#' When `group_by_parent` is `TRUE` and a `parentName` column is present with
#' ≤ 8 unique values, bars are coloured and dodged by parent unit — useful for
#' showing regional rainfall variation within a country.
#'
#' @param results_list   Named list from `chirps_process_months()`.
#' @param year           Integer year shown in the title.
#' @param title_prefix   Character prefix for the chart title (e.g. country name
#'   and admin level).
#' @param group_by_parent Logical. When `TRUE` and `parentName` is available,
#'   colour and dodge bars by parent org unit. Silently ignored if `parentName`
#'   is absent or has > 8 unique values.
#' @return A `ggplot` object.
#' @importFrom ggplot2 ggplot aes geom_col geom_errorbar position_dodge
#'   scale_fill_brewer scale_fill_manual theme_minimal theme element_blank
#'   element_text labs
chirps_make_chart <- function(results_list, year,
                               title_prefix    = "",
                               group_by_parent = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for chart rendering.")

  mon_order <- c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")

  # Combine all months into one flat data frame (no geometry)
  dfs <- lapply(names(results_list), function(mon) {
    d <- as.data.frame(results_list[[mon]])
    d$geometry    <- NULL
    d$month_label <- mon
    d
  })
  combined <- do.call(rbind, dfs)
  combined$month_label <- factor(combined$month_label,
                                  levels = intersect(mon_order, names(results_list)))

  # Decide whether to group by parent
  has_parent  <- "parentName" %in% names(combined) &&
                  any(!is.na(combined$parentName))
  n_parents   <- if (has_parent) length(unique(stats::na.omit(combined$parentName))) else 0L
  use_groups  <- group_by_parent && has_parent && n_parents >= 2L && n_parents <= 8L

  title_str <- if (nzchar(title_prefix))
    paste0(title_prefix, " ", year, " \u2014 Monthly Rainfall (CHIRPS)")
  else
    paste0(year, " Monthly Rainfall (CHIRPS)")

  if (use_groups) {
    # One bar per parent per month, dodged
    chart_df <- stats::aggregate(
      mean_rain ~ month_label + parentName,
      data = combined[!is.na(combined$mean_rain), ],
      FUN  = mean
    )
    chart_df$month_label <- factor(chart_df$month_label,
                                    levels = intersect(mon_order, names(results_list)))

    p <- ggplot2::ggplot(
      chart_df,
      ggplot2::aes(x = month_label, y = mean_rain, fill = parentName)
    ) +
      ggplot2::geom_col(
        position = ggplot2::position_dodge(width = 0.8),
        alpha    = 0.85, width = 0.75
      ) +
      ggplot2::scale_fill_brewer(palette = "Set2") +
      ggplot2::labs(
        x       = NULL,
        y       = "Mean rainfall (mm)",
        fill    = NULL,
        title   = title_str,
        caption = "Bars show mean rainfall across org units within each parent"
      )

  } else {
    # National mean ± 1 SD
    vals_by_month <- split(combined$mean_rain, combined$month_label)
    chart_df <- data.frame(
      month_label = names(vals_by_month),
      mean_mm     = sapply(vals_by_month, mean, na.rm = TRUE),
      sd_mm       = sapply(vals_by_month, stats::sd, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    chart_df$month_label <- factor(chart_df$month_label,
                                    levels = intersect(mon_order, names(results_list)))
    chart_df$ymin <- pmax(0, chart_df$mean_mm - chart_df$sd_mm)
    chart_df$ymax <- chart_df$mean_mm + chart_df$sd_mm

    p <- ggplot2::ggplot(chart_df, ggplot2::aes(x = month_label, y = mean_mm)) +
      ggplot2::geom_col(fill = "#5b9bd5", alpha = 0.85) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = ymin, ymax = ymax),
        width  = 0.35,
        colour = "#333333"
      ) +
      ggplot2::labs(
        x       = NULL,
        y       = "Mean rainfall (mm)",
        title   = title_str,
        caption = "Bars = mean across org units; error bars = \u00b11 SD"
      )
  }

  p +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(size = 11),
      plot.title         = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.caption       = ggplot2::element_text(colour = "#666666", size = 9),
      legend.position    = "bottom"
    )
}

# ── Multi-year helpers ────────────────────────────────────────────────────────

#' Compute per-polygon annual mean rainfall from a single year's results list
#'
#' Averages `mean_rain` across all months in `data_list`, keeping the geometry
#' and DHIS2 attribute columns from the first month's `sf` object.
#'
#' @param data_list Named list of `sf` objects from `chirps_process_months()`.
#' @return An `sf` object with `mean_rain` set to the cross-month mean per
#'   polygon, or `NULL` if `data_list` is empty.
chirps_annual_means_sf <- function(data_list) {
  if (length(data_list) == 0) return(NULL)
  sf_ref   <- data_list[[1]]
  rain_mat <- vapply(data_list, function(s) s$mean_rain, numeric(nrow(sf_ref)))
  sf_ref$mean_rain <- rowMeans(rain_mat, na.rm = TRUE)
  sf_ref
}

#' Build a faceted ggplot2 choropleth of annual mean rainfall across years
#'
#' One map panel per year, showing the per-polygon mean of the selected months.
#'
#' @param multi_results Named list (year → `data_list`) from the year-range run.
#' @param title_prefix  Character prefix for the plot title.
#' @param months        Integer vector of selected months (used in subtitle only).
#' @param palette       Colour palette name (same options as `chirps_make_map()`).
#' @return A `ggplot` object.
chirps_multi_year_map <- function(multi_results, title_prefix = "",
                                   months = NULL, palette = "Blues") {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.")

  mon_names <- c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")

  year_sfs <- lapply(names(multi_results), function(yr) {
    s <- chirps_annual_means_sf(multi_results[[yr]])
    if (is.null(s)) return(NULL)
    s$year_lbl <- yr
    s
  })
  year_sfs <- Filter(Negate(is.null), year_sfs)
  if (length(year_sfs) == 0) stop("No results to map.")

  combined           <- do.call(rbind, year_sfs)
  combined$year_lbl  <- factor(combined$year_lbl,
                                levels = sort(unique(combined$year_lbl)))

  all_vals <- combined$mean_rain[!is.na(combined$mean_rain)]
  vmin <- if (length(all_vals) > 0) stats::quantile(all_vals, 0.05) else 0
  vmax <- if (length(all_vals) > 0) stats::quantile(all_vals, 0.95) else 100

  mon_str <- if (is.null(months) || length(months) == 12)
    "all months"
  else
    paste(mon_names[months], collapse = "+")

  title_str <- if (nzchar(title_prefix))
    paste0(title_prefix, " \u2014 Annual Mean Rainfall")
  else
    "Annual Mean Rainfall"

  caption_str <- paste0("Source: CHIRPS | ", mon_str)

  viridis_opts <- c(viridis="D", plasma="C", inferno="B", magma="A", cividis="E")

  # Tooltip: name + rainfall
  label_col <- if ("name" %in% names(combined)) "name" else {
    nc <- sort(grep("^NAME_", names(combined), value = TRUE))
    if (length(nc)) tail(nc, 1) else NULL
  }
  combined$tooltip <- if (!is.null(label_col))
    paste0("<b>", combined[[label_col]], "</b><br>",
           round(combined$mean_rain, 1), " mm")
  else
    paste0(round(combined$mean_rain, 1), " mm")
  combined$data_id <- if (!is.null(label_col))
    paste0(combined$year_lbl, "_", combined[[label_col]])
  else
    seq_len(nrow(combined))

  n_rows <- ceiling(length(unique(combined$year_lbl)) / 3)

  p <- ggplot2::ggplot(combined) +
    ggiraph::geom_sf_interactive(
      ggplot2::aes(fill = mean_rain, tooltip = tooltip, data_id = data_id),
      color = "white", linewidth = 0.3
    ) +
    ggplot2::facet_wrap(~ year_lbl, ncol = 3) +
    ggplot2::labs(title = title_str, fill = "Rainfall\n(mm)", caption = caption_str) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      legend.position = "right"
    )

  p <- if (palette %in% names(viridis_opts)) {
    p + ggplot2::scale_fill_viridis_c(option = viridis_opts[[palette]],
                                       na.value = "grey80",
                                       limits = c(vmin, vmax), oob = scales::squish)
  } else {
    p + ggplot2::scale_fill_distiller(palette = palette, direction = 1,
                                       na.value = "grey80",
                                       limits = c(vmin, vmax), oob = scales::squish)
  }

  ggiraph::girafe(
    ggobj      = p,
    width_svg  = 10,
    height_svg = n_rows * 3 + 1.5,
    options    = list(
      ggiraph::opts_hover(css = "stroke:orange; stroke-width:1.5px;"),
      ggiraph::opts_tooltip(css = "background:#fff; border:1px solid #ccc;
        padding:6px 8px; border-radius:4px; font-size:13px;"),
      ggiraph::opts_sizing(rescale = FALSE, width = 0.95)
    )
  )
}

#' Build a ggplot2 bar chart comparing rainfall across multiple years
#'
#' Two views selectable via `view`:
#' \describe{
#'   \item{`"annual"`}{One bar per year — mean across org units and selected
#'     months. Error bars = ±1 SD across org units.}
#'   \item{`"monthly"`}{Grouped bars by month, coloured by year — shows how
#'     the seasonal pattern compares across years.}
#' }
#'
#' @param multi_results Named list (year → `data_list`).
#' @param title_prefix  Character prefix for the plot title.
#' @param view          `"annual"` (default) or `"monthly"`.
#' @return A `ggplot` object.
chirps_multi_year_chart <- function(multi_results, title_prefix = "",
                                    view = c("annual", "monthly")) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.")

  view      <- match.arg(view)
  mon_order <- c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")
  years     <- sort(names(multi_results))

  if (view == "annual") {
    rows <- lapply(years, function(yr) {
      dl   <- multi_results[[yr]]
      vals <- unlist(lapply(dl, function(s) s$mean_rain), use.names = FALSE)
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) return(NULL)
      data.frame(year    = yr,
                 mean_mm = mean(vals),
                 stringsAsFactors = FALSE)
    })
    df      <- do.call(rbind, Filter(Negate(is.null), rows))
    df$year <- factor(df$year, levels = years)

    # Multi-year mean ± SD across years (the reference band)
    ref_mean <- mean(df$mean_mm, na.rm = TRUE)
    ref_sd   <- if (nrow(df) > 1) stats::sd(df$mean_mm, na.rm = TRUE) else 0
    ref_ymin <- max(0, ref_mean - ref_sd)
    ref_ymax <- ref_mean + ref_sd
    n_yrs    <- nrow(df)

    title_str <- if (nzchar(title_prefix))
      paste0(title_prefix, " \u2014 Annual Mean Rainfall (CHIRPS)")
    else "Annual Mean Rainfall (CHIRPS)"

    ref_ymin2 <- max(0, ref_mean - 2 * ref_sd)
    ref_ymax2 <- ref_mean + 2 * ref_sd

    p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = mean_mm)) +
      # ±2 SD band (darker grey, drawn first so ±1 SD renders on top)
      ggplot2::annotate("rect",
        xmin = -Inf, xmax = Inf, ymin = ref_ymin2, ymax = ref_ymax2,
        fill = "#888888", alpha = 0.18) +
      # ±1 SD band (lighter grey)
      ggplot2::annotate("rect",
        xmin = -Inf, xmax = Inf, ymin = ref_ymin, ymax = ref_ymax,
        fill = "#aaaaaa", alpha = 0.28) +
      # multi-year mean line
      ggplot2::geom_hline(yintercept = ref_mean,
                          colour = "#555555", linetype = "dashed", linewidth = 0.6) +
      ggplot2::geom_col(fill = "#5b9bd5", alpha = 0.85) +
      ggplot2::labs(x = NULL, y = "Mean rainfall (mm)", title = title_str,
                    caption = paste0(
                      "Bars = mean across org units and selected months  \u2022  ",
                      "Dashed line = ", n_yrs, "-year mean  \u2022  ",
                      "Bands = \u00b11 SD (light) and \u00b12 SD (dark) across years"))

  } else {
    rows <- lapply(years, function(yr) {
      dl <- multi_results[[yr]]
      lapply(names(dl), function(mon) {
        vals <- dl[[mon]]$mean_rain
        data.frame(year = yr, month = mon,
                   mean_mm = mean(vals, na.rm = TRUE),
                   stringsAsFactors = FALSE)
      })
    })
    df       <- do.call(rbind, do.call(c, rows))
    df$month <- factor(df$month, levels = intersect(mon_order, unique(df$month)))
    df$year  <- factor(df$year, levels = years)

    # Multi-year mean ± SD per calendar month (across years, not org units)
    ref <- stats::aggregate(mean_mm ~ month, data = df, FUN = function(x) {
      c(mean = mean(x, na.rm = TRUE), sd = if (length(x) > 1) stats::sd(x) else 0)
    })
    ref <- data.frame(
      month   = ref$month,
      ref_mean = ref$mean_mm[, "mean"],
      ref_sd   = ref$mean_mm[, "sd"]
    )
    ref$ymin  <- pmax(0, ref$ref_mean - ref$ref_sd)
    ref$ymax  <- ref$ref_mean + ref$ref_sd
    ref$ymin2 <- pmax(0, ref$ref_mean - 2 * ref$ref_sd)
    ref$ymax2 <- ref$ref_mean + 2 * ref$ref_sd

    title_str <- if (nzchar(title_prefix))
      paste0(title_prefix, " \u2014 Monthly Rainfall by Year (CHIRPS)")
    else "Monthly Rainfall by Year (CHIRPS)"

    n_yrs <- length(years)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = month, y = mean_mm, fill = year)) +
      # ±2 SD ribbon (darker grey, drawn first)
      ggplot2::geom_ribbon(
        data = ref,
        ggplot2::aes(x = as.integer(month), ymin = ymin2, ymax = ymax2),
        inherit.aes = FALSE,
        fill = "#888888", alpha = 0.18
      ) +
      # ±1 SD ribbon (lighter grey, on top)
      ggplot2::geom_ribbon(
        data = ref,
        ggplot2::aes(x = as.integer(month), ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        fill = "#aaaaaa", alpha = 0.28
      ) +
      # dashed reference line: multi-year monthly mean
      ggplot2::geom_line(
        data = ref,
        ggplot2::aes(x = as.integer(month), y = ref_mean),
        inherit.aes = FALSE,
        colour = "#555555", linetype = "dashed", linewidth = 0.6
      ) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                        alpha = 0.85, width = 0.75) +
      ggplot2::scale_x_discrete() +
      ggplot2::scale_fill_manual(values = .chirps_year_palette(length(years)),
                                  breaks = years) +
      ggplot2::labs(x = NULL, y = "Mean rainfall (mm)", fill = "Year",
                    title = title_str,
                    caption = paste0(
                      "Bars = mean across org units per year  \u2022  ",
                      "Dashed line = ", n_yrs, "-year monthly mean  \u2022  ",
                      "Bands = \u00b11 SD (light) and \u00b12 SD (dark) across years"))
  }

  p + ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(size = 11),
      plot.title         = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.caption       = ggplot2::element_text(colour = "#666", size = 9),
      legend.position    = "bottom"
    )
}

#' Build a faceted anomaly map showing each year's deviation from the multi-year mean
#'
#' For each polygon, computes the mean rainfall across all years (the baseline),
#' then maps each year's deviation from that baseline.  Requires at least 2 years.
#'
#' @param multi_results Named list (year → `data_list`).
#' @param title_prefix  Character prefix for the plot title.
#' @param method        `"mm"` (deviation in millimetres, default) or `"zscore"`
#'   (deviation divided by the multi-year SD — dimensionless).
#' @return A `ggplot` object with a diverging palette centred at zero.
chirps_anomaly_map <- function(multi_results, title_prefix = "",
                                method = c("mm", "zscore")) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.")

  method <- match.arg(method)
  if (length(multi_results) < 2)
    stop("Anomaly map requires at least 2 years of data.")

  year_sfs <- lapply(names(multi_results), function(yr) {
    s <- chirps_annual_means_sf(multi_results[[yr]])
    if (is.null(s)) return(NULL)
    s$year_lbl <- yr
    s
  })
  year_sfs <- Filter(Negate(is.null), year_sfs)
  n_polys  <- nrow(year_sfs[[1]])

  # Per-polygon multi-year mean and SD
  rain_mat   <- vapply(year_sfs, function(s) s$mean_rain, numeric(n_polys))
  multi_mean <- rowMeans(rain_mat, na.rm = TRUE)
  multi_sd   <- apply(rain_mat, 1, stats::sd, na.rm = TRUE)

  # Compute anomaly and attach to each year's sf
  anomaly_sfs <- lapply(year_sfs, function(s) {
    s$anomaly    <- s$mean_rain - multi_mean
    s$anomaly_z  <- ifelse(multi_sd > 0, s$anomaly / multi_sd, 0)
    s$fill_val   <- if (method == "zscore") s$anomaly_z else s$anomaly
    s
  })

  combined          <- do.call(rbind, anomaly_sfs)
  combined$year_lbl <- factor(combined$year_lbl,
                               levels = sort(unique(combined$year_lbl)))

  n_years   <- length(year_sfs)
  anom_name <- if (method == "zscore") "Anomaly\n(SD units)" else "Anomaly\n(mm)"

  # Z-score: fixed symmetric scale -3 to 3 so small anomalies don't look extreme.
  # mm:      data-driven symmetric scale rounded to one decimal.
  if (method == "zscore") {
    scale_lim    <- c(-3, 3)
    scale_breaks <- seq(-3, 3, by = 0.5)
  } else {
    lim       <- max(abs(combined$fill_val), na.rm = TRUE)
    lim       <- ceiling(lim * 10) / 10
    scale_lim <- c(-lim, lim)
    scale_breaks <- ggplot2::waiver()
  }

  title_str <- if (nzchar(title_prefix))
    paste0(title_prefix, " \u2014 Rainfall Anomaly vs ", n_years, "-year Mean")
  else
    paste0("Rainfall Anomaly vs ", n_years, "-year Mean")

  # Tooltip: name + anomaly value
  label_col <- if ("name" %in% names(combined)) "name" else {
    nc <- sort(grep("^NAME_", names(combined), value = TRUE))
    if (length(nc)) tail(nc, 1) else NULL
  }
  val_str <- if (method == "zscore")
    paste0(round(combined$fill_val, 2), " SD")
  else
    paste0(round(combined$fill_val, 1), " mm")

  combined$tooltip <- if (!is.null(label_col))
    paste0("<b>", combined[[label_col]], "</b><br>Anomaly: ", val_str)
  else
    paste0("Anomaly: ", val_str)
  combined$data_id <- if (!is.null(label_col))
    paste0(combined$year_lbl, "_", combined[[label_col]])
  else
    seq_len(nrow(combined))

  n_rows <- ceiling(n_years / 3)

  p <- ggplot2::ggplot(combined) +
    ggiraph::geom_sf_interactive(
      ggplot2::aes(fill = fill_val, tooltip = tooltip, data_id = data_id),
      color = "white", linewidth = 0.3
    ) +
    ggplot2::facet_wrap(~ year_lbl, ncol = 3) +
    ggplot2::scale_fill_distiller(
      palette   = "RdBu",
      direction = 1,
      limits    = scale_lim,
      breaks    = scale_breaks,
      na.value  = "grey80",
      oob       = scales::squish
    ) +
    ggplot2::labs(
      title   = title_str,
      fill    = anom_name,
      caption = paste0("Source: CHIRPS  |  ",
                       "Blue = wetter than ", n_years, "-year mean; red = drier  |  ",
                       if (method == "zscore") "Units: SD" else "Units: mm")
    ) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0.5, colour = "#666", size = 9),
      strip.text   = ggplot2::element_text(face = "bold", size = 10),
      legend.position = "right"
    )

  ggiraph::girafe(
    ggobj      = p,
    width_svg  = 10,
    height_svg = n_rows * 3 + 1.5,
    options    = list(
      ggiraph::opts_hover(css = "stroke:orange; stroke-width:1.5px;"),
      ggiraph::opts_tooltip(css = "background:#fff; border:1px solid #ccc;
        padding:6px 8px; border-radius:4px; font-size:13px;"),
      ggiraph::opts_sizing(rescale = FALSE, width = 0.95)
    )
  )
}

# ── Summary statistics ────────────────────────────────────────────────────────

#' Build a summary statistics data frame from chirps_process_months() output
#'
#' @param results_list Named list from `chirps_process_months()`.
#' @param year         Integer year (added as a column for context).
#' @return A `data.frame` with one row per month.
chirps_stats_table <- function(results_list, year) {
  rows <- lapply(names(results_list), function(mon) {
    vals <- results_list[[mon]]$mean_rain
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NULL)
    data.frame(
      Month       = mon,
      Year        = year,
      Mean_mm     = round(mean(vals),   1),
      SD_mm       = round(stats::sd(vals), 1),
      Min_mm      = round(min(vals),    1),
      Max_mm      = round(max(vals),    1),
      Valid_areas = length(vals),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

# ── Export helpers ────────────────────────────────────────────────────────────

#' Extract polygon sf for one admin level from a DHIS2 geoFeatures object
#'
#' Filters the `geoFeatures` reactive output from `metadata_widget_server` to
#' a single admin level, keeping only non-empty polygon geometries.
#'
#' @param geo_features An `sf` object as returned by `metadata_widget_output$geoFeatures()`.
#' @param level        Integer admin level to extract.
#' @return An `sf` polygon object, or `NULL` if no polygons exist at that level.
#' @importFrom sf st_is_empty st_geometry_type
chirps_geo_from_metadata <- function(geo_features, level) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")
  if (is.null(geo_features) || nrow(geo_features) == 0) return(NULL)

  gf <- geo_features[geo_features$level == level, ]
  if (nrow(gf) == 0) return(NULL)

  poly_types <- c("POLYGON", "MULTIPOLYGON")
  gf <- gf[
    !sf::st_is_empty(gf) &
    as.character(sf::st_geometry_type(gf)) %in% poly_types,
  ]

  if (nrow(gf) == 0) NULL else gf
}

#' Summarise which admin levels in a geoFeatures object have polygon boundaries
#'
#' @param geo_features An `sf` object from `metadata_widget_output$geoFeatures()`.
#' @return A `data.frame` with columns `level`, `levelName`, `n_polygons`,
#'   or `NULL` if no polygon data are found.
#' @importFrom sf st_is_empty st_geometry_type
chirps_geo_levels <- function(geo_features) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")
  if (is.null(geo_features) || nrow(geo_features) == 0) return(NULL)

  poly_types <- c("POLYGON", "MULTIPOLYGON")
  polys <- geo_features[
    !sf::st_is_empty(geo_features) &
    as.character(sf::st_geometry_type(geo_features)) %in% poly_types,
  ]
  if (nrow(polys) == 0) return(NULL)

  df <- as.data.frame(polys[, c("level", "levelName"), drop = TRUE])
  df$geometry <- NULL

  agg <- aggregate(
    list(n_polygons = rep(1L, nrow(df))),
    by  = list(level = df$level, levelName = df$levelName),
    FUN = sum
  )
  agg[order(agg$level), ]
}

#' Combine per-month sf results into a flat data frame for download
#'
#' Drops the geometry column and adds run-metadata columns. Works with both
#' DHIS2 geoFeatures schemas (`id`, `name`, `levelName`, `parentName`) and
#' GADM schemas (`NAME_*`, `GID_*`).
#'
#' @param results_list Named list from `chirps_process_months()`.
#' @param year         Integer year.
#' @param source_label Short description of the boundary source (e.g.
#'   `"DHIS2 metadata"` or `"Custom upload"`).
#' @param level_label  Human-readable level description (e.g. `"Districts"`).
#' @return A `data.frame` (no geometry column).
chirps_combine_data <- function(results_list, year,
                                 source_label = "",
                                 level_label  = "") {
  mon_num <- c(Jan=1,Feb=2,Mar=3,Apr=4,May=5,Jun=6,
               Jul=7,Aug=8,Sep=9,Oct=10,Nov=11,Dec=12)

  rows <- lapply(names(results_list), function(mon) {
    df <- as.data.frame(results_list[[mon]])
    df$geometry    <- NULL
    df$month       <- mon_num[[mon]]
    df$month_name  <- mon
    df$year        <- year
    df$source      <- source_label
    df$level_label <- level_label
    df
  })

  combined <- do.call(rbind, rows)

  # Column ordering: run metadata, then DHIS2 id/name cols (if present),
  # then GADM NAME_/GID_ cols (if present), then rainfall.
  meta_cols  <- c("source", "level_label", "year", "month", "month_name")
  dhis2_cols <- intersect(c("id", "name", "parentName", "levelName", "level"),
                          names(combined))
  name_cols  <- sort(grep("^NAME_", names(combined), value = TRUE))
  gid_cols   <- sort(grep("^GID_",  names(combined), value = TRUE))
  rain_cols  <- c("mean_rain", "valid_pixels")
  other_cols <- setdiff(names(combined),
                        c(meta_cols, dhis2_cols, name_cols, gid_cols, rain_cols))
  col_order  <- c(meta_cols, dhis2_cols, name_cols, gid_cols, other_cols, rain_cols)
  col_order  <- col_order[col_order %in% names(combined)]
  combined[, col_order]
}

#' Write CHIRPS results to an Excel workbook
#'
#' Creates three sheets: Rainfall_Data, Summary_Stats, Metadata.
#'
#' @param flat_df    Combined data frame from `chirps_combine_data()`.
#' @param stats_df      Stats table from `chirps_stats_table()`.
#' @param year          Integer year.
#' @param area_label    Human-readable area name (e.g. country or "Custom").
#' @param months        Integer vector of months analysed.
#' @param source_label  Boundary source description.
#' @param level_label   Admin level description.
#' @return Raw bytes of the `.xlsx` file (suitable for Shiny `downloadHandler`).
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
chirps_write_excel <- function(flat_df, stats_df, year,
                                area_label   = "",
                                months       = 1:12,
                                source_label = "",
                                level_label  = "") {
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("Package 'openxlsx' is required for Excel export.")

  mon_names <- c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Rainfall_Data")
  openxlsx::writeData(wb, "Rainfall_Data", flat_df)

  if (!is.null(stats_df) && nrow(stats_df) > 0) {
    openxlsx::addWorksheet(wb, "Summary_Stats")
    openxlsx::writeData(wb, "Summary_Stats", stats_df)
  }

  meta <- data.frame(
    Parameter = c("Area", "Boundary Source", "Admin Level",
                  "Year", "Months Analyzed",
                  "CHIRPS Version", "Total Records",
                  "Generated On", "Tool"),
    Value = c(
      area_label,
      source_label,
      level_label,
      as.character(year),
      paste(mon_names[months], collapse = ", "),
      "CHIRPS v2.0",
      as.character(nrow(flat_df)),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      paste0("MG2 v", utils::packageVersion("MG2"))
    ),
    stringsAsFactors = FALSE
  )
  openxlsx::addWorksheet(wb, "Metadata")
  openxlsx::writeData(wb, "Metadata", meta)

  tmp <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  readBin(tmp, "raw", file.info(tmp)$size)
}

# ── Flat rainfall file (cross-widget) ─────────────────────────────────────────

#' Save a standardized flat rainfall RDS to the data directory
#'
#' Writes (or overwrites) `rainfall_{year}_lvl{level_str}.rds` containing only
#' the columns needed for joining to evaluation data: `id`, `name`,
#' `parentName`, `year`, `month`, `mean_rain`.  Geometry and extraction
#' diagnostics are dropped so the file is small and data-frame–friendly.
#'
#' Called automatically by `climate_widget_server` after a successful run.
#' Can also be called standalone for scripted workflows.
#'
#' @param flat_df    Combined data frame from `chirps_combine_data()`.
#' @param data_dir   The MG2 data directory (from `directory_widget_output$directory()`).
#' @param year       Integer year.
#' @param level_str  Character level identifier used in the cache key (e.g. `"2"`
#'   or `"custom"`).
#' @return Invisibly, the path the file was written to.
chirps_save_flat <- function(flat_df, data_dir, year, level_str = "custom") {
  keep <- intersect(c("id", "name", "parentName", "year", "month", "mean_rain"),
                    names(flat_df))
  out  <- flat_df[, keep, drop = FALSE]

  fname <- sprintf("rainfall_%d_lvl%s.rds", as.integer(year), level_str)
  path  <- file.path(data_dir, fname)
  saveRDS(out, path)
  invisible(path)
}

#' Load and combine all available flat rainfall RDS files from the data directory
#'
#' Scans `data_dir` for files matching `rainfall_*.rds`, reads each, and
#' row-binds them into a single data frame.  Files from different years and
#' admin levels are combined; duplicates (same `id` + `year` + `month`) are
#' de-duplicated by keeping the first occurrence (lowest level number, which
#' is typically finer-grained).
#'
#' @param data_dir The MG2 data directory.
#' @return A data frame with columns `id`, `name`, `parentName`, `year`,
#'   `month`, `mean_rain`, or `NULL` if no files are found.
chirps_load_rainfall <- function(data_dir) {
  if (is.null(data_dir) || !nzchar(data_dir %||% "")) return(NULL)

  files <- list.files(data_dir, pattern = "^rainfall_.*\\.rds$", full.names = TRUE)
  if (length(files) == 0) return(NULL)

  dfs <- lapply(files, function(f) {
    tryCatch(readRDS(f), error = function(e) NULL)
  })
  dfs <- Filter(Negate(is.null), dfs)
  if (length(dfs) == 0) return(NULL)

  combined <- do.call(rbind, dfs)

  # De-duplicate: keep first occurrence of each id × year × month combination
  dup_key  <- paste(combined$id, combined$year, combined$month, sep = "|")
  combined <- combined[!duplicated(dup_key), ]
  rownames(combined) <- NULL
  combined
}
