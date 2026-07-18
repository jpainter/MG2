# Generate mg2_demo_geo: Sierra Leone district-level sf object for demo maps.
#
# Source: GADM level-2 (district) polygons via geodata::gadm().
# Run once to regenerate data/mg2_demo_geo.rda.
#
# Usage:
#   source("data-raw/generate_demo_geo.R")

library(geodata)
library(sf)
library(dplyr)

# Download GADM level-2 (district) polygons for Sierra Leone
sl2 <- geodata::gadm("SLE", level = 2, path = tempdir())
sl2_sf <- sf::st_as_sf(sl2) |>
  sf::st_transform(4326)                    # ensure WGS84 (leaflet-ready)

# Merge "Western Rural" + "Western Urban" → "Western Area" to match DHIS2
western <- sl2_sf |>
  dplyr::filter(NAME_2 %in% c("Western Rural", "Western Urban")) |>
  sf::st_union() |>
  sf::st_sf(geometry = _)
western$NAME_2 <- "Western Area"

sl2_clean <- sl2_sf |>
  dplyr::filter(!NAME_2 %in% c("Western Rural", "Western Urban")) |>
  dplyr::select(NAME_2, geometry) |>
  dplyr::bind_rows(dplyr::select(western, NAME_2, geometry))

# Build the sf in the format geoFeatures() expects:
#   id, name, level, levelName, parentName, geometry
mg2_demo_geo <- sl2_clean |>
  dplyr::transmute(
    id         = paste0("SLE_demo_", seq_len(dplyr::n())),
    name       = NAME_2,
    level      = 2L,
    levelName  = "District",
    parentName = "Sierra Leone",
    geometry   = geometry
  ) |>
  dplyr::arrange(name)

cat("mg2_demo_geo rows:", nrow(mg2_demo_geo), "\n")
cat("districts:", paste(sort(mg2_demo_geo$name), collapse = ", "), "\n")

usethis::use_data(mg2_demo_geo, overwrite = TRUE, compress = "xz")
