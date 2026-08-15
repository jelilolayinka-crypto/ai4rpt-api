#' AI4RPT — Convert full.json (state > LGA > wards) into LGA-level
#' centroid coordinates CSV for Kwara, Osun, Kano
#' ============================================================================
#' Source file: full.json from github.com/temikeezy/nigeria-geojson-data
#'   Structure: [ { state, lgas: [ { name, wards: [ { name, latitude,
#'   longitude } ] } ] } ]
#'
#' Since full.json only has coordinates at ward level, this computes each
#' LGA's centroid as the mean of its wards' lat/lon — a standard, defensible
#' way to get a representative point per LGA from ward-level data.
#'
#' Usage:
#'   1. Place full.json in your working directory (same folder as this script)
#'   2. source("convert_lga_coords.R")
#'   3. Produces nigeria_lga_coordinates.csv, ready for load_lga_coords()
#'      in onset_cessation.R
#' ============================================================================

library(jsonlite)
library(dplyr)
library(purrr)
library(readr)

PILOT_STATES <- c("Kwara", "Osun", "Kano")

convert_full_json_to_lga_csv <- function(json_path = "full.json",
                                          output_path = "nigeria_lga_coordinates.csv",
                                          states = PILOT_STATES) {

  if (!file.exists(json_path)) {
    stop("Could not find '", json_path, "' — make sure it's in your working directory.")
  }

  raw <- fromJSON(json_path, simplifyDataFrame = FALSE)

  rows <- list()

  for (state_entry in raw) {
    state_name <- state_entry$state
    if (!(state_name %in% states)) next

    for (lga_entry in state_entry$lgas) {
      lga_name <- lga_entry$name
      wards <- lga_entry$wards

      if (is.null(wards) || length(wards) == 0) {
        message("No wards found for ", lga_name, ", ", state_name, " — skipping.")
        next
      }

      lats <- map_dbl(wards, ~ as.numeric(.x$latitude %||% NA))
      lons <- map_dbl(wards, ~ as.numeric(.x$longitude %||% NA))

      lats <- lats[!is.na(lats)]
      lons <- lons[!is.na(lons)]

      if (length(lats) == 0 || length(lons) == 0) {
        message("No valid coordinates for ", lga_name, ", ", state_name, " — skipping.")
        next
      }

      rows[[length(rows) + 1]] <- tibble(
        state = state_name,
        lga = lga_name,
        latitude = mean(lats),
        longitude = mean(lons),
        n_wards_averaged = length(lats)
      )
    }
  }

  if (length(rows) == 0) {
    stop("No matching LGAs found for states: ", paste(states, collapse = ", "),
         ". Check that the state names in full.json exactly match (e.g. 'Kwara', not 'kwara').")
  }

  result <- bind_rows(rows) %>% arrange(state, lga)

  write_csv(result, output_path)
  message("Wrote ", nrow(result), " LGAs to ", output_path)

  result
}

# `%||%` helper (null-coalescing), in case not already loaded via another package
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Run it
# ---------------------------------------------------------------------------
# lga_coords <- convert_full_json_to_lga_csv()
# print(lga_coords)
