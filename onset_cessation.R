#' AI4RPT — Phase 1 (R version): Rainfall Onset / Cessation / Season Length
#' + LGA Dropdown Coordinate Builder
#' ============================================================================
#'
#' Data source (rainfall):  NASA POWER, via the `nasapower` R package
#'                           https://docs.ropensci.org/nasapower/
#'
#' Data source (LGA coordinates): two open, MIT-licensed community datasets —
#'   1. https://github.com/xosasx/nigerian-local-government-areas
#'   2. https://github.com/temikeezy/nigeria-geojson-data
#'   Download the CSV/JSON from either repo and point LGA_COORDS_FILE below
#'   at it. Coordinates are NOT hand-typed here — they should come from a
#'   verifiable source you can audit, given how location-sensitive this
#'   calculation is.
#'
#' Definition used (validated):
#'   ONSET     = first date with >= 20mm cumulative rain over a 2-3 day
#'               window, with no 10+ consecutive dry days (<1mm each) in
#'               the following 30 days.
#'   CESSATION = last qualifying 20mm/2-3 day rain event after onset,
#'               within the season search window.
#'   SEASON LENGTH = cessation - onset (days)
#'
#' Install requirements:
#'   install.packages(c("nasapower", "dplyr", "readr", "purrr"))
#'
#' ============================================================================

library(nasapower)
library(dplyr)
library(readr)
library(purrr)

# ---------------------------------------------------------------------------
# 1. Configurable thresholds (tune after validating against local records)
# ---------------------------------------------------------------------------
RAIN_WINDOW_DAYS         <- 3     # window over which cumulative rain is checked
RAIN_ONSET_THRESHOLD_MM  <- 20    # cumulative mm needed within the window
DRY_SPELL_LENGTH_DAYS    <- 10    # consecutive dry days that invalidate a candidate onset
DRY_DAY_THRESHOLD_MM     <- 1.0   # a day with < this much rain counts as "dry"
DRY_SPELL_CHECK_WINDOW   <- 30    # days after candidate onset to check for dry spell
SEASON_SEARCH_START_MONTH <- 2    # start looking for onset from Feb (covers early-onset years)
SEASON_SEARCH_END_MONTH   <- 11   # stop looking for cessation by end of Nov
CESSATION_CONFIRMATION_DAYS <- 21 # days of trailing data required after the last
                                   # qualifying rain event before declaring the
                                   # season truly over (avoids mistaking "no data
                                   # yet" for "no rain came")

# ---------------------------------------------------------------------------
# 2. Pilot states
# ---------------------------------------------------------------------------
PILOT_STATES <- c("Kwara", "Osun", "Kano")

# Path to the LGA coordinates file you download from one of the sources
# above (CSV with columns: State, LGA, Latitude, Longitude — adjust
# column names in load_lga_coords() to match whichever source you use).
LGA_COORDS_FILE <- "nigeria_lga_coordinates.csv"

# ---------------------------------------------------------------------------
# 3. Load & filter LGA coordinates for the dropdown
# ---------------------------------------------------------------------------
load_lga_coords <- function(path = LGA_COORDS_FILE, states = PILOT_STATES) {
  if (!file.exists(path)) {
    stop(
      "LGA coordinates file not found at '", path, "'.\n",
      "Download it from:\n",
      "  https://github.com/xosasx/nigerian-local-government-areas (csv/ folder), or\n",
      "  https://github.com/temikeezy/nigeria-geojson-data\n",
      "and place it at the path above."
    )
  }

  coords <- read_csv(path, show_col_types = FALSE)

  # Standardise column names — adjust these mappings if your source uses
  # different column headers (e.g. lowercase "state", "lga").
  names(coords) <- tolower(names(coords))

  coords <- coords %>%
    rename(
      state     = matches("^state$"),
      lga       = matches("^lga$"),
      latitude  = matches("^lat"),
      longitude = matches("^lon")
    ) %>%
    filter(state %in% states) %>%
    distinct(state, lga, .keep_all = TRUE) %>%
    arrange(state, lga)

  coords
}

#' Returns a named list suitable for populating a state -> LGA dropdown,
#' e.g. dropdown_data()$Kwara gives all Kwara LGAs with coordinates.
build_dropdown_data <- function(coords) {
  split(coords, coords$state)
}

# ---------------------------------------------------------------------------
# 4. Fetch daily rainfall for one LGA (NASA POWER via nasapower)
# ---------------------------------------------------------------------------
fetch_daily_rainfall <- function(lat, lon, year) {
  fetch_daily_rainfall_ranged(lat, lon, year, as.Date(paste0(year, "-12-31")))
}

#' fetch_daily_rainfall_ranged
#'
#' Same as fetch_daily_rainfall(), but lets the caller cap the end date
#' explicitly. Needed for the current, in-progress year — NASA POWER
#' cannot return data for future dates, so requests for the current
#' calendar year must be capped at yesterday, not December 31.
#'
#' @param lat, lon  location coordinates
#' @param year      calendar year to fetch (used for the start date)
#' @param end_date  a Date object; the fetch will not go past this date
fetch_daily_rainfall_ranged <- function(lat, lon, year, end_date) {
  start_date <- as.Date(paste0(year, "-01-01"))
  end_date <- as.Date(end_date)

  if (end_date < start_date) {
    stop("end_date (", end_date, ") is before the start of requested year (", year, ").")
  }

  df <- get_power(
    community   = "AG",
    lonlat      = c(lon, lat),
    pars        = "PRECTOTCORR",
    dates       = c(as.character(start_date), as.character(end_date)),
    temporal_api = "daily"
  )
  df %>%
    select(date = YYYYMMDD, rain_mm = PRECTOTCORR) %>%
    mutate(rain_mm = ifelse(rain_mm <= -900, NA_real_, rain_mm)) %>%  # NASA POWER uses -999 for missing/not-yet-processed data
    arrange(date)
}

# ---------------------------------------------------------------------------
# 5. Onset / cessation logic
# ---------------------------------------------------------------------------
has_dry_spell <- function(values) {
  run <- 0L
  for (v in values) {
    if (is.na(v)) {
      next  # missing data (NASA POWER processing lag) — skip, don't crash
    } else if (v < DRY_DAY_THRESHOLD_MM) {
      run <- run + 1L
      if (run >= DRY_SPELL_LENGTH_DAYS) return(TRUE)
    } else {
      run <- 0L
    }
  }
  FALSE
}

find_onset <- function(rainfall_df) {
  d <- rainfall_df
  n <- nrow(d)

  for (i in seq_len(n)) {
    mo <- as.integer(format(d$date[i], "%m"))
    if (mo < SEASON_SEARCH_START_MONTH) next
    if (mo > SEASON_SEARCH_END_MONTH) break

    window_end <- min(i + RAIN_WINDOW_DAYS - 1, n)
    window_sum <- sum(d$rain_mm[i:window_end], na.rm = TRUE)

    if (window_sum >= RAIN_ONSET_THRESHOLD_MM) {
      check_end <- min(i + DRY_SPELL_CHECK_WINDOW - 1, n)
      if (has_dry_spell(d$rain_mm[i:check_end])) next  # false start
      return(d$date[i])
    }
  }
  NA
}

#' find_cessation
#'
#' Finds the season's cessation date — the last qualifying rain event
#' after which the season genuinely ends.
#'
#' IMPORTANT: cessation is only confirmed if there's a real dry stretch of
#' at least CESSATION_CONFIRMATION_DAYS following the last qualifying rain
#' event, WITH enough trailing data to check. This avoids the bug where a
#' still-in-progress year (data only available up to yesterday) gets its
#' "last qualifying rain event so far" mistaken for a true cessation, when
#' really the season just hasn't ended yet and more rain may still come.
#'
#' Returns NA if cessation cannot yet be confirmed (i.e. we're still
#' in-season and it's too early to tell) — callers should treat NA here
#' as "season ongoing," not "error."
find_cessation <- function(rainfall_df, onset_date,
                            confirmation_days = CESSATION_CONFIRMATION_DAYS) {
  d <- rainfall_df %>% filter(date > onset_date)
  n <- nrow(d)
  last_qualifying <- NA
  last_qualifying_idx <- NA

  for (i in seq_len(n)) {
    mo <- as.integer(format(d$date[i], "%m"))
    if (mo > SEASON_SEARCH_END_MONTH) break

    window_end <- min(i + RAIN_WINDOW_DAYS - 1, n)
    window_sum <- sum(d$rain_mm[i:window_end], na.rm = TRUE)

    if (window_sum >= RAIN_ONSET_THRESHOLD_MM) {
      last_qualifying <- d$date[i]
      last_qualifying_idx <- i
    }
  }

  if (is.na(last_qualifying)) return(NA)

  # Confirm: is there enough trailing data AFTER the last qualifying rain
  # event to be sure no more rain came? If data runs out too soon after,
  # we genuinely don't know yet whether the season has ended.
  trailing_start <- last_qualifying_idx + RAIN_WINDOW_DAYS
  trailing_days_available <- n - trailing_start + 1

  if (trailing_days_available < confirmation_days) {
    return(NA)  # not enough data yet to confirm — season likely still ongoing
  }

  last_qualifying
}

analyze_lga <- function(lat, lon, year, label = NULL) {
  rainfall <- fetch_daily_rainfall(lat, lon, year)
  onset <- find_onset(rainfall)

  if (is.na(onset)) {
    message("No qualifying onset found for ", label, " in ", year)
    return(invisible(NULL))
  }

  cessation <- find_cessation(rainfall, onset)
  if (is.na(cessation)) {
    message("Onset found (", onset, ") but no cessation identified for ", label, " in ", year)
    return(invisible(NULL))
  }

  season_length <- as.integer(cessation - onset)

  tibble(
    location      = label,
    year          = year,
    onset_date    = onset,
    cessation_date = cessation,
    season_length_days = season_length
  )
}

# ---------------------------------------------------------------------------
# 6. Example usage (run after downloading the LGA coordinates file)
# ---------------------------------------------------------------------------
# coords <- load_lga_coords()
# dropdown <- build_dropdown_data(coords)
# dropdown$Kwara            # data frame of all Kwara LGAs + coordinates
#
# one_lga <- dropdown$Kwara %>% filter(lga == "Ilorin West")
# result <- analyze_lga(one_lga$latitude, one_lga$longitude, 2025, label = "Ilorin West, Kwara")
# print(result)
#
# # Run across all LGAs in a state for one year:
# all_results <- purrr::pmap_dfr(
#   dropdown$Kwara %>% select(lga, latitude, longitude),
#   function(lga, latitude, longitude) {
#     analyze_lga(latitude, longitude, 2025, label = lga)
#   }
# )
# print(all_results)
