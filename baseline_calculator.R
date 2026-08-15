#' AI4RPT — Phase 2 (continued): Historical Baseline Calculator
#' ============================================================================
#' Computes each pilot LGA's actual long-term average onset/cessation window
#' from multi-year NASA POWER data, replacing the placeholder window used
#' in classify_condition() (see advisory_engine.R).
#'
#' IMPORTANT: This requires internet access to run (calls NASA POWER
#' repeatedly — one call per year per LGA). It could not be executed in
#' the assistant's sandbox, which has no network access and is blocked
#' from directly querying the NASA POWER API endpoint. Run this once in
#' an environment with internet access (your machine, KWASU/OSU servers,
#' or the eventual hosting environment), then save baseline_results to a
#' file (baseline_windows.csv) that the app loads at runtime — no need to
#' recompute it every time the app runs.
#'
#' Source: onset_cessation.R (Phase 1) for fetch_daily_rainfall(),
#' find_onset(), find_cessation().
#' ============================================================================

library(dplyr)
library(purrr)
library(lubridate)

# ---------------------------------------------------------------------------
# 1. Baseline period — standard climatological baseline is ~30 years where
#    available; NASA POWER's usable daily record realistically starts
#    around 1984. Using the most recent 20 years (adjustable) balances
#    a long-enough record against relevance to current rainfall patterns,
#    which have been shifting due to climate change.
# ---------------------------------------------------------------------------
BASELINE_START_YEAR <- year(Sys.Date()) - 20
BASELINE_END_YEAR   <- year(Sys.Date()) - 1   # last complete year

# ---------------------------------------------------------------------------
# 2. Compute onset/cessation for every year at one location
# ---------------------------------------------------------------------------
compute_multiyear_onset_cessation <- function(lat, lon, label,
                                               start_year = BASELINE_START_YEAR,
                                               end_year = BASELINE_END_YEAR) {
  years <- start_year:end_year

  results <- map_dfr(years, function(yr) {
    rainfall <- tryCatch(
      fetch_daily_rainfall(lat, lon, yr),
      error = function(e) {
        message("Failed to fetch ", label, " ", yr, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(rainfall)) return(NULL)

    onset <- find_onset(rainfall)
    if (is.na(onset)) return(NULL)

    cessation <- find_cessation(rainfall, onset)

    tibble(
      location = label,
      year = yr,
      onset_date = onset,
      onset_doy = yday(onset),           # day-of-year, for averaging across years
      cessation_date = cessation,
      cessation_doy = if (!is.na(cessation)) yday(cessation) else NA_integer_,
      season_length_days = if (!is.na(cessation)) as.integer(cessation - onset) else NA_integer_
    )
  })

  results
}

# ---------------------------------------------------------------------------
# 3. Summarize into a "normal window" per location
# ---------------------------------------------------------------------------
#' Uses mean day-of-year +/- 1 standard deviation as the "normal" onset
#' window — dates within this range count as normal_onset; earlier counts
#' as early_onset; later counts as delayed_onset. This is a standard
#' climatological approach; adjust the SD multiplier if you want a
#' tighter/looser "normal" band after reviewing the actual spread.
summarize_baseline <- function(multiyear_results, sd_multiplier = 1) {
  multiyear_results %>%
    group_by(location) %>%
    summarise(
      n_years_used = n(),
      mean_onset_doy = mean(onset_doy, na.rm = TRUE),
      sd_onset_doy = sd(onset_doy, na.rm = TRUE),
      mean_cessation_doy = mean(cessation_doy, na.rm = TRUE),
      sd_cessation_doy = sd(cessation_doy, na.rm = TRUE),
      mean_season_length = mean(season_length_days, na.rm = TRUE),
      earliest_normal_onset_doy = mean_onset_doy - sd_multiplier * sd_onset_doy,
      latest_normal_onset_doy = mean_onset_doy + sd_multiplier * sd_onset_doy,
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------
# 4. Run across all three pilot states' LGAs and save results
# ---------------------------------------------------------------------------
# source("onset_cessation.R")
# coords <- load_lga_coords()
#
# all_lgas <- coords  # every LGA across Kwara, Osun, Kano
#
# baseline_raw <- pmap_dfr(
#   all_lgas %>% select(lga, state, latitude, longitude),
#   function(lga, state, latitude, longitude) {
#     compute_multiyear_onset_cessation(
#       lat = latitude, lon = longitude,
#       label = paste0(lga, ", ", state)
#     )
#   }
# )
#
# baseline_windows <- summarize_baseline(baseline_raw)
#
# # Save for reuse — the app loads this file at runtime instead of
# # recomputing baselines on every request.
# write_csv(baseline_windows, "baseline_windows.csv")

# ---------------------------------------------------------------------------
# 5. Feed a saved baseline into classify_condition() from advisory_engine.R
# ---------------------------------------------------------------------------
#' Converts a day-of-year window (from baseline_windows.csv) into actual
#' Date objects for a given year, for use as normal_onset_window in
#' classify_condition().
baseline_window_for_year <- function(baseline_row, target_year) {
  list(
    earliest_normal = as.Date(baseline_row$earliest_normal_onset_doy - 1,
                               origin = paste0(target_year, "-01-01")),
    latest_normal   = as.Date(baseline_row$latest_normal_onset_doy - 1,
                               origin = paste0(target_year, "-01-01"))
  )
}

# Example integration:
# baseline_windows <- read_csv("baseline_windows.csv")
# row <- baseline_windows %>% filter(location == "Ilorin West, Kwara")
# window <- baseline_window_for_year(row, target_year = 2026)
# condition <- classify_condition(rainfall, onset, cessation,
#                                  reference_date = Sys.Date(),
#                                  normal_onset_window = window)
