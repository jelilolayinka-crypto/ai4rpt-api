#' AI4RPT — Rain Outlook: Short-Term Forecast (Open-Meteo)
#' ============================================================================
#' Answers "will it rain on date X?" — genuinely different from the
#' onset/cessation/baseline logic elsewhere, which is diagnostic
#' (climatology + current-season detection), not predictive.
#'
#' Open-Meteo gives a REAL forecast, but only up to ~16 days ahead — this
#' is a hard physical limit of weather forecasting generally, not a
#' limitation specific to this API. Beyond that horizon, we fall back to
#' a climatological likelihood (from the 20-year baseline) — clearly
#' labeled as historical probability, not a forecast.
#'
#' No API key required for Open-Meteo's free tier.
#' ============================================================================

library(httr)
library(jsonlite)
library(dplyr)

FORECAST_HORIZON_DAYS <- 16  # Open-Meteo's practical daily forecast limit

#' fetch_rain_outlook
#'
#' @param lat, lon    location coordinates
#' @param target_date the date being asked about (Date object)
#' @param baseline_row optional 1-row data frame from baseline_windows.csv,
#'                      used for the climatological fallback beyond the
#'                      forecast horizon
#'
#' @return a list describing the outlook for that date
fetch_rain_outlook <- function(lat, lon, target_date, baseline_row = NULL) {
  target_date <- as.Date(target_date)
  today <- Sys.Date()
  days_ahead <- as.integer(target_date - today)

  if (days_ahead < 0) {
    return(list(
      type = "past",
      message = "That date is in the past — use /forecast for historical onset/cessation data instead."
    ))
  }

  if (days_ahead <= FORECAST_HORIZON_DAYS) {
    return(fetch_from_open_meteo(lat, lon, target_date))
  }

  # Beyond forecast horizon — climatological fallback only
  return(climatological_fallback(target_date, baseline_row, days_ahead))
}

fetch_from_open_meteo <- function(lat, lon, target_date) {
  url <- sprintf(
    "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&daily=precipitation_sum,precipitation_probability_max&timezone=auto&forecast_days=%d",
    lat, lon, FORECAST_HORIZON_DAYS
  )

  # Render services can share outbound IP addresses. If another tenant using
  # the same IP causes a temporary rate-limit or gateway error, retry briefly
  # and identify this research application instead of sending a generic
  # libcurl request.
  request_error <- NULL
  resp <- tryCatch(
    RETRY(
      "GET",
      url,
      user_agent("AI4RPT/1.0 (public research prototype)"),
      timeout(20),
      times = 3,
      pause_base = 1,
      pause_cap = 5,
      terminate_on = c(400, 401, 403, 404)
    ),
    error = function(e) {
      request_error <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(resp)) {
    return(list(
      type = "error",
      message = paste0(
        "Open-Meteo connection failed after retries. ",
        if (!is.null(request_error)) request_error else "No response was received."
      )
    ))
  }

  if (status_code(resp) != 200) {
    upstream_body <- tryCatch(
      content(resp, "text", encoding = "UTF-8"),
      error = function(e) ""
    )
    upstream_body <- trimws(substr(upstream_body, 1, 300))
    return(list(
      type = "error",
      upstream_status = status_code(resp),
      message = paste0(
        "Open-Meteo forecast temporarily unavailable (HTTP ",
        status_code(resp), ").",
        if (upstream_body != "") paste0(" Provider response: ", upstream_body) else ""
      )
    ))
  }

  data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  dates <- as.Date(data$daily$time)
  idx <- which(dates == target_date)

  if (length(idx) == 0) {
    return(list(type = "error", message = "Target date not found in forecast response."))
  }

  precip_mm <- data$daily$precipitation_sum[idx]
  prob_pct <- data$daily$precipitation_probability_max[idx]

  list(
    type = "forecast",
    source = "Open-Meteo (short-term forecast)",
    date = as.character(target_date),
    rain_expected = precip_mm > 0.5,
    precipitation_mm = precip_mm,
    probability_percent = prob_pct,
    message = sprintf(
      "%d%% chance of rain, ~%.1fmm expected.",
      round(prob_pct), precip_mm
    )
  )
}

climatological_fallback <- function(target_date, baseline_row, days_ahead) {
  if (is.null(baseline_row) || nrow(baseline_row) != 1) {
    return(list(
      type = "unavailable",
      message = sprintf(
        "%d days out is beyond the %d-day forecast horizon, and no historical baseline is available for this location to estimate likelihood.",
        days_ahead, FORECAST_HORIZON_DAYS
      )
    ))
  }

  target_doy <- as.integer(format(target_date, "%j"))
  onset_doy <- baseline_row$mean_onset_doy[1]
  cessation_doy <- baseline_row$mean_cessation_doy[1]
  sd_onset <- baseline_row$sd_onset_doy[1]

  in_typical_season <- target_doy >= (onset_doy - sd_onset) && target_doy <= cessation_doy

  list(
    type = "climatological",
    source = "20-year historical baseline (NOT a forecast)",
    date = as.character(target_date),
    days_beyond_forecast_horizon = days_ahead - FORECAST_HORIZON_DAYS,
    within_typical_rainy_season = in_typical_season,
    message = if (in_typical_season) {
      sprintf(
        "This date falls within this location's typical rainy season (historical average). No specific forecast is available %d days out — this is a historical likelihood, not a prediction for this particular year.",
        days_ahead
      )
    } else {
      sprintf(
        "This date falls outside this location's typical rainy season based on 20-year history. No specific forecast is available %d days out.",
        days_ahead
      )
    }
  )
}
