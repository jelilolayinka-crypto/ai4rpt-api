#' AI4RPT — Step 2: Backend API (plumber)
#' ============================================================================
#' Exposes the working R logic (onset_cessation.R, advisory_engine.R,
#' baseline_windows.csv) as a live REST API the frontend can call.
#'
#' Setup:
#'   install.packages(c("plumber", "httr"))
#'
#' Run locally (from R console, in this folder):
#'   library(plumber)
#'   pr("plumber.R") %>% pr_run(port = 8000)
#'
#' Then visit http://localhost:8000/__docs__/ for interactive API docs,
#' or call endpoints directly, e.g.:
#'   http://localhost:8000/lgas?state=Kwara
#'   http://localhost:8000/forecast?state=Kwara&lga=Ilorin%20West&crop=maize
#'
#' Files expected in this same folder:
#'   onset_cessation.R, advisory_engine.R, nigeria_lga_coordinates.csv,
#'   baseline_windows.csv
#' ============================================================================

library(plumber)
library(dplyr)
library(readr)
library(httr)

# ---------------------------------------------------------------------------
# Load everything once at startup (not per-request) for performance
# ---------------------------------------------------------------------------
source("onset_cessation.R")
source("advisory_engine.R")
source("baseline_calculator.R")
source("rain_outlook.R")

COORDS <- load_lga_coords()
DROPDOWN <- build_dropdown_data(COORDS)

BASELINE_WINDOWS <- tryCatch(
  read_csv("baseline_windows.csv", show_col_types = FALSE),
  error = function(e) {
    warning("baseline_windows.csv not found — /forecast will use placeholder normal-onset windows until it's generated.")
    NULL
  }
)

#* @apiTitle AI4RPT API
#* @apiDescription Rainfall onset/cessation prediction and crop advisory for Kwara, Osun, Kano pilot states.

# ---------------------------------------------------------------------------
# CORS — allows your frontend (running on a different origin) to call this API
# ---------------------------------------------------------------------------
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods", "GET,OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type")
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

# ---------------------------------------------------------------------------
# GET /states — list the 3 pilot states
# ---------------------------------------------------------------------------
#* @get /states
function() {
  list(states = names(DROPDOWN))
}

# ---------------------------------------------------------------------------
# GET /lgas?state=Kwara — list LGAs (+ coordinates) for a state
# ---------------------------------------------------------------------------
#* @param state The state name (Kwara, Osun, or Kano)
#* @get /lgas
function(state) {
  if (is.null(DROPDOWN[[state]])) {
    return(list(error = paste0("Unknown state '", state, "'. Valid: ", paste(names(DROPDOWN), collapse = ", "))))
  }
  DROPDOWN[[state]] %>% select(lga, latitude, longitude)
}

# ---------------------------------------------------------------------------
# GET /crops — list supported crops
# ---------------------------------------------------------------------------
#* @get /crops
function() {
  list(crops = CROPS)
}

# ---------------------------------------------------------------------------
# GET /forecast?state=Kwara&lga=Ilorin West&crop=maize&year=2026
#
# Main endpoint: pulls current-year rainfall, computes onset/cessation,
# classifies condition against the real baseline window, and returns the
# matching advisory with confidence indicator.
# ---------------------------------------------------------------------------
#* @param state The state name
#* @param lga The LGA name (must match exactly — see /lgas for valid names)
#* @param crop One of: maize, rice, millet, cassava, cowpea
#* @param year Optional. Defaults to current year.
#* @param lang Optional. "en" or "fr" (default "en"). "yo"/"ha" exist in draft
#*        form pending validation and fall back to English with a note.
#* @get /forecast
function(state, lga, crop, year = as.character(format(Sys.Date(), "%Y")), lang = "en") {
  state <- trimws(state)
  lga <- trimws(lga)
  crop <- trimws(crop)
  lang <- trimws(lang)
  year <- as.integer(year)
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  is_current_year <- (year == current_year)

  if (is.null(DROPDOWN[[state]])) {
    return(list(error = paste0(
      "Unknown state '", state, "'. Valid: ", paste(names(DROPDOWN), collapse = ", "),
      " (check for extra spaces in the input)."
    )))
  }

  loc_row <- DROPDOWN[[state]] %>% filter(lga == !!lga)
  if (nrow(loc_row) == 0) {
    res_body <- list(error = paste0("LGA '", lga, "' not found in state '", state, "'."))
    return(res_body)
  }

  # NASA POWER cannot return data for future dates. For the current,
  # in-progress year, only request up to yesterday (today's data may not
  # yet be processed on their end either, so yesterday is the safe cutoff).
  # NASA POWER's near-real-time data can lag by several days before it's
  # fully processed — requesting up to "yesterday" isn't always safe.
  # Using a 4-day buffer avoids hitting unprocessed/missing days entirely.
  fetch_end_date <- if (is_current_year) Sys.Date() - 4 else as.Date(paste0(year, "-12-31"))

  rainfall <- tryCatch(
    fetch_daily_rainfall_ranged(loc_row$latitude, loc_row$longitude, year, fetch_end_date),
    error = function(e) NULL
  )
  if (is.null(rainfall) || nrow(rainfall) == 0) {
    return(list(error = "Could not fetch rainfall data from NASA POWER. Try again shortly."))
  }

  onset <- find_onset(rainfall)
  if (is.na(onset)) {
    return(list(error = "No qualifying onset found yet for this location/year."))
  }
  cessation <- find_cessation(rainfall, onset)

  # Use the real baseline window if available, else the classify_condition() placeholder
  normal_window <- NULL
  if (!is.null(BASELINE_WINDOWS)) {
    label <- paste0(lga, ", ", tools::toTitleCase(state))
    baseline_row <- BASELINE_WINDOWS %>% filter(location == label)
    if (nrow(baseline_row) == 1) {
      normal_window <- baseline_window_for_year(baseline_row, year)
    }
  }

  condition <- classify_condition(
    rainfall_df = rainfall,
    onset_date = onset,
    cessation_date = if (is.na(cessation)) as.Date(paste0(year, "-12-31")) else cessation,
    reference_date = Sys.Date(),
    normal_onset_window = normal_window
  )

  advisory <- get_advisory(crop = crop, condition = condition, language = lang)

  # Season-long variability note — built entirely from the baseline stats
  # we already have (no new data pull needed). This is a general risk
  # signal from historical variability, not a month-by-month dry-spell
  # calendar — a true per-month risk profile would need re-scanning the
  # 20-year daily series for historical dry-spell timing, which is a
  # separate, heavier task if you want that level of detail later.
  variability_note <- NA
  if (!is.null(BASELINE_WINDOWS) && nrow(baseline_row) == 1) {
    sd_onset <- round(baseline_row$sd_onset_doy[1])
    sd_cessation <- round(baseline_row$sd_cessation_doy[1])
    if (!is.na(sd_onset) && !is.na(sd_cessation)) {
      variability_note <- paste0(
        "Over the last ", baseline_row$n_years_used[1], " years, onset for this location has varied by about ",
        sd_onset, " days and cessation by about ", sd_cessation,
        " days year to year — build some flexibility into planting and harvest timing rather than treating these dates as fixed."
      )
    }
  }

  # When cessation isn't yet confirmed (season ongoing), fall back to an
  # ESTIMATE from the 20-year baseline — clearly flagged as an estimate,
  # not a confirmed date, so the frontend can display it differently.
  estimated_cessation <- NA
  estimated_season_length <- NA
  if (is.na(cessation) && !is.null(BASELINE_WINDOWS) && nrow(baseline_row) == 1) {
    mean_cessation_doy <- baseline_row$mean_cessation_doy[1]
    if (!is.na(mean_cessation_doy)) {
      estimated_cessation <- as.Date(mean_cessation_doy - 1, origin = paste0(year, "-01-01"))
      estimated_season_length <- round(baseline_row$mean_season_length[1])
    }
  }

  list(
    state = state,
    lga = lga,
    crop = crop,
    year = year,
    onset_date = as.character(onset),
    cessation_date = if (is.na(cessation)) NA else as.character(cessation),
    cessation_confirmed = !is.na(cessation),
    estimated_cessation_date = if (is.na(estimated_cessation)) NA else as.character(estimated_cessation),
    season_length_days = if (is.na(cessation)) NA else as.integer(cessation - onset),
    estimated_season_length_days = if (is.na(estimated_season_length)) NA else estimated_season_length,
    condition = condition,
    condition_label = advisory$condition_label,
    confidence = advisory$confidence,
    language = advisory$language,
    language_note = advisory$language_note,
    advisory_text = advisory$advisory_text,
    variability_note = variability_note,
    baseline_used = !is.null(normal_window)
  )
}

# ---------------------------------------------------------------------------
# GET /voice?text=...&lang=en — proxy to Azure TTS (English/French only)
# Returns audio/mpeg directly.
# ---------------------------------------------------------------------------
#* @param text The advisory text to synthesize
#* @param lang One of: en, fr (yo/ha not yet supported — see Phase 3b)
#* @get /voice
function(res, text, lang = "en") {
  # Errors return JSON explicitly; only the successful audio response
  # below switches to the raw content-type serializer. Mixing a fixed
  # @serializer annotation with list() error returns causes a
  # "not compatible with requested type" crash — this avoids that.
  json_error <- function(status, message) {
    res$status <- status
    res$serializer <- plumber::serializer_unboxed_json()
    list(error = message)
  }

  if (!lang %in% c("en", "fr")) {
    return(json_error(501, paste0(
      "Voice for language '", lang, "' is not yet available. ",
      "Yoruba/Hausa require the fine-tuned open-source TTS pipeline (Phase 3b, in progress)."
    )))
  }

  key <- trimws(Sys.getenv("AZURE_SPEECH_KEY"))
  region <- trimws(Sys.getenv("AZURE_SPEECH_REGION"))
  if (key == "" || region == "") {
    return(json_error(500, "Server missing AZURE_SPEECH_KEY / AZURE_SPEECH_REGION environment variables."))
  }

  # TEMPORARY DIAGNOSTIC: swapped en-NG-EzinneNeural -> en-US-JennyNeural to
  # test whether the 400 error is caused by the Nigerian English voice not
  # being enabled in this Azure resource's region. Revert once confirmed.
  voice <- if (lang == "en") "en-US-JennyNeural" else "fr-FR-DeniseNeural"
  locale <- if (lang == "en") "en-US" else "fr-FR"

  ssml <- sprintf(
    '<speak version="1.0" xmlns="http://www.w3.org/2001/10/synthesis" xml:lang="%s"><voice name="%s">%s</voice></speak>',
    locale, voice, htmltools::htmlEscape(text)
  )

  # Prefer the exact endpoint Azure shows you (set as AZURE_SPEECH_ENDPOINT),
  # since Foundry/multi-service resources can use a different host than the
  # classic {region}.tts.speech.microsoft.com pattern this falls back to.
  custom_endpoint <- trimws(Sys.getenv("AZURE_SPEECH_ENDPOINT"))
  if (custom_endpoint != "") {
    custom_endpoint <- sub("/+$", "", custom_endpoint)  # strip trailing slash(es)
    request_url <- paste0(custom_endpoint, "/cognitiveservices/v1")
  } else {
    request_url <- sprintf("https://%s.tts.speech.microsoft.com/cognitiveservices/v1", region)
  }

  resp <- POST(
    url = request_url,
    add_headers(
      "Ocp-Apim-Subscription-Key" = key,
      "Content-Type" = "application/ssml+xml",
      "X-Microsoft-OutputFormat" = "audio-16khz-48kbitrate-mono-mp3"
    ),
    body = charToRaw(enc2utf8(ssml))
  )

  if (status_code(resp) != 200) {
    # TEMPORARY DIAGNOSTIC: exposing the exact request details (minus the
    # key itself) so we can see precisely what was sent, since Azure's
    # error body has been empty so far. Remove once root cause is found.
    return(json_error(502, paste0(
      "Azure TTS request failed (HTTP ", status_code(resp), "). ",
      "URL: ", request_url, ". ",
      "Region (length ", nchar(region), "): '", region, "'. ",
      "Key present: ", nchar(key) > 0, " (length ", nchar(key), "). ",
      "Body: ", content(resp, "text", encoding = "UTF-8")
    )))
  }

  res$serializer <- plumber::serializer_content_type("audio/mpeg")
  content(resp, "raw")
}

# ---------------------------------------------------------------------------
# GET /rain_outlook?state=Kwara&lga=Ilorin West&date=2026-08-25
#
# Answers "will it rain on date X?" — real forecast within ~16 days,
# climatological likelihood (clearly labeled as such) beyond that.
# ---------------------------------------------------------------------------
#* @param state The state name
#* @param lga The LGA name
#* @param date Target date, format YYYY-MM-DD
#* @get /rain_outlook
function(state, lga, date) {
  state <- trimws(state)
  lga <- trimws(lga)

  if (is.null(DROPDOWN[[state]])) {
    return(list(error = paste0(
      "Unknown state '", state, "'. Valid: ", paste(names(DROPDOWN), collapse = ", "),
      " (check for extra spaces in the input)."
    )))
  }

  loc_row <- DROPDOWN[[state]] %>% filter(lga == !!lga)
  if (nrow(loc_row) == 0) {
    return(list(error = paste0("LGA '", lga, "' not found in state '", state, "'.")))
  }

  target_date <- tryCatch(as.Date(date), error = function(e) NA)
  if (is.na(target_date)) {
    return(list(error = "Invalid date format. Use YYYY-MM-DD."))
  }

  baseline_row <- NULL
  if (!is.null(BASELINE_WINDOWS)) {
    label <- paste0(lga, ", ", tools::toTitleCase(state))
    br <- BASELINE_WINDOWS %>% filter(location == label)
    if (nrow(br) == 1) baseline_row <- br
  }

  fetch_rain_outlook(loc_row$latitude, loc_row$longitude, target_date, baseline_row)
}
