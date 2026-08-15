#' AI4RPT — Phase 2: Advisory Engine
#' ============================================================================
#' Wires the onset/cessation output (Phase 1) to the validated crop-specific
#' advisory table. Given a location's forecast condition + selected crop,
#' returns the matching advisory message with a confidence indicator.
#'
#' Depends on: onset_cessation.R (Phase 1) for rainfall condition detection.
#'
#' Advisory content source: AI4RPT_Advisory_Rules_Draft.md (validated by
#' Prof. Yusuf). Message text below reproduces that validated table — any
#' future edits should be made in BOTH places, or better, this file should
#' become the single source of truth once validated content is finalized.
#' ============================================================================

library(dplyr)
library(tibble)

# ---------------------------------------------------------------------------
# 1. Forecast conditions (must match Phase 1 output categories)
# ---------------------------------------------------------------------------
CONDITIONS <- c(
  "early_onset",
  "normal_onset",
  "delayed_onset",
  "dry_spell",
  "excess_rainfall",
  "early_cessation",
  "normal_cessation"
)

# Excess rainfall detection: cumulative rainfall meeting/exceeding this
# threshold within the given window signals waterlogging/flood risk —
# a standard heavy-rainfall indicator for humid tropical agromet contexts,
# mirroring how onset uses a 20mm/2-3 day threshold in the other direction.
EXCESS_RAIN_WINDOW_DAYS <- 7
EXCESS_RAIN_THRESHOLD_MM <- 100

CROPS <- c("maize", "rice", "millet", "cassava", "cowpea")

# ---------------------------------------------------------------------------
# 2. Advisory table (validated starter set — general agronomic best practice)
# ---------------------------------------------------------------------------
ADVISORY_TABLE <- tribble(
  ~crop,     ~condition,          ~advisory_en,
  "maize",   "early_onset",       "Confirm at least 2 consecutive rainy days before planting to avoid false-start losses; use early-to-medium maturing varieties if onset is unusually early.",
  "maize",   "normal_onset",      "Plant with onset of rains using recommended spacing; apply basal fertilizer at planting.",
  "maize",   "delayed_onset",     "Switch to early-maturing varieties; avoid late top-dressing that pushes maturity into dry period.",
  "maize",   "dry_spell",         "Mulch to conserve soil moisture; delay fertilizer top-dressing until rains resume to avoid scorch/waste.",
  "maize",   "excess_rainfall",   "Ensure field drainage; watch for nitrogen leaching — consider split fertilizer application.",
  "maize",   "early_cessation",   "Prioritize early-maturing varieties in future seasons; harvest promptly to avoid field losses.",
  "maize",   "normal_cessation",  "Season length is adequate — continue with standard crop management through to harvest.",

  "rice",    "early_onset",       "Prepare nursery/land early; transplant only after onset is confirmed stable.",
  "rice",    "normal_onset",      "Follow standard transplanting window; maintain adequate standing water for lowland rice.",
  "rice",    "delayed_onset",     "Use short-duration varieties; consider nursery delay to match rainfall pattern.",
  "rice",    "dry_spell",         "For lowland rice, supplement with irrigation/stored water if available; for upland, mulch and avoid additional stress.",
  "rice",    "excess_rainfall",   "Monitor for flooding/lodging risk; ensure bunds and drainage channels are functional.",
  "rice",    "early_cessation",   "Ensure grain-filling stage is not compromised — consider supplemental watering if feasible.",
  "rice",    "normal_cessation",  "Season length is adequate — continue with standard crop management through to harvest.",

  "millet",  "early_onset",       "Millet is drought-tolerant — early planting is generally favorable; proceed with land prep.",
  "millet",  "normal_onset",      "Plant with onset; millet tolerates lower rainfall better than maize/rice.",
  "millet",  "delayed_onset",     "Millet remains a good fallback crop under delayed onset due to short duration and drought tolerance.",
  "millet",  "dry_spell",         "Millet generally withstands moderate dry spells; monitor only for prolonged/severe cases.",
  "millet",  "excess_rainfall",   "Watch for waterlogging — millet is less tolerant of excess water than drought.",
  "millet",  "early_cessation",   "Millet's short duration makes it relatively resilient to early cessation; harvest promptly once matured.",
  "millet",  "normal_cessation",  "Season length is adequate — continue with standard crop management through to harvest.",

  "cassava", "early_onset",       "Good planting window; cassava establishes well with early moisture.",
  "cassava", "normal_onset",      "Plant stem cuttings at onset; ensure good soil moisture at establishment.",
  "cassava", "delayed_onset",     "Cassava can tolerate delayed planting better than cereals, but yield may be reduced — plant as soon as rains stabilize.",
  "cassava", "dry_spell",         "Cassava is relatively drought-tolerant once established; young plantings (<3 months) are more vulnerable — mulch if possible.",
  "cassava", "excess_rainfall",   "Ensure good drainage — waterlogging causes tuber rot.",
  "cassava", "early_cessation",   "Cassava's long duration means season length has less immediate impact; monitor moisture at critical bulking stage.",
  "cassava", "normal_cessation",  "Season length is adequate — continue with standard crop management through to harvest.",

  "cowpea",  "early_onset",       "Suitable for early planting; watch for increased pest/disease pressure with extended wet conditions.",
  "cowpea",  "normal_onset",      "Plant with onset; cowpea has short duration and fits well as intercrop or relay crop.",
  "cowpea",  "delayed_onset",     "Cowpea's short cycle makes it a good option for delayed-onset seasons or as a substitute crop.",
  "cowpea",  "dry_spell",         "Cowpea is drought-tolerant; moderate dry spells generally well tolerated, especially post-flowering.",
  "cowpea",  "excess_rainfall",   "High humidity increases risk of fungal disease — monitor closely; avoid waterlogged fields.",
  "cowpea",  "early_cessation",   "Cowpea's short duration is well-suited to shortened seasons — good fallback recommendation.",
  "cowpea",  "normal_cessation",  "Season length is adequate — continue with standard crop management through to harvest."
)

# ---------------------------------------------------------------------------
# 3. Confidence indicator logic (validated as important — do not drop)
# ---------------------------------------------------------------------------
#' Assigns a confidence label to a dry-spell condition based on how far
#' the observed dry run is past the disqualifying threshold. Other
#' conditions are reported at "confirmed" confidence since they're based
#' on completed rainfall events, not an ongoing/uncertain pattern.
assign_confidence <- function(condition, dry_days_observed = NULL) {
  if (condition != "dry_spell") {
    return("confirmed")
  }
  if (is.null(dry_days_observed)) {
    return("moderate")
  }
  if (dry_days_observed >= DRY_SPELL_LENGTH_DAYS) {
    "high"
  } else if (dry_days_observed >= (DRY_SPELL_LENGTH_DAYS * 0.6)) {
    "moderate"
  } else {
    "low"
  }
}

# ---------------------------------------------------------------------------
# 4. Main lookup function
# ---------------------------------------------------------------------------
#' get_advisory
#'
#' @param crop one of CROPS
#' @param condition one of CONDITIONS (output of Phase 1 condition detection)
#' @param dry_days_observed optional, only relevant when condition == "dry_spell"
#' @param language currently only "en" populated; placeholder for
#'        yo (Yoruba), ha (Hausa), fr (French) once translated
#'
#' @return a list with advisory text, condition, crop, and confidence label
get_advisory <- function(crop, condition, dry_days_observed = NULL, language = "en") {
  crop <- tolower(crop)
  condition <- tolower(condition)

  if (!crop %in% CROPS) stop("Unknown crop: ", crop)
  if (!condition %in% CONDITIONS) stop("Unknown condition: ", condition)
  if (language != "en") {
    warning("Only English advisory text is populated so far; falling back to 'en'.")
    language <- "en"
  }

  row <- ADVISORY_TABLE %>% filter(crop == !!crop, condition == !!condition)

  if (nrow(row) == 0) {
    stop("No advisory found for crop='", crop, "', condition='", condition, "'")
  }

  confidence <- assign_confidence(condition, dry_days_observed)

  list(
    crop = crop,
    condition = condition,
    confidence = confidence,
    language = language,
    advisory_text = row$advisory_en[1]
  )
}

# ---------------------------------------------------------------------------
# 5. Bridge from Phase 1 output to a condition label
# ---------------------------------------------------------------------------
#' classify_condition
#'
#' Turns a Phase 1 analyze_lga() result (onset/cessation dates + a rainfall
#' series) into one of the CONDITIONS categories. This is a starter
#' classifier — expand it as more nuanced rules (e.g. excess rainfall
#' thresholds) are validated.
#'
#' @param rainfall_df data frame from fetch_daily_rainfall() (Phase 1)
#' @param onset_date date from find_onset()
#' @param cessation_date date from find_cessation()
#' @param reference_date the "today" for which we're generating advisory
#'        (defaults to Sys.Date())
classify_condition <- function(rainfall_df, onset_date, cessation_date,
                                reference_date = Sys.Date(),
                                normal_onset_window = NULL) {
  # Placeholder normal-onset window: to be replaced with each pilot
  # location's actual long-term average onset date once historical
  # baselines are computed (needs multi-year NASA POWER data per LGA).
  if (is.null(normal_onset_window)) {
    normal_onset_window <- list(
      earliest_normal = as.Date(paste0(format(onset_date, "%Y"), "-04-01")),
      latest_normal    = as.Date(paste0(format(onset_date, "%Y"), "-05-15"))
    )
  }

  # Reference date after cessation -> season-end condition
  if (reference_date >= cessation_date) {
    is_early <- cessation_date < as.Date(paste0(format(cessation_date, "%Y"), "-10-01"))
    return(if (is_early) "early_cessation" else "normal_cessation")
  }

  # Reference date before/at onset -> onset condition
  if (reference_date <= onset_date) {
    if (onset_date < normal_onset_window$earliest_normal) return("early_onset")
    if (onset_date > normal_onset_window$latest_normal) return("delayed_onset")
    return("normal_onset")
  }

  # In-season -> check for excess rainfall first (more urgent/actionable
  # than dry spell risk when both could theoretically be evaluated)
  excess_window <- rainfall_df %>%
    filter(date <= reference_date, date > reference_date - EXCESS_RAIN_WINDOW_DAYS)
  excess_sum <- sum(excess_window$rain_mm, na.rm = TRUE)

  if (excess_sum >= EXCESS_RAIN_THRESHOLD_MM) {
    return("excess_rainfall")
  }

  # Then check for current dry spell around reference_date
  window <- rainfall_df %>%
    filter(date <= reference_date, date > reference_date - DRY_SPELL_CHECK_WINDOW)
  recent_dry_run <- {
    run <- 0L; max_run <- 0L
    for (v in rev(window$rain_mm)) {
      # NASA POWER's near-real-time data can have NA/missing values for the
      # most recent few days due to processing lag. Treat missing days as
      # "unknown, not counted as dry" rather than crashing the comparison.
      if (is.na(v)) {
        next
      } else if (v < DRY_DAY_THRESHOLD_MM) {
        run <- run + 1L; max_run <- max(max_run, run)
      } else {
        run <- 0L
      }
    }
    max_run
  }

  if (recent_dry_run >= (DRY_SPELL_LENGTH_DAYS * 0.6)) {
    return("dry_spell")
  }

  "normal_onset"  # in-season, no flagged condition
}

# ---------------------------------------------------------------------------
# 6. Example usage
# ---------------------------------------------------------------------------
# source("onset_cessation.R")
# coords <- load_lga_coords()
# dropdown <- build_dropdown_data(coords)
# lga <- dropdown$Kwara %>% filter(lga == "Ilorin West")
#
# rainfall <- fetch_daily_rainfall(lga$latitude, lga$longitude, 2025)
# onset <- find_onset(rainfall)
# cessation <- find_cessation(rainfall, onset)
#
# condition <- classify_condition(rainfall, onset, cessation, reference_date = Sys.Date())
# advisory <- get_advisory(crop = "maize", condition = condition, dry_days_observed = 7)
# print(advisory)
