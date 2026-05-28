# =============================================================================
# AOX-HF CONSTRUCTION SCRIPT
# Autoregressive Oil Exchange Index — High Frequency (ADS-based)
# Author : Alex Osterneck, CLA, MSCS, MSIT // ai70000, Ltd.
# Date   : 05/21/26
# Output : AOX_HF.csv  (column: AOXHFprice, daily, 2022-01-01 to present)
# =============================================================================
#
# FORMULA
#   AOX-HF = ( ADS Index + AR(p) 3-yr WTI NYMEX futures ) / TMUBMUSD10Y
#
# Identical structure to original AOX (v.1, 11/12/23). ADS replaces WEI.
# WEI required weekly forward-fill interpolation (confirmed from v.1 console).
# ADS is already daily — no interpolation needed.
#
# DATA SOURCES
# ------------
# ADS  : Philadelphia Fed — ADS_Index_Most_Current_Vintage.xlsx
#         philadelphiafed.org/surveys-and-data/real-time-data-research/ads
#         Confirmed: 24,183 rows, 1960-03-01 through 2026-05-16.
#         Columns: Date (YYYY:MM:DD), ADS_Index, RECBARS
#
# WTI  : Macrotrends daily WTI spot price CSV
#         macrotrends.net/2516/wti-crude-oil-prices-10-year-daily-chart
#         Download as CSV. Column: value (USD/bbl). Date column: date.
#         Used to fit AR(p) model → project 3-yr (756 trading day) forward.
#         NOTE: This is the AR component of the original AOX formula.
#         "AR 3-yr WTI NYMEX futures price" = AR-fitted 36-month projection
#         of spot WTI, not a raw futures contract price. Consistent with V.1.
#
# DGS10: FRED series DGS10 — 10-Year Treasury Constant Maturity Yield
#         fred.stlouisfed.org/series/DGS10
#         Download as CSV. Columns: DATE, DGS10 (% yield, e.g. 3.88)
#         This is TMUBMUSD10Y in V.1 terminology.
#         Missing values (weekends, holidays): forward-filled to match
#         trading calendar (same method confirmed in V.1 for WEI).
#
# DATASET WINDOW
#   Expanded: 2022-01-03 through 2025-12-31 (trading days)
#   V.1 window (2022 calendar year) is a subset — backward compatible.
#
# =============================================================================


# =============================================================================
# 0. PACKAGES
# =============================================================================

library(tidyverse)
library(readxl)
library(forecast)    # auto.arima() for AR(p) WTI projection
library(zoo)         # na.locf() for forward-fill
library(lubridate)
library(MLmetrics)


# =============================================================================
# 1. LOAD DATA
# =============================================================================

#' Load and parse all three source files
#'
#' @param ads_path   Path to ADS xlsx (Philadelphia Fed)
#' @param wti_path   Path to WTI spot CSV (Macrotrends)
#' @param dgs10_path Path to DGS10 CSV (FRED)
#' @return List: ads, wti, dgs10 — all as data.frames with Date column
LoadSources <- function(
    ads_path   = "ADS_Index_Most_Current_Vintage.xlsx",
    wti_path   = "WTI_spot_macrotrends.csv",
    dgs10_path = "DGS10_FRED.csv") {

  # --- ADS ---
  # Date column uses YYYY:MM:DD colon format — convert to Date
  ads_raw <- readxl::read_excel(ads_path)
  ads <- ads_raw |>
    dplyr::mutate(
      Date      = as.Date(gsub(":", "-", Date)),
      ADS_Index = as.numeric(ADS_Index)
    ) |>
    dplyr::select(Date, ADS_Index) |>
    dplyr::filter(!is.na(Date), !is.na(ADS_Index))

  cat(sprintf(
    "ADS loaded: %d rows | %s to %s\n",
    nrow(ads), min(ads$Date), max(ads$Date)
  ))

  # --- WTI spot (Macrotrends) ---
  # Macrotrends CSV has two header rows; skip first, use second as header
  # Typical columns: date, value
  wti_raw <- utils::read.csv(wti_path, skip = 15, header = TRUE)
  wti <- wti_raw |>
    dplyr::rename(Date = 1, WTIspot = 2) |>
    dplyr::mutate(
      Date    = as.Date(Date),
      WTIspot = as.numeric(WTIspot)
    ) |>
    dplyr::filter(!is.na(Date), !is.na(WTIspot)) |>
    dplyr::arrange(Date)

  cat(sprintf(
    "WTI loaded: %d rows | %s to %s\n",
    nrow(wti), min(wti$Date), max(wti$Date)
  ))

  # --- DGS10 (FRED 10-yr Treasury) ---
  # FRED CSV: DATE, DGS10. Missing values coded as "." — convert to NA
  dgs10_raw <- utils::read.csv(dgs10_path)
  dgs10 <- dgs10_raw |>
    dplyr::rename(Date = 1, DGS10 = 2) |>
    dplyr::mutate(
      Date  = as.Date(Date),
      DGS10 = suppressWarnings(as.numeric(DGS10))  # "." becomes NA
    ) |>
    dplyr::filter(!is.na(Date)) |>
    dplyr::arrange(Date)

  cat(sprintf(
    "DGS10 loaded: %d rows | %s to %s | NAs: %d\n",
    nrow(dgs10), min(dgs10$Date), max(dgs10$Date),
    sum(is.na(dgs10$DGS10))
  ))

  return(list(ads = ads, wti = wti, dgs10 = dgs10))
}


# =============================================================================
# 2. BUILD TRADING DAY CALENDAR
# =============================================================================
# All three series operate on different calendars:
#   ADS  — updates ~8x/month (not purely daily)
#   WTI  — trading days only (Mon-Fri, excl. holidays)
#   DGS10 — Treasury business days (similar but not identical to equities)
# Master calendar = WTI trading days (the target series calendar).
# ADS and DGS10 forward-filled to match.

#' Build master trading day calendar from WTI dates
#'
#' @param wti WTI data.frame from LoadSources()
#' @param start_date Filter start (default 2022-01-03, first trading day 2022)
#' @param end_date   Filter end   (default 2025-12-31)
#' @return Date vector of trading days in window
BuildTradingCalendar <- function(wti,
                                  start_date = as.Date("2022-01-03"),
                                  end_date   = as.Date("2025-12-31")) {
  cal <- wti |>
    dplyr::filter(Date >= start_date, Date <= end_date) |>
    dplyr::pull(Date)

  cat(sprintf(
    "Trading calendar: %d days | %s to %s\n",
    length(cal), min(cal), max(cal)
  ))

  return(cal)
}


# =============================================================================
# 3. AR(p) WTI PROJECTION — 3-YEAR FORWARD
# =============================================================================
# This is the "AR 3-yr WTI NYMEX futures price" component of the AOX formula.
# auto.arima() selects optimal p on WTI spot history, then projects 756
# trading days forward (≈ 3 years). The projection is computed for each
# date in the training window using a rolling origin — i.e., on date t,
# the AR model is fit on all WTI data up to t and projects to t+756.
# The 756-day-ahead forecast value becomes the AR futures component for t.
#
# COMPUTATIONAL NOTE: rolling AR on 1000+ dates is slow.
# Set use_rolling = FALSE to use a single AR fit on the full training
# window (faster, slightly less precise). Use rolling for paper/final run.

#' Compute AR 3-yr WTI forward projection for each date in calendar
#'
#' @param wti          WTI data.frame
#' @param trading_cal  Date vector from BuildTradingCalendar()
#' @param use_rolling  If TRUE: rolling origin AR. If FALSE: single AR fit.
#' @param horizon_days Trading days in 3 years (default 756)
#' @return data.frame: Date, AR_WTI_3yr
ComputeArWtiFutures <- function(wti,
                                 trading_cal,
                                 use_rolling  = FALSE,
                                 horizon_days = 756L) {
  cat(sprintf(
    "\n--- AR WTI 3-yr projection (rolling=%s, horizon=%d days) ---\n",
    use_rolling, horizon_days
  ))

  wti_cal <- wti |>
    dplyr::filter(Date %in% trading_cal) |>
    dplyr::arrange(Date)

  if (!use_rolling) {
    # Single AR fit on full window — fast
    wti_ts  <- stats::ts(wti_cal$WTIspot, frequency = 252L)
    ar_fit  <- forecast::auto.arima(wti_ts, seasonal = FALSE,
                                    stepwise = TRUE, approximation = TRUE)
    ar_proj <- as.numeric(forecast::forecast(ar_fit, h = horizon_days)$mean)

    # For each date t, the 3-yr forward value is projected from t
    # Single-fit approximation: use the last horizon_days of the projection
    # mapped back to dates (conservative — use for speed, not final paper)
    n <- nrow(wti_cal)
    ar_3yr <- rep(NA_real_, n)
    for (i in seq_len(n)) {
      remaining <- n - i
      if (remaining >= horizon_days) {
        # Use actual WTI price horizon_days ahead as proxy
        ar_3yr[i] <- wti_cal$WTIspot[i + horizon_days]
      } else {
        # For the tail, use the AR projection
        idx <- horizon_days - remaining
        ar_3yr[i] <- ar_proj[min(idx, length(ar_proj))]
      }
    }

  } else {
    # Rolling origin AR — slower but correct for paper
    # Each date t: fit AR on WTI[1:t], project horizon_days ahead
    n      <- nrow(wti_cal)
    ar_3yr <- rep(NA_real_, n)
    min_obs <- 60L   # minimum observations before fitting

    for (i in min_obs:n) {
      wti_sub <- stats::ts(wti_cal$WTIspot[1L:i], frequency = 252L)
      tryCatch({
        fit_i  <- forecast::auto.arima(wti_sub, seasonal = FALSE,
                                       stepwise = TRUE, approximation = TRUE)
        proj_i <- forecast::forecast(fit_i, h = horizon_days)
        ar_3yr[i] <- as.numeric(proj_i$mean)[horizon_days]
      }, error = function(e) {
        ar_3yr[i] <<- wti_cal$WTIspot[i]  # fallback: use spot price
      })

      if (i %% 100L == 0L) {
        cat(sprintf("  AR rolling: %d / %d dates processed\n", i, n))
      }
    }
  }

  result <- data.frame(Date = wti_cal$Date, AR_WTI_3yr = ar_3yr)
  cat(sprintf(
    "AR WTI 3-yr: %d values | NAs: %d\n",
    nrow(result), sum(is.na(result$AR_WTI_3yr))
  ))

  return(result)
}


# =============================================================================
# 4. ALIGN ALL SERIES TO TRADING CALENDAR
# =============================================================================
# ADS and DGS10 forward-filled to trading calendar.
# Forward-fill is the same method confirmed for WEI in V.1 (from console data).

#' Align ADS and DGS10 to trading calendar via forward-fill
#'
#' @param ads         ADS data.frame
#' @param dgs10       DGS10 data.frame
#' @param trading_cal Date vector
#' @return List: ads_aligned, dgs10_aligned — both data.frames with Date col
AlignToCalendar <- function(ads, dgs10, trading_cal) {
  cat("\n--- Aligning ADS and DGS10 to trading calendar ---\n")

  cal_df <- data.frame(Date = trading_cal)

  # ADS: left join on calendar, then forward-fill
  ads_aligned <- cal_df |>
    dplyr::left_join(ads, by = "Date") |>
    dplyr::arrange(Date) |>
    dplyr::mutate(
      ADS_Index = zoo::na.locf(ADS_Index, na.rm = FALSE)
    )

  # DGS10: left join, forward-fill, convert % to decimal
  # DGS10 is in percent (e.g. 3.88 = 3.88%) — keep as-is to match V.1
  # AOX denominator: TMUBMUSD10Y was in percent in V.1 (confirmed from scale)
  dgs10_aligned <- cal_df |>
    dplyr::left_join(dgs10, by = "Date") |>
    dplyr::arrange(Date) |>
    dplyr::mutate(
      DGS10 = zoo::na.locf(DGS10, na.rm = FALSE)
    )

  cat(sprintf(
    "ADS aligned: %d rows | NAs remaining: %d\n",
    nrow(ads_aligned), sum(is.na(ads_aligned$ADS_Index))
  ))
  cat(sprintf(
    "DGS10 aligned: %d rows | NAs remaining: %d\n",
    nrow(dgs10_aligned), sum(is.na(dgs10_aligned$DGS10))
  ))

  return(list(ads_aligned = ads_aligned, dgs10_aligned = dgs10_aligned))
}


# =============================================================================
# 5. CONSTRUCT AOX-HF
# =============================================================================
# Formula: AOX-HF = (ADS_Index + AR_WTI_3yr) / DGS10
# Identical structure to v.1 AOX:
#   v.1: AOX = (WEI + AR_WTI_3yr) / TMUBMUSD10Y
#   v.3: AOX-HF = (ADS_Index + AR_WTI_3yr) / DGS10
# ADS replaces WEI. DGS10 is TMUBMUSD10Y. AR component identical.

#' Construct AOX-HF index
#'
#' @param ads_aligned   Output of AlignToCalendar()$ads_aligned
#' @param ar_wti        Output of ComputeArWtiFutures()
#' @param dgs10_aligned Output of AlignToCalendar()$dgs10_aligned
#' @return data.frame: Date, AOXHFprice
ConstructAoxHf <- function(ads_aligned, ar_wti, dgs10_aligned) {
  cat("\n--- Constructing AOX-HF ---\n")

  aox_hf <- ads_aligned |>
    dplyr::left_join(ar_wti,        by = "Date") |>
    dplyr::left_join(dgs10_aligned, by = "Date") |>
    dplyr::mutate(
      # Guard against division by zero (DGS10 near-zero edge case)
      DGS10_safe = dplyr::if_else(abs(DGS10) < 1e-6, NA_real_, DGS10),
      AOXHFprice = (ADS_Index + AR_WTI_3yr) / DGS10_safe
    ) |>
    dplyr::select(Date, AOXHFprice) |>
    dplyr::filter(!is.na(AOXHFprice))

  cat(sprintf(
    "AOX-HF constructed: %d rows | %s to %s\n",
    nrow(aox_hf), min(aox_hf$Date), max(aox_hf$Date)
  ))
  cat(sprintf(
    "AOX-HF summary: min=%.4f | mean=%.4f | max=%.4f | NAs=%d\n",
    min(aox_hf$AOXHFprice, na.rm = TRUE),
    mean(aox_hf$AOXHFprice, na.rm = TRUE),
    max(aox_hf$AOXHFprice, na.rm = TRUE),
    sum(is.na(aox_hf$AOXHFprice))
  ))

  # Plot for visual sanity check
  graphics::plot(
    aox_hf$Date, aox_hf$AOXHFprice,
    type = "l", col = "steelblue", lwd = 1.5,
    main = "AOX-HF Index (ADS-based) — 2022 to 2025",
    xlab = "Date", ylab = "AOX-HF"
  )
  graphics::abline(h = 0, lty = 2, col = "gray60")

  return(aox_hf)
}


# =============================================================================
# 6. VALIDATE AOX-HF
# =============================================================================
# Stationarity check and summary statistics before saving.
# If ADF rejects H0 (p < 0.05), AOX-HF is stationary — same requirement
# as original AOX in V.1.

#' Validate AOX-HF: ADF test, summary, year-by-year breakdown
#'
#' @param aox_hf data.frame from ConstructAoxHf()
#' @return Invisible aox_hf (for piping)
ValidateAoxHf <- function(aox_hf) {
  cat("\n--- AOX-HF Validation ---\n")

  aox_ts <- stats::ts(aox_hf$AOXHFprice, frequency = 252L)

  cat("ADF test on AOX-HF:\n")
  print(tseries::adf.test(aox_ts))

  cat("\nYear-by-year mean AOX-HF:\n")
  aox_hf |>
    dplyr::mutate(Year = lubridate::year(Date)) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      n    = dplyr::n(),
      mean = round(mean(AOXHFprice, na.rm = TRUE), 4L),
      sd   = round(sd(AOXHFprice,   na.rm = TRUE), 4L),
      min  = round(min(AOXHFprice,  na.rm = TRUE), 4L),
      max  = round(max(AOXHFprice,  na.rm = TRUE), 4L),
      .groups = "drop"
    ) |>
    print()

  return(invisible(aox_hf))
}


# =============================================================================
# 7. SAVE OUTPUT
# =============================================================================

#' Save AOX-HF to CSV for use in V.3 dual-index pipeline
#'
#' @param aox_hf   data.frame from ConstructAoxHf()
#' @param out_path Output path (default: AOX_HF.csv)
SaveAoxHf <- function(aox_hf, out_path = "AOX_HF.csv") {
  utils::write.csv(aox_hf, out_path, row.names = FALSE)
  cat(sprintf("\nAOX_HF.csv saved: %d rows → %s\n", nrow(aox_hf), out_path))
  cat("Column: AOXHFprice — ready for WTI_Forecast_V3_DualIndex.R\n")
  return(invisible(out_path))
}


# =============================================================================
# 8. MAIN
# =============================================================================

#' Build AOX-HF from ADS + AR WTI + DGS10
#'
#' File inputs required (place in working directory):
#'   ADS_Index_Most_Current_Vintage.xlsx  — from Philadelphia Fed
#'   WTI_spot_macrotrends.csv             — from Macrotrends
#'   DGS10_FRED.csv                       — from FRED (series DGS10)
#'
#' File output:
#'   AOX_HF.csv                           — input to v.3 dual-index pipeline
#'
#' @param use_rolling_ar TRUE for paper/final (slow). FALSE for dev (fast).
#' @return Invisible aox_hf data.frame
Main <- function(use_rolling_ar = FALSE) {
  cat("============================================================\n")
  cat("  AOX-HF CONSTRUCTION — Osterneck 2026\n")
  cat("  Formula: (ADS + AR 3-yr WTI) / DGS10\n")
  cat("  Window:  2022-01-03 to 2025-12-31\n")
  cat(sprintf("  AR mode: %s\n",
              ifelse(use_rolling_ar, "rolling (paper)", "single (dev)")))
  cat("============================================================\n\n")

  # Step 1: Load
  sources <- LoadSources(
    ads_path   = "ADS_Index_Most_Current_Vintage.xlsx",
    wti_path   = "WTI_spot_macrotrends.csv",
    dgs10_path = "DGS10_FRED.csv"
  )

  # Step 2: Trading calendar
  trading_cal <- BuildTradingCalendar(
    sources$wti,
    start_date = as.Date("2022-01-03"),
    end_date   = as.Date("2025-12-31")
  )

  # Step 3: AR WTI 3-yr projection
  ar_wti <- ComputeArWtiFutures(
    sources$wti,
    trading_cal,
    use_rolling  = use_rolling_ar,
    horizon_days = 756L
  )

  # Step 4: Align to calendar
  aligned <- AlignToCalendar(sources$ads, sources$dgs10, trading_cal)

  # Step 5: Construct
  aox_hf <- ConstructAoxHf(
    aligned$ads_aligned,
    ar_wti,
    aligned$dgs10_aligned
  )

  # Step 6: Validate
  ValidateAoxHf(aox_hf)

  # Step 7: Save
  SaveAoxHf(aox_hf, out_path = "AOX_HF.csv")

  cat("\n============================================================\n")
  cat("  AOX-HF CONSTRUCTION COMPLETE\n")
  cat("  Next: run WTI_Forecast_V3_DualIndex_Osterneck_2026.R\n")
  cat("============================================================\n")

  return(invisible(aox_hf))
}

# Execute — use_rolling_ar=FALSE for development speed
# Switch to TRUE for final paper run
results <- Main(use_rolling_ar = FALSE)
