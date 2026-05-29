# =============================================================================
# AOX ALPHA SIGNAL — 2018 Q4 BACKTEST
# Author   : Alex Osterneck (CLA, MSCS, MSIT, M.Acc candidate)
# Entity   : Ai70000, Ltd. — Quantitative Research & Production Laboratory
# File     : AOX_Backtest_2018_Osterneck_2026.R
# Date     : 05/27/26
# Google R Style Guide compliant
# =============================================================================
#
# ─── PURPOSE ─────────────────────────────────────────────────────────────────
#
# This script tests whether the AOX alpha signal held during the 2018 Q4
# WTI crash — the third independent backtest event.
#
# WHY 2018 Q4:
#   WTI crashed from $76.41 (Oct 3) to $42.53 (Dec 24) — a 44% drop
#   in 83 calendar days. Cause: simultaneous OPEC+ supply surge, U.S.
#   Iran sanctions waiver (removed expected supply tightening), and
#   U.S.-China trade war demand fears. A supply glut + demand fear event.
#   DGS10 was 3.00-3.25% throughout — well above the 1% guard condition.
#   No zero-bound distortion (unlike COVID 2020 which failed this guard).
#
# THE THREE BACKTEST EVENTS (if 2018 confirms):
#   2018 Q4 — supply glut + demand fear crash   (downside)
#   2022    — Russia-Ukraine geopolitical shock  (upside)
#   2025    — demand dislocation                 (downside)
#   Three different shock types. Three confirmations = product.
#
# ─── THE SIGNAL (recap) ──────────────────────────────────────────────────────
#
# TRIGGER  : 21-day annualized realized WTI vol > 45%
#            AND DGS10 (10-yr Treasury yield) > 1.0%
#            (The >1% guard prevents false signals when Fed is at zero bound.
#             COVID 2020 failed this guard — DGS10 was 0.50-0.90%.)
# LEAD     : AOX at market close day t predicts WTI direction at close day t+1
# LAG      : Lag-1. One trading day. Close-to-close.
# VALIDATED: 2022 p=0.0315, 2025 pooled p=0.0775/lag-1 p=0.0315
#
# ─── DATA FILES REQUIRED ─────────────────────────────────────────────────────
#
# These are the same input files used in the main pipeline.
# No new downloads needed — all sources cover 2016+.
#
#   chart_20260524T011951.csv    : WTI daily spot price (Macrotrends, 2016+)
#                                  Column: Value (renamed to WTIprice)
#   ADS_Index_Most_Current_Vintage.xlsx : ADS index (Philadelphia Fed, 1960+)
#                                  Column: ADS_Index
#   DGS10__1_.csv                : 10-yr Treasury yield (FRED, 2018+)
#                                  Column: DGS10
#
# =============================================================================


# =============================================================================
# 0. CONFIGURATION
# =============================================================================

# Window: include 2017 for AR model warmup, test signal in 2018
kWindowStart   <- as.Date("2017-01-03")
kWindowEnd     <- as.Date("2018-12-31")
kTestStart     <- as.Date("2018-01-02")   # signal test window
kVolThreshold  <- 0.45    # 45% annualized vol — macro shock trigger
kDgs10Guard    <- 1.00    # DGS10 > 1% required — zero-bound guard
kVolWindow     <- 21L     # rolling lookback in trading days
kVolSmooth     <- 5L      # smoothing window (prevents fragmentation)
kGrangerLagMax <- 5L      # max lag for Granger test
kTsFreq        <- 252L
kTsStart       <- c(2018L, 1L)

graphics::par(mar = c(3, 3, 2.5, 2))

cat("============================================================\n")
cat("  AOX ALPHA SIGNAL — 2018 Q4 BACKTEST\n")
cat("  Ai70000, Ltd. | Osterneck 2026\n")
cat("  Event: WTI crash Oct-Dec 2018 (-44%, 83 days)\n")
cat("  DGS10 guard: >1.0% (no zero-bound distortion)\n")
cat("============================================================\n\n")


# =============================================================================
# 1. PACKAGES
# =============================================================================
# These are the same packages used throughout the main pipeline.
# All should already be installed from the prior session.

library(tidyverse)    # data manipulation and pipe operator
library(MTS)          # multivariate time series — diffM() for stationarity
library(forecast)     # ndiffs() for differencing order, auto.arima()
library(tseries)      # adf.test() for stationarity testing
library(vars)         # VAR() and causality() for Granger test
library(MLmetrics)    # MAPE() for forecast accuracy
library(readxl)       # read_excel() for ADS xlsx file
library(zoo)          # na.locf() for forward-fill alignment


# =============================================================================
# 2. LOAD AND ALIGN SOURCE DATA
# =============================================================================
#
# PLAIN ENGLISH:
# We need three data series covering 2017-2018:
#   (1) WTI daily closing price — what we're trying to predict
#   (2) ADS index — real U.S. economic activity, updates daily
#   (3) DGS10 — 10-year Treasury yield, the denominator in AOX-HF
#
# All three input files are available in the data/ directory.
# We filter to the 2017-2018 window here.

#' Load and align all three source files to the 2018 backtest window
#'
#' @return List: wti, ads, dgs10, cal (aligned master calendar)
LoadAndAlign2018 <- function() {
  cat("=== STEP 1: LOAD SOURCE DATA ===\n")

  # WTI spot price — Macrotrends daily (2016-2026)
  # Original column names: Date (MM/DD/YYYY), Value
  wti_raw <- utils::read.csv("chart_20260524T011951.csv",
                              stringsAsFactors = FALSE)
  colnames(wti_raw) <- c("Date", "WTIprice")
  wti_raw$Date <- as.Date(wti_raw$Date, format = "%m/%d/%Y")
  wti <- wti_raw[!is.na(wti_raw$WTIprice), ]
  wti <- wti[order(wti$Date), ]
  cat(sprintf("WTI loaded: %d rows | %s to %s\n",
              nrow(wti), min(wti$Date), max(wti$Date)))

  # ADS index — Philadelphia Fed (1960-2026)
  # Updates ~8x per month. On non-update days: carry last value forward.
  ads_raw <- readxl::read_excel("ADS_Index_Most_Current_Vintage.xlsx")
  ads_raw$Date <- as.Date(gsub(":", "-", ads_raw$Date))
  ads <- ads_raw[!is.na(ads_raw$Date) & !is.na(ads_raw$ADS_Index),
                 c("Date","ADS_Index")]
  ads <- ads[order(ads$Date), ]
  cat(sprintf("ADS loaded: %d rows | %s to %s\n",
              nrow(ads), min(ads$Date), max(ads$Date)))

  # DGS10 — FRED extended (2018-2026)
  # Missing values on weekends/holidays: forward-filled.
  dgs10_raw <- utils::read.csv("DGS10__1_.csv", stringsAsFactors = FALSE)
  colnames(dgs10_raw) <- c("Date", "DGS10")
  dgs10_raw$Date  <- as.Date(dgs10_raw$Date)
  dgs10_raw$DGS10 <- suppressWarnings(as.numeric(dgs10_raw$DGS10))
  dgs10_raw$DGS10 <- zoo::na.locf(dgs10_raw$DGS10, na.rm = FALSE)
  dgs10 <- dgs10_raw[!is.na(dgs10_raw$Date), ]
  cat(sprintf("DGS10 loaded: %d rows | %s to %s\n",
              nrow(dgs10), min(dgs10$Date), max(dgs10$Date)))

  # Build master calendar from WTI trading days: 2017-01-03 to 2018-12-31
  # (2017 included for AR model warmup — gives the model history before 2018)
  cal <- wti[wti$Date >= kWindowStart & wti$Date <= kWindowEnd, ]
  cal <- cal[order(cal$Date), ]
  cal <- cal[!is.na(cal$WTIprice), ]
  cat(sprintf("\nMaster calendar: %d trading days | %s to %s\n",
              nrow(cal), min(cal$Date), max(cal$Date)))

  # Align ADS to trading calendar via forward-fill
  # (carry last published ADS value forward to each trading day)
  cal <- merge(cal, ads, by = "Date", all.x = TRUE)
  cal <- cal[order(cal$Date), ]
  cal$ADS_Index <- zoo::na.locf(cal$ADS_Index, na.rm = FALSE)
  cat(sprintf("ADS after align: NAs = %d\n", sum(is.na(cal$ADS_Index))))

  # Align DGS10 to trading calendar
  cal <- merge(cal, dgs10, by = "Date", all.x = TRUE)
  cal <- cal[order(cal$Date), ]
  cal$DGS10 <- zoo::na.locf(cal$DGS10, na.rm = FALSE)
  cat(sprintf("DGS10 after align: NAs = %d\n", sum(is.na(cal$DGS10))))

  return(list(wti = wti, ads = ads, dgs10 = dgs10, cal = cal))
}


# =============================================================================
# 3. BUILD AR(3,1,2) 3-YEAR WTI FORWARD PROJECTION
# =============================================================================
#
# PLAIN ENGLISH:
# AOX-HF uses a projected WTI price 3 years into the future as one of its
# inputs. We fit an ARIMA model on historical WTI prices and project forward
# 756 trading days (~3 years). This is the same method used throughout V.1,
# V.2, and V.3 — identical component, just applied to the 2018 window.
# For dates where actual future WTI prices exist (early in the window),
# we use those as the realized anchor instead of the AR projection.

#' Build AR(3,1,2) 3-year WTI forward projection for 2018 window
#'
#' @param cal   Master calendar data.frame
#' @param wti   Full WTI data.frame (for AR model training)
#' @return cal with AR_WTI_3yr column added
BuildArProjection <- function(cal, wti) {
  cat("\n=== STEP 2: AR(3,1,2) WTI FORWARD PROJECTION ===\n")

  # Fit ARIMA on all WTI history up to end of window
  wti_full <- wti[wti$Date <= kWindowEnd, "WTIprice"]
  model    <- forecast::Arima(wti_full, order = c(3L, 1L, 2L))
  fc       <- as.numeric(forecast::forecast(model, h = 756L)$mean)
  cat(sprintf("ARIMA(3,1,2) fit: AIC=%.2f\n", model$aic))

  # For each date: use actual WTI 756 days ahead if available,
  # otherwise use AR model forecast
  n      <- nrow(cal)
  ar_3yr <- rep(NA_real_, n)

  for(i in seq_len(n)) {
    future_idx <- i + 756L
    if(future_idx <= n) {
      # Realized: actual WTI price 756 trading days later
      ar_3yr[i] <- cal$WTIprice[future_idx]
    } else {
      # AR model forecast for dates near end of window
      fc_idx     <- min(future_idx - n, 756L)
      ar_3yr[i]  <- fc[fc_idx]
    }
  }

  cal$AR_WTI_3yr <- round(ar_3yr, 2L)
  cat(sprintf("AR_WTI_3yr: NAs=%d | mean=$%.2f\n",
              sum(is.na(cal$AR_WTI_3yr)),
              mean(cal$AR_WTI_3yr, na.rm = TRUE)))

  return(cal)
}


# =============================================================================
# 4. CONSTRUCT AOX-HF (2018 WINDOW)
# =============================================================================
#
# PLAIN ENGLISH:
# AOX-HF formula: (ADS_Index + AR_WTI_3yr) / DGS10
#
# Three inputs, each doing a specific job:
#   ADS_Index   : real U.S. economic activity (are things good or bad?)
#   AR_WTI_3yr  : where does the market think oil will be in 3 years?
#   DGS10       : what is the risk-free rate? (higher rates = compress index)
#
# DGS10 guard: we only use the signal when DGS10 > 1%.
# In 2018, DGS10 was 3.00-3.25% — well above the guard. No distortion.

#' Construct AOX-HF index for 2018 window
#'
#' @param cal Calendar with ADS_Index, AR_WTI_3yr, DGS10 columns
#' @return cal with AOXHFprice column added
ConstructAoxHf2018 <- function(cal) {
  cat("\n=== STEP 3: CONSTRUCT AOX-HF (2018 WINDOW) ===\n")

  # Guard: DGS10 must be above 1% to prevent zero-bound distortion
  # In 2018 this guard is always satisfied — DGS10 was 2.4-3.25%
  cal$DGS10_safe <- ifelse(cal$DGS10 > kDgs10Guard,
                            cal$DGS10, NA_real_)

  # AOX-HF formula
  cal$AOXHFprice <- round(
    (cal$ADS_Index + cal$AR_WTI_3yr) / cal$DGS10_safe,
    2L
  )

  # Filter to 2018 test window only (drop 2017 warmup)
  cal_2018 <- cal[cal$Date >= kTsStart[1L] &
                  !is.na(cal$AOXHFprice), ]

  cat(sprintf("AOX-HF 2018: %d rows | %.2f to %.2f\n",
              nrow(cal_2018),
              min(cal_2018$AOXHFprice),
              max(cal_2018$AOXHFprice)))

  # Quarter-by-quarter summary
  cal_2018$Quarter <- quarters(cal_2018$Date)
  cat("\n--- AOX-HF by quarter (2018) ---\n")
  for(q in c("Q1","Q2","Q3","Q4")) {
    sub <- cal_2018[cal_2018$Quarter == q, ]
    if(nrow(sub) == 0) next
    cat(sprintf("  %s: n=%d | WTI mean=$%.2f | AOX-HF mean=%.2f\n",
                q, nrow(sub),
                mean(sub$WTIprice, na.rm = TRUE),
                mean(sub$AOXHFprice, na.rm = TRUE)))
  }

  # DGS10 range check — confirm guard satisfied
  cat(sprintf("\nDGS10 range (2018): %.2f%% to %.2f%% — guard >%.0f%% satisfied: %s\n",
              min(cal_2018$DGS10, na.rm = TRUE),
              max(cal_2018$DGS10, na.rm = TRUE),
              kDgs10Guard,
              ifelse(min(cal_2018$DGS10, na.rm = TRUE) > kDgs10Guard,
                     "YES", "NO")))

  return(cal_2018)
}


# =============================================================================
# 5. REALIZED VOLATILITY + SIGNAL DAY IDENTIFICATION
# =============================================================================
#
# PLAIN ENGLISH:
# For each trading day, we look back at the last 21 closing prices and
# compute how violently WTI has been moving. We annualize this to get a
# percentage that can be compared to a benchmark.
#
# The signal fires on days where this number exceeds 45% AND DGS10 > 1%.
# In plain terms: the signal is active when WTI has been moving at a pace
# that implies extreme annual price swings, AND the Fed is not at zero.

#' Compute realized vol and identify signal days
#'
#' @param cal_2018  2018 calendar data.frame with WTIprice
#' @return cal_2018 with vol_21, vol_smooth, hi_flag columns added
ComputeVolAndSignal <- function(cal_2018) {
  cat("\n=== STEP 4: REALIZED VOL + SIGNAL DAYS ===\n")
  cat(sprintf("Vol threshold: %.0f%% annualized\n",
              kVolThreshold * 100))
  cat(sprintf("DGS10 guard:   >%.0f%%\n", kDgs10Guard))

  n      <- nrow(cal_2018)
  vol_21 <- rep(NA_real_, n)

  # Step 1: daily log return = ln(today's price / yesterday's price)
  # Step 2: std dev of last 21 returns * sqrt(252) = annualized vol
  for(i in (kVolWindow + 1L):n) {
    ret      <- diff(log(cal_2018$WTIprice[(i - kVolWindow):i]))
    vol_21[i] <- stats::sd(ret) * sqrt(252)
  }

  # Step 3: smooth with 5-day rolling mean (prevents single-day spikes
  # from fragmenting the regime into disconnected individual days)
  vol_smooth <- as.vector(stats::filter(
    vol_21, rep(1/kVolSmooth, kVolSmooth), sides = 2L))

  # Step 4: flag signal days — vol > 45% AND DGS10 > 1%
  hi_flag <- !is.na(vol_smooth) &
    vol_smooth >= kVolThreshold &
    cal_2018$DGS10 > kDgs10Guard

  cal_2018$vol_21     <- round(vol_21, 4L)
  cal_2018$vol_smooth <- round(vol_smooth, 4L)
  cal_2018$hi_flag    <- hi_flag

  # Report by quarter
  cat("\n--- Signal days by quarter (vol>45% AND DGS10>1%) ---\n")
  cat(sprintf("%-4s %8s %8s %8s %8s\n",
              "Qtr","Total","Signal","Pct","WTI rng"))
  for(q in c("Q1","Q2","Q3","Q4")) {
    sub <- cal_2018[cal_2018$Quarter == q & !is.na(cal_2018$vol_21), ]
    if(nrow(sub) == 0) next
    hi  <- sum(sub$hi_flag, na.rm = TRUE)
    cat(sprintf("%-4s %8d %8d %7.1f%% $%.0f-$%.0f\n",
                q, nrow(sub), hi, hi/nrow(sub)*100,
                min(sub$WTIprice), max(sub$WTIprice)))
  }

  total_signal <- sum(hi_flag, na.rm = TRUE)
  total_valid  <- sum(!is.na(vol_21))
  cat(sprintf("\nTotal signal days: %d / %d (%.1f%%)\n",
              total_signal, total_valid,
              total_signal/total_valid*100))

  return(cal_2018)
}


# =============================================================================
# 6. STATIONARITY PIPELINE
# =============================================================================
#
# PLAIN ENGLISH:
# VAR models (which underlie the Granger test) require that both series
# (WTI and AOX-HF) are stationary — meaning they don't drift up or down
# over time without bound. We test this with the ADF test. If a series
# is non-stationary, we difference it (subtract yesterday from today)
# until it is. This is identical to what we did in V.1 (2023).

#' Test stationarity and produce differenced series for Granger test
#'
#' @param cal_2018  2018 calendar with WTIprice and AOXHFprice
#' @return List: ts_combined, stationary, stat_dated
RunStationarity2018 <- function(cal_2018) {
  cat("\n=== STEP 5: STATIONARITY PIPELINE ===\n")

  # Build bivariate time series object
  ts_combined <- stats::ts(
    data.frame(cal_2018$WTIprice, cal_2018$AOXHFprice),
    frequency = kTsFreq,
    start     = kTsStart
  )

  # ADF test on raw series
  # H0: series has a unit root (non-stationary)
  # p < 0.05: reject H0, series IS stationary
  # p > 0.05: fail to reject, series is NOT stationary — needs differencing
  wti_ts <- stats::ts(cal_2018$WTIprice,
                      frequency = kTsFreq, start = kTsStart)
  aox_ts <- stats::ts(cal_2018$AOXHFprice,
                      frequency = kTsFreq, start = kTsStart)

  adf_wti <- tseries::adf.test(wti_ts)
  adf_aox <- tseries::adf.test(aox_ts)
  cat(sprintf("ADF WTI raw:    p=%.4f %s\n",
              adf_wti$p.value,
              ifelse(adf_wti$p.value < 0.05,
                     "(stationary)", "(non-stationary — will difference)")))
  cat(sprintf("ADF AOX-HF raw: p=%.4f %s\n",
              adf_aox$p.value,
              ifelse(adf_aox$p.value < 0.05,
                     "(stationary)", "(non-stationary — will difference)")))

  # Joint differencing via diffM() — same as V.1
  # This subtracts yesterday's value from today's for both series
  # simultaneously, preserving their relationship
  stationary <- MTS::diffM(ts_combined)
  stat_clean <- stats::na.omit(as.data.frame(stationary))

  # Verify stationarity after differencing
  adf_stat <- apply(stationary, 2, tseries::adf.test)
  cat(sprintf("ADF WTI diff:   p=%.4f %s\n",
              adf_stat[[1]]$p.value,
              ifelse(adf_stat[[1]]$p.value < 0.05,
                     "(stationary — OK)", "(still non-stationary)")))
  cat(sprintf("ADF AOX-HF diff:p=%.4f %s\n",
              adf_stat[[2]]$p.value,
              ifelse(adf_stat[[2]]$p.value < 0.05,
                     "(stationary — OK)", "(still non-stationary)")))

  # Build dated stationary data frame for regime subsetting
  stat_dated <- data.frame(
    date     = tail(cal_2018$Date, nrow(stat_clean)),
    date_chr = as.character(tail(cal_2018$Date, nrow(stat_clean))),
    wti_diff = stat_clean[, 1L],
    aox_diff = stat_clean[, 2L]
  )

  cat(sprintf("Stationary rows: %d\n", nrow(stat_dated)))

  return(list(
    ts_combined = ts_combined,
    stationary  = stationary,
    stat_dated  = stat_dated
  ))
}


# =============================================================================
# 7. GRANGER CAUSALITY — FULL SAMPLE + SIGNAL REGIME
# =============================================================================
#
# PLAIN ENGLISH:
# Granger causality tests whether yesterday's AOX-HF value helps predict
# today's WTI price movement, beyond what WTI's own history predicts.
# If yes — AOX-HF "Granger-causes" WTI and we have a 1-day leading indicator.
#
# We run this test twice:
#   (1) Full 2018 sample — expected to be non-significant (same as V.3)
#   (2) Signal days only (vol>45%, DGS10>1%) — this is the real test
#
# If lag-1 is significant in the signal window — third confirmation.

#' Run Granger causality on full sample and signal regime
#'
#' @param prep      Output of RunStationarity2018()
#' @param cal_2018  2018 calendar with hi_flag column
#' @return List: granger_full, granger_signal, lag1_pvalue
RunGranger2018 <- function(prep, cal_2018) {
  cat("\n=== STEP 6: GRANGER CAUSALITY TEST ===\n")

  # ── Full sample Granger ──────────────────────────────────────
  # Expected: not significant — same finding as 2022-2025 full sample
  cat("--- Full sample (all 2018 trading days) ---\n")
  cat("    (Expected: NOT significant — signal is regime-conditional)\n")

  var_full     <- vars::VAR(prep$stationary,
                             lag.max = kGrangerLagMax,
                             type    = "none")
  granger_full <- vars::causality(var_full,
                                   cause = "cal_2018.AOXHFprice")
  cat(sprintf("    Granger: F=%.4f | p=%.4f %s\n",
              granger_full$Granger$statistic,
              granger_full$Granger$p.value,
              ifelse(granger_full$Granger$p.value < 0.05,
                     "SIG", "(not sig — expected)")))

  # ── Signal regime Granger ────────────────────────────────────
  # The real test: pool all days where vol>45% AND DGS10>1%
  cat("\n--- Signal regime (vol>45% AND DGS10>1% days pooled) ---\n")
  cat("    THIS IS THE PRIMARY TEST\n")

  signal_dates  <- as.character(
    cal_2018$Date[cal_2018$hi_flag & !is.na(cal_2018$hi_flag)])
  window_signal <- prep$stat_dated[
    prep$stat_dated$date_chr %in% signal_dates, ]

  cat(sprintf("    Signal days in stationary matrix: %d\n",
              nrow(window_signal)))

  if(nrow(window_signal) < 30L) {
    cat("    INSUFFICIENT OBS — need 30+ for valid VAR\n")
    return(NULL)
  }

  var_signal    <- vars::VAR(
    window_signal[, c("wti_diff","aox_diff")],
    lag.max = kGrangerLagMax,
    type    = "none"
  )
  granger_sig   <- vars::causality(var_signal, cause = "aox_diff")

  cat(sprintf("\n    Granger: F=%.4f | p=%.4f %s\n",
              granger_sig$Granger$statistic,
              granger_sig$Granger$p.value,
              ifelse(granger_sig$Granger$p.value < 0.05,
                     "** SIGNAL CONFIRMED **", "(not significant)")))

  # ── Lag-1 coefficient ────────────────────────────────────────
  # Lag-1 = "does yesterday's AOX predict today's WTI?"
  # This is the 1-trading-day lead. Our confirmed lead time.
  cat("\n--- Lag structure (which specific lag drives the result) ---\n")
  coef_wti  <- summary(var_signal)$varresult$wti_diff$coefficients
  aox_coefs <- coef_wti[grep("aox_diff", rownames(coef_wti)), ]
  print(round(aox_coefs, 4L))

  lag1_p <- aox_coefs["aox_diff.l1", "Pr(>|t|)"]
  cat(sprintf("\nLag-1 p=%.4f — %s\n",
              lag1_p,
              ifelse(lag1_p < 0.05,
                     "** 1-DAY LEAD CONFIRMED **",
                     "lag-1 not significant")))

  return(list(
    granger_full   = granger_full,
    granger_signal = granger_sig,
    lag1_pvalue    = lag1_p
  ))
}


# =============================================================================
# 8. MAIN — RUN FULL 2018 BACKTEST
# =============================================================================

#' Execute full 2018 Q4 backtest
#'
#' @return All results (invisibly)
Main <- function() {

  # Step 1: Load and align data
  sources <- LoadAndAlign2018()

  # Step 2: Build AR WTI projection
  cal <- BuildArProjection(sources$cal, sources$wti)

  # Step 3: Construct AOX-HF
  cal_2018 <- ConstructAoxHf2018(cal)

  # Step 4: Compute vol + identify signal days
  cal_2018 <- ComputeVolAndSignal(cal_2018)

  # Step 5: Stationarity pipeline
  prep <- RunStationarity2018(cal_2018)

  # Step 6: Granger causality
  granger_out <- RunGranger2018(prep, cal_2018)

  # Final summary
  cat("\n============================================================\n")
  cat("  2018 Q4 BACKTEST — RESULTS SUMMARY\n")
  cat("------------------------------------------------------------\n")
  cat("  Event: WTI crash Oct-Dec 2018 (-44%, supply glut)\n")
  cat(sprintf("  Signal days (vol>45%%, DGS10>1%%): %d\n",
              sum(cal_2018$hi_flag, na.rm = TRUE)))
  cat(sprintf("  DGS10 guard satisfied: YES (min %.2f%%)\n",
              min(cal_2018$DGS10, na.rm = TRUE)))
  if(!is.null(granger_out)) {
    cat(sprintf("  Granger signal regime: p=%.4f %s\n",
                granger_out$granger_signal$Granger$p.value,
                ifelse(granger_out$granger_signal$Granger$p.value < 0.05,
                       "** CONFIRMED **", "not confirmed")))
    cat(sprintf("  Lag-1 (1-day lead):    p=%.4f %s\n",
                granger_out$lag1_pvalue,
                ifelse(granger_out$lag1_pvalue < 0.05,
                       "** CONFIRMED **", "not confirmed")))
    cat("------------------------------------------------------------\n")
    if(granger_out$lag1_pvalue < 0.05) {
      cat("  RESULT: THIRD INDEPENDENT CONFIRMATION\n")
      cat("  Three shock types confirmed:\n")
      cat("    2018 Q4 — supply glut + demand fear (downside)\n")
      cat("    2022    — Russia-Ukraine shock       (upside)\n")
      cat("    2025    — demand dislocation         (downside)\n")
      cat("  STATUS: SIGNAL IS A PRODUCT\n")
    } else {
      cat("  RESULT: Not confirmed in 2018 window\n")
      cat("  Two confirmations remain (2022, 2025)\n")
    }
  }
  cat("============================================================\n")

  return(invisible(list(
    cal_2018    = cal_2018,
    prep        = prep,
    granger_out = granger_out
  )))
}

# Execute
results <- Main()
