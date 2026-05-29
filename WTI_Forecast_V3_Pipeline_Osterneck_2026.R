# =============================================================================
# WTI PRICE FORECASTING — V.3 DUAL-INDEX PIPELINE (EXECUTION SCRIPT)
# Author   : Alex Osterneck  (CLA, MSCS, MSIT, M.Acc candidate)
# File     : WTI_Forecast_V3_Pipeline_Osterneck_2026.R
# Date     : 05/24/26
# =============================================================================
#
# ─── DATA FILES REQUIRED (place in working directory) ────────────────────────
#
#   WTI.csv          : WTI daily closing price, 2022-01-03 to 2025-12-31
#                      Column: WTIprice (USD/bbl)
#                      Source: Macrotrends, chart_20260524T011951.csv
#                      Faithful to V.1 column naming convention (11/12/23)
#
#   AOX.csv          : Original AOX index, WEI-based, 2022-2025
#                      Column: AOXprice
#                      Formula: (WEI + AR 3-yr WTI futures) / TMUBMUSD10Y
#                      V.1 (2022 only) extended to 2025 via AOX_extended.csv
#                      Confirmed: corr(AOX, AOX-HF) = 0.988-0.9996 by year
#
#   AOX_HF_final.csv : AOX-HF index, ADS-based, 2022-2025
#                      Column: AOXHFprice
#                      Formula: (ADS + AR 3-yr WTI futures) / DGS10
#                      ADF p=0.0009 — stationary in levels
#                      Constructed: AOX_HF_Construction_Osterneck_2026.R
#
# ─── PRE-PROCESSING NOTE ─────────────────────────────────────────────────────
#
#   All three CSVs were aligned and column-named via Option A pre-processing
#   to preserve V.1's exact naming convention (WTIprice, AOXprice).
#   Each file: 1,046 trading days, 2022-01-03 through 2025-12-31.
#   Zero NAs across all three files. Perfect date alignment confirmed.
#
# ─── PIPELINE STRUCTURE ──────────────────────────────────────────────────────
#
#   TRACK 1 — AOX  (original, WEI-based):   Modules 1-4 with AOXprice
#   TRACK 2 — AOX-HF (ADS-based):           Modules 1-4 with AOXHFprice
#   MODULE 5 — Cross-index evaluation:       DM test, Granger comparison,
#                                            MAPE table, final plot
#
#   Source: WTI_Forecast_V3_DualIndex_Osterneck_2026.R (full documented script)
#   This file: clean execution with expanded 2022-2025 dataset loaded.
#
# =============================================================================


# =============================================================================
# 0. CONFIGURATION
# =============================================================================

kTsFreq          <- 252L
kTsStart         <- c(2022L, 1L)
kForecastHorizon <- 21L
kLagMax          <- 21L
kNormalizeInputs <- FALSE
kConfidenceInt   <- 0.95
kMapeBenchmarkV1 <- 0.0480   # V.1: 4.80% MAPE, 10-day, 2022 only, AOX original

# Evaluation window — use 10-day to maintain V.1 comparability
kEvalWindow <- 10L

graphics::par(mar = c(3, 3, 2.5, 2))


# =============================================================================
# 1. PACKAGES
# =============================================================================

library(tidyverse)
library(MTS)
library(ggfortify)
library(forecast)
library(tseries)
library(vars)
library(MLmetrics)
library(reticulate)
library(torch)
library(tft)
# remotes::install_github("mlverse/tft")
# py_install(c("chronos-forecasting","torch","pandas","numpy"), pip=TRUE)

chronos_pkg <- reticulate::import("chronos")
torch_py    <- reticulate::import("torch")
np          <- reticulate::import("numpy")


# =============================================================================
# 2. DATA INGESTION
# =============================================================================
# Column names match V.1 exactly: WTIprice, AOXprice.
# AOX-HF uses AOXHFprice — new column, new index.

#' Load all three pipeline data files
#'
#' @param wti_path    Path to WTI CSV
#' @param aox_path    Path to AOX (WEI-based) CSV
#' @param aox_hf_path Path to AOX-HF (ADS-based) CSV
#' @return List: wti, aox, aox_hf, ts_aox (mts), ts_aox_hf (mts)
LoadData <- function(wti_path    = "WTI.csv",
                     aox_path    = "AOX.csv",
                     aox_hf_path = "AOX_HF_final.csv") {

  wti    <- utils::read.csv(wti_path)
  aox    <- utils::read.csv(aox_path)
  aox_hf <- utils::read.csv(aox_hf_path)

  cat(sprintf("WTI:    %d rows | %s to %s\n",
              nrow(wti), wti$Date[1], wti$Date[nrow(wti)]))
  cat(sprintf("AOX:    %d rows | range: %.2f to %.2f\n",
              nrow(aox), min(aox$AOXprice), max(aox$AOXprice)))
  cat(sprintf("AOX-HF: %d rows | range: %.2f to %.2f\n",
              nrow(aox_hf), min(aox_hf$AOXHFprice), max(aox_hf$AOXHFprice)))

  # Bivariate ts — WTI + AOX (original) — V.1 column order preserved
  ts_aox <- stats::ts(
    data.frame(wti$WTIprice, aox$AOXprice),
    frequency = kTsFreq,
    start     = kTsStart
  )

  # Bivariate ts — WTI + AOX-HF
  ts_aox_hf <- stats::ts(
    data.frame(wti$WTIprice, aox_hf$AOXHFprice),
    frequency = kTsFreq,
    start     = kTsStart
  )

  return(list(
    wti       = wti,
    aox       = aox,
    aox_hf    = aox_hf,
    ts_aox    = ts_aox,
    ts_aox_hf = ts_aox_hf
  ))
}


# =============================================================================
# 3. STATIONARITY PIPELINE
# =============================================================================
# Logic UNCHANGED from V.1. lag=22 preserved (V.1 PACF-derived).
# Parameterized to run for either index track.
# AOX-HF note: ADF p=0.0009 confirmed externally — ndiffs() expected to
# return 0, meaning AOX-HF enters VARMA in levels. Formally confirmed here.

#' Run stationarity pipeline for one WTI + index pair
#'
#' @param aox_wti_ts Multivariate ts (WTI + index)
#' @param wti        WTI data.frame
#' @param index_df   Index data.frame
#' @param index_col  Column name of index prices
#' @param label      "AOX" or "AOX-HF"
#' @return List: index_ts, wti_ts, index_diff, wti_diff, stationary
RunStationarityPipeline <- function(aox_wti_ts,
                                    wti,
                                    index_df,
                                    index_col,
                                    label = "AOX") {
  cat(sprintf("\n=== STATIONARITY PIPELINE [%s] ===\n", label))

  index_ts <- stats::ts(index_df[[index_col]],
                        frequency = kTsFreq, start = kTsStart)
  wti_ts   <- stats::ts(wti$WTIprice,
                        frequency = kTsFreq, start = kTsStart)

  cat("--- ADF: raw series ---\n")
  print(tseries::adf.test(index_ts))
  print(tseries::adf.test(wti_ts))

  cat("\n--- ndiffs() ---\n")
  d_idx <- forecast::ndiffs(index_ts)
  d_wti <- forecast::ndiffs(wti_ts)
  cat(sprintf("%s ndiffs: %d  |  WTI ndiffs: %d\n", label, d_idx, d_wti))

  # lag=22: preserved from V.1 — ~1-month autocorrelation in PACF
  index_diff <- diff(index_ts, differences = 2L, lag = 22L)
  wti_diff   <- diff(wti_ts,   differences = 1L, lag = 22L)

  cat("\n--- ADF: differenced ---\n")
  print(tseries::adf.test(index_diff))
  print(tseries::adf.test(wti_diff))

  stationary <- MTS::diffM(aox_wti_ts)

  cat("\n--- ADF: joint STATIONARY ---\n")
  print(apply(stationary, 2, tseries::adf.test))

  if (kNormalizeInputs) {
    stationary <- apply(stationary, 2,
      function(x) bestNormalize::bestNormalize(x)$x.t)
    cat("Normalization applied.\n")
  }

  return(list(
    index_ts   = index_ts,
    wti_ts     = wti_ts,
    index_diff = index_diff,
    wti_diff   = wti_diff,
    stationary = stationary
  ))
}


# =============================================================================
# 4. MODULE 1 — VARMA
# =============================================================================
# V.1 methodology exactly. Parameterized for either index.
# Primary output for paper: Granger p-value comparison across tracks.
#   V.1 AOX (2022 only):  F=2.19, p=0.017
#   AOX extended (2022-2025): TBD — stability test across regimes
#   AOX-HF (2022-2025):       TBD — primary research question

#' Fit VARMA, Granger causality, IRF, 10-day WTI forecast
#'
#' @param prep       Output of RunStationarityPipeline()
#' @param aox_wti_ts Multivariate ts
#' @param wti        WTI data.frame
#' @param index_name VAR object column name (e.g. "AOX.AOXprice")
#' @param label      "AOX" or "AOX-HF"
#' @return List: var_a, w_inv, wti_forecast, wti_actual, mape, granger, label
RunVarmaModule <- function(prep,
                           aox_wti_ts,
                           wti,
                           index_name = "aox.AOXprice",
                           label      = "AOX") {
  cat(sprintf("\n=== MODULE 1: VARMA [%s] ===\n", label))

  # ACF / PACF / CCF — unchanged from V.1
  stats::acf(prep$index_ts, lag.max = 21L)
  stats::acf(prep$wti_ts,   lag.max = 21L)
  stats::ccf(prep$index_ts, prep$wti_ts, lag.max = 21L)

  graphics::par(mfrow = c(1L, 2L))
  stats::acf(prep$stationary,
             lag.max = length(prep$wti_diff) - 266L, plot = TRUE)
  stats::pacf(prep$stationary,
              lag.max = length(prep$wti_diff) - 266L, plot = TRUE)
  graphics::par(mfrow = c(1L, 1L))

  # Lag selection — AIC, identical to V.1
  cat(sprintf("\n--- VARselect (AIC, lag.max=%d) [%s] ---\n", kLagMax, label))
  print(vars::VARselect(
    prep$stationary,
    lag.max = kLagMax,
    ic      = "AIC",
    type    = "none"
  )[["selection"]])

  # Fit — type='none': no intercept on differenced data
  var_a <- vars::VAR(
    prep$stationary,
    lag.max = kLagMax,
    ic      = "AIC",
    type    = "none"
  )
  print(summary(var_a))

  # Coefficient matrix
  est_coefs <- rbind(
    coef(var_a)[[1L]][, 1L],
    coef(var_a)[[2L]][, 1L]
  )
  cat(sprintf("\n--- Coefficient matrix [%s] ---\n", label))
  print(est_coefs)

  # Granger causality — KEY RESULT
  # V.1 benchmark: F=2.19, p=0.017 (AOX, 2022 only)
  # Extended + AOX-HF results determine paper conclusion
  cat(sprintf("\n--- Granger causality: %s -> WTI ---\n", label))
  granger <- vars::causality(var_a, cause = index_name)
  print(granger)
  cat(sprintf(
    "*** Granger p-value [%s]: %.4f  (V.1 benchmark: 0.0170) ***\n",
    label, granger$Granger$p.value
  ))

  # IRF
  irf_result <- vars::irf(
    var_a,
    impulse  = index_name,
    response = "wti.WTIprice",
    n.ahead  = kForecastHorizon,
    ortho    = FALSE
  )
  plot(irf_result, main = sprintf("IRF: %s shock on WTI", label))

  # Forecast — named extraction (V.2 fix)
  raw_fc <- vars::predict(var_a,
                          n.ahead = kForecastHorizon,
                          ci      = kConfidenceInt,
                          dumvar  = NULL)
  plot(raw_fc)

  # Named extraction — no positional swap bug
  w_diff <- raw_fc$fcst[["wti.WTIprice"]][, "fcst"]

  # Programmatic seed — no hardcoded price
  wti_last <- dplyr::last(as.numeric(aox_wti_ts[, "wti.WTIprice"]))
  w_inv    <- cumsum(w_diff) + wti_last
  cat(sprintf("VARMA seed (last WTI) [%s]: $%.2f/bbl\n", label, wti_last))

  graphics::plot.ts(w_inv,
    main = sprintf("VARMA forecast [%s] — price scale", label),
    ylab = "USD/bbl", xlab = "Trading days ahead")

  # MAPE — 10-day window, V.1 comparability
  wti_actual   <- utils::head(wti$WTIprice, n = kEvalWindow)
  wti_forecast <- w_inv[1L:kEvalWindow]
  mape_varma   <- MLmetrics::MAPE(wti_forecast, wti_actual)
  cat(sprintf(
    "VARMA MAPE [%s] (%d-day): %.4f  |  V.1 benchmark: %.4f\n",
    label, kEvalWindow, mape_varma, kMapeBenchmarkV1
  ))

  return(list(
    var_a        = var_a,
    w_inv        = w_inv,
    wti_forecast = wti_forecast,
    wti_actual   = wti_actual,
    mape         = mape_varma,
    granger      = granger,
    label        = label
  ))
}


# =============================================================================
# 5. MODULE 2 — TFT
# =============================================================================

#' Fit TFT with specified index as predictor
#'
#' @param wti       WTI data.frame
#' @param index_df  Index data.frame
#' @param index_col Column name of index prices
#' @param label     "AOX" or "AOX-HF"
#' @param horizon   Forecast horizon
#' @return List: mod_tft, tft_forecast, wti_actual, mape, label
RunTftModule <- function(wti,
                         index_df,
                         index_col,
                         label   = "AOX",
                         horizon = kForecastHorizon) {
  cat(sprintf("\n=== MODULE 2: TFT [%s] ===\n", label))

  df <- data.frame(
    date        = seq.Date(as.Date("2022-01-03"),
                           by = "day", length.out = nrow(wti)),
    wti_price   = wti$WTIprice,
    index_price = index_df[[index_col]],
    id          = "WTI"
  )

  spec <- tft::tft_dataset_spec(
    df,
    id              = id,
    time            = date,
    outcomes        = wti_price,
    predictors      = c(index_price),
    static_features = c(id),
    lookback        = 21L,
    assess_stop     = horizon
  )

  mod_tft <- tft::temporal_fusion_transformer(spec) |>
    tft::fit(data = df, epochs = 50L, batch_size = 32L, learn_rate = 1e-3)

  tft_preds    <- as.data.frame(predict(mod_tft, df))
  wti_actual   <- utils::tail(wti$WTIprice, horizon)[1L:kEvalWindow]
  tft_forecast <- utils::head(tft_preds$.pred, kEvalWindow)
  mape_tft     <- MLmetrics::MAPE(tft_forecast, wti_actual)
  cat(sprintf("TFT MAPE [%s] (%d-day): %.4f\n", label, kEvalWindow, mape_tft))

  return(list(
    mod_tft      = mod_tft,
    tft_forecast = tft_forecast,
    wti_actual   = wti_actual,
    mape         = mape_tft,
    label        = label
  ))
}


# =============================================================================
# 6. MODULE 3 — CHRONOS-2
# =============================================================================
# CONFIRMED: amazon/chronos-2 (Oct 20, 2025)
# 120M param encoder-only. Native covariate support. API: Chronos2Pipeline.
# AOX and AOX-HF both pass as past_covariates.
# AOX-HF advantage: macro-adjusted, rate-normalized, zero publication lag,
# no energy endogeneity in the covariate signal.

#' Run Chronos-2 zero-shot forecast with index as past covariate
#'
#' @param wti       WTI data.frame
#' @param index_df  Index data.frame
#' @param index_col Column name of index prices
#' @param label     "AOX" or "AOX-HF"
#' @param horizon   Forecast horizon
#' @return List: chronos_point, chronos_lo, chronos_hi,
#'               chronos_forecast, wti_actual, mape, label
RunChronosModule <- function(wti,
                             index_df,
                             index_col,
                             label   = "AOX",
                             horizon = kForecastHorizon) {
  cat(sprintf("\n=== MODULE 3: CHRONOS-2 [%s] ===\n", label))

  pipeline <- chronos_pkg$Chronos2Pipeline$from_pretrained(
    "amazon/chronos-2",
    device_map  = "cpu",
    torch_dtype = torch_py$float32
  )

  wti_tensor <- torch_py$tensor(
    reticulate::r_to_py(matrix(wti$WTIprice, nrow = 1L))
  )
  index_tensor <- torch_py$tensor(
    reticulate::r_to_py(matrix(index_df[[index_col]], nrow = 1L))
  )

  py_out <- pipeline$predict(
    context           = wti_tensor,
    past_covariates   = index_tensor,
    prediction_length = as.integer(horizon),
    num_samples       = 20L
  )

  samples       <- reticulate::py_to_r(np$array(py_out))
  chronos_point <- apply(samples[1L, , ], 2L, stats::median)
  chronos_lo    <- apply(samples[1L, , ], 2L, stats::quantile, 0.10)
  chronos_hi    <- apply(samples[1L, , ], 2L, stats::quantile, 0.90)

  wti_actual       <- utils::tail(wti$WTIprice, horizon)[1L:kEvalWindow]
  chronos_forecast <- chronos_point[1L:kEvalWindow]
  mape_chronos     <- MLmetrics::MAPE(chronos_forecast, wti_actual)
  cat(sprintf("Chronos-2 MAPE [%s] (%d-day): %.4f\n",
              label, kEvalWindow, mape_chronos))

  return(list(
    chronos_point    = chronos_point,
    chronos_lo       = chronos_lo,
    chronos_hi       = chronos_hi,
    chronos_forecast = chronos_forecast,
    wti_actual       = wti_actual,
    mape             = mape_chronos,
    label            = label
  ))
}


# =============================================================================
# 7. MODULE 4 — DYNAMIC ENSEMBLE
# =============================================================================

#' Inverse-MAPE weighted ensemble for one index track
#'
#' @param varma_out   Output of RunVarmaModule()
#' @param tft_out     Output of RunTftModule()
#' @param chronos_out Output of RunChronosModule()
#' @return List: weights, ensemble_forecast, wti_actual, mape, label
.ComputeEnsemble <- function(varma_out, tft_out, chronos_out) {
  label <- varma_out$label
  cat(sprintf("\n=== MODULE 4: ENSEMBLE [%s] ===\n", label))

  mapes   <- c(VARMA = varma_out$mape,
               TFT   = tft_out$mape,
               C2    = chronos_out$mape)
  weights <- (1 / mapes) / sum(1 / mapes)

  cat(sprintf(
    "Weights [%s] — VARMA: %.1f%%  TFT: %.1f%%  Chronos-2: %.1f%%\n",
    label,
    weights["VARMA"] * 100,
    weights["TFT"]   * 100,
    weights["C2"]    * 100
  ))

  ensemble_forecast <-
    weights["VARMA"] * varma_out$wti_forecast +
    weights["TFT"]   * tft_out$tft_forecast  +
    weights["C2"]    * chronos_out$chronos_forecast

  wti_actual    <- varma_out$wti_actual
  mape_ensemble <- MLmetrics::MAPE(ensemble_forecast, wti_actual)
  cat(sprintf(
    "Ensemble MAPE [%s]: %.4f  |  V.1: %.4f  |  Delta: %.2f pp\n",
    label, mape_ensemble, kMapeBenchmarkV1,
    (kMapeBenchmarkV1 - mape_ensemble) * 100
  ))

  return(list(
    weights           = weights,
    ensemble_forecast = ensemble_forecast,
    wti_actual        = wti_actual,
    mape              = mape_ensemble,
    label             = label
  ))
}


# =============================================================================
# 8. MODULE 5 — EVALUATION + DIEBOLD-MARIANO + CROSS-INDEX
# =============================================================================
# Three evaluation layers:
#   (a) Within each track: DM tests vs. V.1 VARMA baseline
#   (b) Granger comparison table: V.1 / AOX extended / AOX-HF
#   (c) Cross-index DM: AOX-HF ensemble vs. AOX ensemble
#   (d) Full MAPE table — all models, both tracks
#   (e) Comparison plot

#' Full evaluation with cross-index Diebold-Mariano test
#'
#' @param varma_aox ... varma_hf, tft_aox ... ensemble_hf
#' @return List: mape_table, granger_table, dm_cross, plot_df
RunEvaluation <- function(varma_aox,   tft_aox,   chronos_aox,   ensemble_aox,
                          varma_hf,    tft_hf,    chronos_hf,    ensemble_hf) {
  cat("\n=== MODULE 5: EVALUATION + CROSS-INDEX DIEBOLD-MARIANO ===\n")

  actual     <- varma_aox$wti_actual
  e_varma_aox    <- actual - varma_aox$wti_forecast
  e_tft_aox      <- actual - tft_aox$tft_forecast
  e_chronos_aox  <- actual - chronos_aox$chronos_forecast
  e_ensemble_aox <- actual - ensemble_aox$ensemble_forecast
  e_varma_hf     <- actual - varma_hf$wti_forecast
  e_tft_hf       <- actual - tft_hf$tft_forecast
  e_chronos_hf   <- actual - chronos_hf$chronos_forecast
  e_ensemble_hf  <- actual - ensemble_hf$ensemble_forecast

  # MAPE table
  mape_table <- data.frame(
    Model = c(
      "V.1 VARMA benchmark (2022, 11/12/23)",
      "VARMA — AOX extended (2022-2025)",
      "VARMA — AOX-HF (2022-2025)",
      "TFT — AOX",
      "TFT — AOX-HF",
      "Chronos-2 — AOX",
      "Chronos-2 — AOX-HF",
      "Ensemble — AOX",
      "Ensemble — AOX-HF"
    ),
    MAPE_10day = round(c(
      kMapeBenchmarkV1,
      varma_aox$mape, varma_hf$mape,
      tft_aox$mape,   tft_hf$mape,
      chronos_aox$mape, chronos_hf$mape,
      ensemble_aox$mape, ensemble_hf$mape
    ), 4L),
    stringsAsFactors = FALSE
  )
  mape_table$vs_V1_pp <- round(
    (kMapeBenchmarkV1 - mape_table$MAPE_10day) * 100, 2L
  )
  cat("\n--- MAPE Summary ---\n")
  print(mape_table)

  # Granger comparison — the paper's primary table
  granger_table <- data.frame(
    Index   = c("V.1 AOX (2022 only, benchmark)",
                "AOX extended (2022-2025)",
                "AOX-HF (2022-2025)"),
    F_stat  = round(c(
      2.19,  # V.1 result
      varma_aox$granger$Granger$statistic,
      varma_hf$granger$Granger$statistic
    ), 4L),
    p_value = round(c(
      0.0170,
      varma_aox$granger$Granger$p.value,
      varma_hf$granger$Granger$p.value
    ), 4L),
    Sig_05  = c(TRUE,
                varma_aox$granger$Granger$p.value < 0.05,
                varma_hf$granger$Granger$p.value  < 0.05),
    stringsAsFactors = FALSE
  )
  cat("\n--- Granger Causality Comparison (THE PRIMARY TABLE) ---\n")
  print(granger_table)

  # DM helper
  .Dm <- function(e_new, e_base, label) {
    dm <- forecast::dm.test(e_new, e_base,
                            alternative = "less", h = 1L, power = 2L)
    sig <- ifelse(dm$p.value < 0.05, "** SIGNIFICANT **", "")
    cat(sprintf("  DM [%s vs V.1 VARMA]: p=%.4f %s\n",
                label, dm$p.value, sig))
    return(dm)
  }

  cat("\n--- Diebold-Mariano: all models vs. V.1 VARMA ---\n")
  dm_varma_aox    <- .Dm(e_varma_aox,    e_varma_aox, "VARMA-AOX")
  dm_tft_aox      <- .Dm(e_tft_aox,      e_varma_aox, "TFT-AOX")
  dm_chronos_aox  <- .Dm(e_chronos_aox,  e_varma_aox, "Chronos2-AOX")
  dm_ensemble_aox <- .Dm(e_ensemble_aox, e_varma_aox, "Ensemble-AOX")
  dm_varma_hf     <- .Dm(e_varma_hf,     e_varma_aox, "VARMA-AOX-HF")
  dm_tft_hf       <- .Dm(e_tft_hf,       e_varma_aox, "TFT-AOX-HF")
  dm_chronos_hf   <- .Dm(e_chronos_hf,   e_varma_aox, "Chronos2-AOX-HF")
  dm_ensemble_hf  <- .Dm(e_ensemble_hf,  e_varma_aox, "Ensemble-AOX-HF")

  # Cross-index DM — definitive test
  cat("\n--- DM Cross-index: AOX-HF Ensemble vs. AOX Ensemble ---\n")
  dm_cross <- forecast::dm.test(e_ensemble_hf, e_ensemble_aox,
                                alternative = "less", h = 1L, power = 2L)
  print(dm_cross)

  if (dm_cross$p.value < 0.05) {
    cat("RESULT: AOX-HF ensemble significantly better. ADS validated. Next-day signal viable.\n")
  } else if (ensemble_hf$mape <= ensemble_aox$mape) {
    cat("RESULT: AOX-HF MAPE <= AOX but not significant at 5%.\n")
    cat("        WEI lag does not materially hurt. Both indexes viable.\n")
    cat("        PAPER: corr(AOX, AOX-HF) = 0.988-0.9996 confirms AOX robustness.\n")
  } else {
    cat("RESULT: AOX (WEI) outperforms AOX-HF (ADS). Investigate construction.\n")
  }

  # Multi-horizon MAPE
  cat("\n--- Multi-horizon MAPE ---\n")
  for (h in c(7L, 14L, 21L)) {
    if (length(varma_aox$wti_forecast) >= h) {
      m_aox <- MLmetrics::MAPE(varma_aox$wti_forecast[1L:h], actual[1L:h])
      m_hf  <- MLmetrics::MAPE(varma_hf$wti_forecast[1L:h],  actual[1L:h])
      cat(sprintf("  %2d-day VARMA: AOX=%.4f  AOX-HF=%.4f\n", h, m_aox, m_hf))
    }
  }

  # Final comparison plot
  plot_df <- data.frame(
    day          = 1L:kEvalWindow,
    Actual       = actual,
    Ensemble_AOX = ensemble_aox$ensemble_forecast,
    Ensemble_HF  = ensemble_hf$ensemble_forecast,
    VARMA_AOX    = varma_aox$wti_forecast,
    VARMA_HF     = varma_hf$wti_forecast
  )
  graphics::matplot(
    plot_df$day, plot_df[, -1L],
    type = "b",
    lty  = c(1L, 1L, 2L, 2L, 3L),
    pch  = c(16L, 15L, 16L, 15L, 1L),
    col  = c("black","darkorange","steelblue","firebrick","darkgreen"),
    lwd  = c(2, 2, 1.5, 1.5, 1),
    main = "WTI V.3 Dual-Index: Ensemble + VARMA vs. Actual (10-day)",
    xlab = "Trading days ahead",
    ylab = "USD/bbl"
  )
  graphics::legend(
    "topleft",
    legend = c("Actual","Ensemble-AOX","Ensemble-AOX-HF","VARMA-AOX","VARMA-AOX-HF"),
    col    = c("black","darkorange","steelblue","firebrick","darkgreen"),
    lty    = c(1L, 1L, 2L, 2L, 3L),
    pch    = c(16L, 15L, 16L, 15L, 1L),
    lwd    = c(2, 2, 1.5, 1.5, 1),
    bty    = "n"
  )

  return(list(
    mape_table    = mape_table,
    granger_table = granger_table,
    dm_cross      = dm_cross,
    dm_ensemble_aox = dm_ensemble_aox,
    dm_ensemble_hf  = dm_ensemble_hf,
    plot_df       = plot_df
  ))
}


# =============================================================================
# 9. MAIN
# =============================================================================

#' Execute full V.3 dual-index pipeline
#'
#' @return All module outputs (invisibly)
Main <- function() {
  cat("============================================================\n")
  cat("  WTI FORECASTING V.3 — DUAL-INDEX PIPELINE\n")
  cat("  Osterneck 2026 | Google R Style Guide\n")
  cat("  Dataset: 2022-01-03 to 2025-12-31 (1,046 trading days)\n")
  cat("  V.1 benchmark: 4.80% MAPE (10-day, AOX, 2022, 11/12/23)\n")
  cat("  TRACK 1: AOX (WEI-based, original + extended)\n")
  cat("  TRACK 2: AOX-HF (ADS-based, 2022-2025)\n")
  cat("============================================================\n\n")

  # Load
  data_out <- LoadData("WTI.csv", "AOX.csv", "AOX_HF_final.csv")

  # ── TRACK 1: AOX ──────────────────────────────────────────
  cat("\n############################################################\n")
  cat("  TRACK 1: AOX — ORIGINAL (WEI-based, extended 2022-2025)\n")
  cat("############################################################\n")

  prep_aox  <- RunStationarityPipeline(
    data_out$ts_aox, data_out$wti, data_out$aox, "AOXprice", "AOX")
  varma_aox <- RunVarmaModule(
    prep_aox, data_out$ts_aox, data_out$wti,
    index_name = "aox.AOXprice", label = "AOX")
  tft_aox     <- RunTftModule(
    data_out$wti, data_out$aox, "AOXprice", label = "AOX")
  chronos_aox <- RunChronosModule(
    data_out$wti, data_out$aox, "AOXprice", label = "AOX")
  ensemble_aox <- .ComputeEnsemble(varma_aox, tft_aox, chronos_aox)

  # ── TRACK 2: AOX-HF ───────────────────────────────────────
  cat("\n############################################################\n")
  cat("  TRACK 2: AOX-HF — HIGH FREQUENCY (ADS-based, 2022-2025)\n")
  cat("############################################################\n")

  prep_hf   <- RunStationarityPipeline(
    data_out$ts_aox_hf, data_out$wti, data_out$aox_hf, "AOXHFprice", "AOX-HF")
  varma_hf  <- RunVarmaModule(
    prep_hf, data_out$ts_aox_hf, data_out$wti,
    index_name = "aox_hf.AOXHFprice", label = "AOX-HF")
  tft_hf     <- RunTftModule(
    data_out$wti, data_out$aox_hf, "AOXHFprice", label = "AOX-HF")
  chronos_hf <- RunChronosModule(
    data_out$wti, data_out$aox_hf, "AOXHFprice", label = "AOX-HF")
  ensemble_hf <- .ComputeEnsemble(varma_hf, tft_hf, chronos_hf)

  # ── MODULE 5: Cross-index evaluation ──────────────────────
  eval_out <- RunEvaluation(
    varma_aox, tft_aox, chronos_aox, ensemble_aox,
    varma_hf,  tft_hf,  chronos_hf,  ensemble_hf
  )

  # Final summary
  cat("\n============================================================\n")
  cat("  V.3 PIPELINE COMPLETE\n")
  cat("  --- MAPE ---\n")
  cat(sprintf("  V.1 benchmark   : %.4f\n", kMapeBenchmarkV1))
  cat(sprintf("  Ensemble AOX    : %.4f  (delta: %.2f pp)\n",
              ensemble_aox$mape,
              (kMapeBenchmarkV1 - ensemble_aox$mape) * 100))
  cat(sprintf("  Ensemble AOX-HF : %.4f  (delta: %.2f pp)\n",
              ensemble_hf$mape,
              (kMapeBenchmarkV1 - ensemble_hf$mape) * 100))
  cat("  --- GRANGER ---\n")
  cat(sprintf("  V.1 AOX (2022)  : p=0.0170\n"))
  cat(sprintf("  AOX extended    : p=%.4f\n",
              varma_aox$granger$Granger$p.value))
  cat(sprintf("  AOX-HF          : p=%.4f\n",
              varma_hf$granger$Granger$p.value))
  cat("  --- CROSS-INDEX DM ---\n")
  cat(sprintf("  AOX-HF vs AOX   : p=%.4f\n",
              eval_out$dm_cross$p.value))
  cat("============================================================\n")

  return(invisible(list(
    data_out     = data_out,
    prep_aox     = prep_aox,    prep_hf      = prep_hf,
    varma_aox    = varma_aox,   varma_hf     = varma_hf,
    tft_aox      = tft_aox,     tft_hf       = tft_hf,
    chronos_aox  = chronos_aox, chronos_hf   = chronos_hf,
    ensemble_aox = ensemble_aox,ensemble_hf  = ensemble_hf,
    eval_out     = eval_out
  )))
}

# Execute
results <- Main()
