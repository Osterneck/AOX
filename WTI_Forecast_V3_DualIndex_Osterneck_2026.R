# =============================================================================
# WTI PRICE FORECASTING — HYBRID ENSEMBLE V.3 (DUAL INDEX)
# Author   : Alex Osterneck  (CLA, MSCS, MSIT, M.Acc candidate)
# File     : WTI_Forecast_V3_DualIndex_Osterneck_2026.R
# Date     : 05/21/26
# =============================================================================
#
# ─── PROJECT LINEAGE ─────────────────────────────────────────────────────────
#
#  V.1 — 11/12/23 — CTU-610 Machine Learning & Neural Networks
#         Language   : R (longhand, no code-assist)
#         Model      : Bivariate VARMA (WTI + AOX)
#         Data       : 2022 daily prices, 309 observations
#         Result     : 4.80% MAPE, 10-day forward window
#         Time       : 7 weeks, ~14 hrs/day — budgeted 2 weeks
#
#  V.2 — 05/21/26 — Hybrid ensemble architecture added
#         Models     : VARMA + TFT + Chronos-2, ensemble-weighted
#         Style      : Functional (snake_case), no explicit namespace
#         Status     : Superseded by V.3
#
#  V.3 — 05/21/26 — This file
#         Models     : VARMA + TFT + Chronos-2, ensemble-weighted
#         Style      : Google R Style Guide (BigCamelCase, ::, return())
#         NEW        : Dual-index architecture — AOX vs. AOX-HF in parallel
#
# ─── V.2 SUMMARY (COMMENTED HISTORICAL RECORD) ───────────────────────────────
#
#  V.2 established the hybrid ensemble: VARMA (interpretable baseline) +
#  TFT (nonlinear pattern capture, mlverse/tft) + Chronos-2 (zero-shot
#  foundation model, amazon/chronos-2, Oct 2025, native covariate support,
#  Chronos2Pipeline API) + inverse-MAPE dynamic ensemble weighting +
#  Diebold-Mariano significance testing. All carried forward into V.3.
#
#  V.1 → V.2 implementation fixes (all in V.3):
#    Named extraction $fcst[["WTI.WTIprice"]] replaces positional [2]
#    Programmatic last() seed replaces hardcoded cumsum() + 80.55
#    TS_FREQ=252 replaces inconsistent 309/365
#    Conditional NORMALIZE flag replaces #### commented block
#    Named functions replace flat sequential script
#    Chronos corrected: amazon/chronos-2 not chronos-t5-small
#
#  V.2 → V.3 style changes (Google R Style Guide):
#    BigCamelCase functions, explicit ::, explicit return(), k constants,
#    dot-prefix private functions, L integer suffixes, 80-char lines
#
# ─── AOX: AUTOREGRESSIVE OIL EXCHANGE INDEX (PROPRIETARY) ────────────────────
#
#  Developed: Alex Osterneck, 2023. Timestamped: CTU-610, 11/12/23.
#
#  AOX (original) — WEI-based:
#    Formula : (FRB Weekly Economic Index + AR 3-yr WTI NYMEX futures)
#              / TMUBMUSD10Y
#    WEI     : NY Fed, Lewis-Mertens-Stock. 10 components — Redbook
#              same-store sales, Rasmussen Consumer Index, initial jobless
#              claims, continued jobless claims, tax withholdings, railroad
#              traffic, ASA Staffing Index, steel production, fuel sales,
#              electricity load. Published weekly (Thursday). ~1-week lag.
#    Interpolation: forward-fill confirmed from console data (two
#              consecutive AOX=69.250 observations in tail(AOX_WTI_ts)).
#    Granger : F=2.19, df1=10, df2=556, p=0.017. Confirmed V.1.
#    Window  : 7–21 day forecasting. Not viable for next-day signals
#              due to WEI publication lag.
#    Endogeneity note: WEI includes fuel sales and electricity load —
#              energy consumption signals partially driven by oil prices.
#              Minor circularity risk with WTI as target.
#
#  AOX-HF (high frequency) — ADS-based:
#    Formula : (ADS Business Conditions Index + AR 3-yr WTI NYMEX futures)
#              / TMUBMUSD10Y
#    ADS     : Philadelphia Fed, Aruoba-Diebold-Scotti (2009). 6 components
#              — weekly initial jobless claims, monthly payroll employment,
#              monthly industrial production, monthly real personal income
#              less transfer payments, monthly real manufacturing and trade
#              sales, quarterly real GDP. Updated ~8x/month in real time
#              as each component releases. Zero publication lag.
#    Advantage 1 — Frequency: daily update cadence vs. WEI weekly Thursday.
#              AOX-HF constructible at market close for next-day signal.
#    Advantage 2 — Endogeneity: ADS contains no energy consumption inputs.
#              No circularity risk with WTI as target. Cleaner instrument.
#    Advantage 3 — Citation: Diebold is both the ADS author and the
#              Diebold-Mariano test author already in this paper. Coherent
#              citation network for peer review.
#    Granger : TBD — tested in this file. Primary research question.
#    Window  : If Granger holds, viable for next-day signal construction.
#
#  DUAL-INDEX DESIGN (this file):
#    Both AOX and AOX-HF run in parallel through the full pipeline.
#    VARMA, TFT, and Chronos-2 each run twice — once per index.
#    Granger causality, MAPE, and Diebold-Mariano compared side by side.
#    If AOX-HF Granger p < 0.05 and MAPE competitive: next-day signal
#    validated. If equivalent: WEI lag does not materially hurt — also
#    a publishable result. Either outcome advances the paper.
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
kMapeBenchmarkV1 <- 0.0480   # V.1: 4.80% MAPE, 10-day, AOX original

graphics::par(mar = c(3, 3, 2.5, 2))


# =============================================================================
# 1. PACKAGES
# =============================================================================

library(ggplot2)
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
#
# Three index files required:
#   WTI.csv     : WTI cash-spot NYMEX daily closing price. Column: WTIprice.
#   AOX.csv     : Original AOX (WEI-based). Column: AOXprice.
#   AOX_HF.csv  : AOX-HF (ADS-based). Column: AOXHFprice.
#                 ADS source: Philadelphia Fed daily CSV.
#                 https://www.philadelphiafed.org/surveys-and-data/
#                   real-time-data-research/ads
#                 Construction: (ADS + AR 3-yr WTI NYMEX) / TMUBMUSD10Y
#                 Same formula as AOX, ADS replaces WEI. No interpolation
#                 required — ADS is already daily frequency.

#' Load WTI, AOX, and AOX-HF data
#'
#' @param wti_path    Path to WTI CSV
#' @param aox_path    Path to AOX (WEI-based) CSV
#' @param aox_hf_path Path to AOX-HF (ADS-based) CSV
#' @return List: wti, aox, aox_hf, ts_aox (mts), ts_aox_hf (mts)
LoadData <- function(wti_path    = "WTI.csv",
                     aox_path    = "AOX.csv",
                     aox_hf_path = "AOX_HF.csv") {
  wti    <- utils::read.csv(wti_path)
  aox    <- utils::read.csv(aox_path)
  aox_hf <- utils::read.csv(aox_hf_path)

  # AOX variant: WTI + AOX (original, WEI-based)
  ts_aox <- stats::ts(
    data.frame(wti$WTIprice, aox$AOXprice),
    frequency = kTsFreq,
    start     = kTsStart
  )

  # AOX-HF variant: WTI + AOX-HF (ADS-based)
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
# Logic UNCHANGED from V.1. Parameterized for either index variant.
# Called twice in Main(): once for AOX, once for AOX-HF.

#' Run stationarity pipeline for one WTI + index variant
#'
#' @param aox_wti_ts  Multivariate ts (WTI + index)
#' @param wti         WTI data.frame
#' @param index_df    Index data.frame (AOX or AOX-HF)
#' @param index_col   Column name in index_df (e.g. "AOXprice")
#' @param label       Label for console output ("AOX" or "AOX-HF")
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

  # lag=22: preserved from V.1 (reflects ~1-month PACF autocorrelation)
  index_diff <- diff(index_ts, differences = 2L, lag = 22L)
  wti_diff   <- diff(wti_ts,   differences = 1L, lag = 22L)

  cat("\n--- ADF: differenced ---\n")
  print(tseries::adf.test(index_diff))
  print(tseries::adf.test(wti_diff))

  stationary <- MTS::diffM(aox_wti_ts)

  cat("\n--- ADF: joint STATIONARY ---\n")
  print(apply(stationary, 2, tseries::adf.test))

  if (kNormalizeInputs) {
    stationary <- apply(
      stationary, 2,
      function(x) bestNormalize::bestNormalize(x)$x.t
    )
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
# Parameterized for either index. Called twice — AOX and AOX-HF.
# V.1 methodology exactly. All three V.2 fixes applied.
# Granger causality result is the primary comparison between the two variants.

#' Fit VARMA, Granger causality, IRF, and 10-day forecast
#'
#' @param prep       Output of RunStationarityPipeline()
#' @param aox_wti_ts Multivariate ts (WTI + index)
#' @param wti        WTI data.frame
#' @param index_name Column name of index in ts (e.g. "AOX.AOXprice")
#' @param label      Label for console output ("AOX" or "AOX-HF")
#' @return List: var_a, w_inv, wti_forecast, wti_actual, mape, granger
RunVarmaModule <- function(prep,
                           aox_wti_ts,
                           wti,
                           index_name = "AOX.AOXprice",
                           label      = "AOX") {
  cat(sprintf("\n=== MODULE 1: VARMA [%s] ===\n", label))

  cat("--- ACF / PACF / CCF ---\n")
  stats::acf(prep$index_ts, lag.max = 21L)
  stats::acf(prep$wti_ts,   lag.max = 21L)
  stats::ccf(prep$index_ts, prep$wti_ts, lag.max = 21L)

  graphics::par(mfrow = c(1L, 2L))
  stats::acf(prep$stationary,
             lag.max = length(prep$wti_diff) - 266L, plot = TRUE)
  stats::pacf(prep$stationary,
              lag.max = length(prep$wti_diff) - 266L, plot = TRUE)
  graphics::par(mfrow = c(1L, 1L))

  cat(sprintf("\n--- VARselect (AIC, lag.max=%d) [%s] ---\n",
              kLagMax, label))
  print(vars::VARselect(
    prep$stationary,
    lag.max = kLagMax,
    ic      = "AIC",
    type    = "none"
  )[["selection"]])

  var_a <- vars::VAR(
    prep$stationary,
    lag.max = kLagMax,
    ic      = "AIC",
    type    = "none"
  )
  print(summary(var_a))

  est_coefs <- rbind(
    coef(var_a)[[1L]][, 1L],
    coef(var_a)[[2L]][, 1L]
  )
  cat(sprintf("\n--- Coefficient matrix [%s] ---\n", label))
  print(est_coefs)

  # Granger causality — KEY COMPARISON between AOX and AOX-HF
  # V.1 AOX result: F=2.19, p=0.017
  # AOX-HF result: TBD — primary research question
  cat(sprintf("\n--- Granger causality: %s -> WTI ---\n", label))
  granger <- vars::causality(var_a, cause = index_name)
  print(granger)
  cat(sprintf(
    "Granger p-value [%s]: %.4f  (V.1 AOX benchmark: 0.0170)\n",
    label, granger$Granger$p.value
  ))

  irf_result <- vars::irf(
    var_a,
    impulse  = index_name,
    response = "WTI.WTIprice",
    n.ahead  = kForecastHorizon,
    ortho    = FALSE
  )
  plot(irf_result,
       main = sprintf("IRF: %s shock on WTI", label))

  raw_fc <- vars::predict(var_a,
                          n.ahead = kForecastHorizon,
                          ci      = kConfidenceInt,
                          dumvar  = NULL)
  plot(raw_fc)

  # Named extraction — V.2 fix (no positional swap bug)
  w_diff <- raw_fc$fcst[["WTI.WTIprice"]][, "fcst"]

  # Programmatic seed — V.2 fix (no hardcoded 80.55)
  wti_last <- dplyr::last(as.numeric(aox_wti_ts[, "WTI.WTIprice"]))
  w_inv    <- cumsum(w_diff) + wti_last
  cat(sprintf("VARMA seed (last WTI) [%s]: $%.2f/bbl\n", label, wti_last))

  graphics::plot.ts(
    w_inv,
    main = sprintf("VARMA forecast [%s]", label),
    ylab = "USD/bbl",
    xlab = "Trading days ahead"
  )

  wti_actual   <- utils::head(wti$WTIprice, n = 10L)
  wti_forecast <- w_inv[1L:10L]
  mape_varma   <- MLmetrics::MAPE(wti_forecast, wti_actual)
  cat(sprintf(
    "VARMA MAPE [%s] (10-day): %.4f  |  V.1 benchmark: %.4f\n",
    label, mape_varma, kMapeBenchmarkV1
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
# Parameterized for either index. Called twice — AOX and AOX-HF.
# AOX-HF as TFT predictor: ADS has no energy circularity — cleaner signal
# for the variable selection network to weight.

#' Fit TFT with specified index as predictor
#'
#' @param wti       WTI data.frame
#' @param index_df  Index data.frame (AOX or AOX-HF)
#' @param index_col Column name in index_df
#' @param label     Label for console output
#' @param horizon   Forecast horizon
#' @return List: mod_tft, tft_forecast, wti_actual, mape, label
RunTftModule <- function(wti,
                         index_df,
                         index_col,
                         label   = "AOX",
                         horizon = kForecastHorizon) {
  cat(sprintf("\n=== MODULE 2: TFT [%s] ===\n", label))

  df <- data.frame(
    date        = seq.Date(as.Date("2022-01-01"),
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
  wti_actual   <- utils::tail(wti$WTIprice, horizon)[1L:10L]
  tft_forecast <- utils::head(tft_preds$.pred, 10L)
  mape_tft     <- MLmetrics::MAPE(tft_forecast, wti_actual)
  cat(sprintf("TFT MAPE [%s] (10-day): %.4f\n", label, mape_tft))

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
# Parameterized for either index as past_covariates.
# AOX-HF as Chronos-2 covariate: passes a macro-adjusted, rate-normalized
# real-activity signal with zero publication lag. Richer context than
# WEI-lagged AOX, no energy endogeneity in the covariate signal.

#' Run Chronos-2 zero-shot forecast with specified index as past covariate
#'
#' @param wti       WTI data.frame
#' @param index_df  Index data.frame (AOX or AOX-HF)
#' @param index_col Column name in index_df
#' @param label     Label for console output
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

  wti_actual       <- utils::tail(wti$WTIprice, horizon)[1L:10L]
  chronos_forecast <- chronos_point[1L:10L]
  mape_chronos     <- MLmetrics::MAPE(chronos_forecast, wti_actual)
  cat(sprintf("Chronos-2 MAPE [%s] (10-day): %.4f\n", label, mape_chronos))

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
# Private function. Runs for one index variant at a time.

#' Compute inverse-MAPE weighted ensemble for one index variant
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
# 8. MODULE 5 — EVALUATION + DIEBOLD-MARIANO + CROSS-INDEX COMPARISON
# =============================================================================
# Three layers of evaluation:
#   (a) Within each index: DM tests for ensemble/TFT/Chronos vs. V.1 VARMA
#   (b) Between indexes: DM test AOX-HF ensemble vs. AOX ensemble
#   (c) Summary table: all MAPEs side by side for paper
#   (d) Granger comparison: AOX p=0.017 vs. AOX-HF p=TBD
#   (e) Plot: all forecasts, both index variants, vs. actual

#' Full evaluation including cross-index DM comparison
#'
#' @param varma_aox      RunVarmaModule() output, AOX variant
#' @param tft_aox        RunTftModule() output, AOX variant
#' @param chronos_aox    RunChronosModule() output, AOX variant
#' @param ensemble_aox   .ComputeEnsemble() output, AOX variant
#' @param varma_hf       RunVarmaModule() output, AOX-HF variant
#' @param tft_hf         RunTftModule() output, AOX-HF variant
#' @param chronos_hf     RunChronosModule() output, AOX-HF variant
#' @param ensemble_hf    .ComputeEnsemble() output, AOX-HF variant
#' @return List: mape_table, granger_table, dm_cross, plot_df
RunEvaluation <- function(varma_aox,   tft_aox,   chronos_aox,   ensemble_aox,
                          varma_hf,    tft_hf,    chronos_hf,    ensemble_hf) {
  cat("\n=== MODULE 5: EVALUATION + CROSS-INDEX COMPARISON ===\n")

  actual <- varma_aox$wti_actual   # same actual prices for all models

  # Error vectors — AOX variant
  e_varma_aox    <- actual - varma_aox$wti_forecast
  e_tft_aox      <- actual - tft_aox$tft_forecast
  e_chronos_aox  <- actual - chronos_aox$chronos_forecast
  e_ensemble_aox <- actual - ensemble_aox$ensemble_forecast

  # Error vectors — AOX-HF variant
  e_varma_hf    <- actual - varma_hf$wti_forecast
  e_tft_hf      <- actual - tft_hf$tft_forecast
  e_chronos_hf  <- actual - chronos_hf$chronos_forecast
  e_ensemble_hf <- actual - ensemble_hf$ensemble_forecast

  # MAPE summary table — all models, both variants
  mape_table <- data.frame(
    Model = c(
      "V.1 VARMA (benchmark 11/12/23)",
      "VARMA — AOX (original)",
      "VARMA — AOX-HF",
      "TFT — AOX",
      "TFT — AOX-HF",
      "Chronos-2 — AOX",
      "Chronos-2 — AOX-HF",
      "Ensemble — AOX",
      "Ensemble — AOX-HF"
    ),
    MAPE_10day = round(c(
      kMapeBenchmarkV1,
      varma_aox$mape,
      varma_hf$mape,
      tft_aox$mape,
      tft_hf$mape,
      chronos_aox$mape,
      chronos_hf$mape,
      ensemble_aox$mape,
      ensemble_hf$mape
    ), 4L),
    stringsAsFactors = FALSE
  )
  mape_table$vs_V1_pp <- round(
    (kMapeBenchmarkV1 - mape_table$MAPE_10day) * 100, 2L
  )
  cat("\n--- MAPE Summary (all models, both index variants) ---\n")
  print(mape_table)

  # Granger comparison table — the primary research question
  granger_table <- data.frame(
    Index    = c("AOX (WEI-based)", "AOX-HF (ADS-based)"),
    F_stat   = round(c(
      varma_aox$granger$Granger$statistic,
      varma_hf$granger$Granger$statistic
    ), 4L),
    p_value  = round(c(
      varma_aox$granger$Granger$p.value,
      varma_hf$granger$Granger$p.value
    ), 4L),
    Sig_05   = c(
      varma_aox$granger$Granger$p.value < 0.05,
      varma_hf$granger$Granger$p.value  < 0.05
    ),
    stringsAsFactors = FALSE
  )
  cat("\n--- Granger Causality Comparison ---\n")
  print(granger_table)
  cat("V.1 AOX benchmark: F=2.19, p=0.017\n")

  # DM helper
  .Dm <- function(e_new, e_base, label) {
    dm <- forecast::dm.test(e_new, e_base,
                            alternative = "less", h = 1L, power = 2L)
    cat(sprintf("DM [%s vs V.1 VARMA]: p=%.4f %s\n",
                label, dm$p.value,
                ifelse(dm$p.value < 0.05, "SIGNIFICANT", "")))
    return(dm)
  }

  cat("\n--- Diebold-Mariano: each model vs. V.1 VARMA ---\n")
  dm_varma_aox    <- .Dm(e_varma_aox,    e_varma_aox, "VARMA-AOX")
  dm_tft_aox      <- .Dm(e_tft_aox,      e_varma_aox, "TFT-AOX")
  dm_chronos_aox  <- .Dm(e_chronos_aox,  e_varma_aox, "Chronos2-AOX")
  dm_ensemble_aox <- .Dm(e_ensemble_aox, e_varma_aox, "Ensemble-AOX")
  dm_varma_hf     <- .Dm(e_varma_hf,     e_varma_aox, "VARMA-AOX-HF")
  dm_tft_hf       <- .Dm(e_tft_hf,       e_varma_aox, "TFT-AOX-HF")
  dm_chronos_hf   <- .Dm(e_chronos_hf,   e_varma_aox, "Chronos2-AOX-HF")
  dm_ensemble_hf  <- .Dm(e_ensemble_hf,  e_varma_aox, "Ensemble-AOX-HF")

  # Cross-index DM: AOX-HF ensemble vs. AOX ensemble
  # The definitive test: does ADS produce a statistically better ensemble?
  cat("\n--- DM Cross-index: AOX-HF Ensemble vs. AOX Ensemble ---\n")
  dm_cross <- forecast::dm.test(e_ensemble_hf, e_ensemble_aox,
                                alternative = "less", h = 1L, power = 2L)
  print(dm_cross)
  if (dm_cross$p.value < 0.05) {
    cat("RESULT: AOX-HF ensemble significantly better than AOX ensemble.\n")
    cat("        ADS replacement validated. Next-day signal viable.\n")
  } else if (ensemble_hf$mape <= ensemble_aox$mape) {
    cat("RESULT: AOX-HF MAPE <= AOX MAPE but not significant at 5%.\n")
    cat("        WEI lag does not materially hurt performance.\n")
    cat("        Both indexes viable. Extend window for significance.\n")
  } else {
    cat("RESULT: AOX (WEI) outperforms AOX-HF (ADS).\n")
    cat("        Investigate ADS construction or data alignment.\n")
  }

  # Multi-horizon MAPE — both variants
  cat("\n--- Multi-horizon MAPE ---\n")
  for (h in c(7L, 14L, 21L)) {
    if (length(varma_aox$wti_forecast) >= h) {
      m_aox <- MLmetrics::MAPE(varma_aox$wti_forecast[1L:h], actual[1L:h])
      m_hf  <- MLmetrics::MAPE(varma_hf$wti_forecast[1L:h],  actual[1L:h])
      cat(sprintf("  %2d-day: AOX=%.4f  AOX-HF=%.4f\n", h, m_aox, m_hf))
    }
  }

  # Comparison plot: all ensemble forecasts + actual
  plot_df <- data.frame(
    day          = 1L:10L,
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
    col  = c("black", "darkorange", "steelblue", "firebrick", "darkgreen"),
    lwd  = c(2, 2, 1.5, 1.5, 1),
    main = "WTI V.3 Dual-Index: Ensemble Comparison vs. Actual",
    xlab = "Trading days ahead",
    ylab = "USD/bbl"
  )
  graphics::legend(
    "topleft",
    legend = c("Actual", "Ensemble-AOX", "Ensemble-AOX-HF",
               "VARMA-AOX", "VARMA-AOX-HF"),
    col    = c("black", "darkorange", "steelblue", "firebrick", "darkgreen"),
    lty    = c(1L, 1L, 2L, 2L, 3L),
    pch    = c(16L, 15L, 16L, 15L, 1L),
    lwd    = c(2, 2, 1.5, 1.5, 1),
    bty    = "n"
  )

  return(list(
    mape_table    = mape_table,
    granger_table = granger_table,
    dm_cross      = dm_cross,
    plot_df       = plot_df
  ))
}


# =============================================================================
# 9. MAIN
# =============================================================================

#' Execute full dual-index WTI forecasting pipeline
#'
#' @return All module outputs (invisibly)
Main <- function() {
  cat("============================================================\n")
  cat("  WTI FORECASTING V.3 — DUAL INDEX — Osterneck 2026\n")
  cat("  AOX (WEI-based)  vs.  AOX-HF (ADS-based)\n")
  cat("  V.1 benchmark: 4.80% MAPE (10-day, AOX, 11/12/23)\n")
  cat("  Research question: does ADS improve Granger + MAPE?\n")
  cat("============================================================\n\n")

  # Load all three data files
  data_out <- LoadData("WTI.csv", "AOX.csv", "AOX_HF.csv")

  # --- AOX (original, WEI-based) track ---
  cat("\n############################################################\n")
  cat("  TRACK 1: AOX — ORIGINAL (WEI-based)\n")
  cat("############################################################\n")

  prep_aox <- RunStationarityPipeline(
    aox_wti_ts = data_out$ts_aox,
    wti        = data_out$wti,
    index_df   = data_out$aox,
    index_col  = "AOXprice",
    label      = "AOX"
  )
  varma_aox   <- RunVarmaModule(prep_aox, data_out$ts_aox, data_out$wti,
                                index_name = "AOX.AOXprice", label = "AOX")
  tft_aox     <- RunTftModule(data_out$wti, data_out$aox,
                               "AOXprice", label = "AOX")
  chronos_aox <- RunChronosModule(data_out$wti, data_out$aox,
                                  "AOXprice", label = "AOX")
  ensemble_aox <- .ComputeEnsemble(varma_aox, tft_aox, chronos_aox)

  # --- AOX-HF (ADS-based) track ---
  cat("\n############################################################\n")
  cat("  TRACK 2: AOX-HF — HIGH FREQUENCY (ADS-based)\n")
  cat("############################################################\n")

  prep_hf <- RunStationarityPipeline(
    aox_wti_ts = data_out$ts_aox_hf,
    wti        = data_out$wti,
    index_df   = data_out$aox_hf,
    index_col  = "AOXHFprice",
    label      = "AOX-HF"
  )
  varma_hf   <- RunVarmaModule(prep_hf, data_out$ts_aox_hf, data_out$wti,
                               index_name = "AOX.HF.AOXHFprice",
                               label = "AOX-HF")
  tft_hf     <- RunTftModule(data_out$wti, data_out$aox_hf,
                              "AOXHFprice", label = "AOX-HF")
  chronos_hf <- RunChronosModule(data_out$wti, data_out$aox_hf,
                                 "AOXHFprice", label = "AOX-HF")
  ensemble_hf <- .ComputeEnsemble(varma_hf, tft_hf, chronos_hf)

  # --- Cross-index evaluation ---
  eval_out <- RunEvaluation(
    varma_aox, tft_aox, chronos_aox, ensemble_aox,
    varma_hf,  tft_hf,  chronos_hf,  ensemble_hf
  )

  # Final summary
  cat("\n============================================================\n")
  cat("  V.3 DUAL-INDEX COMPLETE\n")
  cat("  --- MAPE ---\n")
  cat(sprintf("  Ensemble AOX    : %.4f\n", ensemble_aox$mape))
  cat(sprintf("  Ensemble AOX-HF : %.4f\n", ensemble_hf$mape))
  cat(sprintf("  V.1 benchmark   : %.4f\n", kMapeBenchmarkV1))
  cat("  --- GRANGER ---\n")
  cat(sprintf("  AOX    p-value  : %.4f\n",
              varma_aox$granger$Granger$p.value))
  cat(sprintf("  AOX-HF p-value  : %.4f\n",
              varma_hf$granger$Granger$p.value))
  cat("  --- CROSS-INDEX DM ---\n")
  cat(sprintf("  AOX-HF vs AOX   : p=%.4f\n",
              eval_out$dm_cross$p.value))
  cat("============================================================\n")

  return(invisible(list(
    data_out     = data_out,
    prep_aox     = prep_aox,
    prep_hf      = prep_hf,
    varma_aox    = varma_aox,
    tft_aox      = tft_aox,
    chronos_aox  = chronos_aox,
    ensemble_aox = ensemble_aox,
    varma_hf     = varma_hf,
    tft_hf       = tft_hf,
    chronos_hf   = chronos_hf,
    ensemble_hf  = ensemble_hf,
    eval_out     = eval_out
  )))
}

results <- Main()
