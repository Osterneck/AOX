# =============================================================================
# WTI FORECASTING — BEFORE / AFTER CHRONOLOGY
# V.1 (11/12/23)  →  V.2 (05/21/26)
# Author: Alex Osterneck
#
# PURPOSE OF THIS FILE
# --------------------
# Side-by-side documentation of every meaningful change from V.1 to V.2.
# Each section shows the original V.1 code exactly as written, followed by
# the V.2 equivalent, followed by the reason for the change.
#
# What is NOT in this file: logic changes. There are none.
# Every methodological decision — ADF, ndiffs, diffM, AIC lag selection,
# Granger causality, IRF, cumsum inversion, MAPE evaluation — is identical
# in V.1 and V.2. This file documents only implementation changes.
# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# CHANGE 1: TIME-SERIES FREQUENCY CONSTANT
# ─────────────────────────────────────────────────────────────────────────────
#
# V.1 — BEFORE
# Two inconsistent values used across the script with no explanation:

AOX.ts <- ts(AOX$AOXprice, frequency = 309, start = c(2022, 1), end = c(2022, 309))
WTI.ts <- ts(WTI$WTIprice, frequency = 309, start = c(2022, 1), end = c(2022, 309))
# ... then later in the same script:
AOX_WTI_ts <- ts(AOX_WTI, frequency = 365, start = c(2022, 1))

# V.2 — AFTER
# Single canonical constant defined once at the top of the script:

TS_FREQ <- 252     # financial time series convention: trading days per year
AOX.ts  <- ts(AOX$AOXprice, frequency = TS_FREQ, start = c(2022, 1))
WTI.ts  <- ts(WTI$WTIprice, frequency = TS_FREQ, start = c(2022, 1))
AOX_WTI_ts <- ts(AOX_WTI,   frequency = TS_FREQ, start = c(2022, 1))

# WHY: 309 was the row count of the dataset (a data artifact, not a frequency).
# 365 was calendar days (ignores weekends and market holidays).
# 252 is the standard financial convention for trading-day frequency.
# Using one constant eliminates the inconsistency and makes the intent explicit.
# If the dataset changes, one line changes, not three scattered across the script.


# ─────────────────────────────────────────────────────────────────────────────
# CHANGE 2: VARIABLE SWAP WORKAROUND → NAMED LIST EXTRACTION
# ─────────────────────────────────────────────────────────────────────────────
#
# V.1 — BEFORE
# The comment in the original script read:
# "if first line throws object-error use 2nd line first then enter 1st line"
# This was a live workaround for a positional extraction ambiguity:

WTI.WTIprice <- forecast$fcst[2]; AOX.AOXprice
AOX.AOXprice <- forecast$fcst[1]; WTI.WTIprice

A <- WTI.WTIprice$AOX.AOXprice[, 1]; A
W <- AOX.AOXprice$WTI.WTIprice[, 1]; W

# V.2 — AFTER
# Extract by name. Always correct regardless of column ordering:

W_diff <- raw_forecast$fcst[["WTI.WTIprice"]][, "fcst"]
A_diff <- raw_forecast$fcst[["AOX.AOXprice"]][, "fcst"]

# WHY: forecast$fcst is a named list. Positional extraction ([1], [2]) depends
# on the column order of the input data frame, which is not guaranteed to be
# stable if data is refreshed or reordered. Named extraction is deterministic.
# The V.1 workaround was correct behavior for a live session but would silently
# produce wrong forecasts if the column order ever changed.


# ─────────────────────────────────────────────────────────────────────────────
# CHANGE 3: HARDCODED cumsum() SEED → PROGRAMMATIC last()
# ─────────────────────────────────────────────────────────────────────────────
#
# V.1 — BEFORE
# The last observed WTI price was read manually from tail() output and
# hardcoded as a literal:

tail(AOX_WTI_ts)       # run manually, read 80.55 from console output
W <- cumsum(W) + 80.55 # literal from manual inspection

# V.2 — AFTER
# Computed programmatically — works correctly on any dataset refresh:

wti_last <- dplyr::last(as.numeric(AOX_WTI_ts[, "WTI.WTIprice"]))
W_inv    <- cumsum(W_diff) + wti_last

# WHY: 80.55 was the last observed price on 11/12/23. If the dataset is
# extended to include 2023-2025 data, 80.55 is wrong and the entire price-scale
# inversion is wrong. The programmatic version is always correct because it
# reads the last value from whatever data is currently loaded.
# This is the highest-risk bug in V.1 from a data-refresh perspective.


# ─────────────────────────────────────────────────────────────────────────────
# CHANGE 4: REPEATED par(mar=...) → SET ONCE GLOBALLY
# ─────────────────────────────────────────────────────────────────────────────
#
# V.1 — BEFORE
# Margins set manually before each plot (appeared 4 times in the script):

par(mar = c(2.5, 2.5, 2.5, 2.5))
plot(forecast)
# ... code ...
par(mar = c(2.5, 2.5, 1, 2.5))
plot.ts(W)
# ... code ...
par(mfrow = c(1, 2))
acf(STATIONARY, ...)
pacf(STATIONARY, ...)

# V.2 — AFTER
# Set once at the top of the script in a global configuration block:

par(mar = c(3, 3, 2.5, 2))   # set once; all plots inherit this

# WHY: Repeated par() calls are noise. More importantly, if you want to change
# margins globally (e.g., for a different output device), you change one line
# instead of hunting through the script for all occurrences.
# The par(mfrow) calls for multi-panel plots are still local — that's correct,
# since mfrow is layout-specific rather than a global aesthetic preference.


# ─────────────────────────────────────────────────────────────────────────────
# CHANGE 5: COMMENTED-OUT NORMALIZATION → CONDITIONAL FLAG
# ─────────────────────────────────────────────────────────────────────────────
#
# V.1 — BEFORE
# The normalization block was entirely commented out with #### markers,
# making it invisible as an option and impossible to enable without
# manually uncommenting multiple lines:

####install.packages('bestNormalize')
####library(bestNormalize)
####bestNormalize(AOX_ts, mode = 'scale')
####bestNormalize(WTI_ts, mode = 'scale')

# V.2 — AFTER
# A clean boolean flag at the top of the script; the block runs or skips:

NORMALIZE_INPUTS <- FALSE   # set TRUE to enable

if (NORMALIZE_INPUTS) {
  library(bestNormalize)
  STATIONARY <- apply(STATIONARY, 2, function(x) bestNormalize(x)$x.t)
  cat("Normalization applied.\n")
}

# WHY: The normalization decision is real — Alex considered it and chose not
# to apply it on the 2022 data, which was reasonable since WTI and AOX were
# on comparable scales. But burying it in #### comments erases it as an option.
# The flag makes the decision explicit, documented, and trivially reversible.


# ─────────────────────────────────────────────────────────────────────────────
# CHANGE 6: invisible() WRAPPER → STANDARD ASSIGNMENT
# ─────────────────────────────────────────────────────────────────────────────
#
# V.1 — BEFORE
# invisible() used to suppress console output from the coefficient extraction:

invisible(est_coefs <- coef(var.a))

# V.2 — AFTER
# Standard assignment; console output suppressed where needed with
# suppressMessages() or by not printing intermediate objects:

est_coefs <- rbind(coef(var.a)[[1]][, 1],
                   coef(var.a)[[2]][, 1])
cat("\n--- Estimated coefficient matrix ---\n")
print(est_coefs)

# WHY: invisible(x <- expr) is an anti-pattern. It combines assignment and
# output suppression in one expression, which is confusing to read and
# masks the fact that est_coefs is being assigned at all. The V.2 version
# separates the assignment (always happens) from the printing (explicitly
# controlled). The coefficients are also now printed with a header label,
# making the output more readable in a run log.


# ─────────────────────────────────────────────────────────────────────────────
# CHANGE 7: FLAT SCRIPT → NAMED FUNCTIONS
# ─────────────────────────────────────────────────────────────────────────────
#
# V.1 — BEFORE
# The entire analysis was a single flat sequential script approximately
# 200 lines long with no functions. Every line ran at the top level.
# To re-run any part, the entire script had to be re-executed from the top
# or the relevant lines had to be manually selected and run.
#
# Representative V.1 structure (abbreviated):

setwd("path/to/your/data")  # set to your working directory
AOX <- read.csv("AOX.csv")
WTI <- read.csv("WTI.csv")
AOX_WTI <- data.frame(WTI$WTIprice, AOX$AOXprice)
AOX_WTI_ts <- ts(AOX_WTI, frequency = 365, start = c(2022, 1))
plot(AOX_WTI_ts)
# ... 180 more lines at the global level ...

# V.2 — AFTER
# Each logical stage is a named function with explicit inputs and outputs:

LoadData <- function(wti_path = "WTI.csv", aox_path = "AOX.csv") {
  # ... returns list(WTI, AOX, AOX_WTI_ts)
}

RunStationarityPipeline <- function(aox_wti_ts, wti, aox) {
  # ... returns list(AOX.ts, WTI.ts, aox_diff, wti_diff, STATIONARY)
}

RunVarmaModule <- function(prep, aox_wti_ts, wti) {
  # ... returns list(var.a, W_inv, WTI_forecast, WTI_actual, mape, granger)
}

# WHY: Named functions serve three purposes:
# (1) Any module can be re-run in isolation without re-running the full script.
# (2) The interface between stages is explicit — you can see exactly what
#     each module needs and what it produces.
# (3) V.3 and beyond can reuse individual modules without copying code.
# This is the single largest structural improvement from V.1 to V.2.


# ─────────────────────────────────────────────────────────────────────────────
# CHANGE 8: MIXED COMMENT STYLE → UNIFORM SECTION HEADERS
# ─────────────────────────────────────────────────────────────────────────────
#
# V.1 — BEFORE
# Four different comment styles used inconsistently in the same script:

# run forecast on VAR model to determine what price-range over next 50 days
####install.packages('bestNormalize')   # ← quadruple hash for disabled code
library(MLmetrics)
#MAPE between 10% and 25% is low accuracy but still in acceptable range.
#now run VAR(p), which is VAR(2) model on combined multivariate ts

# V.2 — AFTER
# One consistent system: === for major sections, --- for subsections,
# inline # for single-line notes, block comments for multi-line explanation:

# =============================================================================
# 4. MODULE 1 — VARMA BASELINE
# =============================================================================

# --- ADF / PACF ---

var.a <- vars::VAR(STATIONARY, lag.max = LAG_MAX, ic = 'AIC', type = 'none')
# type='none': correct for differenced (stationary) data — no intercept needed

# WHY: Consistent structure makes it trivially easy to navigate a 400-line
# script. The section markers also serve as natural breakpoints when running
# the script interactively — you can see at a glance where each module starts.


# ─────────────────────────────────────────────────────────────────────────────
# CHANGE 9: PIPE OPERATOR — UNUSED IN V.1, INTRODUCED IN V.2
# ─────────────────────────────────────────────────────────────────────────────
#
# V.1 — BEFORE
# The tidyverse was imported (library(tidyverse)) but the pipe operator
# was never used. All operations were written as nested calls:

autoplot(ts(STATIONARY, start = c(2022, 1), frequency = 365)) +
  theme(plot.title = element_text(hjust = 0.5))

# V.2 — AFTER
# Native pipe |> (R 4.1+) used for sequential operations:

ts(STATIONARY, start = c(2022, 1), frequency = TS_FREQ) |>
  autoplot() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

# WHY: The pipe makes the sequence of operations readable left-to-right.
# "Take STATIONARY, make it a ts, autoplot it, apply theme" is the logical
# order and the code now reads in that order. The native pipe |> is used
# rather than magrittr's %>% to avoid the additional dependency.


# ─────────────────────────────────────────────────────────────────────────────
# CHANGE 10: CHRONOS MODEL STRING CORRECTION
# ─────────────────────────────────────────────────────────────────────────────
#
# V.2 FIRST DRAFT — WRONG
# The V.2 script initially used the original 2024 Chronos model string:

pipeline <- chronos$ChronosPipeline$from_pretrained(
  "amazon/chronos-t5-small",    # ← this is the 2024 original, NOT Chronos-2
  device_map  = "cpu",
  torch_dtype = torch_py$float32
)

# V.2 CORRECTED — RIGHT
# Chronos-2 was released October 20, 2025. It has a completely different
# model ID, a different architecture (encoder-only vs. encoder-decoder),
# and a different inference API:

pipeline <- chronos2$Chronos2Pipeline$from_pretrained(
  "amazon/chronos-2",           # ← confirmed Chronos-2 model ID
  device_map  = "cpu",
  torch_dtype = torch_py$float32
)

# WHY THIS MATTERS:
# Chronos (2024): univariate only. Cannot accept AOX as a covariate.
#   Model string: "amazon/chronos-t5-small" / base / large
#   Architecture: T5 encoder-decoder, 4096 token vocabulary
#   API: ChronosPipeline
#
# Chronos-Bolt (Nov 2024): faster variant of original Chronos, still univariate.
#   Model string: "amazon/chronos-bolt-small" / base
#   Architecture: T5 encoder-decoder, patch-based
#   API: ChronosPipeline
#
# Chronos-2 (Oct 20, 2025): the version V.2 actually uses.
#   Model string: "amazon/chronos-2"
#   Architecture: 120M parameter encoder-only transformer
#   Parameters: 120M
#   API: Chronos2Pipeline (different class from ChronosPipeline)
#   Key advantage for V.2: native covariate support — AOX can be passed
#   directly as a past covariate alongside WTI, which original Chronos
#   and Chronos-Bolt cannot do natively.


# ─────────────────────────────────────────────────────────────────────────────
# WHAT ALEX GOT RIGHT IN V.1 — COMPLETE RECORD
# ─────────────────────────────────────────────────────────────────────────────
#
# This section documents every correct methodological decision in V.1.
# None of these changed in V.2.
#
# 1. COVARIATE SELECTION — AOX (CBOE Oil Volatility Index)
#    Most analysts would have used Brent as the second variable. Alex used
#    AOX — the implied volatility index for crude oil futures. This is a
#    sophisticated choice because implied vol embeds forward-looking market
#    expectations, not just realized prices. Granger causality confirmed
#    the choice was correct (F=2.19, p=0.017).
#
# 2. ADF TESTING PROTOCOL
#    Both series tested for stationarity independently before joint modeling.
#    p-values correctly interpreted (need < 0.05 for stationarity).
#    ndiffs() used to determine the correct differencing order rather than
#    assuming d=1. This is Hyndman-correct practice.
#
# 3. JOINT STATIONARITY VIA diffM()
#    Rather than differencing each series separately and recombining,
#    Alex used diffM() from the MTS package for joint multivariate
#    differencing, then verified stationarity on the combined STATIONARY
#    object. Both series returned ADF p < 0.01 after joint differencing.
#    This is the correct procedure for VAR pre-processing.
#
# 4. LAG SELECTION VIA AIC
#    VARselect() with lag.max=21 and ic='AIC' selected p=10.
#    This is aggressive (20 coefficient pairs) but defensible: WTI has
#    weekly cyclicality (~5-day trading week), so lags 5 and 10 carry
#    genuine predictive information. AIC selected p=10 over BIC's p=1,
#    correctly prioritizing in-sample fit for a short forecast horizon.
#
# 5. type='none' IN VAR()
#    Fitting VAR without an intercept on the STATIONARY (differenced) data
#    is correct. The intercept belongs in the levels model, not the
#    differenced model.
#
# 6. GRANGER CAUSALITY CONFIRMATION
#    causality(var.a, cause="AOX.AOXprice") was run before accepting the
#    model. Result: F=2.19, df1=10, df2=556, p=0.017. Correctly interpreted:
#    AOX Granger-causes WTI at the 5% level. The model structure is
#    statistically validated, not just fit.
#
# 7. IMPULSE RESPONSE FUNCTION
#    irf() computed and plotted with n.ahead=21, ortho=FALSE.
#    This shows the trajectory of WTI response to a one-unit shock in AOX
#    over a 21-day horizon. This was above and beyond the course requirement
#    and demonstrates understanding of what VAR models actually reveal.
#
# 8. cumsum() INVERSION
#    After forecasting on the stationary (differenced) series, correctly
#    inverted back to price scale using cumulative sum plus the last
#    observed price as the seed. The logic is: each differenced forecast
#    value is a price change; cumsum converts changes back to levels.
#    The seed (last observed price) anchors the forecast to current reality.
#    This step is where many practitioners make errors. Alex got it right.
#
# 9. MAPE EVALUATION AGAINST ACTUAL PRICES
#    MAPE computed against actual WTI prices on a 10-day forward window.
#    Result: 4.80%. Correctly noted that MAPE between 10-25% is considered
#    acceptable for commodity forecasting; 4.80% is well within that range.
#
# 10. RETICULATE BRIDGE
#     library(reticulate) and py_install("pandas") were included in V.1,
#     demonstrating awareness that R has ecosystem gaps and Python fills them.
#     This foresight is directly used in V.2 for the Chronos-2 integration.
#     V.2 did not introduce reticulate — it was already in V.1.
