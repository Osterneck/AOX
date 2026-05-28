# AOX v.1 (2023) and AOX-HF v.3 (2026)  WTI Price Forecasting — v.3 Hybrid Ensemble with Regime-Conditional Alpha Signal

**Author:** Alex Osterneck, CLA, MSCS, MSIT
**Entity:** Ai70000, Ltd. (Quantitative Research & Production Lab)  
**Version:** v.3 — May 28, 2026  
**Sequel to:** Multivariate-time-series WTI-forecasting with VAR(p) and VARMA (11/12/23)

---

## Overview

This repository contains the complete production-ready source code and data for the quant-model:

> **WTI Price Forecasting v.3: AOX-HF Hybrid Ensemble with Regime-Conditional Alpha Signal**  
> Alex Osterneck, Ai70000, Ltd. (2026)

The quant-model introduces AOX-HF (Autoregressive Oil Exchange Index — High Frequency), a proprietary macro-adjusted WTI context index, and documents:

- 4.80% MAPE on 10-day forward WTI price forecasting (v.1 benchmark, 2023)
- Three independent Granger causality confirmations across 2015-2025
- A regime-conditional interday alpha signal (vol > 45%, DGS10 > 1%)
- A price-alpha signal: 67% OOS directional accuracy, 1.94% expected value per trade, 2.00 win/loss ratio in supply management shock environments

---

## Disclaimer

This repository and all associated code, indices, signals, and analyses are produced exclusively for institutional counterparties including hedge funds, commodity trading advisors, proprietary trading desks, energy portfolio managers, and institutional procurement operations. Nothing herein constitutes investment advice, a recommendation to buy or sell any security or commodity, or an offer or solicitation of any kind. ai70000, Ltd. does not manage, advise, or handle funds on behalf of retail or individual investors. All hypothetical performance results are for illustrative purposes only. Past performance does not guarantee future results.

---

## Repository Structure

```
AOX-HF-WTI-Forecasting/
├── README.md
├── data/
│   ├── WTI.csv                          # WTI daily closing price 2022-2025
│   ├── AOX.csv                          # AOX original WEI-based 2022-2025
│   ├── AOX_HF_final.csv                 # AOX-HF ADS-based 2022-2025
│   ├── WTI_FRED_full.csv                # FRED DCOILWTICO 1986-2026
│   └── AOX_HF_Workbook_Osterneck_2026.xlsx  # 5-sheet workbook, full input trace
├── scripts/
│   ├── AOX_Signal_Validated_Osterneck_2026.R       # CORE — fully executable
│   ├── AOX_HF_Construction_Osterneck_2026.R        # CORE — fully executable
│   ├── WTI_Forecast_v3_DualIndex_Osterneck_2026.R  # Ensemble — requires setup
│   ├── WTI_Forecast_v3_Pipeline_Osterneck_2026.R   # Ensemble — requires setup
│   ├── AOX_Backtest_2018_Osterneck_2026.R           # Backtest — requires setup
│   └── WTI_Forecast_v1_v2_Chronology.R             # Documentation only
└── paper/
    └── WTI_Forecasting_v3_Paper_Osterneck_2026.docx
```

---

## Reproducing the Core Findings

The validated findings in this project — Granger causality confirmations, signal day analysis, price alpha results — are fully reproducible using two scripts only:

### Step 1 — Install R dependencies

```r
install.packages(c("MTS", "forecast", "tseries", "vars",
                   "MLmetrics", "tidyverse", "readxl", "zoo"))
```

### Step 2 — Download source data

The following public data sources are required. Download and place in your working directory:

| File | Source | URL |
|---|---|---|
| WTI daily prices | Macrotrends | https://www.macrotrends.net/2516/wti-crude-oil-prices-10-year-daily-chart |
| ADS Index | Philadelphia Fed | https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/ads |
| DGS10 | FRED St. Louis Fed | https://fred.stlouisfed.org/series/DGS10 |
| DCOILWTICO | FRED St. Louis Fed | https://fred.stlouisfed.org/series/DCOILWTICO |

Pre-processed versions of all four sources are included in the `data/` directory for convenience.

### Step 3 — Run core scripts

```r
# Set working directory to data/ folder
setwd("path/to/data")

# Script 1: AOX-HF construction
source("../scripts/AOX_HF_Construction_Osterneck_2026.R")

# Script 2: Signal validation (Granger, signal days, price alpha)
source("../scripts/AOX_Signal_Validated_Osterneck_2026.R")
```

### Expected key outputs

```
Stationarity:     WTI p=0.096 → p < 0.01 | AOX p = 0.070 → p < 0.01
VARMA Granger:    F = 1.6302 | p = 0.1224  (full sample — expected NS)
Instantaneous:    p = 0.0480  (co-movement confirmed)
Signal days:      2022 = 112 | 2023 = 0 | 2024  =0 | 2025 = 29 | Total = 141
Granger vol>45%:  F = 2.3023 | p = 0.0775
Lag-1:            p = 0.0315  — 1-DAY LEAD CONFIRMED
```

---

## v.3 Hybrid Ensemble Scripts (Additional Setup Required)

Scripts 3-5 implement the full VARMA + TFT + Chronos-2 ensemble architecture. These require:

- R 4.5+
- Python 3.10+
- R packages: `reticulate`, `torch`, `tft` (via `remotes::install_github("mlverse/tft")`)
- Python packages: `chronos-forecasting`, `torch`, `pandas`, `numpy`

```r
# Install Python dependencies via reticulate
reticulate::py_install(c("chronos-forecasting", "torch",
                          "pandas", "numpy"), pip = TRUE)
```

**Note:** The v.3 ensemble MAPE target of 2.5-3.5% is a stated target. Full ensemble validation is in progress. The core validated findings (Granger confirmations, signal analysis, price alpha) are reproducible using scripts 1 and 2 only and do not require the ensemble environment.

---

## AOX Index — Genesis Equations

### AOX (Original, v.1 2023) — Proprietary, © Ai70000 Ltd.
```
a(t) = ( FRB Weekly Economic Index (WEI) + AR 3-yr futures WTI-NYMEX-price ) / TMUBMUSD10Y
```

### AOX-HF (High Frequency, v.3 2026) — Proprietary, © Ai70000 Ltd.
```
a(t) = ( ADS Business Conditions Index + AR(3,1,2) 3-yr WTI-NYMEX futures ) / DGS10

Guard: DGS10 must exceed 1.00% for signal activation
```

---

## Key Results Summary

| Finding | Result |
|---|---|
| V.1 MAPE (10-day, 2022) | 4.80% — highly accurate |
| Granger full sample 2022-2025 | p = 0.1224 — NS (expected, regime-conditional) |
| Granger vol  >45% pooled | F = 3.15, p = 0.0088 — CONFIRMED |
| Lag-1 lead (geopolitical shock) | p = 0.0315 — 1 trading day |
| Lag-2 lead (supply glut) | p = 0.0049 — 2 trading days |
| 2015-2016 OPEC confirmation | F = 4.28, p = 0.0146 — CONFIRMED |
| 2022 Russia-Ukraine confirmation | p = 0.0315 — CONFIRMED |
| 2025 demand dislocation | p = 0.0315 — CONFIRMED |
| False positives 2023 | 0 / 259 trading days |
| False positives 2024 | 0 / 240 trading days |
| Price alpha OOS hit rate | 67% (supply mgmt shock type) |
| Price alpha expected value | 1.94% per trade / $0.83/bbl |
| Win/loss ratio | 2.00 |

---

## Signal Boundary Conditions

| Condition | Description |
|---|---|
| BC-1 Zero-bound | Signal invalid when DGS10 ≤ 1.0% (COVID 2020 violated this) |
| BC-2 Thin sample | < 40 signal days: insufficient for reliable VAR estimation |
| BC-3 Shock type | Price alpha applies to supply management / trade-war only — not fast geopolitical shocks |

---

## R Environment

```r
R version 4.5.0
MTS 1.2.1
forecast 8.21
tseries 0.10-54
vars 1.6-1
MLmetrics 1.1.1
tidyverse 2.0.0
readxl 1.4.3
zoo 1.8-12
```

---

## Citation

Osterneck, A. (2026). WTI Price Forecasting v.3: AOX-HF Hybrid Ensemble with
Regime-Conditional Alpha Signal. Ai70000, Ltd. Quantitative Research & Production
Laboratory. [Submitted for publication.]

---
## License

© 2023-2026 Alex Osterneck / Ai70000, Ltd. All rights reserved.

The AOX and AOX-HF index formulas, engineering methodology, and signal specifications
are proprietary intellectual property of ai70000, Ltd. The source code in this repository
is made available for peer review and academic reproducibility purposes only.
Commercial use, redistribution, or derivative works require written permission from
Ai70000, Ltd.

---

*Ai70000, Ltd. — Quantitative Research & Production Lab*
