# RSFME: An Assessment of Annual Load Estimation Methods in Small Watersheds for Cross Site Comparisons

**Authors:** Nic Gubbins (Colorado State University), Weston M. Slaughter (UMD), Michael J. Vlah (Duke), Spencer Rhea (Duke), William H. McDowell (UNH/FIU), Emily S. Bernhardt (Duke), Matthew R.V. Ross (CSU)

**Target journal:** Hydrology and Earth System Sciences (HESS)

**Contact:** gubbins@colostate.edu

## Overview

This repository contains the code and data pipeline for evaluating four common solute load estimation methods (linear interpolation, Beale ratio, rating, composite) by coarsening high-frequency sensor data from three monitoring networks:

- **Hubbard Brook Experimental Forest (HBEF)** — 15-min sensor data, Watershed 3
- **Plynlimon Research Catchments** — 7-hour auto-sampler data, Upper Hafren
- **NEON** — Daily sensor data from 6 first-order stream sites

The methods are then applied to the [MacroSheds](https://macrosheds.org) synthesis dataset (~93 sites, 112 solutes), producing a publicly available dataset of 16,489 site-years of annual load estimates.

## Repository Structure

```
RSFME/
├── source/                      # Core analysis functions
│   ├── flux_methods.R           #   Load estimation methods (PW, Beale, Rating, Composite)
│   ├── plot_theme.R             #   Shared ggplot2 theme and palettes
│   └── calculate_annual_flux.R  #   MacroSheds annual load computation
├── data/                        # All input and intermediate data (see data/README.md)
├── paper/
│   ├── source/                  # Numbered analysis scripts (01–15)
│   │   └── 00_run_all.R         #   Runner script for full pipeline
│   └── figures/                 # All output figures (30 PNGs)
├── plans/                       # Development plans and decisions log
└── CLAUDE.md                    # Detailed project documentation
```

## Reproducing the Analysis

### Prerequisites

- **R 4.4+**
- Required packages: `tidyverse`, `here`, `feather`, `forecast`, `lfstat`, `lubridate`, `RiverLoad`, `zoo`, `patchwork`, `EGRET`, `macrosheds`

### Data Setup

1. **MacroSheds data:** Download EDI package `edi.1262.2` from the EDI Data Portal and unzip into `data/macrosheds/`.
2. **HBEF sensor data:** Place `w3_sensor_wdisch.feather` in the repository root. This file is proprietary and not included — contact the lead author for access.
3. **Other data files** are included in the repository under `data/`.

See `data/README.md` for detailed provenance of each data file.

### Running

From the repository root in R:

```r
source("paper/source/00_run_all.R")
```

To resume from a specific script (e.g., skip the long-running analysis scripts):

```bash
Rscript paper/source/00_run_all.R 5
```

Scripts 01, 04, 07, and 09 run coarsening analyses with 100 repetitions each and may take 30+ minutes. Scripts 02–03, 05–06, 08, 10–15 generate figures and complete in seconds.

### Outputs

- **Figures** are written to `paper/figures/`, named by figure number (e.g., `fig07_hbef_ca_coarsening.png`).
- **Intermediate data** (coarsening results, simulation outputs) are written to subdirectories of `data/`.
- **Annual load estimates** are written to `data/load_annual.csv`.

## Script Run Order

| # | Script | Description |
|---|--------|-------------|
| 01 | `01_ts_simulation_analysis.R` | ARIMA simulation and coarsening experiment |
| 02 | `02_ts_simulation_figure.R` | Simulation results figure (supplement) |
| 03 | `03_ts_descriptive_figures.R` | Hydrologic and C:Q regime panels |
| 04 | `04_coarsen_analysis_hbef.R` | HBEF coarsening experiment (Ca, NO3) |
| 05 | `05_coarsen_figure_hbef.R` | HBEF coarsening figures (Figs 7–8) |
| 06 | `06_coarsen_example_figure.R` | Coarsening example illustration (Fig 6) |
| 07 | `07_coarsen_analysis_plynlimon.R` | Plynlimon coarsening experiment |
| 08 | `08_coarsen_figure_plynlimon.R` | Plynlimon coarsening figures (Figs 9–10) |
| 09 | `09_coarsen_analysis_neon.R` | NEON coarsening experiment |
| 10 | `10_coarsen_figure_neon.R` | NEON coarsening figures (supplement) |
| 11 | `11_macrosheds_compare.R` | MacroSheds method comparison figures |
| 12 | `12_macrosheds_descriptive.R` | MacroSheds load distribution figure |
| 13 | `13_ca_correlation.R` | Ca–SpCond regression (helper, sourced by 14) |
| 14 | `14_misc_figures.R` | Raw data and C:Q plots (Figs 2–5) |
| 15 | `15_hbef_method_comparison.R` | HBEF method comparison (Fig 11) |

## License

Data availability and usage terms are described in the paper's Data Availability section.
