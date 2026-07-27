# RSFME - River Solute Flux Method Estimation

## Paper Overview

**Title:** An Assessment of Annual Load Estimation Methods in Small Watersheds for Cross Site Comparisons
**Target Journal:** HESS (Hydrology and Earth System Sciences)
**Lead Author:** Nic Gubbins (Colorado State University)
**Coauthors:** Weston Slaughter (UMD), Michael Vlah (Duke), Spencer Rhea (Duke), William McDowell (UNH/FIU), Emily Bernhardt (Duke), Matthew Ross (CSU)

The paper evaluates four common solute load estimation methods (linear interpolation, Beale ratio, rating, composite) by coarsening high-frequency sensor data from HBEF, Plynlimon, and NEON sites. It then applies these methods to the MacroSheds synthesis dataset (~93 sites, 104 solutes) and proposes a decision framework for classifying confidence in load estimates for cross-site comparisons.

## Development Process

This project uses **milestone-driven development**. See `plans/` for current milestone details.

- **M0:** Initial codebase and paper review (this document)
- **M1:** Plan creation and subsequent milestone definition (to be done collaboratively)
- Future milestones will be defined in M1.

### Important Conventions
- **Workflow after every milestone or sub-milestone:** do the work → update CLAUDE.md → update `plans/decisions_made.txt` → commit → push
- **This CLAUDE.md must be updated after all major changes** to keep it an accurate map of the project.
- **`plans/decisions_made.txt`** is a running log of all significant decisions. Update it whenever a non-trivial choice is made about code, paper structure, methods, or figures.
- **Commit early, commit often.** Each sub-milestone (M2a, M2b, etc.) gets its own commit so changes are reviewable.

## Repository Structure

```
RSFME/
├── source/
│   ├── flux_methods.R          # Core flux computation functions (PW, Beale, Rating, Composite, WRTDS)
│   └── plot_theme.R            # Shared HESS-compliant ggplot2 theme, palettes, and ggsave_hess()
├── ms_overwrites.R             # Unit/molecule conversion utilities for MacroSheds data
├── paper/
│   ├── paper_HESS_draft_v2.docx  # Current working draft
│   ├── Run Order.txt             # Execution order for analysis scripts
│   ├── coarsen_plot/             # HBEF data coarsening experiment (Figs 9-10)
│   ├── ts_simulation/            # ARIMA-based synthetic time series experiments (Fig 8)
│   ├── plynlimon_discussion/     # Plynlimon replication of coarsening (Figs 11-12)
│   ├── neon_discussion/          # NEON sensor data coarsening (Figs a1-a8)
│   ├── macrosheds_application/   # MacroSheds dataset load estimation (Fig a9)
│   ├── hbef_comparison_fig/      # Method comparison against sensor truth (Fig 14)
│   ├── hbef_corr_exploration/    # Ca-SpCond regression analysis (Fig 2 inset)
│   ├── misc_figure_creation/     # C:Q plots and raw data figures (Figs 2-5)
│   ├── method_illustration/      # Method illustration diagram (Fig 1, PNG only)
│   └── flowchart/                # Decision framework flowchart (Fig 15, PNGs only)
├── plans/                        # Milestone plans and decisions log
└── CLAUDE.md                     # This file
```

### Script Run Order (from `paper/Run Order.txt`)
1. `ts_simulation/` - ARIMA fitting and simulation experiments
2. `coarsen_plot/` - HBEF data coarsening experiments
3. `plynlimon_discussion/` - Plynlimon replication
4. `macrosheds_application/` - MacroSheds load estimates
5. `hbef_corr_exploration/` - Ca-SpCond investigation
6. `hbef_comparison_fig/` - Method comparison figure
7. `misc_figure_creation/` - Supporting plots

### Key Dependencies
- R packages: tidyverse, RiverLoad, EGRET, macrosheds, feather, here, lubridate, lfstat, patchwork, forecast, zoo, ggthemes, cowplot
- External data: `w3_sensor_wdisch.feather` (HBEF high-freq sensor data, in repo root, gitignored — proprietary)

## M0 Review: Areas of Improvement

### Code Quality Issues

**Critical (all fixed in M2a/M2b):**
1. ~~Hardcoded absolute paths~~ — Fixed: all 7 scripts now use `here('w3_sensor_wdisch.feather')`. NEON script uses `MACROSHEDS_ROOT` env var.
2. ~~Loop variable collisions~~ — Fixed: renamed to `solute_var`, `coarse_n`, `k` to avoid shadowing.
3. ~~Result accumulation bug~~ — Verified not a bug: R for-loops iterate all elements regardless of variable modification.
4. ~~Undefined variables~~ — Fixed: removed `flag` from misc_figs.R, changed `test` to `n_frame` in ms_application_compare.R.
5. ~~File format mismatch~~ — Fixed: Plynlimon analysis now saves `.csv` matching figure script.

**Structural (M2c — mostly fixed):**
6. **Massive code duplication** - HBEF/Plynlimon/NEON analysis scripts are near-copies. Deferred to avoid risk before verification — will revisit after M2d.
7. **No shared configuration** - Watershed areas, water years, site codes, and the Ca conversion coefficient (0.06284158) are hardcoded as magic numbers across multiple files. Deferred — low risk and low urgency.
8. ~~Row-by-row `rbind()` in loops~~ — Fixed: converted to list accumulation + `bind_rows()` in 5 scripts (ts_simulation, ms_application_compare, coarsen HBEF/Plynlimon/NEON).
9. ~~Global variable dependencies~~ — Fixed: `calculate_truth_ts.R` now takes `dn` and `target_wy` as explicit parameters. Monthly branch `q_df` vs `q_df_add` bug also fixed.
10. **No data pipeline** - Data files in various locations, no clear way to reproduce from scratch. `w3_sensor_wdisch.feather` now in repo root (gitignored, proprietary).
11. ~~Performance: data re-read inside rep loop~~ — Fixed: `1_ts_simulation_analysis.R` now reads data, fits ARIMA, and defines functions once outside the loop.

**Minor (all fixed in M2c):**
12. ~~Deprecated ggplot2 usage~~ — Fixed: `size` → `linewidth` for line-based geoms in all plotting scripts. Also fixed `lwd` → `linewidth` in NEON figure.
13. ~~Unused package imports~~ — Fixed: removed unused `library()` calls across 15 scripts. Added missing RiverLoad/lubridate where needed for sourced flux_methods.R.
14. ~~Error band labels swapped~~ in `2_coarsen_figure.R` — Fixed: renamed fill keys to descriptive `band_5pct`/`band_20pct` with explicit named mapping.
15. **Defunct scripts** in `ts_simulation/defunct/` should be cleaned up or clearly archived.
16. ~~Plynlimon site_code hardcoded as 'w3'~~ — Fixed: now uses the `site_code` variable (set to 'UHF').
17. ~~"Enchanced" typo~~ in `3_descriptive_figures.R` — Fixed: → "Enhanced".
18. ~~Debug `plot()` call~~ inside ts_simulation rep loop — Removed.
19. ~~Copy-paste bug~~ in `1_ts_simulation_analysis.R`: "no pattern / base flow" truth used `simulated_series[[4]]` (chemostatic) instead of `[[5]]` (no-pattern). Fixed.
20. ~~Missing closing braces~~ in `1_coarsen_analysis.R`: j and coarse_n loops were never closed. Pre-existing since before M2. Fixed to match Plynlimon version structure.

### Paper Text Issues

**Structural:**
1. **Missing "Results" header** - The section after Methods is labeled "Conclusions" but contains Results content. True conclusions are absent.
2. **Missing Figure 13** - Paper references Figure 14 but there is no Figure 13.
3. **NEON results underserved** - Just says "see Appendix" with a brief paragraph; needs proper treatment.
4. **Table 1 content missing** from extracted text (NEON site descriptions).
5. **Placeholder text** - "FINAL VERSION LINK" in Data Availability section.

**Writing Quality:**
6. **Typos** - "Plylimon" (should be Plynlimon), "Enchanced" (Enhanced), date range "6/19/20166/22/2016" (missing dash).
7. **Equation rendering** - Equations 1-3 appear blank in the docx (symbols not rendering).
8. **Works Cited formatting** inconsistent (some have DOIs, some don't; mixed date formats).
9. **Repetition** between Results observations and Discussion points.

### Figure Quality Issues
1. Figures are generated as individual PNGs with inconsistent styling across scripts.
2. The decision flowchart (Fig 15) is a manually-created PNG - not reproducible from code.
3. Method illustration (Fig 1) is also a manual PNG.
4. No unified theme or color palette across figure scripts.
