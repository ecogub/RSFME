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
- **Workflow after every milestone or sub-milestone:** do the work → commit → push → update CLAUDE.md → update `plans/decisions_made.txt`
- **This CLAUDE.md must be updated after all major changes** to keep it an accurate map of the project.
- **`plans/decisions_made.txt`** is a running log of all significant decisions. Update it whenever a non-trivial choice is made about code, paper structure, methods, or figures.
- **Commit early, commit often.** Each sub-milestone (M2a, M2b, etc.) gets its own commit so changes are reviewable.

## Repository Structure

```
RSFME/
├── source/
│   └── flux_methods.R          # Core flux computation functions (PW, Beale, Rating, Composite, WRTDS)
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
- R packages: tidyverse, RiverLoad, EGRET, macrosheds, feather, here, lubridate, lfstat, patchwork, imputeTS, forecast, zoo
- External data: `w3_sensor_wdisch.feather` (HBEF high-freq sensor data, currently only on desktop - needs to be moved into repo/data)

## M0 Review: Areas of Improvement

### Code Quality Issues

**Critical:**
1. **Hardcoded absolute paths** in 6+ scripts pointing to `C:/Users/gubbi/desktop/w3_sensor_wdisch.feather`. This file needs to be in the repo and referenced via `here()`.
2. **Loop variable collision bugs** - In `1_coarsen_analysis.R` and `1_coarsen_analysis_plynlimon.R`, the outer solute loop variable `i` is clobbered by inner loop variables, potentially causing incorrect results or only processing the last solute.
3. **Possible result accumulation bug** in `1_coarsen_analysis.R` - `out_tbl` is reinitialized inside the rep loop, so only the last rep's method results may survive per coarsening level.
4. **Undefined variables** - `flag` in `misc_figs.R` (line 102) and `test` in `ms_application_compare.R` (line 84) will cause runtime errors.
5. **File format mismatch** - Plynlimon analysis saves `.RData` but the figure script reads `.csv`.

**Structural:**
6. **Massive code duplication** - HBEF/Plynlimon/NEON analysis scripts are near-copies. The coarsen analysis, figure generation, and C:Q plotting code is repeated 3x with minor parameter changes.
7. **No shared configuration** - Watershed areas, water years, site codes, and the Ca conversion coefficient (0.06284158) are hardcoded as magic numbers across multiple files.
8. **Row-by-row `rbind()` in loops** - Severe performance anti-pattern in coarsen_analysis (HBEF, Plynlimon, NEON) and ms_application_compare.R. Should use list accumulation + `bind_rows()`.
9. **Global variable dependencies** - `calculate_truth_ts.R` relies on `dn` and `target_wy` from the calling environment instead of taking them as parameters.
10. **No data pipeline** - Data files scattered (desktop, data/, paper/ subdirs), no clear way to reproduce from scratch.

**Minor:**
11. **Deprecated ggplot2 usage** - `size` should be `linewidth` for line-based geoms throughout all plotting scripts.
12. **Unused package imports** everywhere (forecast, xts, imputeTS loaded in plotting scripts that don't use them).
13. **Error band labels possibly swapped** in `2_coarsen_figure.R` (5% and 20% fill labels reversed).
14. **Defunct scripts** in `ts_simulation/defunct/` should be cleaned up or clearly archived.

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
