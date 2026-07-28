# RSFME - River Solute Flux Method Estimation

## Paper Overview

**Title:** An Assessment of Annual Load Estimation Methods in Small Watersheds for Cross Site Comparisons
**Target Journal:** HESS (Hydrology and Earth System Sciences)
**Lead Author:** Nic Gubbins (Colorado State University)
**Coauthors:** Weston Slaughter (UMD), Michael Vlah (Duke), Spencer Rhea (Duke), William McDowell (UNH/FIU), Emily Bernhardt (Duke), Matthew Ross (CSU)

The paper evaluates four common solute load estimation methods (linear interpolation, Beale ratio, rating, composite) by coarsening high-frequency sensor data from HBEF, Plynlimon, and NEON sites. It then applies these methods to the MacroSheds synthesis dataset (~93 sites, 104 solutes) and proposes a decision framework for classifying confidence in load estimates for cross-site comparisons.

## Development Process

This project uses **milestone-driven development**. See `plans/` for current milestone details.

- **M0:** Initial codebase and paper review — complete
- **M1:** Plan creation — complete (see `plans/m1_planning.md`)
- **M2:** Fix the codebase — complete (M2a–M2d all done; shared config extracted to `source/config.R`; defunct cleanup deferred)
- **M3:** Improve figures — complete (M3a–M3d all done)
- **M4:** Reconcile narrative text — complete (structure fixes, NEON expansion, Conclusions, quantitative audit, post-fix coarsening text corrections, caption fixes all done; Nic-owned items moved to M10)
- **M5:** Organize repo — complete (M5a–M5d done; end-to-end `00_run_all.R` test struck)
- **M6:** Figure 11 panel improvement — complete (A/B panel with 1:1 scatter + difference bar chart; caption updated)
- **M7:** Adversarial code review — 24/25 fixes applied. Ca~SpCond model unified (free intercept, HBEF CSV data, config constants). Self-referential truth confirmed deliberate. Simulation scripts (01–03) removed from repo and paper; remaining scripts renumbered 01–12. Only remaining: global `area` variable (deferred — requires function signature refactor). **All analysis scripts need rerun (M7e).**
- **M8:** Figure improvements (post-rerun) — complete (HBEF crop/color fix, NEON dynamic y-axis per method, facet spacing, stale data cleanup, Figure 11 truth recalculated as direct 15-min integration, captions updated, accuracy sweep done: 7 text fixes applied)
- **M9:** Pre-submission adversarial review fixes — **COMPLETE** except the Nic-owned figshare re-upload. The 2026-07-28 review graded the manuscript 65/100 and found two blocking defects, both fixed: (1) "linear interpolation" actually called RiverLoad `method1`, a mean-C × mean-Q averaging estimator — switched to `method6` and `calculate_pw` renamed **`calculate_li`**; (2) the x-axis tick labels on Figs 7–10 were wrong (the "Weekly" tick was 4-day sampling) — breaks corrected. Full rerun clean (80.8 min, 0 failures). ~60 text corrections, Results/Discussion reorganised, MacroSheds methods split with a new **Table 2**, Figure 11 given a Methods and Results home, **Figures A10–A12** added for three previously orphaned assets, appendix relabelled A1–A12, Data Availability moved after the Conclusions. **Outstanding: figshare re-upload + DOI bump (Nic), and manual replacement of Figures 2 and 4 in Word (M10d).**
- **M10:** Final tasks — **M10b complete** (9 citation year/name mismatches fixed). Remaining Nic-owned: M10a missing references (Appling 2015, Nava 2019, Colin/Neal 2013, NEON — all cited but absent from Works Cited), M10c uncited entries and uncited claims, M10d manual replacement of Figures 2 and 4, M10e formatting + final read-through.

### Important Conventions
- **Workflow after every milestone or sub-milestone:** do the work → update CLAUDE.md → update `plans/decisions_made.txt` → commit → push
- **This CLAUDE.md must be updated after all major changes** to keep it an accurate map of the project.
- **`plans/decisions_made.txt`** is a running log of all significant decisions. Update it whenever a non-trivial choice is made about code, paper structure, methods, or figures.
- **Commit early, commit often.** Each sub-milestone (M2a, M2b, etc.) gets its own commit so changes are reviewable.

## Repository Structure

```
RSFME/
├── source/
│   ├── config.R                 # Shared constants (watershed areas, site codes, target water years, Ca conversion coefficients)
│   ├── flux_methods.R           # Core flux computation functions (PW, Beale, Rating, Composite, WRTDS)
│   ├── plot_theme.R             # Shared HESS-compliant ggplot2 theme, palettes, and ggsave_hess()
│   └── calculate_annual_flux.R  # MacroSheds annual load estimation (reads data/macrosheds/, writes data/load_annual.csv)
├── ms_overwrites.R              # Unit/molecule conversion utilities for MacroSheds data
├── data/
│   ├── README.md                # Data provenance and download instructions
│   ├── coarsen_hbef/            # HBEF coarsening results (.RData)
│   ├── coarsen_plynlimon/       # Plynlimon coarsening results (.csv)
│   ├── coarsen_neon/            # NEON coarsening results (.RData)
│   ├── hbef/                    # HBEF chemistry CSV
│   ├── hbef_published_flux/     # Published HBEF monthly flux data
│   ├── macrosheds/              # EDI download (edi.1262.2) — site metadata, timeseries CSVs
│   ├── neon/                    # NEON stream order CSV
│   ├── plynlimon/               # Plynlimon high-frequency hydrochemistry CSV
│   ├── load_annual.csv          # Computed annual loads (output of calculate_annual_flux.R)
│   └── load_annual_diagnostics.csv
├── paper/
│   ├── paper_HESS_draft_v2.docx          # Original draft (READ-ONLY)
│   ├── paper_HESS_draft_v2_claude_final.docx  # Working copy with all edits applied
│   ├── source/                  # All analysis + figure scripts, numbered in execution order (00–12)
│   │   ├── 00_run_all.R         # Pipeline runner: sources 01–12 in order with timing and error handling
│   │   ├── 01–12_*.R            # Numbered scripts (see Script Run Order below)
│   │   ├── coarsen_helpers.R    # Shared coarsening experiment function
│   │   └── calculate_truth_ts.R # Truth computation helper
│   └── figures/                 # All output figures (PNGs)
├── plans/                       # Milestone plans and decisions log
├── w3_sensor_wdisch.feather     # HBEF sensor data (PROPRIETARY — gitignored)
└── CLAUDE.md                    # This file
```

### Script Run Order (all in `paper/source/`)
| # | Script | Produces |
|---|--------|----------|
| 00 | `00_run_all.R` | Pipeline runner — sources 01–12 in order; accepts `start_from` arg |
| 01 | `01_coarsen_analysis_hbef.R` | HBEF coarsening results → `data/coarsen_hbef/` |
| 02 | `02_coarsen_figure_hbef.R` | `fig07_hbef_ca_coarsening.png`, `fig08_hbef_no3_coarsening.png` |
| 03 | `03_coarsen_example_figure.R` | `fig06_coarsen_example.png` |
| 04 | `04_coarsen_analysis_plynlimon.R` | Plynlimon coarsening results → `data/coarsen_plynlimon/` |
| 05 | `05_coarsen_figure_plynlimon.R` | `fig09_plynlimon_ca_coarsening.png`, `fig10_plynlimon_no3_coarsening.png` |
| 06 | `06_coarsen_analysis_neon.R` | NEON coarsening results → `data/coarsen_neon/` |
| 07 | `07_coarsen_figure_neon.R` | `figa_neon_cond_*.png`, `figa_neon_turb_*.png` (8 supplement figs) |
| 08 | `08_macrosheds_compare.R` | `figa9_macrosheds_density.png`, `figa9_macrosheds_method_comp.png` |
| 09 | `09_macrosheds_descriptive.R` | `figa9_macrosheds_load_hist.png` |
| 10 | `10_ca_correlation.R` | Ca–SpCond regression (sourced by 11, no standalone output) |
| 11 | `11_misc_figures.R` | `fig02–fig05` raw data and C:Q plots (8 PNGs) |
| 12 | `12_hbef_method_comparison.R` | `fig11_hbef_method_ts.png`, `fig11_hbef_method_comparison.png` |

### Key Dependencies
- R packages: tidyverse, RiverLoad, EGRET, macrosheds, feather, here, lubridate, lfstat, patchwork, zoo, imputeTS, cowplot
- External data: `w3_sensor_wdisch.feather` (HBEF high-freq sensor data, in repo root, gitignored — proprietary)

### R Configuration
- R 4.4.1 is installed at `C:\Program Files\R\R-4.4.1\bin\Rscript.exe` but is NOT on the system PATH.
- To run R scripts from the command line, prepend the path or set: `export PATH="$PATH:/c/Program Files/R/R-4.4.1/bin"`

## M0 Review: Areas of Improvement

### Code Quality Issues

**Critical (all fixed in M2a/M2b):**
1. ~~Hardcoded absolute paths~~ — Fixed: all 7 scripts now use `here('w3_sensor_wdisch.feather')`. NEON script uses `MACROSHEDS_ROOT` env var.
2. ~~Loop variable collisions~~ — Fixed: renamed to `solute_var`, `coarse_n`, `k` to avoid shadowing.
3. ~~Result accumulation bug~~ — Verified not a bug: R for-loops iterate all elements regardless of variable modification.
4. ~~Undefined variables~~ — Fixed: removed `flag` from misc_figs.R, changed `test` to `n_frame` in ms_application_compare.R.
5. ~~File format mismatch~~ — Fixed: Plynlimon analysis now saves `.csv` matching figure script.

**Structural (M2c — mostly fixed):**
6. ~~Massive code duplication~~ — Fixed: extracted `run_coarsening_experiment()` into `paper/source/coarsen_helpers.R`, reducing HBEF/Plynlimon/NEON scripts from ~110 lines each to ~35.
7. ~~No shared configuration~~ — Fixed: extracted constants to `source/config.R` (HBEF_AREA, PLYN_AREA, site codes, target water years, CA_SPCOND_SLOPE, COARSEN_REPS, HBEF_SOLUTES); all 13 scripts now source it.
8. ~~Row-by-row `rbind()` in loops~~ — Fixed: converted to list accumulation + `bind_rows()` in 6 scripts (ts_simulation inner loop, ts_simulation outer loop, ms_application_compare, coarsen HBEF/Plynlimon/NEON).
9. ~~Global variable dependencies~~ — Fixed: `calculate_truth_ts.R` now takes `dn` and `target_wy` as explicit parameters. Monthly branch `q_df` vs `q_df_add` bug also fixed.
10. ~~No data pipeline~~ — Fixed: all data consolidated under `data/`, all scripts in `paper/source/`, all figures in `paper/figures/`. See `data/README.md` for provenance.
11. ~~Performance: data re-read inside rep loop~~ — Fixed: `01_ts_simulation_analysis.R` now reads data, fits ARIMA, and defines functions once outside the loop.

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

**Code review findings (fixed post-M5):**
21. ~~`nth_element` argument swap~~ in `coarsen_helpers.R` and `06_coarsen_example_figure.R` — Pre-existing bug: `start_pos` was passed as step-size `n` instead of starting position. Each rep randomly varied sampling frequency instead of phase offset. Fixed: swapped to `nth_element(ts_df$date, start_pos, n = coarse_n)`. All coarsening analyses (04, 07, 09) rerun.
22. ~~O(n²) inner loop~~ in `coarsen_helpers.R` — `coarse_chem` accumulated across all outer iterations and was reprocessed each pass; first element skipped (k=2). Fixed: simplified to single-pass list accumulation within each `coarse_n` iteration.
23. ~~NA crash in composite method~~ in `calculate_annual_flux.R` — `generate_residual_corrected_con()` returns NA with ≤2 paired observations, crashing `calculate_composite_from_rating_filled_df()`. Fixed: guard with `is.data.frame()` check.
24. ~~`ifelse(method == NA)` returns NA~~ in `calculate_annual_flux.R` — `ms_recommended` column got NA instead of 0 when `ideal_method` was NA. Fixed: explicit `is.na()` guard.
25. ~~Unused parameter~~ `raw_data_con_in` in `calculate_annual_flux.R` — function re-filtered `domain_chem` instead of using the pre-filtered parameter. Fixed: now uses `raw_data_con_in`.
26. ~~Unsafe parallelism comment~~ in `calculate_annual_flux.R` — removed `swap lapply -> parLapply` suggestion; `.GlobalEnv` area assignment makes this unsafe.
27. ~~Undefined `good_months`~~ in `flux_methods.R` — dead monthly code path would crash. Fixed: derived from available data length.
28. ~~No-op `eval(datecol)`~~ in `flux_methods.R` — `eval()` on a character string is a no-op. Fixed: removed `eval()` wrapper.
29. ~~Post-fix coarsening audit~~ — After nth_element fix, HBEF Ca results changed: composite is now clearly the best method (near 0% bias), PW has +20% bias. Updated paper paras 146 and 150 to reflect corrected results. Conclusions claim (para 208) verified correct as-is.

### Paper Text Issues

**Structural:**
1. ~~Missing "Results" header~~ — Fixed: renamed "Conclusions" to "Results", added Summary and Conclusions section.
2. ~~Missing Figure 13~~ — Fixed: was a numbering gap, renumbered all figures sequentially (1–12).
3. ~~NEON results underserved~~ — Fixed: expanded from 1 paragraph to 3 (conductivity, turbidity, synthesis).
4. ~~Table 1 content missing~~ — Verified: renders correctly in Word, extraction artifact only.
5. **Placeholder text** - "FINAL VERSION LINK" in Data Availability section — **needs URL from Nic**.

**Writing Quality:**
6. ~~Typos~~ — Fixed: "Plylimon" → "Plynlimon", "Enchanced" → "Enhanced", date range dash added.
7. ~~Equation rendering~~ — Non-issue: equations are inline images, render correctly in Word.
8. **Works Cited formatting** inconsistent (some have DOIs, some don't; mixed date formats) — **Nic task (reference manager)**.
9. ~~Repetition~~ — Fixed: removed duplicate "defensible" judgment from Results, kept Discussion version.

### Figure Quality Issues
1. ~~Inconsistent styling~~ — Fixed: all scripts use `theme_rsfme()` and `ggsave_hess()` from `source/plot_theme.R`.
2. Fig 12 (decision flowchart) is a manually-created PNG — not reproducible from code.
3. Fig 1 (method illustration) is also a manual PNG.
4. ~~No unified theme or color palette~~ — Fixed: colorblind-safe Paul Tol palette, shared error band scales, consistent sizing.
