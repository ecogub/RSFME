# M1 Plan: RSFME Paper Improvement

**Created:** 2026-07-27
**Flow:** Fix code → Improve figures → Reconcile text → Organize repo

---

## M2: Fix the Codebase

The goal is a codebase that runs end-to-end from a clean checkout, produces correct results, and is maintainable. No new analysis — just making the existing analysis reliable and reproducible.

### M2a: Data and path portability
- [x] Move `w3_sensor_wdisch.feather` into repo root (proprietary — stays in `.gitignore`)
- [x] Add `w3_sensor_wdisch.feather` to `.gitignore`
- [x] Replace all 7 hardcoded desktop paths with `here('w3_sensor_wdisch.feather')`
- [x] Fix NEON script's hardcoded path — now uses `Sys.getenv("MACROSHEDS_ROOT")` with fallback to `here('data', 'macrosheds')`
- [x] Update `.gitignore` to properly handle data files
- [x] Add a `data/README.md` describing what each data file is and where to get it if not tracked

### M2b: Critical bugs
- [x] Fix loop variable collision in `1_coarsen_analysis.R` — renamed `i` to `solute_var`, `coarse_n`, `k`
- [x] Fix same collision in `1_coarsen_analysis_plynlimon.R` — renamed `i` to `solute_var`, `coarse_n`
- [x] ~~Fix result accumulation bug~~ — Verified: not a bug. R for-loops iterate all elements regardless of variable modification. Code is inefficient but correct; final save contains all results.
- [x] Fix undefined variable `flag` in `misc_figs.R` — removed `color = flag` from streamflow plot
- [x] Fix undefined variable `test` in `ms_application_compare.R` — changed to `n_frame`
- [x] Fix Plynlimon file format mismatch — changed analysis script to save `.csv` (matching figure script)
- [x] Fix `calculate_truth_ts.R` — added `dn` and `target_wy` as explicit parameters with defaults from calling scope; fixed monthly branch using `q_df` instead of `q_df_add`
- [x] Verified error band labels in `2_coarsen_figure.R` — fill keys are confusingly named but visual output is correct (two swaps cancel out). Will clean up in M2c.

### M2c: Refactor for maintainability
- [x] Replace `rbind()` inside loops with list accumulation + `bind_rows()` (5 scripts)
- [x] Remove unused `library()` calls across all scripts (15 scripts swept)
- [x] Update deprecated `size` → `linewidth` in all ggplot2 line geoms (5 files)
- [x] Stop re-reading data and re-fitting ARIMA inside the rep loop in `1_ts_simulation_analysis.R`
- [x] Stop re-sourcing `calculate_truth_ts.R` inside loops
- [x] Clean up error band label naming in `2_coarsen_figure.R` (descriptive keys + explicit mapping)
- [x] Fix Plynlimon `site_code = 'w3'` bug → now uses `site_code` variable
- [x] Fix "Enchanced" typo → "Enhanced" in `3_descriptive_figures.R`
- [x] Remove debug `plot()` call from inside ts_simulation rep loop
- [x] Extract shared configuration — created `source/config.R`, sourced by all 13 scripts
- [x] Refactor HBEF/Plynlimon/NEON code duplication — extracted `run_coarsening_experiment()` into `coarsen_helpers.R`, reducing ~300 lines to ~35 per script
- [ ] Remove or archive `ts_simulation/defunct/` scripts (deferred — low priority)

### M2d: Verification
- [x] Run each script in order (per `Run Order.txt`) and confirm it completes without error — all 11 figure scripts verified clean; analysis scripts 01/04/07/09 are long-running (skipped full re-run but verified syntax/paths)
- [x] Spot-check that key results (truth values, error envelopes) match what the paper reports — completed via M4b quantitative audit + post-fix coarsening audit; paras 146 and 150 updated to match corrected data
- [x] Commit after each sub-milestone (M2a, M2b, M2c) so changes are reviewable

---

## M3: Improve Figures

The goal is publication-quality figures with a consistent style, regenerated from the fixed code.

### M3a: Establish a unified figure style
- [x] Define a shared `theme_rsfme()` ggplot2 theme — sans-serif Helvetica, base_size 10, HESS-compliant
- [x] Define a consistent color palette for methods — Paul Tol colorblind-friendly (blue/red/green/yellow/black)
- [x] Define consistent error-band styling — green ±5%, yellow ±20% with scale_fill_error_bands()
- [x] Put the theme and palette in `source/plot_theme.R` with ggsave_hess() helper

### M3b: Regenerate main figures
- [x] Fig 1 (method illustration): Kept existing PNG (manually created, acceptable quality)
- [x] Figs 2–5 (raw data, C:Q plots): Regenerated from `14_misc_figures.R` with unified theme
- [x] Fig 6 (coarsening example): Regenerated from `06_coarsen_example_figure.R`
- [x] Figs 7–8 (HBEF coarsening results): Regenerated from `05_coarsen_figure_hbef.R`
- [x] Figs 9–10 (Plynlimon coarsening results): Regenerated from `08_coarsen_figure_plynlimon.R`
- [x] Fig 11 (HBEF method comparison): Regenerated from `15_hbef_method_comparison.R`
- [x] Fig 12 (decision flowchart): Kept existing PNG (manually created, acceptable quality)

### M3c: Regenerate supplement
- [x] Figs a1–a8 (NEON coarsening by method): Regenerated from `10_coarsen_figure_neon.R`
- [x] Fig a9 (MacroSheds load distributions): Regenerated from `11_macrosheds_compare.R` and `12_macrosheds_descriptive.R`
- [x] ts_simulation figure: Regenerated as `fig_supp_ts_simulation.png` (supplement)

### M3d: Review and finalize
- [x] Check all figure numbering matches the paper text — found and fixed 3 cross-reference bugs (7/10→7/8, 9/12→9/10, 8/12→8/10)
- [x] Ensure all figures are saved at appropriate resolution for HESS submission (300 DPI via ggsave_hess())
- [x] Create a `paper/figures/` directory with final versions, clearly named by figure number — 30 figures consolidated

---

## M4: Reconcile Narrative Text

The goal is a paper whose text accurately describes the results from M2/M3, with proper structure and no placeholder content.

### M4a: Fix paper structure
- [x] Rename "Conclusions" section to "Results" — the current content is results, not conclusions
- [x] Add "Summary and Conclusions" section (5 paragraphs) — in `paper_HESS_draft_v2_claude_final.docx`
- [x] Fix figure numbering — renumbered sequentially (9→7, 10→8, 11→9, 12→10, 14→11, 15→12)
- [x] Expand the NEON results beyond "see Appendix" — 3 paragraphs added covering conductivity, turbidity, and synthesis
- [x] Verify Table 1 (NEON site descriptions) renders correctly in the document
- [x] Clarify MacroSheds 210k vs ~16k site-year counts

### M4b: Update results text to match regenerated figures
- [x] Fixed 3 cross-reference bugs from renumbering: "Figures 7 and 10" → "7 and 8", "Figures 9 and 12" → "9 and 10", "Figures 8 and 12" → "8 and 10"
- [x] Applied repetition fix: removed "defensible" judgment sentence from NO3 Results paragraph
- [x] Review all quantitative claims against the actual results — audit complete, all 7 fixes applied (3 trivial + 4 judgment-call, all approved by Nic)
- [x] Ensure figure captions match what the figures actually show — 7 fixes applied (3 method-ranking claims corrected post-nth_element fix, 4 turbidity labels corrected)

### M4c: Clean up writing
- ~~Replace "FINAL VERSION LINK" placeholder~~ — moved to M8
- [x] Fix typos: "Plylimon" → "Plynlimon", "thatgenerally,when" → "that generally, when", "6/19/20166/22/2016" → "6/19/2016–6/22/2016"
- [x] ~~Fix equation rendering~~ — Non-issue: equations are inline images, render correctly in Word
- ~~Standardize Works Cited formatting~~ — moved to M8
- [x] Identify repetition between Results and Discussion — analysis in `plans/m4_deliverables/repetition_analysis.md`; one clear cut, two judgment calls
- ~~Final read-through~~ — moved to M8

---

---

## M5: Organize Repo for Reproducibility

The goal is a repo a reviewer or collaborator can clone and understand immediately: numbered scripts, one figures output folder, a single runner script, and a README that explains what's here.

### M5a: Number scripts in execution order

Rename all analysis and figure scripts to a flat, numbered sequence in `paper/source/`. The number reflects execution order. Helper/utility scripts sourced by others keep descriptive names but move into the same folder.

Current scripts and their new names:

| # | Current location | New name | Produces |
|---|-----------------|----------|----------|
| 01 | `ts_simulation/1_ts_simulation_analysis.R` | `01_ts_simulation_analysis.R` | CSV results, `simulated_series.Rdata` |
| 02 | `ts_simulation/2_ts_simulation_figure.R` | `02_ts_simulation_figure.R` | Fig 7 (supplement), Supp Table 1 |
| 03 | `ts_simulation/3_descriptive_figures.R` | `03_ts_descriptive_figures.R` | Hydro regime + C:Q regime panels |
| 04 | `coarsen_plot/1_coarsen_analysis.R` | `04_coarsen_analysis_hbef.R` | HBEF coarsening .RData |
| 05 | `coarsen_plot/2_coarsen_figure.R` | `05_coarsen_figure_hbef.R` | Figs 7–8 (Ca + NO3 coarsening) |
| 06 | `coarsen_plot/3_coarsen_example_figure.R` | `06_coarsen_example_figure.R` | Fig 6 (thinning example) |
| 07 | `plynlimon_discussion/1_coarsen_analysis_plynlimon.R` | `07_coarsen_analysis_plynlimon.R` | Plynlimon coarsening .csv |
| 08 | `plynlimon_discussion/2_coarsen_figure_plynlimon.R` | `08_coarsen_figure_plynlimon.R` | Figs 9–10 |
| 09 | `neon_discussion/1_coarsen_analysis_neon.R` | `09_coarsen_analysis_neon.R` | NEON coarsening results |
| 10 | `neon_discussion/2_coarsen_figure_neon.R` | `10_coarsen_figure_neon.R` | Figs a1–a8 |
| 11 | `macrosheds_application/ms_application_compare.R` | `11_macrosheds_compare.R` | Method comparison figure |
| 12 | `macrosheds_application/ms_descriptive_figure.R` | `12_macrosheds_descriptive.R` | Fig a9 |
| 13 | `hbef_corr_exploration/Ca_correlation_investigation.R` | `13_ca_correlation.R` | Ca-SpCond regression (sourced by 14) |
| 14 | `misc_figure_creation/misc_figs.R` | `14_misc_figures.R` | Figs 2–5 (raw data, C:Q) |
| 15 | `hbef_comparison_fig/hbef_comparison_fig.R` | `15_hbef_method_comparison.R` | Fig 11 |

Helper scripts (not numbered, moved to `paper/source/`):
- `coarsen_plot/coarsen_helpers.R` → `coarsen_helpers.R`
- `ts_simulation/calculate_truth_ts.R` → `calculate_truth_ts.R`
- `ts_simulation/4_base_storm_sep.R` → `base_storm_sep.R` (utility, not in main pipeline)

- [x] Rename and move all scripts to `paper/source/`
- [x] Update all `source()` and `here()` paths within scripts to reflect new locations
- [x] Delete emptied subdirectories — removed coarsen_plot, plynlimon_discussion, neon_discussion, macrosheds_application, hbef_comparison_fig, hbef_corr_exploration, misc_figure_creation. Kept ts_simulation/defunct/, method_illustration/, flowchart/

### M5b: Consolidate figure output to `paper/figures/`

All scripts currently save figures into their own subdirectories. Redirect all `ggsave()` / `png()` calls to `paper/figures/`, with the script number as filename prefix.

Naming convention: `fig{figure_number}_{short_description}.png`
- e.g., `fig07_hbef_nitrate_coarsening.png`, `figa1_neon_cond_li.png`

Intermediate data files (`.RData`, `.csv` results) go to `paper/data/` instead.

- [x] Create `paper/figures/` directory (data outputs go to `data/` subdirs instead of `paper/data/`)
- [x] Update all output paths in scripts 01–15
- [x] Remove orphaned figure files from old subdirectories — deleted with parent dirs
- [x] Verify all figures land in `paper/figures/` after a full run — 30 figures confirmed

### M5c: Write runner script (`paper/source/00_run_all.R`)

A single script that sources all numbered scripts in order. Should:
- Set the working directory via `here()`
- Check for required packages and data files up front
- Source scripts 01–15 in sequence
- Print timing and status for each step
- Optionally accept a `start_from` argument to resume from a specific script number

- [x] Write `00_run_all.R` — package/data checks, timing, start_from argument
- ~~Test end-to-end execution~~ — struck per user request; individual scripts verified independently
- [x] Document manual steps in README.md and data/README.md

### M5d: Write README

Replace or create a top-level `README.md` that explains the repo as it is now — no history of past issues or milestones.

Contents:
- Paper title, authors, target journal
- Repository structure (tree diagram)
- How to reproduce: prerequisites (R version, packages), data setup, running `00_run_all.R`
- Where outputs go (`paper/figures/`, `paper/data/`)
- Data availability notes (proprietary `w3_sensor_wdisch.feather`, MacroSheds download)
- Contact info

- [x] Write `README.md` — paper overview, repo structure, reproduction instructions, script table
- [x] Remove `paper/Run Order.txt` (superseded by `00_run_all.R`)

---

## M6: Improve Figure 11 — Method Comparison Panel Figure

The goal is to replace the standalone 1:1 scatter plot (`fig11_hbef_method_comparison.png`) with a two-panel figure that better communicates method accuracy relative to truth.

### M6a: Add difference-from-truth bar chart (panel B)
- [x] In `15_hbef_method_comparison.R`, compute `diff = Ca.x - Ca.y` for each method × water year
- [x] Build a bar chart (`geom_col`) of difference from truth, centered around zero (y = 0 reference line)
- [x] Color bars by method using existing `fluxpal` palette
- [x] Group by water year on the x-axis with dodged bars by method
- [x] Use `theme_rsfme()` and consistent axis labels

### M6b: Combine into labeled A/B panel figure
- [x] Label the existing 1:1 scatter as panel **(A)** and the new bar chart as panel **(B)**
- [x] Combine with `patchwork` (`p_comp + p_diff + plot_annotation(tag_levels = 'A')`)
- [x] Share a single legend between the two panels via `plot_layout(guides = 'collect')`
- [x] Save as `fig11_hbef_method_comparison.png` (replaces existing file)

### M6c: Update caption and paper text
- [x] Update the Figure 11 caption in `paper_HESS_draft_v2_claude_final.docx` to reference panel (A) for the 1:1 comparison and panel (B) for the difference-from-truth bar chart
- [x] In-text reference at para 174 still makes sense with two-panel layout — refers to general agreement, not specific panel

---

## M8: Final Tasks

Pre-submission items that require Nic's input or a full document pass.

- [ ] Replace "FINAL VERSION LINK" placeholder in Data Availability section with actual URL
- [ ] Standardize Works Cited formatting (reference manager)
- [ ] Final read-through for clarity and flow (blocked on all other milestones)

---

## M7: Adversarial Code Review

The goal is to document all bugs, logic errors, and methodological concerns found through an adversarial review of the full codebase — prioritized by potential impact on paper results.

### M7a: Critical — could change paper results
- [ ] `hold_factor` contamination across flow regimes (`01_ts_simulation_analysis.R:161-206`) — `hold_factor` is overwritten three times (unaltered→storm→baseflow) without saving intermediate values. By the time enriching/diluting chemistry series are created (lines 189–206), `hold_factor` contains only the baseflow normalization. Enriching and diluting series for unaltered and storm flow use the wrong scaling factor.
- [ ] `mean_or_x` computes variance instead of mean (`ms_overwrites.R:303`) — copy-paste from `sd_or_0` above: `x <- mean(var(x, na.rm = na.rm))` should be `x <- mean(x, na.rm = na.rm)`. Affects MacroSheds daily aggregation of sub-daily data (`calculate_annual_flux.R:80,87`). If input data has multiple observations per day, daily values become the variance of those observations, not the mean.
- [ ] `wyday`/`enlightened_yday` reorder output (`flux_methods.R:414-486`) — splits input dates into Oct-Dec and Jan-Sep subsets, processes each, then concatenates `c(firsthalf, secondhalf)`. Returns results in a different order than the input vector. When used by `decimalDateWY` (line 448), the reordered `wydays` is combined with unreordered `water_year(dates)` (line 451), misaligning columns in the EGRET Sample/Daily files.
- [ ] Self-referential truth in coarsening experiments — scripts 04/05, 07/08, 09/10 define "truth" as the composite method applied to full-resolution data. The composite method therefore benchmarks its own subsampling consistency, not accuracy against an independent ground truth (e.g., direct integral of C×Q at 15-minute resolution). This is a methodological question — may be intentional, but should be documented/justified in the paper.

### M7b: High — affects specific analyses
- [ ] Script 15 ignores intercept in Ca prediction (`15_hbef_method_comparison.R:52,92`) — line 52 fits `lm(Ca ~ spCond)` (with intercept), but line 92 applies only the slope: `dn$IS_spCond * lm_fit$coef[[2]]`. The sensor-derived "truth" for the method comparison figure omits the intercept term, systematically biasing all truth values.
- [ ] Inconsistent Ca~SpCond models between scripts 13 and 15 — script 13 fits `Ca ~ spCond + 0` (no intercept, origin-forced, coefficient 0.06284158 hardcoded in scripts 04/06), while script 15 fits `Ca ~ spCond` (with intercept) on different data (MacroSheds vs HBEF CSV). Two different regression models are used for the same physical relationship at the same site.
- [ ] NEON turbidity filename parsing bug (`10_coarsen_figure_neon.R:84-90`) — turbidity filenames like `TEST100reps_annual_turb2016_w1.RData` split by `_` into 4 parts: `[TEST100reps, annual, turb2016, w1]`. `str_split_i('_', 4)` returns site code (`w1`), not year; `str_split_i('_', 5)` returns NA. The `wy` column gets site codes and `site_code` gets NAs, corrupting NEON turbidity figures.
- [ ] `calculate_wrtds` tryCatch doesn't return NA on error (`flux_methods.R:279-283`) — the error handler's `return(NA)` exits the anonymous handler function, not `calculate_wrtds`. The tryCatch expression evaluates to NA, but this value is discarded. Execution continues to `return(flux_from_egret)` on line 283, which was never assigned, causing a crash instead of graceful degradation.
- [ ] Leap-year scaling bug (`flux_methods.R:173-181`) — `method1_month` yearly branch loops over years but each iteration overwrites `method1` for ALL years with either 366 or 365 days. After the loop, `method1` has the last year's day count applied uniformly. Impact is small (±0.27%) but systematic.
- [ ] `molecular_conversion_map` indexed by position not name (`ms_overwrites.R:119`) — `molecular_conversion_map[v]` where `v` is an integer index looks up the v-th list element, not the molecule name. If `convert_molecules_element` contains `'SO4'` at position 1, it retrieves the conversion for `NH4` (the 1st element) instead.

### M7c: Medium — correctness issues, limited result impact
- [ ] No-flow replacement uses full mean instead of 0.1% (`flux_methods.R:701-706`) — `ifelse(Q <= 0, mean_flow, Q)` inflates Q 1000x on zero-flow days. USGS WRTDS manual specifies 0.1% of mean flow.
- [ ] Gap-blanking loop only processes last break (`flux_methods.R:880`) — `for(i in length(sample_breaks['start']))` iterates over a single value (the count), not `1:length(...)`. If 3 breaks exist, only the 3rd is processed.
- [ ] `drain_area_va` conversion 100x off (`flux_methods.R:793`) — `area / 2.59` converts hectares to sq mi, but should be `area / 259` (hectares → km² → sq mi). Only affects EGRET INFO table metadata, not flux calculations.
- [ ] Plynlimon Q unit conversion 100x off (`07_coarsen_analysis_plynlimon.R:24`) — the mm→m and ha→m² factors are inverted: `(1000/1) * (1/10000)` should be `(1/1000) * (10000/1)`. Since both truth and estimates use the same conversion, relative coarsening errors are unaffected. Absolute flux values are wrong by 100x.
- [ ] `convert_unit` precedence bug (`ms_overwrites.R:235`) — `if(length(new_fraction == 2))` evaluates `new_fraction == 2` first (a logical vector), then takes its `length()` (always ≥1, truthy). Should be `if(length(new_fraction) == 2)`.
- [ ] Non-finite composite concentrations set to 0 (`flux_methods.R:321`) — `con_com[!is.finite(con_com)] <- 0` silently converts Inf/NaN to zero, underestimating loads rather than flagging model failure.
- [ ] Outlier definition uses median±1.5×IQR (`02_ts_simulation_figure.R:405`) — standard boxplot definition uses Q1/Q3 ± 1.5×IQR, not median. Shifts both fences, miscounting outliers.
- [ ] Mislabeled "95% CI" is a prediction interval (`02_ts_simulation_figure.R:399-400`) — `mean ± 1.96*SD` is a 95% prediction interval; a confidence interval of the mean uses `SD/sqrt(n)`.
- [ ] `SiO2_S` / `SiO3_S` suffix in `ms_overwrites.R:76-77` — silicon is `Si`, not `S` (sulfur). The `element_molecule` names use wrong suffix.

### M7d: Low — code quality / robustness
- [ ] Global `area` variable used as free variable throughout `flux_methods.R` — injected via `assign('area', area, envir = .GlobalEnv)`, prevents safe parallelism and creates stale-state risk.
- [ ] `base_storm_sep.R:2` runs `devtools::install_github('cran/EcoHydRology')` every time it's sourced — no version pin, fails without internet, slow.
- [ ] Hardcoded date `20221221` in simulation output filenames (`01_ts_simulation_analysis.R:362`) — downstream scripts (02, 03) read these exact filenames; any change breaks the chain.
- [ ] `calculate_truth_ts.R` hardcodes `site_code = 'w3'` on lines 10, 13, 27, 30 — only valid for HBEF Watershed 3.
- [ ] Copy-paste bug in `02_ts_simulation_figure.R:355` — `p19_data$method <- factor(p15_data$method, ...)` references `p15_data` instead of `p19_data`. Works by accident since both have the same factor levels.
- [ ] `old_bottom` used without initialization for non-fractional units (`ms_overwrites.R:247`) — if `input_unit` has no `/`, `old_bottom` is never defined, causing a crash downstream.

---

## Milestone Timeline

| Milestone | Scope | Depends on |
|-----------|-------|------------|
| M2a | Data/path portability | — |
| M2b | Critical bug fixes | M2a (paths must work first) |
| M2c | Refactoring | M2b (bugs fixed first) |
| M2d | Verification | M2a + M2b + M2c |
| M3a | Figure theme | M2d (code runs clean) |
| M3b | Main figures | M3a |
| M3c | Supplement figures | M3a |
| M3d | Figure finalization | M3b + M3c |
| M4a | Paper structure | — (can start in parallel with M3) |
| M4b | Results reconciliation | M3d (figures finalized) |
| M4c | Writing cleanup | M4a + M4b |
| M5a | Number and move scripts | M3d (figures finalized, scripts stable) |
| M5b | Consolidate figure output | M5a |
| M5c | Runner script | M5a + M5b |
| M5d | README | M5c (repo structure finalized) |
| M6a | Diff-from-truth bar chart | M3d (figures finalized) |
| M6b | A/B panel layout | M6a |
| M6c | Caption and text update | M6b |
| M7a | Critical bugs (paper-impacting) | — |
| M7b | High-severity bugs | M7a |
| M7c | Medium-severity fixes | M7b |
| M7d | Low-severity / code quality | M7c |
| M8 | Final tasks (Nic-owned) | All other milestones |

## Resolved Questions

See `plans/decisions_made.txt` for the full decisions log.
