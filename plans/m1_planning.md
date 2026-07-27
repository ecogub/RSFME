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
- [ ] Add a `data/README.md` describing what each data file is and where to get it if not tracked

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
- [ ] Extract shared configuration (deferred — low urgency)
- [ ] Refactor HBEF/Plynlimon/NEON code duplication (deferred — too risky before verification)
- [ ] Remove or archive `ts_simulation/defunct/` scripts (deferred — low priority)

### M2d: Verification
- [ ] Run each script in order (per `Run Order.txt`) and confirm it completes without error
- [ ] Spot-check that key results (truth values, error envelopes) match what the paper reports
- [ ] Commit after each sub-milestone (M2a, M2b, M2c) so changes are reviewable

---

## M3: Improve Figures

The goal is publication-quality figures with a consistent style, regenerated from the fixed code.

### M3a: Establish a unified figure style
- [x] Define a shared `theme_rsfme()` ggplot2 theme — sans-serif Helvetica, base_size 10, HESS-compliant
- [x] Define a consistent color palette for methods — Paul Tol colorblind-friendly (blue/red/green/yellow/black)
- [x] Define consistent error-band styling — green ±5%, yellow ±20% with scale_fill_error_bands()
- [x] Put the theme and palette in `source/plot_theme.R` with ggsave_hess() helper

### M3b: Regenerate main figures
- [ ] Fig 1 (method illustration): Assess whether this needs to be redrawn or if the current PNG is acceptable
- [ ] Figs 2–5 (raw data, C:Q plots): Regenerate from `misc_figs.R` with unified theme
- [ ] Fig 6 (coarsening example): Regenerate from `3_coarsen_example_figure.R`
- [ ] Figs 9–10 (HBEF coarsening results): Regenerate from `2_coarsen_figure.R`
- [ ] Figs 11–12 (Plynlimon coarsening results): Regenerate from `2_coarsen_figure_plynlimon.R`
- [ ] Fig 14 (HBEF method comparison): Regenerate from `hbef_comparison_fig.R`
- [ ] Fig 15 (decision flowchart): Decide whether to recreate programmatically or polish the existing PNG

### M3c: Regenerate supplement
- [ ] Figs a1–a8 (NEON coarsening by method): Regenerate from `2_coarsen_figure_neon.R`
- [ ] Fig a9 (MacroSheds load distributions): Regenerate from `ms_descriptive_figure.R`
- [ ] Consider whether the ts_simulation figure (currently "pop_test.png") belongs in the main text or supplement

### M3d: Review and finalize
- [ ] Check all figure numbering matches the paper text
- [ ] Ensure all figures are saved at appropriate resolution for HESS submission (typically 300 DPI, specific size requirements)
- [ ] Create a `paper/figures/` directory with final versions, clearly named by figure number

---

## M4: Reconcile Narrative Text

The goal is a paper whose text accurately describes the results from M2/M3, with proper structure and no placeholder content.

### M4a: Fix paper structure
- [x] Rename "Conclusions" section to "Results" — the current content is results, not conclusions
- [ ] Add a proper "Conclusions" section at the end (or "Summary and Conclusions") — blocked on M4b
- [x] Fix figure numbering — renumbered sequentially (9→7, 10→8, 11→9, 12→10, 14→11, 15→12)
- [ ] Expand the NEON results beyond "see Appendix" — blocked on M3 NEON output
- [x] Verify Table 1 (NEON site descriptions) renders correctly in the document
- [x] Clarify MacroSheds 210k vs ~16k site-year counts

### M4b: Update results text to match regenerated figures
- [ ] Review all quantitative claims against the actual (potentially corrected) results — blocked on M3d
- [ ] Update any statistics that changed due to bug fixes in M2 — blocked on M3d
- [ ] Ensure figure captions match what the figures actually show — blocked on M3d

### M4c: Clean up writing
- [ ] Replace "FINAL VERSION LINK" placeholder in Data Availability with actual link — needs URL from Nic
- [x] Fix typos: "Plylimon" → "Plynlimon", "thatgenerally,when" → "that generally, when", "6/19/20166/22/2016" → "6/19/2016–6/22/2016"
- [x] ~~Fix equation rendering~~ — Non-issue: equations are inline images, render correctly in Word
- [ ] Standardize Works Cited formatting — Nic task (reference manager)
- [x] Identify repetition between Results and Discussion — analysis in `plans/m4_deliverables/repetition_analysis.md`; one clear cut, two judgment calls
- [ ] One final read-through for clarity and flow — blocked on all other M4 items

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

- [ ] Rename and move all scripts to `paper/source/`
- [ ] Update all `source()` and `here()` paths within scripts to reflect new locations
- [ ] Delete emptied subdirectories (keep `ts_simulation/defunct/` as-is or archive)

### M5b: Consolidate figure output to `paper/figures/`

All scripts currently save figures into their own subdirectories. Redirect all `ggsave()` / `png()` calls to `paper/figures/`, with the script number as filename prefix.

Naming convention: `fig{figure_number}_{short_description}.png`
- e.g., `fig07_hbef_nitrate_coarsening.png`, `figa1_neon_cond_li.png`

Intermediate data files (`.RData`, `.csv` results) go to `paper/data/` instead.

- [ ] Create `paper/figures/` and `paper/data/` directories
- [ ] Update all output paths in scripts 01–15
- [ ] Remove orphaned figure files from old subdirectories
- [ ] Verify all figures land in `paper/figures/` after a full run

### M5c: Write runner script (`paper/source/00_run_all.R`)

A single script that sources all numbered scripts in order. Should:
- Set the working directory via `here()`
- Check for required packages and data files up front
- Source scripts 01–15 in sequence
- Print timing and status for each step
- Optionally accept a `start_from` argument to resume from a specific script number

- [ ] Write `00_run_all.R`
- [ ] Test end-to-end execution
- [ ] Document any manual steps (e.g., NEON data download via `MACROSHEDS_ROOT`)

### M5d: Write README

Replace or create a top-level `README.md` that explains the repo as it is now — no history of past issues or milestones.

Contents:
- Paper title, authors, target journal
- Repository structure (tree diagram)
- How to reproduce: prerequisites (R version, packages), data setup, running `00_run_all.R`
- Where outputs go (`paper/figures/`, `paper/data/`)
- Data availability notes (proprietary `w3_sensor_wdisch.feather`, MacroSheds download)
- Contact info

- [ ] Write `README.md`
- [ ] Remove or replace `paper/Run Order.txt` (superseded by `00_run_all.R`)

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

## Resolved Questions

See `plans/decisions_made.txt` for the full decisions log.
