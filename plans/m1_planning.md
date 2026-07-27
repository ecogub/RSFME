# M1 Plan: RSFME Paper Improvement

**Created:** 2026-07-27
**Flow:** Fix code → Improve figures → Reconcile text

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
- [ ] Rename "Conclusions" section to "Results" — the current content is results, not conclusions
- [ ] Add a proper "Conclusions" section at the end (or "Summary and Conclusions")
- [ ] Fix figure numbering — there is no Figure 13 (either add one or renumber)
- [ ] Expand the NEON results beyond "see Appendix" — at minimum a paragraph summarizing key findings
- [ ] Verify Table 1 (NEON site descriptions) renders correctly in the document

### M4b: Update results text to match regenerated figures
- [ ] Review all quantitative claims against the actual (potentially corrected) results
- [ ] Update any statistics that changed due to bug fixes in M2
- [ ] Ensure figure captions match what the figures actually show

### M4c: Clean up writing
- [ ] Replace "FINAL VERSION LINK" placeholder in Data Availability with actual link
- [ ] Fix typos: "Plylimon" → "Plynlimon", "Enchanced" → "Enhanced", "6/19/20166/22/2016" → "6/19/2016–6/22/2016"
- [ ] Fix equation rendering (Equations 1–3 need proper symbols)
- [ ] Standardize Works Cited formatting
- [ ] Remove repetition between Results and Discussion sections
- [ ] One final read-through for clarity and flow

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

## Resolved Questions
1. **w3_sensor_wdisch.feather** — Now in repo root. Proprietary data, must stay in `.gitignore`. Scripts should reference via `here('w3_sensor_wdisch.feather')`.
2. **ts_simulation figure** — Moves to supplement.
3. **Decision flowchart (Fig 15)** — Will be recreated programmatically for easy editing.
4. **New analyses** — OK if major improvements in the same conceptual space, but light touch preferred.
5. **MacroSheds count** — 210k site-years computed, ~16k passed quality cutoffs. Both correct in context; paper should clarify the distinction.
