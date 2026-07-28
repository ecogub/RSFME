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
- [x] ~~Remove or archive `ts_simulation/defunct/` scripts~~ OBSOLETE — the entire `ts_simulation` tree was deleted in M7e; no such directory exists anywhere in the repo.

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

## M8: Figure Improvements (Post-Rerun)

The goal is publication-ready figures after the M7e rerun. The rerun changed computed values, so figures need visual QA and fixes.

### M8a: Coarsening figures — cropping and color consistency
- [x] Fix y-axis cropping across all coarsening figures — HBEF Ca switched from `scale_y_continuous(limits=)` (clips data) to `coord_cartesian(ylim=)` (zooms only); all figures now use `coord_cartesian`
- [x] Unify color palette across all coarsening plots (HBEF, Plynlimon, NEON) — HBEF switched from `geom_rect`/`scale_fill_error_bands()` to `annotate()` matching Plynlimon/NEON style
- [x] Make NEON figures taller — height now scales dynamically at 4cm per facet panel (6 sites × 4cm = 24cm for conductivity, 5 × 4cm = 20cm for turbidity)
- [x] Fix HBEF facet label clipping — added `panel.spacing.y = unit(1.2, 'lines')` so "Rating"/"Composite" labels not cut off
- [x] NEON dynamic y-axis per method — each plot's y-axis sized to fit data through a readable frequency (conductivity: monthly; turbidity pw: monthly; turbidity beale/rating/composite: biweekly). Lines allowed to depart view at coarser frequencies.
- [x] Delete stale pre-M7 turbidity data files (`turb_*.RData` with underscore) that caused duplicate panels in NEON turbidity figures

### M8b: Paper text updates
- [x] Update Figure 11 caption (PARA 173) — "with the composite method applied" → "and multiplied by discharge to compute instantaneous load at each 15-minute timestep, then summed annually"
- [x] Update R-squared in PARA 174 — 0.63 → 0.74 (changed due to new truth calculation)
- [x] Add y-axis note to all 8 NEON captions (a1–a8) — "Note that the y-axis range varies between NEON figures to capture the full spread of error, which is greater than in the single-site HBEF and Plynlimon experiments."
- [x] Accuracy sweep — 7 issues found and fixed: (1) stale "no intercept" in Ca~SpCond description, (2) stale R²=0.98/slope=0.063 → 0.66/0.059/intercept=0.013, (3) HBEF area 40.2→42.4 ha, (4) Plynlimon area 120→122 ha, (5) MacroSheds totals 210,058→133,425 and 16,000→26,685, (6) Figs 7-10 image/caption swap fixed, (7) "Figures 7-12"→"Figures 7-10" for coarsening ref

---

## M9: Pre-Submission Adversarial Review Fixes

Findings from the 2026-07-28 adversarial review of the manuscript against the regenerated data, figures, and code. Review grade: **65/100** — major-revision territory, driven almost entirely by two items (M9a-1 and M9a-2).

**Before starting:** M8b claims an accuracy sweep already fixed several items below (Ca~SpCond text, watershed areas, MacroSheds totals, "Figures 7-12"). The `.docx` reviewed at 13:24 on 2026-07-28 still contained the pre-fix values, so parallel edits may not have landed in the reviewed copy. **Verify each item marked (VERIFY) against the current document before editing** — do not re-apply a fix that is already in place.

**Resolved during review, no action needed:**
- ~~Plynlimon "auto-samplers" (Methods) vs "grab samples" (Fig 4 caption)~~ — Not a contradiction per Nic: an auto-sampler collects grab samples. Leave both as written.
- ~~Figures 7/8 and 9/10 Ca↔NO3 swap~~ — Disproved by MD5-hashing the images embedded in the `.docx` against `paper/figures/`. Captions correctly match image content. Only the *filenames* are numbered inversely (`fig07_hbef_ca_coarsening.png` is the paper's Figure 8). No document change needed; see M9c-5 for the filename cleanup.

### M9a: Critical — method identification and figure axes

**Decision (Nic, 2026-07-28): Option A — switch to `method6`.** The paper's entire architecture depends on linear interpolation being one of the four archetypal methods (title framing, Figure 1, Equation 2, the Aulenbach framework being tested, and the decision tree). Function renamed `calculate_pw` → `calculate_li` for clarity.

**Background.** Methods claimed RiverLoad's `method6`; the code called `method1` (`source/flux_methods.R:34`), and `method6` appeared nowhere in the repo. Verified against the installed package: `method6` interpolates C and Q to daily via `approx(rule=2)` then sums C×Q×86400 (exactly the paper's description); `method1` computes mean(C) × mean(Q) × duration × 86400, an averaging estimator that discards the C–Q covariance. Diagnostic confirmation: the sign of the old `pw` bias tracked the inverse of the C:Q slope in every series (diluting Ca overestimated +20%, enriching NO₃ underestimated −37%), and at zero coarsening on Plynlimon the estimator was still +22.6% off — impossible for an interpolating estimator with the complete record in hand.

#### M9a-1: Code changes — DONE
- [x] `source/flux_methods.R` — `calculate_pw` → **`calculate_li`**; `method1(...)` → `method6(...)`; internal `flux_from_pw` → `flux_from_li`; added a comment warning against reverting to `method1`.
- [x] `source/flux_methods.R` monthly branch — `method1_month(...)` → `method6(..., period=period)` so `calculate_li` cannot silently do averaging. The inline 145-line `method1_month` definition is now orphaned dead code (flagged for removal in M9a-5).
- [x] Call sites updated: `paper/source/coarsen_helpers.R:3`, `source/calculate_annual_flux.R:139`.
- [x] Method key `'pw'` → `'li'` throughout: `coarsen_helpers.R:15`, `calculate_annual_flux.R:180,193`, `02_coarsen_figure_hbef.R` (×2), `05_coarsen_figure_plynlimon.R` (×2), `07_coarsen_figure_neon.R` (×2), `12_hbef_method_comparison.R:77`, `source/plot_theme.R` (colour + label keys), `source/hbef_flux_plots.R` (×8, orphaned script updated for consistency).
- [x] **Smoke test passed.** With a complete daily HBEF series: `calculate_li` returns 4.5463 kg/ha/yr for Ca — exactly the hand-computed Σ C×Q×86400 and **0.00%** from truth, versus `method1`'s +19.57%. For NO₃ it returns −0.86% vs truth while a naive paired sum gives −11.20%, the difference being `method6` correctly interpolating the 4-day June gap.
- [x] **Restored `'spCond'` to the NEON solute loop** (`06_coarsen_analysis_neon.R:32`). This became mandatory rather than optional: the existing conductivity `.RData` files store `method='pw'`, so with the figure scripts now looping over `'li'`, Figures a1–a4 would have rendered empty. Also fixes the M9a-4 reproducibility gap.
- [x] Pre-rerun outputs backed up to scratchpad (`pre_li_backup/`) so old-vs-new deltas can be quantified for the text updates.

#### M9a-2: Rerun and regeneration — COMPLETE (2026-07-28, 80.8 min, exit 0, zero failures)
`calculate_annual_flux.R` wrote 133,425 load estimates across 146 sites; all 12 pipeline scripts completed; all 20 NEON `.RData` regenerated (12 spCond + 8 turbidity). **Sanity checks on the delta table both passed:** Beale/rating/composite median errors moved by exactly 0.00 at every frequency in all four site-solutes (only LI changed, as intended), and truth values were identical old vs new.

**Delta — linear interpolation median % error, old (method1) → new (method6):**

| | daily | weekly | ~37d | ~68d |
|---|---|---|---|---|
| HBEF Ca | +20.1 → **−0.5** | +7.6 → **+3.8** | +12.5 → **+5.6** | −5.7 → **+7.8** |
| HBEF NO₃ | −36.9 → **+0.3** | −40.1 → **−16.9** | −48.9 → **−23.0** | −51.9 → **−26.5** |
| PLYN Ca | +22.6 → **+0.4** | +20.1 → **+0.7** | +18.1 → **+10.0** | +1.4 → **+15.6** |
| PLYN NO₃ | +7.4 → **+0.1** | +4.8 → **+0.6** | +4.3 → **+9.9** | −5.1 → **+8.5** |

**Two latent bugs surfaced during regeneration:**
- Script 07 names its output `figa_neon_{var}_{method}.png`, so the `pw`→`li` rename produced new `_li` files and orphaned the old `_pw` ones. The figure-refresh tool caught this by reporting the LI panels as "unchanged"; stale `_pw` files deleted and the mapping corrected.
- `07_coarsen_figure_neon.R` carried two unused `method_names` vectors still keyed on `pw`. Dead code (titles use `method_labels` from `plot_theme.R`), removed.

<details><summary>Original M9a-2 checklist</summary>
Running as a chained background job: `calculate_annual_flux.R` first (Figs 11 and a9 depend on `load_annual.csv`), then `00_run_all.R` for scripts 01–12. Expect ≥45 min for the pipeline plus unknown time for the MacroSheds job; NEON now runs both solutes so script 06 will take roughly double its previous 21.6 min.
- [x] `source/calculate_annual_flux.R` → regenerates `data/load_annual.csv` (133,425 rows) and `load_annual_diagnostics.csv`
- [x] Script 01 → `data/coarsen_hbef/` ; Script 02 → Figs 7–8
- [x] Script 04 → `data/coarsen_plynlimon/` ; Script 05 → Figs 9–10
- [x] Script 06 → `data/coarsen_neon/` (**both spCond and turbidity**) ; Script 07 → Figs a1–a8
- [x] Scripts 08, 09 → MacroSheds appendix figures ; Script 12 → Fig 11
- [x] Confirm all 12 scripts exit clean and no figure panel is empty (the `li` key must resolve in every facet)
- [x] Diff new vs `pre_li_backup/` and tabulate the change in median % error per method × frequency × site-solute
</details>

#### M9a-3: Text and conclusions — COMPLETE (2026-07-28)

**The fix resolved more claims than it broke.** Two statements that were false under the averaging estimator became true with genuine interpolation and were kept (with numbers added): *"Linear interpolation performed comparably with sub-weekly data"* (was −36.9% at daily, now +0.3%) and *"linear interpolation becomes less accurate than the composite method"* as sampling coarsens (LI −26.5% vs composite −16.0% at bimonthly). The Aulenbach-guidelines sentence was **reframed rather than deleted**: his recommendation of LI/averaging for weak C:Q now holds at daily sampling and only breaks down when data coarsen — a more useful finding than the flat contradiction it replaced.

**Claims that were wrong and are now corrected:**
- Figure 8 caption's *"persistent positive bias (~20%)"* for LI — an artifact of the averaging estimator.
- HBEF Ca paragraph claimed composite's residual corrections *"compensated for"* the rating method's −5% bias; in fact composite converges to −5.3% at coarse sampling, essentially identical to rating's −5.2%. The correction only works at daily sampling (composite < 1%).
- Figure 8 caption claimed composite had *"near-zero bias across sampling frequencies"*; **Beale** is actually least biased at coarse frequencies (+0.06%, +2.13%).
- Conclusions claimed LI and Beale were more reliable for weak C:Q; it is **composite and Beale** that degrade gracefully — LI and rating do not.
- Appendix a6 claimed Beale was *"very robust"* for turbidity; its median error reaches −41% at monthly with over half of repetitions exceeding 50%.
- Appendix a4 claimed composite had *"more constrained"* errors than rating; the spread is comparable at the coarsest frequencies.
- Discussion's *"up to ~50 percent"* method spread → **over 30% for HBEF/Plynlimon (max 35.5%) and over 70% for turbidity**.

**Figure 11 improved materially and is now reported honestly.** R² rose 0.74 → **0.79**, the slope moved from 1.59 ± 0.55 to **1.15 ± 0.34** (much nearer 1:1), and p fell from 0.063 to **0.042** — significant at α = 0.05. LI's mean bias vs sensor truth collapsed from +49.5% to **+3.2%**. But the *recommended* method still sits **+16%** above sensor truth while rating (−0.7%) and Beale (+1.2%) land within ~1%, so the text now reports n = 5, the p-value, and that residual bias rather than claiming the framework picks the best method.

**Claims verified as still correct, no edit needed:** the Abstract's "~10%" bound for daily/informative-C:Q (all methods ≤5% at HBEF Ca daily) and its ">50%" bound for coarse/complex C:Q (42–69% of turbidity repetitions exceed 50% at biweekly); the Conclusions' "within ~5%" for composite at weekly or finer; all NEON conductivity claims including *"prone to high errors at the COMO and WALK sites"* (LI reaches +30% at COMO and +15% at WALK by bimonthly); and both turbidity paragraphs.

<details><summary>Original M9a-3 checklist</summary>
- [x] **Abstract** — re-check "errors within ~10%" for daily/informative-C:Q and the ">50%" claim for coarse/complex-C:Q. LI was previously the main driver of the >10% daily errors; both bounds may tighten.
- [x] **Results, HBEF nitrate** — "Linear interpolation performed comparably with sub-weekly data" was false under `method1` (−36.9% at daily). Re-evaluate against the new numbers; it will likely now be *true*, which resolves the contradiction rather than requiring a rewrite.
- [x] **Results, HBEF nitrate** — the follow-on claim that LI "becomes less accurate than the composite method" as sampling coarsens and that autocorrelation-leveraging methods "outperform all others" at high frequency should now be genuinely supportable. Verify and keep.
- [x] **Results, HBEF calcium** — "Linear interpolation exhibited a persistent positive bias (~20%)" was an artifact of the averaging estimator. Replace with the new value; the Figure 8 caption carries the same claim.
- [x] **Results, Plynlimon nitrate** — "best estimated by the linear interpolation and composite methods" and the Figure 9 caption's ranking both need re-checking.
- [x] **Conclusions** — "for solutes with weak or complex C:Q relationships, linear interpolation and the Beale ratio estimator were more reliable" contradicted the Results under the old estimator. Re-derive from the new data and align the two sections (this also closes the M9b internal-contradiction item).
- [x] **Discussion, method-selection magnitude** — "method selection may influence estimate accuracy by up to ~50 percent" was computed with LI as the worst performer; recompute the method spread and restate.
- [x] **NEON Results** — the conductivity paragraph's claim that "Linear interpolation was effective with sub-weekly data but showed large errors at coarser frequencies, particularly at sites with variable discharge such as COMO" now rests on regenerated data for the first time. Verify against the new Figures a1–a4.
- [x] **Appendix captions a1 and a5** — both describe linear-interpolation behaviour ("prone to high errors at the COMO and WALK sites"; "does a poor job of handling the variability in turbidity data"). Re-check both.
- [x] **Figure 11** — the method comparison changes: LI previously showed +49.5% mean bias vs sensor truth, by far the worst of the seven series. Recompute all per-method biases and the recommended-method R². This interacts with the M9b Figure 11 overclaim item.
- [x] **Methods, LI section** — the text already says `method6`, so it becomes correct on its own. Add the RiverLoad citation once M10a resolves the missing Nava et al. (2019) entry.
- [x] ~~**Released dataset**~~ — moved to **M10f** (Nic-owned). M9 is now fully closed.
</details>

#### M9a-4: Decision-framework branch collapse — COMPLETE
- [x] ~~"Linear interpolation" and "simple average" branches were the same estimator~~ — Resolved by the switch. `li` is now genuine interpolation (`method6`) and `average` remains mean(C) × mean(Q over all days) (equivalent to RiverLoad `method4`), so the two branches are substantively different for the first time.
- [x] Methods text for the four decision-tree outcomes verified against the new behaviour (thresholds corrected to strict `>`, and the `abs()` on both autocorrelations now disclosed). Recommended-method distribution across the 26,639 solute-site-years that carry a recommendation is sensible: **li 38.3%, composite 28.7%, average 20.6%, rating 12.5%**.

#### M9a-5: Figure axes and code cleanup — COMPLETE
- [x] **X-axis tick labels on Figures 7–10 corrected.** Breaks were at `c(1,24,96,192,384,768)` hours labelled Hourly/Daily/Weekly/Biweekly/Monthly/Bimonthly — i.e. the "Weekly" tick was 4-day sampling, "Monthly" 16-day and "Bimonthly" 32-day. Now `c(24,168,336,730,1461)` = Daily/Weekly/Biweekly/Monthly/Bimonthly in both `02_coarsen_figure_hbef.R` and `05_coarsen_figure_plynlimon.R`. The 1-hour tick was dropped because on a linear axis it collides with the 24-hour tick and `check.overlap` would discard one unpredictably; sub-daily points still plot, they are simply not tick-labelled. Axis kept **linear** per Nic — people read time linearly.
- [x] **Silent truncation removed.** `filter(hours <= 899)` raised to 1700 (HBEF) and 1400 (Plynlimon) so the coarsest points (68-day and 56-day) are no longer dropped without disclosure.
- [x] **NEON conductivity made reproducible.** `'spCond'` restored to the solute loop in `06_coarsen_analysis_neon.R`; the previous `.RData` were orphaned from a pre-M7 run and the `spCond` save branch was unreachable dead code.
- [x] **NEON stream-order claim corrected** in the text to "first order" (the code filters `stream_order == 1` only).
- [x] Removed the orphaned 145-line `method1_month` inline definition from `source/flux_methods.R`; file re-parses clean.
- [x] Removed two unused `method_names` vectors keyed on the stale `pw` from `07_coarsen_figure_neon.R`; file re-parses clean.
- [x] Verified no `'pw'`, `calculate_pw`, or `method1(` reference remains anywhere in the codebase.
- [x] Figure files renamed to match paper numbering (`fig07`=NO₃, `fig08`=Ca, `fig09`=PLYN NO₃, `fig10`=PLYN Ca) via `git mv`; scripts 02 and 05 updated. Orphaned `figa_neon_*_pw.png` deleted, `figa_neon_*_li.png` added.

### M9b: Quantitative claims contradicted by the data

**Verification against the live `.docx` (13:24 save, re-extracted 2026-07-28 14:05):** M8b's accuracy sweep did land. The following are confirmed **already fixed** and need no further work:
- [x] ~~Watershed areas~~ — now reads 42.4 ha (HBEF) and 122 ha (Upper Hafren).
- [x] ~~Ca~SpCond "no intercept"~~ — now reads "a least squares regression line" with the qualifier removed.
- [x] ~~Ca~SpCond R²/slope~~ — now reads "an R-squared of 0.66, a slope of 0.059, and an intercept of 0.013".
- [x] ~~"Figures 7-12"~~ — now reads "Figures 7-10".

**APPLIED to the `.docx` 2026-07-28 ~14:15** (backup at `scratchpad/docx_backup/paper_backup_141417.docx`; every replacement asserted on an exact occurrence count, script aborts rather than half-applying). Data is the authority for all values below.

- [x] **Plynlimon discharge statistics were 100× too small.** "a mean of 1.08 Lps, a standard deviation of 1.36 Lps, and a yield of 3.4 x 10⁷ liters" → **107.6 Lps / 134.6 Lps / 3.4 x 10⁹ liters**. Sanity check: 122 ha × ~2000 mm/yr ≈ 2.4×10⁹ L; the old value implied ~28 mm of annual runoff at Upper Hafren.
- [x] **HBEF Ca mean/sd were stale** (2 places — Methods data description and the Figure 3 caption). 0.86 / 0.23 → **0.82 / 0.21 mg/L** (computed 0.8212 / 0.2144 under the free-intercept conversion).
- [x] **NO₃ incomplete-day count.** "3 days with incomplete data (2/25/2016, 6/18/2016, and 6/23/2016)" → **"14 days with incomplete data"**; the now-inaccurate three-date list was dropped. The four zero-data days (6/19–6/22) were correct and are unchanged.
- [x] **Plynlimon incomplete-day counts.** Ca 28 → **41**, nitrate 40 → **52**.
- [x] **The M8b count fix had introduced an error.** Results read "generated 133,425 site-years of data" — but 133,425 is the *row count* of `load_annual.csv` (26,685 × 5 methods), not site-years. Now reads **"generated 133,425 load estimates across 146 sites and 100 solutes"**.
- [x] **Terminology unified.** Abstract and Conclusions said "26,685 site-years" while Results said "site-solute-years". All three now read **site-solute-years**. (Note for M9a-3: 26,639 of the 26,685 carry a recommended method — mention if the text ever cites the recommended-method subset.)
- [x] **Figure 11 caption.** "biweekly, discrete grab samples" → **"weekly, discrete grab samples"** (diagnostics show 48–55 samples/water-year for w3 Ca).
- [x] **Section heading.** "Generating estimates for 93 watersheds" → **"Generating estimates for 146 sites"**. The body text's "93 federally funded watershed studies" is correct as written (93 studies → 146 sites) and was left alone.
- [x] **"Linear interpolation performed comparably with sub-weekly data" is wrong.** Median % error at daily sampling, HBEF: composite +0.9, Beale −2.8, rating −34.6, `pw` **−36.9**. `pw` is the worst method at every frequency for nitrate. Rewrite the sentence and the follow-on claim that LI "becomes less accurate than the composite method" only as sampling coarsens.
- [x] **Conclusions contradict Results on weak-C:Q solutes.** Conclusions state that "for solutes with weak or complex C:Q relationships, linear interpolation and the Beale ratio estimator were more reliable"; the Results correctly report composite winning for HBEF nitrate despite the weak fit (+0.9% vs −36.9%). Align the two.
- [x] ~~**Orphaned claim from the deleted simulation study.**~~ APPLIED — the sentence "Under chemostatic or no-pattern C:Q relationships, linear interpolation and the Beale estimator outperform the rating and composite methods, regardless of hydrologic regime" has been deleted. **Still to check in M9a-3:** the surviving first sentence of that paragraph ("users should rely on linear interpolation or averaging methods" when C:Q is weak) was contradicted by the old estimator; re-verify once the LI results land. Also confirm the "hydrologic regime" branch of Figure 12 is still defensible without the simulation.
- [x] ~~**Self-referential truth is never disclosed.**~~ APPLIED — added to the truth-definition paragraph: "Because the composite method also serves as our reference, its coarsening results should be read as a measure of sensitivity to sampling frequency rather than as an independent test of accuracy."
- [x] **Figure 11 is overclaimed.** R² = 0.735 (rounds to the stated 0.74) but **n = 5 years, p = 0.063** — not significant at α = 0.05 — and the fitted slope is 1.59 ± 0.55, not 1:1. Mean bias vs sensor truth: recommended **+13.5%**, Beale +1.2%, rating −0.7%. The framework did not pick the best method. Replace "a reaffirming case study that the Aulenbach 2016 decision flowchart sensibly chooses from among the best load estimation methods" with an honest statement reporting n, p, and the per-method comparison.
- [x] **"Biweekly grab samples" in the Figure 11 caption.** `data/load_annual_diagnostics.csv` shows 48–55 samples per water year for w3 Ca — that is **weekly**. Correct the caption.
- [x] **Data-gap counts.** HBEF nitrate: text says 3 incomplete days, actual is **14** (the 4 zero-data days, 6/19–6/22, are correct). Plynlimon: text says Ca 28 / NO₃ 40 incomplete days, actual is **41 / 52**.
- [x] ~~**Truth is computed on daily means, not the full high-frequency series.**~~ APPLIED — now reads "applying the composite method to **daily means of** the full, high-frequency time series".
- [x] ~~**Decision-framework thresholds.**~~ APPLIED — "greater than or equal to 0.30" → "greater than 0.30" (the string spanned a `<w:lastRenderedPageBreak />` marker), "lower than 0.30" → "of 0.30 or lower", `(>=0.20)` → `(>0.20)` ×2, `(<0.20)` → `(≤0.20)` ×2, and a new sentence discloses that "Both autocorrelations were evaluated as absolute values at lag one." Also fixed the "Rsquared" typo.
- [x] ~~**WRTDS is undocumented.**~~ RESOLVED, no text needed. `calculate_wrtds` is attempted for every site-solute-year but fails for all of them (the pipeline log is full of "ERROR: WRTDS failed to run"), so the `wrtds` branch at `calculate_annual_flux.R:207-213` never fires and the released `load_annual.csv` contains only `average, beale, composite, li, rating`. There is nothing in the dataset to document. **Code-level cleanup opportunity for later:** the WRTDS call still runs on every site-year and contributes materially to the MacroSheds runtime for no output. It is computed and written to `data/load_annual.csv` (`calculate_annual_flux.R:105-111, 207-214`) but never described in Methods. Either add a short Methods paragraph or drop it from the released dataset.
- [x] **Accuracy-threshold wording is inconsistent.** Abstract says "daily or better… errors within ~10%"; Conclusions say composite "within ~5% of truth at weekly or finer". Discussion says method selection shifts accuracy "by up to ~50 percent" — the median spread is closer to **~38%** (max 37.8 points, HBEF NO₃ at daily); ~50% holds only for min–max envelopes. Pick one framing and state which quantity is meant.

### M9c: Figures and captions

- [x] **Re-insert stale figures.** MD5-hashing the `.docx` images against `paper/figures/` shows Figures 7–11 and a1–a8 match the regenerated files byte-for-byte, but **Figures 2, 3, 4, 5, 6 and a9 match nothing on disk** — they predate the current versions. Figure 5 (Plynlimon C:Q) would still show the 100×-wrong discharge axis and Figure 2's inset the old no-intercept regression. Re-insert all six. (Re-verify against the latest `.docx` save first.)
- [x] ~~**Add short in-text references for unreferenced figures.**~~ APPLIED — the three orphaned assets are now **Figures A10, A11 and A12**, each added as a real embedded figure (new media part, relationship and drawing XML cloned from the Figure A9 template, extents recomputed from the PNG aspect ratio) with a caption and an in-text pointer. A10 = distribution of recommended-method load estimates for Ca and NO3-N; A11 = percentage spread between the highest and lowest methods per solute site-year; A12 = the Figure 11 comparison shown as an annual time series. Every appendix figure A1-A12 is now referenced in the text. Three assets exist but are never cited in the text:
  - `figa9_macrosheds_density.png` and `figa9_macrosheds_method_comp.png` — only one "Figure a9" (the load histogram) is referenced. Add brief references in the MacroSheds Results paragraph and give each its own appendix number.
  - `fig11_hbef_method_ts.png` — the Figure 11 caption describes only panels (A) scatter and (B) difference bars; the time-series panel is orphaned. Add a sentence referencing it, or fold it in as a panel.
- [x] **Figure 12 category names — fix in the text.** The caption defines three bins (simple, medium, complex) but the surrounding text uses five names: "simple", "easy", "fair", "medium", "complex", and switches between "fair" and "medium" mid-thought ("Data binned as 'fair' should only be used for limited applications. For example, 'medium' rated estimates could be used…"). Standardize the text on the caption's three bins. The text also refers to "error ranges presented for each category" which the caption does not mention — either add them to the figure or drop the phrase.
- [x] **Figure a9 content mismatch.** Text says Figure a9 shows "distributions of annual solute loads of nitrate (as nitrogen) and calcium"; the caption says only "a histogram of annual load estimates present in the MacroSheds dataset". Make them agree.
- [x] **Rename figure files to match paper numbering.** `fig07_hbef_ca_coarsening.png` is the paper's Figure 8 and `fig08_hbef_no3_coarsening.png` is Figure 7; same inversion for 09/10. Harmless in the PDF but a maintenance trap. Rename via `git mv` and update `02_coarsen_figure_hbef.R` / `05_coarsen_figure_plynlimon.R`. Also reconcile the `fig02`–`fig05` file numbering (8 files across 4 figure slots) with the manuscript's scheme.
- [x] ~~**NEON site-count wording.**~~ APPLIED — now reads "We tested conductivity at six first-order NEON sites (COMO, CUPE, KING, MAYF, TECR, WALK) and turbidity at five of these (all except COMO)".
- [x] ~~**Table 1 caption overreach.**~~ APPLIED — now reads "Note that all sites are larger than HBEF watershed 3, and all but WALK are substantially larger than the Upper Hafren catchment." (WALK 109 ha vs HBEF 42.4 and Upper Hafren 122.)

### M9d: Results / Discussion reorganization

Goal is a clean section split **without rewriting content or breaking narrative continuity** — move whole paragraphs or sentences, adjust only the connective tissue needed to keep transitions readable.

**APPLIED 2026-07-28 ~14:45.** Paragraph-level moves only — no text was rewritten. Implemented with a body-aware splitter (`scratchpad/docx_body.py`) that walks top-level `<w:body>` children, because Table 1's cells are also `<w:p>` and naive index splicing would have corrupted the table. Each source and destination was identity-checked against an expected opening fragment before any move, the transform round-trips exactly, and the element count is asserted. Body went 276 → 275 elements (one deletion).

- [x] ~~**Move the Ca~SpCond calibration paragraph out of Results.**~~ APPLIED — moved from between the nitrate discussion and the Figure 8 caption to immediately after the HBEF chemistry description in Methods, where it belongs alongside the regression it describes.
- [x] ~~**Two synthesis paragraphs out of Results.**~~ APPLIED — "Comparing the results from NO3-N and Ca across methods confirms…" and "These NEON results underscore the importance of C:Q relationship strength…" both moved into the Discussion's "Insights on load estimation uncertainty" subsection, in that order, after the existing coarsening-experiments paragraph.
- [x] ~~**Reorder around Figure 10.**~~ APPLIED — "Comparing Figures 8 and 10 highlights…" now follows the Figure 10 caption instead of sitting between the Figure 9 caption and the Figure 10 image. Figures 9 and 10 now appear back-to-back, which reads correctly because the preceding Plynlimon paragraph introduces both.
- [x] ~~**Merge the orphan NEON sentence.**~~ APPLIED — deleted "Results from the analysis of NEON data are available in the Appendix as Figures a1-a8"; the following two paragraphs already cite Figures a1–a4 and a5–a8 directly.

**Deliberately sequenced into M9a-3 rather than done here** (all three touch paragraphs whose linear-interpolation numbers change with the rerun, so doing them now would mean editing the same text twice):
- [x] ~~Sentence-level moves out of Results~~ APPLIED 2026-07-28 ~15:40, after the delta table made the rewrite possible. Three sentences lifted from Results into the Discussion's "Insights on load estimation uncertainty" paragraph: the Aulenbach (2016) guidelines comparison (**reframed** — see below), the Fazekas (2021) citation (verbatim), and the Beale recommendation (verbatim). The receiving paragraph was rebuilt to absorb them coherently.
  - **The Aulenbach sentence had to be reframed, not just moved.** It previously read "This is contrary to the guidelines suggested by Aulenbach et al. (2016)…" — true under the old averaging estimator, where LI was −36.9% at daily. With genuine interpolation the picture inverts: Aulenbach's recommendation of LI/averaging for weak C:Q now *holds* at daily sampling (LI +0.3%, matching composite) and only breaks down as data coarsen (LI −26.5% vs composite −16.0%). The Discussion now says exactly that, which is a more useful finding than a flat contradiction.
  - Same paragraph's opening claim ("users should rely on linear interpolation or averaging methods" for weak C:Q) was corrected to add the frequency condition.
  - Also tightened the moved synthesis paragraph: "composite… produce accurate estimates across all sampling frequencies" → "within about 5% of truth", since composite settles near −5% for Ca at coarse frequencies rather than staying near zero.
- [x] ~~**Give Figure 11 a Methods and Results home.**~~ APPLIED 2026-07-28. Three-way split, done after the rerun so the numbers only had to be written once:
  - **Methods** gains a Heading3 "Comparison with a sensor-derived record" stating the provenance that was previously missing entirely — grab samples come from the MacroSheds record for the site (~52 Ca samples/water-year), the sensor reference converts 15-min specific conductance via the regression then sums C×Q per timestep, published values are HBWatER (2023), and the comparison spans the five water years (2013–2017) where both records are complete.
  - **Results** gains a Heading2 "Sensor-derived load comparison" holding the figure, its caption, and a purely factual paragraph (R² 0.79, slope 1.15 ± 0.34, p = 0.04, recommended +16%, rating/Beale within ~1%, composite +21%, LI +3%).
  - **Discussion** keeps only interpretation, and now closes on the honest point: "the residual bias in the recommended estimates is a reminder that a defensible choice is not always the most accurate one."
  - Body 279 → 283 elements; the two headings were deliberately given different titles so Methods and Results don't collide.

**Still open, independent of the rerun:**
- [x] ~~**Split the MacroSheds methods paragraph.**~~ APPLIED (Option B, chosen by Nic). The 308-word / 14-sentence block became three paragraphs plus a new **Table 2**:
  - **P1** — what MacroSheds is (dataset, EDI link, 93 studies).
  - **P2** — what we applied and at what level, ending with a new lead-in: "Each solute year was then assigned a recommended method using cutoffs of 0.30 for the model R-squared and 0.20 for autocorrelation, as summarized in Table 2."
  - **Table 2** — the four classification rules, replacing 118 words of nested prose. Caption: "Simplified application of the Aulenbach et al. (2016) decision framework, used to assign a recommended load estimation method to each solute site-year. R-squared is from a log-log linear model of concentration against discharge; both autocorrelations are absolute values at lag one."
  - **P3** — the QA filters (85% discharge coverage, one chemistry sample per water-year quarter), previously buried as the last clause of the block.
  - Table styled to match Table 1 (TableGrid, 12 pt centred header, 9 pt body, same 7287 total width). Body 275 → 279 elements; re-split of the rebuilt XML asserts exactly 2 tables.
  - Two pre-existing errors were eliminated with the replaced sentences: "Solute years with **a** … C:Q **fits**" (article/plural disagreement) and a present/past tense inconsistency ("**are** recommended" vs "were recommended" elsewhere).
- ~~**Consider a "Study sites and data" subsection.**~~ — Dropped per Nic 2026-07-28: the rest of the manuscript covers the site and data description well enough; not worth disturbing the flow.

### M9e: Smaller items

- [x] ~~Delete the raw Word XML tag `<w:t xml:space="preserve">`~~ — **WITHDRAWN, not a real defect.** The literal tag was an artifact of my *extraction* script, whose regex matched `<w:tab />` as an opening `<w:t>` tag. The actual paragraph contains only a space and a tab. No document change made.
- [x] ~~Remove the stray image embedded inside the Likens et al. (1970) reference entry~~ APPLIED (`image29.png`, confirmed real by hashing the embedded media).
- [x] ~~"(Figures 7-12 …)"~~ — already fixed by M8b; now reads Figures 7-10.
- [x] ~~"As shown in Figure 8"~~ APPLIED → **Figure 11** (the sensor-truth comparison).
- [x] ~~"enriching trend at high flows"~~ APPLIED → "**diluting relationship** at high flows" (HBEF Ca log-log slope is −0.12). Note the same paragraph's LI bias figure still needs the M9a-3 pass.
- [x] ~~Figure 2 caption documents no inset~~ APPLIED — caption now ends "…grab samples of calcium; that regression is shown inset."
- [x] ~~Figure 2 cross-reference~~ VERIFIED, no change needed — Figure 2's caption does describe the streamflow record ("Streamflow was collected using a long-running rating, v-notch weir, and stage recorder"), so citing it for the HBEF discharge series is correct. Original concern, but its caption describes HBEF only (Plynlimon has its own Figure 4). Fix the reference or the caption. Same issue for discharge, which is cited to Figures 2 and 4 whose captions are chemistry time series.
- [x] ~~figshare DOI is inconsistent~~ APPLIED — all three now read `https://doi.org/10.6084/m9.figshare.24975504.v2`. The three references were in three different forms: full versioned URL (Data Availability), bare inline text (Results), and a bare string inside its own 9 pt run (Figure a9 caption), which needed a separate anchor. **Nic must bump the version in all three after re-uploading `load_annual.csv`.**
- [x] ~~Figure a9 content mismatch~~ APPLIED — `09_macrosheds_descriptive.R:13` filters `var %in% c('Ca','NO3_N')` and facets into two panels, so the body text was correct and the caption was under-specified. Caption now reads "Histograms of annual calcium and nitrate-N load estimates in the MacroSheds dataset."
- [x] ~~MacroSheds EDI link~~ APPLIED — text now reads "&revision=1, the revision used here; the latest version is linked at macrosheds.org". Original concern while the text says "the latest version is linked at macrosheds.org" — pin to the revision actually used.
- ~~Add HESS-required sections (Author contributions, Competing interests, Acknowledgements)~~ — Dropped per Nic 2026-07-28: handled at submission.
- [x] ~~Move the Data Availability section~~ APPLIED — Heading1 order is now Abstract | Introduction | Methods | Results | Discussion | Summary and Conclusions | Data Availability | Works Cited | Appendix A. Original item to **after the Conclusions** (placement affects readability now, independent of submission formatting).
- [x] ~~Renumber appendix figures~~ APPLIED — all labels and range references are now A1-A12 (17 individual labels plus three ranges), and the appendix heading reads "Appendix A". Original item and label the appendix "Appendix A" per HESS style.
- [x] ~~Verify the three study goals render as a numbered list~~ VERIFIED — all three paragraphs carry `<w:numPr>`, so they are a genuine Word numbered list. Original item (they extract as three separate one-sentence paragraphs).
- [x] ~~Equation 2 dimensional gap~~ APPLIED — the explanatory sentence now adds "The summed product is divided by watershed area to give the areal load in kg/ha/year." Original item into the equation — a genuine dimensional gap in the explanation, independent of image rendering.
- [x] Typos and spacing: "Miam FL" → Miami; "spread across the **county**" → country; "four common load methods **this** study" → for this study; "archetypal of the array methods" → of the array of methods; "(2016),which" → "(2016), which"; "'complex'have" → "'complex' have"; "Note that **All** sites" → all; "time series' standard deviation"; stray multi-space runs mid-sentence in the Introduction and the linear-interpolation Methods paragraph (tracked-change artifacts).

---

## M10: Final Tasks — References and Read-Through

Nic-owned, pre-submission. Reference management is handled in Mendeley.

### M10a: Missing and broken references
- [ ] **`Appling et al. 2015`** — cited ~8 times including as the source of the composite implementation, **absent from Works Cited**.
- [ ] **`Nava et al. 2019`** — the RiverLoad package citation, the software the whole analysis runs on, cited 4 times, **absent from Works Cited**.
- [ ] **`Colin et al., 2012`** — the primary Plynlimon data citation, used in Data Availability and Methods, **has no entry**. "Colin" is Neal's first name; the correct entry is `Neal, Kirchner & Reynolds (2013)`, which is already in the list. The same dataset is currently cited three ways ("Colin et al., 2012", "Kirchner and Reynolds, 2013", and the list entry). Consolidate; the list entry also has a truncated author ("Reynold. B.").
- [ ] **`Gaillardet et al. 1999`** — cited in the Introduction, absent.
- [ ] **`Likens et al. 1977`** — cited in the Introduction, absent (only Likens et al. 1970 and Likens & Buso 2006 are listed).
- [ ] **NEON has no citation anywhere** — no data citation, no data product IDs (DP1.20016, DP1.20033, etc.), no DOI, no release version, and Table 1's watershed areas have no stated source. One of three primary data sources with zero provenance; this alone will trigger a HESS data-availability objection.

### M10b: Year and name mismatches — COMPLETE (2026-07-28)
- [x] `Richards and Holloway, 1986` (in text, 4×) vs **1987** in the list — both years used in text.
- [x] `Dodds et al., 2008` (text) vs **2009** (list).
- [x] `Newman et al., 2014` (text) vs **2015** (list).
- [x] `Coombs and Melack, 2012` (text) vs **2013** (list).
- [x] "Shilling et al., 2017" → **Schilling**.
- [x] "Aulenbauch et al., 2016" → **Aulenbach**.
- [x] "Appling et al., 2016" → **2015** (as used everywhere else).
- [x] "Likens et al., 2006" → **Likens & Buso** (two authors; "et al." is wrong).
- [x] "…Shilling et al., 2017; **etc.**)" — "etc." inside a citation parenthetical is not acceptable.

### M10c: Uncited entries and uncited claims
- [ ] Five list entries are never cited: **Fuka 2014, Godsey 2009, Koger 2018, Moatar (n.d.), Zeileis (n.d.)**. Moatar and Zeileis also lack years.
- [ ] Three of those (Godsey 2009, Koger 2018, Moatar) are C:Q/chemostasis papers, while the paper uses "chemostatic", "enriching", "diluting", and "no pattern" as load-bearing terms — including in Figure 12 — **without ever defining or citing any of them**. This looks like a deleted Introduction paragraph on C:Q behaviour; consider restoring a short one, which would also give Figure 12's branches their grounding.
- [ ] Attributable claims lacking citations: "archetypal of the array methods commonly used in small-watershed ecosystem studies"; "linear interpolation is commonly used in studies"; "The composite method has become a premier choice for many loading analyses"; the HBEF instrumentation description (sonde, UV-VIS nitrate analyzer, v-notch weir, stage recorder — HBWatER 2023 is cited elsewhere but never attached here); "the underlying assumption of covariance of discharge variance and concentration variance" (→ Beale 1962 / Meals 2013); "COMO, a snowmelt-dominated system in Colorado"; "reinforce previous findings that generally, when there is not a strong C:Q relationship…" ("previous findings" with no citation); "(which is common with nonevent supplemented sampling)"; "baseflow-quickflow separation methods"; and the MacroSheds R package, recommended by name with no package citation.

### M10d: Manual figure replacement in Word

Every other figure was refreshed in place by overwriting the embedded image bytes (`scratchpad/refresh_figures.py`, which keeps `wp:extent` so page layout is preserved). **These two could not be**, because each embeds a *single composed image* covering both chemistry and streamflow while the scripts emit them as two separate files — dropping one file into the single slot would silently delete the streamflow panel.

- [ ] **Figure 2** ← `paper/figures/fig02_hbef_chem_ts.png` (this one already carries the Ca–SpCond regression inset the caption now mentions) **+** `paper/figures/fig03_hbef_streamflow.png`
- [ ] **Figure 4** ← `paper/figures/fig02_plynlimon_chem_ts.png` **+** `paper/figures/fig03_plynlimon_streamflow.png`

Notes for whoever does this:
- Compose them the same way the current versions are laid out (chemistry above, streamflow below) so the captions still read correctly.
- Both source files were regenerated in the 2026-07-28 rerun, so the *current* embedded versions are stale — Figure 2's inset still shows the old no-intercept regression and Figure 4's companion streamflow panel still reflects the old Plynlimon discharge conversion.
- ~~Figure 1 and Figure 12~~ are hand-made illustrations with no script output; leave them as they are.
- After replacing, re-run `scratchpad/figmap.py` — it lists any embedded image that no longer matches a file on disk, so a clean run means nothing stale is left.

### M10e: Formatting and final pass
- [ ] Retitle "Works Cited" → "**References**" per HESS convention.
- [ ] Standardize reference formatting via Mendeley (DOIs present/absent, mixed date formats).
- [ ] Replace the "FINAL VERSION LINK" placeholder in Data Availability with the repo URL.
- [ ] Final read-through for clarity and flow (blocked on M9 and all other milestones).

---

### M10f: Released dataset — figshare (moved from M9)

- [ ] **Released dataset** — `load_annual.csv` changes (both the `li` load values and any `ms_recommended` flags on LI-recommended site-years). Re-upload to figshare and mint a new DOI version; update the DOI in Data Availability, Results, and the Figure a9 caption. **Nic-owned — the only M9 item still outstanding.** DOI standardisation (`.v2` vs bare, 3 places) folds into this.
- [ ] After re-uploading, bump the version in **all three** DOI references (Data Availability, the MacroSheds Results paragraph, and the Figure A9 caption). They are currently standardised to `https://doi.org/10.6084/m9.figshare.24975504.v2`, so it is a find-and-replace of the version suffix.

### M10g: Code tasks still outstanding (not Nic-owned — flag for a future milestone)

These are the only known code stragglers. Neither blocks submission.

- [ ] **Global `area` variable** (`source/flux_methods.R`) — injected via `assign('area', area, envir = .GlobalEnv)` and read as a free variable by `calculate_li`, `calculate_beale`, `calculate_rating`, `calculate_wrtds` and `calculate_composite_from_rating_filled_df`. Prevents safe parallelism and makes the functions order-dependent. Fix is to add `area` as an explicit parameter and update all call sites. Deferred since M7d because it touches every flux function signature.
- [x] ~~**WRTDS runs for nothing**~~ DONE 2026-07-28 — cut entirely per Nic. Removed `calculate_wrtds` and the ~670 lines of EGRET machinery from `source/flux_methods.R` (873 → 163 lines), deleted `source/egret_overwrites.R`, stripped the computation and output blocks from `calculate_annual_flux.R`, and dropped EGRET from the required packages. Verified nothing outside WRTDS used the EGRET helpers, then confirmed behaviour-preserving by regenerating Figures 8 and 11 to **byte-identical md5s**. The paper needed no change: its three WRTDS mentions are literature references (Hirsch et al. 2010, Lee et al. 2019), not claims about our computation.

## M7: Adversarial Code Review

The goal is to document all bugs, logic errors, and methodological concerns found through an adversarial review of the full codebase — prioritized by potential impact on paper results.

### M7a: Critical — could change paper results
- [x] `hold_factor` contamination across flow regimes (`01_ts_simulation_analysis.R:161-206`) — `hold_factor` was overwritten three times without saving intermediate values. Fixed: renamed to `hold_factor_unalt`, `hold_factor_storm`, `hold_factor_base` and used the correct factor for each flow regime's chemistry series.
- [x] `mean_or_x` computes variance instead of mean (`ms_overwrites.R:303`) — copy-paste from `sd_or_0`. Fixed: `mean(var(x))` → `mean(x)`.
- [x] `wyday`/`enlightened_yday` reorder output (`flux_methods.R:414-486`) — concatenating `c(firsthalf, secondhalf)` returned results in wrong order. Fixed: use boolean index mask to assign values back to original positions.
- [x] ~~Self-referential truth in coarsening experiments~~ — Deliberate choice per Nic; no change needed.

### M7b: High — affects specific analyses
- [x] Script 15 ignores intercept in Ca prediction (`15_hbef_method_comparison.R:52,92`) — Fixed: added `+ lm_fit$coef[[1]]` to both the dead-code truth (line 78) and the actual truth computation (line 92).
- [x] Inconsistent Ca~SpCond models — Unified: all scripts now use free-intercept `Ca ~ spCond` on HBEF CSV data (intercept=0.01283, slope=0.05906). Config.R updated with `CA_SPCOND_INTERCEPT`. Script 15 no longer fits its own model; scripts 04/05/14/15 all use config constants.
- [x] NEON turbidity filename parsing bug (`10_coarsen_figure_neon.R:84-90`) — Fixed: turbidity filenames extract year from 3rd underscore field (stripping `turb` prefix) and site from 4th field.
- [x] `calculate_wrtds` tryCatch doesn't return NA on error (`flux_methods.R`) — Fixed: assigned `tryCatch(...)` result to `flux_from_egret` so the error handler's NA propagates correctly.
- [x] Leap-year scaling bug (`flux_methods.R:173-181`) — Fixed: replaced per-row loop with vectorized `sapply` that applies correct day count per year.
- [x] `molecular_conversion_map` indexed by position not name (`ms_overwrites.R`) — Fixed: changed `[v]` to `[[convert_molecules_element[v]]]` (name-based lookup) on all 4 references.

### M7c: Medium — correctness issues, limited result impact
- [x] No-flow replacement uses full mean instead of 0.1% (`flux_methods.R:696`) — Fixed: `mean_flow` → `mean_flow * 0.001` per USGS WRTDS manual.
- [x] Gap-blanking loop only processes last break (`flux_methods.R:870`) — Fixed: `for(i in length(...))` → `for(i in 1:length(...))`.
- [x] `drain_area_va` conversion 100x off (`flux_methods.R:783`) — Fixed: `area / 2.59` → `area / 259`.
- [x] Plynlimon Q unit conversion 100x off — Fixed in 3 files (`07`, `08`, `14`): `(1000/1) * (1/10000)` → `(1/1000) * (10000/1)`.
- [x] `convert_unit` precedence bug (`ms_overwrites.R:235`) — Fixed: `length(new_fraction == 2)` → `length(new_fraction) == 2`.
- [x] Non-finite composite concentrations set to 0 (`flux_methods.R:315`) — Fixed: `0` → `NA` so downstream code handles missing values explicitly.
- [x] Outlier definition uses median±1.5×IQR (`02_ts_simulation_figure.R:405`) — Fixed: now computes Q1/Q3 and uses standard boxplot fences.
- [x] Mislabeled "95% CI" is a prediction interval (`02_ts_simulation_figure.R`) — Fixed: renamed to "95% PI" in table output.
- [x] `SiO2_S` / `SiO3_S` suffix in `ms_overwrites.R:76-77` — Fixed: `_S` → `_Si`.

### M7d: Low — code quality / robustness
- [ ] Global `area` variable used as free variable throughout `flux_methods.R` — injected via `assign('area', area, envir = .GlobalEnv)`, prevents safe parallelism. Deferred — requires refactoring all flux method function signatures.
- [x] `base_storm_sep.R:2` runs `devtools::install_github` every time sourced — Fixed: wrapped in `requireNamespace()` guard.
- [x] Hardcoded date `20221221` in simulation output filenames — Fixed: removed date from filenames in scripts 01, 02, 03. Now `{freq}Freq_{reps}Reps.csv`.
- [x] ~~`calculate_truth_ts.R` hardcodes `site_code = 'w3'`~~ — Already uses `HBEF_SITE_CODE` from `source/config.R`.
- [x] Copy-paste bug in `02_ts_simulation_figure.R:355` — Fixed: `p15_data$method` → `p19_data$method`.
- [x] `old_bottom`/`new_bottom` used without initialization for non-fractional units (`ms_overwrites.R`) — Fixed: guarded with `length(fraction) == 2` checks, defaulting `_conver` to 1 when no denominator.

### M7e: Rerun analyses and regenerate figures
The M7 fixes change computed values in multiple code paths. Simulation scripts (01–03) removed from repo; remaining scripts renumbered 01–12. All analysis scripts must be rerun to regenerate intermediate data files and figures.

- [ ] Refactor global `area` variable — add `area` as explicit parameter to `calculate_pw`, `calculate_beale`, `calculate_rating`, `calculate_wrtds` in `flux_methods.R`; update all call sites (deferred)
- [x] Removed simulation scripts (old 01, 02, 03) and base_storm_sep.R — already removed from paper text
- [x] Renumbered remaining scripts 04→01 through 15→12 via git mv
- [x] Updated 00_run_all.R, CLAUDE.md, README.md, data/README.md with new numbering
- [x] Rerun `01_coarsen_analysis_hbef.R` — wyday fix affects WRTDS; Ca intercept fix; non-finite concentration fix
- [x] Rerun `02_coarsen_figure_hbef.R` — regenerate Figs 7–8
- [x] Rerun `04_coarsen_analysis_plynlimon.R` — Q conversion fix changes absolute flux values; wyday fix affects WRTDS
- [x] Rerun `05_coarsen_figure_plynlimon.R` — regenerate Figs 9–10
- [x] Rerun `06_coarsen_analysis_neon.R` — wyday fix affects WRTDS
- [x] Rerun `07_coarsen_figure_neon.R` — filename parsing fix corrects year/site labels on turbidity figures
- [x] Rerun `12_hbef_method_comparison.R` — intercept fix changes sensor-derived truth values
- [x] Diff regenerated figures against current versions — check whether paper text claims still hold
- [x] Update paper text if any quantitative claims changed

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
| M7e | Rerun analyses + regenerate figures | M7a + M7b + M7c + M7d |
| M8a | Coarsening figure crops + colors | M7e (rerun complete) |
| M8b | Additional figure issues (TBD) | M7e |
| M9a | Method identification + figure axes | M8 (decision needed on method1/method6) |
| M9b | Quantitative claim corrections | M9a (axis fix changes frequency claims) |
| M9c | Figures and captions | M9a (figures regenerated) |
| M9d | Results/Discussion reorganization | M9b (text settled first) |
| M9e | Smaller items | — (can run in parallel) |
| M9a | LI method fix + figure axes | M8 |
| M10a-c | References (Nic-owned) | — |
| M10d | Manual replacement of Figures 2 and 4 in Word | M9 (figures regenerated) |
| M10e | Formatting + final read-through (Nic-owned) | All other milestones |

## Resolved Questions

See `plans/decisions_made.txt` for the full decisions log.
