# M4 Plan: Reconcile Narrative Text

**Created:** 2026-07-27
**Updated:** 2026-07-27 — docx skill now available; Claude can read and edit the paper directly.
**Depends on:** M4a can start now (parallel with M2/M3). M4b requires M3d (final figures). M4c requires M4a + M4b.
**Source paper:** `paper/paper_HESS_draft_v2.docx` (READ-ONLY — never modify original drafts)
**Working copy:** `paper/paper_HESS_draft_v2_claude.docx` (created by copying v2; all edits go here)
**Workflow:** Read via `pandoc -t markdown`. Edit via unzip → `merge_runs.py` → edit `word/document.xml` → rezip → `validate.py`. Always operate on the `_claude` copy.

---

## M4a: Fix Paper Structure

Can start immediately — these are structural issues independent of figure regeneration.

### M4a-1: Rename "Conclusions" → "Results" and add real Conclusions

- [x] Rename "Conclusions" heading → "Results"
- [ ] Draft and insert "Summary and Conclusions" section — blocked on M4b (needs final results)

**Deliverable:** Draft a new "Summary and Conclusions" section (~400–600 words) that:
- Summarizes the key findings from the simulation, HBEF coarsening, Plynlimon validation, NEON application, and MacroSheds analysis
- States the main recommendation (which method to use under which conditions, per the decision flowchart)
- Identifies limitations and future directions (e.g., extension to other solutes, larger watersheds, event-based sampling)
- References the decision flowchart (Fig 12) as the practical takeaway

**Action:** Edit `word/document.xml` directly — rename the heading and insert the new section.

### M4a-2: Fix figure numbering — ~~the missing Figure 13~~ numbering gaps

- [x] Identified gaps: Figs 7, 8, 13 were never referenced — pure numbering gaps, not missing content
- [x] Renumbered all figures sequentially: 9→7, 10→8, 11→9, 12→10, 14→11, 15→12 (now 1–12)
- [x] Verified: 15 occurrences updated, appendix figures (a1–a9) untouched

### M4a-3: Expand NEON results

- [ ] Draft 2–3 paragraphs expanding NEON results — blocked on M3 (needs NEON coarsening output)

Current paper says roughly "see Appendix" with one paragraph for the NEON analysis. HESS reviewers will flag this.

**Deliverable:** Draft 2–3 paragraphs (~300–500 words) summarizing:
- Which NEON sites and variables were tested (conductivity, turbidity at MAYF, COMO, CUPE, KING, WALK, TECR)
- How results compare to HBEF — do the same methods win? At what sampling frequencies?
- Whether site-to-site variability matters more than method choice
- Key differences from HBEF/Plynlimon (first-order streams, different solute proxies, shorter records)
- Reference to Appendix figures (a1–a8) for full detail

**Source data:** The NEON coarsening results from `neon_discussion/1_coarsen_analysis_neon.R` output files.

### M4a-4: Verify Table 1

- [x] Table 1 content verified intact in XML (paras 109–129) — all 6 sites, areas, and data types present

### M4a-5: Clarify MacroSheds site-year counts

- [x] Added quality-filter clarification after "210,058 site-years" sentence in the `_claude` copy

---

## M4b: Update Results Text to Match Regenerated Figures

**Blocked until:** M3d (figures finalized after M2 bug fixes). The M2d verification found two pre-existing bugs:
1. ts_simulation used `simulated_series[[4]]` (chemostatic) instead of `[[5]]` (no-pattern) for "no pattern / base flow" truth
2. Missing closing braces in HBEF coarsening script

These fixes may change quantitative results. M4b reconciles the text with whatever the corrected code produces.

### M4b-1: Audit quantitative claims — blocked on M3d

**Process:**
1. Run all scripts in order after M3 is complete
2. Extract key statistics from output files (CSV results, figure data)
3. Compare each quantitative claim in the paper against actual computed values
4. Flag any discrepancies

**Key claims to check (non-exhaustive):**
- Error percentages at each sampling frequency for each method (HBEF Ca, HBEF NO3)
- Which method achieves ±5% / ±20% accuracy at which frequency
- Ranking of methods by accuracy (does the corrected no-pattern truth change any rankings?)
- Plynlimon results vs HBEF results (same patterns or different?)
- MacroSheds method agreement statistics

**Deliverable:** A table of `[claim in paper] → [actual value from code] → [match? / needs update]` with replacement text for any mismatches.

### M4b-2: Update figure captions — blocked on M3d

After M3 regenerates all figures with the unified theme, some captions may need updating:
- Color descriptions (if palette changed from original)
- Panel labels (if figure layout changed)
- Error band descriptions (now using standardized ±5%/±20% from `plot_theme.R`)
- Any new panels added or removed

**Deliverable:** Updated caption text for each figure, keyed by figure number.

### M4b-3: Reconcile simulation results text — blocked on M2d

The ts_simulation bug fix (wrong truth for "no pattern / base flow") could change the narrative about how methods perform under the no-pattern C:Q regime. This is the most likely place where M2 bug fixes will ripple into the text.

**Deliverable:** Re-read the simulation results paragraphs, compare against the corrected `pop_test.png` and `supp_table.csv`, and draft replacement text if needed.

Note: simulation results are not discussed in the current paper text (they go to supplement), so this may have no text impact.

---

## M4c: Clean Up Writing

**Depends on:** M4a (structure is fixed) + M4b (results are reconciled).

### M4c-1: Fix known typos and errors

- [x] "Plylimon" → "Plynlimon" (1 occurrence)
- [x] "thatgenerally,when" → "that generally, when"
- [x] "6/19/20166/22/2016" → "6/19/2016–6/22/2016"
- [ ] "FINAL VERSION LINK" → actual URL — needs URL from Nic

### ~~M4c-2: Fix equation rendering~~ — NON-ISSUE

Equations 1–3 are embedded as inline images (`<w:drawing>` elements), not OMML math markup. They render correctly in Word. The M0 report was based on text extraction which can't read images.

### M4c-3: Standardize Works Cited

- Some references have DOIs, some don't
- Mixed date formats
- Some are incomplete

**Deliverable:** This is best handled by exporting the bibliography from a reference manager (Zotero/Mendeley) in HESS format. Claude can flag inconsistencies but manual cleanup via the reference manager is more reliable.

**Action for Nic:** Export bibliography from reference manager in HESS citation style. Claude can flag inconsistencies in the extracted text, but reference manager cleanup is more reliable for bulk formatting.

### M4c-4: Reduce Results ↔ Discussion repetition

- [x] Analysis complete — see `plans/m4_deliverables/repetition_analysis.md`
- [ ] Apply item 1 fix (remove "defensible" judgment from Results) — deferred to M4c-5 pass

Repetition is milder than M0 suggested. One clear cut, two judgment calls for Nic.

### M4c-5: Final read-through — blocked on all other M4 items

- [ ] Complete read for clarity, flow, and HESS style compliance

**Action:** Full read of the pandoc-extracted text, then apply line-level edits directly to the `_claude` copy. Use the docx comment tool (`scripts/comment.py`) to leave notes on passages that need Nic's judgment rather than a mechanical fix.

---

## Deliverable Format

All edits are made directly to `paper/paper_HESS_draft_v2_claude.docx` (a copy of v2). The original drafts are never touched. After each sub-milestone, the `_claude` copy is validated with `validate.py --original paper_HESS_draft_v2.docx` and converted to PDF for visual inspection. Nic reviews the `_claude` copy in Word and either accepts the changes or asks for revisions.

For sub-milestones that need Nic's input before editing (e.g., which figure to insert for the Fig 13 gap), a short markdown note is produced in `plans/m4_deliverables/` with the question and options.

---

## Sub-milestone Status

| Sub-milestone | Status | Notes |
|---------------|--------|-------|
| M4a-1 | **DONE** | Heading renamed + Conclusions section (5 paragraphs) inserted in `_claude_final.docx`. |
| M4a-2 | **DONE** | Renumbered 9→7, 10→8, 11→9, 12→10, 14→11, 15→12. Fixed 3 cross-ref bugs in _final copy. |
| M4a-3 | **DONE** | 3 paragraphs expanding NEON results (conductivity, turbidity, synthesis) in `_claude_final.docx`. |
| M4a-4 | **DONE** | Table 1 content verified intact in XML (paras 109–129). |
| M4a-5 | **DONE** | Added quality-filter clarification after "210,058 site-years" in para 165. |
| M4b-1 | **IN PROGRESS** | Quantitative audit complete. 3 trivial fixes applied (yield exponent, Ca-SpCond slope, Ca sd). 4 judgment-call proposals written to `plans/m4b_quantitative_audit_proposals.md` for Nic review: Plynlimon C:Q values, Ca-SpCond R², MacroSheds counts, Plynlimon missing days. |
| M4b-2 | Deferred | Caption content unchanged; only theme styling changed. |
| M4b-3 | Deferred | Simulation results go to supplement, not main text. Minimal text impact. |
| M4c-1 | **DONE** | Fixed typos. "FINAL VERSION LINK" still needs URL from Nic. |
| M4c-2 | **NON-ISSUE** | Equations are inline images in the XML, not OMML. |
| M4c-3 | Nic task | Needs reference manager export in HESS style. |
| M4c-4 | **DONE** | Repetition item 1 applied (removed "defensible" from Results). Items 2–3 left for Nic. |
| M4c-5 | Nic task | Final read-through. All structural edits now complete. |

**Working copies:**
- `paper_HESS_draft_v2_claude.docx` — previous working copy (cross-ref bugs, no NEON expansion, no Conclusions)
- `paper_HESS_draft_v2_claude_final.docx` — current working copy with all fixes applied
