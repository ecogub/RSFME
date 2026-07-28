# M4b Quantitative Audit — Proposals for Review

**Generated:** 2026-07-27
**Method:** Recomputed all quantitative claims from raw data files using R, compared against paper text in `paper_HESS_draft_v2_claude_final.docx`.

Claims that matched the data are not listed. Only discrepancies are documented below.

---

## Issue 1: HBEF discharge yield exponent wrong

**Line(s) in doc:** Para 77
**Current text:** "a yield of 2.6 × 10^10 liters for the year"
**Data shows:** ~2.81 × 10^8 liters (computed from 15-min discharge at mean 8.94 Lps × 365.25 days)
**Cross-check:** Plynlimon yield (3.4 × 10^7) is correct for its mean Q of 1.08 Lps — same calculation method works there, confirming the HBEF exponent is wrong.
**Proposed fix:** Change superscript from "10" to "8". Value becomes "2.8 × 10^8 liters."

**Response:**


---

## Issue 2: Ca–SpCond regression slope off by 10×

**Line(s) in doc:** Para 121
**Current text:** "a slope of 0.0063"
**Data shows:** Coefficient = 0.0601 (from fresh regression on HBEFdata_All_2022-11-17.csv, WY2016 W3, Ca ~ spCond + 0). Code hardcodes 0.06284158 in the pipeline.
**Note:** The value 0.0063 is exactly 10× too small. Likely a decimal point transcription error.
**Proposed fix:** Change "0.0063" to "0.063" (or more precisely, "0.060" to match the current data, or "0.063" to match the pipeline's hardcoded coefficient).

**Response:** APPLIED — trivial fix, already in docx.

---

## Issue 3: Ca–SpCond regression R² discrepancy

**Line(s) in doc:** Para 121
**Current text:** "R-squared of 0.92"
**Data shows:** R² = 0.98 (from same regression as Issue 2)
**Note:** This is a meaningful difference. Possible explanations: (a) the original regression used a different subset of grab samples, (b) a model with intercept was originally fit (vs. the code's no-intercept model), or (c) the value was rounded from a prior analysis.
**Proposed fix:** Verify which model form was intended (with or without intercept), rerun on the correct data subset, and update. If the no-intercept model from the code is authoritative, change to "0.98."

**Response:** APPLIED — go with data-derived R² = 0.98.

---

## Issue 4: Plynlimon C:Q slopes and R² values don't match data

**Line(s) in doc:** Paras 92, 95 (text), and Figure 5 caption
**Current text (Fig 5 caption):** "slope of -0.01 with an r-squared of 0.05 for NO3-N and -0.05 with an r-squared of 0.40 for Ca"
**Current text (para 92):** "NO3-N also shows a slight diluting trend at Plynlimon (best fit slope of -0.01) compared to the slight enriching trend at Hubbard Brook (best fit slope of 0.11)"

**Data shows (log-log regression on WY2008 UHF data, q in Lps):**

| Solute | Paper slope | Data slope | Paper R² | Data R² |
|--------|-------------|------------|----------|---------|
| Ca     | -0.05       | **-0.26**  | 0.40     | **0.82** |
| NO3-N  | -0.01       | **-0.11**  | 0.05     | 0.05    |

**Impact on narrative:** The paper states (para 92) that "both fits are less explanatory at Plynlimon than at Hubbard Brook." But the actual data shows Ca R² = 0.82 at Plynlimon vs 0.79 at HBEF — Plynlimon's Ca C:Q is actually *stronger*, not weaker. The Ca slope is also steeper (-0.26 vs -0.12).

These values are consistent across all subsetting approaches (raw 7-hourly data, daily means, independent vs joint filtering). The discrepancy is large enough that it's not a rounding or methodological issue — the paper's values appear to be from a different dataset or calculation.

**Proposed fix:** Update Figure 5 caption, paras 92 and 95 with the correct slopes and R² values. Revise the narrative in para 92 to reflect that Ca at Plynlimon has a *stronger* diluting relationship than at HBEF, while NO3 has a steeper diluting trend (-0.11 vs HBEF's +0.11 enriching trend). The "both fits are less explanatory" statement needs revision.

**Investigation results:** Checked all 3 Plynlimon sites (CR, LHF, UHF) across all water years (2007–2009). No combination produces the paper's values. The text was simply incorrect — not a data change. Conductivity C:Q (slope=-0.01, R²=0.01) vaguely resembles the old NO3 values, suggesting a possible column mix-up in a prior draft.

**Response:** APPLIED — text was incorrect, updated to match data. Rewrote "both fits are less explanatory" to "Ca fit is similarly strong at Plynlimon (R²=0.82) as at Hubbard Brook (R²=0.79), while NO3 fit remains weak at both sites."

---

## Issue 5: MacroSheds site/solute/site-year counts inconsistent

**Line(s) in doc:** Para 14 (Abstract), Para 146 (Methods), Para 188 (Conclusions)
**Current text:**
- Abstract: "16,489 site-years across 93 sites and 112 solutes"
- Methods (para 146): "210,058 site-years of data across 185 sites and 104 solutes"
- Conclusions (para 188): "16,489 site-years of load estimates across 93 sites and 112 solutes"

**Data shows (from current `data/load_annual.csv`):**
- Unique sites: **146** (not 185 or 93)
- Unique solutes: **100** (not 104 or 112)
- Unique (site, solute, water_year) combinations: **26,685** (not 210,058 or 16,489)
- Rows in file: 133,425 = 26,685 × 5 methods

**Note:** The numbers in the paper appear to come from a different version of the MacroSheds dataset (possibly edi.1262.1 vs the current edi.1262.2) or a different run of `calculate_annual_flux.R` with different filtering. The Abstract/Conclusions numbers (93/112/16,489) don't match the Methods numbers (185/104/210,058), and neither matches the current data.

**Proposed fix:** Re-run `source/calculate_annual_flux.R` on the current MacroSheds data and update all three locations with consistent, correct numbers. The Abstract and Conclusions should use the *filtered* counts; Methods should state both the pre- and post-filter counts.

**Response:** APPLIED — go with the data. Updated all three locations (Abstract, Methods, Conclusions) to 26,685 site-years, 146 sites, 100 solutes.

---

## Issue 6: Plynlimon incomplete days count discrepancy

**Line(s) in doc:** Para 86
**Current text:** "The Ca time series had 20 incomplete days, and the nitrate time series had 37 incomplete days."
**Data shows:** Ca = 28 incomplete days, NO3 = 40 incomplete days (defining "incomplete" as any day with fewer observations than the full complement)
**Note:** This could depend on the definition of "incomplete." At 7-hour sampling, there are ~3.4 observations per day. The paper may use a different threshold (e.g., days with <2 observations rather than <max observations). This is a minor issue.
**Proposed fix:** Verify the definition of "incomplete day" and update if needed. If the current values (28/40) are correct with the intended definition, update the text.

**Response:** APPLIED — 28/40 is correct. Updated text.

---

## Issue 7: Ca standard deviation inconsistency between text and caption

**Line(s) in doc:** Para 75 (text) vs Para 81 (Figure 3 caption)
**Current text:**
- Para 75: "standard deviation of 0.23 mg/L"
- Para 81 (Fig 3 caption): "standard deviation of 0.22 mg/L"
**Data shows:** sd = 0.228... → rounds to 0.23 at 2 decimal places
**Proposed fix:** Change caption value from 0.22 to 0.23 for consistency.

**Response:** APPLIED — trivial fix, already in docx.**


---

## Summary

| Issue | Severity | Type | Status |
|-------|----------|------|--------|
| 1. Q yield exponent | High | Clear typo | **APPLIED** — 10^10 → 10^8, 2.6→2.8 |
| 2. Ca-SpCond slope | High | Clear typo | **APPLIED** — 0.0063 → 0.063 |
| 3. Ca-SpCond R² | Medium | Data version | **APPLIED** — 0.92 → 0.98 |
| 4. Plynlimon C:Q | High | Wrong values | **APPLIED** — text was incorrect, updated slopes/R²/narrative |
| 5. MacroSheds counts | High | Data version | **APPLIED** — 26,685 site-years, 146 sites, 100 solutes |
| 6. Plynlimon missing days | Low | Definition | **APPLIED** — 20/37 → 28/40 |
| 7. Ca sd rounding | Low | Rounding | **APPLIED** — 0.22 → 0.23 |
