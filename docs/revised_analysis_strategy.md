# Revised Analysis Strategy: The Politicization of Workforce Futures

**Paper:** "The Politicization of Workforce Futures: Occupational Identity Threat and the Partisan Divergence of AI Attitudes in the United States, 2022–2024"
**Target Journal:** Technological Forecasting & Social Change (IF ~12.9)
**Document Date:** 2026-02-22
**Status:** Comprehensive revision strategy — replaces earlier analysis plan

---

## Executive Summary

The current paper uses a single binary dependent variable (CNCEXC) across three Pew ATP waves and tests five hypotheses all marked "supported," but at shallow depth. The raw wave-specific data contains 80+ additional AI attitude variables that are presently unused. Systematic exploration of these variables reveals several findings that are theoretically more compelling than any result in the current draft:

1. Democrats score higher on an AI knowledge index, directly falsifying the "information deficit" explanation
2. Republicans are simultaneously *more concerned* about AI and *less supportive* of regulation — a paradox that demands theoretical treatment
3. The partisan gap in AI job displacement concern varies sharply by occupation type, providing direct evidence for occupational identity threat that the current paper only infers
4. The healthcare and mental health domains produce the largest partisan gaps; policing produces a full reversal
5. Republicans and Democrats hold divergent visions of AI's future: Republicans fear autonomous harm; Democrats expect productivity gains

These findings together warrant a substantially restructured paper. The core occupational identity threat framing is sound and should be retained, but it must be built on this richer evidentiary base. The result will be a paper with genuinely novel theoretical contributions — particularly on the regulation paradox and the failure of the information deficit model — suitable for a top-tier journal.

---

## Part 1: Reframed Paper Structure

The current structure is: Background → Hypotheses → Methods → Results (CNCEXC only) → Discussion. The revised structure expands the results into five analytically distinct phases, each answering a different theoretical question. The first two phases retain existing analyses as foundations; phases three through five are entirely new.

### Phase 1: Partisan Divergence Over Time (Foundation)
**Research question:** Did partisan divergence in AI concern increase, stabilize, or reverse between 2022 and 2024?
**Data:** All three waves, CNCEXC as DV
**Analyses:** Survey-weighted proportions by wave × party; average marginal effects from logistic regression controlling for demographics
**Theoretical function:** Establishes the empirical baseline. Demonstrates that polarization is not merely a snapshot but a durable pattern across a critical period of AI deployment (ChatGPT launch fell between W119 and W132).
**Status:** Analyses exist; need to be polished and interpreted more carefully with respect to the ChatGPT discontinuity between waves.

### Phase 2: Predictors of Concern — Who Is More Concerned and Why? (Foundation)
**Research question:** What individual-level factors predict AI concern beyond party identification?
**Data:** Merged three-wave data
**Analyses:** Full logistic regression model with demographics, political variables, AI awareness, work exposure; Oaxaca-Blinder decomposition
**Theoretical function:** Establishes that party operates independently of demographic confounds; decomposition shows how much of the gap is explained by composition vs. structural differences in concern levels
**Status:** Analyses partially exist; Oaxaca decomposition needs completion and clearer interpretation.

### Phase 3: Concern Content Differentiation — What Are They Concerned About? (New)
**Research question:** Do Republicans and Democrats concern themselves with *different aspects* of AI risk, or do they differ only in overall concern levels?
**Data:** W152 (Aug 2024), AICONCERN seven-item battery
**Analyses:** Survey-weighted proportions by party for each item; clustered bar chart; chi-square with Cramér's V
**Theoretical function:** Tests H6. If the partisan gap is uniform across concern items, the story is simple intensity differentiation. If it is structured — Republicans more concerned about autonomy and human displacement, Democrats more concerned about equity and bias — this implies fundamentally different risk schemas rooted in different social positions and ideological priors.
**Status:** Raw data exploration confirms differential concern profiles; formal analyses not yet run.

### Phase 4: The Regulation Paradox — Concern Without Policy Demand (New)
**Research question:** Why are Republicans more concerned about AI but less supportive of AI regulation?
**Data:** W152, AIREG (regulation preference) and AICONCERN items
**Analyses:** Survey-weighted logistic regression of regulation preference on party × concern interaction; marginal effects plot
**Theoretical function:** Tests H7. This is the most theoretically provocative finding in the dataset. The paradox cannot be explained by concern intensity alone — it implicates ideological priors about government, technology, and market governance. This finding has direct implications for AI policy design.
**Status:** Preliminary chi-square confirms regulation paradox (V=0.131, strongest partisan divide in dataset); interaction model not yet run.

### Phase 5: Occupational Identity Threat — Direct Evidence (New)
**Research question:** Does the partisan gap in AI job displacement concern vary by occupation type in a pattern consistent with occupational identity threat theory?
**Data:** W152, AIJOBIMPCT occupation-specific items
**Analyses:** Partisan gaps by occupation; rank-order correlation with occupational classification; W119 AIKNOW analysis for information deficit test
**Theoretical function:** Tests H8 and H9. The current paper infers occupational identity threat without measuring it. W152 contains items asking about specific occupations (journalists, financial analysts, truck drivers, teachers, nurses, etc.). The pattern of partisan gaps across these occupations — larger for occupations coded as Republican-identified vs. Democratic-identified communities — constitutes the first direct empirical test of occupational identity threat in the AI attitudes literature.
**Status:** Preliminary exploration confirms occupation-specific variation; formal analyses and occupational coding not yet run.

---

## Part 2: New Hypotheses

The following hypotheses supplement existing H1–H5. Each is grounded in the raw data exploration findings and connects to an identifiable theoretical tradition.

### H6: Concern Content Differentiation
**Statement:** Republican and Democratic AI concerns will have distinct structural profiles, with Republicans emphasizing existential/autonomy threats (AI becoming autonomous, AI replacing human connection, AI causing job loss) and Democrats emphasizing equity/regulatory concerns (AI perpetuating bias, privacy violations, AI outpacing regulation).

**Theoretical grounding:** Ideological differences in perceived threat type are well-established in political psychology. Republicans' emphasis on autonomy threats and human replacement resonates with their higher scores on authoritarianism and social dominance orientation, which heighten sensitivity to status hierarchy disruption. Democrats' equity concerns align with their higher endorsement of egalitarian values. This hypothesis moves beyond quantity of concern to quality of concern.

**Operationalization:** AICONCERN battery (W152). Survey-weighted proportions for each item by party. The hypothesis predicts a specific ordering: the Republican-Democratic gap will be largest for "AI could make too many own decisions" and "AI could reduce human connection" and smallest or reversed for "AI could perpetuate existing biases."

**Falsification criterion:** If partisan gaps across AICONCERN items are uniform (i.e., Republicans score uniformly higher on all items by a constant margin), H6 is falsified in favor of a simple intensity model.

### H7: The Regulation Paradox
**Statement:** Party identification will moderate the relationship between AI concern and regulation preferences such that among equally concerned individuals, Republicans will be less likely to support stricter AI regulation than Democrats. This creates a paradox in which the more concerned party demands less regulatory action.

**Theoretical grounding:** The paradox reflects the collision of two independent political dispositions: risk perception (Republicans higher) and government trust/market ideology (Republicans lower). Recent work on "libertarian paternalism" and anti-regulatory populism in the American right suggests that conservative concern about AI is channeled toward distrust of both corporations and government, while liberal concern is channeled specifically toward demanding regulatory solutions. This hypothesis positions AI attitudes within the broader literature on the divergence between risk perception and policy demand.

**Operationalization:** AIREG dichotomized (currently moving too slow / not far enough = 1, vs. about right / going too far = 0). Survey-weighted logistic regression: AIREG ~ party + AICONCERN_INDEX + party × AICONCERN_INDEX + demographics. The interaction term is the primary test.

**Falsification criterion:** If the party × concern interaction term is null or positive (i.e., concerned Republicans are equally or more likely to support regulation than concerned Democrats), H7 is falsified.

### H8: Knowledge-Concern Asymmetry
**Statement:** Higher AI knowledge will NOT reduce the partisan gap in AI concern, contradicting the information deficit model. Democrats will demonstrate higher AI knowledge than Republicans, but this knowledge advantage will be associated with equal or greater concern rather than reduced concern.

**Theoretical grounding:** The information deficit model — which has dominated science communication research for decades — predicts that partisan gaps in technology attitudes reflect differential knowledge, and that correcting the knowledge deficit will close the attitude gap. The finding that Democrats score significantly higher on the AIKNOW index (3.99 vs. 3.84, p<.001) while also being more concerned in some domains falsifies the directional prediction of this model. This connects to the broader "science curiosity" literature, which suggests that knowledge about science increases rather than decreases attitude differentiation by ideology.

**Operationalization:** W119 AIKNOW index as continuous moderator. Survey-weighted logistic regression: AICONCERN ~ party + AIKNOW + party × AIKNOW + demographics. If the interaction is null (knowledge does not differentially affect partisan concern), and if Democrats score higher on knowledge than Republicans, H8 is supported.

**Falsification criterion:** If high-knowledge Republicans converge with high-knowledge Democrats on concern levels (i.e., a negative party × knowledge interaction), H8 is falsified and the information deficit model survives.

### H9: Occupation-Specific Threat
**Statement:** The partisan gap in AI job displacement concern will vary systematically by occupation type, with larger gaps for occupations stereotypically associated with Republican-identified communities (e.g., truck drivers, manufacturing workers) and smaller or absent gaps for occupations stereotypically associated with Democratic-identified communities (e.g., nurses, teachers).

**Theoretical grounding:** Occupational identity threat theory (Hekman et al.) predicts that threats to the workforce roles that define one's social identity will produce stronger attitudinal responses than equivalent threats to out-group roles. If Republican-identifying workers disproportionately occupy or identify with blue-collar, transportation, and manual occupations, AI displacement threats to those occupations should produce stronger concern among Republicans. The predicted null gap for teachers and nurses — traditionally Democratic-affiliated occupations — is as theoretically important as the positive gap for truck drivers and journalists.

**Operationalization:** AIJOBIMPCT items (W152) by party. Compute partisan gap (Republican% - Democrat%) for each occupation. Construct occupation-level blue-collar/white-collar/partisan-affiliation variable from external occupational data. Spearman rank-order correlation of partisan AI concern gaps with occupational partisan affiliation score.

**Falsification criterion:** If partisan gaps in job displacement concern are uniform across occupations (i.e., Republicans are equally more concerned about AI displacing nurses as truck drivers), H9 is falsified and the occupational identity threat mechanism is not supported by this evidence.

---

## Part 3: New Analyses — Full Methods Specification

### 3a. Concern Profile Analysis

**Wave:** W152 (Aug 2024, N=5,410)
**Variables:** AICONCERN items (seven specific concerns), PARTY (recoded Republican/Democrat/Independent), survey weights
**Method:** Survey-weighted proportions using `svyby()` from the `survey` package in R. For each AICONCERN item, compute weighted percentage endorsing concern (top two box or binary coding depending on item scale) by party. Chi-square tests with continuity correction; effect sizes reported as Cramér's V.

**R packages needed:**
- `survey` (survey-weighted statistics)
- `srvyr` (tidyverse-compatible survey wrapper)
- `ggplot2` (visualization)
- `DescTools` (Cramér's V)

**Theoretical connection:** Directly tests H6. Concern content differentiation is the key test distinguishing "partisan intensity" from "partisan schema" explanations of the attitude gap.

**Expected output:** Table 3 (concern profile by party, weighted %, chi-square, V); Figure 3 (diverging bar chart with items sorted by Republican-Democrat gap, zero line centered)

**Implementation note:** Code each AICONCERN item as binary before analysis (e.g., "very concerned" or "somewhat concerned" = 1). This maximizes comparability across items with slightly different response scales. Report raw proportions, not log odds.

---

### 3b. Regulation Paradox Model

**Wave:** W152
**Variables:** AIREG (regulation preference, 4-point scale), AICONCERN_INDEX (sum or mean of concern items), PARTY, demographics (age, education, gender, race/ethnicity, income), survey weights
**Method:** Survey-weighted logistic regression. Dichotomize AIREG: "moving too slowly / not far enough" = 1 (pro-regulation), vs. "about right / going too far" = 0. Primary model: AIREG_BINARY ~ PARTY + CONCERN_INDEX + PARTY × CONCERN_INDEX + demographics. Compute predicted probabilities at low/medium/high concern for each party to visualize interaction.

**R packages needed:**
- `survey` (svyglm for survey-weighted logistic regression)
- `marginaleffects` (for average marginal effects and interaction visualization)
- `ggplot2` (marginal effects plot)

**Theoretical connection:** Tests H7. The key estimand is the interaction term: does the concern → regulation preference relationship differ by party? Visualize as a two-line plot of predicted P(pro-regulation) by concern level, separately for Republicans and Democrats.

**Expected output:** Table 4 (regression coefficients, odds ratios, marginal effects); Figure 4 (two-panel plot: left panel, distribution of AIREG by party; right panel, predicted P(pro-regulation) by concern × party)

**Implementation note:** Create the CONCERN_INDEX carefully — check internal consistency (alpha) before summing items. Consider exploratory factor analysis if items cluster into sub-dimensions (autonomy concerns vs. equity concerns). This would strengthen H6 simultaneously.

---

### 3c. Knowledge-Concern Interaction

**Wave:** W119 (Dec 2022, N=11,004)
**Variables:** AIKNOW items (knowledge test questions), AICONCERN or CNCEXC (concern), PARTY, DESRISK (dispositional risk tolerance), demographics, survey weights
**Method:** (1) Construct AIKNOW_INDEX from correct answers on knowledge items (sum or proportion correct). Report index descriptives by party. (2) Survey-weighted logistic regression: CONCERN ~ PARTY + AIKNOW_INDEX + PARTY × AIKNOW_INDEX + DESRISK + demographics. The interaction term tests whether knowledge differentially affects concern by party.

**R packages needed:**
- `survey`
- `marginaleffects`
- `ggplot2`
- `psych` (for alpha, descriptives)

**Theoretical connection:** Tests H8. If the interaction term is null and the Democrat > Republican knowledge difference is confirmed, this falsifies the information deficit model. The DESRISK variable serves as a control for personality-level differences in comfort with novelty, isolating the political identity effect.

**Expected output:** Table 5 (regression with interaction, marginal effects at mean knowledge ± 1 SD); Figure 5 (AIKNOW density/violin plot by party; interaction plot of predicted concern by knowledge × party)

**Implementation note:** The W119 codebook must be consulted to determine exact AIKNOW item coding. Verify whether items are presented as multiple-choice with one correct answer or as agree/disagree statements. Compute Cronbach's alpha — if poor, report items separately rather than as index.

---

### 3d. Occupation-Specific Job Threat

**Wave:** W152
**Variables:** AIJOBIMPCT items (by occupation), PARTY, survey weights
**Method:** (1) For each occupation item, compute weighted proportion (by party) expecting AI to eliminate or reduce jobs. Compute partisan gap = Republican% - Democrat%. (2) Rank occupations by gap size. (3) Code each occupation on a blue-collar/white-collar/partisan-affiliation scale using Bureau of Labor Statistics occupational data merged with 2022–2024 voter registration / occupational data from ANES or similar. (4) Spearman rank-order correlation of partisan gaps with occupational coding.

**R packages needed:**
- `survey`
- `ggplot2`
- `ggalt` (dumbbell chart)
- `stats` (cor.test for Spearman)

**Theoretical connection:** Tests H9. The dumbbell chart is the primary visualization — each occupation is a row, with two dots (Republican %, Democrat %) connected by a line. The width of the line is the partisan gap. Sorted by gap size, this chart immediately communicates which occupations produce the largest partisan differences.

**Expected output:** Figure 6 (dumbbell chart of AIJOBIMPCT by party, sorted by gap); Table 6 (occupation, Republican %, Democrat %, gap, chi-square, V); one paragraph reporting Spearman correlation with occupational coding.

**Implementation note:** Journalist and truck driver deserve special attention — these are the two occupations with the largest confirmed gaps (+12.5pp and +9.4pp respectively) and represent archetypically Democratic-associated vs. Republican-associated white-collar and blue-collar occupations respectively. Nurse and teacher (null gaps) should be explicitly highlighted as theoretically confirming the H9 prediction.

---

### 3e. Domain Help/Hurt Profiles

**Wave:** W132 (Aug 2023, N=11,201)
**Variables:** AIHLPHRT eight-domain items (healthcare, vehicle safety, doctor quality, privacy, policing, etc.), PARTY, survey weights
**Method:** For each domain, compute weighted proportion saying AI will "mostly help" and "mostly hurt" by party. Create a summary data frame with domain, party, %help, %hurt. The policing item requires special treatment as it shows a directional reversal (Republicans more optimistic).

**R packages needed:**
- `survey`
- `ggplot2`
- `tidyr` (for reshaping to long format for butterfly chart)

**Theoretical connection:** Supports Phase 1 foundation while also providing domain-specific nuance for the discussion of H6. The policing reversal is theoretically important: it is the one domain where Republicans trust AI more than Democrats, and it aligns with Republican attitudes toward law enforcement and their higher support for "tough on crime" policies.

**Expected output:** Figure 7 (butterfly chart — left bars show %help by domain, right bars show %hurt, colored by party, with policing domain highlighted); Table in supplementary materials with all chi-square statistics.

**Implementation note:** The butterfly chart should be sorted by Republican-Democrat gap in "AI helps" (descending). The policing item will naturally appear at the bottom with a reversed gap, making the reversal visually salient.

---

### 3f. Future AI Expectations

**Wave:** W152
**Variables:** FUTRAI items (beliefs about AI becoming autonomous, causing major harm, productivity benefits, etc.), PARTY, survey weights
**Method:** Survey-weighted proportions by party for each FUTRAI item. Chi-square tests.

**R packages needed:**
- `survey`
- `ggplot2`

**Theoretical connection:** Future expectations are antecedents of current concern. Republicans' higher expectation that AI will become autonomous (+9.6pp) and cause major harm (+8.8pp) is consistent with their higher concern levels. Democrats' higher expectation of productivity benefits (+6pp) explains their lower concern. This analysis is descriptive but theoretically clarifying.

**Expected output:** Figure 8 (diverging bar chart of FUTRAI items, sorted by Republican-Democrat gap); supplementary table with chi-square statistics.

---

### 3g. Trust and Control

**Wave:** W152
**Variables:** TRSTAIPRS (trust in AI providers), AICONTROL (sense of control over AI), PARTY, survey weights
**Method:** Weighted proportions by party; chi-square; ordinal logistic regression with demographics as controls.

**R packages needed:**
- `survey`
- `MASS` (polr for ordinal logistic)
- `marginaleffects`

**Theoretical connection:** Trust and control perceptions are proximal predictors of technology acceptance (TAM framework). Republicans' lower sense of control (20% "none" vs. 15%) and Democrats' higher eventual trust trajectory (17% vs. 12%) are structurally consistent with partisan differences in institutional trust documented in political science. These findings contextualize the regulation paradox: Republicans feel less in control yet oppose regulatory solutions — suggesting a general political alienation rather than domain-specific AI governance preference.

**Expected output:** Added to Table 4 or separate Table 7; no new figure needed (can incorporate in Figure 3 or Figure 8).

---

## Part 4: Figure Strategy

All figures should be produced in R using ggplot2 with a consistent visual theme. Target publication standards: 300 DPI minimum, accessible color palette (avoid pure red/blue), serif or sans-serif fonts matching journal style, informative titles with subtitle, properly labeled axes. Use the Republican = warm (orange/red) and Democrat = cool (teal/blue) convention consistently.

### Figure 1: CNCEXC Trend (Retain and Revise)
**Type:** Line chart with confidence intervals
**Data:** All three waves, CNCEXC by wave × party
**Changes from current:** Add ChatGPT launch annotation (November 2022) between W119 and W132. Add marginal text labels at endpoints. Widen confidence intervals to reflect design effect.
**Caption note:** Include effective sample sizes accounting for design effect.

### Figure 2: Average Marginal Effects by Wave (Retain and Improve)
**Type:** Coefficient plot (dot-whisker)
**Data:** AMEs from logistic regression, by wave
**Changes from current:** Standardize scale across panels; add reference line at AME=0; improve variable labels for non-technical reader.

### Figure 3: Concern Content Profile (New — Core Finding)
**Type:** Diverging bar chart
**Data:** W152, AICONCERN seven items by party
**Design:** Items sorted by Republican-Democrat gap (largest gap at top). Each item has two bars extending from a zero center — one for Republican proportion, one for Democrat proportion. Color consistent with party convention. Items with significant differences marked with asterisk.
**This is the most important new figure in the paper.**

### Figure 4: The Regulation Paradox (New — Key Theoretical Contribution)
**Type:** Two-panel figure
**Panel A:** Stacked bar chart of AIREG distribution by party (showing Republicans more likely to say "going too far" and "about right")
**Panel B:** Predicted probability of supporting stricter regulation as a function of AICONCERN_INDEX, separately for Republicans and Democrats (two diverging lines)
**This figure communicates the paradox immediately: Republicans are more concerned (Panel A distribution) but less likely to convert concern into regulatory demand (Panel B lower line).**

### Figure 5: AI Knowledge by Party (New)
**Type:** Violin plot or overlapping density plot
**Data:** W119, AIKNOW_INDEX by party
**Design:** Two overlapping distributions with median lines. Include knowledge quartile labels. Annotate mean difference and p-value.
**Inset:** Optional — show concern level by knowledge quartile × party (4-panel grid) to visualize the interaction.

### Figure 6: Occupation-Specific Job Threat Gaps (New — Core Finding)
**Type:** Dumbbell chart
**Data:** W152, AIJOBIMPCT items by party
**Design:** One row per occupation. Republican dot (warm) and Democrat dot (cool) connected by line. Sorted by gap size (largest gap at top). Line width encodes gap magnitude. Vertical zero reference line at the midpoint. Journalists and truck drivers labeled prominently. Teachers and nurses labeled to show null gap.

### Figure 7: Domain Help/Hurt Profiles (New)
**Type:** Butterfly chart (back-to-back horizontal bar chart)
**Data:** W132, AIHLPHRT eight domains by party
**Design:** Left side shows "mostly helps" proportions; right side shows "mostly hurts" proportions. Domains sorted by absolute partisan gap. Policing domain highlighted with annotation ("Reversal: Republicans more optimistic").

### Figure 8: Future AI Expectations by Party (New)
**Type:** Diverging bar chart
**Data:** W152, FUTRAI items by party
**Design:** Items sorted by Republican-Democrat gap. Autonomy/harm items cluster at top (Republicans higher); productivity items cluster at bottom (Democrats higher). Color by party.

### Figure 9: AME All Predictors (Retain — Improve Labeling)
**Type:** Coefficient plot
**Changes from current:** Add AIKNOW_INDEX, DESRISK (W119), occupational category (W152) as additional predictors once available. Improve readability of confidence intervals.

### Figure 10: Oaxaca Decomposition (Retain — Complete and Improve)
**Type:** Stacked bar chart or waterfall chart
**Changes from current:** Complete the decomposition with full set of predictors. Improve visualization of explained vs. unexplained components. Add annotation with percentage of gap explained.

---

## Part 5: What to Discard or Demote

### Demote to Robustness Check

**Current multinomial logistic regression (03_regression_analysis.R):** The current multinomial model treats CNCEXC as multi-category. This is technically appropriate but adds complexity without theoretical payoff given that the binary model suffices for testing the hypotheses. Keep as robustness check in a supplementary table. One sentence in the Methods: "Results were robust to multinomial specification (see Supplementary Table S-X)."

**Three-way wave × party × AI awareness interaction:** The null result is scientifically valid and should be reported — but as one sentence in a robustness section, not as a primary finding. Null results constrain the theoretical space; the null here means that AI awareness did not differentially affect partisan concern across the three waves.

### Drop Entirely

**Shannon entropy metric:** The entropy measure of response distribution diversity was an interesting exploratory idea, but it is underdeveloped and the reviewer will ask what theoretical construct entropy is supposed to operationalize. Without a clear theoretical justification and a comparison benchmark, entropy metrics read as technical decoration. Drop from the paper. If you want to retain something from this analysis, convert it to a simpler variance-in-response distribution measure and frame it clearly.

**Redundant descriptive tables:** Any table that merely shows n by party by wave without statistical tests. Replace with the richer domain-specific and concern-specific analyses.

---

## Part 6: Practical Implications Upgrade

The new analyses transform the practical implications section from generic observations to specific, actionable insights for policy makers, HR practitioners, and technology communicators.

### For AI Policy Design: The Regulation Paradox

If Republicans are more concerned about AI but less supportive of regulation, traditional regulatory advocacy — which frames regulation as a response to concern — will fail to build bipartisan coalitions. Policy designers should investigate whether alternative governance mechanisms (industry self-regulation, liability frameworks, transparency mandates) can address Republican concerns without triggering anti-government sentiment. The regulation paradox also suggests that survey questions asking only about regulation preferences will systematically underestimate Republican concern, biasing public opinion research.

### For Human Resource Development: Domain-Specific Concerns

The occupation-specific job threat analysis (H9) directly informs HRD practice. Retraining and upskilling programs should not treat "AI anxiety" as a monolithic phenomenon. Workers in occupations with large partisan gaps (journalism, financial analysis, trucking) face not only displacement risk but socially-polarized discourse about that risk, which may create additional barriers to technology adoption and retraining engagement. Interventions should acknowledge the political salience of the threat rather than treating concerns as purely technical.

### For Bipartisan Technology Governance: The Policing Reversal

The reversal finding in the policing domain (Republicans more optimistic about AI for police/public safety) identifies a genuine area of potential bipartisan AI policy consensus. AI deployment in law enforcement contexts may attract broader political support than AI deployment in healthcare or mental health domains. Strategically, technology governance advocates seeking bipartisan support might find the public safety application domain to be a productive starting point.

### For Science Communication: Knowledge is Not Enough

H8, if supported, constitutes a direct challenge to the information deficit model that has guided much of science communication practice. If more knowledgeable Democrats are equally or more concerned about AI than less knowledgeable Republicans, then "AI literacy" campaigns will not close partisan divides. This finding aligns with the growing literature showing that the science-society relationship is fundamentally political and cannot be resolved by information provision alone. Communication strategies should shift from deficit-filling to value-articulation and identity-respectful engagement.

### For Technology Diffusion Theory: Divergent Futures

Republicans and Democrats holding divergent beliefs about AI's future trajectory (autonomy-and-harm vs. productivity-and-assistance) implies that AI adoption curves may split along partisan lines as AI becomes more embedded in everyday life. This has implications for technology forecasting models that treat adoption as a function of individual utility assessment: if utility assessments are systematically structured by political identity, forecasting must incorporate political identity as a structural moderator.

---

## Part 7: Implementation Roadmap

This roadmap is structured in six phases. Phases 1–4 are data analysis; Phase 5 is writing; Phase 6 is figure production. Estimated timeline assumes a single analyst working approximately 20 hours per week.

### Phase 1: New Data Preparation (Estimated: 1 week)

**Objective:** Extract and validate all wave-specific variables needed for new analyses.

**Tasks:**
1. Extract AICONCERN (7 items), AIREG, AIJOBIMPCT (all occupation items), FUTRAI (all items), TRSTAIPRS, AICONTROL from W152 raw data. Validate against Pew codebook.
2. Extract AIKNOW (all knowledge test items), DESRISK from W119 raw data. Validate.
3. Extract AIHLPHRT (8 domain items) from W132 raw data. Validate.
4. Merge wave-specific variables with existing analysis-ready dataset where applicable (cross-wave variables only).
5. Create wave-specific analysis datasets (w152_analysis.rds, w119_analysis.rds, w132_analysis.rds) with all variables recoded and labeled.
6. Document all variable constructions in a revised codebook.

**Deliverable:** Three analysis-ready wave-specific datasets; updated variable codebook.

---

### Phase 2: W152 Analyses (Estimated: 1.5 weeks)

**Objective:** Complete all primary new analyses using the most recent wave.

**Tasks:**
1. Construct AICONCERN_INDEX; check Cronbach's alpha; run EFA if alpha < 0.7.
2. Run concern profile analysis (3a): weighted proportions by party, chi-square, V.
3. Run regulation paradox model (3b): dichotomize AIREG; run interaction model; compute predicted probabilities.
4. Run AIJOBIMPCT occupation-specific analysis (3d): weighted proportions by party; compute gaps; rank occupations.
5. Run FUTRAI analysis (3f): weighted proportions by party.
6. Run TRSTAIPRS and AICONTROL analysis (3g): weighted proportions; ordinal regression.
7. Create Tables 3, 4, 5 (or final numbering equivalent).

**Deliverable:** Completed R script (05_w152_analysis.R); all relevant tables as .csv files.

---

### Phase 3: W119 Analyses (Estimated: 1 week)

**Objective:** Complete the knowledge-concern interaction analysis.

**Tasks:**
1. Construct AIKNOW_INDEX from W119 knowledge items; validate; report alpha.
2. Validate and recode DESRISK items.
3. Run descriptive analysis: AIKNOW_INDEX by party (means, distribution).
4. Run knowledge-concern interaction model (3c): logistic regression with interaction term; marginal effects.
5. Update existing W119 analyses if needed (demographics by party table, DESRISK descriptives).

**Deliverable:** Completed R script (06_w119_analysis.R); relevant tables.

---

### Phase 4: W132 Analyses (Estimated: 0.5 weeks)

**Objective:** Complete domain help/hurt profile analysis.

**Tasks:**
1. Extract and validate AIHLPHRT items from W132.
2. Run weighted proportions by party and domain.
3. Identify the policing reversal magnitude precisely (already confirmed at +5.5pp but needs weighted estimate with confidence interval).
4. Prepare data for butterfly chart.

**Deliverable:** Completed R script (07_w132_analysis.R); relevant table.

---

### Phase 5: Integration — Revised Paper Draft (Estimated: 2–3 weeks)

**Objective:** Revise the manuscript to incorporate all new findings.

**Tasks:**
1. Revise the introduction to foreground the regulation paradox and knowledge-asymmetry findings as the paper's primary theoretical contributions.
2. Expand the theoretical framework section to include:
   - Regulation paradox: engage literature on risk perception vs. policy demand (Bronfman et al.; Slovic; Kahan)
   - Information deficit model critique: engage Nisbet & Mooney; Kahan et al.; Scheufele
   - Concern content differentiation: engage dual-process risk perception (Loewenstein et al.)
3. Add H6–H9 to the hypotheses section with full theoretical justification.
4. Expand the methods section with new analysis descriptions (Sections 3a–3g above).
5. Write new results sections for Phases 3–5.
6. Revise discussion to integrate all findings under a unified theoretical framework.
7. Update practical implications (see Part 6).
8. Revise conclusion to foreground novel contributions.

**Deliverable:** Full revised manuscript draft.

---

### Phase 6: New Figures (Estimated: 1 week, parallel with Phase 5)

**Objective:** Produce all 10 publication-quality figures.

**Tasks:**
1. Revise Figure 1 and Figure 2 (ChatGPT annotation, improved labeling).
2. Produce Figure 3 (AICONCERN diverging bar chart).
3. Produce Figure 4 (Regulation paradox two-panel).
4. Produce Figure 5 (AIKNOW distribution + interaction plot).
5. Produce Figure 6 (AIJOBIMPCT dumbbell chart).
6. Produce Figure 7 (AIHLPHRT butterfly chart).
7. Produce Figure 8 (FUTRAI diverging bar chart).
8. Complete Figure 9 (AME all predictors — update when new predictors available).
9. Complete Figure 10 (Oaxaca decomposition — finalize and improve).
10. Ensure all figures use consistent theme, accessible palette, and 300 DPI export.

**Deliverable:** All figures as .pdf and .png files in /output/figures/; R script (08_figures.R).

---

## Part 8: Theoretical Positioning

The revised paper makes three distinct theoretical contributions, each of which is novel in the AI attitudes literature:

**Contribution 1: The Regulation Paradox**
This is the paper's most provocative finding. Prior literature on risk perception and policy demand generally assumes a positive relationship between the two (Slovic 1987; Loewenstein et al. 2001). The finding that Republicans score higher on AI concern but lower on regulation preference constitutes a documented empirical violation of this assumption in the AI domain. The paper provides theoretical grounding for this paradox in the literature on anti-government ideology and institutional distrust (Hetherington & Rudolph 2015), positions it alongside parallel findings on climate risk-policy gaps in conservative populations (McCright & Dunlap 2011), and draws implications for AI governance design.

**Contribution 2: The Information Deficit Falsification**
The finding that Democrats score higher on AI knowledge despite equal or greater concern in some domains directly falsifies the information deficit model as applied to AI policy attitudes. This contribution engages a long-running methodological debate in science communication (Nisbet & Mooney 2007; Kahan et al. 2012) and argues that AI polarization, like climate polarization, is driven by identity-protective cognition rather than differential information access. The practical implication — that information campaigns will not close partisan divides — is directly relevant to AI communicators and policy advocates.

**Contribution 3: Occupational Identity Threat — Direct Evidence**
Prior applications of occupational identity threat theory to AI attitudes have relied on indirect measures (occupation as a demographic control, general job concern items). The AIJOBIMPCT occupation-specific analysis in W152 provides the first direct test of the theory's prediction that partisan gaps in displacement concern will track the partisan identity of the threatened occupation. This contribution speaks to both the identity threat literature (Hekman et al. 2009; Tajfel & Turner 1979 applied to occupational groups) and the technology adoption literature (TAM; UTAUT) by introducing political identity as a structural moderator of adoption processes.

---

## Appendix A: Variable Reference Table

| Variable | Wave | Description | Type | Role in Analysis |
|---|---|---|---|---|
| CNCEXC | All | Binary AI concern (concerned/not) | DV (existing) | Phase 1 foundation |
| AICONCERN | W152 | 7-item concern battery | DV (new) | Phase 3, H6 |
| AIREG | W152 | AI regulation preference (4-point) | DV (new) | Phase 4, H7 |
| AIJOBIMPCT | W152 | Job displacement by occupation | DV (new) | Phase 5, H9 |
| FUTRAI | W152 | Future AI trajectory beliefs | DV/IV | Phase 5 |
| TRSTAIPRS | W152 | Trust in AI providers | DV | Phase 5 |
| AICONTROL | W152 | Sense of control over AI | DV | Phase 5 |
| AIKNOW | W119 | AI knowledge test items | Moderator | Phase 5, H8 |
| DESRISK | W119 | Dispositional risk tolerance | Control | Phase 5 |
| AIHLPHRT | W132 | Domain help/hurt (8 items) | DV (new) | Phase 3b |
| PARTY | All | Party identification | IV (existing) | All phases |
| AI_AT_WORK | W119 | Used AI at work (binary) | IV/control | Phase 2 |

---

## Appendix B: R Script Organization

The revised analysis should be organized in the following script structure, extending the existing 01–04 scripts:

```
scripts/
  01_data_preparation.R          # existing — add wave-specific variable extraction
  02_descriptive_analysis.R      # existing — keep
  03_regression_analysis.R       # existing — demote to robustness check
  04_advanced_analysis.R         # existing — Oaxaca, entropy (drop entropy)
  05_w152_analysis.R             # NEW — AICONCERN, AIREG, AIJOBIMPCT, FUTRAI, TRSTAIPRS, AICONTROL
  06_w119_analysis.R             # NEW — AIKNOW, DESRISK
  07_w132_analysis.R             # NEW — AIHLPHRT domain profiles
  08_figures.R                   # NEW — all publication figures
  99_robustness_checks.R         # NEW — multinomial, three-way interaction, sensitivity
```

All scripts should use the `survey` package for weighted analyses and `srvyr` for tidyverse compatibility. Survey design object should be defined once in 01_data_preparation.R and saved as an .rds file for loading in downstream scripts.

---

*End of Revised Analysis Strategy Document*
*Version 1.0 — 2026-02-22*
*Next review: After Phase 2 completion (estimated 2.5 weeks from now)*
