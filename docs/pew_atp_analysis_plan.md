# Pew ATP Analysis Plan

## Working Title

**"Diverging Responses to AI: A Repeated Cross-Sectional Analysis of Attitude Polarization in the United States, 2022–2024"**

---

## 1. Overview and Motivation

This document outlines a structured analytical strategy for examining AI attitude polarization in the United States using three waves of Pew Research Center's American Trends Panel (ATP). The study capitalizes on a natural experiment: Pew fielded attitude surveys on artificial intelligence immediately before (Wave 119, December 2022) and twice after (Wave 132, July–August 2023; Wave 152, August 2024) the mass-market launch of ChatGPT (November 30, 2022). This timing allows for a robust before/after design that tracks shifts in AI attitude distributions across a pivotal 20-month window.

The central construct of interest is **attitude polarization**: whether the American public is not merely shifting its mean evaluation of AI (becoming more or less positive), but actively bifurcating into opposing camps with distinct demographic and ideological profiles. Understanding this dynamic carries practical significance for technology governance, organizational adoption decisions, and public communication strategy.

---

## 2. Research Questions

**RQ1: Distributional polarization over time**
Has the distribution of AI attitudes become more bimodal (polarized) between 2022 and 2024, and if so, at what rate?

**RQ2: Demographic divergence**
Which demographic groups (age cohort, education level, gender, race/ethnicity, party identification, income) show the greatest attitude divergence across the three waves, and do between-group gaps widen or narrow over time?

**RQ3: Political identity as a predictor**
Does political identity predict AI attitude polarization beyond the variance explained by sociodemographic characteristics, and does its predictive weight increase over time in a manner consistent with ideological sorting?

---

## 3. Theoretical Framework

### 3.1 Social Amplification of Risk Framework (SARF)

The Social Amplification of Risk Framework (Kasperson et al., 1988; Pidgeon et al., 2003) posits that the perceived risk of a technology is not simply a function of its objective hazard profile but is amplified or attenuated through social processes, institutional channels, and individual interpretive schemas. For AI, mass-market generative models function as an "amplification station": widespread personal exposure to AI outputs provides concrete experiential anchors around which abstract risk perceptions crystallize. SARF predicts that greater information volume—the media coverage and interpersonal discussion triggered by ChatGPT's launch—amplifies latent attitudinal tendencies, potentially magnifying between-group differences.

This framework is particularly suited to the 2022–2024 period because the informational environment surrounding AI changed dramatically and rapidly. SARF directs our attention to variation in exposure: individuals and groups that were already attuned to AI discourse (high-awareness, high-education) may have responded differently to amplification than those encountering AI for the first time through news coverage.

### 3.2 Knowledge Gap Hypothesis

The Knowledge Gap Hypothesis (Tichenor et al., 1973) proposes that as information about a topic diffuses through society, higher-SES groups assimilate it faster, causing knowledge differentials to widen rather than close. Critically, the hypothesis has been extended beyond factual knowledge to attitude formation: those with greater cognitive resources and information access do not simply "know more" but develop more differentiated, nuanced evaluative frameworks (Bonfadelli, 2002). Applied to AI attitudes, we expect:

1. Higher-education respondents develop a more complex, benefit-and-risk-differentiated stance (neither purely concerned nor purely excited)
2. Lower-education respondents, receiving amplified risk frames with less cognitive scaffolding to contextualize them, consolidate toward the "more concerned" pole
3. This differential consolidation widens the education gap across waves

### 3.3 Adaptation for Repeated Cross-Sectional Design

The original proposal anticipated a Latent Curve Growth Analysis (LCGA) framework requiring a longer panel with four or more time points and reliable individual-level tracking. The available data structure—three repeated cross-sections with only partial panel overlap—calls for an adapted strategy:

- **Primary design**: Repeated cross-section, treating each wave as an independent nationally representative sample
- **Polarization measurement**: Distributional statistics (variance, bimodality coefficient, Esteban-Ray index) applied to attitude proportions across waves
- **Latent structure**: Latent Class Analysis (LCA) conducted separately at each wave, with cross-temporal comparison of class prevalence and class profiles
- **Conditional if applicable**: If respondent IDs overlap sufficiently across waves, Latent Transition Analysis (LTA) will be used to model individual-level attitude change trajectories

This design is standard in public opinion research and aligns with Pew's own analytic conventions for ATP data. While it cannot establish within-person change with certainty (absent confirmed panel overlap), it provides strong population-level evidence for distributional polarization.

---

## 4. Data Source

### 4.1 Pew Research Center American Trends Panel (ATP)

The ATP is an address-recruited, probability-based online panel of U.S. adults. Panel members are recruited via random-sample national address-based sampling (ABS), with oversamples of underrepresented groups. Data are collected via self-administered web surveys; offline respondents may participate by phone or mail. The panel design allows for longitudinal tracking when respondents participate in multiple waves, though not all respondents complete every wave.

All ATP data used in this study are weighted to be representative of the U.S. adult population. Pew provides PEWT (panel weights) and cross-sectional weights in SPSS (.sav) and Stata (.dta) format.

### 4.2 Waves Included

| Wave | Field Dates | N | Primary Topics | AI Context |
|------|-------------|---|----------------|------------|
| Wave 119 | December 12–18, 2022 | 11,004 | AI and human enhancement | Fielded 12–18 days after ChatGPT launch (Nov 30, 2022); baseline pre-adoption |
| Wave 132 | July 31 – August 6, 2023 | 11,201 | AI attitudes (dedicated module) | ~8 months post-ChatGPT; early diffusion phase |
| Wave 152 | August 12–18, 2024 | 5,410 | AI and human enhancement | ~20 months post-ChatGPT; mature diffusion phase |

**Note on sample size reduction in Wave 152**: The smaller N reflects a sub-panel design or module routing. This will be accounted for in weighted analyses; standard errors and confidence intervals will be reported throughout.

### 4.3 Data Access

Data are publicly available through the Pew Research Center Data Archive:
- **URL**: https://www.pewresearch.org/download-datasets/
- **Access**: Free registration required (institutional or personal email)
- **Formats**: SPSS (.sav), Stata (.dta)
- **License**: Academic/non-commercial research use

Download files will be stored locally under `/data/raw/` and will not be redistributed.

---

## 5. Measures

### 5.1 Dependent Variable: AI Concern-Excitement

**Survey item** (exact wording across all three waves):
> "Overall, would you say you feel more concerned than excited about the increased use of artificial intelligence?"

**Response categories**:
1. More concerned than excited
2. More excited than concerned
3. An equal mix of concerned and excited
4. Not sure

**Coding decisions**:
- For polarization metrics: treat "concerned" and "excited" as the two opposing poles; "equal mix" as the centrist position; "not sure" as non-attitude/excluded or analyzed separately
- For LCA: include all four categories as observed indicators
- For multinomial logistic regression: use four-category nominal outcome with "equal mix" as the reference category (substantively interpretable as the moderate/centrist position)

### 5.2 Independent Variables

| Variable | Categories | Notes |
|----------|-----------|-------|
| Age cohort | 18–29, 30–49, 50–64, 65+ | Categorical; coding to match published Pew reports |
| Gender | Man, Woman | Binary coding per ATP standard; other/prefer not may be small cell |
| Race/ethnicity | White (non-Hispanic), Black (non-Hispanic), Hispanic, Asian (non-Hispanic), Other | ATP standard coding |
| Education | HS or less, Some college, BA or more | Three-category standard; four-category (HS/less-than-college/BA/postgrad) as sensitivity check |
| Income | Less than $30k, $30k–$74,999, $75k–$99,999, $100k+ | ATP-standard income bands |
| Political party ID | Republican/lean Rep, Independent (no lean), Democrat/lean Dem | Collapsed leaners per convention |
| Political ideology | Conservative, Moderate, Liberal | Separate from party ID; test both |
| Region | Northeast, Midwest, South, West | Census region |

### 5.3 Moderator: AI Awareness

**Survey item** (wording may vary slightly by wave):
> "How much, if anything, have you heard about artificial intelligence, or AI?"

**Response categories**: A lot / A little / Nothing at all

**Role in analysis**: AI awareness is treated as a moderator hypothesized to amplify polarization. Respondents who report hearing "a lot" about AI should show stronger attitudinal crystallization. This will be tested via awareness × wave interaction terms in regression models.

### 5.4 Panel ID Variable

- **QKEY** (or equivalent respondent identifier in ATP datasets): Enables matching across waves
- Cross-wave panel overlap will be assessed post-download; if ≥ 1,500 respondents appear in all three waves, Latent Transition Analysis (LTA) will supplement the cross-sectional analyses
- Panel attrition will be examined for systematic bias (differential dropout by demographics)

---

## 6. Hypotheses

### H1: Distributional Polarization (SARF-derived)
The distribution of AI concern-excitement will become more bimodal (higher bimodality coefficient, higher Esteban-Ray polarization index) from Wave 119 (2022) to Wave 152 (2024), as social amplification consolidates latent evaluative tendencies into opposing poles.

*Operationalization*: Bimodality coefficient BC > 0.555 threshold (Pfister et al., 2013); Esteban-Ray index calculated on the categorical distribution; chi-square tests of proportional shifts across waves.

### H2a: Widening Education Gap (Knowledge Gap-derived)
The gap between higher- and lower-education respondents in AI attitude distribution will widen from 2022 to 2024. Specifically:
- Higher-education respondents will increasingly occupy the "equal mix" (nuanced/mixed) position
- Lower-education respondents will increasingly occupy the "more concerned" position
- The education × wave interaction will be significant in multinomial regression

### H2b: Widening Age Gap (SARF-derived)
The age gap in AI attitudes will widen from 2022 to 2024:
- Younger adults (18–29) will increasingly occupy the "more excited" position
- Older adults (65+) will increasingly occupy the "more concerned" position
- Published Pew figures provide directional anchors: by 2023, 61% of 65+ respondents reported concern vs. 42% of 18–29 respondents

### H3: Partisan Sorting
Political party identification will become a stronger predictor of AI attitudes across waves, consistent with patterns of ideological sorting observed in other technology and science attitude domains (e.g., climate change, vaccines). The party ID × wave interaction will be significant and will account for variance beyond demographic controls.

### H4: Awareness as Amplifier
AI awareness will moderate the relationship between demographics and attitudes: respondents who report hearing "a lot" about AI will show stronger demographic-attitude associations. The widening of demographic gaps (H2a, H2b, H3) will be most pronounced among the high-awareness subsample.

---

## 7. Analytic Strategy

### Step 1: Data Preparation and Descriptive Analysis

**1a. Variable construction**
- Recode all IVs to common categories across three waves (codebook cross-reference required)
- Construct wave indicator variable
- Apply appropriate survey weights (cross-sectional weights for each wave independently; note: do not pool waves with a single weight without constructing a combined weight)

**1b. Descriptive statistics**
- Frequency distributions of AI concern-excitement by wave (Table 1)
- Cross-tabulations: attitude × demographic × wave (Tables 2–4)
- Trend plots: proportions over time with 95% confidence intervals
- Comparison to published Pew marginals as validity check

**1c. Attrition and nonresponse analysis**
- Compare Wave 119 and Wave 152 demographic compositions
- Test for differential representation of key demographic subgroups across waves

### Step 2: Polarization Metrics

Apply three complementary polarization metrics to capture different aspects of distributional change:

**2a. Variance and distributional spread**
- Calculate the proportion in each response category across waves
- Compute variance of the categorical distribution; test for significant increase over time

**2b. Bimodality coefficient (BC)**
BC = (γ² + 1) / κ, where γ = skewness and κ = excess kurtosis
- BC > 0.555 indicates bimodality (Pfister et al., 2013)
- Calculate at each wave; test for increase from Wave 119 to Wave 152

**2c. Esteban-Ray polarization index**
Adapted from continuous distributions to categorical ordinal data:
- Treat "concerned" and "excited" as opposing poles, "equal mix" as center
- ER index captures both the size of opposing groups and the distance between them
- Bootstrap confidence intervals for significance testing

**2d. Kurtosis analysis**
- Negative excess kurtosis (platykurtic) is consistent with bimodal/polarized distribution
- Track kurtosis change across waves as supplementary diagnostic

### Step 3: Latent Class Analysis (LCA) at Each Wave

LCA identifies latent subgroups (attitude classes) that share distinctive profiles across the attitude items. Given the single primary item, LCA will incorporate secondary attitude items available in each wave (e.g., specific AI use evaluations, AI benefit/harm perceptions) as conditional indicators to produce meaningful class solutions.

**3a. Model selection**
- Fit 2-, 3-, 4-, and 5-class solutions at each wave independently
- Selection criteria: Bayesian Information Criterion (BIC), Akaike Information Criterion (AIC), Lo-Mendell-Rubin (LMR) likelihood ratio test, entropy ≥ 0.80
- Interpret and label classes substantively (e.g., "AI Enthusiasts," "AI Skeptics," "Pragmatic Acceptors," "Uncertain Non-Attitudinalists")

**3b. Class profiles**
- Report class-conditional item probabilities
- Assess class separability via modal assignment and average posterior probabilities

**Software**: Mplus (preferred for LCA/LTA integration) or poLCA package in R

### Step 4: Cross-Temporal LCA Comparison

**4a. Class prevalence shifts**
- Compare class proportions (prevalence) across Wave 119, 132, and 152
- Chi-square or z-tests for proportional differences
- Plot class prevalence trajectories

**4b. Class profile stability**
- Assess whether class profiles (conditional item probabilities) remain substantively similar across waves, supporting a common latent structure
- Configural invariance testing if measurement model is specified

**4c. Polarization interpretation**
- Increasing size of "concerned" and "excited" classes at the expense of "mixed/uncertain" classes = evidence for distributional polarization
- Shifts in class-level demographic composition = evidence for group-level sorting

### Step 5: Multinomial Logistic Regression

**5a. Base models**
For each wave separately, regress AI concern-excitement (4-category nominal) on:
- Block 1: Demographics only (age, gender, race, education, income, region)
- Block 2: Demographics + political identity (party ID, ideology)
- Block 3: Demographics + political identity + AI awareness

Reference category: "An equal mix of concerned and excited"

Report: relative risk ratios (RRR), 95% confidence intervals, model fit (pseudo-R², AIC/BIC)

**5b. Pooled model with wave interactions**
Pool all three waves; include wave as a dummy variable; interact wave with key demographic and political predictors:
- Education × Wave: test H2a
- Age cohort × Wave: test H2b
- Party ID × Wave: test H3
- AI awareness × Wave: test H4

Apply survey weights; use robust standard errors (Taylor linearization or jackknife replication as appropriate for ATP design)

**5c. Auxiliary regressions**
Regress predicted class membership (from LCA) on demographic predictors; interpret as "who belongs to which attitude class."

### Step 6: Interaction Effects and Gap Decomposition

**6a. Predictive margins**
Calculate predictive margins (average marginal effects) for key demographic groups × wave combinations to visualize gap widening/narrowing in interpretable probability units.

**6b. Blinder-Oaxaca decomposition (supplementary)**
If cross-wave regressions show significant coefficient changes, decompose the change in group-level attitude proportions into:
- Composition effect (demographic change in the population)
- Coefficient effect (change in how demographics relate to attitudes)

This distinguishes true attitude change from compositional change in the sample.

### Step 7: Latent Transition Analysis (Conditional)

**Condition for activation**: Panel ID (QKEY) overlap of ≥ 1,500 respondents across at least two of the three waves.

**7a. LTA model**
- Specify the same latent classes identified in Step 3 as the time-1 and time-2 (or time-1/2/3) latent states
- Estimate transition probabilities: P(class j at t+1 | class i at t)
- Identify "stable" respondents (class retention) vs. "changers" (class transition)

**7b. Covariate model**
- Predict transition probabilities from baseline demographics and political identity
- Identify which groups are most likely to transition from "mixed/uncertain" to "concerned" or "excited" (crystallization)

**Software**: Mplus LTA syntax; or depmixS4/seqHMM in R

---

## 8. Robustness Checks

| Check | Procedure |
|-------|-----------|
| Weighted vs. unweighted | Run all primary analyses both ways; report if substantive conclusions differ |
| Alternative class solutions | Report 2- and 4-class solutions alongside the selected k-class solution |
| "Not sure" inclusion/exclusion | Run primary models including and excluding "Not sure" respondents |
| Education operationalization | Test 3-category (HS/some college/BA+) vs. 4-category (HS/some college/BA/postgrad) |
| Party ID coding | Test (1) 3-category (Rep/Ind/Dem) vs. (2) 7-point scale if available |
| Outlier waves | Report results for Wave 119→132 and Wave 132→152 transitions separately |
| Multiple imputation | For variables with > 5% missing, compare listwise deletion vs. MI results |

---

## 9. Expected Contributions

### 9.1 Theoretical Contributions

This study extends the Social Amplification of Risk Framework by providing one of the first empirical tests of SARF in the context of a rapidly diffusing AI technology. Specifically, it examines whether the amplification station function of mass-market AI (ChatGPT) operates differentially across demographic groups, producing not just increased risk salience but attitudinal bifurcation. The study also contributes to the Knowledge Gap literature by testing whether the hypothesis generalizes from factual knowledge gaps to evaluative attitude gaps in a novel technology domain.

### 9.2 Methodological Contributions

The analytic strategy integrates distributional polarization metrics (typically used in political science and economics) with Latent Class Analysis (common in survey methodology) to provide a multi-method triangulated assessment of attitude polarization. This approach offers a replicable template for future panel studies of technology acceptance and risk perception.

### 9.3 Practical Contributions

**For organizations**: Understanding which demographic segments are most susceptible to AI concern—and how rapidly those segments are growing—informs change management and AI adoption communication strategies. The partisan sorting findings (if confirmed) provide actionable intelligence for organizations navigating politically mixed workforces.

**For policymakers**: Evidence of widening demographic gaps in AI attitudes supports targeted AI literacy and public engagement initiatives. Groups showing accelerating concern without corresponding information access represent priority audiences for proactive communication.

**For AI developers and communicators**: If high-awareness respondents show stronger polarization (H4 confirmed), this suggests that simply increasing AI information does not reduce concern but may intensify pre-existing attitudinal tendencies—challenging the "information deficit" model of science communication.

---

## 10. Timeline

| Phase | Tasks | Duration | Target Completion |
|-------|-------|----------|-------------------|
| **Phase 1: Data Acquisition and Cleaning** | Register at pewresearch.org; download Wave 119, 132, 152 files; harmonize variable names and coding across waves; merge datasets; construct panel ID crosswalk | 1 week | Week 1 |
| **Phase 2: Descriptive Analysis** | Frequency tables; cross-tabs; trend plots; marginal validation against published Pew reports; attrition analysis | 1 week | Week 2 |
| **Phase 3: Polarization Metrics and LCA** | Bimodality coefficient; Esteban-Ray index; fit LCA models (2–5 classes) at each wave; select and interpret class solutions; cross-temporal comparison | 2 weeks | Weeks 3–4 |
| **Phase 4: Regression and Interaction Models** | Multinomial logistic regression (wave-specific and pooled); interaction effects; predictive margins; optional LTA if panel overlap confirmed | 2 weeks | Weeks 5–6 |
| **Phase 5: Robustness Checks** | Weighted vs. unweighted; alternative class solutions; sensitivity analyses | 1 week | Week 7 |
| **Phase 6: Writing and Revision** | Introduction and literature review; methods; results; discussion; limitations; formatting for target journal | 3–4 weeks | Weeks 8–11 |

**Total estimated duration**: 11 weeks from data access to submission-ready manuscript.

---

## 11. Limitations and Boundary Conditions

**1. Repeated cross-section design**: Without confirmed panel ID overlap, individual-level change trajectories cannot be established with certainty. All polarization inferences are at the population distribution level.

**2. Single-item DV**: The concern-excitement item conflates valence and salience. A respondent who is "more concerned" because they think AI will be powerful and potentially harmful holds a different attitude than one who is concerned because AI seems confusing and uncontrollable. Where wave-specific secondary items allow, attitude dimensionality will be explored in the LCA.

**3. Wave 152 sample size**: At N=5,410, Wave 152 has roughly half the sample size of earlier waves. Subgroup analyses and interaction tests for smaller demographic cells (e.g., Asian respondents, postgraduate-educated respondents) may be underpowered. Power analysis will be reported for key tests.

**4. Social desirability**: Self-reported AI concern/excitement may be subject to acquiescence bias or normative influence, particularly if respondents perceive a socially desirable response. The repeated cross-sectional design does not allow direct assessment of this threat.

**5. ChatGPT as the implicit referent**: The questionnaire asks about "artificial intelligence" generically, but the 2023–2024 context makes ChatGPT-style generative AI the salient referent for most respondents. Results may not generalize to attitudes about narrower AI applications (medical diagnostics, autonomous vehicles) addressed in separate Pew modules.

---

## 12. Software and Reproducibility

| Task | Software |
|------|---------|
| Data management and cleaning | R (haven, tidyverse) or Stata 17 |
| Survey-weighted descriptives | R (survey package) or Stata (svyset) |
| Polarization metrics | R (custom functions; ineq package for Gini-based alternatives) |
| Latent Class Analysis | Mplus 8.x (preferred); R (poLCA, mclust) as alternative |
| Latent Transition Analysis | Mplus 8.x |
| Multinomial regression | R (nnet, VGAM) or Stata (mlogit) |
| Visualization | R (ggplot2, ggalluvial for transition plots) |
| Version control | Git; analysis scripts stored in `/scripts/` |

All analysis code will be documented and archived. Output tables will be stored in `/output/tables/`; figures in `/output/figures/`.

---

## 13. Key References (Seed Bibliography)

Bonfadelli, H. (2002). The Internet and knowledge gaps: A theoretical and empirical investigation. *European Journal of Communication*, 17(1), 65–84.

Esteban, J., & Ray, D. (1994). On the measurement of polarization. *Econometrica*, 62(4), 819–851.

Kasperson, R. E., Renn, O., Slovic, P., Brown, H. S., Emel, J., Goble, R., ... & Ratick, S. (1988). The social amplification of risk: A conceptual framework. *Risk Analysis*, 8(2), 177–187.

Lazarsfeld, P. F., & Henry, N. W. (1968). *Latent structure analysis*. Houghton Mifflin.

Magidson, J., & Vermunt, J. K. (2004). Latent class models. In D. Kaplan (Ed.), *The SAGE handbook of quantitative methodology for the social sciences* (pp. 175–198). Sage.

Pfister, R., Schwarz, K. A., Janczyk, M., Dale, R., & Freeman, J. B. (2013). Good things peak in pairs: A note on the bimodality coefficient. *Frontiers in Psychology*, 4, 700.

Pidgeon, N., Kasperson, R. E., & Slovic, P. (Eds.). (2003). *The social amplification of risk*. Cambridge University Press.

Pew Research Center. (2023, August). *Americans' views of artificial intelligence*. https://www.pewresearch.org/internet/2023/08/28/americans-use-of-chatgpt-is-ticking-up-but-few-trust-its-election-information/

Pew Research Center. (2024). *Public views of artificial intelligence*. [Wave 152 topline report]

Tichenor, P. J., Donohue, G. A., & Olien, C. N. (1970). Mass media flow and differential growth in knowledge. *Public Opinion Quarterly*, 34(2), 159–170.

Vermunt, J. K., & Magidson, J. (2016). Technical guide for Latent GOLD 5.1: Basic, advanced, and syntax. Statistical Innovations.

---

*Document version: 1.0 | Prepared: February 2026 | Status: Working analysis plan — subject to revision following data access and preliminary exploration*
