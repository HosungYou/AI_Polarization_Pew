# Variable Codebook
## AI Attitude Polarization Study — Pew Research Center American Trends Panel

**Project:** AI Attitude Polarization Using Pew ATP Data (3 Waves)
**Last Updated:** 2026-02-22

---

## Table of Contents

1. [Key Dependent Variable](#key-dependent-variable)
2. [AI Awareness/Use Variables](#ai-awarenessuse-variables)
3. [Wave-Specific AI Variables](#wave-specific-ai-variables)
   - [Wave 119 (Dec 2022) — AI & Healthcare](#wave-119-dec-2022--ai--healthcare-focus)
   - [Wave 132 (Aug 2023) — AI Help vs Hurt](#wave-132-aug-2023--ai-help-vs-hurt)
   - [Wave 152 (Aug 2024) — Comprehensive AI Attitudes](#wave-152-aug-2024--comprehensive-ai-attitudes)
4. [Demographic Variables](#demographic-variables-consistent-across-all-3-waves)
5. [Survey Weights](#survey-weights)
6. [Data Files](#data-files)
7. [Missing Data Codes](#missing-data-codes)
8. [Notes](#notes)

---

## Key Dependent Variable

### CNCEXC

**Full question:** "Overall, would you say the increased use of artificial intelligence (AI) in daily life makes you feel..."

**Present in ALL 3 waves:**

| Wave | Variable Name |
|------|--------------|
| Wave 119 (Dec 2022) | `CNCEXC_W119` |
| Wave 132 (Aug 2023) | `CNCEXC_W132` |
| Wave 152 (Aug 2024) | `CNCEXC_W152` |

**Values:**

| Code | Label |
|------|-------|
| 1 | More excited than concerned |
| 2 | More concerned than excited |
| 3 | Equally concerned and excited |
| 99 | Refused / Don't Know |

**Trend (weighted estimates):**

| Response | W119 (Dec 2022) | W132 (Aug 2023) | W152 (Aug 2024) |
|----------|----------------|----------------|----------------|
| More excited than concerned | 15.7% | 9.2% | 11.3% |
| More concerned than excited | 37.6% | 53.2% | 49.4% |
| Equally concerned and excited | 45.9% | 36.5% | 38.7% |

> **Interpretation:** Concern increased sharply from W119 to W132, with modest recovery of excitement in W152. The "equally" middle category shrank substantially after the ChatGPT/GPT-4 public release period.

---

## AI Awareness/Use Variables

### AI_HEARD

**Full question:** "How much have you heard or read about AI?"

**Present in ALL 3 waves:**

| Wave | Variable Name |
|------|--------------|
| Wave 119 (Dec 2022) | `AI_HEARD_W119` |
| Wave 132 (Aug 2023) | `AI_HEARD_W132` |
| Wave 152 (Aug 2024) | `AI_HEARD_W152` |

**Values:**

| Code | Label |
|------|-------|
| 1 | A lot |
| 2 | A little |
| 3 | Nothing at all |

---

### USEAI

**Full question:** "How often do you interact with AI?"

**Present in W119 and W152 ONLY (not W132):**

| Wave | Variable Name |
|------|--------------|
| Wave 119 (Dec 2022) | `USEAI_W119` |
| Wave 152 (Aug 2024) | `USEAI_W152` |

**Values:**

| Code | Label |
|------|-------|
| 1 | Almost constantly |
| 2 | Several times a day |
| 3 | About once a day |
| 4 | Several times a week |
| 5 | Less often |

---

## Wave-Specific AI Variables

---

### Wave 119 (Dec 2022) — AI & Healthcare Focus

#### AI Knowledge Items (AIKNOW1–7)

Seven-item AI knowledge quiz. Each item has a corresponding `_CORRECT` version indicating whether the respondent answered correctly.

| Variable | Description |
|----------|-------------|
| `AIKNOW1_W119` | AI knowledge item 1 |
| `AIKNOW2_W119` | AI knowledge item 2 |
| `AIKNOW3_W119` | AI knowledge item 3 |
| `AIKNOW4_W119` | AI knowledge item 4 |
| `AIKNOW5_W119` | AI knowledge item 5 |
| `AIKNOW6_W119` | AI knowledge item 6 |
| `AIKNOW7_W119` | AI knowledge item 7 |
| `AIKNOW1_CORRECT_W119` through `AIKNOW7_CORRECT_W119` | Dichotomous correct/incorrect versions |
| `AIKNOW_INDEX_W119` | Composite AI knowledge score (sum of correct items, 0–7) |

---

#### AI Healthcare Variables

| Variable | Description | Scale |
|----------|-------------|-------|
| `AIHCCOMF_W119` | Comfort with AI in healthcare | 1=Very comfortable, 2=Somewhat comfortable, 3=Not too comfortable, 4=Not at all comfortable |
| `AIHCTRT1_W119` | AI healthcare outcomes compared to human providers | Better / About the same / Worse |
| `AIHCTRT2_W119` | Perception of healthcare provider AI adoption speed | Too fast / About right / Too slow |
| `AIHCCHG_QUAL_W119` | AI impact on quality of healthcare | Better / Worse / No difference |
| `AIHCCHG_MIST_W119` | AI impact on medical mistakes | Better / Worse / No difference |
| `AIHCCHG_REL_W119` | AI impact on patient-provider relationship | Better / Worse / No difference |
| `AIHCCHG_RACETHN_W119` | AI impact on racial/ethnic fairness in healthcare | Better / Worse / No difference |
| `AIHCCHG_SECUR_W119` | AI impact on healthcare data security | Better / Worse / No difference |

---

#### AI Mental Health Variables

| Variable | Description |
|----------|-------------|
| `AIMH1_W119` | AI mental health attitude item 1 |
| `AIMH2_W119` | AI mental health attitude item 2 |
| `AIMH3_W119` | AI mental health attitude item 3 |
| `AIMH5_W119` | AI mental health attitude item 5 |

---

#### AI Pain Assessment Variables

| Variable | Description |
|----------|-------------|
| `AIPAIN1_W119` | AI pain assessment attitude item 1 |
| `AIPAIN2_W119` | AI pain assessment attitude item 2 |
| `AIPAIN3_W119` | AI pain assessment attitude item 3 |
| `AIPAIN4_W119` | AI pain assessment attitude item 4 |

---

#### Other AI Application Variables

| Variable | Description |
|----------|-------------|
| `AIPROT1_W119` | AI protection application 1 |
| `AIPROT2_W119` | AI protection application 2 |
| `AICROP1_W119` | AI crop/agriculture application 1 |
| `AICROP2_W119` | AI crop/agriculture application 2 |
| `AIEXT1_W119` | AI extraction/other application 1 |
| `AIEXT2_W119` | AI extraction/other application 2 |
| `AINEWS1_W119` | AI news application 1 |
| `AINEWS2_W119` | AI news application 2 |
| `AIIMAG1_W119` | AI image/visual application 1 |
| `AIIMAG2_W119` | AI image/visual application 2 |

---

#### AI Workplace Variables

| Variable | Description |
|----------|-------------|
| `AIWRK2_W119` | AI workplace attitude item 2 |
| `AIWRK3_W119` | AI workplace attitude item 3 |
| `AIWRKH1_W119` | AI workplace help item 1 |
| `AIWRKH2_W119` | AI workplace help item 2 |
| `AIWRKH3_W119` | AI workplace help item 3 |
| `AIWRKH4_W119` | AI workplace help item 4 |
| `AIWRKH5_W119` | AI workplace help item 5 |
| `AIWRKM1_W119` | AI workplace harm item 1 |
| `AIWRKM2_W119` | AI workplace harm item 2 |
| `AIWRKM3_W119` | AI workplace harm item 3 |
| `AIWRKM4_W119` | AI workplace harm item 4 |

---

#### Dispositional Traits (Wave 119)

Five-point Likert scale items measuring individual predispositions toward risk-taking, creativity, and technology enjoyment.

| Variable | Description | Scale |
|----------|-------------|-------|
| `DESRISK_COMF_W119` | Risk-taking disposition — comfort with risk | 1=Strongly agree to 5=Strongly disagree |
| `DESRISK_CREAT_W119` | Creativity disposition | 1=Strongly agree to 5=Strongly disagree |
| `DESRISK_NTECH_W119` | Technology enjoyment disposition | 1=Strongly agree to 5=Strongly disagree |

---

### Wave 132 (Aug 2023) — AI Help vs Hurt

#### AIHLPHRT — AI Help or Hurt by Domain

**Full question stem:** "Do you think artificial intelligence will mostly help or mostly hurt [domain]?"

**8 domain items:**

| Variable | Domain |
|----------|--------|
| `AIHLPHRT_a_W132` | People finding products and services they want |
| `AIHLPHRT_b_W132` | People's ability to find reliable health information |
| `AIHLPHRT_c_W132` | The economy |
| `AIHLPHRT_d_W132` | US national security |
| `AIHLPHRT_e_W132` | How fast information on the internet spreads |
| `AIHLPHRT_f_W132` | How people connect with healthcare providers |
| `AIHLPHRT_g_W132` | Patients' personal data and records privacy |
| `AIHLPHRT_h_W132` | How fairly people are treated by healthcare providers |

**Values:**

| Code | Label |
|------|-------|
| 1 | Mostly help |
| 2 | Mostly hurt |
| 3 | Help and hurt equally |

> **Note:** Value labels may not be present in the SPSS file — verify against original codebook before analysis.

---

### Wave 152 (Aug 2024) — Comprehensive AI Attitudes

#### AICONTROL1 — Perceived Control Over AI

**Full question:** "How much control do you feel you have over how AI is used in your life?"

**Values:**

| Code | Label |
|------|-------|
| 1 | A great deal |
| 2 | Some |
| 3 | Not too much |
| 4 | Very little |
| 5 | None at all |

---

#### AICONTROL2 — Comfort With or Desire for More Control

**Values:**

| Code | Label |
|------|-------|
| 1 | Comfortable with current level of control |
| 2 | Want more control |
| 3 | Not sure |

---

#### AICHANGE — AI Impact on US in 20 Years

**Full question:** "How do you think AI will affect the United States over the next 20 years?"

**Values:**

| Code | Label |
|------|-------|
| 1 | Very positive |
| 2 | Somewhat positive |
| 3 | Neither positive nor negative |
| 4 | Somewhat negative |
| 5 | Very negative |
| 6 | Not sure |

---

#### AIFUTRIMPCT — AI Future Impact by Domain (10 items)

**Full question stem:** "Do you think AI will have a mostly positive or mostly negative impact on [domain] in the future?"

**Scale:** 1=Very positive, 2=Somewhat positive, 3=Neither, 4=Somewhat negative, 5=Very negative

| Variable | Domain | Form |
|----------|--------|------|
| `AIFUTRIMPCT_a_W152` | Medical care | Form 1 |
| `AIFUTRIMPCT_b_W152` | K-12 education | Form 1 |
| `AIFUTRIMPCT_c_W152` | Elections | Form 1 |
| `AIFUTRIMPCT_d_W152` | Economy | Form 1 |
| `AIFUTRIMPCT_e_W152` | Criminal justice | Form 1 |
| `AIFUTRIMPCT_f_W152` | Arts and entertainment | Form 2 |
| `AIFUTRIMPCT_g_W152` | Personal relationships | Form 2 |
| `AIFUTRIMPCT_h_W152` | How people do their jobs | Form 2 |
| `AIFUTRIMPCT_i_W152` | Environment | Form 2 |
| `AIFUTRIMPCT_j_W152` | News | Form 2 |

> **Split-form design:** Items a–e assigned to Form 1 respondents; items f–j assigned to Form 2 respondents. Check `X_FORM_W152` to identify form assignment.

---

#### AIJOBS — AI Impact on Jobs (Overall)

New variable in Wave 152 measuring perceived AI impact on employment broadly.

---

#### AIJOBIMPCT — AI Job Impact by Domain (10 items)

| Variable | Domain |
|----------|--------|
| `AIJOBIMPCT_a_W152` | Job impact domain a |
| `AIJOBIMPCT_b_W152` | Job impact domain b |
| `AIJOBIMPCT_c_W152` | Job impact domain c |
| `AIJOBIMPCT_d_W152` | Job impact domain d |
| `AIJOBIMPCT_e_W152` | Job impact domain e |
| `AIJOBIMPCT_f_W152` | Job impact domain f |
| `AIJOBIMPCT_g_W152` | Job impact domain g |
| `AIJOBIMPCT_h_W152` | Job impact domain h |
| `AIJOBIMPCT_i_W152` | Job impact domain i |
| `AIJOBIMPCT_j_W152` | Job impact domain j |

---

#### HUMANVAI — Human vs AI Preferences (8 items)

| Variable | Description |
|----------|-------------|
| `HUMANVAI_a_W152` | Human vs AI preference item a |
| `HUMANVAI_b_W152` | Human vs AI preference item b |
| `HUMANVAI_c_W152` | Human vs AI preference item c |
| `HUMANVAI_d_W152` | Human vs AI preference item d |
| `HUMANVAI_e_W152` | Human vs AI preference item e |
| `HUMANVAI_f_W152` | Human vs AI preference item f |
| `HUMANVAI_g_W152` | Human vs AI preference item g |
| `HUMANVAI_h_W152` | Human vs AI preference item h |

---

#### TRSTAIPRS — Trust AI to Protect Personal Information

**Values:**

| Code | Label |
|------|-------|
| 1 | Yes |
| 2 | No |
| 3 | Not sure |

---

#### AICONCERN — Specific AI Concerns (7 items)

**Scale:** 1=Extremely concerned, 2=Very concerned, 3=Somewhat concerned, 4=Not too concerned, 5=Not at all concerned

| Variable | Concern |
|----------|---------|
| `AICONCERN_a_W152` | AI bias and discrimination |
| `AICONCERN_b_W152` | Impersonation via AI (deepfakes, etc.) |
| `AICONCERN_c_W152` | Privacy misuse by AI systems |
| `AICONCERN_d_W152` | Inaccurate or false information generated by AI |
| `AICONCERN_e_W152` | Lack of human understanding of AI decisions |
| `AICONCERN_f_W152` | Loss of human connection due to AI |
| `AICONCERN_g_W152` | Job loss due to AI automation |

---

#### FUTRAI — Future AI Capabilities (4 items)

| Variable | Description |
|----------|-------------|
| `FUTRAI_a_W152` | Future AI capability item a |
| `FUTRAI_b_W152` | Future AI capability item b |
| `FUTRAI_c_W152` | Future AI capability item c |
| `FUTRAI_d_W152` | Future AI capability item d |

---

#### AIREG — AI Regulation Preference

**Full question:** "Do you think the U.S. government's regulation of AI will go too far, not go far enough, or will it be about right?"

**Values:**

| Code | Label |
|------|-------|
| 1 | Go too far |
| 2 | Not go far enough |
| 3 | About right |
| 4 | Not sure |

---

## Demographic Variables (Consistent Across All 3 Waves)

All demographic variables carry the wave suffix (e.g., `F_AGECAT_W119`, `F_AGECAT_W132`, `F_AGECAT_W152`).

| Variable | Description | Values |
|----------|-------------|--------|
| `F_AGECAT` | Age category (4 groups) | 1=18–29, 2=30–49, 3=50–64, 4=65+ |
| `F_GENDER` | Gender identity | 1=A man, 2=A woman, 3=In some other way |
| `F_EDUCCAT` | Education (3 categories) | 1=College graduate or more, 2=Some college, 3=High school graduate or less |
| `F_EDUCCAT2` | Education (6 categories) | 1=Less than high school, 2=High school graduate, 3=Some college, 4=Associate degree, 5=College graduate, 6=Postgraduate degree |
| `F_RACETHNMOD` | Race/ethnicity | 1=White non-Hispanic, 2=Black non-Hispanic, 3=Hispanic, 4=Other non-Hispanic, 5=Asian non-Hispanic |
| `F_PARTY_FINAL` | Political party identification | 1=Republican, 2=Democrat, 3=Independent, 4=Something else |
| `F_PARTYSUM_FINAL` | Party ID with leaners (summary) | 1=Republican/Lean Republican, 2=Democrat/Lean Democrat, 9=Don't know / No lean |
| `F_PARTYSUMIDEO_FINAL` | Party identification + ideology (4-way) | 1=Conservative Republican, 2=Moderate or Liberal Republican, 3=Moderate or Conservative Democrat, 4=Liberal Democrat |
| `F_INC_SDT1` | Household income (9 categories) | 1=Less than $30,000 to 9=$100,000 or more |
| `F_INC_TIER2` | Income tier (3 groups) | 1=Lower income, 2=Middle income, 3=Upper income |
| `F_CREGION` | U.S. Census region | 1=Northeast, 2=Midwest, 3=South, 4=West |
| `F_METRO` | Metropolitan/non-metropolitan status | 1=Metropolitan, 2=Non-metropolitan |
| `F_RELIG` | Religious affiliation | 1=Protestant through 12=Nothing in particular (see full codebook for all codes) |
| `F_MARITAL` | Marital status | 1=Married, 2=Living with partner, 3=Divorced, 4=Separated, 5=Widowed, 6=Never married |
| `F_BORN` | Born-again or evangelical Christian | 1=Yes, 2=No |

---

## Survey Weights

| Wave | Weight Variable |
|------|----------------|
| Wave 119 (Dec 2022) | `WEIGHT_W119` |
| Wave 132 (Aug 2023) | `WEIGHT_W132` |
| Wave 152 (Aug 2024) | `WEIGHT_W152` |

> **Requirement:** Survey weights MUST be applied for all population-level estimates. Unweighted estimates should not be reported as nationally representative. Use the appropriate wave-specific weight variable when analyzing a single wave, and consider calibration for cross-wave comparisons.

---

## Data Files

| Wave | File Path | Rows | Columns |
|------|-----------|------|---------|
| Wave 119 (Dec 2022) | `data/raw/W119_Dec22/ATP W119.sav` | 11,004 | 156 |
| Wave 132 (Aug 2023) | `data/raw/W132_Aug23/ATP W132.sav` | 11,201 | 140 |
| Wave 152 (Aug 2024) | `data/raw/W152_Aug24/ATP W152.sav` | 5,410 | 106 |

All files are in SPSS `.sav` format. Use `pyreadstat`, `haven` (R), or SPSS to import.

---

## Missing Data Codes

| Code | Meaning | Context |
|------|---------|---------|
| 98 | Don't know | Phone mode responses |
| 99 | Refused | Phone mode; also used for web non-response (blank) |

> **Handling:** Codes 98 and 99 are combined and treated as missing (`NA`) for most analytical purposes. Do not include these as valid response categories in frequency distributions or regression models unless explicitly analyzing non-response patterns.

---

## Notes

### Study Design
- **Data type:** Repeated cross-sections — NOT a longitudinal panel. Different individuals are sampled in each wave.
- **Implication:** Cross-wave comparisons are at the aggregate level (population estimates), not individual-level change. Do not use fixed-effects or within-person change models.

### Weighting
- Weighted estimates are required for population-level inference.
- Raw (unweighted) counts should only be used for sample description or quality checks.

### Split-Form Design
- Some Wave 152 questions use a split-form design where different respondents receive different question batteries (Form 1 vs. Form 2).
- Always check the `X_FORM_W152` variable before analyzing items with form assignment.
- Do not combine Form 1 and Form 2 respondents on form-specific items without accounting for the design.

### Variable Naming Convention
- All wave-specific variables end with the wave suffix: `_W119`, `_W132`, or `_W152`.
- Variables available in multiple waves share the same base name (e.g., `CNCEXC`, `AI_HEARD`).

### AI Attitude Trend Caution
- The sharp increase in concern from W119 to W132 coincides with the widespread public release of ChatGPT and GPT-4 (late 2022 – early 2023), and should be interpreted in that context.
- W152 shows modest recovery of the "excited" category, but concern remains elevated relative to W119 baseline.
