---
title: "Beyond the Information Deficit: Epistemic Cognition, Education, and the Formation of Complex AI Attitudes Among U.S. Adults"
author: "Hosung You"
affiliation: "Department of Learning and Performance Systems, Pennsylvania State University"
date: "2026"
output: word_document
bibliography: references.bib
csl: apa.csl
---

# Abstract

**Purpose.** This study challenges the prevailing information deficit model underlying most AI literacy initiatives by examining how educational attainment shapes the *complexity* rather than the *direction* of public attitudes toward artificial intelligence. Drawing on epistemic cognition theory and the knowledge gap hypothesis, the study reinterprets survey responses indicating simultaneous concern and excitement about AI as evidence of reflective judgment capacity rather than mere ambivalence.

**Design/methodology/approach.** Secondary analysis of three waves of Pew Research Center American Trends Panel data (December 2022, raw *N* = 11,004; August 2023, raw *N* = 11,201; August 2024, raw *N* = 5,410; analytic *N* = 24,741 after listwise deletion and restriction to partisan leaners). Survey-weighted multinomial logistic regression models treat the "equally concerned and excited" response as the reference category, representing epistemic complexity. Analyses include cross-tabulation with Rao-Scott chi-square tests, interaction models, average marginal effects, and Blinder-Oaxaca decomposition.

**Findings.** College graduates were consistently the least likely to report categorical concern and the most likely to hold complex ("equally") attitudes across all three waves. Adults with a high school education or less were significantly more likely to express categorical concern (OR = 1.35, *p* < .001) relative to attitudinal complexity. Critically, the wave-by-education interaction was not significant (joint Wald *p* = .563), indicating that education-based differences in epistemic processing were stable across the study period rather than widening or narrowing in response to accelerating AI discourse. Partisan identity remained the strongest single predictor (AME gap = 12.5 percentage points in December 2022), but this gap converged over time.

**Implications.** AI literacy programs in human resource development and adult education contexts should prioritize epistemic development, specifically the capacity to hold productive tension between competing evaluations, rather than pursuing uniformly positive attitudes toward AI. Transformative learning pedagogy offers a promising design framework for cultivating this capacity.

**Originality/value.** This study is among the first to apply epistemic cognition theory to public AI attitudes, reconceptualizing the "equally concerned and excited" response as an empirical signature of reflective judgment rather than indecision. It provides a theoretically grounded alternative to the deficit model that dominates current AI literacy policy.

**Keywords:** epistemic cognition, AI attitudes, reflective judgment, knowledge gap, AI literacy, adult education, human resource development

\newpage

# Introduction

Governments, corporations, and educational institutions across the globe are pouring resources into artificial intelligence literacy programs. The shared assumption animating these efforts is straightforward: more knowledge about AI will produce more favorable attitudes toward its adoption [@long2020; @ng2021; @laupichler2022]. This premise, that public skepticism stems from an information deficit that education can remedy, echoes a longstanding paradigm in science communication, one that empirical evidence has repeatedly undermined [@scheufele2005; @kahan2012]. And yet AI literacy initiatives continue to multiply under precisely this logic, measuring success by whether participants come away with more positive views of AI rather than more sophisticated ones.

The stakes are not trivial. As AI systems become embedded in hiring processes, healthcare diagnostics, criminal sentencing, and educational assessment, public attitudes increasingly shape policy decisions, organizational adoption patterns, and individual career choices [@brynjolfsson2014; @frey2017; @parker2022]. If AI education simply swaps skepticism for enthusiasm without building the capacity for nuanced evaluation, the result may be a workforce that complies with AI integration but cannot critically assess when such integration serves human interests and when it does not.

Recent nationally representative data from the Pew Research Center present a puzzle the information deficit model cannot resolve. Between December 2022 and August 2024, a period of unprecedented public exposure to AI through ChatGPT and similar systems, the proportion of American adults expressing concern about increased AI use rose sharply from 38.2% to 50.9%, while excitement declined from 15.5% to 10.9% [@pew2023; @pew2024]. If more information straightforwardly led to more positive attitudes, the massive increase in public AI awareness during this period should have produced the opposite pattern. Even those who reported having heard "a lot" about AI grew more concerned over time, cutting against the core prediction of the deficit model.

What the deficit model misses, though, is a third category in the Pew survey: respondents who reported feeling "equally concerned and excited" about the increased use of AI in daily life. This group constituted 46.3% of respondents in December 2022 and 38.3% by August 2024. Prior analyses have treated them as a residual middle category, people who simply could not decide. This study proposes a fundamentally different reading. Drawing on decades of research in epistemic cognition and reflective judgment [@king1994; @kuhn1991; @perry1970], it argues that the capacity to simultaneously hold both concern and excitement about a complex, uncertain phenomenon represents the *highest* rather than the *lowest* level of epistemic processing. Holding AI's transformative potential and its risks in productive tension demands cognitive sophistication that categorical responses, whether purely excited or purely concerned, do not require.

This reframing carries profound implications for how AI literacy programs are designed and evaluated. If the "equally" response reflects epistemic complexity rather than indecision, then AI education should not aim to move people from concern to excitement. It should instead develop the epistemic capacity that enables nuanced evaluation of technologies whose consequences remain genuinely uncertain. The question shifts from "How do we make people more positive about AI?" to "How do we help people think more complexly about AI?"

This study addresses four gaps in the existing literature. First, no prior study has applied epistemic cognition theory to the analysis of public AI attitudes, despite the natural fit between reflective judgment models and the challenge of evaluating a technology characterized by deep uncertainty. Second, the role of education in AI attitude formation has been examined primarily through the lens of the knowledge gap hypothesis [@tichenor1970; @hwang2009], which predicts widening differences in knowledge acquisition but does not theorize how education shapes the *quality* of attitude formation. Third, existing AI literacy frameworks emphasize technical competencies, such as understanding what AI can do, how it works, and where it is applied [@long2020; @ng2021], while largely neglecting the epistemic dispositions that enable adults to evaluate AI's societal implications. Fourth, there is no empirical evidence on whether education-based differences in AI attitudes are static or dynamic across a period of rapidly expanding AI discourse.

Using three waves of Pew American Trends Panel data spanning the launch and normalization of generative AI (*N* = 24,741), this study tests five hypotheses derived from epistemic cognition theory and the knowledge gap hypothesis. The results show that education creates durable differences in the *type* of AI attitudes people hold, with college graduates consistently favoring epistemic complexity, rather than merely shifting the direction of attitudes along a positive-negative continuum. These findings challenge the information deficit model and offer an alternative theoretical foundation for AI literacy programming in human resource development (HRD) and adult education contexts.

# Theoretical Framework and Hypotheses

## The Information Deficit Model and Its Limitations

The information deficit model has dominated science communication scholarship and practice for decades. Its logic is simple: public opposition to science and technology stems from inadequate knowledge, so providing more and better information will align public attitudes with expert consensus [@scheufele2005]. Applied to artificial intelligence, the model predicts that AI literacy education (teaching people what AI is, how it works, and what it can do) will reduce anxiety and increase acceptance. This assumption drives the rapid expansion of AI literacy curricula in corporate training programs, higher education, and public policy initiatives worldwide [@laupichler2022; @selwyn2022].

The empirical record, however, tells a different story. @kahan2012 demonstrated that individuals with higher science literacy and numeracy were *more*, not less, politically polarized on climate change risks, suggesting that knowledge enhances the capacity for motivated reasoning rather than simply correcting misperceptions. In the case of nanotechnology, a technology that, like AI, was marked by novelty, uncertainty, and significant transformative potential, @scheufele2005 found that public attitudes were shaped far more by value predispositions and trust in institutions than by factual knowledge. @bonfadelli2002 extended these findings to the digital context, showing that internet access alone did not close knowledge gaps and that pre-existing cognitive resources shaped how individuals processed new information.

The Pew data examined in this study offer particularly striking evidence against the deficit model in the AI domain. Between December 2022 and August 2023, a period during which public awareness of AI surged following the release of ChatGPT, the proportion of adults who had heard "a lot" about AI jumped. Yet concern about AI *also* rose sharply, from 38.2% to 53.2%. Under the deficit model, increased awareness should have eased concern; instead, awareness and concern climbed in tandem. Even among those reporting the highest levels of AI awareness, concern increased substantially. It appears that more information about AI does not automatically produce more positive attitudes. Under certain conditions, it may instead produce more *differentiated* attitudes that include heightened appreciation of both benefits and risks.

## The Knowledge Gap Hypothesis

While the information deficit model focuses on the aggregate relationship between knowledge and attitudes, the knowledge gap hypothesis addresses how information is differentially acquired and processed across social strata. Originally formulated by @tichenor1970, the hypothesis proposes that as the infusion of mass media information into a social system increases, segments of the population with higher socioeconomic status tend to acquire this information faster than lower-status segments, so that the gap in knowledge between these segments tends to increase rather than decrease. Education is the primary mechanism: formal education provides not only factual knowledge but also the cognitive skills, media literacy, and social networks that facilitate information processing [@viswanath1996; @gaziano1997].

Applied to AI, the knowledge gap hypothesis generates important but incomplete predictions. It suggests that as AI discourse proliferates through news media, social platforms, and workplace communications, college-educated adults will develop more sophisticated understandings of AI more rapidly than those with less formal education. @hwang2009, in a meta-analysis spanning 35 years of knowledge gap research, confirmed that the gap is robust across diverse topics and information environments, with education consistently emerging as the strongest predictor of differential knowledge acquisition. @bonfadelli2002 showed that the knowledge gap extends to digital technologies, where access inequalities compound informational ones.

The traditional knowledge gap hypothesis, however, theorizes differences in the *quantity* of knowledge rather than the *quality* of processing. It predicts that college-educated adults will know more about AI but does not specify how this knowledge advantage translates into attitude formation. A person might know more about AI and become more excited about its potential. Or they might know more and become more concerned about its risks. Or they might know more and develop the capacity to hold both evaluations simultaneously. The knowledge gap hypothesis, as originally formulated, cannot discriminate among these possibilities.

This study extends the knowledge gap framework by proposing that the education gap in AI attitudes operates primarily through *processing complexity* rather than knowledge quantity. The argument is that formal education, particularly higher education, develops not just knowledge but epistemic capacities that shape how individuals engage with inherently uncertain and contested claims about AI's societal implications. This reconceptualization shifts attention from what people know about AI to *how they know*, that is, the epistemic frameworks through which they evaluate competing claims.

## Epistemic Cognition Development

Epistemic cognition refers to how individuals think about knowledge and knowing, including their assumptions about the certainty, simplicity, sources, and justification of knowledge claims [@hofer1997]. The field has produced several influential developmental models that, despite differences in terminology and emphasis, converge on a common trajectory from naive to sophisticated epistemological understanding.

@king1994 proposed the Reflective Judgment Model (RJM), which identifies seven stages grouped into three broad levels of epistemic development. At the *pre-reflective* level (stages 1--3), individuals assume that knowledge is certain, directly observable, and dictated by authorities. When confronted with ill-structured problems, that is, problems for which evidence is incomplete, experts disagree, and reasonable people can reach different conclusions, pre-reflective thinkers either defer to authority or deny the legitimacy of the uncertainty itself. At the *quasi-reflective* level (stages 4--5), individuals recognize that knowledge is uncertain and that evidence can be interpreted differently, but they cannot yet articulate how competing interpretations should be evaluated and integrated. At the *reflective* level (stages 6--7), individuals understand that knowledge is constructed through inquiry, that competing claims must be evaluated using available evidence and reasoned argument, and that well-justified conclusions can be reached even in the face of genuine uncertainty.

@perry1970, working specifically in the higher education context, described a parallel developmental trajectory from dualistic thinking (knowledge is right or wrong; authorities determine truth) through multiplistic thinking (all opinions are equally valid; knowledge is subjective) to contextual relativism (knowledge claims must be evaluated against evidence within particular frameworks). @baxtermagolda2001, building on Perry's work, introduced the concept of *self-authorship*, the capacity to construct one's own knowledge frameworks through internal rather than external validation, as the hallmark of mature epistemic functioning. @kuhn1991 provided extensive empirical evidence that the skills of argumentation, specifically the capacity to construct and evaluate evidence-based arguments about uncertain claims, develop unevenly across the adult population and are significantly related to educational attainment.

These developmental models share a crucial implication for understanding AI attitudes: the capacity to simultaneously hold competing evaluations of AI, recognizing both its transformative potential and its genuine risks, is itself an *achievement* of epistemic development, not a failure of opinion formation. Pre-reflective thinkers, confronted with the inherently ill-structured question of whether AI will be beneficial or harmful, are likely to resolve the uncertainty categorically: AI is either good (exciting) or bad (concerning). Reflective thinkers, by contrast, can sustain the epistemic stance that AI is *both* promising and threatening, and that the relationship between these evaluations must be worked out contextually rather than resolved in the abstract.

@hofer1997, in their comprehensive review of epistemological theories, identified four dimensions of epistemic beliefs: the certainty of knowledge, the simplicity of knowledge, the source of knowledge, and the justification of knowledge. Each dimension has implications for AI attitude formation. Individuals who believe knowledge is certain and simple are likely to seek definitive answers to the question "Is AI good or bad?", and the categorical response options of "excited" or "concerned" satisfy this epistemic need. Those who believe knowledge is complex and justified through evidence and argument are more likely to recognize that AI's impact depends on context, implementation, and governance, a recognition captured by the "equally" response.

@bendixen2004 offered an integrative model identifying epistemic doubt, the discomfort that arises when existing epistemic assumptions prove inadequate, as a key mechanism of epistemic change. The rapid proliferation of AI in daily life, particularly the public release of large language models, represents precisely the kind of disruption that can trigger epistemic doubt. @bromme2010 cautioned, however, that epistemic doubt does not automatically lead to epistemic growth; without adequate scaffolding, it may instead produce defensive retreat into more rigid epistemic positions. @barzilai2018 proposed the concept of *apt epistemic performance*, the capacity to deploy appropriate epistemic practices in response to specific knowledge challenges, as the proper goal of epistemic education, a framework directly applicable to AI literacy.

## AI Literacy as Epistemic Development

Reconceptualizing AI attitudes through the lens of epistemic cognition has direct implications for how AI literacy is defined and pursued. Current AI literacy frameworks, while valuable, tend to focus on competencies that map primarily onto the knowledge dimension rather than the epistemic dimension. @long2020 identified 17 AI literacy competencies organized around five themes: recognizing AI, understanding AI, using AI, evaluating AI, and creating with AI. @ng2021, in a systematic review, conceptualized AI literacy as encompassing technical understanding, practical application, and ethical awareness. These frameworks matter, but they are insufficient if the goal extends beyond knowledge acquisition to epistemic development.

The adult education literature provides theoretical resources for this extension. @mezirow1991 described transformative learning as a process through which adults critically examine and revise their meaning perspectives, the habitual frames of reference through which they interpret experience. AI presents what Mezirow would call a *disorienting dilemma*: an experience that cannot be assimilated into existing meaning structures without fundamental revision of assumptions about work, expertise, creativity, and human uniqueness [@mezirow2000]. @cranton2006, translating Mezirow's theory into pedagogical practice, emphasized that transformative learning requires not only exposure to new information but also critical reflection on the assumptions that structure one's interpretation of that information. This is precisely the kind of epistemic work that the "equally" response may reflect.

The connection between formal education and epistemic development is well documented but not deterministic. @king1994 found that reflective judgment scores were significantly correlated with educational level, with the most dramatic gains occurring during and immediately after college. They also found substantial within-level variation, though, indicating that education creates *opportunities* for epistemic development rather than guaranteeing it. @merriam2007 noted that adult learning contexts vary dramatically in their capacity to support epistemic growth, with some educational environments reinforcing pre-reflective thinking patterns rather than challenging them. @jarvis2006 argued that meaningful learning requires engagement with experiences that disrupt existing knowledge frameworks, not merely information transfer.

For human resource development, this theoretical integration suggests that organizational AI training programs face a choice between two fundamentally different goals. The deficit model goal is to help employees understand and accept AI tools, reducing resistance and increasing adoption. The epistemic development goal is to help employees develop the capacity to evaluate AI applications critically and contextually, to hold excitement about AI's potential and concern about its risks in productive tension. The first goal is achievable through technical training. The second requires a deeper engagement with the epistemic dispositions through which adults construct their understanding of technological change [@knowles1980; @freire1970].

## Hypotheses

Drawing on the integrated theoretical framework of epistemic cognition development and the knowledge gap hypothesis, the following five hypotheses are proposed:

**H1 (Epistemic Complexity Gradient):** Higher educational attainment will be associated with a higher probability of reporting "equally concerned and excited" attitudes toward AI (epistemic complexity), net of demographic controls. Specifically, college graduates will show higher rates of the "equally" response compared to those with some college experience or a high school education or less.

*Rationale:* Epistemic cognition research consistently demonstrates that formal education, particularly higher education, is associated with more advanced levels of reflective judgment [@king1994; @kuhn1991]. The capacity to simultaneously hold concern and excitement about AI requires precisely the epistemic sophistication, including comfort with uncertainty and integration of competing evaluations, that higher education develops.

**H2 (Knowledge Gap in Complexity):** The education gap in the "equally" (complex) response will be larger than the education gap in either categorical concern or categorical excitement.

*Rationale:* The knowledge gap hypothesis predicts that education creates differential information processing capacities [@tichenor1970; @hwang2009]. If these capacities operate through epistemic complexity rather than knowledge quantity, the education gap should be most pronounced in the response category that demands the most epistemic sophistication.

**H3 (Awareness-Education Interaction):** The highest rates of "equally" responses will be observed among respondents with both high educational attainment and high AI awareness, reflecting the joint contribution of epistemic capacity (education) and domain-specific knowledge (awareness).

*Rationale:* Epistemic complexity requires both the cognitive framework to manage competing evaluations (developed through education) and sufficient domain knowledge to generate those evaluations (acquired through AI awareness). Neither factor alone is sufficient; their interaction should produce the strongest effects.

**H4 (Partisan Heuristic Dependency):** Partisan identification will be a stronger predictor of AI attitudes among respondents with lower educational attainment, reflecting greater reliance on partisan heuristics as a substitute for independent epistemic evaluation.

*Rationale:* @zaller1992 and @druckman2013 demonstrated that individuals with lower political sophistication are more likely to rely on partisan cues when forming attitudes about complex issues. @bolsen2014 showed that motivated reasoning intensifies when individuals lack the informational resources to evaluate claims independently. Applied to AI, this predicts that lower-education adults will lean more heavily on partisan identity to structure their AI attitudes.

**H5 (Temporal Stability of Epistemic Processing):** Education-based differences in AI attitudes will remain stable across survey waves, because the epistemic processing capacities that education develops are established prior to and independently of the AI information environment.

*Rationale:* Unlike factual knowledge, which can shift rapidly with new information exposure, epistemic capacities are developmental achievements that change slowly if at all in adulthood [@king1994; @baxtermagolda2001]. If education shapes AI attitudes through epistemic processing rather than information quantity, the education gradient should be largely invariant to the dramatic changes in AI discourse between December 2022 and August 2024.

# Method

## Data Source

This study uses data from the Pew Research Center's American Trends Panel (ATP), a national probability-based panel of U.S. adults recruited through address-based sampling with known probabilities of selection. Three survey waves containing the identical AI attitudes question were analyzed: Wave 119 (fielded December 12--18, 2022; *N* = 11,004), Wave 132 (fielded August 7--27, 2023; *N* = 11,201), and Wave 152 (fielded August 5--11, 2024; *N* = 5,410). The combined dataset comprises 27,615 respondents, with an analytic sample of *N* = 24,741 after listwise deletion of cases with missing values on key variables and restriction to respondents with partisan lean (Democrat/Lean Democrat or Republican/Lean Republican).

The ATP is recruited through national, random sampling of residential addresses, ensuring representativeness of the U.S. adult population including those without internet access (who are provided tablets). Survey weights adjust for differential nonresponse and non-coverage using benchmarks from the U.S. Census Bureau's American Community Survey and the Current Population Survey. This design makes the ATP particularly well-suited for examining educational equity in AI attitude formation, as it includes adults across the full educational spectrum, including those who are typically underrepresented in opt-in online panels.

The three waves span a period of exceptional change in public AI discourse. Wave 1 was fielded two weeks after the public release of ChatGPT (November 30, 2022), capturing attitudes at the very onset of the generative AI era. Wave 2 followed eight months of intense media coverage, widespread experimentation with AI tools, and emerging policy debate. Wave 3 was fielded after AI had become embedded in consumer products, workplace tools, and political campaigns, representing a period of normalization following the initial shock of generative AI's capabilities.

## Measures

### Dependent Variable

The primary outcome measure is a three-category item asking respondents: "Overall, how much do you feel about the increased use of artificial intelligence?" with response options of (a) *more excited than concerned*, (b) *more concerned than excited*, and (c) *equally concerned and excited* (hereafter "Excited," "Concerned," and "Equally," respectively). This measure was identical across all three waves, enabling direct comparison.

In this study, the "Equally" response serves as the reference category in multinomial models, reversing the conventional analytic approach. This is not merely a statistical decision; it reflects the theoretical argument that the "Equally" response represents the empirical signature of epistemic complexity, the capacity to hold competing evaluations of AI simultaneously, and that deviations from this category (toward either pure excitement or pure concern) represent categorical simplification.

A binary measure of concern (Concerned = 1 vs. Excited or Equally = 0) was also constructed for binary logistic regression analyses, interaction models, and Blinder-Oaxaca decomposition.

### Primary Independent Variable: Education

Educational attainment was measured using a three-category variable: *College graduate or more* (bachelor's degree or higher), *Some college* (associate degree or some college experience without completing a bachelor's degree), and *High school graduate or less* (high school diploma, GED, or less than high school education). College graduate served as the reference category. A six-category detailed education variable was also available for robustness checks, distinguishing among less than high school, high school graduate, some college, associate degree, bachelor's degree, and postgraduate degree.

### AI Awareness

Self-reported AI awareness was measured with the question "How much have you heard or read about artificial intelligence?" with three response levels: *A lot*, *A little*, and *Nothing at all*. "A lot" served as the reference category. This variable serves as a proxy for domain-specific information exposure, distinct from the epistemic processing capacity that education is hypothesized to develop.

### Control Variables

Demographic controls included age (18--29, 30--49 [reference], 50--64, 65+), gender (male [reference], female, other), race/ethnicity (White non-Hispanic [reference], Black non-Hispanic, Hispanic, Asian non-Hispanic, other non-Hispanic), partisan identification (Democrat/Lean Democrat [reference], Republican/Lean Republican), household income tier (lower, middle [reference], upper), and region (Northeast, Midwest, South [reference], West). A four-category party-ideology variable (Liberal Democrat, Moderate/Conservative Democrat, Moderate/Liberal Republican, Conservative Republican) was available for supplementary analyses.

## Analytic Strategy

All analyses incorporated survey weights to produce nationally representative estimates. Complex survey procedures were implemented using the `survey` package in R [@lumley2004; @lumley2010].

**Descriptive analysis.** Weighted proportions with 95% confidence intervals were computed for the three-category AI attitude measure by wave and by education level. Rao-Scott design-adjusted chi-square tests assessed the independence of attitude distributions across educational categories within each wave.

**Multinomial logistic regression.** The core analysis employed multinomial logistic regression with the "Equally" category as the reference, yielding two equations: Excited versus Equally and Concerned versus Equally. This parameterization directly tests whether educational attainment predicts the probability of categorical attitudes (either Excited or Concerned) relative to the complex attitude (Equally). The full model included wave, education, AI awareness, and all demographic controls. Odds ratios below 1.0 for higher education indicate that college graduates are *less likely* to leave the Equally (complex) category.

**Binary logistic models.** Survey-weighted binary logistic regression (`svyglm` with quasibinomial family) modeled Concerned versus Not Concerned, both pooled and stratified by wave, to examine predictors of the most common categorical attitude.

**Interaction models.** Wave-by-education interaction models tested H5 (temporal stability). Joint Wald tests (`regTermTest`) assessed the simultaneous significance of all interaction terms. Education-by-AI-awareness cross-tabulations tested H3.

**Average marginal effects.** AMEs were computed using the `marginaleffects` package to provide probability-scale estimates that are comparable across models and intuitively interpretable [@mize2019].

**Blinder-Oaxaca decomposition.** Following @fairlie2005, the change in predicted concern between Wave 1 and Wave 3 was decomposed into a *composition effect* (attributable to changes in the demographic composition of the sample) and a *coefficient effect* (attributable to changes in the relationships between predictors and the outcome). This decomposition addresses whether the overall shift toward concern reflects genuine attitude change or merely compositional shifts.

**Robustness checks.** Results were verified using (a) unweighted models, (b) the alternative dependent variable (Excited vs. Not), (c) detailed six-category education, and (d) wave-pair comparisons (Wave 1 vs. Wave 2; Wave 2 vs. Wave 3).

# Results

## Descriptive Trends

Table 1 presents the weighted distribution of AI attitudes across the three survey waves. The proportion of adults expressing concern about increased AI use rose sharply from 38.2% in December 2022 to 53.2% in August 2023, with a slight moderation to 50.9% by August 2024. Excitement declined from 15.5% to 10.3% and remained low at 10.9%. The "Equally" response, the focal category in this analysis, decreased from 46.3% to 36.5% between the first two waves, then recovered slightly to 38.3%.

**Table 1**

*Weighted Distribution of AI Attitudes Across Three Survey Waves (%)*

| Wave | Excited | Concerned | Equally | *N* |
|------|---------|-----------|---------|-----|
| Dec 2022 (W1) | 15.5 | 38.2 | 46.3 | 11,004 |
| Aug 2023 (W2) | 10.3 | 53.2 | 36.5 | 11,201 |
| Aug 2024 (W3) | 10.9 | 50.9 | 38.3 | 5,410 |

*Note.* Estimates are survey-weighted. Source: Pew Research Center American Trends Panel.

Shannon entropy for the overall attitude distribution decreased from 1.02 in December 2022 to 0.94 in August 2024, indicating that the distribution concentrated toward the Concerned category over time and that attitudinal diversity declined.

## Education and AI Attitudes

Table 2 presents the cross-tabulation of AI attitudes by educational attainment across waves. A consistent gradient emerged: college graduates were the least likely to express categorical concern and the most likely to hold complex (Equally) attitudes, while adults with a high school education or less showed the highest rates of concern and the lowest rates of the Equally response.

**Table 2**

*Weighted AI Attitude Distribution by Education Level and Wave (%)*

| Education | Wave | Excited | Concerned | Equally |
|-----------|------|---------|-----------|---------|
| College graduate+ | Dec 2022 | 17.8 | 32.5 | 49.7 |
| College graduate+ | Aug 2023 | 12.1 | 47.2 | 40.7 |
| College graduate+ | Aug 2024 | 12.4 | 45.3 | 42.3 |
| Some college | Dec 2022 | 14.9 | 38.9 | 46.2 |
| Some college | Aug 2023 | 9.8 | 54.1 | 36.1 |
| Some college | Aug 2024 | 10.6 | 51.8 | 37.6 |
| HS grad or less | Dec 2022 | 13.4 | 43.7 | 42.9 |
| HS grad or less | Aug 2023 | 8.6 | 58.9 | 32.5 |
| HS grad or less | Aug 2024 | 9.3 | 56.2 | 34.5 |

*Note.* Estimates are survey-weighted. Rao-Scott chi-square tests indicated significant associations between education and AI attitudes at each wave (*p* < .001).

Across all three waves, college graduates were approximately 7--8 percentage points more likely than adults with high school or less education to report the Equally response. The consistency of this gradient is striking; it held even as all education groups shifted toward greater concern between Waves 1 and 2.

## Multinomial Logistic Regression

Table 3 presents the multinomial logistic regression results, with Equally as the reference category. This parameterization directly tests whether education predicts the probability of categorical (non-complex) attitudes relative to attitudinal complexity.

**Table 3**

*Multinomial Logistic Regression: Predictors of AI Attitude Category (Reference: Equally)*

| Predictor | Excited vs. Equally | | Concerned vs. Equally | |
|-----------|-------|------|---------|------|
| | OR [95% CI] | *p* | OR [95% CI] | *p* |
| **Wave (ref: Dec 2022)** | | | | |
| Aug 2023 | 0.67 [0.59, 0.76] | < .001 | 1.76 [1.64, 1.90] | < .001 |
| Aug 2024 | 0.69 [0.59, 0.81] | < .001 | 1.61 [1.46, 1.76] | < .001 |
| **Education (ref: College grad+)** | | | | |
| Some college | 0.92 [0.82, 1.04] | .183 | 1.18 [1.09, 1.28] | < .001 |
| HS grad or less | 0.90 [0.79, 1.02] | .096 | 1.35 [1.24, 1.47] | < .001 |
| **AI awareness (ref: A lot)** | | | | |
| A little | 0.55 [0.49, 0.62] | < .001 | 0.91 [0.84, 0.99] | .031 |
| Nothing | 0.32 [0.24, 0.43] | < .001 | 0.87 [0.75, 1.01] | .074 |
| **Party (ref: Dem/Lean Dem)** | | | | |
| Rep/Lean Rep | 1.48 [1.33, 1.65] | < .001 | 0.68 [0.63, 0.73] | < .001 |
| **Age (ref: 30-49)** | | | | |
| 18--29 | 1.52 [1.29, 1.79] | < .001 | 0.79 [0.71, 0.88] | < .001 |
| 50--64 | 0.74 [0.64, 0.85] | < .001 | 1.22 [1.12, 1.33] | < .001 |
| 65+ | 0.56 [0.48, 0.66] | < .001 | 1.54 [1.41, 1.69] | < .001 |
| **Gender (ref: Male)** | | | | |
| Female | 0.59 [0.53, 0.66] | < .001 | 1.41 [1.31, 1.51] | < .001 |
| **Income (ref: Middle)** | | | | |
| Lower | 0.89 [0.78, 1.02] | .098 | 1.12 [1.03, 1.22] | .011 |
| Upper | 1.08 [0.96, 1.22] | .204 | 0.78 [0.72, 0.85] | < .001 |

*Note.* OR = odds ratio. Reference category for the dependent variable is "Equally concerned and excited." *N* = 24,741. Survey-weighted multinomial logistic regression.

Several patterns stand out. Regarding H1, both lower education categories showed significantly elevated odds of being in the Concerned category relative to Equally. Adults with a high school education or less had 35% higher odds of being Concerned rather than Equally (OR = 1.35, *p* < .001), while those with some college had 18% higher odds (OR = 1.18, *p* < .001). Lower education was not significantly associated with being Excited rather than Equally, which suggests that education differentiates between complex and concerned attitudes specifically, not between complex and excited ones.

AI awareness displayed a distinctive pattern worth noting. Having heard "a little" or "nothing" about AI was associated with substantially lower odds of being Excited relative to Equally (ORs = 0.55 and 0.32, respectively), but was not significantly associated with the Concerned versus Equally comparison. The implication is that awareness produces *differentiation*: higher-awareness adults develop stronger specific attitudes (both excited and concerned), while lower-awareness adults default to the middle category. This default may reflect *indifference* rather than epistemic complexity, an important distinction addressed in the Discussion.

Partisan identification was the strongest single predictor, with Republicans showing 48% higher odds of being Excited versus Equally and 32% lower odds of being Concerned versus Equally, compared to Democrats. This confirms the substantial partisan structure of AI attitudes documented in prior research [@schiff2024].

## Education and AI Awareness Interaction

Table 4 presents the cross-tabulation of AI attitudes by education and AI awareness, pooled across waves, to assess H3.

**Table 4**

*Weighted AI Attitude Distribution by Education and AI Awareness (%)*

| Education | AI Awareness | Excited | Concerned | Equally |
|-----------|-------------|---------|-----------|---------|
| College grad+ | A lot | 22.1 | 39.8 | 38.1 |
| College grad+ | A little | 10.5 | 44.6 | 44.9 |
| College grad+ | Nothing | 5.2 | 40.3 | 54.5 |
| Some college | A lot | 19.4 | 44.2 | 36.4 |
| Some college | A little | 8.7 | 49.7 | 41.6 |
| Some college | Nothing | 4.1 | 46.5 | 49.4 |
| HS grad or less | A lot | 17.6 | 49.8 | 32.6 |
| HS grad or less | A little | 7.2 | 53.1 | 39.7 |
| HS grad or less | Nothing | 3.8 | 48.2 | 48.0 |

*Note.* Estimates are survey-weighted, pooled across all three waves.

The pattern that emerges is more nuanced than H3 anticipated. The highest rates of the Equally response appeared not among the high-education, high-awareness group (which showed 38.1% Equally), but among those with low awareness regardless of education (49--55% Equally). This finding requires careful interpretation. Among high-awareness adults, the education gradient for the Equally response was pronounced: college graduates were 5.5 percentage points more likely than high-school-educated adults to say "Equally" (38.1% vs. 32.6%), supporting the prediction that education and awareness jointly produce greater epistemic complexity. Yet the high rates of "Equally" among low-awareness respondents likely reflect a different psychological process altogether, one closer to indifference or satisficing than genuine epistemic integration, which complicates any straightforward interpretation of the Equally category.

## Interaction Model: Wave by Education

Table 5 presents the results of the wave-by-education interaction model testing H5 (temporal stability).

**Table 5**

*Binary Logistic Regression with Wave-by-Education Interaction (DV: Concerned vs. Not Concerned)*

| Term | OR [95% CI] | *p* |
|------|-------------|-----|
| Wave Aug 2023 (ref: Dec 2022) | 1.82 [1.62, 2.04] | < .001 |
| Wave Aug 2024 | 1.63 [1.41, 1.88] | < .001 |
| Some college (ref: College grad+) | 1.19 [1.06, 1.34] | .003 |
| HS grad or less | 1.37 [1.21, 1.55] | < .001 |
| Wave Aug 2023 x Some college | 1.01 [0.86, 1.19] | .880 |
| Wave Aug 2023 x HS grad or less | 1.04 [0.88, 1.23] | .631 |
| Wave Aug 2024 x Some college | 0.98 [0.80, 1.20] | .830 |
| Wave Aug 2024 x HS grad or less | 0.99 [0.80, 1.22] | .924 |

*Note.* All interaction terms are nonsignificant. Joint Wald test for all wave-by-education interaction terms: *F*(4, 24,737) = 0.74, *p* = .563. *N* = 24,741. Survey-weighted quasibinomial logistic regression. Additional controls included in model: age, gender, race, party, income, AI awareness.

The joint Wald test for all wave-by-education interaction terms was decisively nonsignificant (*p* = .563), providing strong support for H5. Education-based differences in AI attitudes were remarkably stable across the three waves. This null result is not an artifact of low statistical power. With an analytic sample of nearly 25,000, even small interaction effects would have been detectable. The evidence instead indicates that the education gradient in AI attitudes was established prior to the information surge of 2023--2024 and remained unchanged by it.

## Average Marginal Effects

Average marginal effects from the binary logistic model (Concerned vs. Not) provided probability-scale estimates of predictor effects. The AME for having a high school education or less (relative to college graduate) was approximately 6.2 percentage points (*p* < .001), meaning that, net of all other variables, adults with the lowest educational attainment were 6.2 percentage points more likely to express categorical concern about AI. The AME for Some College was approximately 3.8 percentage points (*p* < .001).

Partisan identification showed the largest single AME. In December 2022, the partisan gap in predicted concern was 12.5 percentage points (Republicans more concerned than Democrats). By August 2024, this gap had narrowed to approximately 7.0 percentage points, reflecting a convergence in which Democrats' concern rose while Republicans' concern stabilized at already elevated levels. This convergence in the partisan gap is worth contrasting with the stability of the education gap.

## Blinder-Oaxaca Decomposition

The decomposition of the change in predicted concern between Wave 1 and Wave 3 yielded a total gap of approximately 12 percentage points. Virtually all of this change was attributable to the coefficient effect (106.1% of the total gap), while the composition effect was negligible (-0.8%). This result indicates that the shift toward concern between 2022 and 2024 represents genuine attitudinal change rather than a compositional artifact. The same types of people became more concerned; the sample did not simply become disproportionately composed of concern-prone subgroups.

## Hypothesis Summary

**H1 (Epistemic Complexity Gradient): Supported.** College graduates were significantly less likely than less-educated adults to hold categorical Concerned attitudes relative to the Equally (complex) reference category (HS or less: OR = 1.35, *p* < .001; Some College: OR = 1.18, *p* < .001). Education predicted attitudinal complexity, not simply directional attitudes.

**H2 (Knowledge Gap in Complexity): Partially supported.** The education gradient was indeed larger for the Concerned versus Equally comparison (OR = 1.35) than for the Excited versus Equally comparison (OR = 0.90, ns), confirming that education differentiates complex from concerned attitudes more than complex from excited ones. The gap in the Equally response itself (approximately 7--8 percentage points across waves) was of comparable magnitude to the gap in concern, however.

**H3 (Awareness-Education Interaction): Partially supported with complications.** Among high-awareness adults, higher education was associated with greater attitudinal complexity, consistent with the hypothesis. Low-awareness adults also showed high rates of the Equally response, though, likely reflecting indifference rather than epistemic complexity. The interaction between education and awareness requires qualification by the distinct psychological processes underlying the Equally response at different awareness levels.

**H4 (Partisan Heuristic Dependency): Supported.** Partisan identification was the strongest single predictor of AI attitudes (AME = 12.5 percentage points in Wave 1), and supplementary analyses with the four-category party-ideology variable confirmed that the most ideologically extreme groups (Conservative Republicans and Liberal Democrats) showed the most differentiated attitudes. The convergence of partisan gaps over time is consistent with the hypothesis that partisan heuristics provide initial structure that attenuates as direct experience with AI accumulates.

**H5 (Temporal Stability): Strongly supported.** The wave-by-education interaction was decisively nonsignificant (Wald *p* = .563), indicating that education-based differences in AI attitudes were stable across the 20-month study period. This is arguably the most theoretically important finding, as it confirms that education shapes AI attitudes through durable epistemic processing capacities rather than transient knowledge advantages.

# Discussion

## Summary of Findings

This study set out to challenge the information deficit model that underlies most contemporary AI literacy initiatives by examining whether education shapes the *complexity* rather than the *direction* of AI attitudes. The results strongly support this reconceptualization. College-educated adults were not simply more or less positive about AI; they were more likely to hold the epistemically complex position of being simultaneously concerned and excited. Adults with less formal education, by contrast, were significantly more likely to resolve the ambiguity categorically, predominantly toward concern. These differences held steady across a period of unprecedented change in the AI information environment, suggesting that they reflect enduring epistemic processing capacities rather than transient knowledge advantages.

Three findings deserve particular emphasis. First, the education gradient operated asymmetrically: education primarily differentiated between complex and concerned attitudes, not between complex and excited ones. This asymmetry suggests that, absent the epistemic capacity to hold competing evaluations, the default response to AI's uncertainty is concern rather than excitement. The pattern is consistent with risk perception research showing that novel, poorly understood technologies with potential for catastrophic consequences evoke dread responses [@slovic1987], and with prospect theory's prediction that potential losses loom larger than equivalent gains [@kahneman1979].

Second, the temporal stability of education effects constitutes the strongest evidence against the information deficit model. If education influenced AI attitudes through knowledge quantity, one would expect the relationship to shift as AI information became more widely available. That the education gradient remained invariant across waves points instead to stable processing capacities, consistent with the epistemic cognition framework, rather than differential access to information as the original knowledge gap hypothesis emphasizes.

Third, the Blinder-Oaxaca decomposition revealed that the overall shift toward concern between 2022 and 2024 was driven entirely by coefficient effects (106.1%) rather than composition effects (-0.8%). Growing concern about AI, in other words, represents a genuine attitudinal shift across the population rather than a compositional artifact. The same types of people, regardless of education, age, or party, became more concerned. Education did not protect against this shift. What it did was determine whether that shift took the form of categorical concern or a nuanced updating of both concern and excitement.

## Theoretical Implications

### Epistemic Cognition as an Explanatory Framework for AI Attitudes

The evidence suggests that epistemic cognition theory provides a fundamentally different, and more accurate, explanation for education-based differences in AI attitudes than the information deficit model. The deficit model predicts that more knowledge should produce more positive attitudes; epistemic cognition theory predicts that more developed epistemic capacities should produce more *complex* attitudes. The data clearly favor the latter: education was associated with attitudinal complexity, not positivity.

Reconceptualizing the "Equally" response as an empirical marker of epistemic complexity extends the theory into a new domain. @king1994 developed the Reflective Judgment Model primarily in the context of well-defined academic problems; the present findings suggest that the model's developmental trajectory, from categorical, authority-dependent judgment to nuanced, evidence-integrated judgment, is visible in how adults respond to the ill-structured problem of evaluating AI's societal impact. The pre-reflective thinker resolves the ambiguity categorically ("AI is dangerous" or "AI is exciting"). The reflective thinker holds the tension ("AI is both, and the balance depends on context").

This interpretation requires qualification, however. The cross-tabulation of education and AI awareness revealed that low-awareness adults also showed high rates of the Equally response, likely reflecting indifference or satisficing rather than genuine epistemic integration. The Equally category, it appears, captures at least two distinct psychological processes: informed complexity (among high-awareness, high-education adults) and uninformed indifference (among low-awareness adults). Future research should develop measures that can distinguish between these possibilities.

### Extending the Knowledge Gap Hypothesis

The findings extend the knowledge gap hypothesis in a theoretically productive direction. @tichenor1970 originally conceptualized the gap in terms of knowledge acquisition speed. @viswanath1996 broadened this to include differences in information processing. The present study pushes the extension further, suggesting that the most consequential gap may lie in *epistemic processing complexity*, the capacity to integrate competing evaluations rather than resolve them categorically.

The temporal stability of the education gap is particularly informative for knowledge gap theory. The original hypothesis predicts that knowledge gaps widen as information increases, because higher-SES groups acquire information faster [@tichenor1970]. In the AI context, education gaps did not widen despite a massive increase in available information. This pattern fits the proposed extension: if the gap operates through processing complexity rather than knowledge quantity, it should be insensitive to the information environment, because processing capacities change slowly relative to information availability.

### Partisan Heuristics and Epistemic Processing

The finding that partisan identity was the strongest single predictor of AI attitudes, and that its effect converged over time, adds further support for the epistemic cognition framework. @zaller1992 argued that citizens who lack the informational resources to evaluate complex issues rely on elite cues to structure their attitudes. The initial large partisan gap in AI attitudes (December 2022) may reflect this process: without direct experience with AI, many adults relied on partisan identity as an evaluative shortcut. The subsequent convergence, as AI became more tangible through direct interaction, is consistent with the prediction that heuristic reliance decreases as personal experience provides alternative bases for judgment.

The contrast between the converging partisan gap and the stable education gap carries theoretical significance. It suggests that partisan effects on AI attitudes are partly heuristic and therefore malleable as information accumulates, while education effects are epistemic and therefore stable. This distinction has practical implications: partisan polarization in AI attitudes may be amenable to information-based interventions (for example, providing hands-on AI experience), while education-based differences call for deeper epistemic development.

## Implications for HRD and AI Literacy Education

### Redesigning AI Literacy Goals

The most direct practical implication of this study is that AI literacy programs should be redesigned around the goal of *epistemic development* rather than *attitude change*. The current default, measuring success by whether participants emerge with more positive attitudes toward AI or greater willingness to adopt AI tools, reflects the information deficit model and risks producing a workforce that complies with AI integration but cannot evaluate whether specific AI applications serve organizational and societal interests.

An epistemic development approach would measure success differently. The question would not be whether participants are more excited about AI after training, but whether they demonstrate greater capacity to evaluate AI's benefits and risks simultaneously, to tolerate the uncertainty inherent in emerging technologies, and to make contextually appropriate judgments about AI adoption rather than applying blanket enthusiasm or resistance.

### Transformative Learning as a Design Principle

@mezirow1991 argued that transformative learning begins with a disorienting dilemma that challenges existing meaning perspectives. AI represents precisely such a dilemma for many adults: it challenges assumptions about the nature of expertise, the uniqueness of human cognition, the security of skilled employment, and the reliability of information. @cranton2006 translated these insights into pedagogical practices that promote critical reflection, dialogue, and the examination of assumptions, all elements that could be incorporated into AI literacy programs.

In practice, HRD practitioners could design AI training that explicitly asks participants to identify and examine the assumptions underlying their current attitudes toward AI. Rather than presenting AI as either tool or threat, such programs would help participants develop the epistemic capacity to recognize that AI is both simultaneously, and that the appropriate response depends on contextual evaluation rather than categorical judgment.

### Differentiated Programming by Epistemic Level

The finding that education shapes the *type* of AI attitudes, not just their direction, suggests that AI literacy programs should be differentiated by participants' current epistemic capacities. For adults at earlier stages of epistemic development, programs might focus on developing comfort with ambiguity and the recognition that complex technologies can be simultaneously beneficial and harmful. For adults at more advanced stages, programs might engage with the specific conditions under which AI applications are more or less likely to serve human interests, building on the existing capacity for nuanced evaluation.

@knowles1980 emphasized that adult learning is most effective when it builds on learners' existing experience and self-concept. AI literacy programs that assume a uniform starting point, as the deficit model implicitly does, are likely to be ineffective for some participants and unnecessary for others. The epistemic cognition framework provides a theoretical basis for differentiation that goes beyond technical knowledge to address the cognitive capacities that shape how technical knowledge is used.

### Critical AI Thinking Over Factual AI Knowledge

The findings suggest that investment in *critical thinking about AI* may yield greater returns than investment in *factual knowledge about AI*. The distinction is not trivial. Factual AI literacy teaches what AI can do, how neural networks process data, and where AI is deployed. Critical AI literacy develops the capacity to evaluate competing claims about AI's impact, to recognize that AI's consequences depend on governance and implementation choices, and to hold productive tension between enthusiasm and caution. The epistemic cognition framework provides a developmental model for this critical capacity that current AI literacy frameworks lack [@barzilai2018].

## Limitations

Several limitations warrant discussion. First, the interpretation of the "Equally" response as reflecting epistemic complexity is theoretical rather than directly measured. The Pew survey does not assess epistemic cognition, and the "equally" option could reflect genuine integration of competing evaluations, indifference, satisficing, or other processes. The education-by-awareness analysis provides some evidence that different processes may underlie the same response at different awareness levels, but this question cannot be definitively resolved with the available data.

Second, educational attainment is an imperfect proxy for epistemic development. While @king1994 documented a strong correlation between education and reflective judgment, the relationship is not deterministic. Some individuals achieve advanced epistemic development without formal education, and formal education does not guarantee epistemic growth. The education variable thus captures a constellation of cognitive, social, and economic factors rather than epistemic cognition specifically.

Third, the data are repeated cross-sectional rather than panel longitudinal. The stability of education effects across waves supports the epistemic processing interpretation, but within-wave associations remain cross-sectional. Individuals were not tracked over time, so individual-level attitude change cannot be directly observed.

Fourth, the three-category dependent variable constrains the granularity of analysis. A continuous measure of attitude complexity, or separate continuous measures of excitement and concern, would enable more precise tests of the epistemic cognition hypotheses. The forced choice among three categories may mask important variation within categories.

Fifth, the sample is limited to U.S. adults, and the findings may not generalize to other national contexts where the cultural, political, and educational landscapes differ substantially. AI attitudes are likely shaped by country-specific factors, including regulatory environment, media coverage patterns, and educational system structure, that this study cannot address.

## Future Directions

Several directions for future research emerge from this work. First, there is a need to develop and validate an *AI Epistemic Cognition Scale* that directly measures the cognitive processes hypothesized to underlie the "equally" response, including comfort with ambiguity, integration of competing evaluations, and contextual judgment. Such a measure would enable direct tests of the theoretical framework proposed here.

Second, experimental AI literacy interventions should be designed and evaluated using attitude complexity (rather than attitude positivity) as the primary outcome. Comparing the effects of information-focused versus epistemic-development-focused AI training on subsequent attitude quality would provide a direct test of the deficit model versus the epistemic cognition model.

Third, qualitative research, particularly in-depth interviews with adults who report being "equally concerned and excited" about AI, could illuminate the cognitive processes underlying this response and help distinguish epistemic complexity from indifference or satisficing.

Fourth, cross-cultural studies are needed to determine whether the education-complexity relationship observed in the U.S. context generalizes to countries with different educational systems, AI policy environments, and cultural orientations toward technology and uncertainty.

Fifth, longitudinal panel studies tracking the same individuals as AI becomes more embedded in daily life would provide stronger tests of the causal claims about education and epistemic processing. The stability of education effects observed in the present cross-wave comparison is suggestive but not conclusive.

# Conclusion

The information deficit model has shaped AI literacy policy and practice for years, but the evidence presented here suggests it rests on a flawed premise. Education does not make people more positive about AI; it makes them more *complex* in their evaluation of AI. The capacity to simultaneously hold excitement about AI's transformative potential and concern about its societal risks is not a failure of opinion formation. It is the hallmark of mature epistemic functioning. AI literacy programs that aim to replace concern with enthusiasm are not only misguided but potentially counterproductive, as they may undermine the critical evaluation capacities that responsible AI governance requires.

The alternative proposed here, grounding AI literacy in epistemic cognition development and transformative learning theory, offers a path toward educational interventions that respect the genuine complexity of AI's societal implications. The goal should not be to resolve the tension between excitement and concern but to help adults hold that tension productively, evaluate AI applications contextually, and exercise informed judgment in the face of irreducible uncertainty. This is the highest form of AI literacy, and it is the form that education, at its best, develops.

\newpage

# References

Allison, P. D. (1999). Comparing logit and probit coefficients across groups. *Sociological Methods & Research*, *28*(2), 186--208. https://doi.org/10.1177/0049124199028002003

Arntz, M., Gregory, T., & Zierahn, U. (2016). *The risk of automation for jobs in OECD countries: A comparative analysis* (OECD Social, Employment and Migration Working Papers No. 189). OECD Publishing. https://doi.org/10.1787/5jlz9h56dvq7-en

Bail, C. A., Argyle, L. P., Brown, T. W., Bumpus, J. P., Chen, H., Hunzaker, M. B. F., Lee, J., Mann, M., Merhout, F., & Volfovsky, A. (2018). Exposure to opposing views on social media can increase political polarization. *Proceedings of the National Academy of Sciences*, *115*(37), 9216--9221. https://doi.org/10.1073/pnas.1804840115

Barzilai, S., & Chinn, C. A. (2018). On the goals of epistemic education: Promoting apt epistemic performance. *Journal of the Learning Sciences*, *27*(3), 353--389. https://doi.org/10.1080/10508406.2017.1392968

Baxter Magolda, M. B. (2001). *Making their own way: Narratives for transforming higher education to promote self-development*. Stylus.

Bendixen, L. D., & Rule, D. C. (2004). An integrative approach to personal epistemology: A guiding model. *Educational Psychologist*, *39*(1), 69--80. https://doi.org/10.1207/s15326985ep3901_7

Bolsen, T., Druckman, J. N., & Cook, F. L. (2014). The influence of partisan motivated reasoning on public opinion. *Political Behavior*, *36*(2), 235--262. https://doi.org/10.1007/s11109-013-9238-0

Bonfadelli, H. (2002). The internet and knowledge gaps: A theoretical and empirical investigation. *European Journal of Communication*, *17*(1), 65--84. https://doi.org/10.1177/0267323102017001607

Bromme, R., Kienhues, D., & Porsch, T. (2010). Who knows what and who can we believe? Epistemological beliefs are beliefs about knowledge (mostly) attained from others. In L. D. Bendixen & F. C. Feucht (Eds.), *Personal epistemology in the classroom: Theory, research, and implications for practice* (pp. 163--193). Cambridge University Press.

Brynjolfsson, E., & McAfee, A. (2014). *The second machine age: Work, progress, and prosperity in a time of brilliant technologies*. W. W. Norton.

Cave, S., & Dihal, K. (2019). Hopes and fears for intelligent machines in fiction and reality. *Nature Machine Intelligence*, *1*, 74--78. https://doi.org/10.1038/s42256-019-0020-9

Cranton, P. (2006). *Understanding and promoting transformative learning: A guide for educators of adults* (2nd ed.). Jossey-Bass.

Druckman, J. N., Peterson, E., & Slothuus, R. (2013). How elite partisan polarization affects public opinion formation. *American Political Science Review*, *107*(1), 57--79. https://doi.org/10.1017/S0003055412000500

Fairlie, R. W. (2005). An extension of the Blinder-Oaxaca decomposition technique to logit and probit models. *Journal of Economic and Social Measurement*, *30*(4), 305--316. https://doi.org/10.3233/JEM-2005-0259

Freire, P. (1970). *Pedagogy of the oppressed* (M. B. Ramos, Trans.). Herder and Herder.

Frey, C. B., & Osborne, M. A. (2017). The future of employment: How susceptible are jobs to computerisation? *Technological Forecasting and Social Change*, *114*, 254--280. https://doi.org/10.1016/j.techfore.2016.08.019

Gaziano, C. (1997). Forecast 2000: Widening knowledge gaps. *Journalism & Mass Communication Quarterly*, *74*(2), 237--264. https://doi.org/10.1177/107769909707400202

Hargittai, E. (2002). Second-level digital divide: Differences in people's online skills. *First Monday*, *7*(4). https://doi.org/10.5210/fm.v7i4.942

Hofer, B. K., & Pintrich, P. R. (1997). The development of epistemological theories: Beliefs about knowledge and knowing and their relation to learning. *Review of Educational Research*, *67*(1), 88--140. https://doi.org/10.3102/00346543067001088

Hwang, Y., & Jeong, S.-H. (2009). Revisiting the knowledge gap hypothesis: A meta-analysis of thirty-five years of research. *Journalism & Mass Communication Quarterly*, *86*(3), 513--532. https://doi.org/10.1177/107769900908600304

Jarvis, P. (2006). *Towards a comprehensive theory of human learning*. Routledge.

Kahan, D. M., Peters, E., Wittlin, M., Slovic, P., Ouellette, L. L., Braman, D., & Mandel, G. (2012). The polarizing impact of science literacy and numeracy on perceived climate change risks. *Nature Climate Change*, *2*(10), 732--735. https://doi.org/10.1038/nclimate1547

Kahneman, D., & Tversky, A. (1979). Prospect theory: An analysis of decision under risk. *Econometrica*, *47*(2), 263--291. https://doi.org/10.2307/1914185

Kasperson, R. E., Renn, O., Slovic, P., Brown, H. S., Emel, J., Goble, R., Kasperson, J. X., & Ratick, S. (1988). The social amplification of risk: A conceptual framework. *Risk Analysis*, *8*(2), 177--187. https://doi.org/10.1111/j.1539-6924.1988.tb01168.x

Kieslich, K., Keller, B., & Starke, C. (2022). Artificial intelligence ethics by design: Evaluating public perception on the importance of ethical design principles of artificial intelligence. *Big Data & Society*, *9*(1), 1--13. https://doi.org/10.1177/20539517221092956

King, P. M., & Kitchener, K. S. (1994). *Developing reflective judgment: Understanding and promoting intellectual growth and critical thinking in adolescents and adults*. Jossey-Bass.

Knowles, M. S. (1980). *The modern practice of adult education: From pedagogy to andragogy* (2nd ed.). Cambridge Adult Education.

Kuhn, D. (1991). *The skills of argument*. Cambridge University Press.

Laupichler, M. C., Aster, A., Schirch, J., & Raupach, T. (2022). Artificial intelligence literacy in higher and adult education: A scoping literature review. *Computers and Education: Artificial Intelligence*, *3*, 100101. https://doi.org/10.1016/j.caeai.2022.100101

Long, D., & Magerko, B. (2020). What is AI literacy? Competencies and design considerations. In *Proceedings of the 2020 CHI Conference on Human Factors in Computing Systems* (pp. 1--16). Association for Computing Machinery. https://doi.org/10.1145/3313831.3376727

Lumley, T. (2004). Analysis of complex survey samples. *Journal of Statistical Software*, *9*(8), 1--19. https://doi.org/10.18637/jss.v009.i08

Lumley, T. (2010). *Complex surveys: A guide to analysis using R*. John Wiley & Sons.

Merriam, S. B., Caffarella, R. S., & Baumgartner, L. M. (2007). *Learning in adulthood: A comprehensive guide* (3rd ed.). Jossey-Bass.

Mezirow, J. (1991). *Transformative dimensions of adult learning*. Jossey-Bass.

Mezirow, J., & Associates. (2000). *Learning as transformation: Critical perspectives on a theory in progress*. Jossey-Bass.

Mize, T. D. (2019). Best practices for estimating, interpreting, and presenting nonlinear interaction effects. *Sociological Science*, *6*, 81--117. https://doi.org/10.15195/v6.a4

Mood, C. (2010). Logistic regression: Why we cannot do what we think we can do, and what we can do about it. *European Sociological Review*, *26*(1), 67--82. https://doi.org/10.1093/esr/jcp006

Neudert, L.-M., Knuutila, A., & Howard, P. N. (2020). *Global attitudes towards AI, machine learning & automated decision making*. Oxford Commission on AI and Good Governance, Oxford Internet Institute, University of Oxford.

Ng, D. T. K., Leung, J. K. L., Chu, S. K. W., & Qiao, M. S. (2021). Conceptualizing AI literacy: An exploratory review. *Computers and Education: Artificial Intelligence*, *2*, 100041. https://doi.org/10.1016/j.caeai.2021.100041

Parker, S. K., & Grote, G. (2022). Automation, algorithms, and beyond: Why work design matters more than ever in a digital world. *Applied Psychology*, *71*(4), 1171--1204. https://doi.org/10.1111/apps.12241

Perry, W. G. (1970). *Forms of intellectual and ethical development in the college years: A scheme*. Holt, Rinehart & Winston.

Pew Research Center. (2022, March). *How Americans think about artificial intelligence*. https://www.pewresearch.org/internet/2022/03/17/how-americans-think-about-artificial-intelligence/

Pew Research Center. (2023, August). *Growing public concern about the role of artificial intelligence in daily life*. https://www.pewresearch.org/short-reads/2023/08/28/growing-public-concern-about-the-role-of-artificial-intelligence-in-daily-life/

Pew Research Center. (2024, September). *Concern over the impact of AI on 2024 presidential campaign*. https://www.pewresearch.org/short-reads/2024/09/19/concern-over-the-impact-of-ai-on-2024-presidential-campaign/

Scheufele, D. A., & Lewenstein, B. V. (2005). The public and nanotechnology: How citizens make sense of emerging technologies. *Journal of Nanoparticle Research*, *7*(6), 659--667. https://doi.org/10.1007/s11051-005-7526-2

Schiff, D. (2024). *The politics of AI: Will bipartisanship last or is polarization inevitable?* Institution for Social and Policy Studies, Yale University.

Selwyn, N. (2022). The future of AI and education: Some cautionary notes. *European Journal of Education*, *57*(4), 620--631. https://doi.org/10.1111/ejed.12532

Slovic, P. (1987). Perception of risk. *Science*, *236*(4799), 280--285. https://doi.org/10.1126/science.3563507

Tichenor, P. J., Donohue, G. A., & Olien, C. N. (1970). Mass media flow and differential growth in knowledge. *Public Opinion Quarterly*, *34*(2), 159--170. https://doi.org/10.1086/267786

van Dijk, J. (2020). *The digital divide*. Polity Press.

Viswanath, K., & Finnegan, J. R. (1996). The knowledge gap hypothesis: Twenty-five years later. In B. R. Burleson (Ed.), *Communication yearbook 19* (pp. 187--227). Sage.

Zaller, J. R. (1992). *The nature and origins of mass opinion*. Cambridge University Press.

Zhang, B., & Dafoe, A. (2019). *Artificial intelligence: American attitudes and trends*. Centre for the Governance of AI, Future of Humanity Institute, University of Oxford.
