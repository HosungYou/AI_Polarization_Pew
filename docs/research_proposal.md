# Diverging Paths: A Repeated Cross-Sectional Analysis of AI Attitude Polarization in the United States, 2022–2024

---

## Abstract

As artificial intelligence (AI) becomes increasingly embedded in organizational processes and everyday life, scholarly and public discourse has predominantly characterized shifts in public sentiment as uniform trends toward either enthusiasm or apprehension. This framing, however, obscures a more consequential phenomenon: AI attitudes are not merely shifting—they are *polarizing*. The present study addresses this gap by conducting the first systematic, multi-wave analysis of AI attitude polarization using three waves of nationally representative data from the Pew Research Center's American Trends Panel (ATP): Wave 119 (December 2022, *N* = 11,004), Wave 132 (August 2023, *N* = 11,201), and Wave 152 (August 2024, *N* = 5,410). We leverage a critical natural experiment afforded by this data: the December 2022 wave was fielded in the same month ChatGPT launched, Wave 132 captured public opinion eight months into the generative AI revolution, and Wave 152 provides a longer-run view as initial media salience faded. The primary dependent variable—CNCEXC, which asks whether AI's increasing use makes respondents feel more excited, more concerned, or equally concerned and excited—reveals a striking pattern: the share expressing net concern rose from 37.6% to 53.2% between December 2022 and August 2023, while the "equally" category collapsed from 45.9% to 36.5%, with partial reversion by August 2024 (49.4% concerned, 38.7% equally). We integrate the Social Amplification of Risk Framework (SARF; Kasperson et al., 1988) with the Knowledge Gap Hypothesis (Tichenor et al., 1970) to theorize why different demographic subpopulations responded so differently to the same informational environment. Employing multinomial and binary logistic regression with survey-weighted data and cross-wave interaction terms, we test whether political affiliation, educational attainment, and AI awareness predict attitude category membership, and whether the demographic composition of these categories changed across waves. Our findings contribute to the management and information systems literatures by identifying which segments of the American public are driving aggregate polarization trends, and by establishing that the collapse of the ambivalent "equally concerned and excited" category represents a meaningful instance of attitude crystallization rather than mere opinion drift. We discuss implications for organizational AI adoption strategies, workforce communication, and the design of public policy interventions that account for attitudinal heterogeneity rather than assuming a monolithic public response to technological change.

**Keywords:** artificial intelligence attitudes, attitude polarization, Pew American Trends Panel, social amplification of risk, knowledge gap, repeated cross-sections, ChatGPT

---

## 1. Introduction

Artificial intelligence is reshaping the landscape of work, organizations, and society with a speed and scope that have few historical parallels. The release of ChatGPT in November 2022 catalyzed a phase transition in public engagement with AI: the application reached 100 million monthly active users within two months, making it the fastest-adopted consumer technology in history (Hu, 2023). Organizations across industries responded with urgency, accelerating investments in generative AI, redesigning workflows, and confronting fundamental questions about the future of human labor (Brynjolfsson & McAfee, 2014; Raisch & Krakowski, 2021). Governments scrambled to develop regulatory frameworks, and workers at every level of the occupational hierarchy now face the prospect that AI systems may augment, displace, or fundamentally transform their roles (Acemoglu & Restrepo, 2020; Autor, 2015; Davenport & Kirby, 2016). The magnitude and velocity of this transformation make understanding public attitudes toward AI not merely an academic curiosity but an organizational and societal imperative.

Yet the prevailing narrative about how the public is responding to AI obscures more than it reveals. Media accounts and policy discussions tend to frame the story in simple directional terms: Americans are "growing more worried" about AI, or alternatively, "embracing" AI at accelerating rates. Scholarly research has largely reinforced this framing by reporting mean-level shifts in AI attitudes across representative samples, treating the population as a single distribution that moves in a unified direction over time. This approach, while informative at the aggregate level, masks what we argue is the more consequential phenomenon: public attitudes toward AI are not uniformly shifting in one direction—they are *polarizing*, with the middle ground of ambivalence eroding as individuals are pushed toward more definite affective positions.

The empirical evidence for this polarization is striking. Data from the Pew Research Center's American Trends Panel (ATP) reveal that the share of Americans expressing more concern than excitement about AI in daily life rose from 37.6% in December 2022 to 53.2% in August 2023—a 15.6 percentage-point shift over eight months. Simultaneously, the share expressing nearly equal concern and excitement fell from 45.9% to 36.5%, and the share describing themselves as primarily excited collapsed from 15.7% to just 9.2%. By August 2024, a partial moderation was visible: 49.4% remained predominantly concerned, 38.7% equally concerned and excited, and 11.3% primarily excited. This temporal pattern—sharp polarization following the ChatGPT launch, with modest reversion as initial media salience faded—constitutes a natural experiment of unusual richness for studying how a major technological disruption reshapes public opinion.

Three features of this pattern deserve particular theoretical attention. First, the dominant direction of movement is toward concern rather than excitement, suggesting that the ChatGPT launch, despite representing an unprecedented demonstration of AI capability, primarily activated threat responses rather than opportunity responses in the broader public. Second, the erosion of the "equally concerned and excited" category between 2022 and 2023 is consistent with attitude crystallization: individuals who previously held genuinely ambivalent views were pushed by new information and heightened media salience to resolve that ambivalence in the direction of concern. Third, the partial reversion in 2024—where concern retreats modestly and the "equally" group recovers slightly—raises questions about whether polarization is a durable structural feature of AI attitudes or a transient response to acute informational shocks.

Despite the theoretical and practical significance of this pattern, existing research has not systematically examined which demographic subpopulations are driving these aggregate shifts, nor whether the composition of the "concerned," "equally," and "excited" categories changed in demographically meaningful ways across the three waves. If political identity, educational attainment, and prior AI awareness moderate how individuals respond to the same informational environment, then the aggregate trends documented by Pew conceal a more differentiated story of divergence along socially structured lines—a story with direct implications for organizational communication, public policy, and the sociology of technology adoption.

The present study addresses these gaps through three analytic objectives. First, we document the distributional pattern of AI attitudes (the CNCEXC variable) across the three Pew ATP waves, establishing the baseline empirical picture that motivates the polarization interpretation. Second, we examine the demographic predictors of attitude category membership at each wave and across waves, using multinomial logistic regression to identify which groups are disproportionately represented in each attitudinal category and binary logistic regression to identify predictors of the "concerned" versus non-concerned distinction that has attracted most policy attention. Third, we test whether the magnitude of demographic group differences in attitude categories widened between 2022 and 2024—the critical test of polarization as a dynamic process rather than a static disparity—using cross-wave interaction terms (wave × demographic) in pooled models.

To theorize the mechanisms underlying these patterns, we integrate two complementary frameworks. The Social Amplification of Risk Framework (SARF; Kasperson et al., 1988) explains how the ChatGPT launch and subsequent generative AI discourse functioned as an information shock that was amplified differentially across social stations—media outlets, professional networks, peer communities—generating different attitudinal responses in different population segments. The Knowledge Gap Hypothesis (Tichenor et al., 1970) explains who diverges and why: as information about AI proliferated at an unprecedented rate, individuals with higher education, greater digital literacy, and stronger political identity processed that information through distinct cognitive and evaluative frameworks, producing the demographic stratification visible in the data. Integrated, these frameworks yield a theoretical account of AI attitude polarization as the product of differential amplification exposure interacting with differential processing capacity.

This study makes four contributions to the management and information systems literatures. First, we provide the most comprehensive multi-wave analysis of AI attitude polarization using the Pew ATP data—the gold standard of nationally representative public opinion research—documenting a dramatic shift across three critical time points spanning the ChatGPT revolution. Second, we identify which demographic groups are the primary drivers of polarization, moving beyond aggregate trend reporting to a subgroup-differentiated understanding of who is becoming more concerned and who is not. Third, we offer and test the attitude crystallization hypothesis: that the 2022–2023 period represented a significant reduction in population-level ambivalence as the ChatGPT shock resolved previously underdetermined AI attitudes in a predominantly negative direction. Fourth, we establish whether political identity predicts AI attitudes above and beyond education and age—a test that speaks directly to whether AI has joined the growing list of ostensibly technical issues that have been absorbed into American partisan identity.

The remainder of this paper proceeds as follows. We first develop our theoretical framework by integrating SARF and the Knowledge Gap Hypothesis and deriving specific hypotheses about demographic predictors of attitude category membership and cross-temporal divergence. We then describe the Pew ATP data, our key measures, and the analytic approach. Next, we present the results of our multinomial and binary logistic regression models and interaction tests. Finally, we discuss implications for theory, practice, and future research.

---

## 2. Theoretical Background and Hypotheses

### 2.1 AI Attitudes: The CNCEXC Structure and the Problem of Crystallization

Public attitudes toward artificial intelligence have attracted growing scholarly and policy attention as AI systems diffuse across economic and social domains. The Pew Research Center has tracked these attitudes through its American Trends Panel using the CNCEXC item, which asks respondents whether the increased use of AI in daily life makes them feel more excited than concerned, more concerned than excited, or equally concerned and excited. This trichotomous structure is theoretically significant because it preserves the possibility of genuine ambivalence as a distinct attitudinal state, rather than collapsing it into the midpoint of a bipolar scale. Ambivalence—the simultaneous experience of positive and negative evaluations of an object—has been extensively studied in social psychology as a qualitatively distinct attitude structure with different antecedents, correlates, and behavioral consequences than univalent positions (Cacioppo & Berntson, 1994; Thompson, Zanna, & Griffin, 1995; van Harreveld, van der Pligt, & de Liver, 2009).

The three ATP waves reveal a pronounced reduction in the ambivalent "equally concerned and excited" category: 45.9% in December 2022, 36.5% in August 2023, and 38.7% in August 2024. This erosion of the middle—coinciding with the surge in the "more concerned" category—is consistent with what attitude theorists term crystallization: the process by which previously underdetermined or genuinely ambivalent attitudes resolve toward a more definite evaluative position under conditions of heightened information salience and social pressure (Converse, 1964; Zaller, 1992). The ChatGPT launch created exactly these conditions: a sudden, massive increase in AI-related media coverage, peer conversation, and occupational relevance that confronted individuals who had previously held vague or balanced views with new and emotionally charged information.

This crystallization interpretation is consequential for organizations and policymakers because it implies that AI attitudes are becoming structurally less malleable. Once an individual has resolved ambivalence into a definite "more concerned" position, the typical change dynamics are different from those governing an ambivalent individual who has not yet committed—the latter is more persuadable, more responsive to new information, and more open to attitude revision through direct experience. If the 2022–2023 period represents a wave of attitude crystallization predominantly in the negative direction, organizations seeking to build public trust in AI face a more entrenched attitudinal landscape than aggregate concern percentages alone would suggest.

Cross-sectional evidence from Pew surveys (2023, 2024) has documented that AI attitudes are stratified by demographics: younger adults, those with higher formal education, and men tend to express greater excitement, while older adults, women, and those with lower educational attainment report heightened concern. These cross-sectional differences, however, leave open the critical dynamic question: are these demographic gaps widening over time, narrowing, or stable? The distinction matters enormously for the polarization thesis. A finding that demographic gaps in AI concern have widened between 2022 and 2024 would provide strong evidence that AI attitudes are diverging along structurally meaningful social cleavages—that AI has become another domain in which pre-existing social inequalities in information access and processing capacity manifest as attitudinal differentiation. A finding of stable or narrowing gaps would be more consistent with a universal shock model in which all subgroups moved in the same direction at approximately the same rate.

### 2.2 Social Amplification of Risk Framework

The Social Amplification of Risk Framework (SARF), introduced by Kasperson and colleagues (1988) and subsequently elaborated by Kasperson and Kasperson (1996) and Pidgeon, Kasperson, and Slovic (2003), provides a comprehensive theoretical architecture for understanding how risk events interact with psychological, social, institutional, and cultural processes to shape public risk perception and behavioral response. At its core, SARF proposes that risk events generate informational signals—data about the hazard, its probability, and its consequences—that are transmitted, processed, and potentially transformed as they pass through a series of "amplification stations." These stations include individual cognitive and affective processing, interpersonal communication networks, institutional actors, mass media, and broader cultural and political structures. At each station, risk signals may be either amplified—generating heightened concern, mobilization, and behavioral change—or attenuated—reducing perceived severity and dampening response.

The framework identifies several mechanisms through which amplification and attenuation occur. The volume and intensity of information flow influence the salience of a risk in public consciousness. The disputability of risk claims—the degree to which experts disagree or evidence is contested—can amplify uncertainty and concern. Dramatization, particularly through vivid anecdotal evidence or worst-case scenario framing, elevates the affective intensity of risk signals beyond what actuarial data alone would warrant. Symbolic connotations link a novel risk to culturally resonant narratives of threat or progress, shaping interpretation through pre-existing meaning structures. Finally, stigmatization attaches negative social labels to technologies associated with a risk, creating self-reinforcing cycles of avoidance and opposition (Kasperson et al., 1988).

The application of SARF to the ChatGPT moment is both natural and theoretically productive. The November 2022 launch of ChatGPT and the subsequent eight months of intense public discourse represent one of the most dramatic technological amplification events in recent memory. Media coverage of generative AI in this period was markedly bifurcated: one stream emphasized dystopian narratives—mass unemployment, autonomous weapons, algorithmic bias, and existential risk—while the opposing stream highlighted transformative productivity gains and creative augmentation. The net effect, as captured by the Pew ATP Wave 132 data, was a massive shift toward concern: the share of "more concerned" respondents increased by 15.6 percentage points in eight months. Under SARF, this shift reflects not simply an updating of rational risk estimates but the operation of amplification mechanisms—dramatized media coverage, occupational threat salience, and peer network reinforcement—that transformed a diffuse sense of AI unease into crystallized concern.

Critically, SARF predicts that this amplification is not uniform across the population. The same news reports about AI-driven automation reach individuals embedded in different social contexts: those in professional networks that celebrate AI innovation experience the same coverage as attenuating (confirming a benign view of AI's trajectory) rather than amplifying. Those in communities more vulnerable to occupational displacement, or embedded in media ecosystems that foreground risk narratives, experience the coverage as amplifying. This differential amplification by social context—professional network composition, media consumption, occupational exposure—implies that the aggregate shift toward concern conceals substantial heterogeneity in who moved and how far. Demographic variables (education, income, political affiliation, geographic region) serve as proxies for the configuration of amplification stations to which individuals are exposed.

The partial reversion between 2023 and 2024 also fits within the SARF framework. Following the acute phase of ChatGPT media saturation, the volume and novelty of AI risk signals attenuated as audiences habituated and media attention shifted to other topics. SARF would predict this as a natural attenuation cycle: once the initial amplification wave passes, some individuals whose attitudes were transiently crystallized by media intensity return toward their baseline ambivalence, while others who underwent genuine attitude crystallization (rooted in new personal experience or deeper deliberative engagement) maintain their shifted positions. The net result—partial reversion at the aggregate level—is consistent with a mixture of these processes.

### 2.3 The Knowledge Gap Hypothesis

The Knowledge Gap Hypothesis, originally articulated by Tichenor, Donohue, and Olien (1970), proposes that as the infusion of mass media information into a social system increases, higher socioeconomic status (SES) segments of the population tend to acquire information at a faster rate than lower-SES segments, widening rather than narrowing the informational gap between groups. The original formulation identified several mechanisms: higher-SES individuals possess greater prior knowledge bases that facilitate assimilation of new information, more developed communication skills, broader social networks that serve as supplementary information channels, and access to media systems that deliver higher-quality informational content.

We extend the Knowledge Gap Hypothesis from factual knowledge acquisition to attitudinal differentiation regarding artificial intelligence. Our extension rests on the premise that attitudes toward complex emerging technologies are constructed through the effortful processing of risk and benefit information, and that the capacity for such processing is stratified by education and digital literacy. As AI-related information proliferated at an unprecedented rate between 2022 and 2024, individuals with higher education and greater digital literacy were better equipped to locate, comprehend, and critically evaluate this information—to distinguish between specific, evidence-based claims about particular AI applications and generalized, affect-laden narratives about AI as a monolithic force. This differential processing capacity implies that the response to the ChatGPT information shock was not uniform: higher-knowledge individuals were able to form more differentiated, calibrated attitudes, while lower-knowledge individuals relied more heavily on heuristic processing of salient affective cues.

The attitudinal consequence, however, is not simply that higher-education individuals hold more positive attitudes. The empirical pattern is more nuanced: Pew data consistently show that college-educated respondents are somewhat more likely to be in the "excited" category and somewhat less likely to be in the "equally" category, but the gap in the "concerned" category across education groups is smaller than the political gap. Education appears to operate primarily through the "excited" dimension—providing the informational resources and direct exposure that enable appreciation of specific AI benefits—rather than through the "concerned" dimension, which is broadly elevated across education groups. This pattern is consistent with the Knowledge Gap prediction that information campaigns differentially benefit higher-SES groups: when AI capability demonstrations (the benefit-emphasizing information) are prominent in the information environment, higher-education individuals extract more from them, widening the excitement gap.

Political identity introduces an additional mechanism for attitudinal differentiation that goes beyond information processing capacity. The cultural cognition thesis (Kahan, 2012; Kahan, Braman, Gastil, Slovic, & Mertz, 2007) holds that individuals' perceptions of risk are shaped by their cultural worldviews, and that more scientifically literate individuals can exhibit greater, not lesser, cultural polarization on risk issues as they selectively deploy their cognitive resources to defend identity-congruent positions. Applied to AI, this predicts that political polarization in AI attitudes may be as strong as or stronger than education-based polarization—and that it may intensify over time as AI becomes a more politically charged topic.

Partisan media ecosystems in the United States are increasingly distinct (Prior, 2013), with Democratic-leaning and Republican-leaning media offering systematically different frames for emerging technologies. Democratic-aligned discourse tends to foreground equity-focused concerns (algorithmic bias, surveillance) alongside enthusiasm for AI's potential in healthcare and climate. Republican-aligned discourse tends to emphasize threats to individual autonomy, traditional employment, and institutional authority from unaccountable technocratic systems. Under SARF, these partisan media ecosystems function as distinct amplification stations that differentially shape the risk signals received by co-partisans, predicting that party affiliation will be an independent predictor of attitude category membership even after controlling for education, age, and income.

Taken together, SARF and the Knowledge Gap Hypothesis yield an integrated account in which the ChatGPT information shock interacted with individual-level and community-level factors to produce the observed aggregate polarization—and in which the specific pattern of who became more concerned, who remained ambivalent, and who became more excited reflects the configuration of amplification stations and processing capacities available to different demographic subpopulations.

### 2.4 Hypotheses

#### Hypothesis 1: Demographic Predictors of AI Attitude Category

If AI attitudes were uniformly distributed across demographic groups, membership in the "more concerned," "equally concerned and excited," and "more excited" categories would be independent of age, education, gender, race/ethnicity, political affiliation, income, and regional location. The theoretical analysis above provides strong reasons to expect systematic demographic stratification. SARF predicts that individuals embedded in different information environments—which are themselves structured by demographic characteristics—will be exposed to differentially amplified or attenuated AI risk signals. The Knowledge Gap Hypothesis predicts that differential processing capacity, rooted in education and digital literacy, will produce systematically different attitudinal outcomes from the same informational environment.

> **H1**: Demographic characteristics—including age, educational attainment, gender, race/ethnicity, political party affiliation, household income, and region—significantly predict membership in the CNCEXC attitude categories (more excited, equally concerned and excited, more concerned).

#### Hypothesis 2: Political Polarization in AI Attitudes

Political identity is theorized as an independent predictor of AI attitude category membership, operating through partisan media ecosystems that differentially amplify AI risk or benefit signals. Under the cultural cognition thesis, this political gap should persist after controlling for the sociodemographic variables that co-vary with party affiliation.

The direction of the predicted political gap requires specification. The Pew (2023, 2024) data suggest that Republicans are more likely than Democrats to express concern about AI—a pattern that may reflect Republican-aligned media's emphasis on AI as a threat to jobs and traditional ways of life, as well as broader patterns of Republican skepticism toward technology sector actors perceived as politically liberal. Democrats may be more likely to occupy the "excited" or "equally" categories, reflecting enthusiasm for AI's potential in addressing social challenges alongside equity-focused concerns that do not resolve into a net "concerned" orientation.

> **H2**: Political party affiliation independently predicts CNCEXC category membership, with Republicans significantly more likely than Democrats to be in the "more concerned" category, and Democrats significantly more likely to be in the "more excited" category, controlling for education, age, gender, race/ethnicity, income, and region.

#### Hypothesis 3: Knowledge Gap — Education and the Excited Category

The Knowledge Gap Hypothesis predicts that educational attainment will be a positive predictor of membership in the "more excited" category and a negative predictor of the "more concerned" category. This reflects the differential capacity of higher-education individuals to engage with and benefit from AI capability demonstrations, which constitute the primary information basis for AI excitement. The education-concern gradient is expected to be weaker and potentially non-monotonic: even highly educated respondents may express concern, but they may combine it with enough excitement to tip toward the "equally" or "excited" categories.

> **H3**: Higher educational attainment predicts a higher probability of membership in the "more excited" category and a lower probability of membership in the "more concerned" category, with college-educated respondents significantly more likely than those without a college degree to be in the "excited" rather than "concerned" category.

#### Hypothesis 4: Cross-Temporal Divergence — Widening Demographic Gaps

The most critical test of the polarization thesis is dynamic: whether demographic group differences in AI concern widened between December 2022 and August 2024. A static pattern—in which Democrats, Republicans, college-educated, and non-college-educated respondents all moved in the same direction by approximately the same magnitude—would be more consistent with a universal information shock than with polarization as a process of differential divergence. Polarization, properly defined, requires that group differences increase over time.

The Knowledge Gap Hypothesis specifically predicts cumulative divergence: at each successive wave, higher-knowledge individuals acquire and process new AI-related information more effectively, further differentiating their attitudes from those of lower-knowledge individuals. If the 2022–2023 period represented an acute amplification episode that disproportionately activated concern in politically conservative and lower-education populations—those more reliant on mainstream media's dramatized risk narratives and less equipped to counterbalance them with specific capability-appreciation—then the demographic gap in concern should have widened between Wave 119 and Wave 132.

> **H4**: The magnitude of demographic group differences in AI concern widened between December 2022 and August 2024, as evidenced by significant wave × demographic interaction effects (particularly wave × party affiliation and wave × education) in pooled models predicting CNCEXC category membership.

#### Hypothesis 5: AI Awareness and Attitude Crystallization

The Pew ATP includes the AI_HEARD variable, which measures respondents' self-reported awareness of AI and related developments. Under SARF, greater AI awareness reflects greater cumulative exposure to the amplification stations—media, peer networks, direct experience—that shape AI attitudes. Individuals with higher AI awareness have been exposed to more information about AI, which, consistent with the Knowledge Gap Hypothesis, should produce more differentiated attitudes rather than defaulting to the ambivalent "equally concerned and excited" middle.

The specific prediction is that AI awareness is associated with a lower probability of the "equally" category and a higher probability of either the "concerned" or "excited" category—not because awareness predicts a particular direction of attitude, but because awareness reduces ambivalence by providing the informational basis for a more definite attitudinal stance. This crystallization prediction distinguishes the awareness effect from a simple valence effect (more awareness → more concerned or more excited) and provides a more nuanced test of the SARF-Knowledge Gap integration.

> **H5**: Higher AI awareness (AI_HEARD) is associated with a lower probability of membership in the "equally concerned and excited" category, with aware individuals more likely to hold either a predominantly "concerned" or predominantly "excited" stance, consistent with the attitude crystallization mechanism.

---

## 3. Method

### 3.1 Data Source

This study draws on three waves of the Pew Research Center's American Trends Panel (ATP), a nationally representative probability-based online panel of U.S. adults maintained by the Pew Research Center. The ATP is recruited through stratified random sampling of residential addresses from the U.S. Postal Service's Delivery Sequence File, the most comprehensive sampling frame available for reaching the civilian, non-institutionalized adult population. Panel members without internet access are provided with a tablet and data plan to participate, minimizing selection bias against older and lower-income Americans who may otherwise be underrepresented in online panels. All ATP surveys are conducted in both English and Spanish. Pew Research Center provides calibrated survey weights for each wave, constructed to align respondent distributions with population benchmarks from the Current Population Survey on age, sex, education, race/ethnicity, Hispanic origin, and region.

This is a *repeated cross-sectional* design: the three waves surveyed largely distinct samples of American adults rather than the same individuals over time. This design differs fundamentally from a panel or longitudinal study. We cannot directly observe within-person attitude change; instead, we estimate population-level shifts in attitude distributions and demographic predictors across waves. This distinction is central to the appropriate interpretation of our findings: we document changes in which demographic groups are disproportionately represented in each attitude category, not changes in individual-level attitude trajectories.

The three waves selected for this study span a critical period in the public understanding of AI:

- **Wave 119** (December 2022, *N* = 11,004): Fielded in the same month ChatGPT was publicly launched. This wave captures pre-ChatGPT saturation baseline attitudes—a period of moderate AI public awareness before generative AI became a household topic.
- **Wave 132** (August 2023, *N* = 11,201): Fielded approximately eight months after the ChatGPT launch, following an extended period of intense media coverage, widespread trial of generative AI tools, and substantial public and policy debate about AI risks and benefits.
- **Wave 152** (August 2024, *N* = 5,410): Fielded approximately twenty months after the ChatGPT launch, providing a longer-run view as initial media salience moderated and AI integration into daily life progressed.

Together, these three waves provide a quasi-experimental window into the dynamics of public AI attitude formation: Wave 119 represents the pre-saturation baseline, Wave 132 captures the peak of the ChatGPT media shock, and Wave 152 represents the post-acute phase. The large sample sizes—particularly in Waves 119 and 132—provide substantial statistical power for the subgroup analyses central to testing the polarization hypotheses.

### 3.2 Dependent Variable

**CNCEXC** (AI Concern-Excitement Orientation) is the primary dependent variable across all analyses. This item, administered identically in all three waves, asks: "Overall, would you say the increased use of artificial intelligence in daily life makes you feel: More excited than concerned / More concerned than excited / Equally concerned and excited." This question is the primary measure Pew Research Center uses in its public reporting on AI attitudes, and its identical wording across waves enables valid cross-temporal comparison.

The trichotomous response structure is theoretically meaningful. Rather than forcing respondents to a bipolar position, it explicitly preserves the "equally" option as a distinct attitudinal stance, which allows us to distinguish between ambivalent respondents (who hold both concern and excitement) and committed respondents who have resolved toward a net position. For the primary analyses, we use CNCEXC in its full trichotomous form as the outcome in multinomial logistic regression. For secondary analyses focused on the Pew-reported "net concern" trend (the key finding that raised from 38% to 52% between 2022 and 2023), we collapse CNCEXC into a binary indicator: "more concerned" (= 1) versus "more excited or equally" (= 0) and employ binary logistic regression.

### 3.3 Independent Variables

#### Demographic Predictors

The following demographic variables are drawn from the ATP profile data and are available in all three waves:

**F_AGECAT**: Age category (18–29, 30–49, 50–64, 65+). Age is expected to predict AI concern, with older adults more likely to express concern given lower AI familiarity and greater occupational and social distance from technology-forward environments.

**F_GENDER**: Gender (Man, Woman; with non-binary/other responses coded separately given small cell sizes). Gender differences in AI attitudes are documented cross-sectionally, with men more likely to express excitement and women more likely to express concern (Pew Research Center, 2023, 2024).

**F_EDUCCAT**: Educational attainment (Less than high school, High school graduate, Some college, Bachelor's degree, Postgraduate). A key predictor under the Knowledge Gap Hypothesis.

**F_RACETHNMOD**: Race/ethnicity (White non-Hispanic, Black non-Hispanic, Hispanic, Asian non-Hispanic, Other). Included to capture differential exposure to algorithmically mediated systems and differential economic vulnerability to AI-related labor market disruption (Benjamin, 2019).

**F_PARTYSUM_FINAL**: Political party affiliation (Republican/Lean Republican, Independent, Democrat/Lean Democrat). The central predictor for H2, testing whether partisan identity is an independent driver of AI attitude polarization.

**F_INC_TIER2**: Household income tier (Lower income, Middle income, Upper income). Income captures structural economic vulnerability to AI-related disruption and access to AI-augmented tools and services.

**F_CREGION**: Census region (Northeast, Midwest, South, West). Regional variation in AI attitudes may reflect differential economic exposure (e.g., technology industry concentration on the coasts) and cultural differences in technology adoption.

**F_METRO**: Metropolitan status (Metro/non-metro). Urban-rural divides in technology access and economic exposure to AI disruption may produce attitudinal differences independent of individual-level demographics.

#### AI-Specific Predictors

**AI_HEARD**: Self-reported AI awareness/familiarity (available in all three waves). This item captures whether respondents have heard "a lot," "a little," or "nothing at all" about AI. Under SARF, greater AI awareness reflects greater cumulative exposure to the amplification stations shaping AI attitudes and is predicted (H5) to be associated with lower ambivalence and higher probability of a committed attitudinal stance.

**USEAI** (Wave 119 and Wave 152 only): Binary indicator of whether the respondent has personally used AI tools. Available in Waves 119 and 152 but not Wave 132; used in supplementary analyses to examine the relationship between direct AI experience and attitude category membership.

**AIHLPHRT** (Wave 132 only): Assesses respondents' perceptions of whether AI will help or hurt people. Available in Wave 132 only; used in supplementary analysis to characterize the attitudinal profile of respondents in different CNCEXC categories at the peak of the ChatGPT shock wave.

**AICONCERN** (Wave 152 only): Specific concerns about AI (Wave 152 only). Used in supplementary analysis to profile the content of concern among "more concerned" respondents at the 2024 wave.

### 3.4 Survey Weights

All analyses incorporate the Pew-provided survey weights (WEIGHT_W119 for Wave 119, WEIGHT_W132 for Wave 132, WEIGHT_W152 for Wave 152) to produce population-representative estimates. Survey weights are applied using survey-adjusted estimation procedures (the `svyglm` function in R's *survey* package, or equivalent weighted logistic regression in Stata), which account for the complex sampling design and provide valid standard errors for inference. All reported proportions, odds ratios, and marginal effects are weighted. Unweighted analyses are reported as robustness checks.

### 3.5 Analytic Strategy

The analytic strategy proceeds through four steps.

#### Step 1: Descriptive Analysis of CNCEXC Across Waves

We first characterize the distribution of CNCEXC responses at each wave, reporting weighted proportions for each category (more excited, equally, more concerned) with 95% confidence intervals. We formally test whether the wave-to-wave shifts in CNCEXC distributions are statistically significant using chi-square tests of independence on weighted contingency tables. We then disaggregate the wave-by-wave distribution by each demographic predictor (education, party, gender, age, race/ethnicity, income, region, metro status), reporting subgroup-specific proportions across waves to provide the descriptive foundation for the polarization analysis.

#### Step 2: Multinomial Logistic Regression — Predictors of CNCEXC Category

Within each wave, we estimate multinomial logistic regression models predicting three-category CNCEXC membership ("equally concerned and excited" as the reference category). Predictors are entered in theoretically motivated sequential blocks:

- *Model 1*: Sociodemographic variables (age, gender, education, race/ethnicity, income, region, metro).
- *Model 2*: Sociodemographics + party affiliation, to assess the independent contribution of political identity.
- *Model 3*: Model 2 + AI_HEARD, to assess the incremental contribution of AI awareness.

Results are reported as odds ratios with 95% confidence intervals and as average marginal effects (AMEs) to facilitate substantive interpretation. Separate models are estimated for each wave, allowing examination of whether the pattern of demographic predictors changed across waves. Weighted logistic regression using the Rao-Scott design-based approach is used to account for the complex survey design.

#### Step 3: Binary Logistic Regression — Predicting "More Concerned"

To facilitate direct comparison with Pew's publicly reported trends (the concern percentage), we estimate binary logistic regression models with a dichotomized CNCEXC outcome (more concerned = 1 vs. all others = 0). The same sequential block structure from Step 2 is employed. Predicted probabilities of the "more concerned" outcome are generated across the distribution of party affiliation and education to illustrate the practical magnitude of demographic effects.

#### Step 4: Cross-Temporal Interaction Models — Testing for Widening Divergence (H4)

The critical test of H4 (cross-temporal divergence) requires pooling data across all three waves and estimating interaction models. We create a pooled dataset with wave as a factor variable and estimate multinomial (and binary) logistic regression models that include main effects for wave and all demographic predictors, plus wave × demographic interaction terms for the focal predictors (wave × party, wave × education, wave × gender, wave × age). Significant interaction terms indicate that the demographic gap in CNCEXC membership changed across waves in a manner beyond what main effects alone predict. The direction and magnitude of significant interactions are probed by computing and plotting marginal probabilities of the "more concerned" category at each wave separately by subgroup, providing visualization of whether demographic gaps widened, narrowed, or remained stable.

---

## 4. Expected Contributions

### Theoretical Contributions

**First, we provide the first integrative theoretical framework combining the Social Amplification of Risk Framework (SARF) with the Knowledge Gap Hypothesis to explain AI attitude polarization in a repeated cross-sectional design.** Prior work applying SARF to technology risk (Kasperson et al., 1988; Pidgeon et al., 2003) has theorized how social amplification stations intensify or attenuate risk signals as they propagate through society. Separately, the Knowledge Gap Hypothesis (Tichenor et al., 1970) has documented that information campaigns systematically benefit higher-socioeconomic-status groups, widening societal knowledge disparities. Our framework bridges these traditions by specifying how the ChatGPT information shock—an acute amplification event—interacted with differential processing capacity across demographic groups to produce the observed aggregate polarization pattern. This integration is not AI-specific; it generalizes to any emerging technology whose risk-benefit profile is uncertain, contested, and transmitted through socially structured information environments.

**Second, we introduce and test the attitude crystallization hypothesis as a specific mechanism of AI attitude polarization.** The collapse of the "equally concerned and excited" category from 45.9% to 36.5% between December 2022 and August 2023, concurrent with the surge in "more concerned" responses, is consistent with a process in which an acute informational shock—the ChatGPT launch and its aftermath—resolved previously underdetermined attitudes in a predominantly negative direction. This crystallization framing reorients the analytical question from "did aggregate concern increase?" (yes, demonstrably) to "did the distribution of attitude structures change, with genuine ambivalence giving way to committed concern?" The latter question has different implications for intervention: crystallized attitudes are more resistant to change than ambivalent ones, implying a narrowing window for communication strategies that seek to move individuals toward more positive AI orientations.

**Third, we advance understanding of political identity as a predictor of AI attitudes.** A finding that party affiliation predicts CNCEXC category membership independently of education, age, and income—and that the partisan gap widened between 2022 and 2024—would constitute evidence that AI has been absorbed into the domain of politically polarized risk perception (Kahan, 2012). This matters for theory because AI presents a case where the objective risk-benefit profile is arguably more empirically tractable than many politically polarized issues (climate change, nuclear power), yet attitudes may nonetheless cleave along partisan lines through the operation of identity-protective cognition and partisan media amplification.

**Fourth, we document the demographic architecture of a historically unprecedented public opinion shift.** Between December 2022 and August 2023, AI concern among the U.S. adult population increased by 15.6 percentage points. This is an unusually large shift for a stable, nationally representative panel in a short time period. By identifying which demographic subgroups drove this shift—whether it was concentrated among certain partisan, educational, or age groups, or whether it was broad-based—we provide a more precise empirical account of how major technological disruptions reshape public opinion landscapes.

### Methodological Contributions

**We provide a methodological template for repeated cross-sectional analysis of attitude polarization.** Unlike longitudinal panel analysis, repeated cross-sections do not permit within-person change estimation, but they offer key advantages for the questions addressed here: larger samples per wave, freedom from attrition bias, and administrative feasibility for large-scale surveys like the Pew ATP. The wave × demographic interaction approach employed in Step 4 of our analytic strategy enables rigorous testing of the widening-gap prediction without requiring panel data, and we document the procedures for survey-weighted multinomial regression in a way that is directly replicable by future researchers using similar data structures.

**We demonstrate the value of the trichotomous CNCEXC structure for attitude research.** Most published analyses of Pew ATP data collapse CNCEXC into a binary concern indicator, discarding information about the ambivalent "equally" category. Our full multinomial treatment preserves this information and enables the crystallization test—the detection of whether the "equally" group is declining is only possible if this category is analyzed as a distinct outcome rather than merged with the non-concerned category. We illustrate how average marginal effects from weighted multinomial models can be interpreted substantively and communicated accessibly to non-methodological audiences.

### Practical Contributions

**For organizational communication and change management**, this research provides an empirically grounded foundation for understanding the attitudinal landscape employees and customers occupy as organizations accelerate AI adoption. If, as our theoretical framework predicts, political identity is a significant independent predictor of AI concern—with Republicans more likely to express net concern—then AI communication strategies that do not account for partisan framing risks are likely to systematically fail with a substantial segment of the workforce. The finding that AI awareness is associated with lower ambivalence (H5), if supported, implies that passive exposure to AI information may be moving individuals toward crystallized concern rather than toward balanced understanding—a dynamic that argues for more careful management of the information environment surrounding AI deployment.

**For policymakers and public institutions**, identifying which demographic groups drove the 2022–2023 polarization provides actionable intelligence for targeted public communication and digital literacy investment. If the surge in concern was concentrated in older, less-educated, and politically conservative populations—those most reliant on mainstream and partisan media amplification—then interventions focused on these groups, and on the media channels through which they receive AI information, may be most consequential for moderating polarization.

**For AI developers and deploying organizations**, the attitude crystallization hypothesis implies a temporal dynamic that is strategically significant: the window in which public AI attitudes are genuinely ambivalent—and thus most responsive to organizational communication and direct experience—may be narrowing. Organizations that deferred substantive engagement with public trust-building during the 2022–2024 period of peak attitude formation may face a more entrenched attitudinal landscape going forward. This argues for early, proactive trust-building strategies that engage potential users before their attitudes crystallize into committed concern positions.

### Limitations

Several limitations of the current design warrant acknowledgment. First, the repeated cross-sectional structure of the Pew ATP prevents within-person trajectory analysis: we observe population-level distributional shifts, not individual attitude change. We cannot rule out that the aggregate increase in concern reflects compositional changes in who responded to each wave rather than genuine attitude change—though Pew's weighting procedures are designed to control for demographic composition across waves. Second, the primary dependent variable (CNCEXC) is a single, trichotomous item that does not capture the full dimensionality of AI attitudes—the separate dimensions of excitement and concern, domain-specific evaluations, or trust in specific AI applications. Third, CNCEXC is a forced-choice format that does not quantify the intensity of concern or excitement within categories, limiting the precision of comparisons across groups. Fourth, self-reported AI awareness (AI_HEARD) and AI use (USEAI) may be subject to response bias, with socially desirable underreporting of non-use or overreporting of familiarity. Fifth, the Pew ATP, while among the most rigorously designed public opinion surveys available, is not immune to panel conditioning effects over time, and the smaller Wave 152 sample reduces statistical power for subgroup analyses in that wave.

Target journals for this work include *Management Science*, *Organization Science*, the *Journal of Applied Psychology*, *MIS Quarterly*, and *Computers in Human Behavior*.

---

## References

Acemoglu, D., & Restrepo, P. (2020). Robots and jobs: Evidence from US labor markets. *Journal of Political Economy*, *128*(6), 2188–2244. https://doi.org/10.1086/705716

Ajzen, I. (1991). The theory of planned behavior. *Organizational Behavior and Human Decision Processes*, *50*(2), 179–211. https://doi.org/10.1016/0749-5978(91)90020-T

Alhakami, A. S., & Slovic, P. (1994). A psychological study of the inverse relationship between perceived risk and perceived benefit. *Risk Analysis*, *14*(6), 1085–1096. https://doi.org/10.1111/j.1539-6924.1994.tb00080.x

Autor, D. H. (2015). Why are there still so many jobs? The history and future of workplace automation. *Journal of Economic Perspectives*, *29*(3), 3–30. https://doi.org/10.1257/jep.29.3.3

Benjamin, R. (2019). *Race after technology: Abolitionist tools for the new Jim Code*. Polity Press.

Brynjolfsson, E., & McAfee, A. (2014). *The second machine age: Work, progress, and prosperity in a time of brilliant technologies*. W. W. Norton.

Cacioppo, J. T., & Berntson, G. G. (1994). Relationship between attitudes and evaluative space: A critical review, with emphasis on the separability of positive and negative substrates. *Psychological Bulletin*, *115*(3), 401–423. https://doi.org/10.1037/0033-2909.115.3.401

Cave, S., & Dihal, K. (2019). Hopes and fears for intelligent machines in fiction and reality. *Nature Machine Intelligence*, *1*(2), 74–78. https://doi.org/10.1038/s42256-019-0020-9

Converse, P. E. (1964). The nature of belief systems in mass publics. In D. E. Apter (Ed.), *Ideology and discontent* (pp. 206–261). Free Press.

Davenport, T. H., & Kirby, J. (2016). *Only humans need apply: Winners and losers in the age of smart machines*. Harper Business.

Davis, F. D. (1989). Perceived usefulness, perceived ease of use, and user acceptance of information technology. *MIS Quarterly*, *13*(3), 319–340. https://doi.org/10.2307/249008

Eurobarometer. (2017). *Attitudes towards the impact of digitisation and automation on daily life* (Special Eurobarometer 460). European Commission.

Fast, E., & Horvitz, E. (2017). Long-term trends in the public perception of artificial intelligence. *Proceedings of the AAAI Conference on Artificial Intelligence*, *31*(1), 963–969. https://doi.org/10.1609/aaai.v31i1.10635

Frey, C. B., & Osborne, M. A. (2017). The future of employment: How susceptible are jobs to computerisation? *Technological Forecasting and Social Change*, *114*, 254–280. https://doi.org/10.1016/j.techfore.2016.08.019

Gallup. (2024). *AI in the workplace: Employee perspectives*. Gallup, Inc.

Gaziano, C. (1983). The knowledge gap: An analytical review of media effects. *Communication Research*, *10*(4), 447–486. https://doi.org/10.1177/009365083010004003

Gaziano, C. (1997). Forecast 2000: Widening knowledge gaps. *Journalism & Mass Communication Quarterly*, *74*(2), 237–264. https://doi.org/10.1177/107769909707400202

Hu, K. (2023, February 2). ChatGPT sets record for fastest-growing user base — analyst note. *Reuters*. https://www.reuters.com/technology/chatgpt-sets-record-fastest-growing-user-base-analyst-note-2023-02-01/

Ipsos. (2023). *Global views on AI 2023*. Ipsos Public Affairs.

Kahan, D. M. (2012). Cultural cognition as a conception of the cultural theory of risk. In S. Roeser, R. Hillerbrand, P. Sandin, & M. Peterson (Eds.), *Handbook of risk theory: Epistemology, decision theory, ethics and social implications of risk* (pp. 725–759). Springer.

Kahan, D. M., Braman, D., Gastil, J., Slovic, P., & Mertz, C. K. (2007). Culture and identity-protective cognition: Explaining the white-male effect in risk perception. *Journal of Empirical Legal Studies*, *4*(3), 465–505. https://doi.org/10.1111/j.1740-1461.2007.00097.x

Kahan, D. M., Jenkins-Smith, H., & Braman, D. (2011). Cultural cognition of scientific consensus. *Journal of Risk Research*, *14*(2), 147–174. https://doi.org/10.1080/13669877.2010.511246

Kasperson, J. X., & Kasperson, R. E. (1996). The social amplification and attenuation of risk. *Annals of the American Academy of Political and Social Science*, *545*, 95–105. https://doi.org/10.1177/0002716296545001010

Kasperson, R. E., Renn, O., Slovic, P., Brown, H. S., Emel, J., Goble, R., Kasperson, J. X., & Ratick, S. (1988). The social amplification of risk: A conceptual framework. *Risk Analysis*, *8*(2), 177–187. https://doi.org/10.1111/j.1539-6924.1988.tb01168.x

Lai, C.-H. (2020). Motivations, usage patterns, and knowledge-sharing behaviors on social media: Comparing younger and older adults. *Computers in Human Behavior*, *115*, Article 106625. https://doi.org/10.1016/j.chb.2020.106625

McPherson, M., Smith-Lovin, L., & Cook, J. M. (2001). Birds of a feather: Homophily in social networks. *Annual Review of Sociology*, *27*(1), 415–444. https://doi.org/10.1146/annurev.soc.27.1.415

Pew Research Center. (2023). *Growing public concern about the role of artificial intelligence in daily life*. Pew Research Center.

Pew Research Center. (2024). *Americans' use of ChatGPT is ticking up, but few trust its election information*. Pew Research Center.

Pidgeon, N., Kasperson, R. E., & Slovic, P. (Eds.). (2003). *The social amplification of risk*. Cambridge University Press.

Priester, J. R., & Petty, R. E. (1996). The gradual threshold model of ambivalence: Relating the positive and negative bases of attitudes to subjective ambivalence. *Journal of Personality and Social Psychology*, *71*(3), 431–449.

Prior, M. (2013). Media and political polarization. *Annual Review of Political Science*, *16*, 101–127. https://doi.org/10.1146/annurev-polisci-100711-135242

Raisch, S., & Krakowski, S. (2021). Artificial intelligence and management: The automation-augmentation paradox. *Academy of Management Review*, *46*(1), 192–210. https://doi.org/10.5465/amr.2018.0072

Renn, O., Burns, W. J., Kasperson, J. X., Kasperson, R. E., & Slovic, P. (1992). The social amplification of risk: Theoretical foundations and empirical applications. *Journal of Social Issues*, *48*(4), 137–160. https://doi.org/10.1111/j.1540-4560.1992.tb01949.x

Slovic, P. (1987). Perception of risk. *Science*, *236*(4799), 280–285. https://doi.org/10.1126/science.3563507

Slovic, P., Finucane, M. L., Peters, E., & MacGregor, D. G. (2007). The affect heuristic. *European Journal of Operational Research*, *177*(3), 1333–1352. https://doi.org/10.1016/j.ejor.2005.04.006

Sunstein, C. R. (2017). *#Republic: Divided democracy in the age of social media*. Princeton University Press.

Thompson, M. M., Zanna, M. P., & Griffin, D. W. (1995). Let's not be indifferent about (attitudinal) ambivalence. In R. E. Petty & J. A. Krosnick (Eds.), *Attitude strength: Antecedents and consequences* (pp. 361–386). Erlbaum.

Tichenor, P. J., Donohue, G. A., & Olien, C. N. (1970). Mass media flow and differential growth in knowledge. *Public Opinion Quarterly*, *34*(2), 159–170. https://doi.org/10.1086/267786

Tversky, A., & Kahneman, D. (1974). Judgment under uncertainty: Heuristics and biases. *Science*, *185*(4157), 1124–1131. https://doi.org/10.1126/science.185.4157.1124

van Dijk, J. A. G. M. (2020). *The digital divide*. Polity Press.

van Harreveld, F., van der Pligt, J., & de Liver, Y. N. (2009). The agony of ambivalence and ways to resolve it: Introducing the MAID model. *Personality and Social Psychology Review*, *13*(1), 45–61.

Venkatesh, V., Morris, M. G., Davis, G. B., & Davis, F. D. (2003). User acceptance of information technology: Toward a unified view. *MIS Quarterly*, *27*(3), 425–478. https://doi.org/10.2307/30036540

Wei, L., & Hindman, D. B. (2011). Does the digital divide matter more? Comparing the effects of new media and old media use on the education-based knowledge gap. *Mass Communication and Society*, *14*(2), 216–235. https://doi.org/10.1080/15205431003642707

Zaller, J. R. (1992). *The nature and origins of mass opinion*. Cambridge University Press. https://doi.org/10.1017/CBO9780511818691

Zhang, B., & Dafoe, A. (2019). *Artificial intelligence: American attitudes and trends*. Oxford University, Centre for the Governance of AI.
