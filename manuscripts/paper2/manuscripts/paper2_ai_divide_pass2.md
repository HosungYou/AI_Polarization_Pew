# The AI Divide: Latent Attitude Profiles and the Stratification of Public Orientations Toward Artificial Intelligence

**Abstract**

Public attitudes toward artificial intelligence (AI) are typically treated as a single spectrum from enthusiasm to anxiety, obscuring qualitative differences that may reproduce social stratification. Drawing on digital divide theory, we propose a three-level AI divide framework and use latent class analysis on Pew Research Center data (N = 10,749; American Trends Panel Wave 132) to identify four attitude profiles: AI-Anxious (9%), AI-Uninformed (33%), AI-Advantaged (21%), and AI-Ambivalent (37%). These profiles replicate across a split-sample design. Structural equation modeling reveals that socioeconomic status predicts class membership both directly and indirectly through AI awareness, and education effects vary by race/ethnicity (LRT p = .007), with stronger penalties for low education among Black respondents. Cross-wave validation (2022--2024) confirms structural stability. Unequal orientations toward AI constitute an emergent axis of digital inequality with implications for technology governance.

**Keywords:** artificial intelligence, digital divide, latent class analysis, public opinion, technology attitudes, social stratification

---

## 1. Introduction

Artificial intelligence has moved from research laboratories into everyday life with remarkable speed. Recommender algorithms curate news feeds, large language models draft correspondence, and automated decision systems shape hiring, lending, and criminal sentencing. Public reactions to this transformation matter. Attitudes toward AI influence adoption behavior (Araujo et al., 2020), shape regulatory preferences (Zhang and Dafoe, 2019), and signal democratic legitimacy for emerging governance frameworks (Floridi et al., 2018). Yet most survey research treats AI attitudes as a single dimension running from excitement to concern, aggregating responses into population-level summaries that mask important heterogeneity.

A growing body of research has documented broad contours of public opinion about AI. Cross-national surveys find that a majority of adults express some mix of enthusiasm and concern, with considerable variation by country, demographic group, and the specific AI application in question (Cave et al., 2019; Eurobarometer, 2017; Neudert et al., 2020). In the United States, the Pew Research Center (2023) reported that 52% of Americans feel more concerned than excited about AI, up from 38% the previous year. Education, income, gender, and age are consistently associated with AI attitudes (Zhang and Dafoe, 2019; Ipsos, 2023). Valuable as these descriptive findings are, they share a common limitation: they treat the population as varying along a single attitudinal dimension, differing only in degree. Little attention has been paid to the possibility that people relate to AI in qualitatively different ways -- that the structure of attitudes, not just their valence, varies across the population.

This paper takes a different approach. Rather than asking how favorable or unfavorable the public is toward AI, we ask: *What distinct orientations toward AI exist in the American public, and how are these orientations distributed across social groups?* The distinction matters because qualitatively different relationships with a technology -- and not just different degrees of enthusiasm -- have different implications for inequality. A person who has never heard of AI and expresses no opinion occupies a categorically different position from someone who is well-informed but deeply concerned, even if both register as "not excited" on a standard survey item. Someone who sees AI as uniformly beneficial across all domains differs just as sharply from someone who distinguishes between health care applications they welcome and surveillance applications they reject, even though both may register as broadly positive.

To theorize these differences, we extend the digital divide framework (van Dijk, 2005, 2020; Hargittai, 2002; Robinson et al., 2015) to the domain of AI attitudes. Classic digital divide research evolved through three stages: from access gaps (first-level divide) to skills gaps (second-level divide) to outcome gaps (third-level divide). We propose an analogous three-level AI divide: (1) an awareness divide separating those who know about AI from those who do not, (2) an attitude complexity divide distinguishing nuanced from simplistic orientations, and (3) an outcome divide reflecting differential perceptions of AI's domain-specific consequences.

Empirically, we use latent class analysis (LCA) on a large, nationally representative dataset -- the Pew Research Center American Trends Panel Wave 132 (August 2023; N = 10,749) -- to identify naturally occurring attitude profiles. We then examine how socioeconomic status, race/ethnicity, and AI awareness predict membership in these profiles, testing whether the social stratification patterns documented in prior digital divide research extend to AI orientations.

Three contributions emerge from this analysis. First, we demonstrate that American adults cluster into four qualitatively distinct AI attitude profiles that replicate across independent subsamples with different domain items. Second, we show that socioeconomic status predicts profile membership both directly and indirectly through awareness, establishing AI awareness as a key mediating mechanism. Third, we document intersectional variation in these relationships, with education effects on AI attitudes differing by race/ethnicity in ways that single-axis models would miss. The evidence, in sum, indicates that public orientations toward AI are not simply a matter of individual preference but are structured by the same social forces that produce other forms of digital inequality.

We develop this argument through a three-level AI divide framework, test five hypotheses using latent class analysis and structural equation modeling, and validate the resulting typology across survey waves.

## 2. Theoretical Framework

### 2.1 From Digital Divides to AI Divides

Since its initial articulation in the 1990s, the concept of the digital divide has evolved through several distinct phases. Early formulations focused almost exclusively on physical access to computers and the internet -- a binary distinction between haves and have-nots (NTIA, 1999). As internet penetration increased, scholars recognized that access alone was insufficient; what people could do with technology mattered equally (Hargittai, 2002; DiMaggio et al., 2004). This insight gave rise to the second-level digital divide, centered on digital skills and literacy (van Dijk, 2005; Hargittai and Hinnant, 2008). A third-level divide has since been theorized around tangible outcomes -- the extent to which internet use translates into social, economic, and political benefits (Wei et al., 2011; Robinson et al., 2015; Ragnedda, 2018).

Van Dijk's (2005, 2020) resources and appropriation model provides the most thorough theoretical integration of these levels. Personal and positional categorical inequalities (including education, income, age, race, and gender) shape the distribution of resources (material, mental, social, cultural), which in turn determine successive kinds of access: motivational access, material access, skills access, and usage access. These access types are sequential and cumulative -- a point worth underscoring. Individuals who lack motivational access, meaning awareness of and interest in a technology, never proceed to the stages where skills and beneficial use become possible.

The rapid diffusion of AI into public life, we argue, has created conditions for a new domain of digital inequality that tracks this established framework but operates at the level of orientations rather than use. Unlike the internet in the early 2000s, AI presents a particular challenge for public engagement: many AI systems operate invisibly, most people do not directly choose to adopt or reject AI, and the consequences of AI are highly domain-specific. Public orientations toward AI are also shaped by what Jasanoff (2015) terms "sociotechnical imaginaries" -- collectively held visions of desirable futures achievable through technology. Media framing, platform experiences, and cultural narratives about AI all contribute to the "algorithmic imaginaries" (Bucher, 2017) through which people make sense of systems they rarely observe directly. These characteristics make AI attitudes an especially revealing site for studying how inequality shapes technology orientations: differential access to information about AI may produce not just different opinions but radically different imaginative frames for understanding what AI is and does.

Prior research on AI attitudes has primarily employed variable-centered approaches -- regression models predicting a single attitude outcome from demographic predictors (Araujo et al., 2020; Zhang and Dafoe, 2019; Cave et al., 2019; Neudert et al., 2020). While establishing that education, age, gender, and technology familiarity predict attitudes, this work assumes that the same model applies to all respondents. A person-centered approach, by contrast, allows for the possibility that the population contains qualitatively distinct subgroups whose attitudes are organized differently. Latent class analysis provides a statistical framework for identifying such subgroups (Hagenaars and McCutcheon, 2002; Collins and Lanza, 2010). Although LCA has been widely used in health behavior, political science, and consumer research, its application to technology attitudes remains limited. A recent exception is Wang et al. (2025), who used LCA to identify AI competence profiles in the Netherlands and documented sociodemographic stratification in AI-related skills and knowledge. Our study extends this emerging line of person-centered inquiry in several ways: we focus on attitudes rather than competencies, use a considerably larger probability-based sample (N = 10,749 vs. ~1,600), incorporate a built-in split-sample replication, and test intersectional hypotheses about how education effects vary by race/ethnicity -- a dimension largely absent from European studies of the AI divide.

### 2.2 A Three-Level AI Divide Framework

We propose a three-level framework for understanding stratified orientations toward AI:

**Level 1: The Awareness Divide.** Before people can form meaningful attitudes about AI, they must know that AI exists and have some understanding of what it does. This echoes van Dijk's (2005) concept of motivational access and earlier findings that awareness of emerging technologies is socially stratified (Brossard et al., 2009). Survey data consistently show that awareness of AI varies by education, income, age, and race/ethnicity (Pew Research Center, 2023; Zhang and Dafoe, 2019). Those without awareness are effectively excluded from public deliberation about AI governance.

**Level 2: The Attitude Complexity Divide.** Among those who are aware of AI, the sophistication and differentiation of attitudes varies. Some individuals hold views that distinguish between AI applications they consider beneficial and those they find concerning. Others maintain undifferentiated positions -- blanket enthusiasm or blanket anxiety -- that do not reflect the domain-specific nature of AI's impacts. This echoes the distinction Hargittai and Hinnant (2008) drew between mere internet access and the quality of online engagement. We conceptualize attitude complexity not as a normative judgment about which views are "correct" but as an empirical description of how differentiated people's orientations are across AI domains.

**Level 3: The Outcome Divide.** At the most concrete level, people differ in whether they perceive AI as likely to help or hurt in specific life domains -- healthcare, criminal justice, personal privacy, employment. These perceived outcomes shape willingness to interact with AI systems (Araujo et al., 2020), support for regulatory frameworks (Zhang and Dafoe, 2019), and capacity to advocate for one's interests in AI-mediated processes. This maps onto what Robinson et al. (2015) and Ragnedda and Muschert (2013) termed the third-level digital divide, where differential engagement patterns produce differential benefits.

What makes these three levels analytically powerful is their interdependence. We theorize them as sequential and cumulative, following van Dijk's (2005) appropriation logic. Awareness (Level 1) is a precondition for developing complex attitudes (Level 2), which in turn shape domain-specific outcome perceptions (Level 3). This sequential structure implies that socioeconomic factors may influence higher-level orientations both directly and indirectly through their effects on awareness.

### 2.3 Hypotheses

Drawing on this framework and the empirical digital divide literature, we derive five hypotheses.

**H1 (Latent heterogeneity).** Distinct latent classes of AI attitudes exist in the American public, reflecting qualitatively different orientations rather than variation along a single continuum.

Education and income, we hypothesize, predict latent class membership, with higher SES associated with more informed AI orientations (**H2: SES stratification**). Education should be especially influential because it shapes both cognitive resources for processing information about complex technologies and exposure to professional environments where AI is salient (van Dijk, 2005; Hargittai, 2002).

**H3 (Awareness mediation).** AI awareness mediates the SES-to-class relationship. Because awareness (Level 1) is the gateway to higher-level orientations and is itself socially stratified, a large portion of the SES effect should operate through this pathway -- as knowledge mediation does in health communication (Viswanath and Bond, 2007).

We further expect the effect of education on class membership to vary by race/ethnicity (**H4: Intersectional variation**). Different racialized experiences with technology -- including algorithmic discrimination, surveillance, and differential access to tech-sector employment (Robinson et al., 2015; Ragnedda and Muschert, 2013) -- may moderate how education translates into AI orientations.

**H5 (Structural stability).** The latent class structure is stable across survey waves, indicating persistent orientations rather than transient responses to news cycles (Lazarsfeld and Henry, 1968).

## 3. Method

### 3.1 Data

We analyze data from the Pew Research Center's American Trends Panel (ATP), a probability-based online panel recruited through national random-digit-dial and address-based sampling. Our primary data come from Wave 132 (fielded August 7--27, 2023), which surveyed N = 10,749 U.S. adults about their attitudes toward artificial intelligence. The survey employed a split-sample design: respondents were randomly assigned to Form A (n = 5,368) or Form B (n = 5,317), with each form containing different AI domain items alongside shared items on general AI attitudes and awareness.

For cross-wave validation, we draw on Wave 119 (December 2022; N = 10,906) and Wave 152 (August 2024; N = 5,363), both of which included the same general AI attitude and awareness items. All analyses use Pew-provided survey weights to adjust for nonresponse and ensure national representativeness. The weighted sample reflects the U.S. adult population in terms of gender (50.5% female, 48.2% male), age (18.7% ages 18--29, 33.9% ages 30--49, 25.2% ages 50--64, 22.2% ages 65+), race/ethnicity (62.6% White non-Hispanic, 15.5% Hispanic, 11.5% Black non-Hispanic, 6.0% Asian non-Hispanic), and education (35.8% college graduate or higher, 30.6% some college, 33.5% high school or less).

### 3.2 Measures

**LCA indicators.** Six manifest indicators were used for each form's latent class analysis. Two indicators were shared across both forms:

*AI attitude* (lca_attitude): "Overall, would you say the increased use of artificial intelligence in daily life makes you feel more excited, more concerned, or an equal mix of excited and concerned?" Coded as a 3-category nominal variable (excited, concerned, equal mix).

*AI awareness* (lca_awareness): "How much, if anything, have you heard or read about artificial intelligence?" Coded as a 3-category ordinal variable (a lot, a little, nothing at all).

*Domain-specific items:* Each form included four binary items asking whether AI in a specific domain would mostly help or mostly hurt. Form A covered: (a) AI's ability to provide accurate information, (b) health care, (c) vehicle safety, and (d) customer service. Form B covered: (e) product search and recommendations, (f) personal privacy, (g) policing, and (h) the quality of a patient's relationship with their doctor. Domain items were coded as binary indicators (1 = not hurt/mostly help, 2 = hurt/mostly hurt).

**Predictors for structural modeling.** Education was measured in three categories: high school graduate or less (33.5%), some college (30.6%), and college graduate or higher (35.8%; reference category). Household income was coded as lower (29.4%), middle (50.5%; reference), or upper (20.1%), based on Pew's income tier classification. Race/ethnicity was measured as White non-Hispanic (reference), Black non-Hispanic, Hispanic, Asian non-Hispanic, and other. Controls included age (18--29, 30--49 [reference], 50--64, 65+), gender (male [reference], female, other), and party identification (Democrat/Lean Democrat [reference], Republican/Lean Republican).

### 3.3 Analytic Strategy

Our analysis proceeds in four stages, following the three-step approach recommended for latent class analysis with auxiliary variables (Vermunt, 2010; Asparouhov and Muthen, 2014).

**Stage 1: Latent class analysis.** We estimated LCA models with 2 through 7 classes separately for Form A and Form B using the poLCA package in R (Linzer and Lewis, 2011). Each model was estimated with 20 random starting values to guard against local maxima, with a maximum of 3,000 EM iterations. Model selection followed a multi-criteria approach: we selected the model with the lowest Bayesian Information Criterion (BIC) among models satisfying two substantive constraints -- entropy above 0.6 (indicating adequate class separation) and a minimum class size exceeding 5% of the sample (ensuring interpretability). This approach balances statistical fit with practical considerations (Nylund et al., 2007). Separate estimation on each form provides a built-in replication check: if similar profile types emerge from different domain items, this supports the robustness of the typology.

**Stage 2: Multinomial logistic regression.** Using modal class assignments from Stage 1, we estimated a series of nested multinomial logistic regression models predicting class membership from sociodemographic characteristics and AI awareness. We compared four models: a base model with demographics and race/ethnicity only; an SES model adding education and income; an awareness model adding AI awareness; and a full model including all predictors. Model comparison used likelihood ratio tests and BIC. Average marginal effects (AME) were computed using the marginaleffects package (Arel-Bundock et al., 2024) to facilitate interpretation across the multinomial outcome.

**Stage 3: Mediation analysis.** To test whether AI awareness mediates the SES-to-class relationship, we estimated path models using lavaan (Rosseel, 2012) with maximum likelihood estimation and bootstrap standard errors (1,000 replications). For each latent class (coded as a binary outcome), we specified a two-equation system: SES predicting awareness, and both SES and awareness predicting class membership. Indirect effects were computed as the product of coefficients with bias-corrected bootstrap confidence intervals.

**Stage 4: Intersectional and cross-wave analyses.** We tested for race-moderated education effects using multi-group multinomial logistic regression, fitting separate models within each racial/ethnic group and comparing education coefficients. A formal interaction test was conducted by comparing a model with education-by-race interaction terms to the full additive model using the likelihood ratio test. Cross-wave validation used the two shared indicators (attitude and awareness) to estimate comparable 2-indicator LCA models across Waves 119, 132, and 152, examining whether similar profile structures and education gradients emerge in different time periods.

## 4. Results

### 4.1 Latent Class Analysis: Identifying AI Attitude Profiles

**Model selection.** For both Form A and Form B, the four-class solution provided the best balance of statistical fit and interpretability. Form A's 4-class model yielded BIC = 38,538.8 with entropy = 0.659 and a minimum class size of 9.0% (Table 1). Form B's 4-class model yielded BIC = 38,343.9 with entropy = 0.643 and a minimum class size of 10.6%. While the 5-class models offered marginally lower BIC values, they produced entropy below the 0.6 threshold in Form B (0.596), suggesting inadequate class separation. Higher BIC in both forms ruled out the 3-class models. That four classes emerged independently from two subsamples provides initial evidence for the robustness of this typology.

[Table 1 about here: LCA model fit comparison for Forms A and B]

**Class profiles.** Four substantively distinct profiles emerged from the class-conditional response probabilities (Figure 1).

*Class 1: AI-Anxious (9.0% of Form A).* Near-uniform negativity defines this smallest class. Ninety percent expressed concern about AI (only 3% excitement), and domain perceptions were almost uniformly pessimistic: the probability of seeing no harm ranged from just 3% (customer service) to 14% (accurate information). Yet this is not an uninformed class -- 36% reported high AI awareness. The AI-Anxious profile captures individuals who know enough about AI to be genuinely alarmed.

*Class 2: AI-Uninformed (33.1%).* What defines this largest class is not what its members think about AI but what they do not know. Essentially none reported high awareness (probability effectively zero), yet their domain perceptions were nearly all benign -- 99% saw no harm from AI in health care, accurate information, or vehicle safety. This pattern suggests what we term "default non-concern": an absence of opinion rather than a considered judgment. That roughly 40% of this class had a high school education or less signals the socioeconomic roots of this informational exclusion.

*Class 3: AI-Advantaged (20.5%).* High awareness (85%) meets broad optimism in this class. Members expressed the highest excitement of any profile (25%) and saw little harm across all domains (91--100%). Nearly half (47%) held a college degree. Representing the population segment best positioned to engage with and benefit from AI-mediated systems, the Advantaged profile stands at the opposite pole from the Uninformed.

*Class 4: AI-Ambivalent (37.4%).* Unlike the Uninformed who default to non-concern and the Anxious who default to worry, the Ambivalent class discriminates. With moderate awareness (39% high), these respondents were relatively comfortable with AI in health care (78% not hurt) and vehicle safety (80%) but considerably more skeptical about customer service (46%) and accurate information (59%). A 69% concern rate and 38% college graduation rate place them between the Advantaged and Uninformed on both knowledge and education. This is the profile of informed, selective skepticism.

[Figure 1 about here: Class-conditional response probabilities for Form A]

**Cross-form replication.** The split-sample design allowed us to assess whether similar profiles emerged when different domain items were used. Euclidean distances between Form A and Form B class profiles in the shared-indicator space (attitude and awareness) confirm strong cross-form correspondence: the AI-Anxious profiles matched with a distance of 0.076, the AI-Ambivalent profiles with 0.071, the AI-Uninformed profiles with 0.240, and the AI-Advantaged profiles with 0.313 (Table 2). Profiles defined primarily by the shared indicators (Anxious and Ambivalent) showed near-perfect replication, while the two profiles more influenced by domain-specific items showed larger but still moderate distances. That the same four-type structure emerged from different domain content supports H1 and indicates that these profiles capture genuine population heterogeneity rather than item-specific artifacts.

[Table 2 about here: Cross-form profile distances on shared indicators]

### 4.2 SES, Awareness, and Class Membership

Nested multinomial logistic regression models are presented in Table 3. Adding SES indicators (education and income) to the base demographic model produced a significant improvement in fit (LRT chi-square = 131, df = 12, p < .001). A much larger improvement followed from adding AI awareness (LRT chi-square = 3,222, df = 3, p < .001), confirming the dominant role of awareness in structuring AI attitudes. Including both SES and awareness in the full model fit much better than the base model (LRT chi-square = 3,265, df = 15, p < .001) and produced a BIC of 9,878, compared to 13,015 for the base model.

[Table 3 about here: Multinomial logistic regression model comparison]

**Average marginal effects.** AI awareness dominated the model (Table 4). Each unit increase on the three-point awareness scale shifted respondents sharply toward the Advantaged class (AME = +1.67, p < .001) and away from all three alternatives, with the largest countervailing effect on Ambivalent membership (AME = -0.97, p < .001). How large is this effect? Roughly 36 times larger than the next strongest predictor -- a magnitude confirming the centrality of awareness in structuring AI orientations.

Education and income effects operated in the direction predicted by H2 but were considerably smaller. High school education or less predicted increased Anxious membership (AME = +0.046, p < .001) and decreased Ambivalent membership (AME = -0.037, p = .036); upper income predicted increased Advantaged membership (AME = +0.033, p = .003). Among demographic controls, age 65+ strongly predicted Uninformed membership (AME = +0.098, p < .001), and Republican identification predicted Anxious membership (AME = +0.062, p < .001). Asian non-Hispanic respondents were also far more likely to be Advantaged (AME = +0.063, p < .001).

[Table 4 about here: Average marginal effects from full multinomial logistic model]

### 4.3 Mediation: SES, Awareness, and Class Membership

Did AI awareness mediate the SES-to-class relationship, as H3 predicted? Results from the bootstrap path models confirmed significant indirect effects through awareness for multiple class outcomes (Table 5).

For the AI-Advantaged class (Class 3), the indirect effect of high school education (relative to college graduate) through awareness was strong and negative (beta = -0.103, 95% CI [-0.123, -0.085], p < .001), indicating that lower education reduces the probability of Advantaged membership to a large degree through reduced awareness. In the opposite direction, for the AI-Uninformed class (Class 2), the indirect effect of high school education through awareness was positive and of similar magnitude (beta = +0.115, 95% CI [0.094, 0.137], p < .001), indicating that lower education increases Uninformed membership through reduced awareness. Upper income showed a significant positive indirect effect on Advantaged membership through awareness (beta = +0.041, 95% CI [0.027, 0.055], p < .001).

Awareness, then, functions as a key mediator. For the Advantaged and Uninformed classes in particular, the indirect effects through awareness exceeded the direct effects of education, supporting the theoretical proposition that awareness operates as a gateway mechanism. Lower-SES individuals are less likely to be aware of AI, and this reduced awareness, in turn, channels them toward the Uninformed class and away from the Advantaged class. H3 is supported.

[Table 5 about here: Mediation results -- indirect effects through AI awareness]

### 4.4 Intersectional Analysis: Education Effects by Race/Ethnicity

To test H4, we examined whether education effects on class membership varied by race/ethnicity. Comparing the model with education-by-race interaction terms to the additive full model yielded a statistically significant likelihood ratio test (LRT p = .007), indicating that education gradients in AI attitudes are not uniform across racial/ethnic groups.

Multi-group analysis revealed important differences in magnitude (Table 6). Among Black non-Hispanic respondents, having a high school education or less was associated with a 1.10 log-odds increase in Anxious class membership relative to the reference class (p = .028) -- nearly three times the 0.39 log-odds increase observed among White non-Hispanic respondents (p = .022). Low education, in other words, carries a far heavier penalty for AI anxiety among Black than among White respondents.

A different pattern emerged among Hispanic respondents: some college education was associated with a 0.88 log-odds increase in Advantaged class membership (p = .013), a relationship not observed in other groups. Specific pathways through which education facilitates positive AI engagement in Hispanic communities likely account for this pattern. White non-Hispanic respondents showed the expected monotonic education gradient, with high school education predicting increased Anxious membership and college education predicting increased Advantaged membership.

These intersectional patterns support H4 and align with broader digital divide research documenting that the "returns" to education in technology engagement are not uniform across social positions (Robinson et al., 2015; Ragnedda and Muschert, 2013). Structural factors -- including differential exposure to AI in occupational settings, varying trust in technology institutions, and racially differentiated experiences with algorithmic systems -- likely contribute to these divergent education gradients.

[Table 6 about here: Multi-group multinomial logistic regression by race/ethnicity]

### 4.5 Cross-Wave Validation

How stable is this structure over time? To assess H5, we estimated comparable two-indicator LCA models using the shared attitude and awareness items across three survey waves: W119 (December 2022), W132 (August 2023), and W152 (August 2024). In all three waves, the BIC-optimal solution was a two-class model (Table 7). This simpler structure reflects the reduced information available from only two indicators, which cannot resolve the full four-class typology identified in the six-indicator models. Even so, the two-class solution consistently distinguished between a more engaged/polarized class and a more moderate/ambivalent class.

In W119, the two classes were labeled AI-Optimistic (higher excitement, higher awareness) and Ambivalent. By W132, they had shifted to AI-Skeptic (higher concern, moderate awareness) and Ambivalent. W152 reproduced this Ambivalent and AI-Skeptic split. Class proportions differed significantly across waves (chi-square = 12,133.6, df = 4, p < .001), reflecting real shifts in the distribution of attitudes as AI became more prominent in public discourse between 2022 and 2024. That the split moved from Optimistic/Ambivalent in December 2022 to Skeptic/Ambivalent by August 2023 aligns with the public launch of ChatGPT and subsequent media coverage emphasizing both AI capabilities and risks.

Despite these distributional shifts, two structural features remained stable. A basic binary division between a more engaged class and a more moderate class persisted across all waves. And the education gradient in the more engaged class held firm: college-educated respondents were heavily represented in whichever class showed higher awareness and more settled attitudes, whether optimistic or skeptical. This structural persistence, even amid shifting sentiment, partially supports H5 and suggests that the stratification of AI orientations reflects lasting social patterns rather than momentary reactions.

[Table 7 about here: Cross-wave LCA fit indices and class proportions]

## 5. Discussion

### 5.1 Four Orientations, Not One Spectrum

A person who has never heard of AI and defaults to non-concern (Uninformed) occupies a categorically different position from one who is highly informed and sees broad benefits (Advantaged), even though both might be coded as "not concerned" on a standard survey item. The four profiles challenge the common practice of reporting AI attitudes as a population average or a simple favorable/unfavorable split. That this four-class structure replicated across independent split samples -- using different domain items but producing profile distances as low as 0.07 on shared indicators -- points to genuine population types (Savage et al., 2013) rather than artifacts of particular question wordings.

### 5.2 The AI Divide as Digital Inequality

Where does the strongest evidence for our framework lie? At the awareness level. AI awareness alone dwarfs every other predictor of class membership (AME = +1.67 for Advantaged), and awareness itself is socially stratified by education and income, creating the kind of cascading inequality van Dijk (2005) theorized for motivational access.

But the framework's deeper payoff lies in distinguishing between the Uninformed and Ambivalent classes -- a distinction that a standard favorable/unfavorable survey measure would miss entirely. Both classes express low excitement about AI. Yet the Uninformed (essentially zero high awareness, 93--99% seeing no harm) represent default non-concern born of ignorance, while the Ambivalent (moderate awareness, harm perceptions ranging from 46% to 80% across domains) represent genuine if critical engagement. That the former are heavily lower-SES (40% high school or less) and the latter more educated (38% college graduate) demonstrates how SES shapes not just the direction but the sophistication of technology attitudes. Consider the Ambivalent class's capacity to discriminate between domains where AI seems benign (health care, vehicle safety) and domains where it seems threatening (customer service, accurate information). This selective judgment exemplifies the attitude complexity our framework predicts -- a capacity that the Uninformed and Anxious classes lack.

### 5.3 Awareness as a Gateway Mechanism

Direct evidence for the sequential structure theorized in our framework comes from the mediation analysis. Education's effect on class membership operates in large part through awareness: the indirect effect of high school education on Advantaged membership through awareness (beta = -0.103) is larger than most direct effects in the model. Awareness, by this evidence, functions as a gateway mechanism -- a necessary precondition for the kind of informed engagement that characterizes the Advantaged and Ambivalent profiles.

What follows for practice? If awareness is a primary pathway through which SES shapes AI attitudes, then interventions targeting AI literacy could partially disrupt the reproduction of inequality. Unlike education or income, which resist short-term policy change, awareness is responsive to communication strategies, media coverage, and public engagement initiatives. Such efforts, if they succeed in reaching lower-SES populations, could shift individuals from the Uninformed toward the Ambivalent or even Advantaged profiles -- not by making them more enthusiastic about AI, but by enabling informed engagement.

### 5.4 Intersectional Complexity

Any simple story about SES and AI attitudes is complicated by the significant interaction between education and race/ethnicity. That low education carries a nearly threefold penalty for Anxious membership among Black respondents compared to White respondents (1.10 vs. 0.39 log-odds) demands close scrutiny. Black Americans with lower education likely have greater exposure to AI systems in punitive contexts -- criminal justice algorithms, welfare eligibility systems, hiring screens -- that promote anxiety about AI (Noble, 2018; Benjamin, 2019; Eubanks, 2018). Lower institutional trust among Black Americans, documented in health and technology domains (Boulware et al., 2003), probably amplifies the effect of low education on technology anxiety. The historical exclusion of Black communities from technology development and governance adds a further layer, creating a context where lower education more strongly predicts feeling threatened by technologies perceived as imposed rather than chosen (Benjamin, 2019).

The unusual Hispanic pattern, where some college education predicts increased Advantaged membership, likely reflects generational dynamics within Hispanic communities, where younger, more educated individuals are engaged with technology at higher rates in bilingual digital environments and serve as technology brokers for their communities (Katz and Gonzalez, 2016). These intersectional findings reinforce the argument by Robinson et al. (2015) and Ragnedda and Muschert (2013) that digital divides cannot be understood through single-axis analyses.

### 5.5 Stability and Change

Between structural stability and distributional change, the cross-wave analysis reveals a productive tension. A basic divide between a more engaged class and a more moderate class persists across all three waves (2022--2024), and the education gradient within the engaged class remains consistent. Yet the *content* shifts -- from optimism (December 2022, before widespread ChatGPT awareness) to skepticism (August 2023 onward). This decoupling of structural stability from attitudinal content carries an important implication: the AI divide is at its core about differential *engagement* with technology, not differential enthusiasm. Higher-SES individuals consistently hold more firmly defined attitudes; what changes is whether those attitudes are positive or negative.

### 5.6 Future Research Directions

What our findings most urgently raise is a causal question: do changes in AI awareness precede attitude shifts, or is the relationship more reciprocal? Longitudinal panel data tracking the same individuals through the rapid diffusion of generative AI tools (2022--2024) would allow researchers to observe profile transitions and test the sequential structure our mediation model assumes. Complementary qualitative work could illuminate what "uninformed" means in practice -- whether these individuals are genuinely unaware of AI or encounter algorithmic systems without recognizing them as such, and whether the Anxious class responds to specific negative experiences or to broader narratives of technology risk. Cross-national comparison would reveal whether the four-profile structure is specific to the American context or generalizes to societies with different AI governance regimes and media environments (Helsper, 2012). Finally, linking attitudinal profiles to behavioral outcomes -- adoption rates, service avoidance, passive acceptance of AI-mediated decisions -- would strengthen the practical relevance of the typology and test whether the AI divide has material consequences beyond the attitudinal domain.

### 5.7 Limitations

Several limitations should be acknowledged. Modal class assignments in the structural models introduce classification error that may attenuate estimates; while three-step approaches partially address this concern (Vermunt, 2010), future work could implement bias-adjusted methods. Because the primary analysis is cross-sectional, causal inference about the SES-to-awareness-to-class pathway remains out of reach, and longitudinal panel data would strengthen the mediation claims. The Pew ATP, while nationally representative, is an online panel, which may underrepresent individuals with the lowest levels of digital engagement -- precisely those most likely to be in the Uninformed class. Cross-wave validation was limited to two indicators, producing a coarser classification than the primary six-indicator models. AI awareness serves a dual role in our analysis -- as an LCA indicator that helps define the latent classes and as a predictor in the structural models. This partial circularity likely inflates the magnitude of awareness effects in the multinomial models, and the dominance of awareness as a predictor (AME = +1.67 for Advantaged membership) should be interpreted with this caveat in mind. Finally, the moderate entropy values in our LCA models (0.643--0.659) indicate that some respondents are not crisply classified, though these values are within acceptable ranges for applied LCA (Clark and Muthen, 2009).

### 5.8 Implications for Policy and Practice

One-third of Americans are effectively absent from public deliberation about AI. The existence of a large Uninformed class (33%) indicates that this absence stems not from opposition but from a lack of the awareness needed to form opinions. Public engagement strategies that assume a baseline of AI knowledge may systematically exclude this segment of the adult population.

The awareness-mediation finding points to the potential for AI literacy initiatives to partially reduce the AI divide, but two caveats are essential. First, the intersectional results caution that one-size-fits-all approaches may be insufficient; the different education gradients by race/ethnicity imply that AI literacy programs should be sensitive to community-specific concerns -- addressing algorithmic bias and surveillance for Black communities, leveraging bilingual digital engagement for Hispanic communities. Second, framing the Uninformed class purely as an information deficit risks reproducing the "deficit model" critiqued in public understanding of science research (Wynne, 1992). The one-third of Americans who lack AI awareness may not simply be missing information; their disengagement may reflect rational responses to structural exclusion from technology development, a lack of perceived relevance rooted in material circumstances, or distrust of institutions that promote technological change (Couldry and Mejias, 2019). Effective AI engagement strategies must therefore address structural conditions alongside informational ones.

For technology companies and policymakers, the four-profile typology offers a more actionable segmentation than simple favorable/unfavorable polling. The Anxious class (9%) needs reassurance grounded in specific domain evidence, not generic claims about AI benefits. The Uninformed class (33%) needs basic awareness before they can meaningfully participate in governance conversations. The Ambivalent class (37%) is engaged enough to provide substantive feedback but needs accessible channels for input. And the Advantaged class (21%), while well-positioned to participate in technical governance, may not represent broader public concerns.

## 6. Conclusion

This study introduced a three-level AI divide framework and documented four distinct orientations toward artificial intelligence in the American public. The AI-Anxious, AI-Uninformed, AI-Advantaged, and AI-Ambivalent profiles capture qualitative differences in how people relate to AI -- differences that are systematically structured by socioeconomic status, mediated by awareness, and moderated by race/ethnicity. These findings extend the digital divide framework from technology access and use into the domain of technology attitudes, arguing that how people *think about* AI is no less stratified than how they *use* the internet.

The AI divide matters because it shapes who participates in the governance of a technology that will increasingly affect everyone. If the most informed, most engaged orientations toward AI are concentrated among higher-SES, predominantly White and Asian populations, then the voices shaping AI policy risk reflecting a narrow slice of the public. Our findings indicate that awareness -- not enthusiasm, not access, but basic knowledge of AI's existence and relevance -- is the critical first step. Bridging the awareness divide will not eliminate disagreement about AI, nor should it. But it can ensure that all segments of the public have the foundation needed to participate in what is likely the most important technology debate of the coming decades.

---

## References

Araujo T, Helberger N, Kruikemeier S and de Vreese CH (2020) In AI we trust? Perceptions about automated decision-making by artificial intelligence. *AI & Society* 35(3): 611--623.

Arel-Bundock V, Greifer N and Heiss A (2024) How to interpret statistical models using marginaleffects for R and Python. *Journal of Statistical Software* 111(9): 1--32.

Asparouhov T and Muthén B (2014) Auxiliary variables in mixture modeling: Three-step approaches using Mplus. *Structural Equation Modeling* 21(3): 329--341.

Benjamin R (2019) *Race After Technology: Abolitionist Tools for the New Jim Code*. Polity Press.

Boulware LE, Cooper LA, Ratner LE, LaVeist TA and Powe NR (2003) Race and trust in the health care system. *Public Health Reports* 118(4): 358--365.

Brossard D, Scheufele DA, Kim E and Lewenstein BV (2009) Religiosity as a perceptual filter: Examining processes of opinion formation about nanotechnology. *Public Understanding of Science* 18(5): 546--558.

Bucher T (2017) The algorithmic imaginary: Exploring the ordinary affects of Facebook algorithms. *Information, Communication & Society* 20(1): 30--44.

Cave S, Coughlan K and Dihal K (2019) Scary robots: Examining public responses to AI. In: *Proceedings of the 2019 AAAI/ACM Conference on AI, Ethics, and Society*, pp. 331--337.

Clark SL and Muthén B (2009) Relating latent class analysis results to variables not included in the analysis. Unpublished manuscript.

Collins LM and Lanza ST (2010) *Latent Class and Latent Transition Analysis: With Applications in the Social, Behavioral, and Health Sciences*. Wiley.

Couldry N and Mejias UA (2019) *The Costs of Connection: How Data Is Colonizing Human Life and Appropriating It for Capitalism*. Stanford University Press.

DiMaggio P, Hargittai E, Celeste C and Shafer S (2004) Digital inequality: From unequal access to differentiated use. In: Neckerman K (ed.) *Social Inequality*. Russell Sage Foundation, pp. 355--400.

Eubanks V (2018) *Automating Inequality: How High-Tech Tools Profile, Police, and Punish the Poor*. St. Martin's Press.

Eurobarometer (2017) *Attitudes Towards the Impact of Digitisation and Automation on Daily Life*. Special Eurobarometer 460. European Commission.

Floridi L, Cowls J, Beltrametti M, et al. (2018) AI4People -- An ethical framework for a good AI society. *Minds and Machines* 28(4): 689--707.

Hagenaars JA and McCutcheon AL (eds) (2002) *Applied Latent Class Analysis*. Cambridge University Press.

Hargittai E (2002) Second-level digital divide: Differences in people's online skills. *First Monday* 7(4).

Hargittai E and Hinnant A (2008) Digital inequality: Differences in young adults' use of the internet. *Communication Research* 35(5): 602--621.

Hargittai E and Hsieh YP (2012) Succinct survey measures of web-use skills. *Social Science Computer Review* 30(1): 95--107.

Helsper EJ (2012) A corresponding fields model for the links between social and digital exclusion. *Communication Theory* 22(4): 403--426.

Ipsos (2023) *Global Views on AI 2023*. Ipsos.

Jasanoff S (2015) Future imperfect: Science, technology, and the imaginations of modernity. In: Jasanoff S and Kim SH (eds) *Dreamscapes of Modernity: Sociotechnical Imaginaries and the Fabrication of Power*. University of Chicago Press, pp. 1--33.

Katz VS and Gonzalez C (2016) Toward meaningful connectivity: Using multilevel communication research to reframe digital inequality. *Journal of Communication* 66(2): 236--249.

Lazarsfeld PF and Henry NW (1968) *Latent Structure Analysis*. Houghton Mifflin.

Linzer DA and Lewis JB (2011) poLCA: An R package for polytomous variable latent class analysis. *Journal of Statistical Software* 42(10): 1--29.

Neudert LM, Knuutila A and Howard PN (2020) *Global Attitudes Towards AI, Machine Learning & Automated Decision Making*. Oxford Internet Institute.

Noble SU (2018) *Algorithms of Oppression: How Search Engines Reinforce Racism*. NYU Press.

NTIA (National Telecommunications and Information Administration) (1999) *Falling Through the Net: Defining the Digital Divide*. U.S. Department of Commerce.

Nylund KL, Asparouhov T and Muthén BO (2007) Deciding on the number of classes in latent class analysis and growth mixture modeling: A Monte Carlo simulation study. *Structural Equation Modeling* 14(4): 535--569.

Pew Research Center (2023) *Growing Public Concern About the Role of Artificial Intelligence in Daily Life*. Pew Research Center.

Ragnedda M (2018) Conceptualizing digital capital. *Telematics and Informatics* 35(8): 2366--2375.

Ragnedda M and Muschert GW (eds) (2013) *The Digital Divide: The Internet and Social Inequality in International Perspective*. Routledge.

Robinson L, Cotten SR, Ono H, et al. (2015) Digital inequalities and why they matter. *Information, Communication & Society* 18(5): 569--582.

Rosseel Y (2012) lavaan: An R package for structural equation modeling. *Journal of Statistical Software* 48(2): 1--36.

Savage M, Devine F, Cunningham N, et al. (2013) A new model of social class? Findings from the BBC's Great British Class Survey experiment. *Sociology* 47(2): 219--250.

van Dijk JAGM (2005) *The Deepening Divide: Inequality in the Information Society*. Sage.

van Dijk JAGM (2020) *The Digital Divide*. Polity Press.

Vermunt JK (2010) Latent class modeling with covariates: Two improved three-step approaches. *Political Analysis* 18(4): 450--469.

Viswanath K and Bond K (2007) Social determinants and nutrition: Reflections on the role of communication. *Journal of Nutrition Education and Behavior* 39(2): S20--S26.

Wang X, Bruns A, Wiertz C and Marquart F (2025) Mapping AI competence profiles: A latent class analysis of AI-related skills and knowledge in the Netherlands. *New Media & Society*. Epub ahead of print. DOI: 10.1177/14614448251315940.

Wei L, Teo HH, Chan HC and Tan BCY (2011) Conceptualizing and testing a social cognitive model of the digital divide. *Information Systems Research* 22(1): 170--187.

Wynne B (1992) Misunderstood misunderstanding: Social identities and public uptake of science. *Public Understanding of Science* 1(3): 281--304.

Zhang B and Dafoe A (2019) Artificial intelligence: American attitudes and trends. Center for the Governance of AI, Future of Humanity Institute, University of Oxford.

---

## Tables and Figures

**Table 1.** Latent class model fit indices for Form A and Form B (W132). Models with 2--7 classes shown. BIC-optimal model indicated in bold; selection criteria: lowest BIC with entropy > 0.6 and minimum class > 5%.

**Table 2.** Cross-form profile distances (Euclidean distance in shared-indicator space). Lower values indicate greater similarity between matched class profiles.

| Form A Class | Form B Match | Distance | Label |
|:---|:---|:---:|:---|
| A_C1 | B_C2 | 0.076 | AI-Anxious |
| A_C4 | B_C1 | 0.071 | AI-Ambivalent |
| A_C3 | B_C3 | 0.313 | AI-Advantaged |
| A_C2 | B_C4 | 0.240 | AI-Uninformed |

**Table 3.** Multinomial logistic regression model comparison. Reference class: largest class (Ambivalent). All models estimated on Form A (N = 5,368).

| Model | Log-Likelihood | df | AIC | BIC | LRT chi-sq | LRT df | LRT p |
|:---|---:|---:|---:|---:|---:|---:|:---|
| Base (demographics + race) | -6,379.1 | 30 | 12,818.3 | 13,014.5 | -- | -- | -- |
| + SES | -6,313.8 | 42 | 12,711.6 | 12,986.3 | 131 | 12 | < .001 |
| + Awareness | -4,767.9 | 33 | 9,601.9 | 9,817.7 | 3,222 | 3 | < .001 |
| Full (all predictors) | -4,746.8 | 45 | 9,583.6 | 9,877.9 | 3,265 | 15 | < .001 |

**Table 4.** Average marginal effects on class membership probability from full multinomial logistic model (selected predictors). Reference categories: College graduate+ (education), Middle (income), White non-Hispanic (race), 30--49 (age), Male (gender), Democrat/Lean Dem (party).

| Predictor | Anxious | Uninformed | Advantaged | Ambivalent |
|:---|:---:|:---:|:---:|:---:|
| AI awareness (1-unit) | -0.198*** | -0.502*** | +1.669*** | -0.970*** |
| HS or less | +0.046*** | -0.011 | +0.002 | -0.037* |
| Some college | +0.022* | -0.011 | +0.004 | -0.016 |
| Lower income | +0.015 | +0.000 | +0.006 | -0.021 |
| Upper income | -0.015 | +0.027 | +0.033** | -0.045* |
| Hispanic | -0.023* | +0.033 | -0.011 | +0.001 |
| Black NH | +0.014 | -0.015 | -0.016 | +0.017 |
| Asian NH | -0.032* | +0.046 | +0.063*** | -0.077** |
| Republican | +0.062*** | -0.055*** | -0.019* | +0.012 |
| Female | +0.021** | -0.008 | -0.037*** | +0.024 |
| Age 65+ | -0.048*** | +0.098*** | +0.024 | -0.074*** |

Note: \*p < .05, \*\*p < .01, \*\*\*p < .001.

**Table 5.** Indirect effects of SES on class membership through AI awareness (bootstrap, 1,000 replications).

| SES Predictor | Class Outcome | Indirect Effect (beta) | 95% CI | p |
|:---|:---|:---:|:---|:---|
| HS or less | AI-Advantaged | -0.103 | [-0.123, -0.085] | < .001 |
| HS or less | AI-Uninformed | +0.115 | [0.094, 0.137] | < .001 |
| HS or less | AI-Anxious | -0.005 | [-0.009, -0.001] | .013 |
| Upper income | AI-Advantaged | +0.041 | [0.027, 0.055] | < .001 |
| Upper income | AI-Uninformed | -0.046 | [-0.062, -0.030] | < .001 |

**Table 6.** Education coefficients (log-odds) on Anxious class membership from multi-group multinomial logistic regression, by race/ethnicity (selected results).

| Race/Ethnicity | Education Level | Log-Odds (Anxious) | SE | p |
|:---|:---|:---:|:---:|:---|
| White NH | HS or less | +0.386 | 0.168 | .022 |
| Black NH | HS or less | +1.096 | 0.499 | .028 |
| Hispanic | Some college | -0.234 | 0.448 | .602 |
| Hispanic | HS or less | +0.307 | 0.402 | .445 |

Education-by-race interaction LRT: p = .007.

**Table 7.** Cross-wave LCA validation: BIC-optimal two-class solutions across survey waves.

| Wave | Date | N | BIC (2-class) | Class Labels |
|:---|:---|---:|---:|:---|
| W119 | Dec 2022 | 10,906 | 41,667.3 | AI-Optimistic + Ambivalent |
| W132 | Aug 2023 | 5,368 | 19,259.0 | AI-Skeptic + Ambivalent |
| W152 | Aug 2024 | 5,363 | 19,163.5 | Ambivalent + AI-Skeptic |

Cross-wave chi-square test of class proportions: X2 = 12,133.6, df = 4, p < .001.

**Figure 1.** Class-conditional response probabilities for the four-class LCA solution (Form A). Bars represent the probability of each response category within each latent class. Top indicators (attitude, awareness) are shared across forms; bottom indicators (domains a--d) are Form A-specific.

**Figure 2.** Average marginal effects of key predictors on latent class membership probability (full model). Points represent AMEs with 95% confidence intervals. Reference categories: College graduate+, Middle income, White NH, Ages 30--49, Male, Democrat/Lean Dem.

**Figure 3.** Mediation path diagram: SES to AI awareness to latent class membership. Standardized coefficients shown for the AI-Advantaged class outcome. Dashed lines represent indirect paths; solid lines represent direct paths.

**Figure 4.** Predicted probability of latent class membership by education level and race/ethnicity. Predictions from the full multinomial logistic model, holding other variables at reference/median values.

**Figure 5.** Cross-wave profile comparison: class-conditional probabilities on shared indicators (attitude and awareness) across W119 (December 2022), W132 (August 2023), and W152 (August 2024).

**Figure 6.** Education gradient in AI-Optimistic/Engaged class membership across survey waves. Proportion of each education group assigned to the most engaged class, with 95% confidence intervals.

---

*Author note:* Data from the Pew Research Center American Trends Panel are publicly available. Replication code and materials are available from the authors upon request. Correspondence should be addressed to [Author information].

*Funding:* [Funding information].

*Declaration of interest:* The authors declare no conflicts of interest.
