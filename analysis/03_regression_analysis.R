# =============================================================================
# 03_regression_analysis.R
# AI Attitude Polarization Study — Pew ATP Data
#
# Purpose:  Main inferential analyses testing demographic predictors of
#           AI attitudes and whether polarization changed across three survey
#           waves (Dec 2022, Aug 2023, Aug 2024).
#
# Input:    data/processed/pew_combined.rds
# Outputs:
#   output/tables/table4_multinomial_full.csv      — Analysis 1
#   output/tables/table5_logistic_by_wave.csv      — Analysis 2
#   output/tables/table6_interaction_models.csv    — Analysis 3
#   output/tables/table7_model_comparison.csv      — Analysis 5
#   output/figures/fig6_predicted_party_polarization.png  — Analysis 4
#   output/figures/fig7_predicted_education_gap.png       — Analysis 4
#
# Author:  [Your Name]
# Date:    2026-02-22
# =============================================================================


# =============================================================================
# 0. SETUP
# =============================================================================

# ---- 0a. Install / load packages --------------------------------------------

required_pkgs <- c(
  "tidyverse", "survey", "srvyr", "nnet", "broom",
  "scales", "ggplot2", "emmeans", "marginaleffects"
)

invisible(lapply(required_pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, quiet = TRUE)
  }
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

cat("Packages loaded.\n")

# ---- 0b. Paths ---------------------------------------------------------------

project_root <- tryCatch(here::here(), error = function(e) {
  "/Users/hosung/AI_Polarization_Pew"
})

data_path   <- file.path(project_root, "data/processed/pew_combined.rds")
out_tables  <- file.path(project_root, "output/tables")
out_figures <- file.path(project_root, "output/figures")

dir.create(out_tables,  showWarnings = FALSE, recursive = TRUE)
dir.create(out_figures, showWarnings = FALSE, recursive = TRUE)

# ---- 0c. Color palette -------------------------------------------------------

col_rep   <- "#B2182B"   # red  — Republican
col_dem   <- "#2166AC"   # blue — Democrat
col_gray  <- "#999999"   # gray — neutral / third category
col_edu_hi  <- "#2166AC"
col_edu_mid <- "#999999"
col_edu_lo  <- "#B2182B"

# ---- 0d. ggplot theme --------------------------------------------------------

theme_pub <- function(...) {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(size = 11, color = "grey40"),
      axis.title       = element_text(size = 11),
      axis.text        = element_text(size = 10),
      legend.title     = element_text(size = 11),
      legend.text      = element_text(size = 10),
      panel.grid.minor = element_blank(),
      ...
    )
}

# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

cat("\n--- Loading data ---\n")
df_raw <- readRDS(data_path)
cat(sprintf("Raw observations: %d\n", nrow(df_raw)))

# ---- 1a. Factor reference levels (explicit, reproducible) -------------------

df <- df_raw %>%
  mutate(
    # DV
    cncexc     = factor(cncexc, levels = c("Equally", "Excited", "Concerned")),
    concerned  = as.integer(concerned),
    excited    = as.integer(excited),

    # Time
    wave       = as.integer(wave),
    wave_date  = factor(wave_date,
                        levels = c("Dec 2022", "Aug 2023", "Aug 2024"),
                        ordered = FALSE),           # treat as nominal in models

    # Demographics — set reference categories explicitly
    age_cat    = relevel(factor(age_cat,
                                levels = c("18-29", "30-49", "50-64", "65+")),
                         ref = "30-49"),
    gender     = relevel(factor(gender,
                                levels = c("Male", "Female", "Other")),
                         ref = "Male"),
    education  = relevel(factor(education,
                                levels = c("College graduate+",
                                           "Some College",
                                           "HS graduate or less")),
                         ref = "College graduate+"),
    race       = relevel(factor(race,
                                levels = c("White non-Hispanic",
                                           "Black non-Hispanic",
                                           "Hispanic",
                                           "Other non-Hispanic",
                                           "Asian non-Hispanic")),
                         ref = "White non-Hispanic"),
    party      = relevel(factor(party,
                                levels = c("Dem/Lean Dem", "Rep/Lean Rep")),
                         ref = "Dem/Lean Dem"),
    party_ideo = factor(party_ideo,
                        levels = c("Lib Dem/Lean Dem",
                                   "Mod/Cons Dem/Lean Dem",
                                   "Mod/Lib Rep/Lean Rep",
                                   "Cons Rep/Lean Rep")),
    income_tier = relevel(factor(income_tier,
                                 levels = c("Lower", "Middle", "Upper")),
                          ref = "Middle"),
    region     = relevel(factor(region,
                                levels = c("Northeast", "Midwest",
                                           "South", "West")),
                         ref = "South"),
    metro      = factor(metro,
                        levels = c("Metropolitan", "Non-metropolitan")),
    ai_heard   = relevel(factor(ai_heard,
                                levels = c("A lot", "A little",
                                           "Nothing")),
                         ref = "A lot")
  )

# ---- 1b. Complete-case subset for main models --------------------------------

main_vars <- c("cncexc", "concerned", "wave_date", "wave",
               "age_cat", "gender", "education", "race",
               "party", "income_tier", "ai_heard", "weight")

df_cc <- df %>%
  select(all_of(main_vars)) %>%
  filter(party %in% c("Dem/Lean Dem", "Rep/Lean Rep")) %>%   # drop pure ind.
  drop_na()

cat(sprintf("Complete-case analytic N: %d (dropped %d)\n",
            nrow(df_cc), nrow(df) - nrow(df_cc)))

# ---- 1c. Survey design objects -----------------------------------------------

# Full stacked design (all waves)
svy_full <- svydesign(ids = ~1,
                      weights = ~weight,
                      data = df_cc,
                      na.action = na.omit)

# Wave-specific designs
svy_w <- lapply(1:3, function(w) {
  d <- df_cc %>% filter(wave == w)
  svydesign(ids = ~1, weights = ~weight, data = d, na.action = na.omit)
})
names(svy_w) <- paste0("wave", 1:3)

cat("Survey designs created.\n")


# =============================================================================
# 2. HELPER FUNCTIONS
# =============================================================================

# ---- 2a. Extract tidy OR table from svyglm ----------------------------------

tidy_svyglm <- function(model, wave_label = NULL, model_label = NULL) {
  coef_mat <- coef(summary(model))
  ci       <- confint(model)

  tbl <- tibble(
    term      = rownames(coef_mat),
    estimate  = coef_mat[, "Estimate"],
    std_error = coef_mat[, "Std. Error"],
    z_value   = coef_mat[, "t value"],
    p_value   = coef_mat[, "Pr(>|t|)"],
    OR        = exp(estimate),
    OR_lo95   = exp(ci[, 1]),
    OR_hi95   = exp(ci[, 2])
  )

  if (!is.null(wave_label))  tbl <- mutate(tbl, wave  = wave_label)
  if (!is.null(model_label)) tbl <- mutate(tbl, model = model_label)
  tbl
}

# ---- 2b. Significance stars --------------------------------------------------

sig_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.10  ~ ".",
    TRUE       ~ ""
  )
}

# ---- 2c. Round and format OR table for export --------------------------------

format_or_table <- function(tbl) {
  tbl %>%
    mutate(
      sig       = sig_stars(p_value),
      OR_fmt    = sprintf("%.3f", OR),
      CI_fmt    = sprintf("[%.3f, %.3f]", OR_lo95, OR_hi95),
      p_fmt     = ifelse(p_value < 0.001, "<0.001",
                         sprintf("%.3f", p_value))
    )
}


# =============================================================================
# ANALYSIS 1: MULTINOMIAL LOGISTIC REGRESSION (FULL SAMPLE)
# =============================================================================

cat("\n=== ANALYSIS 1: Multinomial Logistic Regression ===\n")

# ---- 1. Fit model ------------------------------------------------------------
# nnet::multinom does not natively support survey weights as probability
# weights in the survey-design sense, but accepts a 'weights' argument that
# functions as importance weights.  We pass the normalized survey weight.
# Reference category: "Equally"

multinom_formula <- cncexc ~ wave_date + age_cat + gender + education +
  race + party + income_tier + ai_heard

set.seed(20240822)
m_multinom <- nnet::multinom(
  multinom_formula,
  data    = df_cc,
  weights = weight,
  maxit   = 500,
  trace   = FALSE
)

cat(sprintf("Multinomial model converged: %s\n",
            m_multinom$convergence == 0))
cat(sprintf("Analytic N (multinom): %d\n", nrow(m_multinom$fitted.values)))

# ---- 2. Extract coefficients (two equations: Excited vs Equally,
#         Concerned vs Equally) -----------------------------------------------

coef_multi <- coef(m_multinom)          # 2 x p matrix
vcov_multi  <- vcov(m_multinom)
se_multi    <- sqrt(diag(vcov_multi))

n_coef <- ncol(coef_multi)

# z-scores and p-values manually
z_excited   <- coef_multi["Excited",   ] / se_multi[1:n_coef]
z_concerned <- coef_multi["Concerned", ] / se_multi[(n_coef + 1):(2 * n_coef)]

p_excited   <- 2 * pnorm(-abs(z_excited))
p_concerned <- 2 * pnorm(-abs(z_concerned))

# 95% CI on log-odds scale, then exponentiate
z_crit <- qnorm(0.975)

tbl_multinom_excited <- tibble(
  outcome   = "Excited vs Equally",
  term      = names(coef_multi["Excited", ]),
  log_OR    = as.numeric(coef_multi["Excited", ]),
  se        = se_multi[1:n_coef],
  OR        = exp(log_OR),
  OR_lo95   = exp(log_OR - z_crit * se),
  OR_hi95   = exp(log_OR + z_crit * se),
  z_value   = z_excited,
  p_value   = p_excited,
  sig       = sig_stars(p_excited)
)

tbl_multinom_concerned <- tibble(
  outcome   = "Concerned vs Equally",
  term      = names(coef_multi["Concerned", ]),
  log_OR    = as.numeric(coef_multi["Concerned", ]),
  se        = se_multi[(n_coef + 1):(2 * n_coef)],
  OR        = exp(log_OR),
  OR_lo95   = exp(log_OR - z_crit * se),
  OR_hi95   = exp(log_OR + z_crit * se),
  z_value   = z_concerned,
  p_value   = p_concerned,
  sig       = sig_stars(p_concerned)
)

tbl4 <- bind_rows(tbl_multinom_excited, tbl_multinom_concerned) %>%
  mutate(
    OR_fmt  = sprintf("%.3f", OR),
    CI_fmt  = sprintf("[%.3f, %.3f]", OR_lo95, OR_hi95),
    p_fmt   = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  ) %>%
  select(outcome, term, OR, OR_lo95, OR_hi95, OR_fmt, CI_fmt,
         z_value, p_value, p_fmt, sig)

write_csv(tbl4,
          file.path(out_tables, "table4_multinomial_full.csv"))
cat("Saved: table4_multinomial_full.csv\n")
cat("Reference category for DV: 'Equally'\n")
cat("Reference category for party: 'Dem/Lean Dem'\n")
cat("Reference category for education: 'College graduate+'\n")
cat("Reference category for wave_date: 'Dec 2022'\n")


# =============================================================================
# ANALYSIS 2: WAVE-BY-WAVE BINARY LOGISTIC REGRESSION
# =============================================================================

cat("\n=== ANALYSIS 2: Wave-by-Wave Binary Logistic (Concerned vs Not) ===\n")

logit_formula_wave <- concerned ~ age_cat + gender + education +
  race + party + income_tier + ai_heard

wave_labels <- c("Dec 2022", "Aug 2023", "Aug 2024")

tbl5_list <- vector("list", 3)

for (w in 1:3) {
  cat(sprintf("  Fitting wave %d (%s)...\n", w, wave_labels[w]))

  m_w <- svyglm(
    logit_formula_wave,
    design = svy_w[[w]],
    family = quasibinomial(link = "logit"),
    na.action = na.omit
  )

  n_w <- nrow(model.frame(m_w))
  cat(sprintf("    N = %d\n", n_w))

  tbl5_list[[w]] <- tidy_svyglm(m_w,
                                  wave_label  = wave_labels[w],
                                  model_label = paste0("Wave_", w)) %>%
    mutate(analytic_n = n_w)
}

tbl5 <- bind_rows(tbl5_list) %>%
  format_or_table() %>%
  select(wave, model, term, analytic_n, OR, OR_lo95, OR_hi95,
         OR_fmt, CI_fmt, z_value, p_value, p_fmt, sig)

write_csv(tbl5,
          file.path(out_tables, "table5_logistic_by_wave.csv"))
cat("Saved: table5_logistic_by_wave.csv\n")


# =============================================================================
# ANALYSIS 3: INTERACTION MODELS (KEY HYPOTHESES)
# =============================================================================

cat("\n=== ANALYSIS 3: Interaction Models (Wave × Demographics) ===\n")

# Common covariates (not in the focal interaction)
base_covars <- "age_cat + gender + education + race + party + income_tier + ai_heard"

# ---- 3a. Model A: wave × party -----------------------------------------------

formula_A_int  <- as.formula(
  paste("concerned ~ wave * party + age_cat + gender + education +",
        "race + income_tier + ai_heard"))
formula_A_main <- as.formula(
  paste("concerned ~ wave + party + age_cat + gender + education +",
        "race + income_tier + ai_heard"))

m_A_int  <- svyglm(formula_A_int,  design = svy_full,
                   family = quasibinomial(), na.action = na.omit)
m_A_main <- svyglm(formula_A_main, design = svy_full,
                   family = quasibinomial(), na.action = na.omit)

n_A <- nrow(model.frame(m_A_int))
cat(sprintf("  Model A (wave × party) N = %d\n", n_A))

tbl_A <- tidy_svyglm(m_A_int,
                      model_label = "A_wave_x_party") %>%
  mutate(analytic_n = n_A, focal_interaction = "wave × party")

# ---- 3b. Model B: wave × education ------------------------------------------

formula_B_int  <- as.formula(
  paste("concerned ~ wave * education + age_cat + gender + race +",
        "party + income_tier + ai_heard"))
formula_B_main <- as.formula(
  paste("concerned ~ wave + education + age_cat + gender + race +",
        "party + income_tier + ai_heard"))

m_B_int  <- svyglm(formula_B_int,  design = svy_full,
                   family = quasibinomial(), na.action = na.omit)
m_B_main <- svyglm(formula_B_main, design = svy_full,
                   family = quasibinomial(), na.action = na.omit)

n_B <- nrow(model.frame(m_B_int))
cat(sprintf("  Model B (wave × education) N = %d\n", n_B))

tbl_B <- tidy_svyglm(m_B_int,
                      model_label = "B_wave_x_education") %>%
  mutate(analytic_n = n_B, focal_interaction = "wave × education")

# ---- 3c. Model C: wave × age_cat --------------------------------------------

formula_C_int  <- as.formula(
  paste("concerned ~ wave * age_cat + gender + education + race +",
        "party + income_tier + ai_heard"))
formula_C_main <- as.formula(
  paste("concerned ~ wave + age_cat + gender + education + race +",
        "party + income_tier + ai_heard"))

m_C_int  <- svyglm(formula_C_int,  design = svy_full,
                   family = quasibinomial(), na.action = na.omit)
m_C_main <- svyglm(formula_C_main, design = svy_full,
                   family = quasibinomial(), na.action = na.omit)

n_C <- nrow(model.frame(m_C_int))
cat(sprintf("  Model C (wave × age_cat) N = %d\n", n_C))

tbl_C <- tidy_svyglm(m_C_int,
                      model_label = "C_wave_x_age") %>%
  mutate(analytic_n = n_C, focal_interaction = "wave × age_cat")

# ---- 3d. Combine and export --------------------------------------------------

tbl6 <- bind_rows(tbl_A, tbl_B, tbl_C) %>%
  format_or_table() %>%
  select(model, focal_interaction, term, analytic_n,
         OR, OR_lo95, OR_hi95, OR_fmt, CI_fmt,
         z_value, p_value, p_fmt, sig) %>%
  arrange(model, term)

write_csv(tbl6,
          file.path(out_tables, "table6_interaction_models.csv"))
cat("Saved: table6_interaction_models.csv\n")

# Print interaction terms for quick inspection
cat("\n  Key interaction terms (wave × party, OR):\n")
tbl6 %>%
  filter(model == "A_wave_x_party", grepl("wave:", term)) %>%
  select(term, OR_fmt, CI_fmt, p_fmt, sig) %>%
  print(n = Inf)

cat("\n  Key interaction terms (wave × education, OR):\n")
tbl6 %>%
  filter(model == "B_wave_x_education", grepl("wave:", term)) %>%
  select(term, OR_fmt, CI_fmt, p_fmt, sig) %>%
  print(n = Inf)

cat("\n  Key interaction terms (wave × age_cat, OR):\n")
tbl6 %>%
  filter(model == "C_wave_x_age", grepl("wave:", term)) %>%
  select(term, OR_fmt, CI_fmt, p_fmt, sig) %>%
  print(n = Inf)


# =============================================================================
# ANALYSIS 4: PREDICTED PROBABILITIES PLOTS
# =============================================================================

cat("\n=== ANALYSIS 4: Predicted Probabilities ===\n")

# ---- 4a. Marginal predicted probabilities via a grid approach ----------------
# Strategy: build a prediction grid holding all covariates at their modal
# (categorical) or mean (continuous) values, then vary the focal variables.

modal_age     <- names(sort(table(df_cc$age_cat),    decreasing = TRUE))[1]
modal_gender  <- names(sort(table(df_cc$gender),     decreasing = TRUE))[1]
modal_educ    <- names(sort(table(df_cc$education),  decreasing = TRUE))[1]
modal_race    <- names(sort(table(df_cc$race),       decreasing = TRUE))[1]
modal_party   <- names(sort(table(df_cc$party),      decreasing = TRUE))[1]
modal_income  <- names(sort(table(df_cc$income_tier),decreasing = TRUE))[1]
modal_aiheard <- names(sort(table(df_cc$ai_heard),   decreasing = TRUE))[1]
mean_weight   <- mean(df_cc$weight)

wave_vals <- c(1, 2, 3)
wave_labels_plot <- c("Dec 2022", "Aug 2023", "Aug 2024")

# ---- 4b. Plot: wave × party --------------------------------------------------

cat("  Computing predicted probabilities: wave × party\n")

grid_party <- expand.grid(
  wave      = wave_vals,
  party     = levels(df_cc$party),
  age_cat   = modal_age,
  gender    = modal_gender,
  education = modal_educ,
  race      = modal_race,
  income_tier = modal_income,
  ai_heard  = modal_aiheard,
  weight    = mean_weight,
  stringsAsFactors = FALSE
) %>%
  mutate(
    wave      = as.integer(wave),
    party     = factor(party,     levels = levels(df_cc$party)),
    age_cat   = factor(age_cat,   levels = levels(df_cc$age_cat)),
    gender    = factor(gender,    levels = levels(df_cc$gender)),
    education = factor(education, levels = levels(df_cc$education)),
    race      = factor(race,      levels = levels(df_cc$race)),
    income_tier = factor(income_tier, levels = levels(df_cc$income_tier)),
    ai_heard  = factor(ai_heard,  levels = levels(df_cc$ai_heard))
  )

# Predictions and SEs from Model A interaction
# predict.svyglm returns a svystat object; extract values with as.numeric/SE
pred_A <- predict(m_A_int,
                  newdata = grid_party,
                  type    = "link",
                  se      = TRUE)

grid_party <- grid_party %>%
  mutate(
    fit     = as.numeric(pred_A),
    se_fit  = as.numeric(SE(pred_A)),
    prob    = plogis(fit),
    prob_lo = plogis(fit - 1.96 * se_fit),
    prob_hi = plogis(fit + 1.96 * se_fit),
    wave_label = factor(wave_vals[wave],
                        levels = wave_vals,
                        labels = wave_labels_plot)
  )

fig6 <- ggplot(grid_party,
               aes(x = wave_label, y = prob,
                   color = party, group = party,
                   ymin = prob_lo, ymax = prob_hi)) +
  geom_ribbon(aes(fill = party), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("Dem/Lean Dem" = col_dem, "Rep/Lean Rep" = col_rep),
    name   = "Party ID"
  ) +
  scale_fill_manual(
    values = c("Dem/Lean Dem" = col_dem, "Rep/Lean Rep" = col_rep),
    name   = "Party ID"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    title    = "Predicted Probability of Being 'More Concerned than Excited'\nAbout AI, by Party Identification",
    subtitle = "Covariates held at modal values; 95% CI shown",
    x        = "Survey Wave",
    y        = "Predicted Probability (Concerned)",
    caption  = paste0("Note: Predictions from logistic regression with wave × party interaction.\n",
                      "Reference: Democrat/Lean Democrat. N = ", n_A, ".")
  ) +
  theme_pub()

ggsave(file.path(out_figures, "fig6_predicted_party_polarization.png"),
       plot   = fig6,
       width  = 8,
       height = 5,
       dpi    = 300,
       bg     = "white")
cat("Saved: fig6_predicted_party_polarization.png\n")

# ---- 4c. Plot: wave × education ---------------------------------------------

cat("  Computing predicted probabilities: wave × education\n")

grid_educ <- expand.grid(
  wave      = wave_vals,
  education = levels(df_cc$education),
  age_cat   = modal_age,
  gender    = modal_gender,
  race      = modal_race,
  party     = modal_party,
  income_tier = modal_income,
  ai_heard  = modal_aiheard,
  weight    = mean_weight,
  stringsAsFactors = FALSE
) %>%
  mutate(
    wave      = as.integer(wave),
    education = factor(education, levels = levels(df_cc$education)),
    age_cat   = factor(age_cat,   levels = levels(df_cc$age_cat)),
    gender    = factor(gender,    levels = levels(df_cc$gender)),
    race      = factor(race,      levels = levels(df_cc$race)),
    party     = factor(party,     levels = levels(df_cc$party)),
    income_tier = factor(income_tier, levels = levels(df_cc$income_tier)),
    ai_heard  = factor(ai_heard,  levels = levels(df_cc$ai_heard))
  )

pred_B <- predict(m_B_int,
                  newdata = grid_educ,
                  type    = "link",
                  se      = TRUE)

grid_educ <- grid_educ %>%
  mutate(
    fit     = as.numeric(pred_B),
    se_fit  = as.numeric(SE(pred_B)),
    prob    = plogis(fit),
    prob_lo = plogis(fit - 1.96 * se_fit),
    prob_hi = plogis(fit + 1.96 * se_fit),
    wave_label = factor(wave,
                        levels = wave_vals,
                        labels = wave_labels_plot)
  )

edu_colors <- c(
  "College graduate+"    = col_edu_hi,
  "Some College"         = col_edu_mid,
  "HS graduate or less"  = col_edu_lo
)
edu_labels <- c(
  "College graduate+"   = "College graduate+",
  "Some College"        = "Some college",
  "HS graduate or less" = "HS graduate or less"
)

fig7 <- ggplot(grid_educ,
               aes(x = wave_label, y = prob,
                   color = education, group = education,
                   ymin = prob_lo, ymax = prob_hi)) +
  geom_ribbon(aes(fill = education), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_color_manual(values = edu_colors, labels = edu_labels,
                     name = "Education") +
  scale_fill_manual(values  = edu_colors, labels = edu_labels,
                    name = "Education") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    title    = "Predicted Probability of Being 'More Concerned than Excited'\nAbout AI, by Educational Attainment",
    subtitle = "Covariates held at modal values; 95% CI shown",
    x        = "Survey Wave",
    y        = "Predicted Probability (Concerned)",
    caption  = paste0("Note: Predictions from logistic regression with wave × education interaction.\n",
                      "Reference: College graduate+. N = ", n_B, ".")
  ) +
  theme_pub()

ggsave(file.path(out_figures, "fig7_predicted_education_gap.png"),
       plot   = fig7,
       width  = 8,
       height = 5,
       dpi    = 300,
       bg     = "white")
cat("Saved: fig7_predicted_education_gap.png\n")


# =============================================================================
# ANALYSIS 5: MODEL COMPARISON TABLE (AIC / BIC)
# =============================================================================

cat("\n=== ANALYSIS 5: Model Comparison (AIC / BIC) ===\n")

# Note: svyglm uses quasi-likelihood; AIC is approximate.
# We compute AIC using the standard logLik approach from the underlying glm fit.
# For transparency, we report both quasi-AIC (from logLik) and residual df.

extract_fit_stats <- function(model, model_name, description) {
  ll    <- tryCatch(as.numeric(logLik(model)), error = function(e) NA_real_)
  n_obs <- nrow(model.frame(model))
  n_par <- length(coef(model))
  aic   <- tryCatch(AIC(model), error = function(e) NA_real_)
  bic   <- tryCatch(BIC(model), error = function(e) NA_real_)
  dev   <- tryCatch(deviance(model), error = function(e) NA_real_)
  null_dev <- tryCatch(model$null.deviance, error = function(e) NA_real_)
  df_res   <- tryCatch(model$df.residual,   error = function(e) NA_real_)

  tibble(
    model_name  = model_name,
    description = description,
    n_obs       = n_obs,
    n_params    = n_par,
    logLik      = round(ll,    2),
    AIC         = round(aic,   2),
    BIC         = round(bic,   2),
    deviance    = round(dev,   2),
    null_dev    = round(null_dev, 2),
    df_residual = df_res
  )
}

# Models to compare
model_list <- list(
  # Party
  list(m_A_main, "A_main",  "Concerned ~ wave + party + controls"),
  list(m_A_int,  "A_int",   "Concerned ~ wave × party + controls"),
  # Education
  list(m_B_main, "B_main",  "Concerned ~ wave + education + controls"),
  list(m_B_int,  "B_int",   "Concerned ~ wave × education + controls"),
  # Age
  list(m_C_main, "C_main",  "Concerned ~ wave + age_cat + controls"),
  list(m_C_int,  "C_int",   "Concerned ~ wave × age_cat + controls")
)

tbl7 <- purrr::map_dfr(model_list, function(x) {
  extract_fit_stats(x[[1]], x[[2]], x[[3]])
})

# Add delta AIC within each pair — derive labels from model_name
tbl7 <- tbl7 %>%
  mutate(
    pair = case_when(
      grepl("^A_", model_name) ~ "Party",
      grepl("^B_", model_name) ~ "Education",
      grepl("^C_", model_name) ~ "Age",
      TRUE ~ "Other"
    ),
    type = ifelse(grepl("_int$", model_name), "Interaction", "Main effects")
  ) %>%
  group_by(pair) %>%
  mutate(
    delta_AIC = AIC - min(AIC, na.rm = TRUE),
    delta_BIC = BIC - min(BIC, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(pair, type, model_name, description, n_obs, n_params,
         logLik, AIC, delta_AIC, BIC, delta_BIC, deviance, df_residual)

write_csv(tbl7,
          file.path(out_tables, "table7_model_comparison.csv"))
cat("Saved: table7_model_comparison.csv\n")

cat("\n  Model comparison summary:\n")
print(tbl7 %>% select(pair, type, AIC, delta_AIC, BIC, delta_BIC),
      n = Inf)


# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 65), "\n\n")

cat(sprintf("Analytic sample: N = %d (complete cases)\n", nrow(df_cc)))
cat(sprintf("  Wave 1 (Dec 2022): N = %d\n",
            nrow(df_cc %>% filter(wave == 1))))
cat(sprintf("  Wave 2 (Aug 2023): N = %d\n",
            nrow(df_cc %>% filter(wave == 2))))
cat(sprintf("  Wave 3 (Aug 2024): N = %d\n",
            nrow(df_cc %>% filter(wave == 3))))

cat("\nOutputs saved:\n")
cat("  Tables:\n")
cat("    output/tables/table4_multinomial_full.csv\n")
cat("    output/tables/table5_logistic_by_wave.csv\n")
cat("    output/tables/table6_interaction_models.csv\n")
cat("    output/tables/table7_model_comparison.csv\n")
cat("  Figures:\n")
cat("    output/figures/fig6_predicted_party_polarization.png\n")
cat("    output/figures/fig7_predicted_education_gap.png\n")

cat("\nReference categories:\n")
cat("  DV (cncexc):  'Equally'\n")
cat("  wave_date:    'Dec 2022'\n")
cat("  party:        'Dem/Lean Dem'\n")
cat("  education:    'College graduate+'\n")
cat("  age_cat:      '30-49'\n")
cat("  race:         'White non-Hispanic'\n")
cat("  income_tier:  'Middle'\n")
cat("  region:       'South'\n")
cat("  ai_heard:     'A lot'\n")
cat("  gender:       'Male'\n")
