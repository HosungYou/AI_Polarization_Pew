# =============================================================================
# 04_advanced_analysis.R
# AI Attitude Polarization Study — Pew ATP Data
#
# Purpose:  Advanced inferential analyses extending 03_regression_analysis.R.
#           Ten analysis sections covering corrected binary models, AMEs,
#           Wald tests, Blinder-Oaxaca decomposition, party ideology models,
#           polarization metrics, robustness checks, three-way interactions,
#           and multiple testing correction.
#
# Input:    data/processed/pew_combined.rds
# Outputs:
#   output/tables/tbl_adv01_binary_models.csv
#   output/tables/tbl_adv02_interaction_wave_factor.csv
#   output/tables/tbl_adv03_ames_main.csv
#   output/tables/tbl_adv03_ames_by_wave.csv
#   output/tables/tbl_adv03_avg_predictions.csv
#   output/tables/tbl_adv04_wald_tests.csv
#   output/tables/tbl_adv05_oaxaca.csv
#   output/tables/tbl_adv06_party_ideo_models.csv
#   output/tables/tbl_adv07_polarization_metrics.csv
#   output/tables/tbl_adv08_robustness.csv
#   output/tables/tbl_adv09_threeway.csv
#   output/tables/tbl_adv10_fdr_correction.csv
#   output/figures/fig_adv03_ames_main.png
#   output/figures/fig_adv03_avg_pred_party.png
#   output/figures/fig_adv03_avg_pred_education.png
#   output/figures/fig_adv05_oaxaca.png
#   output/figures/fig_adv06_party_ideo.png
#   output/figures/fig_adv07_polarization_metrics.png
#
# Author:  [Your Name]
# Date:    2026-02-22
# =============================================================================


# =============================================================================
# 0. SETUP
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("04_advanced_analysis.R — Loading packages and data\n")
cat(strrep("=", 65), "\n\n")

# ---- 0a. Install / load packages --------------------------------------------

required_pkgs <- c(
  "tidyverse", "survey", "srvyr", "broom",
  "scales", "ggplot2", "marginaleffects",
  "patchwork", "forcats"
)

invisible(lapply(required_pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message(sprintf("Installing package: %s", p))
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

col_excited  <- "#2166AC"   # blue — Excited
col_concerned <- "#B2182B"  # red  — Concerned
col_equally  <- "#999999"   # gray — Equally / neutral
col_rep      <- "#B2182B"
col_dem      <- "#2166AC"
col_edu_hi   <- "#2166AC"
col_edu_mid  <- "#999999"
col_edu_lo   <- "#B2182B"

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

# ---- 0e. Helper: significance stars ------------------------------------------

sig_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.10  ~ ".",
    TRUE       ~ ""
  )
}

# ---- 0f. Helper: tidy OR table from svyglm -----------------------------------

tidy_svyglm <- function(model, model_label = NULL) {
  coef_mat <- coef(summary(model))
  ci       <- confint(model)

  tbl <- tibble::tibble(
    term      = rownames(coef_mat),
    estimate  = coef_mat[, "Estimate"],
    std_error = coef_mat[, "Std. Error"],
    z_value   = coef_mat[, "t value"],
    p_value   = coef_mat[, "Pr(>|t|)"],
    OR        = exp(estimate),
    OR_lo95   = exp(ci[, 1]),
    OR_hi95   = exp(ci[, 2]),
    OR_fmt    = sprintf("%.3f", exp(estimate)),
    CI_fmt    = sprintf("[%.3f, %.3f]", exp(ci[, 1]), exp(ci[, 2])),
    p_fmt     = ifelse(coef_mat[, "Pr(>|t|)"] < 0.001, "<0.001",
                       sprintf("%.3f", coef_mat[, "Pr(>|t|)"])),
    sig       = sig_stars(coef_mat[, "Pr(>|t|)"])
  )

  if (!is.null(model_label)) tbl <- dplyr::mutate(tbl, model = model_label)
  tbl
}


# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

cat("\n--- Loading data ---\n")
df_raw <- readRDS(data_path)
cat(sprintf("Raw observations: %d\n", nrow(df_raw)))

# ---- 1a. Factor reference levels (consistent with 03_regression_analysis.R) --

df <- df_raw %>%
  mutate(
    # DV
    cncexc    = factor(cncexc,
                       levels = c("Equally", "Excited", "Concerned")),
    concerned = as.integer(concerned),
    excited   = as.integer(excited),

    # Time — wave_date as FACTOR (key fix for analyses 1-4)
    wave_date = factor(wave_date,
                       levels = c("Dec 2022", "Aug 2023", "Aug 2024"),
                       ordered = FALSE),

    # Demographics
    age_cat   = relevel(factor(age_cat,
                               levels = c("18-29", "30-49", "50-64", "65+")),
                        ref = "30-49"),
    gender    = relevel(factor(gender,
                               levels = c("Male", "Female", "Other")),
                        ref = "Male"),
    education = relevel(factor(education,
                               levels = c("College graduate+",
                                          "Some College",
                                          "HS graduate or less")),
                        ref = "College graduate+"),
    education_detail = factor(education_detail),   # keep as-is; relevel below

    race      = relevel(factor(race,
                               levels = c("White non-Hispanic",
                                          "Black non-Hispanic",
                                          "Hispanic",
                                          "Other non-Hispanic",
                                          "Asian non-Hispanic")),
                        ref = "White non-Hispanic"),

    party     = relevel(factor(party,
                               levels = c("Dem/Lean Dem", "Rep/Lean Rep")),
                        ref = "Dem/Lean Dem"),

    # party_ideo — collapse to 4 categories matching partisan-ideological groups
    party_ideo_4 = dplyr::case_when(
      party_ideo %in% c("Cons Rep/Lean Rep")                    ~ "Cons Rep",
      party_ideo %in% c("Mod/Lib Rep/Lean Rep")                 ~ "Mod/Lib Rep",
      party_ideo %in% c("Mod/Cons Dem/Lean Dem")                ~ "Mod/Cons Dem",
      party_ideo %in% c("Lib Dem/Lean Dem")                     ~ "Lib Dem",
      TRUE ~ NA_character_
    ),
    party_ideo_4 = relevel(
      factor(party_ideo_4,
             levels = c("Lib Dem", "Mod/Cons Dem", "Mod/Lib Rep", "Cons Rep")),
      ref = "Lib Dem"
    ),

    income_tier = relevel(factor(income_tier,
                                 levels = c("Lower", "Middle", "Upper")),
                          ref = "Middle"),

    ai_heard  = relevel(factor(ai_heard,
                               levels = c("A lot", "A little", "Nothing")),
                        ref = "A lot"),

    region    = relevel(factor(region,
                               levels = c("Northeast", "Midwest", "South", "West")),
                        ref = "South"),
    metro     = factor(metro,
                       levels = c("Metropolitan", "Non-metropolitan"))
  )

# Relevel education_detail if it exists (use most-common as reference)
if ("education_detail" %in% names(df)) {
  ed_levels <- names(sort(table(df$education_detail), decreasing = TRUE))
  df$education_detail <- relevel(
    factor(df$education_detail, levels = ed_levels),
    ref = ed_levels[1]
  )
}

# ---- 1b. Complete-case analytic subset for main models ----------------------

main_vars <- c("cncexc", "concerned", "excited", "wave_date",
               "age_cat", "gender", "education", "race",
               "party", "income_tier", "ai_heard", "weight")

df_cc <- df %>%
  select(any_of(c(main_vars, "party_ideo_4", "education_detail", "region", "metro"))) %>%
  filter(party %in% c("Dem/Lean Dem", "Rep/Lean Rep")) %>%
  drop_na(all_of(main_vars))

cat(sprintf("Complete-case analytic N: %d (dropped %d)\n",
            nrow(df_cc), nrow(df) - nrow(df_cc)))
cat(sprintf("  Wave 1 (Dec 2022): N = %d\n",
            sum(df_cc$wave_date == "Dec 2022")))
cat(sprintf("  Wave 2 (Aug 2023): N = %d\n",
            sum(df_cc$wave_date == "Aug 2023")))
cat(sprintf("  Wave 3 (Aug 2024): N = %d\n",
            sum(df_cc$wave_date == "Aug 2024")))

# ---- 1c. Survey design objects -----------------------------------------------

svy_full <- svydesign(ids = ~1, weights = ~weight, data = df_cc,
                      na.action = na.omit)

# Wave-specific designs (by wave_date factor)
wave_labels_vec <- c("Dec 2022", "Aug 2023", "Aug 2024")
svy_w <- lapply(wave_labels_vec, function(wl) {
  d <- df_cc %>% filter(wave_date == wl)
  svydesign(ids = ~1, weights = ~weight, data = d, na.action = na.omit)
})
names(svy_w) <- c("w1", "w2", "w3")

cat("Survey designs created.\n")

# Common formula components
main_formula <- concerned ~ wave_date + age_cat + gender + education +
  race + party + income_tier + ai_heard


# =============================================================================
# ANALYSIS 1: CORRECTED BINARY MODELS (svyglm, wave_date as factor)
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS 1: Two Separate Binary svyglm Models\n")
cat(strrep("=", 65), "\n")

# ---- 1a. Model 1: Excited vs Not --------------------------------------------

cat("  Fitting Model 1: Excited ~ wave_date + demographics...\n")

m1_excited <- svyglm(
  excited ~ wave_date + age_cat + gender + education +
    race + party + income_tier + ai_heard,
  design    = svy_full,
  family    = quasibinomial(link = "logit"),
  na.action = na.omit
)
n_m1 <- nrow(model.frame(m1_excited))
cat(sprintf("    N = %d\n", n_m1))

# ---- 1b. Model 2: Concerned vs Not ------------------------------------------

cat("  Fitting Model 2: Concerned ~ wave_date + demographics...\n")

m1_concerned <- svyglm(
  concerned ~ wave_date + age_cat + gender + education +
    race + party + income_tier + ai_heard,
  design    = svy_full,
  family    = quasibinomial(link = "logit"),
  na.action = na.omit
)
n_m1c <- nrow(model.frame(m1_concerned))
cat(sprintf("    N = %d\n", n_m1c))

# ---- 1c. Combine and export --------------------------------------------------

tbl_adv01 <- bind_rows(
  tidy_svyglm(m1_excited,   model_label = "Excited_vs_Not") %>%
    mutate(dv = "Excited",   analytic_n = n_m1),
  tidy_svyglm(m1_concerned, model_label = "Concerned_vs_Not") %>%
    mutate(dv = "Concerned", analytic_n = n_m1c)
) %>%
  mutate(
    OR_fmt = sprintf("%.3f", OR),
    CI_fmt = sprintf("[%.3f, %.3f]", OR_lo95, OR_hi95),
    p_fmt  = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  ) %>%
  select(dv, model, term, analytic_n, OR, OR_lo95, OR_hi95,
         OR_fmt, CI_fmt, estimate, std_error, z_value,
         p_value, p_fmt, sig)

write_csv(tbl_adv01,
          file.path(out_tables, "tbl_adv01_binary_models.csv"))
cat("Saved: tbl_adv01_binary_models.csv\n")

cat("\n  Key wave_date terms (Excited model):\n")
tbl_adv01 %>%
  filter(dv == "Excited", grepl("wave_date", term)) %>%
  select(term, OR_fmt, CI_fmt, p_fmt, sig) %>%
  print(n = Inf)

cat("\n  Key wave_date terms (Concerned model):\n")
tbl_adv01 %>%
  filter(dv == "Concerned", grepl("wave_date", term)) %>%
  select(term, OR_fmt, CI_fmt, p_fmt, sig) %>%
  print(n = Inf)


# =============================================================================
# ANALYSIS 2: INTERACTION MODELS WITH wave_date AS FACTOR
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS 2: Wave × Demographic Interactions (wave_date as factor)\n")
cat(strrep("=", 65), "\n")

# ---- 2a. Model A: concerned ~ wave_date * party + covariates ----------------

cat("  Fitting Model A: wave_date × party...\n")

m2_A <- svyglm(
  concerned ~ wave_date * party +
    age_cat + gender + education + race + income_tier + ai_heard,
  design    = svy_full,
  family    = quasibinomial(link = "logit"),
  na.action = na.omit
)
n_2A <- nrow(model.frame(m2_A))
cat(sprintf("    N = %d\n", n_2A))

# ---- 2b. Model B: concerned ~ wave_date * education + covariates ------------

cat("  Fitting Model B: wave_date × education...\n")

m2_B <- svyglm(
  concerned ~ wave_date * education +
    age_cat + gender + race + party + income_tier + ai_heard,
  design    = svy_full,
  family    = quasibinomial(link = "logit"),
  na.action = na.omit
)
n_2B <- nrow(model.frame(m2_B))
cat(sprintf("    N = %d\n", n_2B))

# ---- 2c. Model C: concerned ~ wave_date * age_cat + covariates --------------

cat("  Fitting Model C: wave_date × age_cat...\n")

m2_C <- svyglm(
  concerned ~ wave_date * age_cat +
    gender + education + race + party + income_tier + ai_heard,
  design    = svy_full,
  family    = quasibinomial(link = "logit"),
  na.action = na.omit
)
n_2C <- nrow(model.frame(m2_C))
cat(sprintf("    N = %d\n", n_2C))

# ---- 2d. Combine and export --------------------------------------------------

tbl_adv02 <- bind_rows(
  tidy_svyglm(m2_A, model_label = "A_wavedate_x_party") %>%
    mutate(analytic_n = n_2A, focal = "wave_date × party"),
  tidy_svyglm(m2_B, model_label = "B_wavedate_x_education") %>%
    mutate(analytic_n = n_2B, focal = "wave_date × education"),
  tidy_svyglm(m2_C, model_label = "C_wavedate_x_age") %>%
    mutate(analytic_n = n_2C, focal = "wave_date × age_cat")
) %>%
  mutate(
    OR_fmt = sprintf("%.3f", OR),
    CI_fmt = sprintf("[%.3f, %.3f]", OR_lo95, OR_hi95),
    p_fmt  = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  ) %>%
  select(focal, model, term, analytic_n, OR, OR_lo95, OR_hi95,
         OR_fmt, CI_fmt, estimate, std_error, z_value,
         p_value, p_fmt, sig) %>%
  arrange(model, term)

write_csv(tbl_adv02,
          file.path(out_tables, "tbl_adv02_interaction_wave_factor.csv"))
cat("Saved: tbl_adv02_interaction_wave_factor.csv\n")

# Print interaction terms
for (mod_nm in c("A_wavedate_x_party", "B_wavedate_x_education",
                 "C_wavedate_x_age")) {
  cat(sprintf("\n  Interaction terms (%s):\n", mod_nm))
  tbl_adv02 %>%
    filter(model == mod_nm, grepl(":", term)) %>%
    select(term, OR_fmt, CI_fmt, p_fmt, sig) %>%
    print(n = Inf)
}


# =============================================================================
# ANALYSIS 3: AVERAGE MARGINAL EFFECTS (AMEs)
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS 3: Average Marginal Effects (marginaleffects)\n")
cat(strrep("=", 65), "\n")

# ---- 3a. AMEs from main binary model (concerned ~ demographics) -------------

cat("  Computing AMEs from main binary model...\n")

ame_main <- tryCatch({
  marginaleffects::avg_slopes(m1_concerned)
}, error = function(e) {
  cat(sprintf("  WARNING: avg_slopes failed: %s\n", conditionMessage(e)))
  NULL
})

if (!is.null(ame_main)) {
  tbl_adv03_main <- as.data.frame(ame_main) %>%
    as_tibble() %>%
    mutate(
      AME_fmt = sprintf("%.4f", estimate),
      CI_fmt  = sprintf("[%.4f, %.4f]", conf.low, conf.high),
      p_fmt   = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)),
      sig     = sig_stars(p.value),
      model   = "Concerned_vs_Not_main"
    ) %>%
    select(model, term, contrast, estimate, std.error, conf.low, conf.high,
           AME_fmt, CI_fmt, p.value, p_fmt, sig)

  write_csv(tbl_adv03_main,
            file.path(out_tables, "tbl_adv03_ames_main.csv"))
  cat("Saved: tbl_adv03_ames_main.csv\n")

  # ---- AME plot (main model) -------------------------------------------------

  plot_ame_df <- tbl_adv03_main %>%
    filter(term != "(Intercept)") %>%
    mutate(
      label = paste0(term, ": ", contrast),
      sig_alpha = ifelse(p.value < 0.05, 1.0, 0.4)
    ) %>%
    arrange(estimate)

  fig_ame_main <- ggplot(
    plot_ame_df,
    aes(x = estimate, y = reorder(label, estimate),
        xmin = conf.low, xmax = conf.high,
        color = estimate > 0, alpha = sig_alpha)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(height = 0.25, linewidth = 0.7) +
    geom_point(size = 2.5) +
    scale_color_manual(
      values = c("TRUE" = col_concerned, "FALSE" = col_excited),
      guide  = "none"
    ) +
    scale_alpha_identity() +
    labs(
      title    = "Average Marginal Effects on P(Concerned About AI)",
      subtitle = "Faded points: p > 0.05",
      x        = "Average Marginal Effect (probability scale)",
      y        = NULL,
      caption  = sprintf("N = %d. Survey-weighted quasibinomial model.", n_m1c)
    ) +
    theme_pub() +
    theme(axis.text.y = element_text(size = 8))

  ggsave(file.path(out_figures, "fig_adv03_ames_main.png"),
         plot = fig_ame_main,
         width = 10, height = max(6, nrow(plot_ame_df) * 0.28),
         dpi = 300, bg = "white")
  cat("Saved: fig_adv03_ames_main.png\n")

} else {
  cat("  Skipped AME main plot (model unavailable).\n")
}

# ---- 3b. AMEs by wave from interaction models --------------------------------

cat("  Computing AMEs by wave for wave × party model...\n")

ame_wave_party <- tryCatch({
  marginaleffects::avg_slopes(
    m2_A,
    variables  = "party",
    by         = "wave_date"
  )
}, error = function(e) {
  cat(sprintf("  WARNING: avg_slopes by wave failed: %s\n", conditionMessage(e)))
  NULL
})

ame_wave_educ <- tryCatch({
  marginaleffects::avg_slopes(
    m2_B,
    variables  = "education",
    by         = "wave_date"
  )
}, error = function(e) {
  cat(sprintf("  WARNING: avg_slopes education by wave failed: %s\n",
              conditionMessage(e)))
  NULL
})

tbl_ame_by_wave_list <- list()

if (!is.null(ame_wave_party)) {
  tbl_ame_by_wave_list[["party"]] <- as.data.frame(ame_wave_party) %>%
    as_tibble() %>%
    mutate(model = "wave_x_party", focal_var = "party")
}
if (!is.null(ame_wave_educ)) {
  tbl_ame_by_wave_list[["educ"]] <- as.data.frame(ame_wave_educ) %>%
    as_tibble() %>%
    mutate(model = "wave_x_education", focal_var = "education")
}

if (length(tbl_ame_by_wave_list) > 0) {
  tbl_adv03_by_wave <- bind_rows(tbl_ame_by_wave_list) %>%
    mutate(
      sig = sig_stars(p.value),
      AME_fmt = sprintf("%.4f", estimate),
      CI_fmt  = sprintf("[%.4f, %.4f]", conf.low, conf.high),
      p_fmt   = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    )

  write_csv(tbl_adv03_by_wave,
            file.path(out_tables, "tbl_adv03_ames_by_wave.csv"))
  cat("Saved: tbl_adv03_ames_by_wave.csv\n")
  cat("\n  AMEs by wave (party):\n")
  print(tbl_adv03_by_wave %>%
          filter(focal_var == "party") %>%
          select(wave_date, term, contrast, AME_fmt, CI_fmt, p_fmt, sig),
        n = Inf)
}

# ---- 3c. Average Predictions by wave × party, wave × education --------------

cat("  Computing average predictions by wave × party...\n")

avg_pred_party <- tryCatch({
  marginaleffects::avg_predictions(
    m2_A,
    by        = c("wave_date", "party")
  )
}, error = function(e) {
  cat(sprintf("  WARNING: avg_predictions (party) failed: %s\n",
              conditionMessage(e)))
  NULL
})

avg_pred_educ <- tryCatch({
  marginaleffects::avg_predictions(
    m2_B,
    by        = c("wave_date", "education")
  )
}, error = function(e) {
  cat(sprintf("  WARNING: avg_predictions (education) failed: %s\n",
              conditionMessage(e)))
  NULL
})

tbl_avg_pred_list <- list()
if (!is.null(avg_pred_party)) {
  tbl_avg_pred_list[["party"]] <- as.data.frame(avg_pred_party) %>%
    as_tibble() %>% mutate(grouping = "party")
}
if (!is.null(avg_pred_educ)) {
  tbl_avg_pred_list[["educ"]] <- as.data.frame(avg_pred_educ) %>%
    as_tibble() %>% mutate(grouping = "education")
}

if (length(tbl_avg_pred_list) > 0) {
  tbl_adv03_avg <- bind_rows(tbl_avg_pred_list) %>%
    mutate(
      pred_fmt = sprintf("%.4f", estimate),
      CI_fmt   = sprintf("[%.4f, %.4f]", conf.low, conf.high)
    )

  write_csv(tbl_adv03_avg,
            file.path(out_tables, "tbl_adv03_avg_predictions.csv"))
  cat("Saved: tbl_adv03_avg_predictions.csv\n")

  # Plot: average predictions wave × party
  if (!is.null(avg_pred_party)) {
    df_plot_party <- as.data.frame(avg_pred_party) %>% as_tibble() %>%
      mutate(
        wave_date = factor(wave_date, levels = wave_labels_vec),
        party     = factor(party, levels = c("Dem/Lean Dem", "Rep/Lean Rep"))
      )

    fig_avg_pred_party <- ggplot(
      df_plot_party,
      aes(x = wave_date, y = estimate, color = party, group = party,
          ymin = conf.low, ymax = conf.high)
    ) +
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
        title    = "Average Predicted P(Concerned) by Party × Wave",
        subtitle = "Average marginal predictions; 95% CI shown",
        x        = "Survey Wave", y = "Avg. Predicted P(Concerned)",
        caption  = sprintf("N = %d. Survey-weighted quasibinomial.", n_2A)
      ) +
      theme_pub()

    ggsave(file.path(out_figures, "fig_adv03_avg_pred_party.png"),
           plot = fig_avg_pred_party,
           width = 8, height = 5, dpi = 300, bg = "white")
    cat("Saved: fig_adv03_avg_pred_party.png\n")
  }

  # Plot: average predictions wave × education
  if (!is.null(avg_pred_educ)) {
    edu_colors <- c(
      "College graduate+"   = col_edu_hi,
      "Some College"        = col_edu_mid,
      "HS graduate or less" = col_edu_lo
    )

    df_plot_educ <- as.data.frame(avg_pred_educ) %>% as_tibble() %>%
      mutate(
        wave_date = factor(wave_date, levels = wave_labels_vec),
        education = factor(education,
                           levels = c("College graduate+",
                                      "Some College",
                                      "HS graduate or less"))
      )

    fig_avg_pred_educ <- ggplot(
      df_plot_educ,
      aes(x = wave_date, y = estimate, color = education, group = education,
          ymin = conf.low, ymax = conf.high)
    ) +
      geom_ribbon(aes(fill = education), alpha = 0.15, color = NA) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 3) +
      scale_color_manual(values = edu_colors, name = "Education") +
      scale_fill_manual(values  = edu_colors, name = "Education") +
      scale_y_continuous(labels = percent_format(accuracy = 1),
                         limits = c(0, 1)) +
      labs(
        title    = "Average Predicted P(Concerned) by Education × Wave",
        subtitle = "Average marginal predictions; 95% CI shown",
        x        = "Survey Wave", y = "Avg. Predicted P(Concerned)",
        caption  = sprintf("N = %d. Survey-weighted quasibinomial.", n_2B)
      ) +
      theme_pub()

    ggsave(file.path(out_figures, "fig_adv03_avg_pred_education.png"),
           plot = fig_avg_pred_educ,
           width = 8, height = 5, dpi = 300, bg = "white")
    cat("Saved: fig_adv03_avg_pred_education.png\n")
  }
}


# =============================================================================
# ANALYSIS 4: JOINT WALD TESTS (regTermTest)
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS 4: Joint Wald Tests for Interaction Terms\n")
cat(strrep("=", 65), "\n")

run_wald_test <- function(model, term_pattern, label) {
  result <- tryCatch(
    survey::regTermTest(model, term_pattern),
    error = function(e) {
      cat(sprintf("  WARNING: Wald test failed for %s: %s\n",
                  label, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(result)) {
    return(tibble(
      hypothesis     = label,
      term_pattern   = as.character(term_pattern),
      F_statistic    = NA_real_,
      df_numerator   = NA_real_,
      df_denominator = NA_real_,
      p_value        = NA_real_,
      sig            = NA_character_
    ))
  }
  tibble(
    hypothesis     = label,
    term_pattern   = as.character(deparse(term_pattern)),
    F_statistic    = as.numeric(result$Ftest["F"]),
    df_numerator   = as.numeric(result$Ftest["df"]),
    df_denominator = as.numeric(result$df),
    p_value        = as.numeric(result$p),
    sig            = sig_stars(as.numeric(result$p))
  )
}

# H0: all wave_date × party terms = 0
wald_A <- run_wald_test(m2_A, ~ wave_date:party,
                        "H0: all wave_date × party terms = 0")

# H0: all wave_date × education terms = 0
wald_B <- run_wald_test(m2_B, ~ wave_date:education,
                        "H0: all wave_date × education terms = 0")

# H0: all wave_date × age_cat terms = 0
wald_C <- run_wald_test(m2_C, ~ wave_date:age_cat,
                        "H0: all wave_date × age_cat terms = 0")

tbl_adv04 <- bind_rows(wald_A, wald_B, wald_C) %>%
  mutate(
    F_fmt = sprintf("%.3f", F_statistic),
    p_fmt = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  )

write_csv(tbl_adv04,
          file.path(out_tables, "tbl_adv04_wald_tests.csv"))
cat("Saved: tbl_adv04_wald_tests.csv\n")

cat("\n  Joint Wald Test Results:\n")
tbl_adv04 %>%
  select(hypothesis, F_fmt, df_numerator, df_denominator, p_fmt, sig) %>%
  print(n = Inf)


# =============================================================================
# ANALYSIS 5: BLINDER-OAXACA DECOMPOSITION
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS 5: Blinder-Oaxaca Decomposition (W1 vs W3)\n")
cat(strrep("=", 65), "\n")

# ---- 5a. Prepare W1 and W3 data ---------------------------------------------

df_w1 <- df_cc %>% filter(wave_date == "Dec 2022")
df_w3 <- df_cc %>% filter(wave_date == "Aug 2024")

# Decomposition covariates
oax_vars <- c("age_cat", "gender", "education", "race",
              "party", "income_tier", "ai_heard")

oax_formula <- as.formula(
  paste("concerned ~", paste(oax_vars, collapse = " + "))
)

# ---- 5b. Fit wave-specific logistic models ----------------------------------

cat(sprintf("  W1 N = %d, W3 N = %d\n", nrow(df_w1), nrow(df_w3)))

m_oax_w1 <- glm(oax_formula, data = df_w1, weights = weight,
                family = binomial(link = "logit"))
m_oax_w3 <- glm(oax_formula, data = df_w3, weights = weight,
                family = binomial(link = "logit"))

# Raw means
p_w1_raw <- weighted.mean(df_w1$concerned, df_w1$weight, na.rm = TRUE)
p_w3_raw <- weighted.mean(df_w3$concerned, df_w3$weight, na.rm = TRUE)
raw_gap  <- p_w3_raw - p_w1_raw

cat(sprintf("  Weighted P(concerned): W1 = %.4f, W3 = %.4f, Gap = %.4f\n",
            p_w1_raw, p_w3_raw, raw_gap))

# ---- 5c. Fairlie (2005) manual decomposition --------------------------------
# Counterfactual: apply W1 coefficients to W3 X-composition
# Counterfactual P(concerned | X_W3, beta_W1)

pred_counter_w3_beta_w1 <- weighted.mean(
  predict(m_oax_w1, newdata = df_w3, type = "response"),
  df_w3$weight
)

pred_w1_fitted <- weighted.mean(
  predict(m_oax_w1, newdata = df_w1, type = "response"),
  df_w1$weight
)

pred_w3_fitted <- weighted.mean(
  predict(m_oax_w3, newdata = df_w3, type = "response"),
  df_w3$weight
)

# Composition effect: change in X while holding coefficients at W1 values
composition_effect <- pred_counter_w3_beta_w1 - pred_w1_fitted

# Coefficient effect: change in coefficients while holding X at W3 values
coefficient_effect <- pred_w3_fitted - pred_counter_w3_beta_w1

# Total explained by the decomposition
total_decomp <- composition_effect + coefficient_effect

cat(sprintf("\n  Decomposition results:\n"))
cat(sprintf("    Raw gap (W3 - W1):       %.4f\n", raw_gap))
cat(sprintf("    Composition effect:       %.4f (%.1f%%)\n",
            composition_effect,
            100 * composition_effect / raw_gap))
cat(sprintf("    Coefficient effect:       %.4f (%.1f%%)\n",
            coefficient_effect,
            100 * coefficient_effect / raw_gap))
cat(sprintf("    Sum (composition + coef): %.4f\n", total_decomp))

# ---- 5d. Variable-level composition contributions ---------------------------
# For each covariate, compute contribution to composition effect
# by refitting the counterfactual while changing one variable at a time

compute_var_contribution <- function(var) {
  df_counter <- df_w3
  # Replace W3 distribution of this variable with W1 distribution
  # via reweighting — not possible directly, so use coefficient approach:
  # partial composition = difference in mean linear predictor
  # for this variable's terms, using W1 betas
  terms_w1 <- coef(m_oax_w1)
  terms_w1_nm <- names(terms_w1)

  # Model matrix for W1 and W3
  mm_w1 <- model.matrix(oax_formula, data = df_w1)
  mm_w3 <- model.matrix(oax_formula, data = df_w3)

  # Find columns belonging to this variable
  var_cols <- grep(paste0("^", var), colnames(mm_w1), value = TRUE)
  if (length(var_cols) == 0) return(NA_real_)

  # Partial linear predictor contribution: beta_W1 * (mean_X_W3 - mean_X_W1)
  mean_xw1 <- apply(
    mm_w1[, var_cols, drop = FALSE] * df_w1$weight / sum(df_w1$weight),
    2, sum
  )
  mean_xw3 <- apply(
    mm_w3[, var_cols, drop = FALSE] * df_w3$weight / sum(df_w3$weight),
    2, sum
  )

  beta_w1 <- terms_w1[var_cols]
  sum(beta_w1 * (mean_xw3 - mean_xw1), na.rm = TRUE)
}

var_contribs <- sapply(oax_vars, compute_var_contribution)

tbl_adv05 <- tibble(
  component            = c("Raw gap (W3 - W1)",
                            "Composition effect (total)",
                            "Coefficient effect (total)",
                            paste0("  Composition: ", oax_vars)),
  value                = c(raw_gap, composition_effect, coefficient_effect,
                            var_contribs),
  pct_of_gap           = value / raw_gap * 100
) %>%
  mutate(
    value_fmt  = sprintf("%.4f", value),
    pct_fmt    = sprintf("%.1f%%", pct_of_gap)
  )

write_csv(tbl_adv05,
          file.path(out_tables, "tbl_adv05_oaxaca.csv"))
cat("Saved: tbl_adv05_oaxaca.csv\n")

# ---- 5e. Oaxaca bar plot -------------------------------------------------------

df_oax_plot <- tibble(
  component = c("Composition\n(demographic shifts)",
                "Coefficient\n(changed relationships)"),
  value     = c(composition_effect, coefficient_effect),
  direction = ifelse(c(composition_effect, coefficient_effect) > 0,
                     "Positive", "Negative")
)

fig_oax <- ggplot(
  df_oax_plot,
  aes(x = component, y = value, fill = direction)
) +
  geom_col(width = 0.5, color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_text(
    aes(label = sprintf("%.4f\n(%.1f%%)", value, value / raw_gap * 100),
        vjust = ifelse(value >= 0, -0.3, 1.3)),
    size = 3.5, color = "grey20"
  ) +
  scale_fill_manual(
    values = c("Positive" = col_concerned, "Negative" = col_excited),
    guide  = "none"
  ) +
  labs(
    title    = "Blinder-Oaxaca Decomposition: Change in P(Concerned), W1→W3",
    subtitle = sprintf("Total gap = %.4f; W1 = %.4f, W3 = %.4f",
                       raw_gap, p_w1_raw, p_w3_raw),
    x        = NULL,
    y        = "Contribution to change in P(Concerned)",
    caption  = paste0("Fairlie (2005) method. W1 coefficients applied to W3 composition.\n",
                      sprintf("W1 N = %d, W3 N = %d.", nrow(df_w1), nrow(df_w3)))
  ) +
  theme_pub()

ggsave(file.path(out_figures, "fig_adv05_oaxaca.png"),
       plot = fig_oax,
       width = 7, height = 5, dpi = 300, bg = "white")
cat("Saved: fig_adv05_oaxaca.png\n")


# =============================================================================
# ANALYSIS 6: PARTY IDEOLOGY 4-CATEGORY MODELS
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS 6: Party Ideology 4-Category Models\n")
cat(strrep("=", 65), "\n")

# Subset: require non-NA party_ideo_4
df_ideo <- df_cc %>% filter(!is.na(party_ideo_4))
svy_ideo <- svydesign(ids = ~1, weights = ~weight, data = df_ideo,
                      na.action = na.omit)

cat(sprintf("  Ideology subsample N = %d\n", nrow(df_ideo)))

# ---- 6a. Main model with party_ideo_4 ----------------------------------------

cat("  Fitting main model with party_ideo_4...\n")

m6_main <- svyglm(
  concerned ~ wave_date + party_ideo_4 +
    age_cat + gender + education + race + income_tier + ai_heard,
  design    = svy_ideo,
  family    = quasibinomial(link = "logit"),
  na.action = na.omit
)
n_6m <- nrow(model.frame(m6_main))
cat(sprintf("    N = %d\n", n_6m))

# ---- 6b. Interaction: wave_date × party_ideo_4 -------------------------------

cat("  Fitting wave_date × party_ideo_4 interaction model...\n")

m6_int <- svyglm(
  concerned ~ wave_date * party_ideo_4 +
    age_cat + gender + education + race + income_tier + ai_heard,
  design    = svy_ideo,
  family    = quasibinomial(link = "logit"),
  na.action = na.omit
)
n_6i <- nrow(model.frame(m6_int))
cat(sprintf("    N = %d\n", n_6i))

tbl_adv06 <- bind_rows(
  tidy_svyglm(m6_main, model_label = "Main_party_ideo4") %>%
    mutate(analytic_n = n_6m, model_type = "main"),
  tidy_svyglm(m6_int,  model_label = "Interaction_wave_x_ideo4") %>%
    mutate(analytic_n = n_6i, model_type = "interaction")
) %>%
  mutate(
    OR_fmt = sprintf("%.3f", OR),
    CI_fmt = sprintf("[%.3f, %.3f]", OR_lo95, OR_hi95),
    p_fmt  = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  ) %>%
  select(model_type, model, term, analytic_n, OR, OR_lo95, OR_hi95,
         OR_fmt, CI_fmt, estimate, std_error, z_value, p_value, p_fmt, sig)

write_csv(tbl_adv06,
          file.path(out_tables, "tbl_adv06_party_ideo_models.csv"))
cat("Saved: tbl_adv06_party_ideo_models.csv\n")

cat("\n  Interaction terms (wave × party_ideo_4):\n")
tbl_adv06 %>%
  filter(model_type == "interaction", grepl(":", term)) %>%
  select(term, OR_fmt, CI_fmt, p_fmt, sig) %>%
  print(n = Inf)

# ---- 6c. Average predictions wave × party_ideo_4 ----------------------------

avg_pred_ideo <- tryCatch({
  marginaleffects::avg_predictions(
    m6_int,
    by = c("wave_date", "party_ideo_4")
  )
}, error = function(e) {
  cat(sprintf("  WARNING: avg_predictions (ideo) failed: %s\n",
              conditionMessage(e)))
  NULL
})

if (!is.null(avg_pred_ideo)) {
  ideo_colors <- c(
    "Cons Rep"    = "#B2182B",
    "Mod/Lib Rep" = "#EF8A62",
    "Mod/Cons Dem" = "#67A9CF",
    "Lib Dem"     = "#2166AC"
  )

  df_ideo_plot <- as.data.frame(avg_pred_ideo) %>% as_tibble() %>%
    mutate(
      wave_date   = factor(wave_date, levels = wave_labels_vec),
      party_ideo_4 = factor(party_ideo_4,
                            levels = c("Cons Rep", "Mod/Lib Rep",
                                       "Mod/Cons Dem", "Lib Dem"))
    )

  fig_ideo <- ggplot(
    df_ideo_plot,
    aes(x = wave_date, y = estimate,
        color = party_ideo_4, group = party_ideo_4,
        ymin = conf.low, ymax = conf.high)
  ) +
    geom_ribbon(aes(fill = party_ideo_4), alpha = 0.12, color = NA) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 3) +
    scale_color_manual(values = ideo_colors, name = "Party-Ideology") +
    scale_fill_manual(values  = ideo_colors, name = "Party-Ideology") +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       limits = c(0, 1)) +
    labs(
      title    = "Average Predicted P(Concerned) by Party-Ideology × Wave",
      subtitle = "Average marginal predictions; 95% CI",
      x        = "Survey Wave",
      y        = "Avg. Predicted P(Concerned)",
      caption  = sprintf("N = %d. Reference: Lib Dem.", n_6i)
    ) +
    theme_pub()

  ggsave(file.path(out_figures, "fig_adv06_party_ideo.png"),
         plot = fig_ideo,
         width = 8, height = 5, dpi = 300, bg = "white")
  cat("Saved: fig_adv06_party_ideo.png\n")
}


# =============================================================================
# ANALYSIS 7: FORMAL POLARIZATION METRICS
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS 7: Formal Polarization Metrics\n")
cat(strrep("=", 65), "\n")

# ---- 7a. Helper functions ---------------------------------------------------

# Shannon entropy for a named probability vector
shannon_entropy <- function(probs) {
  probs <- probs[probs > 0]
  -sum(probs * log(probs))
}

# Weighted variance
weighted_var <- function(x, w) {
  w    <- w / sum(w)
  mu   <- sum(w * x)
  sum(w * (x - mu)^2)
}

# Numeric attitude scale: Excited = 0, Equally = 0.5, Concerned = 1
attitude_scale <- c("Excited" = 0, "Equally" = 0.5, "Concerned" = 1)

# ---- 7b. Overall metrics by wave --------------------------------------------

compute_wave_metrics <- function(data, group_var = NULL) {
  if (!is.null(group_var)) {
    data <- data %>% group_by(wave_date, !!sym(group_var))
  } else {
    data <- data %>% group_by(wave_date)
  }

  data %>%
    summarise(
      n_weighted = sum(weight, na.rm = TRUE),
      # Category proportions
      p_excited  = sum(weight[cncexc == "Excited"],  na.rm = TRUE) /
        sum(weight, na.rm = TRUE),
      p_equally  = sum(weight[cncexc == "Equally"],  na.rm = TRUE) /
        sum(weight, na.rm = TRUE),
      p_concerned = sum(weight[cncexc == "Concerned"], na.rm = TRUE) /
        sum(weight, na.rm = TRUE),
      # Shannon entropy
      entropy    = shannon_entropy(c(p_excited, p_equally, p_concerned)),
      # Weighted variance of numeric scale
      wt_variance = {
        x <- attitude_scale[as.character(cncexc)]
        weighted_var(x[!is.na(x)], weight[!is.na(x)])
      },
      .groups = "drop"
    ) %>%
    mutate(
      # Max entropy for 3 categories = log(3)
      norm_entropy = entropy / log(3),
      pct_excited  = p_excited  * 100,
      pct_equally  = p_equally  * 100,
      pct_concerned = p_concerned * 100
    )
}

# Overall
metrics_overall <- compute_wave_metrics(df_cc) %>%
  mutate(subgroup = "Overall", subgroup_level = "Overall")

# By party
metrics_party <- compute_wave_metrics(df_cc, "party") %>%
  rename(subgroup_level = party) %>%
  mutate(subgroup = "Party")

# By education
metrics_educ <- compute_wave_metrics(df_cc, "education") %>%
  rename(subgroup_level = education) %>%
  mutate(subgroup = "Education")

# By age
metrics_age <- compute_wave_metrics(df_cc, "age_cat") %>%
  rename(subgroup_level = age_cat) %>%
  mutate(subgroup = "Age")

tbl_adv07 <- bind_rows(
  metrics_overall, metrics_party, metrics_educ, metrics_age
) %>%
  select(subgroup, subgroup_level, wave_date,
         n_weighted, pct_excited, pct_equally, pct_concerned,
         entropy, norm_entropy, wt_variance) %>%
  arrange(subgroup, subgroup_level, wave_date)

write_csv(tbl_adv07,
          file.path(out_tables, "tbl_adv07_polarization_metrics.csv"))
cat("Saved: tbl_adv07_polarization_metrics.csv\n")

cat("\n  Overall polarization metrics by wave:\n")
metrics_overall %>%
  select(wave_date, pct_excited, pct_equally, pct_concerned,
         entropy, norm_entropy, wt_variance) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  print(n = Inf)

cat("\n  Weighted variance by wave and party:\n")
metrics_party %>%
  select(wave_date, subgroup_level, wt_variance, entropy) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  print(n = Inf)

# ---- 7c. Polarization metrics figure ----------------------------------------

fig_polar <- metrics_party %>%
  mutate(
    wave_date        = factor(wave_date, levels = wave_labels_vec),
    subgroup_level   = factor(subgroup_level,
                               levels = c("Dem/Lean Dem", "Rep/Lean Rep"))
  ) %>%
  ggplot(aes(x = wave_date, y = wt_variance,
             color = subgroup_level, group = subgroup_level)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("Dem/Lean Dem" = col_dem, "Rep/Lean Rep" = col_rep),
    name   = "Party ID"
  ) +
  labs(
    title    = "Within-Group Attitude Variance by Party × Wave",
    subtitle = "Numeric scale: Excited=0, Equally=0.5, Concerned=1 (survey-weighted)",
    x        = "Survey Wave",
    y        = "Weighted Variance",
    caption  = "Higher values = more within-group heterogeneity."
  ) +
  theme_pub()

fig_entropy_overall <- metrics_overall %>%
  mutate(wave_date = factor(wave_date, levels = wave_labels_vec)) %>%
  ggplot(aes(x = wave_date, y = norm_entropy, group = 1)) +
  geom_line(linewidth = 1.1, color = col_equally) +
  geom_point(size = 3, color = col_equally) +
  scale_y_continuous(limits = c(0, 1), labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Normalized Shannon Entropy of AI Attitude Distribution",
    subtitle = "Overall sample by wave (1 = maximum uncertainty)",
    x        = "Survey Wave",
    y        = "Normalized Entropy (H / log(3))"
  ) +
  theme_pub()

fig_polar_combined <- fig_entropy_overall / fig_polar +
  patchwork::plot_annotation(
    title   = "Polarization Metrics Across Survey Waves",
    caption = sprintf("N = %d. Pew ATP, 3 waves.", nrow(df_cc))
  )

ggsave(file.path(out_figures, "fig_adv07_polarization_metrics.png"),
       plot   = fig_polar_combined,
       width  = 8, height = 9,
       dpi    = 300, bg = "white")
cat("Saved: fig_adv07_polarization_metrics.png\n")


# =============================================================================
# ANALYSIS 8: ROBUSTNESS CHECKS
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS 8: Robustness Checks\n")
cat(strrep("=", 65), "\n")

robustness_list <- list()

# ---- 8a. Unweighted vs weighted comparison ----------------------------------

cat("  8a. Unweighted logistic regression...\n")

m8a_unweighted <- glm(
  concerned ~ wave_date + age_cat + gender + education +
    race + party + income_tier + ai_heard,
  data   = df_cc,
  family = binomial(link = "logit")
)

tbl_8a_unweighted <- broom::tidy(m8a_unweighted, conf.int = TRUE) %>%
  mutate(
    model       = "8a_unweighted",
    description = "Main model WITHOUT survey weights (glm)",
    OR          = exp(estimate),
    OR_lo95     = exp(conf.low),
    OR_hi95     = exp(conf.high),
    OR_fmt      = sprintf("%.3f", OR),
    CI_fmt      = sprintf("[%.3f, %.3f]", OR_lo95, OR_hi95),
    p_fmt       = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)),
    sig         = sig_stars(p.value)
  ) %>%
  rename(std_error = std.error, z_value = statistic, p_value = p.value)

# Survey-weighted version for comparison (reuse m1_concerned)
tbl_8a_weighted <- tidy_svyglm(m1_concerned,
                                model_label = "8a_weighted") %>%
  mutate(description = "Main model WITH survey weights (svyglm)")

robustness_list[["8a"]] <- bind_rows(
  tbl_8a_weighted %>%
    select(model, description, term, OR, OR_lo95, OR_hi95,
           OR_fmt, CI_fmt, z_value, p_value, p_fmt, sig),
  tbl_8a_unweighted %>%
    select(model, description, term, OR, OR_lo95, OR_hi95,
           OR_fmt, CI_fmt, z_value, p_value, p_fmt, sig)
) %>% mutate(check = "8a_unweighted_vs_weighted")

cat("  8a complete.\n")

# ---- 8b. Alternative DV: excited as outcome ----------------------------------

cat("  8b. Alternative DV: excited...\n")

# m1_excited was already fitted in Analysis 1
tbl_8b <- tidy_svyglm(m1_excited, model_label = "8b_excited_dv") %>%
  mutate(
    description = "Robustness: Excited (vs Not) as DV",
    check       = "8b_alt_dv_excited",
    OR_fmt      = sprintf("%.3f", OR),
    CI_fmt      = sprintf("[%.3f, %.3f]", OR_lo95, OR_hi95),
    p_fmt       = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  )

robustness_list[["8b"]] <- tbl_8b %>%
  select(check, model, description, term, OR, OR_lo95, OR_hi95,
         OR_fmt, CI_fmt, z_value, p_value, p_fmt, sig)

cat("  8b complete.\n")

# ---- 8c. Wave-pair interaction models ---------------------------------------

cat("  8c. Wave-pair interaction models...\n")

# W1 vs W2 only
df_w1w2   <- df_cc %>% filter(wave_date %in% c("Dec 2022", "Aug 2023")) %>%
  mutate(wave_date = droplevels(wave_date))
svy_w1w2  <- svydesign(ids = ~1, weights = ~weight, data = df_w1w2,
                        na.action = na.omit)

m8c_w1w2_party <- svyglm(
  concerned ~ wave_date * party +
    age_cat + gender + education + race + income_tier + ai_heard,
  design    = svy_w1w2,
  family    = quasibinomial(link = "logit"),
  na.action = na.omit
)

# W2 vs W3 only
df_w2w3   <- df_cc %>% filter(wave_date %in% c("Aug 2023", "Aug 2024")) %>%
  mutate(wave_date = droplevels(wave_date))
svy_w2w3  <- svydesign(ids = ~1, weights = ~weight, data = df_w2w3,
                        na.action = na.omit)

m8c_w2w3_party <- svyglm(
  concerned ~ wave_date * party +
    age_cat + gender + education + race + income_tier + ai_heard,
  design    = svy_w2w3,
  family    = quasibinomial(link = "logit"),
  na.action = na.omit
)

tbl_8c <- bind_rows(
  tidy_svyglm(m8c_w1w2_party, model_label = "8c_w1w2_party") %>%
    mutate(analytic_n = nrow(model.frame(m8c_w1w2_party)),
           description = "W1 vs W2: wave_date × party"),
  tidy_svyglm(m8c_w2w3_party, model_label = "8c_w2w3_party") %>%
    mutate(analytic_n = nrow(model.frame(m8c_w2w3_party)),
           description = "W2 vs W3: wave_date × party")
) %>%
  mutate(
    check  = "8c_wave_pair",
    OR_fmt = sprintf("%.3f", OR),
    CI_fmt = sprintf("[%.3f, %.3f]", OR_lo95, OR_hi95),
    p_fmt  = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  )

robustness_list[["8c"]] <- tbl_8c %>%
  select(check, model, description, analytic_n, term, OR, OR_lo95, OR_hi95,
         OR_fmt, CI_fmt, z_value, p_value, p_fmt, sig)

cat(sprintf("  8c: W1-W2 N = %d, W2-W3 N = %d\n",
            nrow(df_w1w2), nrow(df_w2w3)))

# ---- 8d. Detailed education (6-category) ------------------------------------

cat("  8d. Detailed 6-category education...\n")

df_eded <- df_cc %>% filter(!is.na(education_detail))

if (nrow(df_eded) > 100 && nlevels(df_eded$education_detail) > 1) {
  svy_eded <- svydesign(ids = ~1, weights = ~weight, data = df_eded,
                         na.action = na.omit)

  m8d <- svyglm(
    concerned ~ wave_date + education_detail + age_cat + gender +
      race + party + income_tier + ai_heard,
    design    = svy_eded,
    family    = quasibinomial(link = "logit"),
    na.action = na.omit
  )

  tbl_8d <- tidy_svyglm(m8d, model_label = "8d_educ_detail") %>%
    mutate(
      analytic_n  = nrow(model.frame(m8d)),
      description = "6-category detailed education",
      check       = "8d_educ_detail",
      OR_fmt      = sprintf("%.3f", OR),
      CI_fmt      = sprintf("[%.3f, %.3f]", OR_lo95, OR_hi95),
      p_fmt       = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
    )

  robustness_list[["8d"]] <- tbl_8d %>%
    select(check, model, description, analytic_n, term, OR, OR_lo95, OR_hi95,
           OR_fmt, CI_fmt, z_value, p_value, p_fmt, sig)
  cat(sprintf("  8d: N = %d, education levels = %d\n",
              nrow(df_eded), nlevels(df_eded$education_detail)))
} else {
  cat("  8d: education_detail not available or insufficient variation; skipped.\n")
}

# ---- Combine and export robustness table -------------------------------------

tbl_adv08 <- bind_rows(robustness_list)

write_csv(tbl_adv08,
          file.path(out_tables, "tbl_adv08_robustness.csv"))
cat("Saved: tbl_adv08_robustness.csv\n")

cat("\n  Wave-pair interaction comparison (party term W1→W2 vs W2→W3):\n")
tbl_8c %>%
  filter(grepl(":", term)) %>%
  select(description, term, OR_fmt, CI_fmt, p_fmt, sig) %>%
  print(n = Inf)


# =============================================================================
# ANALYSIS 9: THREE-WAY INTERACTION: wave_date × party × ai_heard
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS 9: Three-Way Interaction (wave_date × party × ai_heard)\n")
cat(strrep("=", 65), "\n")

cat("  Fitting three-way interaction model...\n")

m9_threeway <- svyglm(
  concerned ~ wave_date * party * ai_heard +
    education + age_cat + gender + race + income_tier,
  design    = svy_full,
  family    = quasibinomial(link = "logit"),
  na.action = na.omit
)
n_9 <- nrow(model.frame(m9_threeway))
cat(sprintf("  N = %d\n", n_9))

tbl_adv09 <- tidy_svyglm(m9_threeway, model_label = "3way_wave_x_party_x_aiheard") %>%
  mutate(
    analytic_n = n_9,
    OR_fmt     = sprintf("%.3f", OR),
    CI_fmt     = sprintf("[%.3f, %.3f]", OR_lo95, OR_hi95),
    p_fmt      = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)),
    term_type  = case_when(
      grepl("wave_date.*party.*ai_heard|wave_date.*ai_heard.*party|",
            term) |
        (stringr::str_count(term, ":") >= 2) ~ "Three-way",
      stringr::str_count(term, ":") == 1 ~ "Two-way",
      TRUE ~ "Main effect"
    )
  ) %>%
  select(model, term_type, term, analytic_n, OR, OR_lo95, OR_hi95,
         OR_fmt, CI_fmt, estimate, std_error, z_value, p_value, p_fmt, sig) %>%
  arrange(term_type, term)

write_csv(tbl_adv09,
          file.path(out_tables, "tbl_adv09_threeway.csv"))
cat("Saved: tbl_adv09_threeway.csv\n")

cat("\n  Three-way interaction terms:\n")
tbl_adv09 %>%
  filter(term_type == "Three-way") %>%
  select(term, OR_fmt, CI_fmt, p_fmt, sig) %>%
  print(n = Inf)

cat("\n  Key two-way interaction terms:\n")
tbl_adv09 %>%
  filter(term_type == "Two-way") %>%
  select(term, OR_fmt, CI_fmt, p_fmt, sig) %>%
  print(n = Inf)

# Joint Wald test for all three-way terms
wald_9_3way <- tryCatch(
  survey::regTermTest(m9_threeway, ~ wave_date:party:ai_heard),
  error = function(e) {
    cat(sprintf("  WARNING: 3-way Wald test failed: %s\n", conditionMessage(e)))
    NULL
  }
)

if (!is.null(wald_9_3way)) {
  cat(sprintf("\n  Joint Wald test (all three-way terms):\n"))
  cat(sprintf("    F = %.3f, df = (%d, %d), p = %s\n",
              as.numeric(wald_9_3way$Ftest["F"]),
              as.numeric(wald_9_3way$Ftest["df"]),
              as.numeric(wald_9_3way$df),
              ifelse(as.numeric(wald_9_3way$p) < 0.001,
                     "<0.001", sprintf("%.3f", as.numeric(wald_9_3way$p)))))
  tbl_adv09 <- tbl_adv09 %>%
    mutate(
      joint_wald_F   = ifelse(term_type == "Three-way",
                              as.numeric(wald_9_3way$Ftest["F"]), NA_real_),
      joint_wald_p   = ifelse(term_type == "Three-way",
                              as.numeric(wald_9_3way$p), NA_real_)
    )
  write_csv(tbl_adv09,
            file.path(out_tables, "tbl_adv09_threeway.csv"))
}


# =============================================================================
# ANALYSIS 10: MULTIPLE TESTING CORRECTION (Benjamini-Hochberg FDR)
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS 10: Multiple Testing Correction (Benjamini-Hochberg FDR)\n")
cat(strrep("=", 65), "\n")

# Collect all p-values from the three interaction models (Analysis 2)
# Focus on interaction terms only

p_vals_list <- list(
  tidy_svyglm(m2_A, model_label = "wave_x_party") %>%
    filter(grepl(":", term)) %>%
    mutate(model = "wave × party"),
  tidy_svyglm(m2_B, model_label = "wave_x_education") %>%
    filter(grepl(":", term)) %>%
    mutate(model = "wave × education"),
  tidy_svyglm(m2_C, model_label = "wave_x_age") %>%
    filter(grepl(":", term)) %>%
    mutate(model = "wave × age_cat")
)

tbl_pvals <- bind_rows(p_vals_list) %>%
  select(model, term, estimate, std_error, z_value, p_value, sig)

# Apply BH FDR correction
tbl_adv10 <- tbl_pvals %>%
  mutate(
    p_adj_BH   = p.adjust(p_value, method = "BH"),
    sig_raw    = sig_stars(p_value),
    sig_adj    = sig_stars(p_adj_BH),
    p_raw_fmt  = ifelse(p_value  < 0.001, "<0.001", sprintf("%.4f", p_value)),
    p_adj_fmt  = ifelse(p_adj_BH < 0.001, "<0.001", sprintf("%.4f", p_adj_BH)),
    OR         = exp(estimate),
    OR_fmt     = sprintf("%.3f", OR)
  ) %>%
  arrange(p_value) %>%
  select(model, term, OR, OR_fmt, estimate, std_error, z_value,
         p_value, p_raw_fmt, sig_raw, p_adj_BH, p_adj_fmt, sig_adj)

write_csv(tbl_adv10,
          file.path(out_tables, "tbl_adv10_fdr_correction.csv"))
cat("Saved: tbl_adv10_fdr_correction.csv\n")

cat("\n  Multiple-testing corrected results (interaction terms):\n")
tbl_adv10 %>%
  select(model, term, OR_fmt, p_raw_fmt, sig_raw, p_adj_fmt, sig_adj) %>%
  print(n = Inf)

# Summary count of significant terms before/after correction
n_sig_raw <- sum(tbl_adv10$p_value  < 0.05, na.rm = TRUE)
n_sig_adj <- sum(tbl_adv10$p_adj_BH < 0.05, na.rm = TRUE)
cat(sprintf("\n  Terms significant at p < .05 (raw): %d / %d\n",
            n_sig_raw, nrow(tbl_adv10)))
cat(sprintf("  Terms significant at q < .05 (BH FDR): %d / %d\n",
            n_sig_adj, nrow(tbl_adv10)))


# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("04_advanced_analysis.R COMPLETE\n")
cat(strrep("=", 65), "\n\n")

cat(sprintf("Analytic sample: N = %d (complete cases, party leaners only)\n",
            nrow(df_cc)))
cat(sprintf("  Wave 1 (Dec 2022): N = %d\n",
            sum(df_cc$wave_date == "Dec 2022")))
cat(sprintf("  Wave 2 (Aug 2023): N = %d\n",
            sum(df_cc$wave_date == "Aug 2023")))
cat(sprintf("  Wave 3 (Aug 2024): N = %d\n",
            sum(df_cc$wave_date == "Aug 2024")))

cat("\nTables written:\n")
tables_written <- c(
  "tbl_adv01_binary_models.csv",
  "tbl_adv02_interaction_wave_factor.csv",
  "tbl_adv03_ames_main.csv",
  "tbl_adv03_ames_by_wave.csv",
  "tbl_adv03_avg_predictions.csv",
  "tbl_adv04_wald_tests.csv",
  "tbl_adv05_oaxaca.csv",
  "tbl_adv06_party_ideo_models.csv",
  "tbl_adv07_polarization_metrics.csv",
  "tbl_adv08_robustness.csv",
  "tbl_adv09_threeway.csv",
  "tbl_adv10_fdr_correction.csv"
)
for (t in tables_written) cat(sprintf("  output/tables/%s\n", t))

cat("\nFigures written:\n")
figs_written <- c(
  "fig_adv03_ames_main.png",
  "fig_adv03_avg_pred_party.png",
  "fig_adv03_avg_pred_education.png",
  "fig_adv05_oaxaca.png",
  "fig_adv06_party_ideo.png",
  "fig_adv07_polarization_metrics.png"
)
for (f in figs_written) cat(sprintf("  output/figures/%s\n", f))

cat("\nReference categories:\n")
cat("  wave_date:    'Dec 2022'\n")
cat("  party:        'Dem/Lean Dem'\n")
cat("  party_ideo_4: 'Lib Dem'\n")
cat("  education:    'College graduate+'\n")
cat("  age_cat:      '30-49'\n")
cat("  race:         'White non-Hispanic'\n")
cat("  income_tier:  'Middle'\n")
cat("  gender:       'Male'\n")
cat("  ai_heard:     'A lot'\n")
cat("  region:       'South'\n")
