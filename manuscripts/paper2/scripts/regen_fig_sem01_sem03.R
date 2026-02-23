#!/usr/bin/env Rscript
# Regenerate fig_sem01_ame_by_class.png and fig_sem03_predicted_probs.png
# Uses pre-computed tbl_sem02_ame_by_class.csv and re-fits m_full from data

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(scales)
  library(nnet)
})

here_base <- "/Users/hosung/AI_Polarization_Pew/manuscripts/paper2"
dir_out   <- file.path(here_base, "output")
dir_tbl   <- file.path(dir_out, "tables")
dir_fig   <- file.path(dir_out, "figures")

# Class label lookup
class_profile_labels <- c(
  "1" = "AI-Anxious", "2" = "AI-Uninformed",
  "3" = "AI-Advantaged", "4" = "AI-Ambivalent"
)

remap_class <- function(x) {
  dplyr::case_when(
    x %in% names(class_profile_labels) ~ class_profile_labels[x],
    grepl("^class_x?1$", x) ~ "AI-Anxious",
    grepl("^class_x?2$", x) ~ "AI-Uninformed",
    grepl("^class_x?3$", x) ~ "AI-Advantaged",
    grepl("^class_x?4$", x) ~ "AI-Ambivalent",
    TRUE ~ x
  )
}

# ── Fig 1: AME Forest Plot ──────────────────────────────────────────────────
message("Drawing fig_sem01_ame_by_class.png...")
tbl_ame <- read.csv(file.path(dir_tbl, "tbl_sem02_ame_by_class.csv"),
                    stringsAsFactors = FALSE)

if (nrow(tbl_ame) > 0) {
  ame_plot <- tbl_ame |>
    mutate(
      predictor_cat = case_when(
        term %in% c("edu_some_college", "edu_hs_or_less",
                    "income_lower", "income_upper")       ~ "SES",
        term %in% c("race_black", "race_hispanic",
                    "race_asian", "race_other")           ~ "Race/Ethnicity",
        term %in% c("party_rep", "party_ind")             ~ "Partisan",
        term == "awareness_num"                           ~ "AI Awareness",
        TRUE                                              ~ "Demographic"
      ),
      term_label = dplyr::recode(term,
        edu_some_college = "Some college",
        edu_hs_or_less   = "HS or less",
        income_lower     = "Lower income",
        income_upper     = "Upper income",
        race_black       = "Black NH",
        race_hispanic    = "Hispanic",
        race_asian       = "Asian NH",
        race_other       = "Other race",
        age_1829         = "Age 18\u201329",
        age_5064         = "Age 50\u201364",
        age_65plus       = "Age 65+",
        gender_female    = "Female",
        party_rep        = "Republican",
        awareness_num    = "AI awareness (1\u21923)"
      ),
      group_label = remap_class(as.character(group))
    )

  if (!"conf.low" %in% names(ame_plot)) {
    ame_plot <- ame_plot |>
      mutate(conf.low  = estimate - 1.96 * std.error,
             conf.high = estimate + 1.96 * std.error)
  }

  cat_colors <- c(
    "SES"           = "#1f77b4",
    "Race/Ethnicity" = "#ff7f0e",
    "Partisan"      = "#d62728",
    "AI Awareness"  = "#2ca02c",
    "Demographic"   = "#7f7f7f"
  )

  p1 <- ggplot(ame_plot,
               aes(x = estimate, y = reorder(term_label, estimate),
                   color = predictor_cat)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.25, linewidth = 0.6) +
    geom_point(size = 2) +
    facet_wrap(~ group_label, scales = "free_x", ncol = 3) +
    scale_color_manual(values = cat_colors, name = "Predictor\ncategory") +
    labs(
      title    = "Average Marginal Effects on Latent Class Membership",
      subtitle = "Full model; 95% CI; reference: College grad+, Middle income, White NH, Age 30\u201349, Male, Democrat",
      x        = "AME on Pr(class membership)",
      y        = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(strip.background = element_rect(fill = "gray90"),
          legend.position  = "bottom")

  ggsave(file.path(dir_fig, "fig_sem01_ame_by_class.png"),
         p1, width = 12, height = 8, dpi = 300, bg = "white")
  message("Saved fig_sem01_ame_by_class.png")
} else {
  message("No AME data — skipping fig_sem01.")
}

# ── Fig 3: Predicted Probabilities ─────────────────────────────────────────
message("\nDrawing fig_sem03_predicted_probs.png...")

# Need to re-fit the model — load Form A data
df_raw <- readRDS(file.path(dir_out, "paper2_form_a_with_class.rds"))
df_raw$lca_class <- factor(df_raw$lca_class)

# Recode predictors (same logic as 03_sem.R)
awareness_var <- if ("ai_heard" %in% names(df_raw)) "ai_heard" else "awareness"
edu_var       <- if ("education_cat" %in% names(df_raw)) "education_cat" else
                 if ("education"     %in% names(df_raw)) "education"     else
                 if ("educ_cat"      %in% names(df_raw)) "educ_cat"      else "educ"
inc_var       <- if ("income_tier"   %in% names(df_raw)) "income_tier"   else
                 if ("income_cat"    %in% names(df_raw)) "income_cat"    else "income"
race_var      <- if ("race_eth"      %in% names(df_raw)) "race_eth"      else
                 if ("race_ethn"     %in% names(df_raw)) "race_ethn"     else "race"
age_var       <- if ("age_cat"       %in% names(df_raw)) "age_cat"       else
                 if ("age_group"     %in% names(df_raw)) "age_group"     else "age"
gen_var       <- if ("gender"        %in% names(df_raw)) "gender"        else "sex"
party_var     <- if ("party_id"      %in% names(df_raw)) "party_id"      else
                 if ("party"         %in% names(df_raw)) "party"         else "partisan"

df_raw <- df_raw |>
  mutate(
    awareness_num = case_when(
      as.character(.data[[awareness_var]]) %in% c("Nothing at all","Nothing","1") ~ 1L,
      as.character(.data[[awareness_var]]) %in% c("A little","A little bit","2")  ~ 2L,
      as.character(.data[[awareness_var]]) %in% c("A lot","3")                    ~ 3L,
      TRUE ~ as.integer(as.character(.data[[awareness_var]]))
    ),
    edu_hs_or_less   = as.integer(grepl("HS|high school|less",   .data[[edu_var]],   ignore.case=TRUE)),
    edu_some_college = as.integer(grepl("some college|2-year|assoc", .data[[edu_var]], ignore.case=TRUE)),
    income_lower     = as.integer(grepl("lower|low|under",       .data[[inc_var]],   ignore.case=TRUE)),
    income_upper     = as.integer(grepl("upper|high|over",       .data[[inc_var]],   ignore.case=TRUE)),
    race_black       = as.integer(grepl("^black|^african",       .data[[race_var]],  ignore.case=TRUE)),
    race_hispanic    = as.integer(grepl("^hispanic|^latino",     .data[[race_var]],  ignore.case=TRUE)),
    race_asian       = as.integer(grepl("^asian",                .data[[race_var]],  ignore.case=TRUE)),
    race_other       = as.integer(grepl("^other",                .data[[race_var]],  ignore.case=TRUE)),
    age_1829         = as.integer(grepl("18.?29|18-29",          .data[[age_var]],   ignore.case=TRUE)),
    age_5064         = as.integer(grepl("50.?64|50-64",          .data[[age_var]],   ignore.case=TRUE)),
    age_65plus       = as.integer(grepl("65|older",              .data[[age_var]],   ignore.case=TRUE)),
    gender_female    = as.integer(grepl("female|woman",          .data[[gen_var]],   ignore.case=TRUE)),
    gender_other     = as.integer(grepl("other|non-binary",      .data[[gen_var]],   ignore.case=TRUE)),
    party_rep        = as.integer(grepl("rep|republican",        .data[[party_var]], ignore.case=TRUE))
  )

key_vars <- c("lca_class","awareness_num","edu_hs_or_less","edu_some_college",
              "income_lower","income_upper","race_black","race_hispanic","race_asian",
              "age_1829","age_5064","age_65plus","gender_female","party_rep")

df_a <- df_raw |> filter(if_all(all_of(key_vars), ~ !is.na(.))) |> as.data.frame()
if (!"weight" %in% names(df_a)) df_a$weight <- 1

ref_class <- names(sort(table(df_a$lca_class), decreasing = TRUE))[1]
df_a$lca_class <- relevel(df_a$lca_class, ref = ref_class)
message("  Form A N = ", nrow(df_a), "  ref class = ", ref_class)

pred_demo  <- "age_1829 + age_5064 + age_65plus + gender_female + party_rep"
pred_race  <- "race_black + race_hispanic + race_asian + race_other"
pred_ses   <- "edu_some_college + edu_hs_or_less + income_lower + income_upper"
pred_aware <- "awareness_num"
f_full <- paste("lca_class ~", pred_demo, "+", pred_race, "+", pred_ses, "+", pred_aware)

message("  Fitting full multinomial model...")
m_full <- tryCatch(
  multinom(as.formula(f_full), data = df_a, weights = df_a$weight,
           maxit = 500, trace = FALSE),
  error = function(e) { message("  Model error: ", e$message); NULL }
)

if (!is.null(m_full)) {
  race_labels <- c("white"="White NH","black"="Black NH",
                   "hispanic"="Hispanic","asian"="Asian NH")

  edu_levels <- tibble(
    edu_label        = c("College grad+", "Some college", "HS or less"),
    edu_some_college = c(0, 1, 0),
    edu_hs_or_less   = c(0, 0, 1)
  )

  race_scenarios <- tibble(
    race_label    = names(race_labels),
    race_black    = c(0, 1, 0, 0),
    race_hispanic = c(0, 0, 1, 0),
    race_asian    = c(0, 0, 0, 1),
    race_other    = c(0, 0, 0, 0)
  )

  scenario_grid <- crossing(edu_levels, race_scenarios) |>
    mutate(
      awareness_num = median(df_a$awareness_num, na.rm = TRUE),
      income_lower  = 0, income_upper = 0,
      age_1829      = 0, age_5064 = 0, age_65plus = 0,
      gender_female = 1, party_rep = 0
    )

  pred_probs <- predict(m_full, newdata = scenario_grid, type = "probs") |>
    as_tibble() |>
    bind_cols(scenario_grid |> select(edu_label, race_label)) |>
    pivot_longer(-c(edu_label, race_label),
                 names_to = "lca_class", values_to = "pred_prob") |>
    mutate(
      edu_label       = factor(edu_label,
                               levels = c("HS or less","Some college","College grad+")),
      race_label      = dplyr::recode(race_label, !!!race_labels),
      lca_class_label = remap_class(as.character(lca_class))
    )

  p3 <- ggplot(pred_probs,
               aes(x = edu_label, y = pred_prob,
                   color = race_label, group = race_label)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    facet_wrap(~ lca_class_label, scales = "free_y", ncol = 3) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_brewer(palette = "Set1", name = "Race/Ethnicity") +
    labs(
      title    = "Predicted Probability of Latent Class Membership\nby Education Level and Race/Ethnicity",
      subtitle = "Holding: Middle income, Median AI awareness, Female, Democrat/Lean Dem, Age 30\u201349",
      x        = "Education level",
      y        = "Predicted probability of class membership"
    ) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x      = element_text(angle = 20, hjust = 1),
      strip.background = element_rect(fill = "gray90"),
      legend.position  = "bottom"
    )

  ggsave(file.path(dir_fig, "fig_sem03_predicted_probs.png"),
         p3, width = 11, height = 7, dpi = 300, bg = "white")
  message("Saved fig_sem03_predicted_probs.png")
} else {
  message("Model unavailable — skipping fig_sem03.")
}

message("Done.")
