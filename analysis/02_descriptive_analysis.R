# =============================================================================
# 02_descriptive_analysis.R
# AI Attitude Polarization Study — Pew ATP Data
#
# Purpose:  Produce weighted descriptive statistics, polarization metrics,
#           and publication-quality visualizations from pew_combined.rds.
#
# Input:    data/processed/pew_combined.rds
# Outputs:  output/tables/table1_cncexc_trend.csv
#           output/tables/table2_demographic_crosstabs.csv
#           output/tables/table3_polarization_index.csv
#           output/figures/fig1_cncexc_trend.png
#           output/figures/fig2_political_polarization.png
#           output/figures/fig3_education_gap.png
#           output/figures/fig4_age_divergence.png
#           output/figures/fig5_awareness_attitude.png
#
# Data variables used:
#   cncexc      : factor ("Excited", "Concerned", "Equally")
#   concerned   : binary (1 = concerned, 0 = not)
#   excited     : binary (1 = excited, 0 = not)
#   ai_heard    : factor ("A lot", "A little", "Nothing at all")
#   useai       : factor (AI use frequency; NA for Wave 2 / W132)
#   wave        : numeric (1, 2, 3)
#   wave_date   : character ("Dec 2022", "Aug 2023", "Aug 2024")
#   weight      : survey weight
#   age_cat     : factor ("18-29", "30-49", "50-64", "65+")
#   gender      : factor ("Man", "Woman", "Other")
#   education   : factor ("College graduate+", "Some College",
#                          "HS graduate or less")
#   race        : factor ("White NH", "Black NH", "Hispanic", "Other",
#                          "Asian NH")
#   party       : factor ("Rep/Lean Rep", "Dem/Lean Dem")
#   party_ideo  : factor ("Conservative Rep", "Mod/Lib Rep",
#                          "Mod/Con Dem", "Liberal Dem")
#   income_tier : factor ("Lower", "Middle", "Upper")
#   region      : factor ("Northeast", "Midwest", "South", "West")
#   metro       : factor ("Metropolitan", "Non-metropolitan")
#
# Author:  [Your Name]
# Date:    2026-02-22
# =============================================================================


# =============================================================================
# 0. SETUP
# =============================================================================

# ---- 0a. Install / load packages --------------------------------------------

required_pkgs <- c("tidyverse", "survey", "srvyr", "scales", "ggplot2")

invisible(lapply(required_pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, quiet = TRUE)
  }
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

cat("Packages loaded.\n")


# ---- 0b. Paths --------------------------------------------------------------

# If running interactively, set working directory to project root:
# setwd("/Users/hosung/AI_Polarization_Pew")

DATA_PATH   <- "data/processed/pew_combined.rds"
FIGURES_DIR <- "output/figures"
TABLES_DIR  <- "output/tables"

dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLES_DIR,  recursive = TRUE, showWarnings = FALSE)


# ---- 0c. Color palette (per specification) ----------------------------------

CLR_EXCITED  <- "#2166AC"   # blue
CLR_CONCERNED <- "#B2182B"  # red
CLR_EQUALLY  <- "#999999"   # gray

# Named vector for consistent mapping throughout all figures
ATTITUDE_COLORS <- c(
  "Excited"   = CLR_EXCITED,
  "Concerned" = CLR_CONCERNED,
  "Equally"   = CLR_EQUALLY
)

# Caption reused on all figures
CAPTION_STR <- paste0(
  "Source: Pew Research Center American Trends Panel (Dec 2022, Aug 2023, ",
  "Aug 2024).\nEstimates are survey-weighted."
)


# ---- 0d. Publication ggplot2 theme ------------------------------------------

theme_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      # Grid
      panel.grid.minor    = element_blank(),
      panel.grid.major    = element_line(color = "grey90", linewidth = 0.4),
      # Axes
      axis.title          = element_text(size = rel(0.95), color = "grey20"),
      axis.text           = element_text(size = rel(0.85), color = "grey30"),
      axis.ticks          = element_line(color = "grey60"),
      # Legend
      legend.position     = "bottom",
      legend.title        = element_text(size = rel(0.9), face = "bold"),
      legend.text         = element_text(size = rel(0.85)),
      legend.key.size     = unit(0.9, "lines"),
      # Titles
      plot.title          = element_text(size = rel(1.1), face = "bold",
                                         margin = margin(b = 5)),
      plot.subtitle       = element_text(size = rel(0.9), color = "grey45",
                                         margin = margin(b = 10)),
      plot.caption        = element_text(size = rel(0.72), color = "grey55",
                                         hjust = 0, margin = margin(t = 8)),
      plot.title.position = "plot",
      # Facets
      strip.background    = element_rect(fill = "grey95", color = "grey75"),
      strip.text          = element_text(size = rel(0.88), face = "bold"),
      # Margins
      plot.margin         = margin(12, 14, 8, 12)
    )
}


# ---- 0e. Save-figure helper -------------------------------------------------

save_fig <- function(plot, filename, width = 8, height = 5, dpi = 300) {
  path <- file.path(FIGURES_DIR, filename)
  ggsave(filename = path, plot = plot,
         width = width, height = height,
         dpi = dpi, bg = "white")
  cat(sprintf("  Saved figure: %s\n", path))
  invisible(path)
}


# =============================================================================
# 1. LOAD DATA
# =============================================================================

cat("\n--- 1. Loading data ---\n")

if (!file.exists(DATA_PATH)) {
  stop(sprintf(
    "Data file not found: %s\nRun 01_data_preparation.R first.", DATA_PATH
  ))
}

df <- readRDS(DATA_PATH)

cat(sprintf("Loaded: %d rows x %d columns\n", nrow(df), ncol(df)))
cat("Waves present:", paste(sort(unique(df$wave)), collapse = ", "), "\n")
cat("Wave dates   :", paste(unique(df$wave_date[order(df$wave)]),
                             collapse = ", "), "\n")
cat("cncexc levels:", paste(levels(df$cncexc), collapse = " | "), "\n\n")

# Ensure wave_date is ordered for plotting
df <- df |>
  mutate(wave_date = factor(wave_date,
                             levels = c("Dec 2022", "Aug 2023", "Aug 2024")))


# =============================================================================
# 2. SURVEY DESIGN OBJECT
# =============================================================================

cat("--- 2. Building survey design ---\n")

svy <- df |>
  filter(!is.na(weight), !is.na(cncexc)) |>
  as_survey_design(weights = weight)

cat(sprintf("Survey design created: %d observations (non-missing weight & cncexc)\n\n",
            nrow(svy$variables)))


# =============================================================================
# 3. TABLE 1: WEIGHTED CNCEXC TREND TABLE
# =============================================================================

cat("--- 3. Table 1: Weighted CNCEXC trend ---\n")

tbl1_raw <- svy |>
  group_by(wave_date, cncexc) |>
  summarise(
    prop = survey_mean(vartype = "ci", level = 0.95),
    n    = unweighted(n()),
    .groups = "drop"
  )

# Wide format: each attitude as its own column set
tbl1_wide <- tbl1_raw |>
  select(wave_date, cncexc, prop, prop_low, prop_upp, n) |>
  pivot_wider(
    names_from  = cncexc,
    values_from = c(prop, prop_low, prop_upp, n),
    names_glue  = "{cncexc}_{.value}"
  )

# Print formatted table
cat("\nTable 1: Weighted CNCEXC proportions by wave\n")
cat(rep("-", 70), "\n", sep = "")

tbl1_print <- tbl1_raw |>
  mutate(
    pct    = sprintf("%.1f%%", prop * 100),
    ci     = sprintf("[%.1f%%, %.1f%%]", prop_low * 100, prop_upp * 100),
    label  = paste0(pct, " ", ci)
  ) |>
  select(wave_date, cncexc, label, n) |>
  pivot_wider(names_from = cncexc, values_from = c(label, n))

print(as.data.frame(tbl1_print))
cat(rep("-", 70), "\n\n", sep = "")

# Save
write.csv(tbl1_raw,
          file.path(TABLES_DIR, "table1_cncexc_trend.csv"),
          row.names = FALSE)
cat("  Saved: output/tables/table1_cncexc_trend.csv\n\n")


# =============================================================================
# 4. FIGURE 1: CNCEXC TREND OVER TIME (LINE CHART)
# =============================================================================

cat("--- 4. Figure 1: CNCEXC trend line chart ---\n")

# Order levels: Excited on top, Concerned, Equally
fig1_data <- tbl1_raw |>
  mutate(pct = prop * 100,
         pct_low  = prop_low  * 100,
         pct_high = prop_upp  * 100,
         cncexc = factor(cncexc, levels = c("Excited", "Concerned", "Equally")))

fig1 <- ggplot(fig1_data,
               aes(x = wave_date, y = pct, color = cncexc,
                   group = cncexc)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.2) +
  # Percentage labels offset above each point
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            vjust = -1.1, size = 3.4, fontface = "bold",
            show.legend = FALSE) +
  scale_color_manual(
    values = ATTITUDE_COLORS,
    name   = "Attitude toward AI",
    guide  = guide_legend(override.aes = list(linewidth = 1.5, size = 3))
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    limits = c(0, 70),
    breaks = seq(0, 70, by = 10)
  ) +
  labs(
    title    = "Public Attitudes Toward AI, 2022–2024",
    subtitle = "Weighted % who feel excited, concerned, or equally both",
    x        = NULL,
    y        = "Weighted Percentage (%)",
    caption  = CAPTION_STR
  ) +
  theme_pub()

save_fig(fig1, "fig1_cncexc_trend.png", width = 8, height = 5)


# =============================================================================
# 5. TABLE 2: DEMOGRAPHIC CROSSTABS (WEIGHTED + CHI-SQUARE)
# =============================================================================

cat("--- 5. Table 2: Demographic crosstabs ---\n")

# Demographics to cross-tabulate
DEMO_VARS <- c("age_cat", "gender", "education", "race",
               "party", "income_tier")

# ---- Helper: weighted cross-tab for one demographic variable ----------------
weighted_crosstab <- function(svy_design, demo_var) {
  sym_var <- sym(demo_var)

  svy_design |>
    filter(!is.na(!!sym_var)) |>
    group_by(wave_date, !!sym_var, cncexc) |>
    summarise(
      prop     = survey_mean(vartype = "ci", level = 0.95),
      n_unwt   = unweighted(n()),
      .groups  = "drop"
    ) |>
    rename(subgroup = !!sym_var) |>
    mutate(demo_var  = demo_var,
           subgroup  = as.character(subgroup))
}

# ---- Helper: Rao-Scott design-adjusted chi-square per wave ------------------
chisq_per_wave <- function(svy_design, demo_var) {
  waves <- sort(unique(svy_design$variables$wave_date))
  fmla  <- as.formula(paste("~ cncexc +", demo_var))

  map_dfr(waves, function(w) {
    sub <- svy_design |>
      filter(wave_date == w, !is.na(.data[[demo_var]]))
    # Need at least 2 non-empty cells per row/col for test to run
    tst <- tryCatch(
      svychisq(fmla, design = sub, statistic = "Chisq"),
      error = function(e) NULL
    )
    if (is.null(tst)) {
      tibble(wave_date = w, demo_var = demo_var,
             chi_sq = NA_real_, df = NA_real_, p_value = NA_real_)
    } else {
      tibble(wave_date = w, demo_var = demo_var,
             chi_sq   = round(as.numeric(tst$statistic), 3),
             df       = as.numeric(tst$parameter),
             p_value  = round(tst$p.value, 4))
    }
  })
}

# Run all crosstabs and chi-square tests
cat("  Computing weighted crosstabs for:", paste(DEMO_VARS, collapse = ", "), "\n")

crosstabs_list <- map(DEMO_VARS, ~ weighted_crosstab(svy, .x))
names(crosstabs_list) <- DEMO_VARS

tbl2_long <- bind_rows(crosstabs_list)

cat("  Running Rao-Scott chi-square tests...\n")
chisq_all <- map_dfr(DEMO_VARS, ~ chisq_per_wave(svy, .x))

# Merge chi-square results into the crosstab table
tbl2_final <- tbl2_long |>
  left_join(chisq_all, by = c("wave_date", "demo_var"))

# Print summary
cat("\nChi-square test results:\n")
print(as.data.frame(chisq_all))

write.csv(tbl2_final,
          file.path(TABLES_DIR, "table2_demographic_crosstabs.csv"),
          row.names = FALSE)
cat("\n  Saved: output/tables/table2_demographic_crosstabs.csv\n\n")


# =============================================================================
# 6. FIGURE 2: POLITICAL POLARIZATION (party × wave, faceted)
# =============================================================================

cat("--- 6. Figure 2: Political polarization ---\n")

fig2_data <- crosstabs_list[["party"]] |>
  filter(!is.na(subgroup)) |>
  mutate(
    cncexc   = factor(cncexc, levels = c("Excited", "Equally", "Concerned")),
    subgroup = factor(subgroup, levels = c("Rep/Lean Rep", "Dem/Lean Dem"))
  )

fig2 <- ggplot(fig2_data,
               aes(x = subgroup, y = prop, fill = cncexc)) +
  geom_col(position = "dodge", width = 0.72, color = "white",
           linewidth = 0.25) +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp),
                position  = position_dodge(width = 0.72),
                width     = 0.2,
                linewidth = 0.45,
                color     = "grey30") +
  facet_wrap(~ wave_date, ncol = 3) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, 0.75)) +
  scale_fill_manual(values = ATTITUDE_COLORS,
                    name   = "Attitude toward AI") +
  labs(
    title    = "AI Attitudes by Party Identification, 2022–2024",
    subtitle = "Weighted proportions with 95% confidence intervals",
    x        = NULL,
    y        = "Weighted Proportion",
    caption  = CAPTION_STR
  ) +
  theme_pub()

save_fig(fig2, "fig2_political_polarization.png", width = 10, height = 6)


# =============================================================================
# 7. FIGURE 3: EDUCATION GAP (education × wave, faceted)
# =============================================================================

cat("--- 7. Figure 3: Education gap ---\n")

# Ordered education factor
educ_order <- c("College graduate+", "Some College", "HS graduate or less")

fig3_data <- crosstabs_list[["education"]] |>
  filter(!is.na(subgroup)) |>
  mutate(
    cncexc   = factor(cncexc, levels = c("Excited", "Equally", "Concerned")),
    subgroup = factor(subgroup, levels = educ_order)
  )

fig3 <- ggplot(fig3_data,
               aes(x = subgroup, y = prop, fill = cncexc)) +
  geom_col(position = "dodge", width = 0.72, color = "white",
           linewidth = 0.25) +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp),
                position  = position_dodge(width = 0.72),
                width     = 0.2,
                linewidth = 0.45,
                color     = "grey30") +
  facet_wrap(~ wave_date, ncol = 3) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, 0.75)) +
  scale_fill_manual(values = ATTITUDE_COLORS,
                    name   = "Attitude toward AI") +
  scale_x_discrete(labels = function(x)
    stringr::str_wrap(x, width = 14)) +
  labs(
    title    = "AI Attitudes by Education Level, 2022–2024",
    subtitle = "Weighted proportions with 95% confidence intervals",
    x        = NULL,
    y        = "Weighted Proportion",
    caption  = CAPTION_STR
  ) +
  theme_pub() +
  theme(axis.text.x = element_text(size = rel(0.82)))

save_fig(fig3, "fig3_education_gap.png", width = 10, height = 6)


# =============================================================================
# 8. FIGURE 4: AGE-BASED DIVERGENCE (age_cat × wave, faceted)
# =============================================================================

cat("--- 8. Figure 4: Age-based divergence ---\n")

age_order <- c("18-29", "30-49", "50-64", "65+")

fig4_data <- crosstabs_list[["age_cat"]] |>
  filter(!is.na(subgroup)) |>
  mutate(
    cncexc   = factor(cncexc, levels = c("Excited", "Equally", "Concerned")),
    subgroup = factor(subgroup, levels = age_order)
  )

fig4 <- ggplot(fig4_data,
               aes(x = subgroup, y = prop, fill = cncexc)) +
  geom_col(position = "dodge", width = 0.72, color = "white",
           linewidth = 0.25) +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp),
                position  = position_dodge(width = 0.72),
                width     = 0.2,
                linewidth = 0.45,
                color     = "grey30") +
  facet_wrap(~ wave_date, ncol = 3) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, 0.75)) +
  scale_fill_manual(values = ATTITUDE_COLORS,
                    name   = "Attitude toward AI") +
  labs(
    title    = "AI Attitudes by Age Group, 2022–2024",
    subtitle = "Weighted proportions with 95% confidence intervals",
    x        = "Age Group",
    y        = "Weighted Proportion",
    caption  = CAPTION_STR
  ) +
  theme_pub()

save_fig(fig4, "fig4_age_divergence.png", width = 10, height = 6)


# =============================================================================
# 9. TABLE 3: POLARIZATION INDEX
# =============================================================================

cat("--- 9. Table 3: Polarization index ---\n")

# Polarization Index = |%Excited - %Concerned| (weighted) per wave × subgroup
# Higher values indicate one sentiment strongly dominates.

compute_polarization_index <- function(crosstab_df, demo_label) {
  wide <- crosstab_df |>
    select(wave_date, subgroup, cncexc, prop) |>
    pivot_wider(names_from = cncexc, values_from = prop)

  # Safely add columns that may be absent after pivot (if a category has
  # zero respondents in some wave/subgroup combination)
  if (!hasName(wide, "Excited"))   wide$Excited   <- NA_real_
  if (!hasName(wide, "Concerned")) wide$Concerned <- NA_real_
  if (!hasName(wide, "Equally"))   wide$Equally   <- NA_real_

  wide |>
    mutate(
      demo_var         = demo_label,
      polarization_idx = abs(Excited - Concerned),
      dominant         = case_when(
        !is.na(Excited) & !is.na(Concerned) & Excited   > Concerned ~ "Excited",
        !is.na(Excited) & !is.na(Concerned) & Concerned > Excited   ~ "Concerned",
        !is.na(Excited) & !is.na(Concerned)                         ~ "Tied",
        TRUE                                                         ~ NA_character_
      )
    ) |>
    select(wave_date, demo_var, subgroup,
           pct_excited   = Excited,
           pct_concerned = Concerned,
           pct_equally   = Equally,
           polarization_idx, dominant)
}

tbl3_parts <- imap(crosstabs_list, ~ compute_polarization_index(.x, .y))
tbl3 <- bind_rows(tbl3_parts)

# Add overall row (no demographic breakdown)
overall_pivot <- tbl1_raw |>
  select(wave_date, cncexc, prop) |>
  pivot_wider(names_from = cncexc, values_from = prop) |>
  mutate(
    demo_var         = "overall",
    subgroup         = "All respondents",
    polarization_idx = abs(Excited - Concerned),
    dominant         = case_when(
      Excited   > Concerned ~ "Excited",
      Concerned > Excited   ~ "Concerned",
      TRUE                  ~ "Tied"
    )
  ) |>
  rename(pct_excited   = Excited,
         pct_concerned = Concerned,
         pct_equally   = Equally) |>
  select(wave_date, demo_var, subgroup,
         pct_excited, pct_concerned, pct_equally,
         polarization_idx, dominant)

tbl3 <- bind_rows(overall_pivot, tbl3) |>
  arrange(demo_var, subgroup, wave_date) |>
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  )

# Print compact summary
cat("\nPolarization index summary (top 20 rows by polarization_idx):\n")
print(
  tbl3 |>
    arrange(desc(polarization_idx)) |>
    head(20) |>
    mutate(
      pct_excited   = sprintf("%.1f%%", pct_excited   * 100),
      pct_concerned = sprintf("%.1f%%", pct_concerned * 100),
      polarization_idx = sprintf("%.3f", polarization_idx)
    ) |>
    as.data.frame()
)

write.csv(tbl3,
          file.path(TABLES_DIR, "table3_polarization_index.csv"),
          row.names = FALSE)
cat("\n  Saved: output/tables/table3_polarization_index.csv\n\n")


# =============================================================================
# 10. FIGURE 5: AI AWARENESS AND ATTITUDE (ai_heard × wave)
# =============================================================================

cat("--- 10. Figure 5: AI awareness and attitude ---\n")

# Ordered awareness levels
heard_order <- c("A lot", "A little", "Nothing")

awareness_data <- svy |>
  filter(!is.na(ai_heard)) |>
  mutate(ai_heard = factor(ai_heard, levels = heard_order)) |>
  group_by(wave_date, ai_heard, cncexc) |>
  summarise(
    prop    = survey_mean(vartype = "ci", level = 0.95),
    n_unwt  = unweighted(n()),
    .groups = "drop"
  ) |>
  mutate(cncexc = factor(cncexc, levels = c("Excited", "Equally", "Concerned")))

fig5 <- ggplot(awareness_data,
               aes(x = ai_heard, y = prop, fill = cncexc)) +
  geom_col(position = "dodge", width = 0.72, color = "white",
           linewidth = 0.25) +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp),
                position  = position_dodge(width = 0.72),
                width     = 0.2,
                linewidth = 0.45,
                color     = "grey30") +
  facet_wrap(~ wave_date, ncol = 3) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, 0.80)) +
  scale_fill_manual(values = ATTITUDE_COLORS,
                    name   = "Attitude toward AI") +
  scale_x_discrete(labels = function(x)
    stringr::str_wrap(x, width = 10)) +
  labs(
    title    = "AI Attitudes by Prior Awareness of AI, 2022–2024",
    subtitle = "Weighted proportions with 95% confidence intervals",
    x        = "How much have you heard about AI?",
    y        = "Weighted Proportion",
    caption  = CAPTION_STR
  ) +
  theme_pub()

save_fig(fig5, "fig5_awareness_attitude.png", width = 10, height = 6)


# =============================================================================
# 11. FINAL SUMMARY
# =============================================================================

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("Analysis complete.\n\n")

cat("FIGURES saved to:", FIGURES_DIR, "\n")
cat("  fig1_cncexc_trend.png          — Line chart: 3 attitudes over time\n")
cat("  fig2_political_polarization.png — Bar chart: party × wave\n")
cat("  fig3_education_gap.png          — Bar chart: education × wave\n")
cat("  fig4_age_divergence.png         — Bar chart: age group × wave\n")
cat("  fig5_awareness_attitude.png     — Bar chart: ai_heard × wave\n")

cat("\nTABLES saved to:", TABLES_DIR, "\n")
cat("  table1_cncexc_trend.csv         — Weighted CNCEXC proportions by wave\n")
cat("  table2_demographic_crosstabs.csv — Crosstabs + Rao-Scott chi-sq tests\n")
cat("  table3_polarization_index.csv   — |%Excited - %Concerned| by group\n")
cat("\nNext: run 03_regression_analysis.R\n")
cat(rep("=", 70), "\n", sep = "")
