# =============================================================================
# 04_descriptives.R
# Paper 2: The AI Divide — Descriptive Statistics & Visualizations
# Target Journal: New Media & Society
# =============================================================================
#
# Purpose:
#   Generates all descriptive tables and figures for the "AI Divide" paper,
#   documenting socioeconomic and intersectional patterns in AI attitudes
#   across education, race/ethnicity, and income using Pew ATP Wave 132
#   (Aug 2023), with trend context from W119 (Dec 2022) and W152 (Aug 2024).
#
# Input:
#   paper2_ai_divide/output/paper2_w132_full.rds    (primary; fallback to enriched)
#   data/processed/pew_w132_enriched.rds            (fallback)
#   data/processed/pew_w119.rds                     (trend)
#   data/processed/pew_w132.rds                     (trend)
#   data/processed/pew_w152.rds                     (trend)
#
# Outputs — Tables (paper2_ai_divide/output/tables/):
#   tbl_01_sample_description.csv
#   tbl_02_attitude_by_edu_race.csv
#   tbl_03_awareness_by_ses.csv
#   tbl_04_domain_by_education.csv
#
# Outputs — Figures (paper2_ai_divide/output/figures/, 300 dpi PNG):
#   fig01_attitude_trend.png         (8 × 5 in)
#   fig02_education_attitude_mosaic.png  (9 × 6 in)
#   fig03_awareness_by_ses.png       (8 × 5 in)
#   fig04_intersectional_concern.png (10 × 6 in)
#   fig05_domain_by_education.png    (9 × 7 in)
#
# Author:  [Your Name]
# Date:    2026-02-23
# =============================================================================


# =============================================================================
# 0. SETUP
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(survey)
  library(srvyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

cat("Packages loaded.\n")

# ---- 0b. Resolve project root -----------------------------------------------

project_root <- tryCatch(
  here::here(),
  error = function(e) "/Users/hosung/AI_Polarization_Pew"
)
cat("Project root:", project_root, "\n")

p2_root <- file.path(project_root, "paper2_ai_divide")

# Output paths
out_tables  <- file.path(p2_root, "output", "tables")
out_figures <- file.path(p2_root, "output", "figures")

dir.create(out_tables,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures, recursive = TRUE, showWarnings = FALSE)


# ---- 0c. Color palettes (per NM&S specification) ----------------------------

# Attitude colors (consistent with Paper 1 convention, adapted for NM&S)
CLR_CONCERNED <- "#E15759"
CLR_EXCITED   <- "#4E79A7"
CLR_EQUALLY   <- "#76B7B2"

ATTITUDE_COLORS <- c(
  "Concerned" = CLR_CONCERNED,
  "Excited"   = CLR_EXCITED,
  "Equally"   = CLR_EQUALLY
)

# Education gradient (high → low SES = dark → light blue → warm)
EDU_COLORS <- c(
  "College graduate+"   = "#2166AC",
  "Some College"        = "#92C5DE",
  "HS graduate or less" = "#F4A582"
)

# Race/ethnicity palette
RACE_COLORS <- c(
  "White non-Hispanic" = "#4E79A7",
  "Black non-Hispanic" = "#F28E2B",
  "Hispanic"           = "#E15759",
  "Asian non-Hispanic" = "#76B7B2",
  "Other non-Hispanic" = "#999999"
)

# Short race labels for figure axes
RACE_SHORT <- c(
  "White non-Hispanic" = "White",
  "Black non-Hispanic" = "Black",
  "Hispanic"           = "Hispanic",
  "Asian non-Hispanic" = "Asian",
  "Other non-Hispanic" = "Other"
)

CAPTION_STR <- paste0(
  "Source: Pew Research Center American Trends Panel, Wave 132 (Aug 2023).\n",
  "Estimates are survey-weighted."
)

CAPTION_TREND <- paste0(
  "Source: Pew Research Center American Trends Panel (Dec 2022, Aug 2023, Aug 2024).\n",
  "Estimates are survey-weighted."
)


# ---- 0d. Publication-quality ggplot2 theme ----------------------------------

theme_nms <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor    = element_blank(),
      panel.grid.major    = element_line(color = "grey92", linewidth = 0.35),
      axis.title          = element_text(size = rel(0.95), color = "grey20"),
      axis.text           = element_text(size = rel(0.85), color = "grey30"),
      axis.ticks          = element_line(color = "grey65", linewidth = 0.35),
      legend.position     = "bottom",
      legend.title        = element_text(size = rel(0.88), face = "bold"),
      legend.text         = element_text(size = rel(0.82)),
      legend.key.size     = unit(0.85, "lines"),
      # No plot title (will be added in manuscript captions)
      plot.title          = element_blank(),
      plot.subtitle       = element_blank(),
      plot.caption        = element_text(size = rel(0.70), color = "grey55",
                                         hjust = 0, margin = margin(t = 6)),
      plot.title.position = "plot",
      strip.background    = element_rect(fill = "grey95", color = "grey78",
                                         linewidth = 0.3),
      strip.text          = element_text(size = rel(0.88), face = "bold",
                                         margin = margin(t = 4, b = 4)),
      plot.margin         = margin(10, 12, 8, 10)
    )
}


# ---- 0e. Save figure helper --------------------------------------------------

save_fig <- function(plot, filename, width, height, dpi = 300) {
  path <- file.path(out_figures, filename)
  ggsave(filename = path, plot = plot,
         width = width, height = height, dpi = dpi, bg = "white")
  cat(sprintf("  Saved: %s\n", path))
  invisible(path)
}


# ---- 0f. Domain metadata (Form A items a-d; Form B items e-h) ---------------

DOMAIN_MAP <- tibble::tribble(
  ~var_suffix, ~form, ~domain_label,                               ~domain_short,
  "a",         "A",   "Finding accurate information online",       "Accurate info",
  "b",         "A",   "People taking care of their health",        "Health care",
  "c",         "A",   "Companies making safe cars/trucks",         "Vehicle safety",
  "d",         "A",   "Companies providing quality customer svc",  "Customer service",
  "e",         "B",   "Finding products and services online",      "Product search",
  "f",         "B",   "People keeping personal info private",      "Privacy",
  "g",         "B",   "Police maintaining public safety",          "Policing",
  "h",         "B",   "Doctors providing quality care",            "Doctor quality"
)


# =============================================================================
# 1. LOAD DATA
# =============================================================================

cat("\n--- 1. Loading data ---\n")

# ---- 1a. W132 primary analytic data -----------------------------------------

p2_data_path    <- file.path(p2_root, "output", "paper2_w132_full.rds")
enriched_path   <- file.path(project_root, "data", "processed", "pew_w132_enriched.rds")

if (file.exists(p2_data_path)) {
  cat("  Loading:", p2_data_path, "\n")
  df132 <- readRDS(p2_data_path)
} else if (file.exists(enriched_path)) {
  cat("  paper2_w132_full.rds not found. Using fallback:", enriched_path, "\n")
  df132 <- readRDS(enriched_path)
} else {
  stop("No W132 data file found. Run data preparation scripts first.")
}

cat(sprintf("  W132 loaded: %d rows x %d cols\n", nrow(df132), ncol(df132)))

# ---- 1b. Harmonize column names from enriched format ------------------------

# Rename _rc domain columns to canonical short names: AIHLPHRT_a ... AIHLPHRT_h
rc_cols   <- paste0("AIHLPHRT_", letters[1:8], "_W132_rc")
canon_cols <- paste0("AIHLPHRT_", letters[1:8])
for (i in seq_along(rc_cols)) {
  if (rc_cols[i] %in% names(df132) && !canon_cols[i] %in% names(df132)) {
    df132[[canon_cols[i]]] <- as.numeric(df132[[rc_cols[i]]])
  }
}

# Ensure income_tier column (may be named "income" in some versions)
if (!"income_tier" %in% names(df132) && "income" %in% names(df132)) {
  df132 <- rename(df132, income_tier = income)
}

# Derive form assignment from split-sample NA pattern
# (items a-d = Form A; items e-h = Form B; respondents answered only one form)
if (!"form" %in% names(df132)) {
  df132 <- df132 |>
    mutate(
      form = case_when(
        !is.na(AIHLPHRT_a) ~ "A",
        !is.na(AIHLPHRT_e) ~ "B",
        TRUE               ~ NA_character_
      )
    )
}

cat("  Form distribution (derived):\n")
print(table(df132$form, useNA = "ifany"))

# ---- 1c. Three-wave trend data ----------------------------------------------

w119_path <- file.path(project_root, "data", "processed", "pew_w119.rds")
w132_path <- file.path(project_root, "data", "processed", "pew_w132.rds")
w152_path <- file.path(project_root, "data", "processed", "pew_w152.rds")

pew_w119 <- readRDS(w119_path)
pew_w132 <- readRDS(w132_path)
pew_w152 <- readRDS(w152_path)

# Harmonize wave labels for plotting
add_wave_label <- function(df, label, date_label, wave_num) {
  df |>
    mutate(
      wave_label = label,
      wave_date  = date_label,
      wave_num   = wave_num
    )
}

trend_df <- bind_rows(
  add_wave_label(pew_w119, "W119 Dec 2022", "Dec 2022", 1),
  add_wave_label(pew_w132, "W132 Aug 2023", "Aug 2023", 2),
  add_wave_label(pew_w152, "W152 Aug 2024", "Aug 2024", 3)
) |>
  mutate(
    wave_date = factor(wave_date, levels = c("Dec 2022", "Aug 2023", "Aug 2024"))
  )

cat(sprintf("  Trend data: %d rows across 3 waves\n", nrow(trend_df)))


# =============================================================================
# 2. BUILD SURVEY DESIGN OBJECTS
# =============================================================================

cat("\n--- 2. Building survey design objects ---\n")

# W132 analytic sample (non-missing weight and cncexc)
df132_analytic <- df132 |>
  filter(!is.na(weight), weight > 0, !is.na(cncexc))

svy132 <- df132_analytic |>
  as_survey_design(weights = weight)

cat(sprintf("  W132 survey design: N = %d\n", nrow(df132_analytic)))

# Three-wave trend survey design
svy_trend <- trend_df |>
  filter(!is.na(weight), weight > 0, !is.na(cncexc)) |>
  as_survey_design(weights = weight)

cat(sprintf("  Trend survey design: N = %d\n", nrow(svy_trend$variables)))


# =============================================================================
# TABLE 1: SAMPLE DESCRIPTION
# =============================================================================

cat("\n--- Table 1: Sample Description ---\n")

# Helper: weighted proportion table for one variable
wtd_prop_table <- function(svy, var, group_col = NULL) {
  sym_var <- sym(var)

  if (is.null(group_col)) {
    out <- svy |>
      filter(!is.na(!!sym_var)) |>
      group_by(!!sym_var) |>
      summarise(
        wtd_pct = survey_mean(vartype = NULL) * 100,
        n_unwt  = unweighted(n()),
        .groups = "drop"
      ) |>
      rename(category = !!sym_var) |>
      mutate(variable = var, group = "Overall")
  } else {
    sym_grp <- sym(group_col)
    out <- svy |>
      filter(!is.na(!!sym_var), !is.na(!!sym_grp)) |>
      group_by(!!sym_grp, !!sym_var) |>
      summarise(
        wtd_pct = survey_mean(vartype = NULL) * 100,
        n_unwt  = unweighted(n()),
        .groups = "drop"
      ) |>
      rename(category = !!sym_var, group = !!sym_grp) |>
      mutate(variable = var)
  }
  out |>
    mutate(
      category = as.character(category),
      group    = as.character(group)
    )
}

demo_vars_tbl1 <- c("education", "race", "income_tier",
                    "age_cat", "gender", "party")

# Overall column
tbl1_overall <- map_dfr(
  demo_vars_tbl1,
  ~ wtd_prop_table(svy132, .x, group_col = NULL)
)

# By Form A / Form B
svy132_form <- df132 |>
  filter(!is.na(weight), weight > 0, !is.na(cncexc), !is.na(form)) |>
  as_survey_design(weights = weight)

tbl1_by_form <- map_dfr(
  demo_vars_tbl1,
  ~ wtd_prop_table(svy132_form, .x, group_col = "form")
)

# Overall N
n_overall <- nrow(df132_analytic)
n_formA   <- sum(df132_analytic$form == "A", na.rm = TRUE)
n_formB   <- sum(df132_analytic$form == "B", na.rm = TRUE)

tbl1_final <- bind_rows(tbl1_overall, tbl1_by_form) |>
  mutate(
    wtd_pct = round(wtd_pct, 1),
    total_n = case_when(
      group == "Overall" ~ n_overall,
      group == "A"       ~ n_formA,
      group == "B"       ~ n_formB,
      TRUE               ~ NA_integer_
    )
  ) |>
  select(variable, category, group, n_unwt, wtd_pct, total_n) |>
  arrange(variable, group, category)

write_csv(tbl1_final, file.path(out_tables, "tbl_01_sample_description.csv"))
cat(sprintf("  Saved: tbl_01_sample_description.csv  (%d rows)\n", nrow(tbl1_final)))

# Print compact summary
cat("\n  Sample N: Overall =", n_overall,
    "| Form A =", n_formA,
    "| Form B =", n_formB, "\n")


# =============================================================================
# TABLE 2: AI ATTITUDE × EDUCATION × RACE (THE AI DIVIDE TABLE)
# =============================================================================

cat("\n--- Table 2: AI Attitude by Education × Race ---\n")

# Weighted proportions of Excited/Concerned/Equally by education, within each race group
# Include cell Ns and Rao-Scott chi-square per racial group

race_levels <- levels(df132_analytic$race)
edu_levels  <- levels(df132_analytic$education)

compute_attitude_by_edu_within_race <- function(svy, race_grp) {

  df_sub <- svy |>
    filter(race == race_grp, !is.na(education), !is.na(cncexc))

  n_sub <- nrow(df_sub$variables)
  if (n_sub < 30) {
    cat(sprintf("  Skipping race group '%s' (n = %d < 30)\n", race_grp, n_sub))
    return(NULL)
  }

  props <- df_sub |>
    group_by(education, cncexc) |>
    summarise(
      prop   = survey_mean(vartype = "ci", level = 0.95),
      n_unwt = unweighted(n()),
      .groups = "drop"
    ) |>
    mutate(race = race_grp)

  # Rao-Scott chi-square: cncexc ~ education within race group
  rs <- tryCatch(
    svychisq(~ cncexc + education, design = df_sub, statistic = "Chisq"),
    error = function(e) NULL
  )
  chi_sq  <- if (!is.null(rs)) round(as.numeric(rs$statistic), 3) else NA_real_
  chi_df  <- if (!is.null(rs)) as.numeric(rs$parameter)          else NA_real_
  chi_p   <- if (!is.null(rs)) round(rs$p.value, 4)              else NA_real_
  chi_sig <- case_when(
    is.na(chi_p)  ~ "",
    chi_p < 0.001 ~ "***",
    chi_p < 0.01  ~ "**",
    chi_p < 0.05  ~ "*",
    chi_p < 0.10  ~ ".",
    TRUE          ~ ""
  )

  props |>
    mutate(
      chi_sq  = chi_sq,
      chi_df  = chi_df,
      chi_p   = chi_p,
      chi_sig = chi_sig
    )
}

tbl2_list <- map(race_levels, function(r) {
  compute_attitude_by_edu_within_race(svy132, r)
})
names(tbl2_list) <- race_levels

tbl2_final <- bind_rows(tbl2_list) |>
  mutate(
    across(c(prop, prop_low, prop_upp), ~ round(. * 100, 1))
  ) |>
  rename(wtd_pct = prop, ci_low = prop_low, ci_upp = prop_upp) |>
  select(race, education, cncexc, wtd_pct, ci_low, ci_upp,
         n_unwt, chi_sq, chi_df, chi_p, chi_sig) |>
  arrange(race, education, cncexc)

write_csv(tbl2_final, file.path(out_tables, "tbl_02_attitude_by_edu_race.csv"))
cat(sprintf("  Saved: tbl_02_attitude_by_edu_race.csv  (%d rows)\n", nrow(tbl2_final)))


# =============================================================================
# TABLE 3: AI AWARENESS DISTRIBUTION BY EDUCATION × INCOME × RACE
# =============================================================================

cat("\n--- Table 3: AI Awareness by SES ---\n")

# "A lot" / "A little" / "Nothing" by education × income, and by education × race
# This documents the first-level AI divide (awareness gap)

# Part A: education × income
tbl3_edu_inc <- svy132 |>
  filter(!is.na(education), !is.na(income_tier), !is.na(ai_heard)) |>
  group_by(education, income_tier, ai_heard) |>
  summarise(
    prop   = survey_mean(vartype = "ci", level = 0.95),
    n_unwt = unweighted(n()),
    .groups = "drop"
  ) |>
  mutate(stratifier = "education x income",
         group1 = as.character(education),
         group2 = as.character(income_tier))

# Part B: education × race
tbl3_edu_race <- svy132 |>
  filter(!is.na(education), !is.na(race), !is.na(ai_heard)) |>
  group_by(education, race, ai_heard) |>
  summarise(
    prop   = survey_mean(vartype = "ci", level = 0.95),
    n_unwt = unweighted(n()),
    .groups = "drop"
  ) |>
  mutate(stratifier = "education x race",
         group1 = as.character(education),
         group2 = as.character(race))

tbl3_final <- bind_rows(
  tbl3_edu_inc  |> select(stratifier, group1, group2, ai_heard, prop, prop_low, prop_upp, n_unwt),
  tbl3_edu_race |> select(stratifier, group1, group2, ai_heard, prop, prop_low, prop_upp, n_unwt)
) |>
  mutate(across(c(prop, prop_low, prop_upp), ~ round(. * 100, 1))) |>
  rename(wtd_pct = prop, ci_low = prop_low, ci_upp = prop_upp)

write_csv(tbl3_final, file.path(out_tables, "tbl_03_awareness_by_ses.csv"))
cat(sprintf("  Saved: tbl_03_awareness_by_ses.csv  (%d rows)\n", nrow(tbl3_final)))


# =============================================================================
# TABLE 4: DOMAIN HELP/HURT BY EDUCATION (+ PARTY COMPARISON COLUMN)
# =============================================================================

cat("\n--- Table 4: Domain Help/Hurt by Education ---\n")

# For each AIHLPHRT domain, weighted % saying "mostly hurt" by education level
# Party column as comparison (to show SES predicts independently of party)
# Form A domains only (items a-d, the primary LCA form)

form_a_domains <- filter(DOMAIN_MAP, form == "A")$var_suffix
form_a_cols    <- paste0("AIHLPHRT_", form_a_domains)
form_a_present <- intersect(form_a_cols, names(df132))

if (length(form_a_present) == 0) {
  cat("  WARNING: No Form A domain columns found. Skipping Table 4.\n")
} else {

  domain_hurt_by_group <- function(svy, domain_col, group_var) {
    sym_grp <- sym(group_var)
    sym_dom <- sym(domain_col)

    svy |>
      filter(!is.na(!!sym_dom), !is.na(!!sym_grp)) |>
      mutate(hurt = as.integer(!!sym_dom == 2)) |>
      group_by(!!sym_grp) |>
      summarise(
        pct_hurt = survey_mean(hurt, vartype = "ci", level = 0.95, na.rm = TRUE),
        n_unwt   = unweighted(n()),
        .groups  = "drop"
      ) |>
      rename(subgroup = !!sym_grp) |>
      mutate(
        group_var  = group_var,
        domain_col = domain_col,
        subgroup   = as.character(subgroup)
      )
  }

  # Survey design using Form A respondents only
  svy132_formA <- df132 |>
    filter(!is.na(weight), weight > 0, !is.na(cncexc), form == "A") |>
    as_survey_design(weights = weight)

  tbl4_parts <- map_dfr(
    form_a_present,
    function(col) {
      edu_part   <- domain_hurt_by_group(svy132_formA, col, "education")
      party_part <- domain_hurt_by_group(svy132_formA, col, "party")
      bind_rows(edu_part, party_part)
    }
  )

  sfx <- str_extract(form_a_present, "[a-h]$")
  domain_labels_a <- DOMAIN_MAP |>
    filter(var_suffix %in% sfx) |>
    select(var_suffix, domain_short, domain_label)

  tbl4_final <- tbl4_parts |>
    mutate(var_suffix = str_extract(domain_col, "[a-h]$")) |>
    left_join(domain_labels_a, by = "var_suffix") |>
    mutate(across(c(pct_hurt, pct_hurt_low, pct_hurt_upp), ~ round(. * 100, 1))) |>
    select(domain_short, domain_label, group_var, subgroup,
           pct_hurt, pct_hurt_low, pct_hurt_upp, n_unwt) |>
    arrange(domain_short, group_var, subgroup)

  write_csv(tbl4_final, file.path(out_tables, "tbl_04_domain_by_education.csv"))
  cat(sprintf("  Saved: tbl_04_domain_by_education.csv  (%d rows)\n", nrow(tbl4_final)))
}


# =============================================================================
# FIGURE 1: THREE-WAVE ATTITUDE TREND
# =============================================================================

cat("\n--- Figure 1: Three-wave attitude trend ---\n")

fig1_data <- svy_trend |>
  group_by(wave_date, cncexc) |>
  summarise(
    prop    = survey_mean(vartype = "ci", level = 0.95),
    n_unwt  = unweighted(n()),
    .groups = "drop"
  ) |>
  mutate(
    pct      = prop * 100,
    pct_low  = prop_low * 100,
    pct_high = prop_upp * 100,
    cncexc   = factor(cncexc, levels = c("Concerned", "Equally", "Excited"))
  )

fig1 <- ggplot(fig1_data,
               aes(x = wave_date, y = pct, color = cncexc, group = cncexc)) +
  geom_ribbon(aes(ymin = pct_low, ymax = pct_high, fill = cncexc),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.0, stroke = 0.5) +
  geom_text(
    aes(label = sprintf("%.0f%%", pct)),
    vjust = -1.1, size = 3.2, fontface = "bold", show.legend = FALSE
  ) +
  scale_color_manual(
    values = ATTITUDE_COLORS,
    name   = "AI attitude",
    guide  = guide_legend(override.aes = list(linewidth = 1.4, size = 2.8))
  ) +
  scale_fill_manual(values = ATTITUDE_COLORS, guide = "none") +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    limits = c(0, 72),
    breaks = seq(0, 70, by = 10)
  ) +
  labs(
    x       = NULL,
    y       = "Weighted percentage (%)",
    caption = CAPTION_TREND
  ) +
  theme_nms()

save_fig(fig1, "fig01_attitude_trend.png", width = 8, height = 5)


# =============================================================================
# FIGURE 2: EDUCATION × ATTITUDE GROUPED BAR, FACETED BY WAVE
# =============================================================================

cat("\n--- Figure 2: Education × Attitude by Wave ---\n")

# Uses the three-wave trend survey design, grouped by education
fig2_data <- svy_trend |>
  filter(!is.na(education)) |>
  group_by(wave_date, education, cncexc) |>
  summarise(
    prop   = survey_mean(vartype = "ci", level = 0.95),
    n_unwt = unweighted(n()),
    .groups = "drop"
  ) |>
  mutate(
    cncexc    = factor(cncexc, levels = c("Concerned", "Equally", "Excited")),
    education = factor(education,
                       levels = c("College graduate+", "Some College",
                                  "HS graduate or less"))
  )

fig2 <- ggplot(fig2_data,
               aes(x = education, y = prop, fill = cncexc)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.72, color = "white", linewidth = 0.2) +
  geom_errorbar(
    aes(ymin = prop_low, ymax = prop_upp),
    position  = position_dodge(width = 0.75),
    width     = 0.18, linewidth = 0.4, color = "grey35"
  ) +
  facet_wrap(~ wave_date, ncol = 3) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 0.75)
  ) +
  scale_fill_manual(
    values = ATTITUDE_COLORS,
    name   = "AI attitude",
    guide  = guide_legend(reverse = FALSE)
  ) +
  labs(
    x       = NULL,
    y       = "Weighted proportion",
    caption = CAPTION_TREND
  ) +
  theme_nms() +
  theme(axis.text.x = element_text(size = rel(0.80)))

save_fig(fig2, "fig02_education_attitude_mosaic.png", width = 9, height = 6)


# =============================================================================
# FIGURE 3: AI AWARENESS BY EDUCATION × INCOME (1st-level AI Divide)
# =============================================================================

cat("\n--- Figure 3: AI Awareness by SES ---\n")

# Heatmap: % "heard a lot about AI" by education (rows) × income (columns)
fig3_data <- svy132 |>
  filter(!is.na(education), !is.na(income_tier), !is.na(ai_heard)) |>
  mutate(heard_lot = as.integer(ai_heard == "A lot")) |>
  group_by(education, income_tier) |>
  summarise(
    pct_heard_lot = survey_mean(heard_lot, vartype = "ci", level = 0.95,
                                na.rm = TRUE) * 100,
    pct_heard_lot_low = pct_heard_lot_low * 100,   # will be computed below
    n_unwt = unweighted(n()),
    .groups = "drop"
  )

# Re-compute cleanly with proper CI extraction
fig3_data <- svy132 |>
  filter(!is.na(education), !is.na(income_tier), !is.na(ai_heard)) |>
  mutate(heard_lot = as.integer(ai_heard == "A lot")) |>
  group_by(education, income_tier) |>
  summarise(
    res    = list(survey_mean(heard_lot, vartype = "ci", level = 0.95, na.rm = TRUE)),
    n_unwt = unweighted(n()),
    .groups = "drop"
  ) |>
  mutate(
    pct      = map_dbl(res, ~ .x[[1]] * 100),
    pct_low  = map_dbl(res, ~ .x[[2]] * 100),
    pct_high = map_dbl(res, ~ .x[[3]] * 100)
  ) |>
  select(-res) |>
  mutate(
    education  = factor(education,
                        levels = c("College graduate+", "Some College",
                                   "HS graduate or less")),
    income_tier = factor(income_tier, levels = c("Lower", "Middle", "Upper"))
  )

# Heatmap with cell labels
fig3 <- ggplot(fig3_data,
               aes(x = income_tier, y = fct_rev(education), fill = pct)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(
    aes(label = sprintf("%.0f%%\n(n=%d)", pct, n_unwt)),
    size = 3.3, color = "grey10", lineheight = 1.1
  ) +
  scale_fill_gradient2(
    low      = "#F7F7F7",
    mid      = "#92C5DE",
    high     = "#2166AC",
    midpoint = 40,
    limits   = c(10, 70),
    name     = "% heard\na lot about AI",
    labels   = label_percent(scale = 1, accuracy = 1)
  ) +
  scale_x_discrete(position = "bottom") +
  labs(
    x       = "Household income tier",
    y       = "Education level",
    caption = CAPTION_STR
  ) +
  theme_nms() +
  theme(
    panel.grid  = element_blank(),
    axis.ticks  = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = rel(0.82))
  )

save_fig(fig3, "fig03_awareness_by_ses.png", width = 8, height = 5)


# =============================================================================
# FIGURE 4: INTERSECTIONAL CONCERN — EDUCATION × RACE/ETHNICITY
# =============================================================================

cat("\n--- Figure 4: Intersectional concern (education × race) ---\n")

# Dot plot with 95% CIs: predicted % "Concerned" by education within each race group
# Faceted by race/ethnicity (only groups with adequate cell N)

fig4_raw <- svy132 |>
  filter(!is.na(education), !is.na(race), !is.na(cncexc)) |>
  mutate(concerned_bin = as.integer(cncexc == "Concerned")) |>
  group_by(race, education) |>
  summarise(
    res    = list(survey_mean(concerned_bin, vartype = "ci", level = 0.95,
                              na.rm = TRUE)),
    n_unwt = unweighted(n()),
    .groups = "drop"
  ) |>
  mutate(
    pct      = map_dbl(res, ~ .x[[1]] * 100),
    pct_low  = map_dbl(res, ~ .x[[2]] * 100),
    pct_high = map_dbl(res, ~ .x[[3]] * 100)
  ) |>
  select(-res)

# Filter to race groups with minimum cell N
CELL_MIN <- 30
fig4_data <- fig4_raw |>
  group_by(race) |>
  filter(all(n_unwt >= CELL_MIN)) |>
  ungroup() |>
  mutate(
    education  = factor(education,
                        levels = rev(c("College graduate+", "Some College",
                                       "HS graduate or less"))),
    race_short = recode(as.character(race), !!!RACE_SHORT),
    race_short = factor(race_short,
                        levels = c("White", "Black", "Hispanic", "Asian", "Other"))
  )

race_n_map <- fig4_data |>
  group_by(race_short) |>
  summarise(total_n = sum(n_unwt)) |>
  mutate(facet_label = sprintf("%s\n(n=%d)", race_short, total_n))

fig4_data <- fig4_data |>
  left_join(select(race_n_map, race_short, facet_label), by = "race_short") |>
  mutate(facet_label = factor(facet_label,
                              levels = race_n_map$facet_label[order(race_n_map$race_short)]))

fig4 <- ggplot(fig4_data,
               aes(x = pct, y = education, color = education)) +
  geom_errorbarh(
    aes(xmin = pct_low, xmax = pct_high),
    height = 0.22, linewidth = 0.7, alpha = 0.7
  ) +
  geom_point(size = 3.0, stroke = 0.5) +
  facet_wrap(~ facet_label, ncol = 3) +
  scale_color_manual(
    values = EDU_COLORS,
    name   = "Education level"
  ) +
  scale_x_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    limits = c(0, 80)
  ) +
  scale_y_discrete(labels = function(x) str_wrap(x, 14)) +
  labs(
    x       = "% expressing concern about AI",
    y       = NULL,
    caption = CAPTION_STR
  ) +
  theme_nms() +
  theme(
    panel.grid.major.y  = element_blank(),
    strip.text          = element_text(size = rel(0.83))
  )

save_fig(fig4, "fig04_intersectional_concern.png", width = 10, height = 6)


# =============================================================================
# FIGURE 5: DOMAIN HELP/HURT PROFILES BY EDUCATION (FORM A ONLY)
# =============================================================================

cat("\n--- Figure 5: Domain help/hurt by education (diverging bars) ---\n")

if (length(form_a_present) == 0) {
  cat("  WARNING: No Form A domain columns found. Skipping Figure 5.\n")
} else {

  # Build weighted % help and % hurt for each Form A domain by education
  domain_by_edu <- map_dfr(
    form_a_present,
    function(col) {
      sfx <- str_extract(col, "[a-h]$")
      svy132_formA |>
        filter(!is.na(!!sym(col)), !is.na(education)) |>
        mutate(
          response = case_when(
            !!sym(col) == 1 ~ "help",
            !!sym(col) == 2 ~ "hurt",
            !!sym(col) == 3 ~ "equal",
            TRUE            ~ NA_character_
          )
        ) |>
        filter(!is.na(response)) |>
        group_by(education, response) |>
        summarise(
          prop   = survey_mean(vartype = "ci", level = 0.95),
          n_unwt = unweighted(n()),
          .groups = "drop"
        ) |>
        mutate(var_suffix = sfx)
    }
  )

  # Compute domain ordering by overall % hurt (descending) for visual clarity
  domain_order_fig5 <- domain_by_edu |>
    filter(response == "hurt") |>
    group_by(var_suffix) |>
    summarise(mean_hurt = mean(prop), .groups = "drop") |>
    arrange(desc(mean_hurt)) |>
    pull(var_suffix)

  # Join domain labels and build signed pct for diverging chart
  fig5_data <- domain_by_edu |>
    filter(response %in% c("help", "hurt")) |>
    left_join(
      select(DOMAIN_MAP, var_suffix, domain_short, domain_label),
      by = "var_suffix"
    ) |>
    mutate(
      pct_signed = case_when(
        response == "hurt" ~ -prop * 100,
        response == "help" ~  prop * 100
      ),
      response_f = factor(response,
                          levels = c("hurt", "help"),
                          labels = c("Mostly hurt", "Mostly help")),
      var_suffix  = factor(var_suffix, levels = rev(domain_order_fig5)),
      domain_short = factor(domain_short,
                            levels = DOMAIN_MAP$domain_short[
                              match(rev(domain_order_fig5), DOMAIN_MAP$var_suffix)
                            ]),
      education   = factor(education,
                           levels = c("College graduate+", "Some College",
                                      "HS graduate or less"))
    )

  fig5 <- ggplot(fig5_data,
                 aes(x = pct_signed, y = domain_short, fill = education,
                     alpha = response_f)) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.72
    ) +
    geom_vline(xintercept = 0, linewidth = 0.5, color = "grey25") +
    facet_wrap(~ education, ncol = 3) +
    scale_x_continuous(
      labels   = function(x) paste0(abs(x), "%"),
      breaks   = seq(-80, 80, by = 20),
      limits   = c(-80, 80)
    ) +
    scale_fill_manual(
      values = EDU_COLORS,
      name   = "Education level",
      guide  = "none"
    ) +
    scale_alpha_manual(
      values = c("Mostly hurt" = 0.92, "Mostly help" = 0.92),
      guide  = "none"
    ) +
    # Custom x-axis annotation
    annotate("text", x = -40, y = 0.4, label = expression(phantom(x) %<-% "Mostly hurt"),
             size = 3.0, color = "grey35", hjust = 0.5) +
    annotate("text", x =  40, y = 0.4, label = expression("Mostly help" %->% phantom(x)),
             size = 3.0, color = "grey35", hjust = 0.5) +
    labs(
      x       = NULL,
      y       = NULL,
      caption = paste0(CAPTION_STR, "\nForm A domains only (items a-d).")
    ) +
    theme_nms() +
    theme(
      axis.text.y        = element_text(size = rel(0.85)),
      panel.grid.major.y = element_blank(),
      axis.text.x        = element_text(size = rel(0.80))
    )

  save_fig(fig5, "fig05_domain_by_education.png", width = 9, height = 7)
}


# =============================================================================
# COMPLETION SUMMARY
# =============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("04_descriptives.R — COMPLETE\n")
cat(strrep("=", 70), "\n\n")

cat("TABLES saved to:", out_tables, "\n")
cat("  tbl_01_sample_description.csv      — Weighted demographics by Form A/B\n")
cat("  tbl_02_attitude_by_edu_race.csv    — AI attitude × education × race\n")
cat("  tbl_03_awareness_by_ses.csv        — AI awareness × education × income/race\n")
cat("  tbl_04_domain_by_education.csv     — Domain hurt % by education + party\n")

cat("\nFIGURES saved to:", out_figures, "\n")
cat("  fig01_attitude_trend.png           — 3-wave trend with CIs (8×5 in)\n")
cat("  fig02_education_attitude_mosaic.png — Edu × attitude by wave (9×6 in)\n")
cat("  fig03_awareness_by_ses.png          — Awareness heatmap edu×income (8×5 in)\n")
cat("  fig04_intersectional_concern.png    — Concern by edu×race dotplot (10×6 in)\n")
cat("  fig05_domain_by_education.png       — Diverging domain bars by edu (9×7 in)\n")

cat("\nNext: run 05_regression.R (logistic / multinomial models)\n")
cat(strrep("=", 70), "\n")
