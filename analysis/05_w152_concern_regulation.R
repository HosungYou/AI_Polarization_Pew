# =============================================================================
# 05_w152_concern_regulation.R
# AI Attitude Polarization Study — Pew ATP Wave 152 (Aug 2024)
#
# Purpose:  Wave-specific analyses for revised paper targeting
#           Technological Forecasting & Social Change.
#           Examines partisan differences in:
#             H6  — Concern content profile (AICONCERN a-g)
#             H7  — The regulation paradox (AIREG × party × concern)
#             H9  — Occupation-specific job threat (AIJOBIMPCT a-j)
#             H6s — Future AI expectations (FUTRAI a-d)
#             Sup — Trust and control (TRSTAIPRS, AICONTROL1/2)
#
# Inputs:
#   data/processed/pew_w152_enriched.rds   [preferred]
#   data/raw/W152_Aug24/ATP W152.sav       [fallback — script builds inline]
#
# Outputs:
#   output/figures/fig3_concern_content_profile.png
#   output/figures/fig4_regulation_paradox.png
#   output/figures/fig6_occupation_job_threat.png
#   output/figures/fig8_future_expectations.png
#   output/tables/tbl_05_concern_profile.csv
#   output/tables/tbl_05_regulation_paradox.csv
#   output/tables/tbl_05_occupation_threat.csv
#   output/tables/tbl_05_trust_control.csv
#
# Author:  [Your Name]
# Date:    2026-02-22
# =============================================================================


# =============================================================================
# 0. SETUP
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("05_w152_concern_regulation.R\n")
cat(strrep("=", 65), "\n\n")

# ---- 0a. Packages ------------------------------------------------------------

required_pkgs <- c(
  "tidyverse", "haven", "survey", "srvyr",
  "marginaleffects", "ggplot2", "patchwork", "scales"
)

invisible(lapply(required_pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message(sprintf("Installing: %s", p))
    install.packages(p, quiet = TRUE)
  }
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

cat("Packages loaded.\n")

# ---- 0b. Paths ---------------------------------------------------------------

project_root <- tryCatch(here::here(), error = function(e) {
  "/Users/hosung/AI_Polarization_Pew"
})

enriched_path <- file.path(project_root,
                           "data/processed/pew_w152_enriched.rds")
raw_sav_path  <- file.path(project_root,
                           "data/raw/W152_Aug24/ATP W152.sav")

out_tables  <- file.path(project_root, "output/tables")
out_figures <- file.path(project_root, "output/figures")

dir.create(out_tables,  showWarnings = FALSE, recursive = TRUE)
dir.create(out_figures, showWarnings = FALSE, recursive = TRUE)

# ---- 0c. Color palette & theme -----------------------------------------------

col_rep <- "#B2182B"   # red  — Republican
col_dem <- "#2166AC"   # blue — Democrat

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

# ---- 0d. Cramér's V helper ---------------------------------------------------

cramers_v <- function(x, y) {
  tbl <- table(x, y)
  chi <- suppressWarnings(chisq.test(tbl))
  n   <- sum(tbl)
  k   <- min(nrow(tbl), ncol(tbl))
  sqrt(chi$statistic / (n * (k - 1)))
}


# =============================================================================
# 1. LOAD / BUILD ANALYTIC DATASET
# =============================================================================

cat("\n--- Loading data ---\n")

build_w152 <- function(sav_path) {
  # Read raw SPSS file and construct analysis-ready data frame
  raw <- haven::read_sav(sav_path)

  # Strip all haven labels up front
  raw <- haven::zap_labels(raw)

  # ---- Party (2-category: Rep/Lean Rep vs Dem/Lean Dem) --------------------
  # F_PARTYSUM_FINAL: 1=Rep/Lean Rep, 2=Dem/Lean Dem, 9=DK/No lean
  party <- dplyr::case_when(
    raw$F_PARTYSUM_FINAL == 1 ~ "Rep/Lean Rep",
    raw$F_PARTYSUM_FINAL == 2 ~ "Dem/Lean Dem",
    TRUE ~ NA_character_
  )

  # ---- Age category --------------------------------------------------------
  # F_AGECAT: 1=18-29, 2=30-49, 3=50-64, 4=65+, 99=Refused
  age_cat <- dplyr::case_when(
    raw$F_AGECAT == 1 ~ "18-29",
    raw$F_AGECAT == 2 ~ "30-49",
    raw$F_AGECAT == 3 ~ "50-64",
    raw$F_AGECAT == 4 ~ "65+",
    TRUE ~ NA_character_
  )

  # ---- Gender --------------------------------------------------------------
  # F_GENDER: 1=Man, 2=Woman, 3=Other, 99=Refused
  gender <- dplyr::case_when(
    raw$F_GENDER == 1 ~ "Male",
    raw$F_GENDER == 2 ~ "Female",
    raw$F_GENDER == 3 ~ "Other",
    TRUE ~ NA_character_
  )

  # ---- Education -----------------------------------------------------------
  # F_EDUCCAT: 1=College+, 2=Some College, 3=HS or less, 99=Refused
  education <- dplyr::case_when(
    raw$F_EDUCCAT == 1 ~ "College graduate+",
    raw$F_EDUCCAT == 2 ~ "Some College",
    raw$F_EDUCCAT == 3 ~ "HS graduate or less",
    TRUE ~ NA_character_
  )

  # ---- Race/ethnicity ------------------------------------------------------
  # F_RACETHNMOD: 1=White NH, 2=Black NH, 3=Hispanic, 4=Other, 5=Asian NH
  race <- dplyr::case_when(
    raw$F_RACETHNMOD == 1 ~ "White non-Hispanic",
    raw$F_RACETHNMOD == 2 ~ "Black non-Hispanic",
    raw$F_RACETHNMOD == 3 ~ "Hispanic",
    raw$F_RACETHNMOD == 4 ~ "Other non-Hispanic",
    raw$F_RACETHNMOD == 5 ~ "Asian non-Hispanic",
    TRUE ~ NA_character_
  )

  # ---- Income tier ---------------------------------------------------------
  # F_INC_TIER2: 1=Lower, 2=Middle, 3=Upper, 99=Refused
  income_tier <- dplyr::case_when(
    raw$F_INC_TIER2 == 1 ~ "Lower",
    raw$F_INC_TIER2 == 2 ~ "Middle",
    raw$F_INC_TIER2 == 3 ~ "Upper",
    TRUE ~ NA_character_
  )

  # ---- AI familiarity ------------------------------------------------------
  # AI_HEARD_W152: 1=A lot, 2=A little, 3=Nothing, 99=DK/Refused
  ai_heard <- dplyr::case_when(
    raw$AI_HEARD_W152 == 1 ~ "A lot",
    raw$AI_HEARD_W152 == 2 ~ "A little",
    raw$AI_HEARD_W152 == 3 ~ "Nothing",
    TRUE ~ NA_character_
  )

  # ---- Survey weight -------------------------------------------------------
  weight <- raw$WEIGHT_W152

  # ---- Form assignment (split-sample) -------------------------------------
  form <- dplyr::case_when(
    raw$FORM_W152 == 1 ~ "Form1",
    raw$FORM_W152 == 2 ~ "Form2",
    TRUE ~ NA_character_
  )

  # ---- AICONCERN (a–g): recode to binary extremely/very (1-2) -------------
  # Raw: 1=Extremely, 2=Very, 3=Somewhat, 4=Not too, 5=Not at all, 99=DK/Ref
  recode_concern_bin <- function(x) {
    ifelse(x %in% c(1, 2), 1L,
           ifelse(x %in% c(3, 4, 5), 0L, NA_integer_))
  }

  concern_items <- paste0("AICONCERN_", letters[1:7], "_W152")
  concern_bin   <- lapply(raw[concern_items], recode_concern_bin)
  names(concern_bin) <- paste0("concern_", letters[1:7])

  # ---- AIREG: regulation preference ----------------------------------------
  # 1=Goes too far, 2=Not far enough, 3=Not sure, 99=Refused
  aireg <- dplyr::case_when(
    raw$AIREG_W152 == 1 ~ "Goes too far",
    raw$AIREG_W152 == 2 ~ "Not far enough",
    raw$AIREG_W152 == 3 ~ "Not sure",
    TRUE ~ NA_character_
  )
  reg_support <- ifelse(raw$AIREG_W152 == 2, 1L,
                        ifelse(raw$AIREG_W152 %in% c(1, 3), 0L, NA_integer_))

  # ---- AIJOBIMPCT (a–j): recode to binary "fewer jobs" (code 2) -----------
  recode_fewer <- function(x) {
    ifelse(x == 2, 1L,
           ifelse(x %in% c(1, 3, 4), 0L, NA_integer_))
  }

  jobimpct_items <- paste0("AIJOBIMPCT_", letters[1:10], "_W152")
  jobimpct_bin   <- lapply(raw[jobimpct_items], recode_fewer)
  names(jobimpct_bin) <- paste0("fewer_", letters[1:10])

  # ---- FUTRAI (a–d): recode to binary extremely/very likely (1-2) ----------
  recode_likely_bin <- function(x) {
    ifelse(x %in% c(1, 2), 1L,
           ifelse(x %in% c(3, 4, 5), 0L, NA_integer_))
  }

  futrai_items <- paste0("FUTRAI_", letters[1:4], "_W152")
  futrai_bin   <- lapply(raw[futrai_items], recode_likely_bin)
  names(futrai_bin) <- paste0("futrai_", letters[1:4])

  # ---- TRSTAIPRS: trust AI eventually ---------------------------------------
  # 1=Yes, 2=No, 3=Not sure, 99=Refused
  trstaiprs <- dplyr::case_when(
    raw$TRSTAIPRS_W152 == 1 ~ "Yes",
    raw$TRSTAIPRS_W152 == 2 ~ "No",
    raw$TRSTAIPRS_W152 == 3 ~ "Not sure",
    TRUE ~ NA_character_
  )

  # ---- AICONTROL1: how much control over AI in life ------------------------
  # 1=Great deal, 2=Quite a bit, 3=Some, 4=Not too much, 5=None, 99=DK
  # Binary: "None" (5)
  ctrl1_none <- ifelse(raw$AICONTROL1_W152 == 5, 1L,
                       ifelse(raw$AICONTROL1_W152 %in% 1:4, 0L, NA_integer_))

  aicontrol1_cat <- dplyr::case_when(
    raw$AICONTROL1_W152 == 1 ~ "A great deal",
    raw$AICONTROL1_W152 == 2 ~ "Quite a bit",
    raw$AICONTROL1_W152 == 3 ~ "Some",
    raw$AICONTROL1_W152 == 4 ~ "Not too much",
    raw$AICONTROL1_W152 == 5 ~ "None",
    TRUE ~ NA_character_
  )

  # ---- AICONTROL2: comfort vs. want more control ---------------------------
  # 1=Comfortable, 2=Want more control, 3=Not sure, 99=Refused
  aicontrol2_cat <- dplyr::case_when(
    raw$AICONTROL2_W152 == 1 ~ "Comfortable",
    raw$AICONTROL2_W152 == 2 ~ "Want more control",
    raw$AICONTROL2_W152 == 3 ~ "Not sure",
    TRUE ~ NA_character_
  )
  ctrl2_want_more <- ifelse(raw$AICONTROL2_W152 == 2, 1L,
                            ifelse(raw$AICONTROL2_W152 %in% c(1, 3), 0L,
                                   NA_integer_))

  # ---- Also keep general AI job concern (AIJOBS) ---------------------------
  # 1=More jobs, 2=Fewer jobs, 3=No much diff, 4=Not sure, 99=Refused
  aijobs_fewer <- ifelse(raw$AIJOBS_W152 == 2, 1L,
                         ifelse(raw$AIJOBS_W152 %in% c(1, 3, 4), 0L, NA_integer_))

  # ---- Aggregate concern index (mean of 7 binary items) --------------------
  concern_mat <- do.call(cbind, concern_bin)
  concern_idx <- rowMeans(concern_mat, na.rm = TRUE)
  concern_idx[rowSums(!is.na(concern_mat)) < 4] <- NA_real_  # need ≥4 valid

  # ---- Assemble data frame -------------------------------------------------
  out <- tibble::tibble(
    party       = party,
    age_cat     = age_cat,
    gender      = gender,
    education   = education,
    race        = race,
    income_tier = income_tier,
    ai_heard    = ai_heard,
    weight      = weight,
    form        = form,
    aireg       = aireg,
    reg_support = reg_support,
    trstaiprs   = trstaiprs,
    aicontrol1  = aicontrol1_cat,
    aicontrol2  = aicontrol2_cat,
    ctrl1_none  = ctrl1_none,
    ctrl2_want_more = ctrl2_want_more,
    aijobs_fewer    = aijobs_fewer,
    concern_idx     = concern_idx
  )

  # Attach binary concern columns
  out <- dplyr::bind_cols(out, tibble::as_tibble(concern_bin))
  out <- dplyr::bind_cols(out, tibble::as_tibble(jobimpct_bin))
  out <- dplyr::bind_cols(out, tibble::as_tibble(futrai_bin))

  out
}

# ---- Load enriched file if it exists; otherwise build from raw --------------

if (file.exists(enriched_path)) {
  cat(sprintf("Loading enriched data: %s\n", enriched_path))
  df_raw <- readRDS(enriched_path)

  # If the enriched file has haven labels, strip them
  df_raw <- df_raw %>%
    mutate(across(where(haven::is.labelled), haven::zap_labels))

  # Check if it already has the analysis-ready columns we need
  needs_rebuild <- !all(c("party", "reg_support", "concern_a",
                          "fewer_a", "futrai_a") %in% names(df_raw))

  if (needs_rebuild) {
    cat("Enriched file lacks needed columns — rebuilding from raw SAV.\n")
    df <- build_w152(raw_sav_path)
  } else {
    df <- df_raw
  }

} else {
  cat(sprintf("Enriched file not found. Building from raw SAV: %s\n",
              raw_sav_path))
  df <- build_w152(raw_sav_path)
}

cat(sprintf("Total observations: %d\n", nrow(df)))

# ---- Restrict to two-party identifiers for partisan analyses ----------------

df_party <- df %>%
  filter(party %in% c("Dem/Lean Dem", "Rep/Lean Rep")) %>%
  mutate(
    party = factor(party,
                   levels = c("Dem/Lean Dem", "Rep/Lean Rep")),
    age_cat     = factor(age_cat,
                         levels = c("18-29", "30-49", "50-64", "65+")),
    gender      = factor(gender,
                         levels = c("Male", "Female", "Other")),
    education   = factor(education,
                         levels = c("College graduate+",
                                    "Some College",
                                    "HS graduate or less")),
    race        = factor(race,
                         levels = c("White non-Hispanic",
                                    "Black non-Hispanic",
                                    "Hispanic",
                                    "Other non-Hispanic",
                                    "Asian non-Hispanic")),
    income_tier = factor(income_tier,
                         levels = c("Lower", "Middle", "Upper")),
    ai_heard    = factor(ai_heard,
                         levels = c("A lot", "A little", "Nothing"))
  )

cat(sprintf("Two-party subsample: %d\n", nrow(df_party)))

# ---- Survey design objects ---------------------------------------------------

svy_full <- svydesign(ids = ~1, weights = ~weight, data = df,
                      na.action = na.omit)

svy_party <- svydesign(ids = ~1, weights = ~weight, data = df_party,
                       na.action = na.omit)

# Form-specific (split-sample)
df_form1  <- df_party %>% filter(form == "Form1")
df_form2  <- df_party %>% filter(form == "Form2")

svy_form1 <- svydesign(ids = ~1, weights = ~weight, data = df_form1,
                       na.action = na.omit)
svy_form2 <- svydesign(ids = ~1, weights = ~weight, data = df_form2,
                       na.action = na.omit)

cat("Survey designs created.\n")


# =============================================================================
# HELPER: survey-weighted proportion with CI
# =============================================================================

svy_prop <- function(svy_design, var, group = NULL) {
  # Returns a tibble with (optionally by-group) proportion=1 for a binary var
  if (is.null(group)) {
    f  <- as.formula(paste0("~", var))
    sv <- svymean(f, design = svy_design, na.rm = TRUE)
    tibble(
      estimate = as.numeric(sv)[1],
      se       = as.numeric(survey::SE(sv))[1],
      lo95     = estimate - 1.96 * se,
      hi95     = estimate + 1.96 * se
    )
  } else {
    f  <- as.formula(paste0("~", var))
    g  <- as.formula(paste0("~", group))
    sv <- svyby(f, by = g, design = svy_design, FUN = svymean, na.rm = TRUE)
    grp_vals <- sv[[group]]
    est      <- sv[[var]]
    se_vals  <- sv[["se"]]
    tibble(
      !!group   := grp_vals,
      estimate  = as.numeric(est),
      se        = as.numeric(se_vals),
      lo95      = estimate - 1.96 * se,
      hi95      = estimate + 1.96 * se
    )
  }
}

sig_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.10  ~ ".",
    TRUE       ~ ""
  )
}


# =============================================================================
# ANALYSIS 1: CONCERN CONTENT PROFILE (H6)
# =============================================================================

cat("\n=== ANALYSIS 1: Concern Content Profile (H6) ===\n")

# ---- Item metadata -----------------------------------------------------------

concern_vars  <- paste0("concern_", letters[1:7])
concern_labels <- c(
  concern_a = "Bias in AI decisions",
  concern_b = "AI impersonating people",
  concern_c = "Personal info misuse",
  concern_d = "Inaccurate information",
  concern_e = "Not understanding AI",
  concern_f = "Reducing human connection",
  concern_g = "Job loss"
)

# ---- Survey-weighted proportions by party -----------------------------------

concern_profile <- purrr::map_dfr(concern_vars, function(vname) {
  res <- svy_prop(svy_party, var = vname, group = "party")
  res$item <- vname
  res
})

# ---- Chi-square tests and Cramér's V ----------------------------------------

concern_tests <- purrr::map_dfr(concern_vars, function(vname) {
  sub <- df_party %>%
    select(all_of(vname), party) %>%
    drop_na()
  chi <- suppressWarnings(chisq.test(table(sub[[vname]], sub$party)))
  v   <- as.numeric(cramers_v(sub[[vname]], sub$party))
  tibble(
    item      = vname,
    chi_sq    = round(chi$statistic, 2),
    df_chi    = chi$parameter,
    p_value   = chi$p.value,
    cramers_v = round(v, 4),
    sig       = sig_stars(chi$p.value)
  )
})

# ---- Compute partisan gap (Rep – Dem) and sort ------------------------------

concern_wide <- concern_profile %>%
  select(party, item, estimate) %>%
  tidyr::pivot_wider(names_from = party, values_from = estimate) %>%
  rename(prop_dem = `Dem/Lean Dem`, prop_rep = `Rep/Lean Rep`) %>%
  mutate(
    gap       = prop_rep - prop_dem,
    item_label = concern_labels[item]
  )

concern_profile <- concern_profile %>%
  left_join(concern_wide %>% select(item, gap, item_label), by = "item") %>%
  mutate(
    item_label = factor(item_label,
                        levels = concern_wide %>%
                          arrange(gap) %>%
                          pull(item_label))
  )

# ---- Figure 3: Diverging / grouped bar chart --------------------------------

# Side-by-side grouped bar (cleaner for publication)
fig3 <- ggplot(
  concern_profile,
  aes(x = estimate, y = item_label, fill = party)
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  geom_errorbar(
    aes(xmin = lo95, xmax = hi95),
    position  = position_dodge(width = 0.7),
    width     = 0.25,
    linewidth = 0.5,
    color     = "grey30",
    orientation = "y"
  ) +
  # Annotate gap for each item
  geom_text(
    data = concern_wide,
    aes(
      x     = pmax(prop_rep, prop_dem) + 0.03,
      y     = item_label,
      label = sprintf("%+.1f%%", gap * 100)
    ),
    inherit.aes = FALSE,
    size = 3.2,
    hjust = 0,
    color = "grey30"
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.95),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(
    values = c("Dem/Lean Dem" = col_dem, "Rep/Lean Rep" = col_rep),
    labels = c("Dem/Lean Dem" = "Democrat/Lean Dem",
               "Rep/Lean Rep" = "Republican/Lean Rep"),
    name   = "Party ID"
  ) +
  labs(
    title    = "AI Concern Profile by Party Identification (Wave 3, Aug 2024)",
    subtitle = "% extremely or very concerned; survey-weighted; 95% CI shown",
    x        = "% Extremely or Very Concerned",
    y        = NULL,
    caption  = paste0(
      "Note: Items sorted by size of partisan gap (Rep \u2212 Dem). ",
      "Gap annotations show pp difference.\n",
      sprintf("N = %d (party identifiers/leaners).", nrow(df_party))
    )
  ) +
  theme_pub() +
  theme(legend.position = "top")

ggsave(
  file.path(out_figures, "fig3_concern_content_profile.png"),
  plot   = fig3,
  width  = 8,
  height = 6,
  dpi    = 300,
  bg     = "white"
)
cat("Saved: fig3_concern_content_profile.png\n")

# ---- Export concern profile table -------------------------------------------

tbl_concern <- concern_profile %>%
  left_join(concern_tests, by = "item") %>%
  mutate(
    pct      = round(estimate * 100, 1),
    pct_lo95 = round(lo95 * 100, 1),
    pct_hi95 = round(hi95 * 100, 1),
    gap_pp   = round(gap * 100, 1)
  ) %>%
  select(item, item_label, party, pct, pct_lo95, pct_hi95, gap_pp,
         chi_sq, df_chi, p_value, cramers_v, sig) %>%
  arrange(item, party)

readr::write_csv(tbl_concern,
                 file.path(out_tables, "tbl_05_concern_profile.csv"))
cat("Saved: tbl_05_concern_profile.csv\n")

# Print summary
cat("\nConcern profile: partisan gaps (Rep - Dem, pp):\n")
concern_wide %>%
  arrange(desc(abs(gap))) %>%
  mutate(gap_pp = round(gap * 100, 1)) %>%
  select(item_label, gap_pp) %>%
  print(n = Inf)


# =============================================================================
# ANALYSIS 2: THE REGULATION PARADOX (H7)
# =============================================================================

cat("\n=== ANALYSIS 2: The Regulation Paradox (H7) ===\n")

# ---- Build a concern indicator for the moderation test ---------------------
# Use the overall concern index (≥ median = "high concern")

df_reg <- df_party %>%
  filter(!is.na(reg_support), !is.na(concern_idx)) %>%
  mutate(
    concern_hi = as.integer(concern_idx >= median(concern_idx, na.rm = TRUE)),
    concern_hi = factor(concern_hi,
                        levels = c(0, 1),
                        labels = c("Lower concern", "Higher concern"))
  )

svy_reg <- svydesign(ids = ~1, weights = ~weight, data = df_reg,
                     na.action = na.omit)

cat(sprintf("Regulation analysis N: %d\n", nrow(df_reg)))

# ---- 2a. Cross-tab: regulation × party × concern ---------------------------

crosstab_reg <- svyby(
  ~reg_support,
  by     = ~party + concern_hi,
  design = svy_reg,
  FUN    = svymean,
  na.rm  = TRUE
)

crosstab_reg_df <- tibble(
  party      = crosstab_reg[["party"]],
  concern_hi = crosstab_reg[["concern_hi"]],
  prop_support = as.numeric(crosstab_reg[["reg_support"]]),
  se_support   = as.numeric(survey::SE(crosstab_reg))
) %>%
  mutate(
    lo95 = prop_support - 1.96 * se_support,
    hi95 = prop_support + 1.96 * se_support
  )

# ---- 2b. Logistic regression: reg_support ~ party * concern_hi + controls --

reg_formula <- reg_support ~
  party * concern_hi +
  age_cat + gender + education + race + income_tier + ai_heard

m_reg <- svyglm(
  reg_formula,
  design = svy_reg,
  family = quasibinomial(link = "logit"),
  na.action = na.omit
)

n_reg   <- nrow(model.frame(m_reg))
coef_m  <- coef(summary(m_reg))
ci_m    <- confint(m_reg)

tbl_reg_coef <- tibble(
  term      = rownames(coef_m),
  estimate  = coef_m[, "Estimate"],
  std_error = coef_m[, "Std. Error"],
  t_value   = coef_m[, "t value"],
  p_value   = coef_m[, "Pr(>|t|)"],
  OR        = exp(estimate),
  OR_lo95   = exp(ci_m[, 1]),
  OR_hi95   = exp(ci_m[, 2]),
  sig       = sig_stars(coef_m[, "Pr(>|t|)"])
)

cat("\nRegulation model — key terms:\n")
tbl_reg_coef %>%
  select(term, OR, OR_lo95, OR_hi95, p_value, sig) %>%
  mutate(across(where(is.double), ~round(., 3))) %>%
  print(n = Inf)

# ---- 2c. Predicted probabilities (party × concern_hi) ----------------------

# Build a profile grid holding demographics at modal values
modal <- list(
  age_cat     = names(sort(table(df_reg$age_cat),    decreasing = TRUE))[1],
  gender      = names(sort(table(df_reg$gender),     decreasing = TRUE))[1],
  education   = names(sort(table(df_reg$education),  decreasing = TRUE))[1],
  race        = names(sort(table(df_reg$race),        decreasing = TRUE))[1],
  income_tier = names(sort(table(df_reg$income_tier),decreasing = TRUE))[1],
  ai_heard    = names(sort(table(df_reg$ai_heard),    decreasing = TRUE))[1],
  weight      = mean(df_reg$weight)
)

grid_reg <- expand.grid(
  party      = levels(df_reg$party),
  concern_hi = levels(df_reg$concern_hi),
  stringsAsFactors = FALSE
) %>%
  tibble::as_tibble() %>%
  mutate(
    age_cat     = modal$age_cat,
    gender      = modal$gender,
    education   = modal$education,
    race        = modal$race,
    income_tier = modal$income_tier,
    ai_heard    = modal$ai_heard,
    weight      = modal$weight,
    party       = factor(party,      levels = levels(df_reg$party)),
    concern_hi  = factor(concern_hi, levels = levels(df_reg$concern_hi)),
    age_cat     = factor(age_cat,    levels = levels(df_reg$age_cat)),
    gender      = factor(gender,     levels = levels(df_reg$gender)),
    education   = factor(education,  levels = levels(df_reg$education)),
    race        = factor(race,       levels = levels(df_reg$race)),
    income_tier = factor(income_tier,levels = levels(df_reg$income_tier)),
    ai_heard    = factor(ai_heard,   levels = levels(df_reg$ai_heard))
  )

pred_reg  <- predict(m_reg, newdata = grid_reg, type = "link", se = TRUE)
grid_reg  <- grid_reg %>%
  mutate(
    fit    = as.numeric(pred_reg),
    se_fit = as.numeric(SE(pred_reg)),
    prob   = plogis(fit),
    lo95   = plogis(fit - 1.96 * se_fit),
    hi95   = plogis(fit + 1.96 * se_fit)
  )

# ---- 2d. Figure 4: Two-panel ------------------------------------------------

# Panel A: Observed proportions — party × concern × reg support (grouped bar)
panel_A <- ggplot(
  crosstab_reg_df,
  aes(x = concern_hi, y = prop_support, fill = party)
) +
  geom_col(
    position = position_dodge(width = 0.65),
    width = 0.55
  ) +
  geom_errorbar(
    aes(ymin = lo95, ymax = hi95),
    position = position_dodge(width = 0.65),
    width = 0.25,
    linewidth = 0.5,
    color = "grey30"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_fill_manual(
    values = c("Dem/Lean Dem" = col_dem, "Rep/Lean Rep" = col_rep),
    labels = c("Dem/Lean Dem" = "Dem/Lean Dem",
               "Rep/Lean Rep" = "Rep/Lean Rep"),
    name   = "Party ID"
  ) +
  labs(
    title    = "A. Observed Support for\nMore Regulation",
    subtitle = "Survey-weighted; 95% CI",
    x        = "AI Concern Level",
    y        = "P(Regulation Not Far Enough)"
  ) +
  theme_pub() +
  theme(legend.position = "top")

# Panel B: Predicted probabilities from interaction model
panel_B <- ggplot(
  grid_reg,
  aes(x = concern_hi, y = prob,
      color = party, group = party,
      ymin = lo95, ymax = hi95)
) +
  geom_line(
    linewidth = 1.1,
    position  = position_dodge(width = 0.25)
  ) +
  geom_point(
    size     = 3.5,
    position = position_dodge(width = 0.25)
  ) +
  geom_errorbar(
    width     = 0.15,
    linewidth = 0.6,
    position  = position_dodge(width = 0.25)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_color_manual(
    values = c("Dem/Lean Dem" = col_dem, "Rep/Lean Rep" = col_rep),
    labels = c("Dem/Lean Dem" = "Dem/Lean Dem",
               "Rep/Lean Rep" = "Rep/Lean Rep"),
    name   = "Party ID"
  ) +
  labs(
    title    = "B. Predicted Probability of\nSupporting More Regulation",
    subtitle = "Logistic regression; covariates at modal; 95% CI",
    x        = "AI Concern Level",
    y        = "Predicted P(Not Far Enough)"
  ) +
  theme_pub() +
  theme(legend.position = "top")

fig4 <- panel_A + panel_B +
  plot_annotation(
    title   = "The Regulation Paradox: Party × Concern Interaction (Wave 3, Aug 2024)",
    caption = paste0(
      "Note: 'Concern level' split at median of 7-item concern index.\n",
      sprintf("N = %d (complete cases, party identifiers).", n_reg)
    )
  )

ggsave(
  file.path(out_figures, "fig4_regulation_paradox.png"),
  plot   = fig4,
  width  = 10,
  height = 5,
  dpi    = 300,
  bg     = "white"
)
cat("Saved: fig4_regulation_paradox.png\n")

# ---- Export regulation table ------------------------------------------------
# Sheet 1: logistic regression coefficients
# Sheet 2: observed cross-tabulation (party × concern × reg support)

tbl_reg_coef_out <- tbl_reg_coef %>%
  mutate(
    OR_fmt  = sprintf("%.3f", OR),
    CI_fmt  = sprintf("[%.3f, %.3f]", OR_lo95, OR_hi95),
    p_fmt   = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  )

tbl_reg_crosstab_out <- crosstab_reg_df %>%
  mutate(
    pct      = round(prop_support * 100, 1),
    pct_lo95 = round(lo95 * 100, 1),
    pct_hi95 = round(hi95 * 100, 1)
  ) %>%
  select(party, concern_hi, pct, pct_lo95, pct_hi95, prop_support,
         se_support)

# Write both sections, separated by a blank-row marker, into one CSV
tbl_reg_combined <- bind_rows(
  tbl_reg_coef_out %>%
    mutate(section = "logistic_model") %>%
    select(section, term, estimate, std_error, OR, OR_lo95, OR_hi95,
           OR_fmt, CI_fmt, t_value, p_value, p_fmt, sig),
  tibble(section = "--- observed_crosstab ---",
         term = NA_character_, estimate = NA_real_, std_error = NA_real_,
         OR = NA_real_, OR_lo95 = NA_real_, OR_hi95 = NA_real_,
         OR_fmt = NA_character_, CI_fmt = NA_character_,
         t_value = NA_real_, p_value = NA_real_, p_fmt = NA_character_,
         sig = NA_character_),
  tbl_reg_crosstab_out %>%
    mutate(
      section   = "observed_crosstab",
      term      = paste(party, concern_hi, sep = " | "),
      estimate  = prop_support,
      std_error = se_support,
      OR        = NA_real_,
      OR_lo95   = NA_real_,
      OR_hi95   = NA_real_,
      OR_fmt    = NA_character_,
      CI_fmt    = sprintf("[%.1f%%, %.1f%%]", pct_lo95, pct_hi95),
      t_value   = NA_real_,
      p_value   = NA_real_,
      p_fmt     = NA_character_,
      sig       = NA_character_
    ) %>%
    select(section, term, estimate, std_error, OR, OR_lo95, OR_hi95,
           OR_fmt, CI_fmt, t_value, p_value, p_fmt, sig)
)

readr::write_csv(tbl_reg_combined,
                 file.path(out_tables, "tbl_05_regulation_paradox.csv"))
cat("Saved: tbl_05_regulation_paradox.csv\n")


# =============================================================================
# ANALYSIS 3: OCCUPATION-SPECIFIC JOB THREAT (H9)
# =============================================================================

cat("\n=== ANALYSIS 3: Occupation-Specific Job Threat (H9) ===\n")

# ---- Occupation metadata ----------------------------------------------------

occ_vars   <- paste0("fewer_", letters[1:10])
occ_labels <- c(
  fewer_a = "Lawyers",
  fewer_b = "Software engineers",
  fewer_c = "Cashiers",
  fewer_d = "Factory workers",
  fewer_e = "Medical doctors",
  fewer_f = "Teachers",
  fewer_g = "Journalists",
  fewer_h = "Truck drivers",
  fewer_i = "Musicians",
  fewer_j = "Mental health therapists"
)

# Form assignment: a-e on Form 1, f-j on Form 2
occ_form <- c(
  fewer_a = "Form1", fewer_b = "Form1", fewer_c = "Form1",
  fewer_d = "Form1", fewer_e = "Form1",
  fewer_f = "Form2", fewer_g = "Form2", fewer_h = "Form2",
  fewer_i = "Form2", fewer_j = "Form2"
)

# ---- Survey-weighted % "fewer jobs" by party, respecting form ---------------

occ_results <- purrr::map_dfr(occ_vars, function(vname) {
  form_id  <- occ_form[vname]
  svy_use  <- if (form_id == "Form1") svy_form1 else svy_form2

  res <- svy_prop(svy_use, var = vname, group = "party")
  res$item  <- vname
  res$form  <- form_id
  res
})

# ---- Compute partisan gap ---------------------------------------------------

occ_wide <- occ_results %>%
  select(party, item, estimate) %>%
  tidyr::pivot_wider(names_from = party, values_from = estimate) %>%
  rename(prop_dem = `Dem/Lean Dem`, prop_rep = `Rep/Lean Rep`) %>%
  mutate(
    gap       = prop_rep - prop_dem,
    occ_label = occ_labels[item]
  )

occ_results <- occ_results %>%
  left_join(occ_wide %>% select(item, gap, occ_label), by = "item") %>%
  mutate(
    occ_label = factor(occ_label,
                       levels = occ_wide %>%
                         arrange(gap) %>%
                         pull(occ_label))
  )

# ---- Chi-square tests by form -----------------------------------------------

occ_tests <- purrr::map_dfr(occ_vars, function(vname) {
  form_id <- occ_form[vname]
  sub <- if (form_id == "Form1") df_form1 else df_form2
  sub <- sub %>% select(all_of(vname), party) %>% drop_na()
  chi <- suppressWarnings(chisq.test(table(sub[[vname]], sub$party)))
  v   <- as.numeric(cramers_v(sub[[vname]], sub$party))
  tibble(
    item      = vname,
    occ_label = occ_labels[vname],
    form      = form_id,
    n         = nrow(sub),
    chi_sq    = round(chi$statistic, 2),
    df_chi    = chi$parameter,
    p_value   = chi$p.value,
    cramers_v = round(v, 4),
    sig       = sig_stars(chi$p.value)
  )
})

cat("\nOccupation job-threat tests:\n")
occ_tests %>%
  arrange(desc(abs(p_value))) %>%
  print(n = Inf)

# ---- Figure 6: Dumbbell chart -----------------------------------------------

# Separate Rep/Dem for dumbbell
occ_dem <- occ_results %>% filter(party == "Dem/Lean Dem")
occ_rep <- occ_results %>% filter(party == "Rep/Lean Rep")
occ_seg <- occ_dem %>%
  select(occ_label, dem_est = estimate) %>%
  left_join(occ_rep %>% select(occ_label, rep_est = estimate), by = "occ_label")

fig6 <- ggplot() +
  # Connecting segment
  geom_segment(
    data = occ_seg,
    aes(x = dem_est, xend = rep_est,
        y = occ_label, yend = occ_label),
    color = "grey60",
    linewidth = 0.8
  ) +
  # Democrat dots
  geom_point(
    data = occ_dem,
    aes(x = estimate, y = occ_label),
    color = col_dem, size = 4
  ) +
  # Republican dots
  geom_point(
    data = occ_rep,
    aes(x = estimate, y = occ_label),
    color = col_rep, size = 4
  ) +
  # Error bars — Democrat
  geom_errorbar(
    data = occ_dem,
    aes(xmin = lo95, xmax = hi95, y = occ_label),
    width = 0.25, color = col_dem, linewidth = 0.5,
    orientation = "y"
  ) +
  # Error bars — Republican
  geom_errorbar(
    data = occ_rep,
    aes(xmin = lo95, xmax = hi95, y = occ_label),
    width = 0.25, color = col_rep, linewidth = 0.5,
    orientation = "y"
  ) +
  # Gap annotation
  geom_text(
    data = occ_seg %>%
      left_join(occ_wide %>% select(occ_label, gap), by = "occ_label"),
    aes(
      x     = pmax(dem_est, rep_est) + 0.03,
      y     = occ_label,
      label = sprintf("%+.1f%%", gap * 100)
    ),
    size  = 3.2,
    hjust = 0,
    color = "grey30"
  ) +
  # Manual legend
  annotate("point", x = 0.07, y = 1.5, color = col_rep, size = 4) +
  annotate("text",  x = 0.09, y = 1.5, label = "Rep/Lean Rep",
           size = 3.5, hjust = 0) +
  annotate("point", x = 0.07, y = 1.0, color = col_dem, size = 4) +
  annotate("text",  x = 0.09, y = 1.0, label = "Dem/Lean Dem",
           size = 3.5, hjust = 0) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Occupation-Specific AI Job Displacement Concern by Party (Wave 3, Aug 2024)",
    subtitle = "% saying AI will lead to fewer jobs; split-sample design; survey-weighted; 95% CI",
    x        = "% Saying AI Will Lead to Fewer Jobs",
    y        = NULL,
    caption  = paste0(
      "Note: Items a\u2013e on Form 1 (N\u2248", nrow(df_form1),
      "); items f\u2013j on Form 2 (N\u2248", nrow(df_form2), ").\n",
      "Occupations sorted by Rep \u2212 Dem gap; annotations show pp difference."
    )
  ) +
  theme_pub() +
  theme(panel.grid.major.x = element_line(color = "grey92"))

ggsave(
  file.path(out_figures, "fig6_occupation_job_threat.png"),
  plot   = fig6,
  width  = 8,
  height = 7,
  dpi    = 300,
  bg     = "white"
)
cat("Saved: fig6_occupation_job_threat.png\n")

# ---- Export occupation table ------------------------------------------------

tbl_occ <- occ_results %>%
  mutate(
    pct      = round(estimate * 100, 1),
    pct_lo95 = round(lo95 * 100, 1),
    pct_hi95 = round(hi95 * 100, 1),
    gap_pp   = round(gap * 100, 1)
  ) %>%
  left_join(occ_tests %>% select(item, n, chi_sq, p_value, cramers_v, sig),
            by = "item") %>%
  select(item, occ_label, form, party, n, pct, pct_lo95, pct_hi95, gap_pp,
         chi_sq, p_value, cramers_v, sig) %>%
  arrange(item, party)

readr::write_csv(tbl_occ,
                 file.path(out_tables, "tbl_05_occupation_threat.csv"))
cat("Saved: tbl_05_occupation_threat.csv\n")


# =============================================================================
# ANALYSIS 4: FUTURE AI EXPECTATIONS (H6 supplement)
# =============================================================================

cat("\n=== ANALYSIS 4: Future AI Expectations (H6 supplement) ===\n")

# ---- Item metadata ----------------------------------------------------------

futrai_vars   <- paste0("futrai_", letters[1:4])
futrai_labels <- c(
  futrai_a = "AI will think on its own",
  futrai_b = "AI will cause major harm",
  futrai_c = "AI will make humans more productive",
  futrai_d = "AI will make humans happier"
)

# ---- Survey-weighted proportions by party -----------------------------------

futrai_profile <- purrr::map_dfr(futrai_vars, function(vname) {
  res <- svy_prop(svy_party, var = vname, group = "party")
  res$item <- vname
  res
})

# ---- Partisan gap -----------------------------------------------------------

futrai_wide <- futrai_profile %>%
  select(party, item, estimate) %>%
  tidyr::pivot_wider(names_from = party, values_from = estimate) %>%
  rename(prop_dem = `Dem/Lean Dem`, prop_rep = `Rep/Lean Rep`) %>%
  mutate(
    gap        = prop_rep - prop_dem,
    item_label = futrai_labels[item]
  )

futrai_profile <- futrai_profile %>%
  left_join(futrai_wide %>% select(item, gap, item_label), by = "item") %>%
  mutate(
    item_label = factor(item_label,
                        levels = futrai_wide %>%
                          arrange(gap) %>%
                          pull(item_label))
  )

# ---- Chi-square tests -------------------------------------------------------

futrai_tests <- purrr::map_dfr(futrai_vars, function(vname) {
  sub <- df_party %>%
    select(all_of(vname), party) %>%
    drop_na()
  chi <- suppressWarnings(chisq.test(table(sub[[vname]], sub$party)))
  v   <- as.numeric(cramers_v(sub[[vname]], sub$party))
  tibble(
    item      = vname,
    chi_sq    = round(chi$statistic, 2),
    df_chi    = chi$parameter,
    p_value   = chi$p.value,
    cramers_v = round(v, 4),
    sig       = sig_stars(chi$p.value)
  )
})

# ---- Figure 8: Diverging bar chart ------------------------------------------

fig8 <- ggplot(
  futrai_profile,
  aes(x = estimate, y = item_label, fill = party)
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width     = 0.6
  ) +
  geom_errorbar(
    aes(xmin = lo95, xmax = hi95),
    position    = position_dodge(width = 0.7),
    width       = 0.25,
    linewidth   = 0.5,
    color       = "grey30",
    orientation = "y"
  ) +
  geom_text(
    data = futrai_wide,
    aes(
      x     = pmax(prop_rep, prop_dem) + 0.03,
      y     = item_label,
      label = sprintf("%+.1f%%", gap * 100)
    ),
    inherit.aes = FALSE,
    size  = 3.2,
    hjust = 0,
    color = "grey30"
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.95),
    expand = expansion(mult = c(0, 0.08))
  ) +
  scale_fill_manual(
    values = c("Dem/Lean Dem" = col_dem, "Rep/Lean Rep" = col_rep),
    labels = c("Dem/Lean Dem" = "Democrat/Lean Dem",
               "Rep/Lean Rep" = "Republican/Lean Rep"),
    name   = "Party ID"
  ) +
  labs(
    title    = "Future AI Expectations by Party Identification (Wave 3, Aug 2024)",
    subtitle = "% extremely or very likely to occur in next 20 years; survey-weighted; 95% CI",
    x        = "% Extremely or Very Likely",
    y        = NULL,
    caption  = paste0(
      "Note: Items sorted by partisan gap (Rep \u2212 Dem). ",
      "Gap annotations show pp difference.\n",
      sprintf("N = %d (party identifiers/leaners).", nrow(df_party))
    )
  ) +
  theme_pub() +
  theme(legend.position = "top")

ggsave(
  file.path(out_figures, "fig8_future_expectations.png"),
  plot   = fig8,
  width  = 7,
  height = 4,
  dpi    = 300,
  bg     = "white"
)
cat("Saved: fig8_future_expectations.png\n")

cat("\nFuture AI expectations: partisan gaps (Rep - Dem, pp):\n")
futrai_wide %>%
  arrange(desc(abs(gap))) %>%
  mutate(gap_pp = round(gap * 100, 1)) %>%
  left_join(futrai_tests %>% select(item, p_value, sig), by = "item") %>%
  select(item_label, gap_pp, p_value, sig) %>%
  print(n = Inf)


# =============================================================================
# ANALYSIS 5: TRUST AND CONTROL (Supplementary)
# =============================================================================

cat("\n=== ANALYSIS 5: Trust and Control (Supplementary) ===\n")

# ---- 5a. TRSTAIPRS: % trusting AI eventually --------------------------------
# Use pre-built binary: trust_yes = 1 if trstaiprs == "Yes"

df_party_trust <- df_party %>%
  mutate(trust_yes = as.integer(trstaiprs == "Yes" & !is.na(trstaiprs)))

svy_party_trust <- svydesign(ids = ~1, weights = ~weight,
                              data = df_party_trust, na.action = na.omit)

trust_svyby <- svyby(~trust_yes, by = ~party, design = svy_party_trust,
                     FUN = svymean, na.rm = TRUE)

trust_by_party <- tibble(
  party          = trust_svyby[["party"]],
  prop_yes_trust = as.numeric(trust_svyby[["trust_yes"]]),
  se_trust       = as.numeric(survey::SE(trust_svyby))
) %>%
  mutate(
    lo95 = prop_yes_trust - 1.96 * se_trust,
    hi95 = prop_yes_trust + 1.96 * se_trust,
    pct  = round(prop_yes_trust * 100, 1)
  )

chi_trust <- suppressWarnings(
  chisq.test(
    table(
      df_party %>% filter(!is.na(trstaiprs)) %>% pull(trstaiprs),
      df_party %>% filter(!is.na(trstaiprs)) %>% pull(party)
    )
  )
)

cat("\nTrust AI eventually (% Yes) by party:\n")
print(trust_by_party %>% select(party, pct, lo95, hi95))
cat(sprintf("Chi-sq = %.2f, p = %.4f, V = %.4f\n",
            chi_trust$statistic,
            chi_trust$p.value,
            cramers_v(
              df_party %>% filter(!is.na(trstaiprs)) %>% pull(trstaiprs),
              df_party %>% filter(!is.na(trstaiprs)) %>% pull(party)
            )))

# ---- 5b. AICONTROL1: % saying "None" ----------------------------------------

ctrl1_svyby <- svyby(~ctrl1_none, by = ~party, design = svy_party,
                     FUN = svymean, na.rm = TRUE)

ctrl1_by_party <- tibble(
  party     = ctrl1_svyby[["party"]],
  prop_none = as.numeric(ctrl1_svyby[["ctrl1_none"]]),
  se_ctrl1  = as.numeric(survey::SE(ctrl1_svyby))
) %>%
  mutate(
    lo95 = prop_none - 1.96 * se_ctrl1,
    hi95 = prop_none + 1.96 * se_ctrl1,
    pct  = round(prop_none * 100, 1)
  )

chi_ctrl1 <- suppressWarnings(
  chisq.test(
    table(
      df_party %>% filter(!is.na(aicontrol1)) %>% pull(aicontrol1),
      df_party %>% filter(!is.na(aicontrol1)) %>% pull(party)
    )
  )
)

cat("\nAI control (% 'None') by party:\n")
print(ctrl1_by_party %>% select(party, pct, lo95, hi95))
cat(sprintf("Chi-sq = %.2f, p = %.4f, V = %.4f\n",
            chi_ctrl1$statistic,
            chi_ctrl1$p.value,
            cramers_v(
              df_party %>% filter(!is.na(aicontrol1)) %>% pull(aicontrol1),
              df_party %>% filter(!is.na(aicontrol1)) %>% pull(party)
            )))

# ---- 5c. AICONTROL2: % wanting more control ---------------------------------

ctrl2_svyby <- svyby(~ctrl2_want_more, by = ~party, design = svy_party,
                     FUN = svymean, na.rm = TRUE)

ctrl2_by_party <- tibble(
  party          = ctrl2_svyby[["party"]],
  prop_want_more = as.numeric(ctrl2_svyby[["ctrl2_want_more"]]),
  se_ctrl2       = as.numeric(survey::SE(ctrl2_svyby))
) %>%
  mutate(
    lo95 = prop_want_more - 1.96 * se_ctrl2,
    hi95 = prop_want_more + 1.96 * se_ctrl2,
    pct  = round(prop_want_more * 100, 1)
  )

chi_ctrl2 <- suppressWarnings(
  chisq.test(
    table(
      df_party %>% filter(!is.na(aicontrol2)) %>% pull(aicontrol2),
      df_party %>% filter(!is.na(aicontrol2)) %>% pull(party)
    )
  )
)

cat("\nAI control (% 'Want more control') by party:\n")
print(ctrl2_by_party %>% select(party, pct, lo95, hi95))
cat(sprintf("Chi-sq = %.2f, p = %.4f, V = %.4f\n",
            chi_ctrl2$statistic,
            chi_ctrl2$p.value,
            cramers_v(
              df_party %>% filter(!is.na(aicontrol2)) %>% pull(aicontrol2),
              df_party %>% filter(!is.na(aicontrol2)) %>% pull(party)
            )))

# ---- 5d. Full cross-tab for supplementary tables ----------------------------

make_full_crosstab <- function(var_cat, party_vec, label) {
  sub <- data.frame(var = var_cat, party = party_vec) %>%
    filter(!is.na(var), !is.na(party))
  tbl_n   <- table(sub$var, sub$party)
  tbl_pct <- prop.table(tbl_n, margin = 2) * 100
  chi     <- suppressWarnings(chisq.test(tbl_n))
  v       <- cramers_v(sub$var, sub$party)

  out <- as_tibble(as.data.frame.table(tbl_pct)) %>%
    rename(response = Var1, party = Var2, pct = Freq) %>%
    mutate(
      label     = label,
      pct       = round(pct, 1),
      chi_sq    = round(chi$statistic, 2),
      p_value   = chi$p.value,
      cramers_v = round(v, 4),
      sig       = sig_stars(chi$p.value)
    )
  out
}

tbl_trust_ctrl <- bind_rows(
  make_full_crosstab(df_party$trstaiprs, df_party$party,
                     "Trust AI eventually (TRSTAIPRS)"),
  make_full_crosstab(df_party$aicontrol1, df_party$party,
                     "Control over AI in life (AICONTROL1)"),
  make_full_crosstab(df_party$aicontrol2, df_party$party,
                     "Comfortable vs. want more control (AICONTROL2)")
)

readr::write_csv(tbl_trust_ctrl,
                 file.path(out_tables, "tbl_05_trust_control.csv"))
cat("Saved: tbl_05_trust_control.csv\n")


# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 65), "\n\n")
cat(sprintf("Wave 3 (Aug 2024) N total:          %d\n", nrow(df)))
cat(sprintf("Party identifier/leaner subsample:  %d\n", nrow(df_party)))
cat(sprintf("  Rep/Lean Rep: %d\n",
            sum(df_party$party == "Rep/Lean Rep")))
cat(sprintf("  Dem/Lean Dem: %d\n",
            sum(df_party$party == "Dem/Lean Dem")))
cat(sprintf("Form 1 (occupations a-e): %d\n", nrow(df_form1)))
cat(sprintf("Form 2 (occupations f-j): %d\n", nrow(df_form2)))

cat("\nFigures saved:\n")
cat("  output/figures/fig3_concern_content_profile.png   (8 x 6 in, 300 dpi)\n")
cat("  output/figures/fig4_regulation_paradox.png        (10 x 5 in, 300 dpi)\n")
cat("  output/figures/fig6_occupation_job_threat.png     (8 x 7 in, 300 dpi)\n")
cat("  output/figures/fig8_future_expectations.png       (7 x 4 in, 300 dpi)\n")

cat("\nTables saved:\n")
cat("  output/tables/tbl_05_concern_profile.csv\n")
cat("  output/tables/tbl_05_regulation_paradox.csv\n")
cat("  output/tables/tbl_05_occupation_threat.csv\n")
cat("  output/tables/tbl_05_trust_control.csv\n")

cat("\nKey findings summary:\n")

cat("\n  CONCERN PROFILE (H6) — Top 3 partisan gaps:\n")
concern_wide %>%
  arrange(desc(gap)) %>%
  slice_head(n = 3) %>%
  mutate(gap_pp = round(gap * 100, 1)) %>%
  select(item_label, gap_pp) %>%
  print()

cat("\n  FUTURE EXPECTATIONS — Top partisan gaps:\n")
futrai_wide %>%
  arrange(desc(abs(gap))) %>%
  mutate(gap_pp = round(gap * 100, 1)) %>%
  select(item_label, gap_pp) %>%
  print()

cat("\n  OCCUPATION JOB THREAT (H9) — Top 3 partisan gaps:\n")
occ_wide %>%
  arrange(desc(abs(gap))) %>%
  slice_head(n = 3) %>%
  mutate(gap_pp = round(gap * 100, 1)) %>%
  select(occ_label, gap_pp) %>%
  print()
