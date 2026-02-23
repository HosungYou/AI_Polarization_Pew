# =============================================================================
# 06_w119_knowledge.R
# AI Attitude Polarization Study — Pew ATP W119 (Dec 2022)
#
# Purpose:  Analyze AI knowledge and technology disposition as predictors of
#           partisan AI concern. Tests the "information deficit" model (H8):
#           do higher knowledge levels reduce concern, and does this effect
#           differ by party?
#
# Theoretical context:
#   Democrats score higher on AI knowledge AND report greater dispositional
#   comfort with new technology, yet were initially LESS concerned about AI
#   than Republicans (Dec 2022). This contradicts the information deficit
#   model (more knowledge → less concern). By W152 (2024), Democrats caught
#   up in concern, suggesting knowledge did not inoculate against it.
#
# Reference: Kahan et al. (2012) — "polarization amplification" alternative
#
# Input:
#   data/processed/pew_w119_enriched.rds  (preferred, if available)
#   data/raw/W119_Dec22/ATP W119.sav       (fallback)
#
# Outputs:
#   output/figures/fig5_knowledge_distribution.png
#   output/figures/fig_06_knowledge_interaction.png
#   output/figures/fig_06_knowledge_items.png
#   output/tables/tbl_06_knowledge_by_party.csv
#   output/tables/tbl_06_knowledge_interaction.csv
#   output/tables/tbl_06_desrisk_models.csv
#
# Author:  [Your Name]
# Date:    2026-02-22
# =============================================================================


# =============================================================================
# 0. SETUP
# =============================================================================

# ---- 0a. Install / load packages --------------------------------------------

required_pkgs <- c(
  "tidyverse", "haven", "survey", "srvyr",
  "marginaleffects", "ggplot2", "scales", "broom"
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

enriched_path <- file.path(project_root,
                           "data/processed/pew_w119_enriched.rds")
raw_path      <- file.path(project_root,
                           "data/raw/W119_Dec22/ATP W119.sav")
out_tables    <- file.path(project_root, "output/tables")
out_figures   <- file.path(project_root, "output/figures")

dir.create(out_tables,  showWarnings = FALSE, recursive = TRUE)
dir.create(out_figures, showWarnings = FALSE, recursive = TRUE)

# ---- 0c. Color palette -------------------------------------------------------

col_rep  <- "#B2182B"   # red  — Republican
col_dem  <- "#2166AC"   # blue — Democrat
col_gray <- "#999999"   # gray — neutral

# ---- 0d. Publication ggplot2 theme ------------------------------------------

theme_pub <- function(...) {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(size = 11, color = "grey40"),
      plot.caption     = element_text(size = 9,  color = "grey55",
                                      hjust = 0, margin = margin(t = 8)),
      axis.title       = element_text(size = 11),
      axis.text        = element_text(size = 10),
      legend.title     = element_text(size = 11),
      legend.text      = element_text(size = 10),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      plot.margin      = margin(12, 14, 8, 12),
      ...
    )
}

# ---- 0e. Save-figure helper --------------------------------------------------

save_fig <- function(plot, filename, width = 7, height = 5, dpi = 300) {
  path <- file.path(out_figures, filename)
  ggsave(filename = path, plot = plot,
         width = width, height = height,
         dpi = dpi, bg = "white")
  cat(sprintf("  Saved figure: %s\n", path))
  invisible(path)
}

# ---- 0f. Significance stars --------------------------------------------------

sig_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.10  ~ ".",
    TRUE       ~ ""
  )
}

# ---- 0g. Tidy svyglm helper --------------------------------------------------

tidy_svyglm <- function(model, model_label = NULL) {
  coef_mat <- coef(summary(model))
  ci       <- confint(model)

  tbl <- tibble::tibble(
    term      = rownames(coef_mat),
    estimate  = coef_mat[, "Estimate"],
    std_error = coef_mat[, "Std. Error"],
    t_value   = coef_mat[, "t value"],
    p_value   = coef_mat[, "Pr(>|t|)"],
    OR        = exp(estimate),
    OR_lo95   = exp(ci[, 1]),
    OR_hi95   = exp(ci[, 2]),
    sig       = sig_stars(p_value),
    OR_fmt    = sprintf("%.3f", OR),
    CI_fmt    = sprintf("[%.3f, %.3f]", OR_lo95, OR_hi95),
    p_fmt     = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  )

  if (!is.null(model_label)) tbl <- dplyr::mutate(tbl, model = model_label)
  tbl
}


# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

cat("\n--- 1. Loading data ---\n")

# ---- 1a. Load: enriched RDS first, fall back to raw SPSS --------------------

ENRICHED_CORE_COLS <- c("ai_knowledge", "party", "concerned", "weight")

if (file.exists(enriched_path)) {

  cat(sprintf("Loading enriched RDS: %s\n", enriched_path))
  df_raw <- readRDS(enriched_path)

  has_enriched_cols <- all(ENRICHED_CORE_COLS %in% names(df_raw))

  if (has_enriched_cols) {
    cat("Enriched data detected — using pre-cleaned columns.\n")
    # Strip any residual haven labels
    df_raw <- df_raw %>% haven::zap_labels()
    use_enriched <- TRUE
  } else {
    cat("Enriched file present but lacks expected columns — treating as raw.\n")
    use_enriched <- FALSE
  }

} else {
  cat(sprintf("Enriched data not found. Loading raw SPSS: %s\n", raw_path))
  use_enriched <- FALSE
}

if (!use_enriched) {
  if (!file.exists(raw_path)) {
    stop(sprintf("Raw SPSS file not found:\n  %s\nCannot proceed.", raw_path))
  }
  df_raw <- haven::read_sav(raw_path) %>% haven::zap_labels()
  cat(sprintf("Raw SPSS loaded: %d rows x %d columns\n",
              nrow(df_raw), ncol(df_raw)))
}

# ---- 1b. Harmonize columns (raw SPSS path only) -----------------------------

if (!use_enriched) {

  know_items <- c("AIKNOW1_CORRECT_W119", "AIKNOW2_CORRECT_W119",
                  "AIKNOW3_CORRECT_W119", "AIKNOW5_CORRECT_W119",
                  "AIKNOW6_CORRECT_W119", "AIKNOW7_CORRECT_W119")

  df_raw <- df_raw %>%
    select(
      AIKNOW_INDEX_W119,
      all_of(know_items),
      DESRISK_NTECH_W119,
      F_PARTYSUM_FINAL,
      F_PARTYSUMIDEO_FINAL,
      CNCEXC_W119,
      AI_HEARD_W119,
      F_AGECAT,
      F_GENDER,
      F_EDUCCAT,
      F_RACETHNMOD,
      F_INC_TIER2,
      WEIGHT_W119
    ) %>%
    rename(
      ai_knowledge_raw  = AIKNOW_INDEX_W119,
      desrisk_ntech_raw = DESRISK_NTECH_W119,
      party_raw         = F_PARTYSUM_FINAL,
      party_ideo_raw    = F_PARTYSUMIDEO_FINAL,
      cncexc_raw        = CNCEXC_W119,
      ai_heard_raw      = AI_HEARD_W119,
      age_cat_raw       = F_AGECAT,
      gender_raw        = F_GENDER,
      educ_raw          = F_EDUCCAT,
      race_raw          = F_RACETHNMOD,
      income_raw        = F_INC_TIER2,
      weight            = WEIGHT_W119
    )

  names(df_raw)[names(df_raw) %in% know_items] <-
    paste0("know_item", c(1, 2, 3, 5, 6, 7))
}

cat(sprintf("Working rows after loading: %d\n", nrow(df_raw)))

# ---- 1c. Recode variables (two paths: enriched vs raw SPSS) -----------------

if (use_enriched) {

  # ------------------------------------------------------------------
  # ENRICHED PATH: most columns already decoded as character strings
  # and ai_knowledge is numeric 0-6. Only need to:
  #   - set factor levels / reference categories
  #   - reverse-code desrisk_ntech (numeric 1-5)
  #   - rename AIKNOW*_CORRECT_W119 to know_item* for item analysis
  # ------------------------------------------------------------------

  # Rename uppercase knowledge-item columns to compact names (if present)
  raw_item_cols <- grep("^AIKNOW[0-9]_CORRECT_W119$", names(df_raw), value = TRUE)
  if (length(raw_item_cols) > 0) {
    item_nums <- sub("AIKNOW([0-9])_CORRECT_W119", "\\1", raw_item_cols)
    new_names <- paste0("know_item", item_nums)
    names(df_raw)[names(df_raw) %in% raw_item_cols] <- new_names
  }

  # Normalize ai_heard: enriched uses "Nothing" not "Nothing at all"
  df_raw <- df_raw %>%
    mutate(ai_heard = case_when(
      ai_heard == "Nothing" ~ "Nothing at all",
      TRUE                  ~ ai_heard
    ))

  # Normalize gender: enriched uses "Male"/"Female" not "Man"/"Woman"
  df_raw <- df_raw %>%
    mutate(gender = case_when(
      gender == "Male"   ~ "Man",
      gender == "Female" ~ "Woman",
      TRUE               ~ gender
    ))

  df <- df_raw %>%
    mutate(
      # Knowledge (already numeric 0-6)
      ai_knowledge = as.numeric(ai_knowledge),

      # Party factor with reference
      party = relevel(
        factor(party, levels = c("Dem/Lean Dem", "Rep/Lean Rep")),
        ref = "Dem/Lean Dem"
      ),

      # Party ideology (already character)
      party_ideo = as.character(party_ideo),

      # Binary concerned (already 0/1 integer)
      concerned = as.integer(concerned),

      # cncexc factor
      cncexc = factor(cncexc, levels = c("Equally", "Excited", "Concerned")),

      # AI heard factor
      ai_heard = relevel(
        factor(ai_heard, levels = c("A lot", "A little", "Nothing at all")),
        ref = "A lot"
      ),

      # Tech disposition: desrisk_ntech is 1 (most comfortable) to 5 (least)
      # Reverse so higher = more tech-positive
      tech_disposition = case_when(
        desrisk_ntech %in% 1:5 ~ as.numeric(6 - desrisk_ntech),
        TRUE                   ~ NA_real_
      ),
      desrisk_ntech_orig = case_when(
        desrisk_ntech %in% 1:5 ~ as.numeric(desrisk_ntech),
        TRUE                   ~ NA_real_
      ),

      # Age
      age_cat = relevel(
        factor(age_cat, levels = c("18-29", "30-49", "50-64", "65+")),
        ref = "30-49"
      ),

      # Gender
      gender = relevel(
        factor(gender, levels = c("Man", "Woman", "Other gender", "Other")),
        ref = "Man"
      ),

      # Education
      education = relevel(
        factor(education,
               levels = c("College graduate+", "Some College",
                          "HS graduate or less")),
        ref = "College graduate+"
      ),

      # Race
      race = relevel(
        factor(race,
               levels = c("White non-Hispanic", "Black non-Hispanic",
                          "Hispanic", "Other non-Hispanic",
                          "Asian non-Hispanic")),
        ref = "White non-Hispanic"
      ),

      # Income tier
      income_tier = relevel(
        factor(income_tier, levels = c("Lower", "Middle", "Upper")),
        ref = "Middle"
      )
    )

} else {

  # ------------------------------------------------------------------
  # RAW SPSS PATH: all variables are numeric codes; decode everything
  # ------------------------------------------------------------------

  df <- df_raw %>%
    mutate(

      # Knowledge index (0-6); recompute from items if pre-computed missing
      ai_knowledge = case_when(
        !is.na(ai_knowledge_raw) & ai_knowledge_raw <= 7 ~
          as.numeric(ai_knowledge_raw),
        TRUE ~ rowSums(
          across(starts_with("know_item"), ~ as.numeric(.x == 1)),
          na.rm = FALSE
        )
      ),
      ai_knowledge = ifelse(ai_knowledge > 7, NA_real_, ai_knowledge),

      # Party (1=Rep/Lean Rep, 2=Dem/Lean Dem, 9=refuse → NA)
      party = case_when(
        party_raw == 1 ~ "Rep/Lean Rep",
        party_raw == 2 ~ "Dem/Lean Dem",
        TRUE           ~ NA_character_
      ),
      party = relevel(
        factor(party, levels = c("Dem/Lean Dem", "Rep/Lean Rep")),
        ref = "Dem/Lean Dem"
      ),

      # Party ideology
      party_ideo = case_when(
        party_ideo_raw == 1 ~ "Conservative Rep/Lean",
        party_ideo_raw == 2 ~ "Moderate/Liberal Rep/Lean",
        party_ideo_raw == 3 ~ "Moderate/Conservative Dem/Lean",
        party_ideo_raw == 4 ~ "Liberal Dem/Lean",
        TRUE                ~ NA_character_
      ),

      # Binary concerned
      concerned = case_when(
        cncexc_raw == 2          ~ 1L,
        cncexc_raw %in% c(1, 3) ~ 0L,
        TRUE                     ~ NA_integer_
      ),

      # 3-category attitude
      cncexc = case_when(
        cncexc_raw == 1 ~ "Excited",
        cncexc_raw == 2 ~ "Concerned",
        cncexc_raw == 3 ~ "Equally",
        TRUE            ~ NA_character_
      ),
      cncexc = factor(cncexc, levels = c("Equally", "Excited", "Concerned")),

      # AI heard (1=A lot, 2=A little, 3=Nothing at all; 99=NA)
      ai_heard = case_when(
        ai_heard_raw == 1  ~ "A lot",
        ai_heard_raw == 2  ~ "A little",
        ai_heard_raw == 3  ~ "Nothing at all",
        TRUE               ~ NA_character_
      ),
      ai_heard = relevel(
        factor(ai_heard, levels = c("A lot", "A little", "Nothing at all")),
        ref = "A lot"
      ),

      # Tech disposition: reverse-code (higher = more tech-positive)
      tech_disposition = case_when(
        desrisk_ntech_raw %in% 1:5 ~ as.numeric(6 - desrisk_ntech_raw),
        TRUE                        ~ NA_real_
      ),
      desrisk_ntech_orig = case_when(
        desrisk_ntech_raw %in% 1:5 ~ as.numeric(desrisk_ntech_raw),
        TRUE                        ~ NA_real_
      ),
      desrisk_ntech = desrisk_ntech_raw,

      # Age
      age_cat = case_when(
        age_cat_raw == 1 ~ "18-29",
        age_cat_raw == 2 ~ "30-49",
        age_cat_raw == 3 ~ "50-64",
        age_cat_raw == 4 ~ "65+",
        TRUE             ~ NA_character_
      ),
      age_cat = relevel(
        factor(age_cat, levels = c("18-29", "30-49", "50-64", "65+")),
        ref = "30-49"
      ),

      # Gender
      gender = case_when(
        gender_raw == 1 ~ "Man",
        gender_raw == 2 ~ "Woman",
        gender_raw == 3 ~ "Other gender",
        TRUE            ~ NA_character_
      ),
      gender = relevel(
        factor(gender, levels = c("Man", "Woman", "Other gender")),
        ref = "Man"
      ),

      # Education
      education = case_when(
        educ_raw == 1 ~ "College graduate+",
        educ_raw == 2 ~ "Some College",
        educ_raw == 3 ~ "HS graduate or less",
        TRUE          ~ NA_character_
      ),
      education = relevel(
        factor(education,
               levels = c("College graduate+", "Some College",
                          "HS graduate or less")),
        ref = "College graduate+"
      ),

      # Race
      race = case_when(
        race_raw == 1 ~ "White non-Hispanic",
        race_raw == 2 ~ "Black non-Hispanic",
        race_raw == 3 ~ "Hispanic",
        race_raw == 4 ~ "Other non-Hispanic",
        race_raw == 5 ~ "Asian non-Hispanic",
        TRUE          ~ NA_character_
      ),
      race = relevel(
        factor(race,
               levels = c("White non-Hispanic", "Black non-Hispanic",
                          "Hispanic", "Other non-Hispanic",
                          "Asian non-Hispanic")),
        ref = "White non-Hispanic"
      ),

      # Income tier
      income_tier = case_when(
        income_raw == 1 ~ "Lower",
        income_raw == 2 ~ "Middle",
        income_raw == 3 ~ "Upper",
        TRUE            ~ NA_character_
      ),
      income_tier = relevel(
        factor(income_tier, levels = c("Lower", "Middle", "Upper")),
        ref = "Middle"
      )
    )
}

cat(sprintf("After recoding: %d rows\n", nrow(df)))

# ---- 1d. Complete-case analytic subset --------------------------------------

main_vars <- c("concerned", "ai_knowledge", "party", "ai_heard",
               "age_cat", "gender", "education", "race",
               "income_tier", "weight")

df_cc <- df %>%
  filter(!is.na(party)) %>%      # drop pure independents / refused
  select(all_of(main_vars),
         tech_disposition, desrisk_ntech_orig,
         starts_with("know_item"),
         party_ideo) %>%
  drop_na(all_of(main_vars))

cat(sprintf("Complete-case analytic N: %d (dropped %d rows with missing on key vars)\n",
            nrow(df_cc), nrow(df) - nrow(df_cc)))
cat(sprintf("  Dem/Lean Dem: %d | Rep/Lean Rep: %d\n",
            sum(df_cc$party == "Dem/Lean Dem"),
            sum(df_cc$party == "Rep/Lean Rep")))

# ---- 1e. Survey design object -----------------------------------------------

svy <- svydesign(ids     = ~1,
                 weights = ~weight,
                 data    = df_cc,
                 na.action = na.omit)

cat("Survey design created.\n\n")


# =============================================================================
# ANALYSIS 1: AI KNOWLEDGE DISTRIBUTION BY PARTY (H8)
# =============================================================================

cat(strrep("=", 65), "\n")
cat("ANALYSIS 1: AI Knowledge Distribution by Party\n")
cat(strrep("=", 65), "\n\n")

# ---- 1a. Survey-weighted means and SDs by party -----------------------------

know_by_party <- svyby(
  formula = ~ai_knowledge,
  by      = ~party,
  design  = svy,
  FUN     = svymean,
  na.rm   = TRUE
)

know_sd_rep <- sqrt(as.numeric(svyvar(~ai_knowledge,
                                       design = subset(svy, party == "Rep/Lean Rep"),
                                       na.rm = TRUE)))
know_sd_dem <- sqrt(as.numeric(svyvar(~ai_knowledge,
                                       design = subset(svy, party == "Dem/Lean Dem"),
                                       na.rm = TRUE)))

cat("Weighted means by party:\n")
print(know_by_party)
cat(sprintf("  Dem SD: %.3f  |  Rep SD: %.3f\n\n", know_sd_dem, know_sd_rep))

# ---- 1b. Survey-weighted t-test ---------------------------------------------

ttest_know <- svyttest(ai_knowledge ~ party, design = svy)
cat("Survey-weighted t-test (Dem vs Rep):\n")
print(ttest_know)
cat("\n")

mean_dem  <- know_by_party$ai_knowledge[know_by_party$party == "Dem/Lean Dem"]
mean_rep  <- know_by_party$ai_knowledge[know_by_party$party == "Rep/Lean Rep"]
se_dem    <- know_by_party$se[know_by_party$party == "Dem/Lean Dem"]
se_rep    <- know_by_party$se[know_by_party$party == "Rep/Lean Rep"]
t_stat    <- ttest_know$statistic
p_val     <- ttest_know$p.value

# ---- 1c. Proportion scoring 6 or 7 (high knowledge) by party ---------------

# Note: index goes 0-6 (6 items), so "high" = 5 or 6
high_know_thresh <- max(df_cc$ai_knowledge, na.rm = TRUE) - 1

hi_know_dem_obj <- svymean(~I(ai_knowledge >= high_know_thresh),
                           design = subset(svy, party == "Dem/Lean Dem"),
                           na.rm = TRUE)
hi_know_rep_obj <- svymean(~I(ai_knowledge >= high_know_thresh),
                           design = subset(svy, party == "Rep/Lean Rep"),
                           na.rm = TRUE)

# svymean on a logical returns two values (FALSE row, TRUE row); extract TRUE
hi_know_dem <- as.numeric(hi_know_dem_obj)[length(hi_know_dem_obj)]
hi_know_rep <- as.numeric(hi_know_rep_obj)[length(hi_know_rep_obj)]

cat(sprintf("High knowledge (score >= %d) — Dem: %.1f%%  Rep: %.1f%%  Gap: %.1fpp\n\n",
            high_know_thresh,
            hi_know_dem * 100,
            hi_know_rep * 100,
            (hi_know_dem - hi_know_rep) * 100))

# ---- 1d. Save knowledge-by-party table --------------------------------------

tbl_know_party <- tibble(
  party            = know_by_party$party,
  mean_knowledge   = round(know_by_party$ai_knowledge, 3),
  se_mean          = round(know_by_party$se, 3),
  sd_knowledge     = round(c(know_sd_dem, know_sd_rep), 3),
  pct_high_know    = round(c(hi_know_dem, hi_know_rep) * 100, 1),
  high_know_thresh = high_know_thresh,
  t_statistic      = round(t_stat, 3),
  p_value          = round(p_val, 4),
  sig              = sig_stars(p_val),
  n_unweighted     = c(
    sum(df_cc$party == "Dem/Lean Dem"),
    sum(df_cc$party == "Rep/Lean Rep")
  )
)

write_csv(tbl_know_party,
          file.path(out_tables, "tbl_06_knowledge_by_party.csv"))
cat(sprintf("Saved: tbl_06_knowledge_by_party.csv\n\n"))

# ---- 1e. Figure 5: Overlapping density plot by party -----------------------

cat("Building Figure 5: Overlapping knowledge density plot...\n")

# Compute weighted density for each party using individual-level data
# srvyr gives us weighted counts per score level
know_dist <- df_cc %>%
  group_by(party, ai_knowledge) %>%
  summarise(
    n_wt = sum(weight),
    n    = n(),
    .groups = "drop"
  ) %>%
  group_by(party) %>%
  mutate(prop = n_wt / sum(n_wt)) %>%
  ungroup() %>%
  filter(!is.na(ai_knowledge))

# Annotation text
annot_txt <- sprintf(
  "Dem mean = %.2f (SD = %.2f)\nRep mean = %.2f (SD = %.2f)\nt = %.2f, p %s",
  mean_dem, know_sd_dem,
  mean_rep, know_sd_rep,
  t_stat,
  ifelse(p_val < 0.001, "< 0.001", sprintf("= %.3f", p_val))
)

# Vertical lines for group means
vlines <- tibble(
  party = factor(c("Dem/Lean Dem", "Rep/Lean Rep"),
                 levels = c("Dem/Lean Dem", "Rep/Lean Rep")),
  xint  = c(mean_dem, mean_rep),
  color = c(col_dem, col_rep)
)

fig5 <- ggplot(know_dist,
               aes(x = ai_knowledge, y = prop,
                   fill = party, color = party)) +
  geom_col(data = know_dist %>% filter(party == "Rep/Lean Rep"),
           alpha = 0.45, width = 0.8, position = "identity") +
  geom_col(data = know_dist %>% filter(party == "Dem/Lean Dem"),
           alpha = 0.45, width = 0.8, position = "identity") +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.5) +
  geom_vline(data = vlines,
             aes(xintercept = xint, color = party),
             linetype = "dashed", linewidth = 0.85, show.legend = FALSE) +
  annotate("text",
           x = max(df_cc$ai_knowledge, na.rm = TRUE) * 0.62,
           y = max(know_dist$prop) * 0.92,
           label = annot_txt,
           hjust = 0, vjust = 1,
           size = 3.4, color = "grey25",
           family = "mono") +
  scale_x_continuous(breaks = 0:max(df_cc$ai_knowledge, na.rm = TRUE),
                     name = "AI Knowledge Index") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     name = "Weighted Proportion") +
  scale_fill_manual(
    values = c("Dem/Lean Dem" = col_dem, "Rep/Lean Rep" = col_rep),
    name   = "Party ID"
  ) +
  scale_color_manual(
    values = c("Dem/Lean Dem" = col_dem, "Rep/Lean Rep" = col_rep),
    name   = "Party ID"
  ) +
  labs(
    title    = "Distribution of AI Knowledge Index by Party Identification",
    subtitle = "Pew ATP W119, December 2022 (survey-weighted proportions)",
    caption  = paste0(
      "Note: Index = sum of correct answers (0\u2013",
      max(df_cc$ai_knowledge, na.rm = TRUE),
      "). Dashed lines = group means.\n",
      "Source: Pew Research Center American Trends Panel."
    )
  ) +
  theme_pub() +
  guides(fill  = guide_legend(override.aes = list(alpha = 0.6)),
         color = guide_legend(override.aes = list(linewidth = 1.5)))

save_fig(fig5, "fig5_knowledge_distribution.png", width = 7, height = 5)


# =============================================================================
# ANALYSIS 2: KNOWLEDGE AS MODERATOR OF PARTY → CONCERN (H8 KEY TEST)
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS 2: Knowledge as Moderator of Party Effect on Concern\n")
cat(strrep("=", 65), "\n\n")

# ---- 2a. Model 1: Main effects (party + knowledge + controls) ---------------

formula_main <- concerned ~
  party + ai_knowledge +
  age_cat + gender + education + race + income_tier + ai_heard

m1 <- svyglm(formula_main,
             design    = svy,
             family    = quasibinomial(link = "logit"),
             na.action = na.omit)

n_m1 <- nrow(model.frame(m1))
cat(sprintf("Model 1 (main effects) N = %d\n", n_m1))

# ---- 2b. Model 2: Interaction (party × knowledge) ---------------------------

formula_int <- concerned ~
  party * ai_knowledge +
  age_cat + gender + education + race + income_tier + ai_heard

m2 <- svyglm(formula_int,
             design    = svy,
             family    = quasibinomial(link = "logit"),
             na.action = na.omit)

n_m2 <- nrow(model.frame(m2))
cat(sprintf("Model 2 (interaction)  N = %d\n\n", n_m2))

# ---- 2c. Report interaction term --------------------------------------------

tbl_m2 <- tidy_svyglm(m2, "Model2_interaction")

int_row <- tbl_m2 %>%
  filter(grepl("party.*ai_knowledge|ai_knowledge.*party", term,
               ignore.case = TRUE))

if (nrow(int_row) > 0) {
  cat("KEY INTERACTION TERM (party × ai_knowledge):\n")
  print(int_row %>% select(term, OR_fmt, CI_fmt, p_fmt, sig))

  int_or  <- int_row$OR[1]
  int_p   <- int_row$p_value[1]

  # Interpret direction
  if (int_p >= 0.05) {
    cat("\n  --> Interaction NOT significant (p >= 0.05)\n")
    cat("  --> Information deficit model is FALSIFIED:\n")
    cat("      Knowledge does not differentially reduce concern by party.\n")
  } else if (int_or < 1) {
    cat("\n  --> Significant NEGATIVE interaction (OR < 1)\n")
    cat("  --> Partial information deficit: knowledge reduces Rep concern\n")
    cat("      faster (or reduces Dem concern less).\n")
  } else {
    cat("\n  --> Significant POSITIVE interaction (OR > 1)\n")
    cat("  --> Polarization AMPLIFICATION (Kahan 2012):\n")
    cat("      Knowledge increases the partisan gap in concern.\n")
  }
} else {
  cat("WARNING: Could not locate interaction term in Model 2 coefficients.\n")
  cat("Term names present:\n")
  print(tbl_m2$term)
}
cat("\n")

# ---- 2d. Combine models into export table -----------------------------------

tbl_m1 <- tidy_svyglm(m1, "Model1_main_effects")

tbl_interact_export <- bind_rows(
  tbl_m1 %>% mutate(analytic_n = n_m1),
  tbl_m2 %>% mutate(analytic_n = n_m2)
) %>%
  select(model, term, analytic_n, OR, OR_lo95, OR_hi95,
         OR_fmt, CI_fmt, t_value, p_value, p_fmt, sig)

write_csv(tbl_interact_export,
          file.path(out_tables, "tbl_06_knowledge_interaction.csv"))
cat("Saved: tbl_06_knowledge_interaction.csv\n\n")

# ---- 2e. Predicted probabilities across knowledge by party ------------------

cat("Computing predicted probabilities for interaction plot...\n")

# Build prediction grid: vary party and ai_knowledge; hold controls at modal
modal_age    <- names(sort(table(df_cc$age_cat),    decreasing = TRUE))[1]
modal_gender <- names(sort(table(df_cc$gender),     decreasing = TRUE))[1]
modal_educ   <- names(sort(table(df_cc$education),  decreasing = TRUE))[1]
modal_race   <- names(sort(table(df_cc$race),       decreasing = TRUE))[1]
modal_income <- names(sort(table(df_cc$income_tier),decreasing = TRUE))[1]
modal_heard  <- names(sort(table(df_cc$ai_heard),   decreasing = TRUE))[1]

know_range <- 0:max(df_cc$ai_knowledge, na.rm = TRUE)

pred_grid <- expand.grid(
  party       = levels(df_cc$party),
  ai_knowledge = know_range,
  age_cat     = modal_age,
  gender      = modal_gender,
  education   = modal_educ,
  race        = modal_race,
  income_tier = modal_income,
  ai_heard    = modal_heard,
  weight      = mean(df_cc$weight),
  stringsAsFactors = FALSE
) %>%
  mutate(
    party       = factor(party,       levels = levels(df_cc$party)),
    age_cat     = factor(age_cat,     levels = levels(df_cc$age_cat)),
    gender      = factor(gender,      levels = levels(df_cc$gender)),
    education   = factor(education,   levels = levels(df_cc$education)),
    race        = factor(race,        levels = levels(df_cc$race)),
    income_tier = factor(income_tier, levels = levels(df_cc$income_tier)),
    ai_heard    = factor(ai_heard,    levels = levels(df_cc$ai_heard))
  )

pred_link <- predict(m2,
                     newdata = pred_grid,
                     type    = "link",
                     se      = TRUE)

pred_grid <- pred_grid %>%
  mutate(
    fit     = as.numeric(pred_link),
    se_fit  = as.numeric(SE(pred_link)),
    prob    = plogis(fit),
    prob_lo = plogis(fit - 1.96 * se_fit),
    prob_hi = plogis(fit + 1.96 * se_fit)
  )

# ---- 2f. Interaction plot ---------------------------------------------------

# Compute observed weighted proportions at each knowledge level for reference
obs_props <- df_cc %>%
  group_by(party, ai_knowledge) %>%
  summarise(
    prop_concerned = weighted.mean(concerned, w = weight, na.rm = TRUE),
    n              = n(),
    .groups        = "drop"
  ) %>%
  filter(!is.na(ai_knowledge), !is.na(prop_concerned))

fig_int <- ggplot(pred_grid,
                  aes(x      = ai_knowledge,
                      y      = prob,
                      color  = party,
                      fill   = party,
                      group  = party,
                      ymin   = prob_lo,
                      ymax   = prob_hi)) +
  geom_ribbon(alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(data = obs_props,
             aes(x = ai_knowledge, y = prop_concerned,
                 color = party, fill = party),
             shape = 21, size = 2.5, alpha = 0.6,
             inherit.aes = FALSE) +
  scale_x_continuous(breaks = know_range,
                     name   = "AI Knowledge Index") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1),
                     name   = "Predicted P(More Concerned than Excited)") +
  scale_color_manual(
    values = c("Dem/Lean Dem" = col_dem, "Rep/Lean Rep" = col_rep),
    name   = "Party ID"
  ) +
  scale_fill_manual(
    values = c("Dem/Lean Dem" = col_dem, "Rep/Lean Rep" = col_rep),
    name   = "Party ID"
  ) +
  labs(
    title    = "Knowledge × Party Interaction: Effect on AI Concern",
    subtitle = "Predicted probabilities from logistic regression with party × knowledge interaction",
    caption  = paste0(
      "Note: Lines = model predictions; dots = observed weighted proportions.\n",
      "Covariates held at modal values. 95% CI shown. N = ", n_m2, ".\n",
      "Source: Pew Research Center American Trends Panel, Dec 2022."
    )
  ) +
  theme_pub()

save_fig(fig_int, "fig_06_knowledge_interaction.png", width = 7, height = 5)


# =============================================================================
# ANALYSIS 3: TECHNOLOGY DISPOSITION (DESRISK) AS CONTROL
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS 3: Technology Disposition (DESRISK) as Control\n")
cat(strrep("=", 65), "\n\n")

# ---- 3a. Subset with non-missing tech_disposition ---------------------------

df_desrisk <- df_cc %>% filter(!is.na(tech_disposition))
svy_desrisk <- svydesign(ids     = ~1,
                          weights = ~weight,
                          data    = df_desrisk,
                          na.action = na.omit)

cat(sprintf("N with tech_disposition: %d\n\n", nrow(df_desrisk)))

# ---- 3b. Weighted mean DESRISK by party -------------------------------------

desrisk_by_party <- svyby(
  formula = ~tech_disposition,
  by      = ~party,
  design  = svy_desrisk,
  FUN     = svymean,
  na.rm   = TRUE
)

cat("Tech disposition (higher = more tech-positive) by party:\n")
print(desrisk_by_party)

desrisk_ttest <- svyttest(tech_disposition ~ party, design = svy_desrisk)
cat("\nT-test for tech disposition by party:\n")
print(desrisk_ttest)
cat("\n")

# ---- 3c. Model A: party effect WITHOUT tech disposition (same sample) -------

formula_nodesrisk <- concerned ~
  party + ai_knowledge +
  age_cat + gender + education + race + income_tier + ai_heard

m_nodesrisk <- svyglm(formula_nodesrisk,
                       design    = svy_desrisk,
                       family    = quasibinomial(link = "logit"),
                       na.action = na.omit)

n_nodesrisk <- nrow(model.frame(m_nodesrisk))

or_party_nodesrisk <- exp(coef(m_nodesrisk)["partyRep/Lean Rep"])
ci_party_nodesrisk <- exp(confint(m_nodesrisk)["partyRep/Lean Rep", ])

cat(sprintf("Party OR (without DESRISK): %.3f [%.3f, %.3f]\n",
            or_party_nodesrisk, ci_party_nodesrisk[1], ci_party_nodesrisk[2]))

# ---- 3d. Model B: party effect WITH tech disposition ------------------------

formula_desrisk <- concerned ~
  party + ai_knowledge + tech_disposition +
  age_cat + gender + education + race + income_tier + ai_heard

m_desrisk <- svyglm(formula_desrisk,
                     design    = svy_desrisk,
                     family    = quasibinomial(link = "logit"),
                     na.action = na.omit)

n_desrisk <- nrow(model.frame(m_desrisk))

or_party_desrisk <- exp(coef(m_desrisk)["partyRep/Lean Rep"])
ci_party_desrisk <- exp(confint(m_desrisk)["partyRep/Lean Rep", ])

cat(sprintf("Party OR (with    DESRISK): %.3f [%.3f, %.3f]\n",
            or_party_desrisk, ci_party_desrisk[1], ci_party_desrisk[2]))

# Attenuation calculation
pct_attenuation <- (1 - (log(or_party_desrisk) / log(or_party_nodesrisk))) * 100
cat(sprintf("\nAttenuation of log-OR after adding tech disposition: %.1f%%\n",
            pct_attenuation))
if (abs(pct_attenuation) > 10) {
  cat("  --> Tech disposition PARTIALLY mediates the party effect.\n")
} else {
  cat("  --> Little attenuation: party effect is largely independent of\n")
  cat("      dispositional risk tolerance.\n")
}
cat("\n")

# ---- 3e. Export DESRISK model table -----------------------------------------

tbl_nd  <- tidy_svyglm(m_nodesrisk, "Model_without_DESRISK") %>%
  mutate(analytic_n = n_nodesrisk)
tbl_d   <- tidy_svyglm(m_desrisk,   "Model_with_DESRISK") %>%
  mutate(analytic_n = n_desrisk)

tbl_desrisk_export <- bind_rows(tbl_nd, tbl_d) %>%
  select(model, term, analytic_n, OR, OR_lo95, OR_hi95,
         OR_fmt, CI_fmt, t_value, p_value, p_fmt, sig)

write_csv(tbl_desrisk_export,
          file.path(out_tables, "tbl_06_desrisk_models.csv"))
cat("Saved: tbl_06_desrisk_models.csv\n\n")


# =============================================================================
# ANALYSIS 4: ITEM-LEVEL KNOWLEDGE BY PARTY
# =============================================================================

cat(strrep("=", 65), "\n")
cat("ANALYSIS 4: Item-Level Knowledge Correct Rate by Party\n")
cat(strrep("=", 65), "\n\n")

# Check which know_item columns exist in df_cc
item_cols <- grep("^know_item", names(df_cc), value = TRUE)

if (length(item_cols) == 0) {
  cat("Knowledge item columns not available in analytic dataset.\n")
  cat("Skipping item-level analysis.\n\n")
} else {

  cat(sprintf("Found %d knowledge item columns: %s\n\n",
              length(item_cols), paste(item_cols, collapse = ", ")))

  # Map item column names to readable labels
  item_labels <- c(
    know_item1 = "AIKNOW1\n(DeepFake)",
    know_item2 = "AIKNOW2\n(Facial recog.)",
    know_item3 = "AIKNOW3\n(Self-driving)",
    know_item5 = "AIKNOW5\n(ChatGPT)",
    know_item6 = "AIKNOW6\n(Algorithm)",
    know_item7 = "AIKNOW7\n(Virtual asst.)"
  )

  # Compute weighted proportion correct per item per party
  item_results <- lapply(item_cols, function(col) {
    # Survey design with this item non-missing
    df_item <- df_cc %>% filter(!is.na(.data[[col]]), !is.na(party))
    svy_item <- svydesign(ids = ~1, weights = ~weight, data = df_item)

    by_result <- svyby(
      formula = as.formula(paste0("~", col)),
      by      = ~party,
      design  = svy_item,
      FUN     = svymean,
      na.rm   = TRUE
    )

    tibble(
      item       = col,
      item_label = item_labels[col],
      party      = by_result$party,
      pct_correct = by_result[[col]] * 100,
      se          = by_result$se * 100
    )
  })

  tbl_items <- bind_rows(item_results) %>%
    mutate(
      item_label = ifelse(is.na(item_label), item, item_label),
      party      = factor(party, levels = c("Dem/Lean Dem", "Rep/Lean Rep"))
    )

  cat("Item-level % correct by party:\n")
  print(tbl_items %>% select(item, party, pct_correct, se) %>%
          pivot_wider(names_from = party,
                      values_from = c(pct_correct, se)))
  cat("\n")

  # ---- 4b. Grouped bar chart -------------------------------------------------

  fig_items <- ggplot(tbl_items,
                      aes(x    = item_label,
                          y    = pct_correct,
                          fill = party)) +
    geom_col(position = position_dodge(width = 0.75),
             width = 0.65, color = "white", linewidth = 0.25) +
    geom_errorbar(aes(ymin = pct_correct - 1.96 * se,
                      ymax = pct_correct + 1.96 * se),
                  position  = position_dodge(width = 0.75),
                  width     = 0.25,
                  linewidth = 0.4,
                  color     = "grey30") +
    scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1),
                       limits = c(0, 105),
                       name   = "% Correct (Survey-Weighted)") +
    scale_fill_manual(
      values = c("Dem/Lean Dem" = col_dem, "Rep/Lean Rep" = col_rep),
      name   = "Party ID"
    ) +
    labs(
      title    = "AI Knowledge Item Correct Rates by Party Identification",
      subtitle = "Pew ATP W119, December 2022 (survey-weighted; 95% CI bars)",
      x        = "Knowledge Item",
      caption  = paste0(
        "Note: Items reflect knowledge questions from ATP W119.\n",
        "Source: Pew Research Center American Trends Panel."
      )
    ) +
    theme_pub()

  save_fig(fig_items, "fig_06_knowledge_items.png", width = 8, height = 5)

}


# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("ANALYSIS COMPLETE — 06_w119_knowledge.R\n")
cat(strrep("=", 65), "\n\n")

cat(sprintf("Analytic sample: N = %d\n", nrow(df_cc)))
cat(sprintf("  Dem/Lean Dem : N = %d\n", sum(df_cc$party == "Dem/Lean Dem")))
cat(sprintf("  Rep/Lean Rep : N = %d\n", sum(df_cc$party == "Rep/Lean Rep")))

cat("\nKey findings (information deficit model test):\n")
cat(sprintf("  Democratic AI knowledge mean : %.2f\n", mean_dem))
cat(sprintf("  Republican AI knowledge mean : %.2f\n", mean_rep))
cat(sprintf("  Party knowledge gap          : %.2f (t = %.2f, p %s)\n",
            mean_dem - mean_rep, t_stat,
            ifelse(p_val < 0.001, "< 0.001", sprintf("= %.3f", p_val))))

cat("\nFigures saved to output/figures/:\n")
cat("  fig5_knowledge_distribution.png      (Analysis 1)\n")
cat("  fig_06_knowledge_interaction.png     (Analysis 2)\n")
cat("  fig_06_knowledge_items.png           (Analysis 4, if items available)\n")

cat("\nTables saved to output/tables/:\n")
cat("  tbl_06_knowledge_by_party.csv        (Analysis 1)\n")
cat("  tbl_06_knowledge_interaction.csv     (Analysis 2)\n")
cat("  tbl_06_desrisk_models.csv            (Analysis 3)\n")

cat("\nReference categories for regression models:\n")
cat("  party        : Dem/Lean Dem\n")
cat("  education    : College graduate+\n")
cat("  age_cat      : 30-49\n")
cat("  race         : White non-Hispanic\n")
cat("  income_tier  : Middle\n")
cat("  ai_heard     : A lot\n")
cat("  gender       : Man\n")
