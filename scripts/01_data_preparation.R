# =============================================================================
# 01_data_preparation.R
# AI Attitude Polarization Study — Pew ATP Data Preparation
# Waves: W119 (Dec 2022), W132 (Aug 2023), W152 (Aug 2024)
# =============================================================================
# Description:
#   Loads raw SPSS files for three ATP waves, extracts and harmonizes variables,
#   recodes to analysis-ready factors, stacks into a combined dataset, and saves
#   processed .rds files for downstream analysis.
#
# Input:
#   data/raw/W119_Dec22/ATP W119.sav
#   data/raw/W132_Aug23/ATP W132.sav
#   data/raw/W152_Aug24/ATP W152.sav
#
# Output:
#   data/processed/pew_combined.rds
#   data/processed/pew_w119.rds
#   data/processed/pew_w132.rds
#   data/processed/pew_w152.rds
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(haven)      # Read SPSS .sav files
  library(tidyverse)  # Data wrangling and piping
  library(survey)     # Survey design (loaded for downstream use)
  library(labelled)   # Work with SPSS/Stata value labels
})

# Resolve project root: works from RStudio, Rscript, or here::here()
project_root <- tryCatch(
  dirname(dirname(rstudioapi::getActiveDocumentContext()$path)),
  error = function(e) {
    here_root <- tryCatch(here::here(), error = function(e2) NULL)
    if (!is.null(here_root)) return(here_root)
    getwd()
  }
)

message("Project root: ", project_root)

# Helper: build absolute path from project root
proj_path <- function(...) file.path(project_root, ...)

# Ensure output directory exists
dir.create(proj_path("data", "processed"), showWarnings = FALSE, recursive = TRUE)

# Raw data file paths
path_w119 <- proj_path("data", "raw", "W119_Dec22", "ATP W119.sav")
path_w132 <- proj_path("data", "raw", "W132_Aug23", "ATP W132.sav")
path_w152 <- proj_path("data", "raw", "W152_Aug24", "ATP W152.sav")

# Verify all input files exist before proceeding
walk(list(path_w119, path_w132, path_w152), function(p) {
  if (!file.exists(p)) stop("Input file not found: ", p)
})
message("All input files found. Proceeding with data preparation.")


# =============================================================================
# 1. Load Raw SPSS Data
# =============================================================================

message("\n--- Loading raw SPSS files ---")

raw_w119 <- read_sav(path_w119)
message("W119 loaded: ", nrow(raw_w119), " rows, ", ncol(raw_w119), " columns")

raw_w132 <- read_sav(path_w132)
message("W132 loaded: ", nrow(raw_w132), " rows, ", ncol(raw_w132), " columns")

raw_w152 <- read_sav(path_w152)
message("W152 loaded: ", nrow(raw_w152), " rows, ", ncol(raw_w152), " columns")


# =============================================================================
# 2. Variable Extraction Helpers
# =============================================================================

# Demographic variable names are identical across all three waves
DEMO_VARS <- c(
  "F_AGECAT", "F_GENDER", "F_EDUCCAT", "F_EDUCCAT2",
  "F_RACETHNMOD", "F_PARTY_FINAL", "F_PARTYSUM_FINAL",
  "F_PARTYSUMIDEO_FINAL", "F_INC_SDT1", "F_INC_TIER2",
  "F_CREGION", "F_METRO", "F_RELIG", "F_MARITAL"
)

# Safely extract a variable by name, returning NA_real_ if absent (e.g., USEAI
# is not present in W132).
safe_pull <- function(df, var_name) {
  if (var_name %in% names(df)) {
    zap_labels(df[[var_name]])
  } else {
    rep(NA_real_, nrow(df))
  }
}

# Extract wave-specific core variables, rename to a common schema, and attach
# demographics.
#
# Arguments:
#   raw_df        — raw data frame from read_sav()
#   wave_num      — integer wave number (1, 2, or 3)
#   wave_date_str — character label, e.g. "Dec 2022"
#   suffix        — wave suffix for variable lookup, e.g. "W119"
#   has_useai     — logical; TRUE if USEAI_{suffix} exists in this wave
extract_wave <- function(raw_df, wave_num, wave_date_str, suffix, has_useai) {

  message("Extracting wave ", wave_num, " (", wave_date_str, ") ...")

  # Verify required wave-specific variables exist
  required_wave_vars <- c(
    paste0("CNCEXC_",   suffix),
    paste0("AI_HEARD_", suffix),
    paste0("WEIGHT_",   suffix)
  )
  missing_req <- setdiff(required_wave_vars, names(raw_df))
  if (length(missing_req) > 0) {
    stop("Missing required variables in wave ", wave_num, ": ",
         paste(missing_req, collapse = ", "))
  }

  # Warn (but don't stop) if any demographic variables are absent
  missing_demo <- setdiff(DEMO_VARS, names(raw_df))
  if (length(missing_demo) > 0) {
    warning("Wave ", wave_num, " missing demographic variables: ",
            paste(missing_demo, collapse = ", "),
            " — filled with NA.")
  }

  # Build core data frame with harmonized variable names
  core <- tibble(
    wave         = wave_num,
    wave_date    = wave_date_str,
    cncexc_raw   = zap_labels(raw_df[[paste0("CNCEXC_",   suffix)]]),
    ai_heard_raw = zap_labels(raw_df[[paste0("AI_HEARD_", suffix)]]),
    useai_raw    = if (has_useai)
                     zap_labels(raw_df[[paste0("USEAI_", suffix)]])
                   else
                     NA_real_,
    weight       = zap_labels(raw_df[[paste0("WEIGHT_",   suffix)]])
  )

  # Append demographics (safe_pull returns NA vector for absent columns)
  demo <- map_dfc(set_names(DEMO_VARS), ~ safe_pull(raw_df, .x))

  bind_cols(core, demo)
}


# =============================================================================
# 3. Extract Each Wave
# =============================================================================

wave1 <- extract_wave(raw_w119, wave_num = 1, wave_date_str = "Dec 2022",
                      suffix = "W119", has_useai = TRUE)
wave2 <- extract_wave(raw_w132, wave_num = 2, wave_date_str = "Aug 2023",
                      suffix = "W132", has_useai = FALSE)
wave3 <- extract_wave(raw_w152, wave_num = 3, wave_date_str = "Aug 2024",
                      suffix = "W152", has_useai = TRUE)


# =============================================================================
# 4. Recoding Function
# =============================================================================

# Apply consistent recoding to a single wave data frame.
# Numeric 99 (and wave-specific refused codes where noted) become NA before
# factor creation.  All raw numeric columns are dropped after recoding.
recode_wave <- function(df) {

  df %>%
    mutate(

      # -----------------------------------------------------------------------
      # cncexc: Overall feel more concerned/excited/equally about AI
      # Codes: 1 = Excited, 2 = Concerned, 3 = Equally; 99 = Refused/NA
      # -----------------------------------------------------------------------
      cncexc = if_else(cncexc_raw == 99, NA_real_, cncexc_raw),
      cncexc = factor(cncexc,
                      levels = 1:3,
                      labels = c("Excited", "Concerned", "Equally")),

      # -----------------------------------------------------------------------
      # ai_heard: How much heard about AI
      # Codes: 1 = A lot, 2 = A little, 3 = Nothing; 99 = Refused/NA
      # -----------------------------------------------------------------------
      ai_heard = if_else(ai_heard_raw == 99, NA_real_, ai_heard_raw),
      ai_heard = factor(ai_heard,
                        levels = 1:3,
                        labels = c("A lot", "A little", "Nothing")),

      # -----------------------------------------------------------------------
      # useai: AI interaction frequency (W119 and W152 only; NA for W132)
      # Codes: 1=Almost constantly, 2=Several times a day, 3=About once a day,
      #        4=Several times a week, 5=Less often; 99=DK/Ref; NA=not asked
      # -----------------------------------------------------------------------
      useai = case_when(
        is.na(useai_raw)  ~ NA_real_,
        useai_raw == 99   ~ NA_real_,
        TRUE              ~ useai_raw
      ),
      useai = factor(useai,
                     levels = 1:5,
                     labels = c("Almost constantly", "Several times/day",
                                "About once/day", "Several times/week",
                                "Less often")),

      # -----------------------------------------------------------------------
      # age_cat: Age category
      # Pew F_AGECAT codes: 1=18-29, 2=30-49, 3=50-64, 4=65+; 99=DK/Ref
      # -----------------------------------------------------------------------
      age_cat = if_else(F_AGECAT == 99, NA_real_, F_AGECAT),
      age_cat = factor(age_cat,
                       levels = 1:4,
                       labels = c("18-29", "30-49", "50-64", "65+")),

      # -----------------------------------------------------------------------
      # gender: Gender identity
      # Pew F_GENDER codes: 1 = Male, 2 = Female, 3 = Other/non-binary;
      #                     99 = DK/Ref
      # -----------------------------------------------------------------------
      gender = case_when(
        F_GENDER == 99 ~ NA_character_,
        F_GENDER == 1  ~ "Male",
        F_GENDER == 2  ~ "Female",
        F_GENDER == 3  ~ "Other",
        TRUE           ~ NA_character_
      ),
      gender = factor(gender, levels = c("Male", "Female", "Other")),

      # -----------------------------------------------------------------------
      # education: 3-category education
      # Pew F_EDUCCAT codes: 1=College graduate+, 2=Some College,
      #                      3=H.S. graduate or less; 99=DK/Ref
      # -----------------------------------------------------------------------
      education = if_else(F_EDUCCAT == 99, NA_real_, F_EDUCCAT),
      education = factor(education,
                         levels = 1:3,
                         labels = c("College graduate+", "Some College",
                                    "HS graduate or less")),

      # -----------------------------------------------------------------------
      # education_detail: Detailed education categories
      # Pew F_EDUCCAT2 codes: 1=Less than HS, 2=HS graduate,
      #   3=Some college no degree, 4=Associate degree,
      #   5=College grad/some postgrad, 6=Postgraduate; 99=DK/Ref
      # -----------------------------------------------------------------------
      education_detail = if_else(F_EDUCCAT2 == 99, NA_real_, F_EDUCCAT2),
      education_detail = factor(education_detail,
                                levels = 1:6,
                                labels = c("Less than HS",
                                           "HS graduate",
                                           "Some college",
                                           "Associate degree",
                                           "College graduate",
                                           "Postgraduate")),

      # -----------------------------------------------------------------------
      # race: Race/ethnicity
      # Pew F_RACETHNMOD codes: 1=White non-Hisp, 2=Black non-Hisp,
      #   3=Hispanic, 4=Other non-Hisp, 5=Asian non-Hisp; 99=DK/Ref
      # -----------------------------------------------------------------------
      race = if_else(F_RACETHNMOD == 99, NA_real_, F_RACETHNMOD),
      race = factor(race,
                    levels = 1:5,
                    labels = c("White non-Hispanic",
                               "Black non-Hispanic",
                               "Hispanic",
                               "Other non-Hispanic",
                               "Asian non-Hispanic")),

      # -----------------------------------------------------------------------
      # party: Party identification (binary leaned)
      # Pew F_PARTYSUM_FINAL codes: 1=Rep/Lean Rep, 2=Dem/Lean Dem, 9=DK/No lean
      # -----------------------------------------------------------------------
      party = case_when(
        F_PARTYSUM_FINAL == 9 ~ NA_character_,
        F_PARTYSUM_FINAL == 1 ~ "Rep/Lean Rep",
        F_PARTYSUM_FINAL == 2 ~ "Dem/Lean Dem",
        TRUE                  ~ NA_character_
      ),
      party = factor(party,
                     levels = c("Rep/Lean Rep", "Dem/Lean Dem")),

      # -----------------------------------------------------------------------
      # party_ideo: Detailed party + ideology
      # Pew F_PARTYSUMIDEO_FINAL codes:
      #   1=Conservative Rep/Lean Rep, 2=Moderate/Liberal Rep/Lean Rep,
      #   3=Moderate/Conservative Dem/Lean Dem, 4=Liberal Dem/Lean Dem;
      #   9=DK/Ref
      # -----------------------------------------------------------------------
      party_ideo = if_else(F_PARTYSUMIDEO_FINAL == 9, NA_real_,
                           F_PARTYSUMIDEO_FINAL),
      party_ideo = factor(party_ideo,
                          levels = 1:4,
                          labels = c("Cons Rep/Lean Rep",
                                     "Mod/Lib Rep/Lean Rep",
                                     "Mod/Cons Dem/Lean Dem",
                                     "Lib Dem/Lean Dem")),

      # -----------------------------------------------------------------------
      # income_tier: 3-tier income classification
      # Pew F_INC_TIER2 codes: 1=Lower, 2=Middle, 3=Upper; 99=DK/Ref
      # -----------------------------------------------------------------------
      income_tier = if_else(F_INC_TIER2 == 99, NA_real_, F_INC_TIER2),
      income_tier = factor(income_tier,
                           levels = 1:3,
                           labels = c("Lower", "Middle", "Upper")),

      # -----------------------------------------------------------------------
      # region: Census region
      # Pew F_CREGION codes: 1=Northeast, 2=Midwest, 3=South, 4=West
      # -----------------------------------------------------------------------
      region = factor(F_CREGION,
                      levels = 1:4,
                      labels = c("Northeast", "Midwest", "South", "West")),

      # -----------------------------------------------------------------------
      # metro: Metro area status
      # Pew F_METRO codes: 1=Metropolitan, 2=Non-metropolitan
      # -----------------------------------------------------------------------
      metro = factor(F_METRO,
                     levels = 1:2,
                     labels = c("Metropolitan", "Non-metropolitan"))

    ) %>%
    # Drop raw numeric extraction columns; keep final recoded variables
    select(-cncexc_raw, -ai_heard_raw, -useai_raw,
           -starts_with("F_"))
}


# =============================================================================
# 5. Apply Recoding to Each Wave
# =============================================================================

message("\n--- Recoding variables ---")

pew_w119 <- recode_wave(wave1)
message("W119 recoded: ", nrow(pew_w119), " rows")

pew_w132 <- recode_wave(wave2)
message("W132 recoded: ", nrow(pew_w132), " rows")

pew_w152 <- recode_wave(wave3)
message("W152 recoded: ", nrow(pew_w152), " rows")


# =============================================================================
# 6. Stack Waves into Combined Dataset
# =============================================================================

message("\n--- Combining waves ---")

pew_combined <- bind_rows(pew_w119, pew_w132, pew_w152) %>%
  mutate(
    wave = factor(wave,
                  levels = 1:3,
                  labels = c("W119 Dec 2022",
                             "W132 Aug 2023",
                             "W152 Aug 2024")),
    wave_date = factor(wave_date,
                       levels = c("Dec 2022", "Aug 2023", "Aug 2024"))
  )

message("Combined dataset: ", nrow(pew_combined), " rows x ",
        ncol(pew_combined), " columns")


# =============================================================================
# 7. Binary Dependent Variables
# =============================================================================

message("\n--- Creating binary DVs ---")

pew_combined <- pew_combined %>%
  mutate(
    # concerned: 1 if cncexc == "Concerned", 0 for Excited or Equally, NA if
    # cncexc is missing.
    concerned = case_when(
      is.na(cncexc)         ~ NA_real_,
      cncexc == "Concerned" ~ 1,
      TRUE                  ~ 0
    ),

    # excited: 1 if cncexc == "Excited", 0 otherwise, NA if missing.
    excited = case_when(
      is.na(cncexc)       ~ NA_real_,
      cncexc == "Excited" ~ 1,
      TRUE                ~ 0
    )
  )

# Propagate binary DVs back to individual wave frames
pew_w119 <- pew_combined %>% filter(as.integer(wave) == 1)
pew_w132 <- pew_combined %>% filter(as.integer(wave) == 2)
pew_w152 <- pew_combined %>% filter(as.integer(wave) == 3)


# =============================================================================
# 8. Save Processed Files
# =============================================================================

message("\n--- Saving processed files ---")

saveRDS(pew_combined, proj_path("data", "processed", "pew_combined.rds"))
message("Saved: data/processed/pew_combined.rds  (",
        nrow(pew_combined), " rows)")

saveRDS(pew_w119, proj_path("data", "processed", "pew_w119.rds"))
message("Saved: data/processed/pew_w119.rds  (", nrow(pew_w119), " rows)")

saveRDS(pew_w132, proj_path("data", "processed", "pew_w132.rds"))
message("Saved: data/processed/pew_w132.rds  (", nrow(pew_w132), " rows)")

saveRDS(pew_w152, proj_path("data", "processed", "pew_w152.rds"))
message("Saved: data/processed/pew_w152.rds  (", nrow(pew_w152), " rows)")


# =============================================================================
# 9. Summary Statistics
# =============================================================================

message("\n", strrep("=", 70))
message("SUMMARY STATISTICS")
message(strrep("=", 70))

# --- 9a. Sample size per wave ------------------------------------------------
message("\n--- Sample size per wave ---")
n_per_wave <- pew_combined %>%
  count(wave) %>%
  rename(n_respondents = n)
print(n_per_wave)

# --- 9b. CNCEXC distribution per wave (unweighted) ---------------------------
message("\n--- CNCEXC distribution per wave (unweighted counts and %) ---")
cncexc_dist <- pew_combined %>%
  group_by(wave) %>%
  count(cncexc) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()
print(cncexc_dist, n = Inf)

# --- 9c. CNCEXC distribution per wave (weighted) -----------------------------
message("\n--- CNCEXC distribution per wave (weighted %) ---")
cncexc_wtd <- pew_combined %>%
  filter(!is.na(cncexc)) %>%
  group_by(wave, cncexc) %>%
  summarise(wt_n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(wave) %>%
  mutate(wt_pct = round(wt_n / sum(wt_n) * 100, 1)) %>%
  select(wave, cncexc, wt_pct) %>%
  ungroup()
print(cncexc_wtd, n = Inf)

# --- 9d. Binary DV summary ---------------------------------------------------
message("\n--- Binary DVs: % concerned and % excited by wave (unweighted) ---")
dv_summ <- pew_combined %>%
  group_by(wave) %>%
  summarise(
    n_valid       = sum(!is.na(concerned)),
    n_missing     = sum(is.na(concerned)),
    pct_concerned = round(mean(concerned, na.rm = TRUE) * 100, 1),
    pct_excited   = round(mean(excited,   na.rm = TRUE) * 100, 1),
    pct_equally   = 100 - pct_concerned - pct_excited
  )
print(dv_summ)

# --- 9e. Missing data report -------------------------------------------------
message("\n--- Missing data report (% missing per key variable, per wave) ---")

key_vars <- c("cncexc", "ai_heard", "useai", "age_cat", "gender",
              "education", "race", "party", "income_tier", "region", "metro")

missing_report <- pew_combined %>%
  group_by(wave) %>%
  summarise(
    across(
      all_of(key_vars),
      ~ round(mean(is.na(.)) * 100, 1),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(-wave, names_to = "variable", values_to = "pct_missing") %>%
  pivot_wider(names_from = wave, values_from = pct_missing)

print(missing_report, n = Inf)

# --- 9f. Survey weight sanity check ------------------------------------------
message("\n--- Survey weight summary by wave ---")
weight_check <- pew_combined %>%
  group_by(wave) %>%
  summarise(
    n            = n(),
    weight_min   = round(min(weight,  na.rm = TRUE), 4),
    weight_max   = round(max(weight,  na.rm = TRUE), 4),
    weight_mean  = round(mean(weight, na.rm = TRUE), 4),
    weight_sum   = round(sum(weight,  na.rm = TRUE), 1)
  )
print(weight_check)

# --- 9g. Party distribution check --------------------------------------------
message("\n--- Party (binary) distribution per wave (unweighted) ---")
party_dist <- pew_combined %>%
  group_by(wave) %>%
  count(party) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()
print(party_dist, n = Inf)

message("\n", strrep("=", 70))
message("Data preparation complete.")
message(strrep("=", 70), "\n")
