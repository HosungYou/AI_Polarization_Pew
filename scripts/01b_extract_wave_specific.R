# =============================================================================
# 01b_extract_wave_specific.R
# AI Attitude Polarization Study — Wave-Specific Variable Extraction
# Waves: W119 (Dec 2022), W132 (Aug 2023), W152 (Aug 2024)
# =============================================================================
# Description:
#   Loads raw SPSS files for three ATP waves, extracts wave-specific AI
#   variables not present in the harmonized combined dataset produced by
#   01_data_preparation.R, applies consistent recoding, constructs composite
#   indices, and saves enriched processed datasets alongside summary
#   diagnostics.
#
# Input:
#   data/raw/W119_Dec22/ATP W119.sav
#   data/raw/W132_Aug23/ATP W132.sav
#   data/raw/W152_Aug24/ATP W152.sav
#
# Output:
#   data/processed/pew_w119_enriched.rds
#   data/processed/pew_w132_enriched.rds
#   data/processed/pew_w152_enriched.rds
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(haven)      # Read SPSS .sav files
  library(tidyverse)  # Data wrangling and piping
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
message("All input files found.")


# =============================================================================
# 1. Shared Helpers
# =============================================================================

# Demographic variable names shared across all three waves
DEMO_VARS <- c(
  "F_PARTYSUM_FINAL", "F_PARTYSUMIDEO_FINAL",
  "F_AGECAT", "F_GENDER", "F_EDUCCAT", "F_EDUCCAT2",
  "F_RACETHNMOD", "F_INC_SDT1", "F_INC_TIER2",
  "F_CREGION", "F_METRO", "F_RELIG", "F_MARITAL"
)

# safe_pull: extract and zap_labels a variable; return NA vector if absent.
safe_pull <- function(df, var_name) {
  if (var_name %in% names(df)) {
    zap_labels(df[[var_name]])
  } else {
    rep(NA_real_, nrow(df))
  }
}

# find_vars: grep for variables matching a pattern in a data frame.
# Reports found / not-found for transparency.
find_vars <- function(df, pattern, wave_label) {
  candidates <- grep(pattern, names(df), value = TRUE)
  if (length(candidates) == 0) {
    message("  [", wave_label, "] No variables matching '", pattern, "' found — skipped.")
  } else {
    message("  [", wave_label, "] Found: ", paste(candidates, collapse = ", "))
  }
  candidates
}

# pull_vars: extract a named list of variables from df (with safe_pull each).
pull_vars <- function(df, var_names) {
  map(set_names(var_names), ~ safe_pull(df, .x))
}

# na99: replace a single NA-code value (default 99) with NA_real_.
na99 <- function(x, code = 99) {
  if_else(x == code, NA_real_, x)
}

# Recode shared demographics, used identically for all three waves.
# Input df must contain all F_* columns (already zap_labels'd).
recode_demographics <- function(df) {
  df %>%
    mutate(
      # party: binary leaned party affiliation
      # F_PARTYSUM_FINAL: 1=Rep/Lean Rep, 2=Dem/Lean Dem, 9=DK/no lean
      party = case_when(
        F_PARTYSUM_FINAL == 9 ~ NA_character_,
        F_PARTYSUM_FINAL == 1 ~ "Rep/Lean Rep",
        F_PARTYSUM_FINAL == 2 ~ "Dem/Lean Dem",
        TRUE                  ~ NA_character_
      ),
      party = factor(party, levels = c("Rep/Lean Rep", "Dem/Lean Dem")),

      # party_ideo: detailed party × ideology
      # F_PARTYSUMIDEO_FINAL: 1=Cons Rep, 2=Mod/Lib Rep,
      #                       3=Mod/Cons Dem, 4=Lib Dem; 9=DK/Ref
      party_ideo = na99(F_PARTYSUMIDEO_FINAL, 9),
      party_ideo = factor(party_ideo,
                          levels = 1:4,
                          labels = c("Cons Rep/Lean Rep",
                                     "Mod/Lib Rep/Lean Rep",
                                     "Mod/Cons Dem/Lean Dem",
                                     "Lib Dem/Lean Dem")),

      # age_cat: F_AGECAT 1=18-29, 2=30-49, 3=50-64, 4=65+; 99=DK/Ref
      age_cat = na99(F_AGECAT),
      age_cat = factor(age_cat, levels = 1:4,
                       labels = c("18-29", "30-49", "50-64", "65+")),

      # gender: F_GENDER 1=Male, 2=Female, 3=Other; 99=DK/Ref
      gender = case_when(
        F_GENDER == 99 ~ NA_character_,
        F_GENDER == 1  ~ "Male",
        F_GENDER == 2  ~ "Female",
        F_GENDER == 3  ~ "Other",
        TRUE           ~ NA_character_
      ),
      gender = factor(gender, levels = c("Male", "Female", "Other")),

      # education: F_EDUCCAT 1=College grad+, 2=Some college, 3=HS or less
      education = na99(F_EDUCCAT),
      education = factor(education, levels = 1:3,
                         labels = c("College graduate+", "Some College",
                                    "HS graduate or less")),

      # education_detail: F_EDUCCAT2 6-category
      education_detail = na99(F_EDUCCAT2),
      education_detail = factor(education_detail, levels = 1:6,
                                labels = c("Less than HS", "HS graduate",
                                           "Some college", "Associate degree",
                                           "College graduate", "Postgraduate")),

      # race: F_RACETHNMOD 1=White NH, 2=Black NH, 3=Hispanic,
      #                    4=Other NH, 5=Asian NH; 99=DK/Ref
      race = na99(F_RACETHNMOD),
      race = factor(race, levels = 1:5,
                    labels = c("White non-Hispanic", "Black non-Hispanic",
                               "Hispanic", "Other non-Hispanic",
                               "Asian non-Hispanic")),

      # income_tier: F_INC_TIER2 1=Lower, 2=Middle, 3=Upper; 99=DK/Ref
      income_tier = na99(F_INC_TIER2),
      income_tier = factor(income_tier, levels = 1:3,
                           labels = c("Lower", "Middle", "Upper")),

      # region: F_CREGION 1=Northeast, 2=Midwest, 3=South, 4=West
      region = factor(F_CREGION, levels = 1:4,
                      labels = c("Northeast", "Midwest", "South", "West")),

      # metro: F_METRO 1=Metropolitan, 2=Non-metropolitan
      metro = factor(F_METRO, levels = 1:2,
                     labels = c("Metropolitan", "Non-metropolitan"))
    ) %>%
    select(-starts_with("F_"))
}


# =============================================================================
# 2. Load Raw SPSS Data
# =============================================================================

message("\n--- Loading raw SPSS files ---")

raw_w119 <- read_sav(path_w119)
message("W119 loaded: ", nrow(raw_w119), " rows, ", ncol(raw_w119), " columns")

raw_w132 <- read_sav(path_w132)
message("W132 loaded: ", nrow(raw_w132), " rows, ", ncol(raw_w132), " columns")

raw_w152 <- read_sav(path_w152)
message("W152 loaded: ", nrow(raw_w152), " rows, ", ncol(raw_w152), " columns")


# =============================================================================
# 3. W119 — AI Knowledge, Workplace, Healthcare
# =============================================================================

message("\n", strrep("-", 70))
message("W119: Extracting wave-specific variables")
message(strrep("-", 70))

# --- 3a. Identify wave-specific variable groups ------------------------------

# AIKNOW raw responses (AIKNOW4 is absent in data — only 1,2,3,5,6,7 exist)
aiknow_raw_vars   <- find_vars(raw_w119, "^AIKNOW[0-9]_W119$",         "W119")
aiknow_corr_vars  <- find_vars(raw_w119, "^AIKNOW[0-9]_CORRECT_W119$", "W119")
desrisk_vars      <- find_vars(raw_w119, "^DESRISK_.*_W119$",           "W119")
aiwrk2_vars       <- find_vars(raw_w119, "^AIWRK2_._W119$",             "W119")
aiwrk3_vars       <- find_vars(raw_w119, "^AIWRK3_._W119$",             "W119")
aiwrkh_vars       <- find_vars(raw_w119, "^AIWRKH[0-9]+(_.|_W119)$",    "W119")
# AIWRKH1, AIWRKH4 have no sub-letter suffix; H2, H3 do
aiwrkh1_var       <- find_vars(raw_w119, "^AIWRKH1_W119$",              "W119")
aiwrkh2_vars      <- find_vars(raw_w119, "^AIWRKH2_._W119$",            "W119")
aiwrkh3_vars      <- find_vars(raw_w119, "^AIWRKH3_._W119$",            "W119")
aiwrkh4_var       <- find_vars(raw_w119, "^AIWRKH4_W119$",              "W119")
aiwrkm1_var       <- find_vars(raw_w119, "^AIWRKM1_W119$",              "W119")
aiwrkm2_vars      <- find_vars(raw_w119, "^AIWRKM2_._W119$",            "W119")
aiwrkm3_vars      <- find_vars(raw_w119, "^AIWRKM3_._W119$",            "W119")
aiwrkm4_vars      <- find_vars(raw_w119, "^AIWRKM4_._W119$",            "W119")

# --- 3b. Build demographic tibble -------------------------------------------

demo_w119 <- map_dfc(set_names(DEMO_VARS), ~ safe_pull(raw_w119, .x))

# --- 3c. Build wave-specific tibble -----------------------------------------

ws_w119 <- tibble(
  # Core identifiers
  cncexc_raw   = zap_labels(raw_w119[["CNCEXC_W119"]]),
  ai_heard_raw = zap_labels(raw_w119[["AI_HEARD_W119"]]),
  useai_raw    = safe_pull(raw_w119, "USEAI_W119"),
  weight       = zap_labels(raw_w119[["WEIGHT_W119"]]),

  # AI knowledge index (0-7 composite, already in raw data)
  AIKNOW_INDEX_W119 = zap_labels(raw_w119[["AIKNOW_INDEX_W119"]])
)

# Append AIKNOW raw response items
for (v in aiknow_raw_vars)  ws_w119[[v]] <- safe_pull(raw_w119, v)
# Append AIKNOW correctness items
for (v in aiknow_corr_vars) ws_w119[[v]] <- safe_pull(raw_w119, v)
# Append DESRISK items
for (v in desrisk_vars)     ws_w119[[v]] <- safe_pull(raw_w119, v)
# Append AIWRK2 / AIWRK3 items
for (v in aiwrk2_vars)      ws_w119[[v]] <- safe_pull(raw_w119, v)
for (v in aiwrk3_vars)      ws_w119[[v]] <- safe_pull(raw_w119, v)
# Append AIWRKH items (help)
for (v in c(aiwrkh1_var, aiwrkh2_vars, aiwrkh3_vars, aiwrkh4_var)) {
  ws_w119[[v]] <- safe_pull(raw_w119, v)
}
# Append AIWRKM items (harm)
for (v in c(aiwrkm1_var, aiwrkm2_vars, aiwrkm3_vars, aiwrkm4_vars)) {
  ws_w119[[v]] <- safe_pull(raw_w119, v)
}
# Healthcare comfort
ws_w119[["AIHCCOMF_W119"]] <- safe_pull(raw_w119, "AIHCCOMF_W119")

# Bind demographics
ws_w119 <- bind_cols(ws_w119, demo_w119)

# --- 3d. Recode W119 variables ----------------------------------------------

pew_w119_enriched <- ws_w119 %>%
  mutate(

    # -------------------------------------------------------------------------
    # Core attitudinal variables
    # -------------------------------------------------------------------------

    # cncexc: 1=Excited, 2=Concerned, 3=Equally; 99=Refused
    cncexc = na99(cncexc_raw),
    cncexc = factor(cncexc, levels = 1:3,
                    labels = c("Excited", "Concerned", "Equally")),

    # concerned: binary 1=Concerned, 0=otherwise
    concerned = case_when(
      is.na(cncexc) ~ NA_real_,
      cncexc == "Concerned" ~ 1,
      TRUE ~ 0
    ),

    # ai_heard: 1=A lot, 2=A little, 3=Nothing; 99=Refused
    ai_heard = na99(ai_heard_raw),
    ai_heard = factor(ai_heard, levels = 1:3,
                      labels = c("A lot", "A little", "Nothing")),

    # useai: 1=Almost constantly ... 5=Less often; 99=DK/Ref
    useai = case_when(
      is.na(useai_raw) ~ NA_real_,
      useai_raw == 99  ~ NA_real_,
      TRUE             ~ useai_raw
    ),
    useai = factor(useai, levels = 1:5,
                   labels = c("Almost constantly", "Several times/day",
                              "About once/day", "Several times/week",
                              "Less often")),

    # -------------------------------------------------------------------------
    # AI knowledge index: 0-7 composite (keep numeric as-is)
    # -------------------------------------------------------------------------
    ai_knowledge = na99(AIKNOW_INDEX_W119),

    # -------------------------------------------------------------------------
    # DESRISK items: 1-5 Likert (1=Extremely well ... 5=Not at all); 99=Refused
    # -------------------------------------------------------------------------
    desrisk_comf  = na99(DESRISK_COMF_W119),
    desrisk_creat = na99(DESRISK_CREAT_W119),
    desrisk_ntech = na99(DESRISK_NTECH_W119),

    # -------------------------------------------------------------------------
    # AIWRK2: Impact of AI on specific job aspects
    # 1=Major impact, 2=Minor impact, 3=No impact, 9=Not sure, 99=Refused
    # -------------------------------------------------------------------------
    across(all_of(aiwrk2_vars),
           ~ case_when(.x == 99 ~ NA_real_, .x == 9 ~ NA_real_, TRUE ~ .x),
           .names = "{.col}_rc"),

    # -------------------------------------------------------------------------
    # AIWRK3: Whether AI helps or hurts job aspects
    # 1=Help more than hurts, 2=Equally, 3=Hurt more than helps,
    # 9=Not sure, 99=Refused
    # -------------------------------------------------------------------------
    across(all_of(aiwrk3_vars),
           ~ case_when(.x == 99 ~ NA_real_, .x == 9 ~ NA_real_, TRUE ~ .x),
           .names = "{.col}_rc"),

    # -------------------------------------------------------------------------
    # AIWRKH1: How much heard about AI helping at work
    # 1=A lot, 2=A little, 3=Nothing at all, 99=Refused
    # -------------------------------------------------------------------------
    across(all_of(aiwrkh1_var),
           ~ na99(.x),
           .names = "{.col}_rc"),

    # AIWRKH2: Favor/oppose AI helping in workplace policies (a, b)
    # 1=Favor, 2=Oppose, 9=Not sure, 99=Refused
    across(all_of(aiwrkh2_vars),
           ~ case_when(.x == 99 ~ NA_real_, .x == 9 ~ NA_real_, TRUE ~ .x),
           .names = "{.col}_rc"),

    # AIWRKH3: Extent AI helps with specific work tasks (a-d)
    # (same coding as AIWRKH2: 1=Favor, 2=Oppose, 9=Not sure, 99=Refused)
    across(all_of(aiwrkh3_vars),
           ~ case_when(.x == 99 ~ NA_real_, .x == 9 ~ NA_real_, TRUE ~ .x),
           .names = "{.col}_rc"),

    # AIWRKH4: Overall AI impact on ability to do job well
    # 1=A lot, 2=A little, 3=Nothing at all, 99=Refused
    across(all_of(aiwrkh4_var),
           ~ na99(.x),
           .names = "{.col}_rc"),

    # -------------------------------------------------------------------------
    # AIWRKM1: How much heard about AI harming at work
    # 1=A lot, 2=A little, 3=Nothing at all, 99=Refused
    # -------------------------------------------------------------------------
    across(all_of(aiwrkm1_var),
           ~ na99(.x),
           .names = "{.col}_rc"),

    # AIWRKM2 / AIWRKM3: AI harm-specific items (same coding as AIWRKH2/3)
    # 1=Favor, 2=Oppose, 9=Not sure, 99=Refused
    across(all_of(c(aiwrkm2_vars, aiwrkm3_vars)),
           ~ case_when(.x == 99 ~ NA_real_, .x == 9 ~ NA_real_, TRUE ~ .x),
           .names = "{.col}_rc"),

    # AIWRKM4: Favor/oppose specific AI harm mitigation policies (a, b)
    across(all_of(aiwrkm4_vars),
           ~ case_when(.x == 99 ~ NA_real_, .x == 9 ~ NA_real_, TRUE ~ .x),
           .names = "{.col}_rc"),

    # -------------------------------------------------------------------------
    # AIHCCOMF: Comfort with AI in healthcare
    # 1=Very comfortable, 2=Somewhat comfortable,
    # 3=Somewhat uncomfortable, 4=Very uncomfortable; 99=Refused
    # -------------------------------------------------------------------------
    aihccomf = na99(AIHCCOMF_W119),
    aihccomf = factor(aihccomf, levels = 1:4,
                      labels = c("Very comfortable", "Somewhat comfortable",
                                 "Somewhat uncomfortable", "Very uncomfortable"))

  ) %>%
  # Drop raw extraction columns (keep _rc recodes and named factors above)
  select(-cncexc_raw, -ai_heard_raw, -useai_raw,
         -AIKNOW_INDEX_W119,
         -all_of(desrisk_vars),
         -all_of(aiwrk2_vars), -all_of(aiwrk3_vars),
         -all_of(c(aiwrkh1_var, aiwrkh2_vars, aiwrkh3_vars, aiwrkh4_var)),
         -all_of(c(aiwrkm1_var, aiwrkm2_vars, aiwrkm3_vars, aiwrkm4_vars)),
         -AIHCCOMF_W119) %>%
  recode_demographics()

message("W119 enriched: ", nrow(pew_w119_enriched), " rows, ",
        ncol(pew_w119_enriched), " columns")


# =============================================================================
# 4. W132 — AI Help/Hurt by Domain
# =============================================================================

message("\n", strrep("-", 70))
message("W132: Extracting wave-specific variables")
message(strrep("-", 70))

aihlphrt_vars <- find_vars(raw_w132, "^AIHLPHRT_._W132$", "W132")

demo_w132 <- map_dfc(set_names(DEMO_VARS), ~ safe_pull(raw_w132, .x))

ws_w132 <- tibble(
  cncexc_raw   = zap_labels(raw_w132[["CNCEXC_W132"]]),
  ai_heard_raw = zap_labels(raw_w132[["AI_HEARD_W132"]]),
  weight       = zap_labels(raw_w132[["WEIGHT_W132"]])
)

for (v in aihlphrt_vars) ws_w132[[v]] <- safe_pull(raw_w132, v)

ws_w132 <- bind_cols(ws_w132, demo_w132)

# Recode W132
pew_w132_enriched <- ws_w132 %>%
  mutate(

    # cncexc: 1=Excited, 2=Concerned, 3=Equally; 99=Refused
    cncexc = na99(cncexc_raw),
    cncexc = factor(cncexc, levels = 1:3,
                    labels = c("Excited", "Concerned", "Equally")),

    concerned = case_when(
      is.na(cncexc) ~ NA_real_,
      cncexc == "Concerned" ~ 1,
      TRUE ~ 0
    ),

    # ai_heard: 1=A lot, 2=A little, 3=Nothing; 99=Refused
    ai_heard = na99(ai_heard_raw),
    ai_heard = factor(ai_heard, levels = 1:3,
                      labels = c("A lot", "A little", "Nothing")),

    # AIHLPHRT items: 1=Helps more, 2=Hurts more, 3=Not sure, 99=Refused
    # Recode 99 -> NA; 3 (Not sure) kept as valid category
    across(all_of(aihlphrt_vars),
           ~ case_when(.x == 99 ~ NA_real_, TRUE ~ .x),
           .names = "{.col}_rc"),

    # hurt_more_* binary flags: 1 if hurts more, else 0
    across(all_of(aihlphrt_vars),
           ~ case_when(
               .x == 99 ~ NA_real_,
               .x == 2  ~ 1,
               TRUE      ~ 0
             ),
           .names = "hurt_more_{.col}")

  ) %>%
  # Rename hurt_more_ columns to drop the AIHLPHRT prefix for readability
  rename_with(~ str_replace(.x, "hurt_more_AIHLPHRT_", "hurt_more_"),
              starts_with("hurt_more_AIHLPHRT_")) %>%
  select(-cncexc_raw, -ai_heard_raw, -all_of(aihlphrt_vars)) %>%
  recode_demographics()

message("W132 enriched: ", nrow(pew_w132_enriched), " rows, ",
        ncol(pew_w132_enriched), " columns")


# =============================================================================
# 5. W152 — Concerns, Job Impact, Human vs AI, Regulation, Future
# =============================================================================

message("\n", strrep("-", 70))
message("W152: Extracting wave-specific variables")
message(strrep("-", 70))

# Identify variable groups
aiconcern_vars  <- find_vars(raw_w152, "^AICONCERN_._W152$",  "W152")
aijobimpct_vars <- find_vars(raw_w152, "^AIJOBIMPCT_._W152$", "W152")
humanvai_vars   <- find_vars(raw_w152, "^HUMANVAI_._W152$",   "W152")
futrai_vars     <- find_vars(raw_w152, "^FUTRAI_._W152$",     "W152")

# AIJOBS: check explicitly
if ("AIJOBS_W152" %in% names(raw_w152)) {
  message("  [W152] Found: AIJOBS_W152")
} else {
  message("  [W152] AIJOBS_W152 not found — will fill with NA")
}

demo_w152 <- map_dfc(set_names(DEMO_VARS), ~ safe_pull(raw_w152, .x))

ws_w152 <- tibble(
  cncexc_raw   = zap_labels(raw_w152[["CNCEXC_W152"]]),
  ai_heard_raw = zap_labels(raw_w152[["AI_HEARD_W152"]]),
  useai_raw    = safe_pull(raw_w152, "USEAI_W152"),
  weight       = zap_labels(raw_w152[["WEIGHT_W152"]]),

  # Split-form indicator (CRITICAL for AIJOBIMPCT / AIFUTRIMPCT)
  # 1 = Form 1, 2 = Form 2
  form         = safe_pull(raw_w152, "FORM_W152"),

  # Regulation preference
  AIREG_W152        = safe_pull(raw_w152, "AIREG_W152"),

  # Control perceptions
  AICONTROL1_W152   = safe_pull(raw_w152, "AICONTROL1_W152"),
  AICONTROL2_W152   = safe_pull(raw_w152, "AICONTROL2_W152"),

  # Trust AI with personal info
  TRSTAIPRS_W152    = safe_pull(raw_w152, "TRSTAIPRS_W152"),

  # Overall AI jobs impact
  AIJOBS_W152       = safe_pull(raw_w152, "AIJOBS_W152")
)

# Append multi-item batteries
for (v in aiconcern_vars)  ws_w152[[v]] <- safe_pull(raw_w152, v)
for (v in aijobimpct_vars) ws_w152[[v]] <- safe_pull(raw_w152, v)
for (v in humanvai_vars)   ws_w152[[v]] <- safe_pull(raw_w152, v)
for (v in futrai_vars)     ws_w152[[v]] <- safe_pull(raw_w152, v)

ws_w152 <- bind_cols(ws_w152, demo_w152)

# Recode W152
pew_w152_enriched <- ws_w152 %>%
  mutate(

    # -------------------------------------------------------------------------
    # Core attitudinal variables
    # -------------------------------------------------------------------------

    # cncexc: 1=Excited, 2=Concerned, 3=Equally; 99=Refused
    cncexc = na99(cncexc_raw),
    cncexc = factor(cncexc, levels = 1:3,
                    labels = c("Excited", "Concerned", "Equally")),

    concerned = case_when(
      is.na(cncexc) ~ NA_real_,
      cncexc == "Concerned" ~ 1,
      TRUE ~ 0
    ),

    # ai_heard: 1=A lot, 2=A little, 3=Nothing; 99=Refused
    ai_heard = na99(ai_heard_raw),
    ai_heard = factor(ai_heard, levels = 1:3,
                      labels = c("A lot", "A little", "Nothing")),

    # useai: 1=Almost constantly ... 5=Less often; 99=DK/Ref
    useai = case_when(
      is.na(useai_raw) ~ NA_real_,
      useai_raw == 99  ~ NA_real_,
      TRUE             ~ useai_raw
    ),
    useai = factor(useai, levels = 1:5,
                   labels = c("Almost constantly", "Several times/day",
                              "About once/day", "Several times/week",
                              "Less often")),

    # -------------------------------------------------------------------------
    # AICONCERN items: 1=Extremely concerned ... 5=Not at all concerned; 99=NA
    # Keep as ordinal numeric (lower = more concerned)
    # -------------------------------------------------------------------------
    across(all_of(aiconcern_vars),
           ~ na99(.x),
           .names = "{.col}_rc"),

    # concern_intensity: computed in a second mutate pass below (needs _rc cols)

    # -------------------------------------------------------------------------
    # AIREG: Regulation preference
    # 1=Too far, 2=Not far enough, 3=Not sure, 99=Refused
    # -------------------------------------------------------------------------
    aireg = case_when(
      AIREG_W152 == 99 ~ NA_real_,
      TRUE             ~ AIREG_W152
    ),
    aireg = factor(aireg, levels = 1:3,
                   labels = c("Too far", "Not far enough", "Not sure")),

    # reg_support: 1 if want more regulation (Not far enough), else 0
    reg_support = case_when(
      is.na(aireg)             ~ NA_real_,
      aireg == "Not far enough" ~ 1,
      TRUE                     ~ 0
    ),

    # -------------------------------------------------------------------------
    # AICONTROL1: Amount of control over AI use in life
    # 1=A great deal, 2=Quite a bit, 3=Some, 4=Not too much, 5=None; 99=DK/Ref
    # -------------------------------------------------------------------------
    aicontrol1 = na99(AICONTROL1_W152),

    # -------------------------------------------------------------------------
    # AICONTROL2: Satisfaction with level of control (categorical)
    # 1=Comfortable with current control, 2=Want more control, 3=Not sure
    # 99=Refused
    # -------------------------------------------------------------------------
    aicontrol2 = case_when(
      AICONTROL2_W152 == 99 ~ NA_real_,
      TRUE                  ~ AICONTROL2_W152
    ),
    aicontrol2 = factor(aicontrol2, levels = 1:3,
                        labels = c("Comfortable with current",
                                   "Want more control",
                                   "Not sure")),

    # -------------------------------------------------------------------------
    # TRSTAIPRS: Trust AI for personal information
    # 1=Yes will, 2=No will not, 3=Not sure, 99=Refused
    # -------------------------------------------------------------------------
    trstaiprs = case_when(
      TRSTAIPRS_W152 == 99 ~ NA_real_,
      TRUE                 ~ TRSTAIPRS_W152
    ),
    trstaiprs = factor(trstaiprs, levels = 1:3,
                       labels = c("Yes, will", "No, will not", "Not sure")),

    # -------------------------------------------------------------------------
    # AIJOBS: Overall AI impact on jobs
    # 1=More jobs, 2=Fewer jobs, 3=No much diff, 4=Not sure, 99=Refused
    # -------------------------------------------------------------------------
    aijobs = case_when(
      AIJOBS_W152 == 99 ~ NA_real_,
      TRUE              ~ AIJOBS_W152
    ),
    aijobs = factor(aijobs, levels = 1:4,
                    labels = c("More jobs", "Fewer jobs",
                               "Not much difference", "Not sure")),

    # -------------------------------------------------------------------------
    # AIJOBIMPCT items: occupation-specific job impact
    # 1=More jobs, 2=Fewer jobs, 3=Not much difference, 4=Not sure, 99=Refused
    # NOTE: Only asked to Form 1 or Form 2 respondents (check X_FORM_W152/FORM_W152)
    # -------------------------------------------------------------------------
    across(all_of(aijobimpct_vars),
           ~ case_when(.x == 99 ~ NA_real_, TRUE ~ .x),
           .names = "{.col}_rc"),

    # job_threat_index: computed in a second mutate pass below (needs _rc cols)

    # -------------------------------------------------------------------------
    # HUMANVAI items: human vs AI performance comparison
    # 1=AI better, 2=AI worse, 3=About same, 4=Not sure, 99=Refused
    # -------------------------------------------------------------------------
    across(all_of(humanvai_vars),
           ~ case_when(.x == 99 ~ NA_real_, TRUE ~ .x),
           .names = "{.col}_rc"),

    # -------------------------------------------------------------------------
    # FUTRAI items: future AI expectations (likelihood scale)
    # 1=Extremely likely, 2=Very likely, 3=Somewhat likely,
    # 4=Not too likely, 5=Not at all likely, 6=Not sure, 99=Refused
    # -------------------------------------------------------------------------
    across(all_of(futrai_vars),
           ~ case_when(.x == 99 ~ NA_real_, TRUE ~ .x),
           .names = "{.col}_rc")

  ) %>%
  select(-cncexc_raw, -ai_heard_raw, -useai_raw,
         -AIREG_W152, -AICONTROL1_W152, -AICONTROL2_W152,
         -TRSTAIPRS_W152, -AIJOBS_W152,
         -all_of(aiconcern_vars),
         -all_of(aijobimpct_vars),
         -all_of(humanvai_vars),
         -all_of(futrai_vars)) %>%
  recode_demographics() %>%
  # --- Second pass: composite indices that depend on _rc columns -------------
  mutate(
    # concern_intensity: row mean of reverse-coded AICONCERN items.
    # Original: 1=Extremely concerned, 5=Not at all concerned.
    # After 6 - x:  1 -> 5  (extremely concerned -> high score = more concerned).
    # Result: higher score = more concerned.
    concern_intensity = {
      rc_cols <- paste0(aiconcern_vars, "_rc")
      rc_present <- intersect(rc_cols, names(.))
      if (length(rc_present) == 0) {
        NA_real_
      } else {
        rowMeans(6 - select(., all_of(rc_present)), na.rm = TRUE)
      }
    },

    # job_threat_index: proportion of AIJOBIMPCT _rc items coded 2 ("Fewer jobs").
    # Respondents who were not asked (all NA due to form split) get NA_real_.
    job_threat_index = {
      rc_cols   <- paste0(aijobimpct_vars, "_rc")
      rc_present <- intersect(rc_cols, names(.))
      if (length(rc_present) == 0) {
        NA_real_
      } else {
        fewer_mat <- select(., all_of(rc_present))
        binary    <- (fewer_mat == 2) * 1L          # 1 if "Fewer jobs", 0 otherwise
        binary[is.na(fewer_mat)] <- NA_real_         # preserve NAs
        # If all items for a row are NA (not asked), result is NA; else row mean
        ifelse(rowSums(!is.na(binary)) == 0,
               NA_real_,
               rowMeans(binary, na.rm = TRUE))
      }
    }
  )

message("W152 enriched: ", nrow(pew_w152_enriched), " rows, ",
        ncol(pew_w152_enriched), " columns")


# =============================================================================
# 6. Save Enriched Datasets
# =============================================================================

message("\n--- Saving enriched datasets ---")

out_w119 <- proj_path("data", "processed", "pew_w119_enriched.rds")
out_w132 <- proj_path("data", "processed", "pew_w132_enriched.rds")
out_w152 <- proj_path("data", "processed", "pew_w152_enriched.rds")

saveRDS(pew_w119_enriched, out_w119)
message("Saved: data/processed/pew_w119_enriched.rds  (", nrow(pew_w119_enriched), " rows)")

saveRDS(pew_w132_enriched, out_w132)
message("Saved: data/processed/pew_w132_enriched.rds  (", nrow(pew_w132_enriched), " rows)")

saveRDS(pew_w152_enriched, out_w152)
message("Saved: data/processed/pew_w152_enriched.rds  (", nrow(pew_w152_enriched), " rows)")


# =============================================================================
# 7. Summary Statistics
# =============================================================================

message("\n", strrep("=", 70))
message("SUMMARY STATISTICS")
message(strrep("=", 70))


# --- Helper: % missing for a vector -----------------------------------------
pct_miss <- function(x) round(mean(is.na(x)) * 100, 1)


# =============================================================================
# 7a. W119 Summary
# =============================================================================

message("\n", strrep("-", 40))
message("W119 Summary")
message(strrep("-", 40))

message("\nDimensions: ", nrow(pew_w119_enriched), " rows x ",
        ncol(pew_w119_enriched), " columns")

message("\n% missing on key wave-specific variables:")
w119_key_vars <- c("ai_knowledge",
                   "desrisk_comf", "desrisk_creat", "desrisk_ntech",
                   "aihccomf",
                   "AIWRKH1_W119_rc", "AIWRKM1_W119_rc",
                   paste0(aiknow_raw_vars, collapse = NULL))

# Build missing table for W119
w119_miss_df <- tibble(
  variable = c("ai_knowledge", "desrisk_comf", "desrisk_creat", "desrisk_ntech",
               "aihccomf", "AIWRKH1_W119_rc", "AIWRKM1_W119_rc"),
  pct_missing = map_dbl(
    c("ai_knowledge", "desrisk_comf", "desrisk_creat", "desrisk_ntech",
      "aihccomf", "AIWRKH1_W119_rc", "AIWRKM1_W119_rc"),
    ~ if (.x %in% names(pew_w119_enriched)) pct_miss(pew_w119_enriched[[.x]]) else NA_real_
  )
)
# Add AIKNOW raw items
aiknow_rc_in_data <- intersect(aiknow_raw_vars, names(pew_w119_enriched))
if (length(aiknow_rc_in_data) > 0) {
  w119_miss_df <- bind_rows(
    w119_miss_df,
    tibble(variable    = aiknow_rc_in_data,
           pct_missing = map_dbl(aiknow_rc_in_data,
                                 ~ pct_miss(pew_w119_enriched[[.x]])))
  )
}
print(w119_miss_df, n = Inf)

message("\nParty distribution (W119):")
print(count(pew_w119_enriched, party) %>%
        mutate(pct = round(n / sum(n) * 100, 1)),
      n = Inf)

message("\nCross-tab: party x concerned (W119, unweighted counts):")
print(with(pew_w119_enriched, table(party, concerned, useNA = "ifany")))


# =============================================================================
# 7b. W132 Summary
# =============================================================================

message("\n", strrep("-", 40))
message("W132 Summary")
message(strrep("-", 40))

message("\nDimensions: ", nrow(pew_w132_enriched), " rows x ",
        ncol(pew_w132_enriched), " columns")

message("\n% missing on key wave-specific variables (AIHLPHRT items):")
aihlphrt_rc_in_data <- paste0(aihlphrt_vars, "_rc")
aihlphrt_rc_present <- intersect(aihlphrt_rc_in_data, names(pew_w132_enriched))

w132_miss_df <- tibble(
  variable    = aihlphrt_rc_present,
  pct_missing = map_dbl(aihlphrt_rc_present,
                        ~ pct_miss(pew_w132_enriched[[.x]]))
)
print(w132_miss_df, n = Inf)

message("\nParty distribution (W132):")
print(count(pew_w132_enriched, party) %>%
        mutate(pct = round(n / sum(n) * 100, 1)),
      n = Inf)

message("\nCross-tab: party x concerned (W132, unweighted counts):")
print(with(pew_w132_enriched, table(party, concerned, useNA = "ifany")))

message("\nhurt_more distribution (AIHLPHRT_a: Social media):")
if ("hurt_more_a_W132" %in% names(pew_w132_enriched)) {
  print(table(pew_w132_enriched[["hurt_more_a_W132"]], useNA = "ifany"))
}


# =============================================================================
# 7c. W152 Summary
# =============================================================================

message("\n", strrep("-", 40))
message("W152 Summary")
message(strrep("-", 40))

message("\nDimensions: ", nrow(pew_w152_enriched), " rows x ",
        ncol(pew_w152_enriched), " columns")

message("\n% missing on key wave-specific variables:")
w152_core_vars <- c("cncexc", "ai_heard", "useai",
                    "aireg", "reg_support",
                    "aicontrol1", "aicontrol2",
                    "trstaiprs", "aijobs",
                    "concern_intensity", "job_threat_index")
w152_core_present <- intersect(w152_core_vars, names(pew_w152_enriched))

w152_miss_df <- tibble(
  variable    = w152_core_present,
  pct_missing = map_dbl(w152_core_present,
                        ~ pct_miss(pew_w152_enriched[[.x]]))
)

# Add AICONCERN items
aiconcern_rc <- paste0(aiconcern_vars, "_rc")
aiconcern_rc_present <- intersect(aiconcern_rc, names(pew_w152_enriched))
if (length(aiconcern_rc_present) > 0) {
  w152_miss_df <- bind_rows(
    w152_miss_df,
    tibble(variable    = aiconcern_rc_present,
           pct_missing = map_dbl(aiconcern_rc_present,
                                 ~ pct_miss(pew_w152_enriched[[.x]])))
  )
}

print(w152_miss_df, n = Inf)

message("\nForm split distribution (W152):")
print(table(pew_w152_enriched[["form"]], useNA = "ifany"))

message("\nParty distribution (W152):")
print(count(pew_w152_enriched, party) %>%
        mutate(pct = round(n / sum(n) * 100, 1)),
      n = Inf)

message("\nCross-tab: party x concerned (W152, unweighted counts):")
print(with(pew_w152_enriched, table(party, concerned, useNA = "ifany")))

message("\nAIREG distribution (W152):")
print(table(pew_w152_enriched[["aireg"]], useNA = "ifany"))

message("\nConcern intensity summary (W152, higher = more concerned):")
print(summary(pew_w152_enriched[["concern_intensity"]]))

message("\nJob threat index summary (W152, proportion saying fewer jobs):")
print(summary(pew_w152_enriched[["job_threat_index"]]))

message("\n", strrep("=", 70))
message("01b_extract_wave_specific.R complete.")
message(strrep("=", 70), "\n")
