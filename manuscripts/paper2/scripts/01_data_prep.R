# =============================================================================
# Script:  01_data_prep.R
# Project: Paper 2 — The AI Divide (Latent Class Analysis)
# Purpose: Load W132 data, apply split-sample logic, recode variables for
#          poLCA, and save analysis-ready datasets for Form A and Form B.
#
# Inputs:
#   - data/processed/pew_w132_enriched.rds   (preferred)
#   - data/raw/W132_Aug23/ATP W132.sav        (fallback)
#
# Outputs:
#   - manuscripts/paper2/output/paper2_w132_full.rds   Full sample + form flag
#   - manuscripts/paper2/output/paper2_form_a.rds       Form A LCA-ready subset
#   - manuscripts/paper2/output/paper2_form_b.rds       Form B LCA-ready subset
#
# Split-sample note:
#   Form A (~5,590): domains a-d (accurate info, health care, vehicle safety,
#                    customer service)
#   Form B (~5,549): domains e-h (product search, privacy, policing, doctor
#                    quality)
#   Each respondent answered ONLY their 4 domain items; the other 4 are
#   structurally NA and must NOT be treated as missing data.
#
# Author:  [Author]
# Date:    2026-02-23
# =============================================================================

library(tidyverse)

# Helper: not-in operator
`%nin%` <- Negate(`%in%`)

# --------------------------------------------------------------------------- #
# 0. Paths                                                                     #
# --------------------------------------------------------------------------- #

PROJECT_ROOT  <- "/Users/hosung/AI_Polarization_Pew"
ENRICHED_RDS  <- file.path(PROJECT_ROOT, "data/processed/pew_w132_enriched.rds")
RAW_SPSS      <- file.path(PROJECT_ROOT, "data/raw/W132_Aug23/ATP W132.sav")
OUT_DIR       <- file.path(PROJECT_ROOT, "manuscripts/paper2/output")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# --------------------------------------------------------------------------- #
# 1. Load data — enriched RDS preferred, raw SPSS fallback                    #
# --------------------------------------------------------------------------- #

message("\n=== Step 1: Loading data ===")

use_enriched <- file.exists(ENRICHED_RDS)

if (use_enriched) {
  message("  Loading enriched RDS: ", ENRICHED_RDS)
  raw <- readRDS(ENRICHED_RDS)
  message("  Loaded: ", nrow(raw), " rows x ", ncol(raw), " columns")
  data_source <- "enriched"
} else if (file.exists(RAW_SPSS)) {
  message("  Enriched RDS not found. Loading raw SPSS: ", RAW_SPSS)
  if (!requireNamespace("haven", quietly = TRUE)) stop("Package 'haven' required for SPSS import.")
  raw <- haven::read_sav(RAW_SPSS)
  message("  Loaded: ", nrow(raw), " rows x ", ncol(raw), " columns")
  data_source <- "spss"
} else {
  stop("Neither enriched RDS nor raw SPSS file found. Check paths.")
}

# --------------------------------------------------------------------------- #
# 2. Standardise variable names                                                #
#    Enriched RDS already has clean names; raw SPSS needs renaming.           #
# --------------------------------------------------------------------------- #

message("\n=== Step 2: Standardising variable names ===")

if (data_source == "spss") {
  message("  Renaming SPSS variables to analysis names ...")

  # Domain items: recode 99 (refused) → NA first, then keep 1/2/3 scale
  # SPSS: 1=help more, 2=hurt more, 3=equal/not sure, 99=refused
  spss_to_clean <- function(x) {
    x <- haven::zap_labels(as.numeric(x))
    x[x == 99] <- NA
    x
  }

  raw <- raw %>%
    rename(
      weight      = WEIGHT_W132,
      cncexc_raw  = CNCEXC_W132,
      heard_raw   = AI_HEARD_W132,
      educ_raw    = F_EDUCCAT,
      race_raw    = F_RACETHNMOD,
      party_raw   = F_PARTYSUM_FINAL,
      income_raw  = F_INC_TIER2,
      age_raw     = F_AGECAT,
      gender_raw  = F_GENDER
    ) %>%
    rename_with(~ str_replace(., "^AIHLPHRT_([a-h])_W132$", "dom_\\1_raw"),
                matches("^AIHLPHRT_[a-h]_W132$")) %>%
    # Recode domain items
    mutate(across(matches("^dom_[a-h]_raw$"), spss_to_clean)) %>%
    # --- cncexc factor ---
    mutate(
      cncexc = case_when(
        haven::zap_labels(as.integer(cncexc_raw)) == 1 ~ "Excited",
        haven::zap_labels(as.integer(cncexc_raw)) == 2 ~ "Concerned",
        haven::zap_labels(as.integer(cncexc_raw)) == 3 ~ "Equally",
        TRUE ~ NA_character_
      ),
      cncexc = factor(cncexc, levels = c("Excited", "Concerned", "Equally")),
      # --- ai_heard factor ---
      ai_heard = case_when(
        haven::zap_labels(as.integer(heard_raw)) == 1 ~ "A lot",
        haven::zap_labels(as.integer(heard_raw)) == 2 ~ "A little",
        haven::zap_labels(as.integer(heard_raw)) == 3 ~ "Nothing",
        TRUE ~ NA_character_
      ),
      ai_heard = factor(ai_heard, levels = c("A lot", "A little", "Nothing")),
      # --- demographics ---
      education = case_when(
        haven::zap_labels(as.integer(educ_raw)) == 1 ~ "College graduate+",
        haven::zap_labels(as.integer(educ_raw)) == 2 ~ "Some College",
        haven::zap_labels(as.integer(educ_raw)) == 3 ~ "HS graduate or less",
        TRUE ~ NA_character_
      ),
      education = factor(education,
                         levels = c("College graduate+", "Some College",
                                    "HS graduate or less")),
      race = case_when(
        haven::zap_labels(as.integer(race_raw)) == 1 ~ "White non-Hispanic",
        haven::zap_labels(as.integer(race_raw)) == 2 ~ "Black non-Hispanic",
        haven::zap_labels(as.integer(race_raw)) == 3 ~ "Hispanic",
        haven::zap_labels(as.integer(race_raw)) == 4 ~ "Asian non-Hispanic",
        haven::zap_labels(as.integer(race_raw)) == 5 ~ "Other non-Hispanic",
        TRUE ~ NA_character_
      ),
      race = factor(race,
                    levels = c("White non-Hispanic", "Black non-Hispanic",
                               "Hispanic", "Asian non-Hispanic",
                               "Other non-Hispanic")),
      party = case_when(
        haven::zap_labels(as.integer(party_raw)) %in% c(1, 2) ~ "Rep/Lean Rep",
        haven::zap_labels(as.integer(party_raw)) %in% c(4, 5) ~ "Dem/Lean Dem",
        TRUE ~ NA_character_  # pure independents and DK → NA
      ),
      party = factor(party, levels = c("Rep/Lean Rep", "Dem/Lean Dem")),
      income_tier = case_when(
        haven::zap_labels(as.integer(income_raw)) == 1 ~ "Lower",
        haven::zap_labels(as.integer(income_raw)) == 2 ~ "Middle",
        haven::zap_labels(as.integer(income_raw)) == 3 ~ "Upper",
        TRUE ~ NA_character_
      ),
      income_tier = factor(income_tier, levels = c("Lower", "Middle", "Upper")),
      age_cat = case_when(
        haven::zap_labels(as.integer(age_raw)) == 1 ~ "18-29",
        haven::zap_labels(as.integer(age_raw)) == 2 ~ "30-49",
        haven::zap_labels(as.integer(age_raw)) == 3 ~ "50-64",
        haven::zap_labels(as.integer(age_raw)) == 4 ~ "65+",
        TRUE ~ NA_character_
      ),
      age_cat = factor(age_cat, levels = c("18-29", "30-49", "50-64", "65+")),
      gender = case_when(
        haven::zap_labels(as.integer(gender_raw)) == 1 ~ "Male",
        haven::zap_labels(as.integer(gender_raw)) == 2 ~ "Female",
        haven::zap_labels(as.integer(gender_raw)) == 3 ~ "Other",
        TRUE ~ NA_character_
      ),
      gender = factor(gender, levels = c("Male", "Female", "Other"))
    ) %>%
    # Rename recoded domain items to match enriched naming convention
    rename_with(~ str_replace(., "^dom_([a-h])_raw$", "AIHLPHRT_\\1_W132_rc"),
                matches("^dom_[a-h]_raw$"))

  message("  SPSS renaming and recoding complete.")

} else {
  # Enriched RDS: standardise education level label if needed
  if ("HS graduate or less" %nin% levels(raw$education)) {
    # Handle possible label variant "HS or less"
    levels(raw$education) <- str_replace(levels(raw$education),
                                         "^HS or less$",
                                         "HS graduate or less")
  }
  message("  Enriched RDS variables already in standard format.")
}

# Helper not-in operator (used above; define before first use if SPSS path)
`%nin%` <- Negate(`%in%`)

# --------------------------------------------------------------------------- #
# 3. Identify form membership                                                  #
#    Form A: has at least one non-NA among items a-d                          #
#    Form B: has at least one non-NA among items e-h                          #
# --------------------------------------------------------------------------- #

message("\n=== Step 3: Identifying form membership ===")

dom_a_vars <- paste0("AIHLPHRT_", letters[1:4], "_W132_rc")
dom_b_vars <- paste0("AIHLPHRT_", letters[5:8], "_W132_rc")

raw <- raw %>%
  mutate(
    n_valid_a = rowSums(!is.na(across(all_of(dom_a_vars)))),
    n_valid_b = rowSums(!is.na(across(all_of(dom_b_vars)))),
    form = case_when(
      n_valid_a >= 1 & n_valid_b == 0 ~ "A",
      n_valid_b >= 1 & n_valid_a == 0 ~ "B",
      n_valid_a >= 1 & n_valid_b >= 1 ~ "both",   # flag for inspection
      TRUE                             ~ "neither" # flag for inspection
    )
  )

message("  Form assignment (pre-filter):")
print(table(raw$form, useNA = "always"))

# Sanity check: "both" and "neither" should be near zero
n_both    <- sum(raw$form == "both",    na.rm = TRUE)
n_neither <- sum(raw$form == "neither", na.rm = TRUE)
if (n_both > 0)    warning("  ", n_both,    " respondents have valid data on BOTH forms — review.")
if (n_neither > 0) message("  ", n_neither, " respondents have no valid domain data (will be excluded).")

# --------------------------------------------------------------------------- #
# 4. Create LCA indicator variables                                            #
#    poLCA requires integer indicators starting at 1 (no 0s, no NAs in the   #
#    variables passed to the formula — missingness is handled by subsetting). #
# --------------------------------------------------------------------------- #

message("\n=== Step 4: Creating LCA indicator variables ===")

# 4a. Attitude (cncexc): 1=Excited, 2=Concerned, 3=Equally
# 4b. Awareness (ai_heard): 1=A lot, 2=A little, 3=Nothing
# 4c. Domain "hurt" binary: 1=not hurt (help or equal/not sure), 2=hurt
#     Source: AIHLPHRT_*_W132_rc where 1=help, 2=hurt, 3=equal/not sure

make_hurt_binary <- function(x) {
  # 1 (help) or 3 (equal) → 1 (not hurt)
  # 2 (hurt)              → 2 (hurt)
  # NA stays NA
  case_when(
    x == 2L ~ 2L,
    x %in% c(1L, 3L) ~ 1L,
    TRUE ~ NA_integer_
  )
}

raw <- raw %>%
  mutate(
    # Attitude — map factor levels to integers
    lca_attitude = case_when(
      cncexc == "Excited"   ~ 1L,
      cncexc == "Concerned" ~ 2L,
      cncexc == "Equally"   ~ 3L,
      TRUE ~ NA_integer_
    ),
    # Awareness
    lca_awareness = case_when(
      ai_heard == "A lot"    ~ 1L,
      ai_heard == "A little" ~ 2L,
      ai_heard == "Nothing"  ~ 3L,
      TRUE ~ NA_integer_
    ),
    # Domain hurt-binary indicators (Form A: a-d; Form B: e-h)
    lca_dom_a = make_hurt_binary(AIHLPHRT_a_W132_rc),
    lca_dom_b = make_hurt_binary(AIHLPHRT_b_W132_rc),
    lca_dom_c = make_hurt_binary(AIHLPHRT_c_W132_rc),
    lca_dom_d = make_hurt_binary(AIHLPHRT_d_W132_rc),
    lca_dom_e = make_hurt_binary(AIHLPHRT_e_W132_rc),
    lca_dom_f = make_hurt_binary(AIHLPHRT_f_W132_rc),
    lca_dom_g = make_hurt_binary(AIHLPHRT_g_W132_rc),
    lca_dom_h = make_hurt_binary(AIHLPHRT_h_W132_rc)
  )

message("  LCA attitude distribution:")
print(table(raw$lca_attitude, useNA = "always"))
message("  LCA awareness distribution:")
print(table(raw$lca_awareness, useNA = "always"))
message("  LCA domain-a hurt distribution (Form A respondents only):")
print(table(raw$lca_dom_a[raw$form == "A"], useNA = "always"))
message("  LCA domain-e hurt distribution (Form B respondents only):")
print(table(raw$lca_dom_e[raw$form == "B"], useNA = "always"))

# --------------------------------------------------------------------------- #
# 5. Derived variables                                                         #
# --------------------------------------------------------------------------- #

message("\n=== Step 5: Creating derived variables ===")

raw <- raw %>%
  mutate(
    # SES composite: education × income ordered cross
    # Both variables are ordered low→high; composite = average ordinal rank
    educ_num = case_when(
      education == "HS graduate or less"  ~ 1L,
      education == "Some College"         ~ 2L,
      education == "College graduate+"    ~ 3L,
      TRUE ~ NA_integer_
    ),
    income_num = case_when(
      income_tier == "Lower"  ~ 1L,
      income_tier == "Middle" ~ 2L,
      income_tier == "Upper"  ~ 3L,
      TRUE ~ NA_integer_
    ),
    ses_composite = (educ_num + income_num) / 2,

    # hurt_count: number of "hurt" responses on the 4 domain items answered
    hurt_count = case_when(
      form == "A" ~ as.integer(
        (lca_dom_a == 2L) + (lca_dom_b == 2L) +
        (lca_dom_c == 2L) + (lca_dom_d == 2L)
      ),
      form == "B" ~ as.integer(
        (lca_dom_e == 2L) + (lca_dom_f == 2L) +
        (lca_dom_g == 2L) + (lca_dom_h == 2L)
      ),
      TRUE ~ NA_integer_
    ),

    # domain_differentiation: 0 if all 4 answers identical, 1 if any differ
    domain_differentiation = case_when(
      form == "A" ~ {
        vals <- cbind(lca_dom_a, lca_dom_b, lca_dom_c, lca_dom_d)
        as.integer(apply(vals, 1, function(r) {
          r_valid <- r[!is.na(r)]
          if (length(r_valid) < 2) NA_integer_ else as.integer(length(unique(r_valid)) > 1L)
        }))
      },
      form == "B" ~ {
        vals <- cbind(lca_dom_e, lca_dom_f, lca_dom_g, lca_dom_h)
        as.integer(apply(vals, 1, function(r) {
          r_valid <- r[!is.na(r)]
          if (length(r_valid) < 2) NA_integer_ else as.integer(length(unique(r_valid)) > 1L)
        }))
      },
      TRUE ~ NA_integer_
    )
  )

message("  hurt_count distribution (by form):")
print(table(raw$hurt_count, raw$form, useNA = "always"))
message("  domain_differentiation distribution (by form):")
print(table(raw$domain_differentiation, raw$form, useNA = "always"))

# --------------------------------------------------------------------------- #
# 6. Filter to analysis sample                                                 #
#    Keep: valid cncexc, valid ai_heard, valid party (partisan leaners),      #
#          form is A or B, and at least 3 valid domain items on their form    #
# --------------------------------------------------------------------------- #

message("\n=== Step 6: Applying analysis filters ===")

message("  N before filters: ", nrow(raw))

# Count valid domain items per respondent on their assigned form
raw <- raw %>%
  mutate(
    n_valid_dom = case_when(
      form == "A" ~ n_valid_a,
      form == "B" ~ n_valid_b,
      TRUE        ~ 0L
    )
  )

analysis <- raw %>%
  filter(
    !is.na(lca_attitude),               # valid cncexc
    !is.na(lca_awareness),              # valid ai_heard
    !is.na(party),                      # partisan leaners only
    form %in% c("A", "B"),             # valid form assignment
    n_valid_dom >= 3L                   # at least 3 domain items answered
  )

message("  N after filters: ", nrow(analysis))
message("  N by form after filters:")
print(table(analysis$form))

# --------------------------------------------------------------------------- #
# 7. Subset by form                                                            #
# --------------------------------------------------------------------------- #

message("\n=== Step 7: Subsetting by form ===")

form_a <- analysis %>%
  filter(form == "A") %>%
  select(
    # Survey weights
    weight,
    # LCA indicators
    lca_attitude, lca_awareness,
    lca_dom_a, lca_dom_b, lca_dom_c, lca_dom_d,
    # Derived
    form, hurt_count, domain_differentiation, ses_composite,
    # Demographics
    education, race, party, income_tier, age_cat, gender,
    # Original recoded domains (for reference)
    AIHLPHRT_a_W132_rc, AIHLPHRT_b_W132_rc,
    AIHLPHRT_c_W132_rc, AIHLPHRT_d_W132_rc,
    # Original attitude / awareness
    cncexc, ai_heard
  )

form_b <- analysis %>%
  filter(form == "B") %>%
  select(
    weight,
    lca_attitude, lca_awareness,
    lca_dom_e, lca_dom_f, lca_dom_g, lca_dom_h,
    form, hurt_count, domain_differentiation, ses_composite,
    education, race, party, income_tier, age_cat, gender,
    AIHLPHRT_e_W132_rc, AIHLPHRT_f_W132_rc,
    AIHLPHRT_g_W132_rc, AIHLPHRT_h_W132_rc,
    cncexc, ai_heard
  )

message("  Form A ready for LCA: ", nrow(form_a), " respondents")
message("  Form B ready for LCA: ", nrow(form_b), " respondents")

# --------------------------------------------------------------------------- #
# 8. Save outputs                                                              #
# --------------------------------------------------------------------------- #

message("\n=== Step 8: Saving outputs ===")

full_path   <- file.path(OUT_DIR, "paper2_w132_full.rds")
form_a_path <- file.path(OUT_DIR, "paper2_form_a.rds")
form_b_path <- file.path(OUT_DIR, "paper2_form_b.rds")

saveRDS(analysis, full_path)
saveRDS(form_a,   form_a_path)
saveRDS(form_b,   form_b_path)

message("  Saved: ", full_path)
message("  Saved: ", form_a_path)
message("  Saved: ", form_b_path)

# --------------------------------------------------------------------------- #
# 9. Summary statistics                                                        #
# --------------------------------------------------------------------------- #

message("\n=== Step 9: Summary statistics ===")

pct <- function(x, total) sprintf("%.1f%%", 100 * x / total)

cat("\n", strrep("=", 60), "\n")
cat(" SAMPLE SUMMARY\n")
cat(strrep("=", 60), "\n\n")

cat("Total analysis N:", nrow(analysis), "\n")
cat("  Form A:        ", nrow(form_a),  "\n")
cat("  Form B:        ", nrow(form_b),  "\n\n")

# --- Effective N (weighted) ---
cat("Effective weighted N (sum of weights):\n")
cat("  Form A:", round(sum(form_a$weight, na.rm = TRUE), 1), "\n")
cat("  Form B:", round(sum(form_b$weight, na.rm = TRUE), 1), "\n\n")

# --- AI attitude ---
cat("AI Attitude (cncexc):\n")
att_tbl <- table(analysis$cncexc, analysis$form)
print(addmargins(att_tbl))
cat("\n")

# --- AI awareness ---
cat("AI Awareness (ai_heard):\n")
aw_tbl <- table(analysis$ai_heard, analysis$form)
print(addmargins(aw_tbl))
cat("\n")

# --- Party ---
cat("Party identification:\n")
pty_tbl <- table(analysis$party, analysis$form)
print(addmargins(pty_tbl))
cat("\n")

# --- Education ---
cat("Education:\n")
edu_tbl <- table(analysis$education, analysis$form)
print(addmargins(edu_tbl))
cat("\n")

# --- Race ---
cat("Race/Ethnicity:\n")
race_tbl <- table(analysis$race, analysis$form)
print(addmargins(race_tbl))
cat("\n")

# --- Income ---
cat("Income tier:\n")
inc_tbl <- table(analysis$income_tier, analysis$form)
print(addmargins(inc_tbl))
cat("\n")

# --- Age ---
cat("Age category:\n")
age_tbl <- table(analysis$age_cat, analysis$form)
print(addmargins(age_tbl))
cat("\n")

# --- Gender ---
cat("Gender:\n")
gen_tbl <- table(analysis$gender, analysis$form)
print(addmargins(gen_tbl))
cat("\n")

# --- Domain items: % hurt by form ---
cat("Domain items — % saying AI 'hurts more' (poLCA indicator = 2):\n")

hurt_pct <- function(vec, label) {
  valid <- vec[!is.na(vec)]
  p <- mean(valid == 2L, na.rm = TRUE) * 100
  cat(sprintf("  %-35s  n=%5d  hurt=%.1f%%\n", label, length(valid), p))
}

cat("  Form A domains:\n")
hurt_pct(form_a$lca_dom_a, "a: Accurate information")
hurt_pct(form_a$lca_dom_b, "b: Health care")
hurt_pct(form_a$lca_dom_c, "c: Vehicle safety")
hurt_pct(form_a$lca_dom_d, "d: Customer service")

cat("  Form B domains:\n")
hurt_pct(form_b$lca_dom_e, "e: Product search")
hurt_pct(form_b$lca_dom_f, "f: Privacy")
hurt_pct(form_b$lca_dom_g, "g: Policing")
hurt_pct(form_b$lca_dom_h, "h: Doctor quality")
cat("\n")

# --- Derived variables ---
cat("hurt_count distribution (Form A):\n")
print(table(form_a$hurt_count, useNA = "always"))
cat("\nhurt_count distribution (Form B):\n")
print(table(form_b$hurt_count, useNA = "always"))
cat("\n")

cat("domain_differentiation (Form A):  0=all same  1=varied\n")
print(table(form_a$domain_differentiation, useNA = "always"))
cat("\ndomain_differentiation (Form B):\n")
print(table(form_b$domain_differentiation, useNA = "always"))
cat("\n")

# --- Missing data report ---
cat(strrep("-", 60), "\n")
cat(" MISSING DATA REPORT (analysis sample)\n")
cat(strrep("-", 60), "\n")

key_vars <- c("lca_attitude", "lca_awareness", "party", "education",
              "race", "income_tier", "age_cat", "gender",
              "ses_composite", "hurt_count", "domain_differentiation",
              "weight")

miss_report <- analysis %>%
  summarise(across(all_of(key_vars),
                   list(n_miss = ~ sum(is.na(.)),
                        pct_miss = ~ sprintf("%.1f%%", 100 * mean(is.na(.)))))) %>%
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_pattern = "(.+)_(n_miss|pct_miss)") %>%
  arrange(desc(n_miss))

print(miss_report, n = Inf)

cat("\n", strrep("=", 60), "\n")
cat(" DATA PREPARATION COMPLETE\n")
cat(strrep("=", 60), "\n\n")

message("Script 01_data_prep.R finished successfully.")
