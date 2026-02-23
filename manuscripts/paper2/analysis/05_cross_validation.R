# =============================================================================
# Script:  05_cross_validation.R
# Project: Paper 2 — The AI Divide (Latent Class Analysis)
# Purpose: Validate LCA profile structure across survey waves (W119, W132,
#          W152) using the two indicators common to all waves: attitude toward
#          AI (cncexc) and AI awareness (ai_heard).
#
# Inputs:
#   - data/processed/pew_w119.rds
#   - data/processed/pew_w152.rds
#   - manuscripts/paper2/output/paper2_form_a_with_class.rds  (W132 LCA results)
#
# Outputs — Tables:
#   - manuscripts/paper2/output/tables/tbl_cv01_fit_by_wave.csv
#   - manuscripts/paper2/output/tables/tbl_cv02_profiles_by_wave.csv
#   - manuscripts/paper2/output/tables/tbl_cv03_class_proportions.csv
#   - manuscripts/paper2/output/tables/tbl_cv04_education_by_wave.csv
#
# Outputs — Figures (300 dpi PNG, no titles):
#   - manuscripts/paper2/output/figures/fig_cv01_profile_comparison.png
#   - manuscripts/paper2/output/figures/fig_cv02_class_proportions.png
#   - manuscripts/paper2/output/figures/fig_cv03_education_gradient.png
#
# Notes:
#   - W119 (Dec 2022) and W152 (Aug 2024) have only 2 LCA indicators.
#   - W132 (Aug 2023) is the primary wave with 6 indicators per form; here
#     we re-run a 2-indicator LCA on W132 for an apples-to-apples comparison.
#   - All LCA models use complete-case data (listwise deletion on indicators).
#   - Survey weights are used for demographic cross-tabs but NOT in the LCA
#     itself, consistent with the main analysis in scripts 02–04.
#   - Class labels are assigned post-hoc by inspecting conditional probabilities
#     on attitude (Excited vs. Concerned) and awareness (A lot vs. Nothing).
#
# Author:  [Author]
# Date:    2026-02-23
# =============================================================================

library(tidyverse)
library(poLCA)

# --------------------------------------------------------------------------- #
# 0. Paths                                                                     #
# --------------------------------------------------------------------------- #

PROJECT_ROOT <- "/Users/hosung/AI_Polarization_Pew"
DATA_DIR     <- file.path(PROJECT_ROOT, "data/processed")
OUT_DIR      <- file.path(PROJECT_ROOT, "manuscripts/paper2/output")
TBL_DIR      <- file.path(OUT_DIR, "tables")
FIG_DIR      <- file.path(OUT_DIR, "figures")

dir.create(TBL_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# Consistent color palette for class profiles (up to 5 classes)
CLASS_COLORS <- c(
  "#2C7BB6",  # blue   — typically AI-Optimistic / high awareness
  "#D7191C",  # red    — typically AI-Skeptic / concerned
  "#1A9641",  # green  — Ambivalent / moderate
  "#FDAE61",  # orange — Low-engagement / low awareness
  "#984EA3"   # purple — spare
)

# Wave labels for plot facets / axis ticks
WAVE_LABELS <- c(
  "W119" = "W119\n(Dec 2022)",
  "W132" = "W132\n(Aug 2023)",
  "W152" = "W152\n(Aug 2024)"
)

set.seed(20260223)  # reproducibility for poLCA random starts

#' Get number of classes from a poLCA model (field not stored directly)
get_nclass <- function(model) length(model$P)

# --------------------------------------------------------------------------- #
# 1. Load and prepare data                                                     #
# --------------------------------------------------------------------------- #

message("\n=== Step 1: Loading data ===")

# --- 1a. W119 ---
w119_raw <- readRDS(file.path(DATA_DIR, "pew_w119.rds"))
message("  W119 loaded: ", nrow(w119_raw), " rows")

# --- 1b. W152 ---
w152_raw <- readRDS(file.path(DATA_DIR, "pew_w152.rds"))
message("  W152 loaded: ", nrow(w152_raw), " rows")

# --- 1c. W132 Form A with class assignments (for profile comparison) ---
w132_path <- file.path(OUT_DIR, "paper2_form_a_with_class.rds")
if (file.exists(w132_path)) {
  w132_cls <- readRDS(w132_path)
  message("  W132 Form A (with class) loaded: ", nrow(w132_cls), " rows")
  has_w132_cls <- TRUE
} else {
  # Fallback: load the Form A prep file without class labels
  w132_fallback <- file.path(OUT_DIR, "paper2_form_a.rds")
  if (file.exists(w132_fallback)) {
    w132_cls <- readRDS(w132_fallback)
    message("  W132 Form A (no class column) loaded from fallback: ",
            nrow(w132_cls), " rows")
    has_w132_cls <- FALSE
  } else {
    stop("Neither paper2_form_a_with_class.rds nor paper2_form_a.rds found. ",
         "Run scripts 02–03 first.")
  }
}

# --------------------------------------------------------------------------- #
# 2. Recode cross-wave indicators                                              #
#    poLCA requires integer indicators starting at 1.                         #
#    Convention (same as 01_data_prep.R):                                     #
#      lca_attitude : 1=Excited, 2=Concerned, 3=Equally                      #
#      lca_awareness: 1=A lot,   2=A little,  3=Nothing                      #
# --------------------------------------------------------------------------- #

message("\n=== Step 2: Recoding indicators ===")

recode_indicators <- function(df, wave_label) {
  # Handle both factor and character cncexc / ai_heard
  df <- df %>%
    mutate(
      lca_attitude = case_when(
        as.character(cncexc) == "Excited"   ~ 1L,
        as.character(cncexc) == "Concerned" ~ 2L,
        as.character(cncexc) == "Equally"   ~ 3L,
        TRUE ~ NA_integer_
      ),
      lca_awareness = case_when(
        as.character(ai_heard) == "A lot"    ~ 1L,
        as.character(ai_heard) == "A little" ~ 2L,
        as.character(ai_heard) == "Nothing"  ~ 3L,
        TRUE ~ NA_integer_
      ),
      wave = wave_label
    )

  # Standardise demographic variables to factor levels used across waves
  if ("education" %in% names(df)) {
    df <- df %>%
      mutate(education = factor(
        as.character(education),
        levels = c("HS graduate or less", "Some College", "College graduate+")
      ))
  }
  if ("income_tier" %in% names(df)) {
    df <- df %>%
      mutate(income_tier = factor(
        as.character(income_tier),
        levels = c("Lower", "Middle", "Upper")
      ))
  }
  if ("party" %in% names(df)) {
    df <- df %>%
      mutate(party = factor(
        as.character(party),
        levels = c("Rep/Lean Rep", "Dem/Lean Dem")
      ))
  }
  if ("age_cat" %in% names(df)) {
    df <- df %>%
      mutate(age_cat = factor(
        as.character(age_cat),
        levels = c("18-29", "30-49", "50-64", "65+")
      ))
  }
  if ("gender" %in% names(df)) {
    df <- df %>%
      mutate(gender = factor(
        as.character(gender),
        levels = c("Male", "Female", "Other")
      ))
  }

  n_complete <- sum(!is.na(df$lca_attitude) & !is.na(df$lca_awareness))
  message("  ", wave_label, ": ", n_complete,
          " complete cases on both indicators")
  df
}

w119 <- recode_indicators(w119_raw, "W119")
w152 <- recode_indicators(w152_raw, "W152")

# W132: recode if lca_attitude / lca_awareness not already present
if (!"lca_attitude" %in% names(w132_cls)) {
  w132 <- recode_indicators(w132_cls, "W132")
} else {
  w132 <- w132_cls %>% mutate(wave = "W132")
  message("  W132: lca_attitude/lca_awareness already present, wave tag added")
}

# --------------------------------------------------------------------------- #
# 3. LCA helper functions                                                      #
# --------------------------------------------------------------------------- #

message("\n=== Step 3: Defining LCA helpers ===")

# poLCA formula for 2-indicator model
LCA_FORMULA_2IND <- cbind(lca_attitude, lca_awareness) ~ 1

# Run LCA across K = 2:5 classes with nrep random starts; return best model
# per K (maximum log-likelihood).
run_lca_grid <- function(df, k_max = 5, nrep = 10, verbose = FALSE) {
  # Complete cases only
  df_cc <- df %>%
    filter(!is.na(lca_attitude), !is.na(lca_awareness))

  results <- vector("list", k_max - 1L)

  for (k in 2:k_max) {
    best_model <- NULL
    best_llik  <- -Inf

    for (r in seq_len(nrep)) {
      tryCatch({
        m <- poLCA(LCA_FORMULA_2IND, data = df_cc,
                   nclass = k, maxiter = 2000,
                   tol = 1e-8, na.rm = FALSE,
                   nrep = 1, verbose = FALSE,
                   calc.se = (r == 1))  # SE only on first rep to save time
        if (m$llik > best_llik) {
          best_llik  <- m$llik
          best_model <- m
        }
      }, error = function(e) {
        if (verbose) message("    K=", k, " rep ", r, " failed: ", e$message)
      })
    }

    if (!is.null(best_model)) {
      results[[k - 1L]] <- best_model
      if (verbose) {
        message("    K=", k, "  BIC=", round(best_model$bic, 2),
                "  llik=", round(best_model$llik, 2))
      }
    }
  }
  results
}

# Extract fit indices from a list of poLCA models (one per K)
extract_fit <- function(model_list, wave_label) {
  purrr::map_dfr(model_list, function(m) {
    if (is.null(m)) return(NULL)
    tibble(
      wave     = wave_label,
      nclass   = get_nclass(m),
      N        = m$N,
      llik     = round(m$llik, 3),
      AIC      = round(m$aic, 3),
      BIC      = round(m$bic, 3),
      df       = m$resid.df,
      chi_sq   = round(m$Chisq, 3),
      Gsq      = round(m$Gsq, 3)
    )
  })
}

# Extract class-conditional probabilities from best model
extract_profiles <- function(model, wave_label, class_names = NULL) {
  probs <- model$probs

  # Attitude probabilities (3 categories: Excited, Concerned, Equally)
  att <- as.data.frame(probs$lca_attitude) %>%
    rownames_to_column("class_num") %>%
    mutate(
      class_num = as.integer(class_num),
      indicator = "attitude",
      wave      = wave_label
    ) %>%
    rename(
      `Excited`  = `Pr(1)`,
      `Concerned` = `Pr(2)`,
      `Equally`  = `Pr(3)`
    ) %>%
    pivot_longer(c(Excited, Concerned, Equally),
                 names_to = "category", values_to = "prob")

  # Awareness probabilities (3 categories: A lot, A little, Nothing)
  awr <- as.data.frame(probs$lca_awareness) %>%
    rownames_to_column("class_num") %>%
    mutate(
      class_num = as.integer(class_num),
      indicator = "awareness",
      wave      = wave_label
    ) %>%
    rename(
      `A lot`    = `Pr(1)`,
      `A little` = `Pr(2)`,
      `Nothing`  = `Pr(3)`
    ) %>%
    pivot_longer(c(`A lot`, `A little`, `Nothing`),
                 names_to = "category", values_to = "prob")

  profiles <- bind_rows(att, awr)

  # Class sizes
  sizes <- tibble(
    class_num  = seq_along(model$P),
    class_prop = model$P,
    wave       = wave_label
  )

  profiles <- profiles %>%
    left_join(sizes, by = c("class_num", "wave"))

  # Attach class names if provided
  if (!is.null(class_names)) {
    name_map <- tibble(class_num = seq_along(class_names),
                       class_name = class_names)
    profiles <- profiles %>% left_join(name_map, by = "class_num")
  } else {
    profiles <- profiles %>%
      mutate(class_name = paste0("Class ", class_num))
  }
  profiles
}

# Assign substantive class labels based on conditional probabilities:
#   "AI-Optimistic"  : P(Excited) > 0.4 AND P(A lot) > 0.3
#   "AI-Skeptic"     : P(Concerned) > 0.5
#   "Ambivalent"     : P(Equally) > 0.3 OR moderate on both
#   "Low-Engagement" : P(Nothing) > 0.4
#   Otherwise        : "Class N"
label_classes <- function(model) {
  att_probs <- model$probs$lca_attitude   # rows = classes
  awr_probs <- model$probs$lca_awareness

  labels <- character(get_nclass(model))
  for (k in seq_len(get_nclass(model))) {
    p_excited   <- att_probs[k, 1]
    p_concerned <- att_probs[k, 2]
    p_equally   <- att_probs[k, 3]
    p_alot      <- awr_probs[k, 1]
    p_nothing   <- awr_probs[k, 3]

    if (p_nothing > 0.40) {
      labels[k] <- "Low-Engagement"
    } else if (p_concerned > 0.50) {
      labels[k] <- "AI-Skeptic"
    } else if (p_excited > 0.35 && p_alot > 0.25) {
      labels[k] <- "AI-Optimistic"
    } else if (p_equally > 0.30) {
      labels[k] <- "Ambivalent"
    } else {
      labels[k] <- paste0("Class ", k)
    }
  }
  labels
}

# --------------------------------------------------------------------------- #
# 4. Run LCA for each wave                                                     #
# --------------------------------------------------------------------------- #

message("\n=== Step 4: Running LCA grid (K = 2–5) for each wave ===")

message("  --- W119 ---")
w119_models <- run_lca_grid(w119, k_max = 5, nrep = 10, verbose = TRUE)

message("  --- W152 ---")
w152_models <- run_lca_grid(w152, k_max = 5, nrep = 10, verbose = TRUE)

message("  --- W132 (2-indicator re-run for comparability) ---")
w132_models <- run_lca_grid(w132, k_max = 5, nrep = 10, verbose = TRUE)

# --------------------------------------------------------------------------- #
# 5. Model selection: BIC-optimal K                                            #
# --------------------------------------------------------------------------- #

message("\n=== Step 5: Model selection by BIC ===")

select_best_model <- function(model_list, wave_label) {
  fit <- extract_fit(model_list, wave_label)
  best_row <- fit %>% filter(!is.na(BIC)) %>% arrange(BIC) %>% slice(1)
  k_best <- best_row$nclass
  message("  ", wave_label, ": BIC-optimal K = ", k_best,
          "  (BIC = ", round(best_row$BIC, 2), ")")
  model_list[[k_best - 1L]]
}

w119_best <- select_best_model(w119_models, "W119")
w152_best <- select_best_model(w152_models, "W152")
w132_best <- select_best_model(w132_models, "W132")

# Assign substantive labels
w119_labels <- label_classes(w119_best)
w152_labels <- label_classes(w152_best)
w132_labels <- label_classes(w132_best)

message("  W119 class labels: ", paste(w119_labels, collapse = ", "))
message("  W132 class labels: ", paste(w132_labels, collapse = ", "))
message("  W152 class labels: ", paste(w152_labels, collapse = ", "))

# Attach predicted class membership back to each wave's data frame
attach_class <- function(df, model, labels, wave_label) {
  df_cc <- df %>%
    filter(!is.na(lca_attitude), !is.na(lca_awareness)) %>%
    mutate(
      class_num  = model$predclass,
      class_name = labels[model$predclass]
    )
  df_cc
}

w119_classed <- attach_class(w119, w119_best, w119_labels, "W119")
w152_classed <- attach_class(w152, w152_best, w152_labels, "W152")
w132_classed <- attach_class(w132, w132_best, w132_labels, "W132")

# --------------------------------------------------------------------------- #
# 6. Table 1 — LCA fit indices by wave                                         #
# --------------------------------------------------------------------------- #

message("\n=== Step 6: Building tbl_cv01_fit_by_wave ===")

fit_all <- bind_rows(
  extract_fit(w119_models, "W119"),
  extract_fit(w132_models, "W132"),
  extract_fit(w152_models, "W152")
) %>%
  arrange(wave, nclass) %>%
  mutate(
    wave_label = WAVE_LABELS[wave],
    bic_flag   = ""
  )

# Mark BIC-optimal row per wave
for (wv in unique(fit_all$wave)) {
  idx_opt <- which(fit_all$wave == wv &
                     fit_all$BIC == min(fit_all$BIC[fit_all$wave == wv],
                                        na.rm = TRUE))
  fit_all$bic_flag[idx_opt] <- "*"
}

write_csv(fit_all, file.path(TBL_DIR, "tbl_cv01_fit_by_wave.csv"))
message("  Saved tbl_cv01_fit_by_wave.csv")
print(fit_all %>% dplyr::select(wave, nclass, N, llik, AIC, BIC, bic_flag))

# --------------------------------------------------------------------------- #
# 7. Table 2 — Class-conditional profiles by wave                              #
# --------------------------------------------------------------------------- #

message("\n=== Step 7: Building tbl_cv02_profiles_by_wave ===")

profiles_all <- bind_rows(
  extract_profiles(w119_best, "W119", w119_labels),
  extract_profiles(w132_best, "W132", w132_labels),
  extract_profiles(w152_best, "W152", w152_labels)
)

write_csv(profiles_all, file.path(TBL_DIR, "tbl_cv02_profiles_by_wave.csv"))
message("  Saved tbl_cv02_profiles_by_wave.csv")

# --------------------------------------------------------------------------- #
# 8. Table 3 — Class proportions by wave                                       #
# --------------------------------------------------------------------------- #

message("\n=== Step 8: Building tbl_cv03_class_proportions ===")

class_props <- profiles_all %>%
  distinct(wave, class_num, class_name, class_prop) %>%
  arrange(wave, class_num) %>%
  mutate(class_pct = round(class_prop * 100, 1))

write_csv(class_props, file.path(TBL_DIR, "tbl_cv03_class_proportions.csv"))
message("  Saved tbl_cv03_class_proportions.csv")
print(class_props)

# --------------------------------------------------------------------------- #
# 9. Table 4 — Education gradient by wave (multinomial logistic)               #
# --------------------------------------------------------------------------- #

message("\n=== Step 9: Building tbl_cv04_education_by_wave ===")

# Run multinomial logit: class_name ~ education + income_tier + party + age_cat
# Use nnet::multinom; class_name is the outcome.
# Reference class = lowest-engagement / most concerned class per wave.
# We report the education coefficient (log-OR vs. reference).

if (!requireNamespace("nnet", quietly = TRUE)) {
  message("  Package 'nnet' not available — skipping multinomial models.")
  educ_results <- tibble()
} else {
  run_multinom <- function(df_classed, wave_label) {
    # Keep only rows with complete demographics
    key_demo <- c("education", "income_tier", "party", "age_cat")
    missing_cols <- setdiff(key_demo, names(df_classed))
    if (length(missing_cols) > 0) {
      message("  ", wave_label, ": missing demographic columns: ",
              paste(missing_cols, collapse = ", "), " — skipping multinom.")
      return(NULL)
    }

    df_mod <- df_classed %>%
      filter(!is.na(education), !is.na(income_tier),
             !is.na(party), !is.na(age_cat)) %>%
      mutate(
        class_name = factor(class_name),
        education  = relevel(factor(education, levels = c(
          "HS graduate or less", "Some College", "College graduate+"
        )), ref = "HS graduate or less"),
        income_tier = relevel(factor(income_tier,
                                     levels = c("Lower", "Middle", "Upper")),
                              ref = "Lower"),
        party = relevel(factor(party,
                               levels = c("Rep/Lean Rep", "Dem/Lean Dem")),
                        ref = "Rep/Lean Rep"),
        age_cat = relevel(factor(age_cat,
                                 levels = c("18-29", "30-49", "50-64", "65+")),
                          ref = "18-29")
      )

    if (nlevels(df_mod$class_name) < 2) {
      message("  ", wave_label, ": fewer than 2 class levels — skipping.")
      return(NULL)
    }

    m <- nnet::multinom(
      class_name ~ education + income_tier + party + age_cat,
      data = df_mod, trace = FALSE, MaxNWts = 2000
    )

    coef_df <- as.data.frame(coef(m))
    # coef(m) is (K-1) x P; rows = non-reference classes, cols = predictors
    coef_df %>%
      rownames_to_column("outcome_class") %>%
      pivot_longer(-outcome_class, names_to = "term", values_to = "log_OR") %>%
      filter(str_detect(term, "^education")) %>%
      mutate(
        wave        = wave_label,
        educ_level  = str_remove(term, "^education"),
        log_OR      = round(log_OR, 4)
      ) %>%
      dplyr::select(wave, outcome_class, educ_level, log_OR)
  }

  educ_w119 <- run_multinom(w119_classed, "W119")
  educ_w132 <- run_multinom(w132_classed, "W132")
  educ_w152 <- run_multinom(w152_classed, "W152")

  educ_results <- bind_rows(educ_w119, educ_w132, educ_w152)
}

if (nrow(educ_results) > 0) {
  write_csv(educ_results, file.path(TBL_DIR, "tbl_cv04_education_by_wave.csv"))
  message("  Saved tbl_cv04_education_by_wave.csv")
  print(educ_results)
} else {
  message("  No education results to save.")
}

# --------------------------------------------------------------------------- #
# 10. Structural stability: chi-square on class proportions                    #
# --------------------------------------------------------------------------- #

message("\n=== Step 10: Structural stability tests ===")

# Build a cross-tab: wave x class_name (using class label strings mapped
# to a common set derived from label_classes).
# We compare W119 vs. W132 and W132 vs. W152.

make_wave_class_table <- function(df_classed, wave_label) {
  df_classed %>%
    count(wave, class_name) %>%
    mutate(wave = wave_label)
}

wave_class_long <- bind_rows(
  make_wave_class_table(w119_classed, "W119"),
  make_wave_class_table(w132_classed, "W132"),
  make_wave_class_table(w152_classed, "W152")
)

# Pivot to wide: rows = wave, cols = class_name (fill NA with 0)
wave_class_wide <- wave_class_long %>%
  pivot_wider(names_from = class_name, values_from = n, values_fill = 0) %>%
  column_to_rownames("wave")

message("  Wave x class counts:")
print(wave_class_wide)

tryCatch({
  chi_result <- chisq.test(wave_class_wide)
  message("  Chi-square test: X2 = ", round(chi_result$statistic, 3),
          "  df = ", chi_result$parameter,
          "  p = ", format.pval(chi_result$p.value, digits = 3))
}, error = function(e) {
  message("  Chi-square test failed (classes not aligned across waves): ",
          e$message)
  message("  This is expected when waves have different optimal K.")
})

# --------------------------------------------------------------------------- #
# 11. Figure 1 — Profile comparison across waves                               #
# --------------------------------------------------------------------------- #

message("\n=== Step 11: Saving fig_cv01_profile_comparison.png ===")

# Facet by wave; within each facet, lines/bars for each class showing
# P(category | class) for attitude and awareness.
# Use a dot-plot (point + line) style: x = category, y = prob, color = class.

# Category ordering for the x-axis
att_levels <- c("Excited", "Equally", "Concerned")
awr_levels <- c("A lot", "A little", "Nothing")

plot_profiles <- profiles_all %>%
  mutate(
    indicator_label = case_when(
      indicator == "attitude"  ~ "AI Attitude",
      indicator == "awareness" ~ "AI Awareness",
      TRUE                     ~ indicator
    ),
    # Convert category to character first to avoid mixed-factor issue,
    # then re-factor with combined ordered levels after splitting
    category_chr = as.character(category),
    wave_label   = WAVE_LABELS[wave],
    wave_label   = factor(wave_label, levels = WAVE_LABELS)
  ) %>%
  select(-category_chr) %>%
  # Drop any rows with NA class_name to avoid NA legend entry
  filter(!is.na(class_name)) %>%
  # Set ordered factor levels per indicator using integer codes to avoid
  # mixed-factor issue in case_when
  mutate(
    category = factor(
      as.character(category),
      levels = c(att_levels, setdiff(awr_levels, att_levels))
    )
  )

# Determine consistent color assignment: sort class_names alphabetically
all_class_names <- sort(unique(plot_profiles$class_name))
class_color_map <- setNames(
  CLASS_COLORS[seq_along(all_class_names)],
  all_class_names
)

fig_cv01 <- ggplot(plot_profiles,
                   aes(x = category, y = prob,
                       color = class_name, group = class_name)) +
  geom_line(linewidth = 0.8, alpha = 0.85) +
  geom_point(size = 2.5) +
  facet_grid(wave_label ~ indicator_label, scales = "free_x") +
  scale_color_manual(values = class_color_map, name = "Class") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = NULL, y = "Class-conditional probability") +
  theme_bw(base_size = 11) +
  theme(
    strip.text        = element_text(size = 10, face = "bold"),
    axis.text.x       = element_text(angle = 30, hjust = 1, size = 9),
    axis.text.y       = element_text(size = 9),
    legend.position   = "bottom",
    legend.title      = element_text(size = 10),
    panel.grid.minor  = element_blank(),
    plot.title        = element_blank()
  )

ggsave(file.path(FIG_DIR, "fig_cv01_profile_comparison.png"),
       fig_cv01, width = 10, height = 6, dpi = 300, bg = "white")
message("  Saved fig_cv01_profile_comparison.png")

# --------------------------------------------------------------------------- #
# 12. Figure 2 — Class proportions across waves (stacked bar)                  #
# --------------------------------------------------------------------------- #

message("\n=== Step 12: Saving fig_cv02_class_proportions.png ===")

class_props_plot <- class_props %>%
  mutate(
    wave_label = WAVE_LABELS[wave],
    wave_label = factor(wave_label, levels = WAVE_LABELS),
    class_name = factor(class_name, levels = rev(all_class_names))
  )

fig_cv02 <- ggplot(class_props_plot,
                   aes(x = wave_label, y = class_pct, fill = class_name)) +
  geom_col(width = 0.55, color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(class_pct, "%")),
            position = position_stack(vjust = 0.5),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_manual(values = rev(class_color_map[levels(class_props_plot$class_name)]),
                    name = "Class") +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 101), expand = c(0, 0)) +
  labs(x = NULL, y = "Class proportion (%)") +
  theme_bw(base_size = 11) +
  theme(
    axis.text       = element_text(size = 10),
    legend.position = "right",
    legend.title    = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_blank()
  )

ggsave(file.path(FIG_DIR, "fig_cv02_class_proportions.png"),
       fig_cv02, width = 8, height = 5, dpi = 300, bg = "white")
message("  Saved fig_cv02_class_proportions.png")

# --------------------------------------------------------------------------- #
# 13. Figure 3 — Education gradient in "AI-Optimistic" class across waves      #
# --------------------------------------------------------------------------- #

message("\n=== Step 13: Saving fig_cv03_education_gradient.png ===")

# For each wave, compute weighted proportion in each education group that
# belongs to the "most engaged / optimistic" class.
# We identify that class as the one with highest P(Excited) + P(A lot).

identify_optimistic_class <- function(model, labels) {
  att_probs <- model$probs$lca_attitude
  awr_probs <- model$probs$lca_awareness
  scores    <- att_probs[, 1] + awr_probs[, 1]  # P(Excited) + P(A lot)
  labels[which.max(scores)]
}

opt_class_w119 <- identify_optimistic_class(w119_best, w119_labels)
opt_class_w132 <- identify_optimistic_class(w132_best, w132_labels)
opt_class_w152 <- identify_optimistic_class(w152_best, w152_labels)

message("  Optimistic class identified:")
message("    W119: ", opt_class_w119)
message("    W132: ", opt_class_w132)
message("    W152: ", opt_class_w152)

# Compute proportion in optimistic class by education, per wave
# Use survey weights if available; otherwise unweighted
compute_educ_gradient <- function(df_classed, opt_class, wave_label) {
  has_weight <- "weight" %in% names(df_classed) &&
                !all(is.na(df_classed$weight))

  if (!"education" %in% names(df_classed)) {
    message("  ", wave_label, ": no education variable — skipping gradient.")
    return(NULL)
  }

  df_g <- df_classed %>%
    filter(!is.na(education)) %>%
    mutate(is_opt = as.integer(class_name == opt_class))

  if (has_weight) {
    df_g <- df_g %>% filter(!is.na(weight))
    result <- df_g %>%
      group_by(education) %>%
      summarise(
        prop_opt = sum(is_opt * weight, na.rm = TRUE) /
                   sum(weight, na.rm = TRUE),
        n        = n(),
        .groups  = "drop"
      )
  } else {
    result <- df_g %>%
      group_by(education) %>%
      summarise(
        prop_opt = mean(is_opt, na.rm = TRUE),
        n        = n(),
        .groups  = "drop"
      )
  }

  # Wilson binomial CI (95%)
  result <- result %>%
    mutate(
      se_prop  = sqrt(prop_opt * (1 - prop_opt) / n),
      ci_lo    = pmax(0, prop_opt - 1.96 * se_prop),
      ci_hi    = pmin(1, prop_opt + 1.96 * se_prop),
      wave     = wave_label,
      wave_label_fmt = WAVE_LABELS[wave_label]
    )
  result
}

educ_grad_w119 <- compute_educ_gradient(w119_classed, opt_class_w119, "W119")
educ_grad_w132 <- compute_educ_gradient(w132_classed, opt_class_w132, "W132")
educ_grad_w152 <- compute_educ_gradient(w152_classed, opt_class_w152, "W152")

educ_grad_all <- bind_rows(educ_grad_w119, educ_grad_w132, educ_grad_w152)

if (nrow(educ_grad_all) > 0) {
  educ_grad_all <- educ_grad_all %>%
    mutate(
      education = factor(education,
                         levels = c("HS graduate or less",
                                    "Some College",
                                    "College graduate+")),
      wave_label_fmt = factor(WAVE_LABELS[wave],
                              levels = WAVE_LABELS)
    )

  # x-axis labels for education
  educ_xlab <- c(
    "HS graduate or less" = "HS or less",
    "Some College"        = "Some college",
    "College graduate+"   = "College+"
  )

  fig_cv03 <- ggplot(educ_grad_all,
                     aes(x = education, y = prop_opt,
                         color = wave_label_fmt, group = wave_label_fmt)) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = wave_label_fmt),
                alpha = 0.12, color = NA) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 3) +
    scale_x_discrete(labels = educ_xlab) +
    scale_y_continuous(limits = c(0, 1),
                       labels = scales::percent_format(),
                       breaks = seq(0, 1, 0.1)) +
    scale_color_manual(
      values = c("W119\n(Dec 2022)" = "#2166AC",
                 "W132\n(Aug 2023)" = "#4DAC26",
                 "W152\n(Aug 2024)" = "#D01C8B"),
      name = "Survey wave"
    ) +
    scale_fill_manual(
      values = c("W119\n(Dec 2022)" = "#2166AC",
                 "W132\n(Aug 2023)" = "#4DAC26",
                 "W152\n(Aug 2024)" = "#D01C8B"),
      name = "Survey wave"
    ) +
    labs(x = "Education", y = "Proportion in AI-Optimistic class (95% CI)") +
    theme_bw(base_size = 11) +
    theme(
      axis.text       = element_text(size = 10),
      legend.position = "right",
      legend.title    = element_text(size = 10),
      panel.grid.minor   = element_blank(),
      plot.title         = element_blank()
    )

  ggsave(file.path(FIG_DIR, "fig_cv03_education_gradient.png"),
         fig_cv03, width = 9, height = 5, dpi = 300, bg = "white")
  message("  Saved fig_cv03_education_gradient.png")
} else {
  message("  No education gradient data — fig_cv03 skipped.")
}

# --------------------------------------------------------------------------- #
# 14. Final summary                                                            #
# --------------------------------------------------------------------------- #

message("\n", strrep("=", 60))
message(" CROSS-VALIDATION SUMMARY")
message(strrep("=", 60))

message("\nBIC-optimal solutions:")
message("  W119 (Dec 2022): K = ", get_nclass(w119_best),
        "  classes: ", paste(w119_labels, collapse = "; "))
message("  W132 (Aug 2023): K = ", get_nclass(w132_best),
        "  classes: ", paste(w132_labels, collapse = "; "))
message("  W152 (Aug 2024): K = ", get_nclass(w152_best),
        "  classes: ", paste(w152_labels, collapse = "; "))

message("\nSaved tables:")
message("  ", file.path(TBL_DIR, "tbl_cv01_fit_by_wave.csv"))
message("  ", file.path(TBL_DIR, "tbl_cv02_profiles_by_wave.csv"))
message("  ", file.path(TBL_DIR, "tbl_cv03_class_proportions.csv"))
if (nrow(educ_results) > 0)
  message("  ", file.path(TBL_DIR, "tbl_cv04_education_by_wave.csv"))

message("\nSaved figures:")
message("  ", file.path(FIG_DIR, "fig_cv01_profile_comparison.png"))
message("  ", file.path(FIG_DIR, "fig_cv02_class_proportions.png"))
if (nrow(educ_grad_all) > 0)
  message("  ", file.path(FIG_DIR, "fig_cv03_education_gradient.png"))

message("\nScript 05_cross_validation.R finished successfully.")
