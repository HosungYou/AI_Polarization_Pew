# =============================================================================
# 02b_lca_sensitivity_no_awareness.R — Sensitivity Analysis: LCA without
# lca_awareness as an Indicator
# =============================================================================
#
# PURPOSE / MOTIVATION
# ---------------------
# In the main analysis (02_lca.R), lca_awareness is included both as an LCA
# indicator and as a predictor in downstream structural equation models. This
# creates a potential circularity concern: if awareness helps define the latent
# classes, then using class membership to predict awareness-related outcomes
# may be partially tautological.
#
# This sensitivity analysis addresses that concern by re-estimating the LCA
# using only 5 indicators (attitude + 4 domain items per form), excluding
# lca_awareness from the measurement model entirely. lca_awareness is retained
# in the dataset and examined post-hoc as a covariate to characterise classes
# and to enable comparison with the main solution.
#
# If the 4-class structure is replicated and class compositions are stable
# across the two solutions, we can be more confident that the typology does
# not depend on the circularity-inducing indicator.
#
# INDICATORS USED (5 per form, vs. 6 in main analysis)
# ------------------------------------------------------
#   Form A: lca_attitude, lca_dom_a, lca_dom_b, lca_dom_c, lca_dom_d
#   Form B: lca_attitude, lca_dom_e, lca_dom_f, lca_dom_g, lca_dom_h
#
# lca_awareness is kept in the data and analysed post-hoc (Section 12b).
#
# MODEL SETTINGS (identical to main analysis)
# -------------------------------------------
#   Seeds        : 20240101 (Form A), 20240102 (Form B)
#   Random starts: nrep = 20
#   Max iterations: maxiter = 3000
#   Selection    : lowest BIC among models with entropy > 0.6 AND min class > 5%
#
# OUTPUT FILES (all carry _noaware suffix to distinguish from main results)
# -------------------------------------------------------------------------
#   paper2_form_a_with_class_noaware.rds
#   paper2_form_b_with_class_noaware.rds
#   tbl_lca_fit_comparison_noaware.csv
#   tbl_lca_profiles_form_a_noaware.csv
#   tbl_lca_profiles_form_b_noaware.csv
#   tbl_lca_demographics_noaware.csv
#   tbl_lca_crossform_distances_noaware.csv
#   tbl_lca_awareness_by_class_noaware.csv   (post-hoc awareness distribution)
#   tbl_lca_class_crosswalk_form_a.csv       (original vs. no-awareness classes)
#   fig_lca_fit_comparison_noaware.png
#   fig_lca_profiles_form_a_noaware.png
#   fig_lca_profiles_form_b_noaware.png
#   fig_lca_heatmap_form_a_noaware.png
#   fig_lca_heatmap_form_b_noaware.png
#   fig_lca_demographics_form_a_noaware.png
#   fig_lca_demographics_form_b_noaware.png
#   fig_lca_awareness_dist_noaware.png       (post-hoc awareness by class)
# =============================================================================

library(tidyverse)
library(poLCA)

# ── Paths ────────────────────────────────────────────────────────────────────
base_dir  <- "/Users/hosung/AI_Polarization_Pew/manuscripts/paper2"
out_dir   <- file.path(base_dir, "output")
tbl_dir   <- file.path(out_dir, "tables")
fig_dir   <- file.path(out_dir, "figures")

dir.create(tbl_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ── Load data ─────────────────────────────────────────────────────────────────
message("Loading Form A and Form B data...")
form_a <- readRDS(file.path(out_dir, "paper2_form_a.rds"))
form_b <- readRDS(file.path(out_dir, "paper2_form_b.rds"))

message(sprintf("  Form A: n = %d", nrow(form_a)))
message(sprintf("  Form B: n = %d", nrow(form_b)))

# =============================================================================
# SECTION 1 — Indicator specification (5 indicators; awareness EXCLUDED)
# =============================================================================

# All indicators must be positive integers starting at 1 for poLCA.
# Verify coding before running LCA; stop with an informative message if bad.

check_lca_indicator <- function(df, var, expected_levels) {
  vals       <- df[[var]]
  vals_clean <- vals[!is.na(vals)]
  if (!all(vals_clean %in% expected_levels)) {
    bad <- setdiff(unique(vals_clean), expected_levels)
    stop(sprintf(
      "Variable '%s' has unexpected values: %s. Expected: %s",
      var, paste(bad, collapse = ", "), paste(expected_levels, collapse = ", ")
    ))
  }
  invisible(TRUE)
}

# Form A — 5 indicators (lca_awareness deliberately excluded)
form_a_vars <- c("lca_attitude",
                 "lca_dom_a", "lca_dom_b", "lca_dom_c", "lca_dom_d")
form_a_levels <- list(
  lca_attitude = 1:3,
  lca_dom_a    = 1:2,
  lca_dom_b    = 1:2,
  lca_dom_c    = 1:2,
  lca_dom_d    = 1:2
)

# Form B — 5 indicators (lca_awareness deliberately excluded)
form_b_vars <- c("lca_attitude",
                 "lca_dom_e", "lca_dom_f", "lca_dom_g", "lca_dom_h")
form_b_levels <- list(
  lca_attitude = 1:3,
  lca_dom_e    = 1:2,
  lca_dom_f    = 1:2,
  lca_dom_g    = 1:2,
  lca_dom_h    = 1:2
)

message("Checking indicator coding...")
for (v in names(form_a_levels)) check_lca_indicator(form_a, v, form_a_levels[[v]])
for (v in names(form_b_levels)) check_lca_indicator(form_b, v, form_b_levels[[v]])
message("  All indicators pass coding checks.")

# Drop rows with any missing on the 5 LCA indicators (listwise).
# NOTE: rows with missing lca_awareness are still included here, because
# lca_awareness is NOT an LCA indicator in this sensitivity analysis.
form_a_lca <- form_a %>% drop_na(all_of(form_a_vars))
form_b_lca <- form_b %>% drop_na(all_of(form_b_vars))
message(sprintf("  Form A after listwise deletion (5 indicators): n = %d (dropped %d)",
                nrow(form_a_lca), nrow(form_a) - nrow(form_a_lca)))
message(sprintf("  Form B after listwise deletion (5 indicators): n = %d (dropped %d)",
                nrow(form_b_lca), nrow(form_b) - nrow(form_b_lca)))

# =============================================================================
# SECTION 2 — Entropy helper
# =============================================================================

#' Get number of classes from a poLCA model (field not stored directly)
get_nclass <- function(model) length(model$P)

#' Compute entropy-based class separation (Ramaswamy et al. 1993)
#' Returns a value in [0, 1]; higher = cleaner separation
compute_entropy <- function(model) {
  posterior <- model$posterior
  n         <- nrow(posterior)
  K         <- ncol(posterior)
  E <- -sum(posterior * log(posterior + 1e-10)) / (n * log(K))
  1 - E
}

# =============================================================================
# SECTION 3 — Model estimation function
# =============================================================================

#' Fit LCA models with 2–max_K classes and collect fit indices
#'
#' @param df        Data frame with complete cases on indicator variables
#' @param vars      Character vector of indicator variable names
#' @param max_K     Maximum number of classes to fit (default 7)
#' @param nrep      Random starts per model (default 20)
#' @param maxiter   Maximum EM iterations (default 3000)
#' @return List with elements: models (list of poLCA fits), fit_table (tibble)
run_lca_series <- function(df, vars, max_K = 7, nrep = 20, maxiter = 3000) {

  # Build formula: cbind(v1, v2, ...) ~ 1
  # Note: poLCA does not support survey weights directly.
  # Standard practice: estimate LCA unweighted, apply weights in post-estimation.
  lhs     <- paste0("cbind(", paste(vars, collapse = ", "), ")")
  formula <- as.formula(paste(lhs, "~ 1"))

  n <- nrow(df)

  models   <- vector("list", max_K - 1)
  fit_rows <- vector("list", max_K - 1)

  for (K in 2:max_K) {
    message(sprintf("    Fitting %d-class model (nrep = %d)...", K, nrep))

    fit <- tryCatch(
      {
        poLCA(formula, data = df, nclass = K, nrep = nrep,
              maxiter = maxiter, verbose = FALSE)
      },
      error = function(e) {
        message(sprintf("      WARNING: %d-class model failed: %s", K, e$message))
        NULL
      }
    )

    models[[K - 1]] <- fit

    if (is.null(fit)) {
      fit_rows[[K - 1]] <- tibble(
        n_class        = K,
        log_likelihood = NA_real_,
        AIC            = NA_real_,
        BIC            = NA_real_,
        G_squared      = NA_real_,
        df             = NA_integer_,
        entropy        = NA_real_,
        max_prob       = NA_real_,
        min_class_pct  = NA_real_,
        converged      = FALSE
      )
    } else {
      class_sizes <- table(fit$predclass) / n
      max_prob    <- max(fit$P)

      fit_rows[[K - 1]] <- tibble(
        n_class        = K,
        log_likelihood = fit$llik,
        AIC            = fit$aic,
        BIC            = fit$bic,
        G_squared      = fit$Gsq,
        df             = fit$resid.df,
        entropy        = compute_entropy(fit),
        max_prob       = max_prob,
        min_class_pct  = min(class_sizes) * 100,
        converged      = TRUE
      )
    }
  }

  fit_table <- bind_rows(fit_rows)
  list(models = models, fit_table = fit_table)
}

# =============================================================================
# SECTION 4 — Model selection helper
# =============================================================================

#' Select optimal model: lowest BIC among models with entropy > 0.6
#' and smallest class > 5% of sample
select_model <- function(fit_table, models) {
  candidates <- fit_table %>%
    filter(converged, entropy > 0.6, min_class_pct > 5)

  if (nrow(candidates) == 0) {
    message("    No model passes entropy + min-class criteria; relaxing to BIC only.")
    candidates <- fit_table %>% filter(converged)
  }

  if (nrow(candidates) == 0) {
    stop("No models converged successfully.")
  }

  best_row <- candidates %>% slice_min(BIC, n = 1, with_ties = FALSE)
  best_K   <- best_row$n_class
  message(sprintf("    Selected: %d-class model (BIC = %.1f, entropy = %.3f, min class = %.1f%%)",
                  best_K, best_row$BIC, best_row$entropy, best_row$min_class_pct))
  models[[best_K - 1]]
}

# =============================================================================
# SECTION 5 — Run LCA on Form A (5-indicator, no-awareness model)
# =============================================================================

message("\n=== Form A LCA [Sensitivity: no awareness indicator] ===")
set.seed(20240101)
result_a <- run_lca_series(
  df      = form_a_lca,
  vars    = form_a_vars,
  max_K   = 7,
  nrep    = 20,
  maxiter = 3000
)

fit_table_a <- result_a$fit_table %>% mutate(form = "A")
message("\nForm A fit indices (no-awareness model):")
print(fit_table_a)

best_a <- select_model(result_a$fit_table, result_a$models)

# =============================================================================
# SECTION 6 — Run LCA on Form B (5-indicator, no-awareness model)
# =============================================================================

message("\n=== Form B LCA [Sensitivity: no awareness indicator] ===")
set.seed(20240102)
result_b <- run_lca_series(
  df      = form_b_lca,
  vars    = form_b_vars,
  max_K   = 7,
  nrep    = 20,
  maxiter = 3000
)

fit_table_b <- result_b$fit_table %>% mutate(form = "B")
message("\nForm B fit indices (no-awareness model):")
print(fit_table_b)

best_b <- select_model(result_b$fit_table, result_b$models)

# =============================================================================
# SECTION 7 — Save fit comparison table
# =============================================================================

fit_combined <- bind_rows(fit_table_a, fit_table_b) %>%
  dplyr::select(form, n_class, log_likelihood, AIC, BIC, G_squared, df,
                entropy, max_prob, min_class_pct, converged)

write_csv(fit_combined,
          file.path(tbl_dir, "tbl_lca_fit_comparison_noaware.csv"))
message("\nSaved: tbl_lca_fit_comparison_noaware.csv")

# =============================================================================
# SECTION 8 — Profile extraction helpers
# =============================================================================

#' Convert poLCA probs list to a tidy tibble
#' Returns: class, variable, response_level, probability
extract_profiles <- function(model, vars) {
  K <- get_nclass(model)
  map_dfr(seq_along(vars), function(j) {
    var_name <- vars[j]
    probs_j  <- model$probs[[var_name]]   # K × n_levels matrix
    map_dfr(1:K, function(k) {
      tibble(
        class          = k,
        variable       = var_name,
        response_level = seq_len(ncol(probs_j)),
        probability    = probs_j[k, ]
      )
    })
  })
}

# =============================================================================
# SECTION 9 — Substantive class labels
# =============================================================================
# Labels mirror those in the main analysis to facilitate direct comparison.
# Update after inspecting the no-awareness profiles if classes shift.

assign_labels_a <- function(K) {
  base_labels <- c("AI-Anxious", "AI-Uninformed", "AI-Advantaged", "AI-Ambivalent")
  if (K <= length(base_labels)) {
    base_labels[1:K]
  } else {
    c(base_labels, paste0("Class ", (length(base_labels) + 1):K))
  }
}

assign_labels_b <- function(K) {
  base_labels <- c("AI-Anxious", "AI-Uninformed", "AI-Advantaged", "AI-Ambivalent")
  if (K <= length(base_labels)) {
    base_labels[1:K]
  } else {
    c(base_labels, paste0("Class ", (length(base_labels) + 1):K))
  }
}

# =============================================================================
# SECTION 10 — Profile tables
# =============================================================================

profiles_a <- extract_profiles(best_a, form_a_vars) %>%
  mutate(form        = "A",
         class_label = assign_labels_a(get_nclass(best_a))[class])

profiles_b <- extract_profiles(best_b, form_b_vars) %>%
  mutate(form        = "B",
         class_label = assign_labels_b(get_nclass(best_b))[class])

write_csv(profiles_a, file.path(tbl_dir, "tbl_lca_profiles_form_a_noaware.csv"))
write_csv(profiles_b, file.path(tbl_dir, "tbl_lca_profiles_form_b_noaware.csv"))
message("Saved: tbl_lca_profiles_form_a_noaware.csv and tbl_lca_profiles_form_b_noaware.csv")

# =============================================================================
# SECTION 11 — Add class assignments to data and save (_noaware files)
# =============================================================================

form_a_lca <- form_a_lca %>%
  mutate(lca_class_noaware       = best_a$predclass,
         lca_class_label_noaware = assign_labels_a(get_nclass(best_a))[lca_class_noaware])

form_b_lca <- form_b_lca %>%
  mutate(lca_class_noaware       = best_b$predclass,
         lca_class_label_noaware = assign_labels_b(get_nclass(best_b))[lca_class_noaware])

saveRDS(form_a_lca, file.path(out_dir, "paper2_form_a_with_class_noaware.rds"))
saveRDS(form_b_lca, file.path(out_dir, "paper2_form_b_with_class_noaware.rds"))
message("Saved: paper2_form_a_with_class_noaware.rds and paper2_form_b_with_class_noaware.rds")

# =============================================================================
# SECTION 12 — Demographic composition
# =============================================================================

demo_vars <- c("education", "race", "income", "age", "gender", "party")

#' Compute weighted proportions of demographic variable within each class
#' and perform chi-square test of independence
tabulate_demo <- function(df, demo_var, class_col, weight_var = "weight", form_label) {

  if (!demo_var %in% names(df)) {
    message(sprintf("    Skipping '%s' — not found in Form %s data", demo_var, form_label))
    return(NULL)
  }

  df_clean <- df %>% filter(!is.na(.data[[demo_var]]), !is.na(.data[[class_col]]))

  if (weight_var %in% names(df_clean)) {
    wtd_tab <- df_clean %>%
      group_by(.data[[class_col]], .data[[demo_var]]) %>%
      summarise(n_wtd = sum(.data[[weight_var]], na.rm = TRUE), .groups = "drop") %>%
      rename(lca_class = 1) %>%
      group_by(lca_class) %>%
      mutate(pct = n_wtd / sum(n_wtd) * 100) %>%
      ungroup()
  } else {
    wtd_tab <- df_clean %>%
      count(.data[[class_col]], .data[[demo_var]]) %>%
      rename(lca_class = 1, n_wtd = n) %>%
      group_by(lca_class) %>%
      mutate(pct = n_wtd / sum(n_wtd) * 100) %>%
      ungroup()
  }

  chi_tab  <- table(df_clean[[class_col]], df_clean[[demo_var]])
  chi_test <- chisq.test(chi_tab, simulate.p.value = TRUE, B = 2000)
  message(sprintf("    %s × class [Form %s]: chi2(%.0f) = %.2f, p = %.4f",
                  demo_var, form_label,
                  chi_test$parameter,
                  chi_test$statistic,
                  chi_test$p.value))

  wtd_tab %>%
    mutate(demographic = demo_var,
           form        = form_label,
           chi2        = chi_test$statistic,
           chi2_p      = chi_test$p.value) %>%
    rename(level = .data[[demo_var]])
}

message("\n--- Demographic composition: Form A (no-awareness classes) ---")
demo_a <- map_dfr(demo_vars,
                  ~tabulate_demo(form_a_lca, .x,
                                 class_col  = "lca_class_noaware",
                                 weight_var = "weight",
                                 form_label = "A"))

message("\n--- Demographic composition: Form B (no-awareness classes) ---")
demo_b <- map_dfr(demo_vars,
                  ~tabulate_demo(form_b_lca, .x,
                                 class_col  = "lca_class_noaware",
                                 weight_var = "weight",
                                 form_label = "B"))

demo_all <- bind_rows(demo_a, demo_b)
write_csv(demo_all, file.path(tbl_dir, "tbl_lca_demographics_noaware.csv"))
message("Saved: tbl_lca_demographics_noaware.csv")

# =============================================================================
# SECTION 12b — Post-hoc awareness distribution by no-awareness class
# =============================================================================
# lca_awareness was excluded from the LCA but is retained in the data.
# Here we compute the mean and response-level distribution of lca_awareness
# within each no-awareness class as a post-hoc characterisation.

message("\n--- Post-hoc awareness distribution by no-awareness class ---")

summarise_awareness <- function(df, class_col, weight_var = "weight", form_label) {

  if (!"lca_awareness" %in% names(df)) {
    message(sprintf("    lca_awareness not found in Form %s data — skipping.", form_label))
    return(NULL)
  }

  df_clean <- df %>%
    filter(!is.na(lca_awareness), !is.na(.data[[class_col]]))

  # Mean lca_awareness (unweighted and weighted)
  mean_tbl <- df_clean %>%
    group_by(class_noaware = .data[[class_col]]) %>%
    summarise(
      n_valid          = n(),
      mean_awareness   = mean(lca_awareness, na.rm = TRUE),
      mean_aware_wtd   = if (weight_var %in% names(df_clean))
        weighted.mean(lca_awareness, w = .data[[weight_var]], na.rm = TRUE)
      else
        NA_real_,
      .groups = "drop"
    ) %>%
    mutate(form = form_label)

  message(sprintf("  Form %s — awareness means by no-awareness class:", form_label))
  print(mean_tbl)

  # Response-level distribution of lca_awareness within each class
  dist_tbl <- df_clean %>%
    group_by(class_noaware = .data[[class_col]], lca_awareness) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(class_noaware) %>%
    mutate(pct_within_class = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(form = form_label,
           awareness_label = case_when(
             lca_awareness == 1 ~ "A lot",
             lca_awareness == 2 ~ "A little",
             lca_awareness == 3 ~ "Nothing",
             TRUE               ~ as.character(lca_awareness)
           ))

  list(means = mean_tbl, dist = dist_tbl)
}

aware_a <- summarise_awareness(form_a_lca,
                               class_col  = "lca_class_noaware",
                               weight_var = "weight",
                               form_label = "A")

aware_b <- summarise_awareness(form_b_lca,
                               class_col  = "lca_class_noaware",
                               weight_var = "weight",
                               form_label = "B")

# Combine and save
aware_means <- bind_rows(
  if (!is.null(aware_a)) aware_a$means else NULL,
  if (!is.null(aware_b)) aware_b$means else NULL
)
aware_dist <- bind_rows(
  if (!is.null(aware_a)) aware_a$dist else NULL,
  if (!is.null(aware_b)) aware_b$dist else NULL
)

awareness_summary <- left_join(
  aware_dist,
  aware_means %>% dplyr::select(class_noaware, form, mean_awareness, mean_aware_wtd),
  by = c("class_noaware", "form")
)

write_csv(awareness_summary,
          file.path(tbl_dir, "tbl_lca_awareness_by_class_noaware.csv"))
message("Saved: tbl_lca_awareness_by_class_noaware.csv")

# =============================================================================
# SECTION 13 — Figure 1: Model Fit Comparison (BIC and AIC by n_class)
# =============================================================================

message("\n--- Generating Figure 1: Model fit comparison (no-awareness) ---")

fit_plot_data <- fit_combined %>%
  filter(converged) %>%
  dplyr::select(form, n_class, AIC, BIC) %>%
  pivot_longer(cols = c(AIC, BIC), names_to = "criterion", values_to = "value")

p_fit <- ggplot(fit_plot_data,
                aes(x = n_class, y = value,
                    color = form, linetype = criterion, shape = form)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2:7) +
  scale_color_manual(values = c("A" = "#E57373", "B" = "#4FC3F7"),
                     labels = c("A" = "Form A", "B" = "Form B")) +
  scale_linetype_manual(values = c("AIC" = "dashed", "BIC" = "solid")) +
  labs(x        = "Number of classes",
       y        = "Information criterion",
       color    = "Form",
       linetype = "Criterion",
       shape    = "Form",
       title    = "Sensitivity: LCA without awareness indicator") +
  theme_classic(base_size = 12) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank())

ggsave(file.path(fig_dir, "fig_lca_fit_comparison_noaware.png"),
       p_fit, width = 6, height = 4.5, dpi = 300)
message("Saved: fig_lca_fit_comparison_noaware.png")

# =============================================================================
# SECTION 14 — Figure 2: Class Profile Plots (grouped bar charts)
# =============================================================================

message("--- Generating Figure 2: Class profile plots (no-awareness) ---")

# Human-readable variable labels (awareness label retained for post-hoc figure)
var_labels <- c(
  lca_attitude  = "Attitude toward AI",
  lca_awareness = "AI awareness",
  lca_dom_a     = "Accurate info",
  lca_dom_b     = "Health care",
  lca_dom_c     = "Vehicle safety",
  lca_dom_d     = "Customer service",
  lca_dom_e     = "Product search",
  lca_dom_f     = "Privacy",
  lca_dom_g     = "Policing",
  lca_dom_h     = "Doctor quality"
)

response_labels_attitude  <- c("1" = "Excited",   "2" = "Concerned",   "3" = "Equal")
response_labels_awareness <- c("1" = "A lot",     "2" = "A little",    "3" = "Nothing")
response_labels_binary    <- c("1" = "Not hurt",  "2" = "Hurt")

get_response_label <- function(var, level) {
  if (var == "lca_attitude")  return(response_labels_attitude[as.character(level)])
  if (var == "lca_awareness") return(response_labels_awareness[as.character(level)])
  return(response_labels_binary[as.character(level)])
}

make_profile_plot <- function(profiles, form_label, n_classes) {

  plot_data <- profiles %>%
    mutate(
      var_label  = var_labels[variable],
      resp_label = mapply(get_response_label, variable, response_level),
      class_f    = factor(class_label)
    ) %>%
    filter(response_level == 1)

  class_colors <- c("#D32F2F", "#1976D2", "#388E3C", "#F57C00",
                    "#7B1FA2", "#0097A7", "#5D4037")[1:n_classes]

  ggplot(plot_data,
         aes(x    = reorder(var_label, probability),
             y    = probability,
             fill = class_f)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    scale_fill_manual(values = class_colors, name = "Class") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    coord_flip() +
    labs(x     = NULL,
         y     = "Probability (positive response)",
         title = sprintf("Form %s — No-awareness LCA profiles", form_label)) +
    theme_classic(base_size = 12) +
    theme(legend.position     = "bottom",
          panel.grid.major.x  = element_line(color = "grey90"))
}

p_profile_a <- make_profile_plot(profiles_a, "A", get_nclass(best_a))
p_profile_b <- make_profile_plot(profiles_b, "B", get_nclass(best_b))

ggsave(file.path(fig_dir, "fig_lca_profiles_form_a_noaware.png"),
       p_profile_a, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "fig_lca_profiles_form_b_noaware.png"),
       p_profile_b, width = 7, height = 5, dpi = 300)
message("Saved: fig_lca_profiles_form_a_noaware.png and fig_lca_profiles_form_b_noaware.png")

# Full response distribution heatmaps ─────────────────────────────────────────
make_heatmap <- function(profiles, form_label) {
  plot_data <- profiles %>%
    mutate(
      var_label  = var_labels[variable],
      resp_label = mapply(get_response_label, variable, response_level),
      cell_label = sprintf("%.0f%%", probability * 100),
      class_f    = factor(paste0("C", class, "\n", class_label))
    )

  ggplot(plot_data,
         aes(x    = class_f,
             y    = interaction(var_label, resp_label, sep = ": "),
             fill = probability)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = cell_label), size = 2.8, color = "white") +
    scale_fill_gradient2(low     = "#EF9A9A", mid = "#EF5350", high = "#B71C1C",
                         midpoint = 0.5, limits = c(0, 1),
                         labels   = scales::percent_format(),
                         name     = "Probability") +
    labs(x = NULL, y = NULL,
         title = sprintf("Form %s — No-awareness LCA heatmap", form_label)) +
    theme_classic(base_size = 10) +
    theme(axis.text.x     = element_text(angle = 0, hjust = 0.5),
          axis.text.y     = element_text(size = 8),
          legend.position = "right")
}

p_heat_a <- make_heatmap(profiles_a, "A")
p_heat_b <- make_heatmap(profiles_b, "B")

ggsave(file.path(fig_dir, "fig_lca_heatmap_form_a_noaware.png"),
       p_heat_a, width = 8, height = 6, dpi = 300)
ggsave(file.path(fig_dir, "fig_lca_heatmap_form_b_noaware.png"),
       p_heat_b, width = 8, height = 6, dpi = 300)
message("Saved: fig_lca_heatmap_form_a_noaware.png and fig_lca_heatmap_form_b_noaware.png")

# =============================================================================
# SECTION 15 — Figure 3: Demographic composition (stacked bar)
# =============================================================================

message("--- Generating Figure 3: Demographic composition (no-awareness) ---")

if (nrow(demo_all) > 0) {

  demo_plot_data <- demo_all %>%
    mutate(class_f = factor(paste0("C", lca_class)),
           level_f = factor(level))

  make_demo_plot <- function(form_label) {
    df_form <- demo_plot_data %>% filter(form == form_label)
    if (nrow(df_form) == 0) return(NULL)

    ggplot(df_form,
           aes(x = class_f, y = pct, fill = level_f)) +
      geom_col(position = "stack", width = 0.7) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      scale_fill_viridis_d(option = "D", name = "Category") +
      facet_wrap(~demographic, scales = "free_x") +
      labs(x     = "Latent class (no-awareness model)",
           y     = "Weighted proportion (%)",
           title = sprintf("Form %s — Demographic composition (no-awareness classes)",
                           form_label)) +
      theme_classic(base_size = 11) +
      theme(legend.position  = "bottom",
            strip.background = element_rect(fill = "grey95", color = NA),
            strip.text       = element_text(face = "bold", size = 9))
  }

  p_demo_a <- make_demo_plot("A")
  p_demo_b <- make_demo_plot("B")

  if (!is.null(p_demo_a)) {
    ggsave(file.path(fig_dir, "fig_lca_demographics_form_a_noaware.png"),
           p_demo_a, width = 10, height = 8, dpi = 300)
    message("Saved: fig_lca_demographics_form_a_noaware.png")
  }
  if (!is.null(p_demo_b)) {
    ggsave(file.path(fig_dir, "fig_lca_demographics_form_b_noaware.png"),
           p_demo_b, width = 10, height = 8, dpi = 300)
    message("Saved: fig_lca_demographics_form_b_noaware.png")
  }
} else {
  message("  Skipping Figure 3 — no demographic data available.")
}

# =============================================================================
# SECTION 15b — Figure: Post-hoc awareness distribution by no-awareness class
# =============================================================================

if (nrow(aware_dist) > 0) {
  message("--- Generating post-hoc awareness distribution figure ---")

  p_aware <- ggplot(aware_dist,
                    aes(x    = factor(class_noaware),
                        y    = pct_within_class,
                        fill = factor(awareness_label,
                                      levels = c("A lot", "A little", "Nothing")))) +
    geom_col(position = "stack", width = 0.7) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    scale_fill_manual(values = c("A lot"    = "#1565C0",
                                 "A little" = "#64B5F6",
                                 "Nothing"  = "#E0E0E0"),
                      name   = "Awareness level") +
    facet_wrap(~form, labeller = labeller(form = c("A" = "Form A", "B" = "Form B"))) +
    labs(x     = "Latent class (no-awareness model)",
         y     = "Percent within class",
         title = "Post-hoc awareness distribution by no-awareness class") +
    theme_classic(base_size = 12) +
    theme(legend.position  = "bottom",
          strip.background = element_rect(fill = "grey95", color = NA),
          strip.text       = element_text(face = "bold"))

  ggsave(file.path(fig_dir, "fig_lca_awareness_dist_noaware.png"),
         p_aware, width = 8, height = 5, dpi = 300)
  message("Saved: fig_lca_awareness_dist_noaware.png")
}

# =============================================================================
# SECTION 16 — Cross-Form Comparison (shared indicator: attitude only)
# =============================================================================
# In the sensitivity model the only shared indicator is lca_attitude
# (awareness is excluded). Cross-form comparison therefore uses attitude alone.

message("\n=== Cross-Form Comparison (shared indicator: attitude only) ===")

shared_vars <- c("lca_attitude")

extract_shared_profiles <- function(model, vars) {
  K <- get_nclass(model)
  map_dfr(shared_vars, function(v) {
    if (!v %in% vars) return(NULL)
    prob_mat <- model$probs[[v]]   # K × n_levels
    map_dfr(1:K, function(k) {
      tibble(class = k, variable = v,
             response_level = seq_len(ncol(prob_mat)),
             probability    = prob_mat[k, ])
    })
  })
}

shared_a <- extract_shared_profiles(best_a, form_a_vars) %>%
  unite("indicator", variable, response_level, sep = "_") %>%
  pivot_wider(names_from = indicator, values_from = probability) %>%
  rename_with(~paste0(.x, "_A"), -class)

shared_b <- extract_shared_profiles(best_b, form_b_vars) %>%
  unite("indicator", variable, response_level, sep = "_") %>%
  pivot_wider(names_from = indicator, values_from = probability) %>%
  rename_with(~paste0(.x, "_B"), -class)

common_cols_a <- grep("_A$", names(shared_a), value = TRUE)
common_cols_b <- gsub("_A$", "_B", common_cols_a)

if (all(common_cols_b %in% names(shared_b))) {
  mat_a <- as.matrix(shared_a[, common_cols_a])
  mat_b <- as.matrix(shared_b[, common_cols_b])

  dist_matrix <- matrix(NA,
                        nrow = nrow(mat_a), ncol = nrow(mat_b),
                        dimnames = list(
                          paste0("A_C", shared_a$class),
                          paste0("B_C", shared_b$class)
                        ))

  for (i in seq_len(nrow(mat_a))) {
    for (j in seq_len(nrow(mat_b))) {
      dist_matrix[i, j] <- sqrt(sum((mat_a[i, ] - mat_b[j, ])^2))
    }
  }

  message("\nPairwise Euclidean distances in attitude subspace (lower = more similar):")
  print(round(dist_matrix, 3))

  matches <- apply(dist_matrix, 1, function(row) {
    best_j <- which.min(row)
    data.frame(form_a_class = NA, form_b_class = colnames(dist_matrix)[best_j],
               distance = row[best_j])
  }) %>%
    bind_rows(.id = "form_a_class")

  message("\nBest cross-form profile matches (attitude only):")
  print(matches)

  write_csv(
    as.data.frame(dist_matrix) %>% rownames_to_column("form_a_class"),
    file.path(tbl_dir, "tbl_lca_crossform_distances_noaware.csv")
  )
  message("Saved: tbl_lca_crossform_distances_noaware.csv")

} else {
  message("  Shared indicator columns not aligned — skipping distance computation.")
}

# =============================================================================
# SECTION 17 — COMPARISON WITH ORIGINAL (MAIN) CLASS SOLUTION
# =============================================================================
# Load the original class assignments from 02_lca.R and compare them
# with the no-awareness class assignments on Form A.
#
# Goals:
#   1. Cross-tabulate original vs. no-awareness classes to assess stability.
#   2. Report whether the same number of classes was selected.
#   3. Compute awareness means/distributions by no-awareness class (already
#      done in Section 12b; repeated here for direct comparison framing).
# =============================================================================

message("\n=== COMPARISON: Original vs. No-Awareness Class Solution (Form A) ===")

orig_file_a <- file.path(out_dir, "paper2_form_a_with_class.rds")

if (file.exists(orig_file_a)) {

  orig_a <- readRDS(orig_file_a)

  # Report number of classes in each solution
  n_orig_classes   <- length(unique(orig_a$lca_class[!is.na(orig_a$lca_class)]))
  n_noaware_classes <- get_nclass(best_a)
  message(sprintf("  Original solution (with awareness):    %d classes", n_orig_classes))
  message(sprintf("  No-awareness solution:                 %d classes", n_noaware_classes))

  if (n_orig_classes == n_noaware_classes) {
    message("  >> 4-class structure HOLDS without the awareness indicator.")
  } else {
    message(sprintf(
      "  >> Class count DIFFERS: original = %d, no-awareness = %d.",
      n_orig_classes, n_noaware_classes
    ))
  }

  # Match respondents across the two datasets (by rowname / CASEID if available)
  # Strategy: inner join on CASEID if present, else on row number for overlapping rows
  join_key <- intersect(c("CASEID", "caseid", "respondent_id", "id"),
                        intersect(names(orig_a), names(form_a_lca)))

  if (length(join_key) > 0) {
    key <- join_key[1]
    message(sprintf("  Joining on key: %s", key))

    comparison_df <- inner_join(
      orig_a     %>% dplyr::select(all_of(key), lca_class, lca_awareness),
      form_a_lca %>% dplyr::select(all_of(key), lca_class_noaware),
      by = key
    )
  } else {
    # Fall back: assume same row order (both derived from form_a, same listwise deletion base)
    # This is valid only if the two listwise-deleted datasets have the same rows.
    # We match on the intersection of row indices.
    message("  No unique ID column found — matching on row order (form_a_lca rows).")
    common_n <- min(nrow(orig_a), nrow(form_a_lca))
    comparison_df <- tibble(
      lca_class         = orig_a$lca_class[seq_len(common_n)],
      lca_awareness     = orig_a$lca_awareness[seq_len(common_n)],
      lca_class_noaware = form_a_lca$lca_class_noaware[seq_len(common_n)]
    )
  }

  message(sprintf("  Matched records for cross-tabulation: n = %d", nrow(comparison_df)))

  # ── Cross-tabulation: original class × no-awareness class ─────────────────
  if (nrow(comparison_df) > 0 &&
      !all(is.na(comparison_df$lca_class)) &&
      !all(is.na(comparison_df$lca_class_noaware))) {

    crosstab <- table(
      Original    = comparison_df$lca_class,
      NoAwareness = comparison_df$lca_class_noaware
    )

    message("\nCross-tabulation: Original class (rows) × No-awareness class (cols):")
    print(crosstab)

    # Row-proportions (% of each original class that maps to each no-awareness class)
    crosstab_pct <- prop.table(crosstab, margin = 1) * 100
    message("\nRow percentages (% of original class assigned to each no-awareness class):")
    print(round(crosstab_pct, 1))

    # Save cross-tabulation
    crosstab_df <- as.data.frame(crosstab) %>%
      rename(original_class = Original, noaware_class = NoAwareness, n = Freq) %>%
      group_by(original_class) %>%
      mutate(row_pct = n / sum(n) * 100) %>%
      ungroup()

    write_csv(crosstab_df,
              file.path(tbl_dir, "tbl_lca_class_crosswalk_form_a.csv"))
    message("Saved: tbl_lca_class_crosswalk_form_a.csv")

    # ── Awareness means by no-awareness class (comparison frame) ─────────────
    if ("lca_awareness" %in% names(comparison_df)) {
      aware_comp <- comparison_df %>%
        filter(!is.na(lca_awareness)) %>%
        group_by(lca_class_noaware) %>%
        summarise(
          n             = n(),
          mean_awareness = mean(lca_awareness, na.rm = TRUE),
          pct_alot      = mean(lca_awareness == 1, na.rm = TRUE) * 100,
          pct_alittle   = mean(lca_awareness == 2, na.rm = TRUE) * 100,
          pct_nothing   = mean(lca_awareness == 3, na.rm = TRUE) * 100,
          .groups       = "drop"
        )

      message("\nAwareness means and distributions by no-awareness class (Form A):")
      print(aware_comp)
    }

    # ── Summary verdict ────────────────────────────────────────────────────────
    # Assess stability: majority assignment = proportion of respondents whose
    # class assignment is the modal no-awareness class within their original class
    if (nrow(crosstab_df) > 0) {
      modal_assignments <- crosstab_df %>%
        group_by(original_class) %>%
        slice_max(n, n = 1, with_ties = FALSE) %>%
        ungroup()

      overall_agreement <- sum(modal_assignments$n) / sum(crosstab_df$n) * 100

      message(sprintf(
        "\nOverall assignment stability: %.1f%% of respondents share modal class across solutions.",
        overall_agreement
      ))

      if (overall_agreement >= 70) {
        message("  >> Class assignments are STABLE (>= 70% agreement).")
      } else if (overall_agreement >= 50) {
        message("  >> Class assignments show MODERATE stability (50–70% agreement).")
      } else {
        message("  >> Class assignments show LOW stability (< 50% agreement).")
        message("     Interpret no-awareness solution with caution.")
      }
    }

  } else {
    message("  Insufficient matched data for cross-tabulation.")
  }

} else {
  message(sprintf(
    "  Original class file not found at:\n    %s\n  Run 02_lca.R first, then re-run this script.",
    orig_file_a
  ))
}

# =============================================================================
# SECTION 18 — Session summary
# =============================================================================

message("\n")
message("=============================================================")
message(" SENSITIVITY LCA COMPLETE (no-awareness model)")
message("=============================================================")
message(sprintf(" Form A: %d-class solution", get_nclass(best_a)))
message(sprintf("   BIC      = %.1f", best_a$bic))
message(sprintf("   Entropy  = %.3f", compute_entropy(best_a)))
message(sprintf("   N (used) = %d", nrow(form_a_lca)))
message("")
message(sprintf(" Form B: %d-class solution", get_nclass(best_b)))
message(sprintf("   BIC      = %.1f", best_b$bic))
message(sprintf("   Entropy  = %.3f", compute_entropy(best_b)))
message(sprintf("   N (used) = %d", nrow(form_b_lca)))
message("")
message(" Files written:")
message(sprintf("  %s/tbl_lca_fit_comparison_noaware.csv",        tbl_dir))
message(sprintf("  %s/tbl_lca_profiles_form_a_noaware.csv",       tbl_dir))
message(sprintf("  %s/tbl_lca_profiles_form_b_noaware.csv",       tbl_dir))
message(sprintf("  %s/tbl_lca_demographics_noaware.csv",          tbl_dir))
message(sprintf("  %s/tbl_lca_crossform_distances_noaware.csv",   tbl_dir))
message(sprintf("  %s/tbl_lca_awareness_by_class_noaware.csv",    tbl_dir))
message(sprintf("  %s/tbl_lca_class_crosswalk_form_a.csv",        tbl_dir))
message(sprintf("  %s/paper2_form_a_with_class_noaware.rds",      out_dir))
message(sprintf("  %s/paper2_form_b_with_class_noaware.rds",      out_dir))
message(sprintf("  %s/fig_lca_fit_comparison_noaware.png",        fig_dir))
message(sprintf("  %s/fig_lca_profiles_form_a_noaware.png",       fig_dir))
message(sprintf("  %s/fig_lca_profiles_form_b_noaware.png",       fig_dir))
message(sprintf("  %s/fig_lca_heatmap_form_a_noaware.png",        fig_dir))
message(sprintf("  %s/fig_lca_heatmap_form_b_noaware.png",        fig_dir))
message(sprintf("  %s/fig_lca_demographics_form_a_noaware.png",   fig_dir))
message(sprintf("  %s/fig_lca_demographics_form_b_noaware.png",   fig_dir))
message(sprintf("  %s/fig_lca_awareness_dist_noaware.png",        fig_dir))
message("=============================================================")
message("")
message("NOTES:")
message("  1. lca_awareness was EXCLUDED from the LCA indicators.")
message("     It appears in the data and is analysed post-hoc (Section 12b).")
message("  2. The cross-form comparison uses lca_attitude only (the sole")
message("     shared indicator in the 5-variable specification).")
message("  3. Class labels in assign_labels_a() / assign_labels_b() may")
message("     need updating after inspecting the no-awareness profiles.")
message("  4. Class column in saved RDS files is 'lca_class_noaware'")
message("     (not 'lca_class') to avoid collisions with the main solution.")
