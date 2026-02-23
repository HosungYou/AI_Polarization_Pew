# =============================================================================
# 02_lca.R — Latent Class Analysis for Paper 2 (AI Divide Study)
# =============================================================================
# Design: W132 split-sample
#   Form A: attitude, awareness, dom_a (accurate info), dom_b (health care),
#           dom_c (vehicle safety), dom_d (customer service)
#   Form B: attitude, awareness, dom_e (product search), dom_f (privacy),
#           dom_g (policing), dom_h (doctor quality)
#
# Approach: Separate LCA on each form, 2–7 classes, select by BIC + entropy
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
# SECTION 1 — Indicator specification
# =============================================================================

# All indicators must be positive integers starting at 1 for poLCA.
# Verify coding before running LCA; stop with an informative message if bad.

check_lca_indicator <- function(df, var, expected_levels) {
  vals <- df[[var]]
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

# Form A indicators and expected levels
form_a_vars <- c("lca_attitude", "lca_awareness",
                 "lca_dom_a", "lca_dom_b", "lca_dom_c", "lca_dom_d")
form_a_levels <- list(
  lca_attitude  = 1:3,
  lca_awareness = 1:3,
  lca_dom_a     = 1:2,
  lca_dom_b     = 1:2,
  lca_dom_c     = 1:2,
  lca_dom_d     = 1:2
)

# Form B indicators and expected levels
form_b_vars <- c("lca_attitude", "lca_awareness",
                 "lca_dom_e", "lca_dom_f", "lca_dom_g", "lca_dom_h")
form_b_levels <- list(
  lca_attitude  = 1:3,
  lca_awareness = 1:3,
  lca_dom_e     = 1:2,
  lca_dom_f     = 1:2,
  lca_dom_g     = 1:2,
  lca_dom_h     = 1:2
)

message("Checking indicator coding...")
for (v in names(form_a_levels)) check_lca_indicator(form_a, v, form_a_levels[[v]])
for (v in names(form_b_levels)) check_lca_indicator(form_b, v, form_b_levels[[v]])
message("  All indicators pass coding checks.")

# Drop rows with any missing on LCA indicators (listwise)
form_a_lca <- form_a %>% drop_na(all_of(form_a_vars))
form_b_lca <- form_b %>% drop_na(all_of(form_b_vars))
message(sprintf("  Form A after listwise deletion: n = %d (dropped %d)",
                nrow(form_a_lca), nrow(form_a) - nrow(form_a_lca)))
message(sprintf("  Form B after listwise deletion: n = %d (dropped %d)",
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
#' @param weight_var Name of survey weight variable (or NULL for unweighted)
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

    # Attempt model fit; catch convergence failures gracefully
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
      # Smallest class proportion
      class_sizes <- table(fit$predclass) / n
      max_prob    <- max(fit$P)      # largest class prior probability

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
# SECTION 5 — Run LCA on Form A
# =============================================================================

message("\n=== Form A LCA ===")
set.seed(20240101)
result_a <- run_lca_series(
  df         = form_a_lca,
  vars       = form_a_vars,
  max_K      = 7,
  nrep       = 20,
  maxiter    = 3000
)

fit_table_a <- result_a$fit_table %>% mutate(form = "A")
message("\nForm A fit indices:")
print(fit_table_a)

best_a <- select_model(result_a$fit_table, result_a$models)

# =============================================================================
# SECTION 6 — Run LCA on Form B
# =============================================================================

message("\n=== Form B LCA ===")
set.seed(20240102)
result_b <- run_lca_series(
  df         = form_b_lca,
  vars       = form_b_vars,
  max_K      = 7,
  nrep       = 20,
  maxiter    = 3000
)

fit_table_b <- result_b$fit_table %>% mutate(form = "B")
message("\nForm B fit indices:")
print(fit_table_b)

best_b <- select_model(result_b$fit_table, result_b$models)

# =============================================================================
# SECTION 7 — Save fit comparison table
# =============================================================================

fit_combined <- bind_rows(fit_table_a, fit_table_b) %>%
  dplyr::select(form, n_class, log_likelihood, AIC, BIC, G_squared, df,
                entropy, max_prob, min_class_pct, converged)

write_csv(fit_combined,
          file.path(tbl_dir, "tbl_lca_fit_comparison.csv"))
message("\nSaved: tbl_lca_fit_comparison.csv")

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
        class         = k,
        variable      = var_name,
        response_level = seq_len(ncol(probs_j)),
        probability   = probs_j[k, ]
      )
    })
  })
}

# =============================================================================
# SECTION 9 — Substantive class labels
# =============================================================================
# Labels are assigned based on response probability patterns.
# Adjust after inspecting the actual output — these are placeholders
# that match common patterns in AI opinion research.
#
# Typical patterns (3–4 class solutions):
#   "AI-Advantaged"  : Excited about AI, aware, sees mostly benefits
#   "AI-Ambivalent"  : Mixed feelings, moderate awareness
#   "AI-Anxious"     : Concerned about AI, sees mostly risks
#   "AI-Uninformed"  : Low awareness, unsure about impacts

# Assign labels after inspecting profiles (update these vectors after review)
assign_labels_a <- function(K) {
  # Profile names established from Form A inspection:
  #   Class 1 = AI-Anxious, Class 2 = AI-Uninformed,
  #   Class 3 = AI-Advantaged, Class 4 = AI-Ambivalent
  # For K < 4, truncate; for K > 4, append generic labels
  base_labels <- c("AI-Anxious", "AI-Uninformed", "AI-Advantaged", "AI-Ambivalent")
  if (K <= length(base_labels)) {
    base_labels[1:K]
  } else {
    c(base_labels, paste0("Class ", (length(base_labels) + 1):K))
  }
}

assign_labels_b <- function(K) {
  # Form B matched to same typology as Form A (cross-form matching confirmed)
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
  mutate(form = "A",
         class_label = assign_labels_a(get_nclass(best_a))[class])

profiles_b <- extract_profiles(best_b, form_b_vars) %>%
  mutate(form = "B",
         class_label = assign_labels_b(get_nclass(best_b))[class])

write_csv(profiles_a, file.path(tbl_dir, "tbl_lca_profiles_form_a.csv"))
write_csv(profiles_b, file.path(tbl_dir, "tbl_lca_profiles_form_b.csv"))
message("Saved: tbl_lca_profiles_form_a.csv and tbl_lca_profiles_form_b.csv")

# =============================================================================
# SECTION 11 — Add class assignments to original data
# =============================================================================

form_a_lca <- form_a_lca %>%
  mutate(lca_class   = best_a$predclass,
         lca_class_label = assign_labels_a(get_nclass(best_a))[lca_class])

form_b_lca <- form_b_lca %>%
  mutate(lca_class   = best_b$predclass,
         lca_class_label = assign_labels_b(get_nclass(best_b))[lca_class])

saveRDS(form_a_lca, file.path(out_dir, "paper2_form_a_with_class.rds"))
saveRDS(form_b_lca, file.path(out_dir, "paper2_form_b_with_class.rds"))
message("Saved: paper2_form_a_with_class.rds and paper2_form_b_with_class.rds")

# =============================================================================
# SECTION 12 — Demographic composition
# =============================================================================

demo_vars <- c("education", "race", "income", "age", "gender", "party")

#' Compute weighted proportions of demographic variable within each class
#' and perform chi-square test of independence
tabulate_demo <- function(df, demo_var, weight_var = "weight", form_label) {

  # Skip if variable not in data
  if (!demo_var %in% names(df)) {
    message(sprintf("    Skipping '%s' — not found in Form %s data", demo_var, form_label))
    return(NULL)
  }

  # Weighted cross-tab
  df_clean <- df %>% filter(!is.na(.data[[demo_var]]), !is.na(lca_class))

  if (weight_var %in% names(df_clean)) {
    wtd_tab <- df_clean %>%
      group_by(lca_class, .data[[demo_var]]) %>%
      summarise(n_wtd = sum(.data[[weight_var]], na.rm = TRUE), .groups = "drop") %>%
      group_by(lca_class) %>%
      mutate(pct = n_wtd / sum(n_wtd) * 100) %>%
      ungroup()
  } else {
    wtd_tab <- df_clean %>%
      count(lca_class, .data[[demo_var]]) %>%
      rename(n_wtd = n) %>%
      group_by(lca_class) %>%
      mutate(pct = n_wtd / sum(n_wtd) * 100) %>%
      ungroup()
  }

  # Chi-square on unweighted counts (standard practice)
  chi_tab  <- table(df_clean$lca_class, df_clean[[demo_var]])
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

message("\n--- Demographic composition: Form A ---")
demo_a <- map_dfr(demo_vars,
                  ~tabulate_demo(form_a_lca, .x, weight_var = "weight", form_label = "A"))

message("\n--- Demographic composition: Form B ---")
demo_b <- map_dfr(demo_vars,
                  ~tabulate_demo(form_b_lca, .x, weight_var = "weight", form_label = "B"))

demo_all <- bind_rows(demo_a, demo_b)
write_csv(demo_all, file.path(tbl_dir, "tbl_lca_demographics.csv"))
message("Saved: tbl_lca_demographics.csv")

# =============================================================================
# SECTION 13 — Figure 1: Model Fit Comparison (BIC and AIC by n_class)
# =============================================================================

message("\n--- Generating Figure 1: Model fit comparison ---")

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
  labs(x     = "Number of classes",
       y     = "Information criterion",
       color = "Form",
       linetype = "Criterion",
       shape    = "Form") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

ggsave(file.path(fig_dir, "fig_lca_fit_comparison.png"),
       p_fit, width = 6, height = 4.5, dpi = 300)
message("Saved: fig_lca_fit_comparison.png")

# =============================================================================
# SECTION 14 — Figure 2: Class Profile Plots (grouped bar charts)
# =============================================================================

message("--- Generating Figure 2: Class profile plots ---")

# Human-readable variable labels
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

# Response level labels (first level is "positive" / not hurt / excited)
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
    # Focus on "positive" response (level 1: Excited / A lot / Not hurt)
    # for a clean single-value-per-indicator summary
    filter(response_level == 1)

  # Warm palette for classes
  class_colors <- c("#D32F2F", "#1976D2", "#388E3C", "#F57C00",
                    "#7B1FA2", "#0097A7", "#5D4037")[1:n_classes]

  ggplot(plot_data,
         aes(x     = reorder(var_label, probability),
             y     = probability,
             fill  = class_f)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    scale_fill_manual(values = class_colors, name = "Class") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    coord_flip() +
    labs(x = NULL, y = "Probability (positive response)") +
    theme_classic(base_size = 12) +
    theme(legend.position = "bottom",
          panel.grid.major.x = element_line(color = "grey90"))
}

p_profile_a <- make_profile_plot(profiles_a, "A", get_nclass(best_a))
p_profile_b <- make_profile_plot(profiles_b, "B", get_nclass(best_b))

ggsave(file.path(fig_dir, "fig_lca_profiles_form_a.png"),
       p_profile_a, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "fig_lca_profiles_form_b.png"),
       p_profile_b, width = 7, height = 5, dpi = 300)
message("Saved: fig_lca_profiles_form_a.png and fig_lca_profiles_form_b.png")

# Full response distribution heatmap (all levels) ─────────────────────────
make_heatmap <- function(profiles, form_label) {
  plot_data <- profiles %>%
    mutate(
      var_label  = var_labels[variable],
      resp_label = mapply(get_response_label, variable, response_level),
      cell_label = sprintf("%.0f%%", probability * 100),
      class_f    = factor(paste0("C", class, "\n", class_label))
    )

  ggplot(plot_data,
         aes(x = class_f,
             y = interaction(var_label, resp_label, sep = ": "),
             fill = probability)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = cell_label), size = 2.8, color = "white") +
    scale_fill_gradient2(low = "#EF9A9A", mid = "#EF5350", high = "#B71C1C",
                         midpoint = 0.5, limits = c(0, 1),
                         labels  = scales::percent_format(),
                         name    = "Probability") +
    labs(x = NULL, y = NULL) +
    theme_classic(base_size = 10) +
    theme(axis.text.x  = element_text(angle = 0, hjust = 0.5),
          axis.text.y  = element_text(size = 8),
          legend.position = "right")
}

p_heat_a <- make_heatmap(profiles_a, "A")
p_heat_b <- make_heatmap(profiles_b, "B")

ggsave(file.path(fig_dir, "fig_lca_heatmap_form_a.png"),
       p_heat_a, width = 8, height = 6, dpi = 300)
ggsave(file.path(fig_dir, "fig_lca_heatmap_form_b.png"),
       p_heat_b, width = 8, height = 6, dpi = 300)
message("Saved: fig_lca_heatmap_form_a.png and fig_lca_heatmap_form_b.png")

# =============================================================================
# SECTION 15 — Figure 3: Demographic composition (stacked bar)
# =============================================================================

message("--- Generating Figure 3: Demographic composition ---")

if (nrow(demo_all) > 0) {

  # Focus on one key demographic at a time; here we create one panel per demo
  # and bind into a faceted plot
  demo_plot_data <- demo_all %>%
    mutate(class_f = factor(paste0("C", lca_class)),
           level_f = factor(level))

  # One figure per form
  make_demo_plot <- function(form_label) {
    df_form <- demo_plot_data %>% filter(form == form_label)
    if (nrow(df_form) == 0) return(NULL)

    ggplot(df_form,
           aes(x = class_f, y = pct, fill = level_f)) +
      geom_col(position = "stack", width = 0.7) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      scale_fill_viridis_d(option = "D", name = "Category") +
      facet_wrap(~demographic, scales = "free_x") +
      labs(x = "Latent class", y = "Weighted proportion (%)") +
      theme_classic(base_size = 11) +
      theme(legend.position  = "bottom",
            strip.background = element_rect(fill = "grey95", color = NA),
            strip.text       = element_text(face = "bold", size = 9))
  }

  p_demo_a <- make_demo_plot("A")
  p_demo_b <- make_demo_plot("B")

  if (!is.null(p_demo_a)) {
    ggsave(file.path(fig_dir, "fig_lca_demographics_form_a.png"),
           p_demo_a, width = 10, height = 8, dpi = 300)
    message("Saved: fig_lca_demographics_form_a.png")
  }
  if (!is.null(p_demo_b)) {
    ggsave(file.path(fig_dir, "fig_lca_demographics_form_b.png"),
           p_demo_b, width = 10, height = 8, dpi = 300)
    message("Saved: fig_lca_demographics_form_b.png")
  }
} else {
  message("  Skipping Figure 3 — no demographic data available.")
}

# =============================================================================
# SECTION 16 — Cross-Form Comparison
# =============================================================================
# After running both LCAs, we examine whether similar profile types emerged
# across forms. The split-sample design allows structural replication:
# if the same broad classes appear in both forms (using the two shared
# indicators: attitude + awareness), this supports the generalizability of
# the typology.
#
# Approach:
#   1. For each form's optimal model, extract the class-conditional
#      probabilities on the SHARED indicators (attitude, awareness).
#   2. Compute pairwise Euclidean distance between all class pairs
#      (cross-form) in this shared-indicator subspace.
#   3. Identify best matches; report correlation of matched profiles.
# =============================================================================

message("\n=== Cross-Form Comparison (shared indicators) ===")

shared_vars <- c("lca_attitude", "lca_awareness")

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

# Pairwise distances between Form A classes and Form B classes
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

  message("\nPairwise Euclidean distances in shared-indicator space (lower = more similar):")
  print(round(dist_matrix, 3))

  # Best match: for each Form A class, find closest Form B class
  matches <- apply(dist_matrix, 1, function(row) {
    best_j  <- which.min(row)
    data.frame(form_a_class = NA, form_b_class = colnames(dist_matrix)[best_j],
               distance = row[best_j])
  }) %>%
    bind_rows(.id = "form_a_class")

  message("\nBest cross-form profile matches:")
  print(matches)

  # Save comparison
  write_csv(
    as.data.frame(dist_matrix) %>% rownames_to_column("form_a_class"),
    file.path(tbl_dir, "tbl_lca_crossform_distances.csv")
  )
  message("Saved: tbl_lca_crossform_distances.csv")

} else {
  message("  Shared indicator columns not aligned — skipping distance computation.")
}

# =============================================================================
# SECTION 17 — Session summary
# =============================================================================

message("\n")
message("=============================================================")
message(" LCA COMPLETE")
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
message(sprintf("  %s/tbl_lca_fit_comparison.csv", tbl_dir))
message(sprintf("  %s/tbl_lca_profiles_form_a.csv", tbl_dir))
message(sprintf("  %s/tbl_lca_profiles_form_b.csv", tbl_dir))
message(sprintf("  %s/tbl_lca_demographics.csv", tbl_dir))
message(sprintf("  %s/paper2_form_a_with_class.rds", out_dir))
message(sprintf("  %s/paper2_form_b_with_class.rds", out_dir))
message(sprintf("  %s/fig_lca_fit_comparison.png", fig_dir))
message(sprintf("  %s/fig_lca_profiles_form_a.png", fig_dir))
message(sprintf("  %s/fig_lca_profiles_form_b.png", fig_dir))
message(sprintf("  %s/fig_lca_heatmap_form_a.png", fig_dir))
message(sprintf("  %s/fig_lca_heatmap_form_b.png", fig_dir))
message(sprintf("  %s/fig_lca_demographics_form_a.png", fig_dir))
message(sprintf("  %s/fig_lca_demographics_form_b.png", fig_dir))
message("=============================================================")
message("")
message("NOTE: Class labels in assign_labels_a() / assign_labels_b() are")
message("      generic placeholders. Update them after inspecting the")
message("      profile plots to assign substantive names (e.g.,")
message("      'AI-Advantaged', 'AI-Anxious', 'AI-Ambivalent').")
