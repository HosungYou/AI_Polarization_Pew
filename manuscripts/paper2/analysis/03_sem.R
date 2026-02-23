################################################################################
# Paper 2: AI Divide — 03_sem.R
# 3-Step SEM Approach: SES + Race → AI Awareness → Latent Class Membership
#
# Inputs:
#   output/paper2_form_a_with_class.rds   (Form A + LCA class from 02_lca.R)
#   output/paper2_form_b_with_class.rds   (Form B replication)
#
# Outputs:
#   output/tables/tbl_sem01_multinomial_models.csv
#   output/tables/tbl_sem02_ame_by_class.csv
#   output/tables/tbl_sem03_mediation_results.csv
#   output/tables/tbl_sem04_multigroup_comparison.csv
#   output/figures/fig_sem01_ame_by_class.png
#   output/figures/fig_sem02_mediation_diagram.png
#   output/figures/fig_sem03_predicted_probs.png
################################################################################

# ── 0. Packages ───────────────────────────────────────────────────────────────
required_pkgs <- c("tidyverse", "lavaan", "nnet", "marginaleffects",
                   "survey", "ggplot2", "scales", "broom", "patchwork")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing: ", pkg)
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# ── 0. Paths ──────────────────────────────────────────────────────────────────
here_base <- "/Users/hosung/AI_Polarization_Pew/manuscripts/paper2"
dir_out   <- file.path(here_base, "output")
dir_tbl   <- file.path(dir_out, "tables")
dir_fig   <- file.path(dir_out, "figures")

dir.create(dir_tbl, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)

set.seed(20240101)   # reproducibility for bootstrap

################################################################################
# PART 1: DATA PREPARATION
################################################################################

message("\n=== PART 1: Data Preparation ===")

# -- 1a. Load Form A ------------------------------------------------------------
df_raw <- readRDS(file.path(dir_out, "paper2_form_a_with_class.rds"))

# Defensive: make sure lca_class is factor
stopifnot("lca_class" %in% names(df_raw))
df_raw$lca_class <- factor(df_raw$lca_class)

# -- 1b. AI Awareness as ordered numeric ---------------------------------------
# Expect a variable like ai_heard with labels Nothing/A little/A lot
# Try common naming conventions
awareness_var <- if ("ai_heard" %in% names(df_raw)) "ai_heard" else
                 if ("awareness" %in% names(df_raw)) "awareness" else
                 stop("Cannot locate AI awareness variable. Expected 'ai_heard' or 'awareness'.")

df_raw <- df_raw |>
  mutate(
    awareness_num = case_when(
      as.character(.data[[awareness_var]]) %in%
        c("Nothing at all", "Nothing", "1") ~ 1L,
      as.character(.data[[awareness_var]]) %in%
        c("A little", "A little bit", "2") ~ 2L,
      as.character(.data[[awareness_var]]) %in%
        c("A lot", "3") ~ 3L,
      TRUE ~ as.integer(as.character(.data[[awareness_var]]))
    )
  )

# -- 1c. Education dummies (ref = College grad+) --------------------------------
# Expect variable education_cat or educ with levels:
#   "HS or less", "Some college", "College grad", "Postgrad" (or similar)
edu_var <- if ("education_cat" %in% names(df_raw)) "education_cat" else
           if ("education"     %in% names(df_raw)) "education"     else
           if ("educ_cat"      %in% names(df_raw)) "educ_cat"      else
           if ("educ"          %in% names(df_raw)) "educ"          else
           stop("Cannot locate education variable.")

df_raw <- df_raw |>
  mutate(
    edu_hs_or_less   = as.integer(grepl("HS|high school|less",
                                         .data[[edu_var]], ignore.case = TRUE)),
    edu_some_college = as.integer(grepl("some college|2-year|assoc",
                                         .data[[edu_var]], ignore.case = TRUE)),
    # ref = College grad+ (edu_hs_or_less == 0 & edu_some_college == 0)
  )

# -- 1d. Income dummies (ref = Middle) -----------------------------------------
inc_var <- if ("income_tier"  %in% names(df_raw)) "income_tier"  else
           if ("income_cat"   %in% names(df_raw)) "income_cat"   else
           if ("income"       %in% names(df_raw)) "income"       else
           stop("Cannot locate income variable.")

df_raw <- df_raw |>
  mutate(
    income_lower = as.integer(grepl("lower|low|under",
                                     .data[[inc_var]], ignore.case = TRUE)),
    income_upper = as.integer(grepl("upper|high|over",
                                     .data[[inc_var]], ignore.case = TRUE)),
    # ref = Middle
  )

# -- 1e. Race dummies (ref = White NH) -----------------------------------------
race_var <- if ("race_eth"  %in% names(df_raw)) "race_eth"  else
            if ("race_ethn" %in% names(df_raw)) "race_ethn" else
            if ("race"      %in% names(df_raw)) "race"      else
            stop("Cannot locate race/ethnicity variable.")

df_raw <- df_raw |>
  mutate(
    # Use ^pattern to avoid "non-Hispanic" matching "hispanic"
    race_black    = as.integer(grepl("^black|^african",
                                      .data[[race_var]], ignore.case = TRUE)),
    race_hispanic = as.integer(grepl("^hispanic|^latino",
                                      .data[[race_var]], ignore.case = TRUE)),
    race_asian    = as.integer(grepl("^asian",
                                      .data[[race_var]], ignore.case = TRUE)),
    race_other    = as.integer(grepl("^other",
                                      .data[[race_var]], ignore.case = TRUE)),
    # ref = White NH
    race_nonwhite = as.integer(!grepl("^white",
                                       .data[[race_var]], ignore.case = TRUE))
  )

# -- 1f. Age dummies (ref = 30-49) ---------------------------------------------
age_var <- if ("age_cat"  %in% names(df_raw)) "age_cat"  else
           if ("age_group" %in% names(df_raw)) "age_group" else
           if ("age"       %in% names(df_raw)) "age"       else
           stop("Cannot locate age variable.")

df_raw <- df_raw |>
  mutate(
    age_1829   = as.integer(grepl("18.?29|18-29", .data[[age_var]], ignore.case = TRUE)),
    age_5064   = as.integer(grepl("50.?64|50-64", .data[[age_var]], ignore.case = TRUE)),
    age_65plus = as.integer(grepl("65|older",     .data[[age_var]], ignore.case = TRUE)),
    # ref = 30-49
  )

# -- 1g. Gender dummies (ref = Male) -------------------------------------------
gen_var <- if ("gender" %in% names(df_raw)) "gender" else
           if ("sex"    %in% names(df_raw)) "sex"    else
           stop("Cannot locate gender variable.")

df_raw <- df_raw |>
  mutate(
    gender_female = as.integer(grepl("female|woman",
                                      .data[[gen_var]], ignore.case = TRUE)),
    gender_other  = as.integer(grepl("other|non-binary|nonbinary|trans",
                                      .data[[gen_var]], ignore.case = TRUE)),
    # ref = Male
  )

# -- 1h. Party dummies (ref = Dem/Lean Dem) ------------------------------------
party_var <- if ("party_id"  %in% names(df_raw)) "party_id"  else
             if ("party"     %in% names(df_raw)) "party"     else
             if ("partisan"  %in% names(df_raw)) "partisan"  else
             stop("Cannot locate party variable.")

df_raw <- df_raw |>
  mutate(
    party_rep = as.integer(grepl("rep|republican",
                                  .data[[party_var]], ignore.case = TRUE)),
    party_ind = as.integer(grepl("ind|independent",
                                  .data[[party_var]], ignore.case = TRUE)),
    # ref = Dem/Lean Dem
  )

# -- 1i. Binary outcomes per LCA class -----------------------------------------
class_levels <- levels(df_raw$lca_class)
message("LCA classes found: ", paste(class_levels, collapse = ", "))

for (cl in class_levels) {
  safe_name <- janitor::make_clean_names(cl)
  df_raw[[paste0("class_", safe_name)]] <-
    as.integer(df_raw$lca_class == cl)
}

# Keep only complete cases on key vars
key_vars <- c("lca_class", "awareness_num",
              "edu_hs_or_less", "edu_some_college",
              "income_lower", "income_upper",
              "race_black", "race_hispanic", "race_asian",
              "age_1829", "age_5064", "age_65plus",
              "gender_female", "party_rep")

df_a <- df_raw |>
  filter(if_all(all_of(key_vars), ~ !is.na(.))) |>
  as.data.frame()

message("Form A analytic N = ", nrow(df_a))

# Weight variable (use uniform 1 if not present)
wt_var <- if ("weight" %in% names(df_a)) "weight" else {
  warning("No 'weight' variable found — using uniform weights.")
  df_a$weight <- 1
  "weight"
}

################################################################################
# PART 2: MULTINOMIAL LOGISTIC REGRESSION (3-step approach)
################################################################################

message("\n=== PART 2: Multinomial Logistic Regression ===")

# Helper: run nnet::multinom with weights and extract summary
run_multinom <- function(formula_str, data, wt_col = "weight", maxit = 500) {
  fmla <- as.formula(formula_str)
  tryCatch(
    multinom(fmla, data = data, weights = data[[wt_col]],
             maxit = maxit, trace = FALSE),
    error = function(e) {
      message("  multinom error: ", conditionMessage(e))
      NULL
    }
  )
}

# Reference level = largest class
ref_class <- names(sort(table(df_a$lca_class), decreasing = TRUE))[1]
df_a$lca_class <- relevel(df_a$lca_class, ref = ref_class)
message("Reference class set to: ", ref_class)

# Predictor sets
pred_demo  <- "age_1829 + age_5064 + age_65plus + gender_female + party_rep"
pred_race  <- "race_black + race_hispanic + race_asian + race_other"
pred_ses   <- "edu_some_college + edu_hs_or_less + income_lower + income_upper"
pred_aware <- "awareness_num"

# Nested model formulas
f_base     <- paste("lca_class ~", pred_demo, "+", pred_race)
f_ses      <- paste("lca_class ~", pred_demo, "+", pred_race, "+", pred_ses)
f_aware    <- paste("lca_class ~", pred_demo, "+", pred_race, "+", pred_aware)
f_full     <- paste("lca_class ~", pred_demo, "+", pred_race, "+",
                     pred_ses, "+", pred_aware)

message("  Fitting Base model…")
m_base  <- run_multinom(f_base,  df_a)
message("  Fitting SES model…")
m_ses   <- run_multinom(f_ses,   df_a)
message("  Fitting Awareness model…")
m_aware <- run_multinom(f_aware, df_a)
message("  Fitting Full model…")
m_full  <- run_multinom(f_full,  df_a)

# -- Model comparison table ---------------------------------------------------
make_model_row <- function(mod, model_name) {
  if (is.null(mod)) return(NULL)
  ll  <- logLik(mod)
  aic <- AIC(mod)
  bic <- BIC(mod)
  np  <- attr(ll, "df")
  tibble(model = model_name, logLik = as.numeric(ll),
         df = np, AIC = aic, BIC = bic, N = nrow(mod$fitted.values))
}

model_list  <- list(Base = m_base, SES = m_ses,
                    Awareness = m_aware, Full = m_full)
model_rows  <- map2_dfr(model_list, names(model_list), make_model_row)

# LRT vs base
lrt_row <- function(mod, name, ref_mod) {
  if (is.null(mod) || is.null(ref_mod)) return(NULL)
  chi  <- 2 * (as.numeric(logLik(mod)) - as.numeric(logLik(ref_mod)))
  ddf  <- attr(logLik(mod), "df") - attr(logLik(ref_mod), "df")
  pval <- pchisq(chi, df = ddf, lower.tail = FALSE)
  tibble(model = name, LRT_chi2 = chi, LRT_df = ddf, LRT_p = pval)
}

lrt_rows <- bind_rows(
  lrt_row(m_ses,   "SES",      m_base),
  lrt_row(m_aware, "Awareness", m_base),
  lrt_row(m_full,  "Full",     m_base)
)

tbl_models <- left_join(model_rows, lrt_rows, by = "model")
write.csv(tbl_models,
          file.path(dir_tbl, "tbl_sem01_multinomial_models.csv"),
          row.names = FALSE)
message("  Saved tbl_sem01_multinomial_models.csv")

################################################################################
# PART 3: AVERAGE MARGINAL EFFECTS
################################################################################

message("\n=== PART 3: Average Marginal Effects ===")

if (!is.null(m_full)) {
  # marginaleffects::avg_slopes for multinomial nnet objects
  ame_all <- tryCatch({
    marginaleffects::avg_slopes(m_full,
                                variables = c("edu_some_college",
                                              "edu_hs_or_less",
                                              "income_lower",
                                              "income_upper",
                                              "race_black",
                                              "race_hispanic",
                                              "race_asian",
                                              "age_1829",
                                              "age_5064",
                                              "age_65plus",
                                              "gender_female",
                                              "party_rep",
                                              "awareness_num"))
  }, error = function(e) {
    message("  avg_slopes error: ", conditionMessage(e),
            "\n  Falling back to manual AME via predictions…")
    NULL
  })

  if (!is.null(ame_all)) {
    tbl_ame <- as_tibble(ame_all) |>
      select(term, group, estimate, std.error, statistic, p.value,
             conf.low, conf.high)

    write.csv(tbl_ame,
              file.path(dir_tbl, "tbl_sem02_ame_by_class.csv"),
              row.names = FALSE)
    message("  Saved tbl_sem02_ame_by_class.csv")
  } else {
    # Fallback: manual delta-method AME for binary predictors
    manual_ame <- function(model, var, data) {
      d1 <- d0 <- data
      d1[[var]] <- 1
      d0[[var]] <- 0
      p1 <- predict(model, newdata = d1, type = "probs")
      p0 <- predict(model, newdata = d0, type = "probs")
      colMeans(p1 - p0)
    }

    binary_vars <- c("edu_some_college", "edu_hs_or_less",
                     "income_lower", "income_upper",
                     "race_black", "race_hispanic", "race_asian",
                     "age_1829", "age_5064", "age_65plus",
                     "gender_female", "party_rep")

    ame_list <- map(binary_vars, function(v) {
      eff <- manual_ame(m_full, v, df_a)
      tibble(term = v, group = names(eff), estimate = as.numeric(eff))
    })

    tbl_ame <- bind_rows(ame_list)
    write.csv(tbl_ame,
              file.path(dir_tbl, "tbl_sem02_ame_by_class.csv"),
              row.names = FALSE)
    message("  Saved tbl_sem02_ame_by_class.csv (fallback manual AME)")
  }
} else {
  message("  Skipping AME — full model failed to converge.")
  tbl_ame <- NULL
}

################################################################################
# PART 4: MEDIATION ANALYSIS (SES → Awareness → Class)
################################################################################

message("\n=== PART 4: Mediation Analysis (lavaan) ===")

library(lavaan)

# Build one mediation model per focal class (binary outcome)
run_mediation <- function(class_var, data, n_boot = 1000) {
  message("  Mediation for outcome: ", class_var)

  med_spec <- paste0("
    # Stage 1: SES → Awareness (mediator)
    awareness_num ~ a1*edu_some_college + a2*edu_hs_or_less +
                    a3*income_lower     + a4*income_upper     +
                    race_black + race_hispanic + race_asian +
                    age_1829 + age_5064 + age_65plus +
                    gender_female + party_rep

    # Stage 2: Awareness + SES → Class outcome
    ", class_var, " ~ b1*awareness_num +
                       c1*edu_some_college + c2*edu_hs_or_less +
                       c3*income_lower     + c4*income_upper   +
                       race_black + race_hispanic + race_asian +
                       age_1829 + age_5064 + age_65plus +
                       gender_female + party_rep

    # Indirect effects (a * b)
    ind_edu_some  := a1 * b1
    ind_edu_hs    := a2 * b1
    ind_inc_lower := a3 * b1
    ind_inc_upper := a4 * b1

    # Total effects
    total_edu_some  := c1 + a1 * b1
    total_edu_hs    := c2 + a2 * b1
    total_inc_lower := c3 + a3 * b1
    total_inc_upper := c4 + a4 * b1
  ")

  fit <- tryCatch(
    sem(med_spec,
        data       = data,
        estimator  = "ML",
        se         = "bootstrap",
        bootstrap  = n_boot,
        warn       = FALSE),
    error = function(e) {
      message("    lavaan error: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(fit)) return(NULL)

  if (!lavInspect(fit, "converged")) {
    message("    lavaan did not converge for ", class_var)
    return(NULL)
  }

  pe <- parameterEstimates(fit, boot.ci.type = "bca.simple", level = 0.95) |>
    filter(op %in% c("~", ":=")) |>
    mutate(outcome = class_var)

  pe
}

# Identify binary class columns created in Part 1
class_cols <- grep("^class_", names(df_a), value = TRUE)
message("  Binary class columns: ", paste(class_cols, collapse = ", "))

med_results <- map(class_cols, run_mediation, data = df_a, n_boot = 1000)
names(med_results) <- class_cols

# Combine and save
tbl_med <- bind_rows(med_results[!sapply(med_results, is.null)])

if (nrow(tbl_med) > 0) {
  write.csv(tbl_med,
            file.path(dir_tbl, "tbl_sem03_mediation_results.csv"),
            row.names = FALSE)
  message("  Saved tbl_sem03_mediation_results.csv")
} else {
  message("  No mediation results to save — all models failed.")
}

################################################################################
# PART 5: MULTI-GROUP ANALYSIS BY RACE
################################################################################

message("\n=== PART 5: Multi-Group Analysis by Race ===")

race_groups <- list(
  white_nh   = df_a |> filter(race_black == 0, race_hispanic == 0,
                               race_asian == 0, race_other == 0),
  black_nh   = df_a |> filter(race_black == 1),
  hispanic   = df_a |> filter(race_hispanic == 1),
  asian_nh   = df_a |> filter(race_asian == 1)
)

# Minimum N guard
min_n <- 50
race_groups <- keep(race_groups, ~ nrow(.x) >= min_n)
message("  Groups with N >= ", min_n, ": ", paste(names(race_groups), collapse = ", "))

# Formula without race dummies (within-group analysis)
f_norace <- paste("lca_class ~",
                  pred_demo, "+",
                  pred_ses,  "+",
                  pred_aware)

mg_models <- map(race_groups, function(g_data) {
  g_data$lca_class <- relevel(g_data$lca_class, ref = ref_class)
  run_multinom(f_norace, g_data)
})

# Extract coefficients into tidy table
extract_coefs <- function(mod, group_name) {
  if (is.null(mod)) return(NULL)
  s  <- summary(mod)
  cf <- s$coefficients
  se <- s$standard.errors

  map_dfr(rownames(cf), function(cls) {
    tibble(
      race_group = group_name,
      class      = cls,
      term       = colnames(cf),
      estimate   = as.numeric(cf[cls, ]),
      std.error  = as.numeric(se[cls, ])
    ) |>
      mutate(z = estimate / std.error,
             p.value = 2 * pnorm(abs(z), lower.tail = FALSE),
             conf.low  = estimate - 1.96 * std.error,
             conf.high = estimate + 1.96 * std.error)
  })
}

tbl_mg <- map2_dfr(mg_models, names(mg_models), extract_coefs)

if (nrow(tbl_mg) > 0) {
  write.csv(tbl_mg,
            file.path(dir_tbl, "tbl_sem04_multigroup_comparison.csv"),
            row.names = FALSE)
  message("  Saved tbl_sem04_multigroup_comparison.csv")
}

# Wald test: education × race interaction in full sample
message("  Wald test: education × race interaction…")
f_interact <- paste("lca_class ~",
                    pred_demo, "+", pred_race, "+", pred_ses, "+", pred_aware,
                    "+ edu_some_college:race_black + edu_hs_or_less:race_black",
                    "+ edu_some_college:race_hispanic + edu_hs_or_less:race_hispanic")

m_interact <- run_multinom(f_interact, df_a)

if (!is.null(m_interact) && !is.null(m_full)) {
  lrt_interact <- anova(m_full, m_interact, test = "Chisq")
  message("  Interaction LRT p-value: ",
          round(lrt_interact[2, "Pr(Chi)"], 4))
}

################################################################################
# PART 6: FIGURES
################################################################################

message("\n=== PART 6: Figures ===")

# ── Fig 1: AME Forest Plot ───────────────────────────────────────────────────
message("  Drawing fig_sem01_ame_by_class.png…")

if (!is.null(tbl_ame) && nrow(tbl_ame) > 0) {

  # Categorise predictors
  ame_plot <- tbl_ame |>
    mutate(
      predictor_cat = case_when(
        term %in% c("edu_some_college", "edu_hs_or_less",
                    "income_lower", "income_upper") ~ "SES",
        term %in% c("race_black", "race_hispanic",
                    "race_asian", "race_other")      ~ "Race/Ethnicity",
        term %in% c("party_rep", "party_ind")        ~ "Partisan",
        term == "awareness_num"                       ~ "AI Awareness",
        TRUE                                          ~ "Demographic"
      ),
      term_label = recode(term,
        edu_some_college = "Some college",
        edu_hs_or_less   = "HS or less",
        income_lower     = "Lower income",
        income_upper     = "Upper income",
        race_black       = "Black NH",
        race_hispanic    = "Hispanic",
        race_asian       = "Asian NH",
        race_other       = "Other race",
        age_1829         = "Age 18–29",
        age_5064         = "Age 50–64",
        age_65plus       = "Age 65+",
        gender_female    = "Female",
        party_rep        = "Republican",
        awareness_num    = "AI awareness (1→3)"
      )
    )

  cat_colors <- c(
    "SES"          = "#1f77b4",
    "Race/Ethnicity"= "#ff7f0e",
    "Partisan"     = "#d62728",
    "AI Awareness" = "#2ca02c",
    "Demographic"  = "#7f7f7f"
  )

  # Use estimate / conf.low / conf.high if present; else ±1.96*se
  if (!"conf.low" %in% names(ame_plot)) {
    ame_plot <- ame_plot |>
      mutate(conf.low  = estimate - 1.96 * std.error,
             conf.high = estimate + 1.96 * std.error)
  }

  # Remap raw class variable names in 'group' column to display labels
  class_name_map <- c(
    "1" = "AI-Anxious", "2" = "AI-Uninformed",
    "3" = "AI-Advantaged", "4" = "AI-Ambivalent"
  )
  ame_plot <- ame_plot |>
    mutate(group_label = case_when(
      group %in% names(class_name_map) ~ class_name_map[group],
      grepl("^class_x1|^1$", group)   ~ "AI-Anxious",
      grepl("^class_x2|^2$", group)   ~ "AI-Uninformed",
      grepl("^class_x3|^3$", group)   ~ "AI-Advantaged",
      grepl("^class_x4|^4$", group)   ~ "AI-Ambivalent",
      TRUE ~ group
    ))

  p1 <- ggplot(ame_plot,
               aes(x = estimate, y = reorder(term_label, estimate),
                   color = predictor_cat)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.25, linewidth = 0.6) +
    geom_point(size = 2) +
    facet_wrap(~ group_label, scales = "free_x", ncol = 3) +
    scale_color_manual(values = cat_colors, name = "Predictor\ncategory") +
    labs(
      title   = "Average Marginal Effects on Latent Class Membership",
      subtitle = "Full model; 95% CI; reference categories: College grad+, Middle income, White NH, Age 30–49, Male, Democrat",
      x       = "AME on Pr(class membership)",
      y       = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "gray90"),
      legend.position  = "bottom"
    )

  ggsave(file.path(dir_fig, "fig_sem01_ame_by_class.png"),
         p1, width = 12, height = 8, dpi = 300, bg = "white")
  message("  Saved fig_sem01_ame_by_class.png")
} else {
  message("  Skipping fig_sem01 — no AME data.")
}

# ── Fig 2: Mediation Path Diagram ─────────────────────────────────────────────
message("  Drawing fig_sem02_mediation_diagram.png…")

if (nrow(tbl_med) > 0) {
  # Pull one focal class for diagram (AI-Anxious or first class)
  focal_cls <- class_cols[1]
  med_focal <- tbl_med |> filter(outcome == focal_cls)

  # Map raw variable names to display labels for the focal class
  class_display_labels <- c(
    "class_x1" = "AI-Anxious",
    "class_x2" = "AI-Uninformed",
    "class_x3" = "AI-Advantaged",
    "class_x4" = "AI-Ambivalent"
  )
  focal_cls_label <- if (focal_cls %in% names(class_display_labels)) {
    class_display_labels[[focal_cls]]
  } else {
    # Try to match by position: class_cols[1] = Class 1, etc.
    pos <- match(focal_cls, class_cols)
    if (!is.na(pos)) {
      c("AI-Anxious", "AI-Uninformed", "AI-Advantaged", "AI-Ambivalent")[pos]
    } else {
      focal_cls
    }
  }

  # Key path coefficients
  pull_est <- function(lhs, op, rhs, tbl) {
    v <- tbl |>
      filter(lhs == !!lhs, op == !!op, rhs == !!rhs) |>
      pull(est)
    if (length(v) == 0) NA_real_ else round(v[1], 3)
  }

  a1  <- pull_est("awareness_num", "~", "edu_some_college", med_focal)
  a2  <- pull_est("awareness_num", "~", "edu_hs_or_less",   med_focal)
  b1  <- pull_est(focal_cls,        "~", "awareness_num",   med_focal)
  c1  <- pull_est(focal_cls,        "~", "edu_some_college", med_focal)
  c2  <- pull_est(focal_cls,        "~", "edu_hs_or_less",  med_focal)
  ind1 <- pull_est("ind_edu_some", ":=", "", med_focal |>
                     mutate(rhs = ifelse(op == ":=", "", rhs)))
  ind2 <- pull_est("ind_edu_hs",   ":=", "", med_focal |>
                     mutate(rhs = ifelse(op == ":=", "", rhs)))

  # Simple ggplot path diagram
  nodes <- tibble(
    label = c("Education\n(SES)", "AI Awareness", "Class Membership\n(focal)"),
    x     = c(0, 2, 4),
    y     = c(0, 1, 0)
  )

  arrows <- tibble(
    x    = c(0,  0, 2),
    y    = c(0,  0, 1),
    xend = c(2,  4, 4),
    yend = c(1,  0, 0),
    lab  = c(paste0("a\u2081 = ", a1, " (Some coll.)\na\u2082 = ", a2, " (HS or less)"),
             paste0("c\u2081\u2019 = ", c1, " (Some coll.)\nc\u2082\u2019 = ", c2, " (HS or less)"),
             paste0("b = ", b1)),
    mid_x = c(0.9, 2,   3.1),
    mid_y = c(0.65, -0.15, 0.65)
  )

  p2 <- ggplot() +
    geom_segment(data = arrows,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                 color = "gray30", linewidth = 1) +
    geom_label(data = arrows,
               aes(x = mid_x, y = mid_y, label = lab),
               size = 3.5, fill = "white", label.size = 0.2) +
    geom_label(data = nodes,
               aes(x = x, y = y, label = label),
               size = 4, fill = "#d6e8f7", label.size = 0.4,
               fontface = "bold") +
    annotate("text", x = 2, y = -0.4,
             label = paste0("Indirect: Some college = ", round(a1 * b1, 3),
                            "  |  HS or less = ", round(a2 * b1, 3)),
             size = 3.5, color = "#1f77b4") +
    labs(title    = paste("Mediation: SES → AI Awareness → Class Membership"),
         subtitle = paste0("Focal class: ", focal_cls_label,
                           " | Bootstrap SE (n=1000) | ML estimator"),
         caption  = "a = path from education to awareness; b = awareness to class;\nc' = direct effect of education on class; Indirect = a×b") +
    coord_cartesian(xlim = c(-0.5, 4.5), ylim = c(-0.6, 1.4)) +
    theme_void(base_size = 12) +
    theme(plot.title    = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
          plot.caption  = element_text(hjust = 0.5, color = "gray50"))

  ggsave(file.path(dir_fig, "fig_sem02_mediation_diagram.png"),
         p2, width = 9, height = 6, dpi = 300, bg = "white")
  message("  Saved fig_sem02_mediation_diagram.png")
} else {
  message("  Skipping fig_sem02 — no mediation data.")
}

# ── Fig 3: Predicted Probabilities by Education × Race ────────────────────────
message("  Drawing fig_sem03_predicted_probs.png…")

if (!is.null(m_full)) {
  race_labels <- c(
    "white"    = "White NH",
    "black"    = "Black NH",
    "hispanic" = "Hispanic",
    "asian"    = "Asian NH"
  )

  edu_levels <- tibble(
    edu_label        = c("College grad+", "Some college", "HS or less"),
    edu_some_college = c(0, 1, 0),
    edu_hs_or_less   = c(0, 0, 1)
  )

  race_scenarios <- tibble(
    race_label = names(race_labels),
    race_black    = c(0, 1, 0, 0),
    race_hispanic = c(0, 0, 1, 0),
    race_asian    = c(0, 0, 0, 1),
    race_other    = c(0, 0, 0, 0)
  )

  # Marginal scenario grid (hold everything else at median/mode)
  scenario_grid <- crossing(edu_levels, race_scenarios) |>
    mutate(
      awareness_num  = median(df_a$awareness_num, na.rm = TRUE),
      income_lower   = 0, income_upper = 0,
      age_1829       = 0, age_5064 = 0, age_65plus = 0,
      gender_female  = 1,
      party_rep      = 0
    )

  # Class label lookup for facet display
  class_profile_labels <- c(
    "1" = "AI-Anxious", "2" = "AI-Uninformed",
    "3" = "AI-Advantaged", "4" = "AI-Ambivalent"
  )

  pred_probs <- predict(m_full, newdata = scenario_grid, type = "probs") |>
    as_tibble() |>
    bind_cols(scenario_grid |> select(edu_label, race_label)) |>
    pivot_longer(-c(edu_label, race_label),
                 names_to = "lca_class", values_to = "pred_prob") |>
    mutate(
      edu_label  = factor(edu_label,
                           levels = c("HS or less", "Some college", "College grad+")),
      race_label = recode(race_label, !!!race_labels),
      # Remap class labels: handle numeric strings and named factor levels
      lca_class_label = case_when(
        lca_class %in% names(class_profile_labels) ~
          class_profile_labels[lca_class],
        lca_class == levels(df_a$lca_class)[1] ~ "AI-Anxious",
        lca_class == levels(df_a$lca_class)[2] ~ "AI-Uninformed",
        lca_class == levels(df_a$lca_class)[3] ~ "AI-Advantaged",
        lca_class == levels(df_a$lca_class)[4] ~ "AI-Ambivalent",
        TRUE ~ lca_class
      )
    )

  p3 <- ggplot(pred_probs,
               aes(x = edu_label, y = pred_prob,
                   color = race_label, group = race_label)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    facet_wrap(~ lca_class_label, scales = "free_y", ncol = 3) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_brewer(palette = "Set1", name = "Race/Ethnicity") +
    labs(
      title    = "Predicted Probability of Latent Class Membership\nby Education Level and Race/Ethnicity",
      subtitle = "Holding: Middle income, Median AI awareness, Female, Democrat/Lean Dem, Age 30–49",
      x        = "Education level",
      y        = "Predicted probability of class membership"
    ) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x      = element_text(angle = 20, hjust = 1),
      strip.background = element_rect(fill = "gray90"),
      legend.position  = "bottom"
    )

  ggsave(file.path(dir_fig, "fig_sem03_predicted_probs.png"),
         p3, width = 11, height = 7, dpi = 300, bg = "white")
  message("  Saved fig_sem03_predicted_probs.png")
} else {
  message("  Skipping fig_sem03 — full model unavailable.")
}

################################################################################
# PART 7: FORM B REPLICATION
################################################################################

message("\n=== PART 7: Form B Replication ===")

form_b_path <- file.path(dir_out, "paper2_form_b_with_class.rds")

if (file.exists(form_b_path)) {

  message("  Loading Form B…")
  df_b_raw <- readRDS(form_b_path)

  # Apply same recoding as Form A (DRY principle: wrap in a function above
  # in production; here we repeat key steps for clarity)
  df_b_raw$lca_class <- factor(df_b_raw$lca_class)

  # Recode awareness
  df_b_raw <- df_b_raw |>
    mutate(
      awareness_num = case_when(
        as.character(.data[[awareness_var]]) %in%
          c("Nothing at all", "Nothing", "1") ~ 1L,
        as.character(.data[[awareness_var]]) %in%
          c("A little", "A little bit", "2") ~ 2L,
        as.character(.data[[awareness_var]]) %in%
          c("A lot", "3") ~ 3L,
        TRUE ~ as.integer(as.character(.data[[awareness_var]]))
      ),
      edu_hs_or_less   = as.integer(grepl("HS|high school|less",
                                           .data[[edu_var]], ignore.case = TRUE)),
      edu_some_college = as.integer(grepl("some college|2-year|assoc",
                                           .data[[edu_var]], ignore.case = TRUE)),
      income_lower     = as.integer(grepl("lower|low|under",
                                           .data[[inc_var]], ignore.case = TRUE)),
      income_upper     = as.integer(grepl("upper|high|over",
                                           .data[[inc_var]], ignore.case = TRUE)),
      race_black       = as.integer(grepl("^black|^african",  .data[[race_var]], ignore.case = TRUE)),
      race_hispanic    = as.integer(grepl("^hispanic|^latino",.data[[race_var]], ignore.case = TRUE)),
      race_asian       = as.integer(grepl("^asian",          .data[[race_var]], ignore.case = TRUE)),
      race_other       = as.integer(grepl("^other",
                                            .data[[race_var]], ignore.case = TRUE)),
      age_1829         = as.integer(grepl("18.?29|18-29",   .data[[age_var]], ignore.case = TRUE)),
      age_5064         = as.integer(grepl("50.?64|50-64",   .data[[age_var]], ignore.case = TRUE)),
      age_65plus       = as.integer(grepl("65|older",        .data[[age_var]], ignore.case = TRUE)),
      gender_female    = as.integer(grepl("female|woman",    .data[[gen_var]], ignore.case = TRUE)),
      party_rep        = as.integer(grepl("rep|republican",  .data[[party_var]], ignore.case = TRUE))
    )

  if (!"weight" %in% names(df_b_raw)) df_b_raw$weight <- 1

  df_b <- df_b_raw |>
    filter(if_all(all_of(key_vars), ~ !is.na(.))) |>
    as.data.frame()

  # Align reference level with Form A
  if (ref_class %in% levels(df_b$lca_class)) {
    df_b$lca_class <- relevel(df_b$lca_class, ref = ref_class)
  }

  message("  Form B analytic N = ", nrow(df_b))
  message("  Fitting Form B full model…")

  m_full_b <- run_multinom(f_full, df_b)

  if (!is.null(m_full_b)) {
    cf_a <- summary(m_full)$coefficients
    cf_b <- summary(m_full_b)$coefficients

    # Align columns
    shared_cols <- intersect(colnames(cf_a), colnames(cf_b))
    shared_rows <- intersect(rownames(cf_a), rownames(cf_b))

    replication_tbl <- map_dfr(shared_rows, function(cls) {
      map_dfr(shared_cols, function(v) {
        tibble(
          class      = cls,
          term       = v,
          coef_formA = cf_a[cls, v],
          coef_formB = cf_b[cls, v],
          diff       = cf_b[cls, v] - cf_a[cls, v]
        )
      })
    })

    write.csv(replication_tbl,
              file.path(dir_tbl, "tbl_sem05_formb_replication.csv"),
              row.names = FALSE)
    message("  Saved tbl_sem05_formb_replication.csv")
  }

} else {
  message("  Form B file not found — skipping replication.")
}

################################################################################
# FINAL SUMMARY
################################################################################

message("\n=== Output Summary ===")

tables_out <- list.files(dir_tbl, pattern = "^tbl_sem", full.names = FALSE)
figures_out <- list.files(dir_fig, pattern = "^fig_sem", full.names = FALSE)

message("Tables created:")
walk(tables_out, ~ message("  ", .x))

message("Figures created:")
walk(figures_out, ~ message("  ", .x))

message("\n03_sem.R complete.")
