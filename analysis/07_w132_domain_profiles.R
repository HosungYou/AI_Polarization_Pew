# =============================================================================
# 07_w132_domain_profiles.R
# AI Attitude Polarization Study — Pew ATP Wave 132 (Aug 2023)
#
# Purpose:  Analyze W132 domain-specific AI help/hurt perceptions by party.
#           Four analyses:
#             1. Domain help/hurt profiles by party (butterfly + dumbbell charts)
#             2. Survey-weighted chi-square tests (Rao-Scott) + Cramér's V
#             3. Domain clusters / correlation heatmap
#             4. Policing reversal — deeper look by party x demographics
#
# Input:    data/processed/pew_w132_enriched.rds  (preferred)
#           data/raw/W132_Aug23/ATP W132.sav       (fallback)
#
# Outputs:
#   output/figures/fig7_domain_helphurt_profiles.png   (9 x 6 in, 300 dpi)
#   output/figures/fig7b_dumbbell_pct_help.png         (8 x 5 in, 300 dpi)
#   output/figures/fig7c_domain_heatmap.png            (7 x 6 in, 300 dpi)
#   output/figures/fig7d_policing_subgroups.png        (10 x 6 in, 300 dpi)
#   output/tables/tbl_07_domain_profiles.csv
#   output/tables/tbl_07_chisq_tests.csv
#   output/tables/tbl_07_domain_correlations.csv
#   output/tables/tbl_07_policing_subgroups.csv
#
# Author:   [Your Name]
# Date:     2026-02-22
# Journal:  Technological Forecasting & Social Change
# =============================================================================


# =============================================================================
# 0. SETUP
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("07_w132_domain_profiles.R — W132 Domain AI Help/Hurt Analysis\n")
cat(strrep("=", 65), "\n\n")

# ---- 0a. Packages ------------------------------------------------------------

required_pkgs <- c(
  "tidyverse", "haven", "survey", "srvyr",
  "ggplot2", "patchwork", "scales"
)

invisible(lapply(required_pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message(sprintf("Installing package: %s", p))
    install.packages(p, quiet = TRUE)
  }
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

cat("Packages loaded.\n")

# ---- 0b. Paths ---------------------------------------------------------------

project_root <- tryCatch(
  here::here(),
  error = function(e) "/Users/hosung/AI_Polarization_Pew"
)

enriched_path <- file.path(project_root, "data/processed/pew_w132_enriched.rds")
raw_path      <- file.path(project_root, "data/raw/W132_Aug23/ATP W132.sav")
out_figures   <- file.path(project_root, "output/figures")
out_tables    <- file.path(project_root, "output/tables")

dir.create(out_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(out_tables,  recursive = TRUE, showWarnings = FALSE)

# ---- 0c. Color palette -------------------------------------------------------

COL_REP   <- "#B2182B"   # Republican red
COL_DEM   <- "#2166AC"   # Democrat blue
COL_HELP  <- "#4DAC26"   # green — "mostly help"
COL_HURT  <- "#E08214"   # orange — "mostly hurt"
COL_EQUAL <- "#999999"   # gray  — "equal / not sure"

# ---- 0d. Publication theme ---------------------------------------------------

theme_pub <- function(...) {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(size = 11, color = "grey40"),
      plot.caption     = element_text(size = 9,  color = "grey50"),
      axis.title       = element_text(size = 11),
      axis.text        = element_text(size = 10),
      legend.title     = element_text(size = 11),
      legend.text      = element_text(size = 10),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold", size = 11),
      ...
    )
}

# ---- 0e. Domain metadata -----------------------------------------------------

DOMAIN_MAP <- tibble::tribble(
  ~var_suffix, ~domain_label,
  "a", "Finding accurate\ninformation online",
  "b", "People taking care\nof their health",
  "c", "Companies making\nsafe cars/trucks",
  "d", "Companies providing\nquality customer service",
  "e", "Finding products\nand services online",
  "f", "People keeping personal\ninformation private",
  "g", "Police maintaining\npublic safety",
  "h", "Doctors providing\nquality care to patients"
)

# Short labels for heatmap / compact displays
DOMAIN_SHORT <- tibble::tribble(
  ~var_suffix, ~short_label,
  "a", "Accurate info",
  "b", "Health care",
  "c", "Vehicle safety",
  "d", "Customer service",
  "e", "Product search",
  "f", "Privacy",
  "g", "Policing",
  "h", "Doctor quality"
)


# =============================================================================
# 1. DATA LOADING & PREPARATION
# =============================================================================

cat("\n--- Loading data ---\n")

load_and_prep <- function() {

  # ---- Branch: enriched RDS vs raw SPSS ------------------------------------

  if (file.exists(enriched_path)) {

    # == ENRICHED PATH ========================================================
    cat("Loading enriched RDS:", enriched_path, "\n")
    raw <- readRDS(enriched_path)

    # Strip haven labels if present
    raw <- dplyr::mutate(raw, dplyr::across(
      dplyr::where(haven::is.labelled), haven::zap_labels
    ))

    # Enriched convention: AIHLPHRT_a_W132_rc, AIHLPHRT_b_W132_rc, ...
    ai_vars <- paste0("AIHLPHRT_", letters[1:8], "_W132_rc")
    ai_vars <- intersect(ai_vars, names(raw))

    # Rename _rc columns to canonical short names: AIHLPHRT_a, ..., AIHLPHRT_h
    rename_map <- setNames(
      ai_vars,
      paste0("AIHLPHRT_", letters[seq_along(ai_vars)])
    )
    raw <- dplyr::rename(raw, !!!rename_map)

    # Standard columns already present in enriched data
    # party: "Rep/Lean Rep" / "Dem/Lean Dem" (character/factor)
    # weight, age_cat, gender, education, race, income_tier

    df <- raw

    # Rename income_tier -> income for uniformity
    if ("income_tier" %in% names(df) && !"income" %in% names(df)) {
      df <- dplyr::rename(df, income = income_tier)
    }

    # -- 1c. party_label: enriched already has clean string party -------------
    df <- dplyr::mutate(df,
      party_label = factor(party,
                           levels = c("Rep/Lean Rep", "Dem/Lean Dem"))
    )

    # -- 1d. AIHLPHRT values already recoded (1/2/3, NA for refused) ----------
    ai_cols <- paste0("AIHLPHRT_", letters[1:8])
    ai_cols <- intersect(ai_cols, names(df))
    # Ensure numeric (not labelled)
    df <- dplyr::mutate(df,
      dplyr::across(dplyr::all_of(ai_cols), as.numeric)
    )

    # -- 1e. Education: clean labels already present --------------------------
    if ("education" %in% names(df) && is.character(df$education)) {
      df <- dplyr::mutate(df,
        education = dplyr::case_when(
          grepl("College grad", education, ignore.case = TRUE) ~ "College graduate+",
          grepl("Some College|some college", education)         ~ "Some College",
          grepl("H\\.S\\.|HS|high school", education, ignore.case = TRUE) ~ "HS or less",
          TRUE ~ NA_character_
        ),
        education = factor(education,
                           levels = c("College graduate+", "Some College", "HS or less"))
      )
    }

    # -- 1f. Race: clean labels already present --------------------------------
    if ("race" %in% names(df) && is.character(df$race)) {
      df <- dplyr::mutate(df,
        race = dplyr::case_when(
          grepl("White", race)    ~ "White NH",
          grepl("Black", race)    ~ "Black NH",
          grepl("Hispanic", race) ~ "Hispanic",
          grepl("Asian", race)    ~ "Asian NH",
          grepl("Other", race)    ~ "Other",
          TRUE                    ~ NA_character_
        ),
        race = factor(race,
                      levels = c("White NH", "Black NH", "Hispanic", "Asian NH", "Other"))
      )
    }

    # -- 1g. Income: clean labels already present ------------------------------
    if ("income" %in% names(df) && is.character(df$income)) {
      df <- dplyr::mutate(df,
        income = dplyr::case_when(
          grepl("Lower|lower", income)   ~ "Lower",
          grepl("Middle|middle", income) ~ "Middle",
          grepl("Upper|upper", income)   ~ "Upper",
          TRUE ~ NA_character_
        ),
        income = factor(income, levels = c("Lower", "Middle", "Upper"))
      )
    }

  } else {

    # == RAW SPSS PATH =========================================================
    cat("Enriched file not found. Loading raw SPSS:", raw_path, "\n")
    raw <- haven::read_sav(raw_path)
    raw <- dplyr::mutate(raw, dplyr::across(
      dplyr::where(haven::is.labelled), haven::zap_labels
    ))

    # Select relevant columns
    ai_vars   <- paste0("AIHLPHRT_", letters[1:8], "_W132")
    keep_vars <- c(
      ai_vars, "F_PARTYSUM_FINAL", "WEIGHT_W132",
      "CNCEXC_W132", "AI_HEARD_W132",
      "F_AGECAT", "F_GENDER", "F_EDUCCAT",
      "F_RACETHNMOD", "F_INC_TIER2"
    )
    keep_vars <- intersect(keep_vars, names(raw))
    df <- dplyr::select(raw, dplyr::all_of(keep_vars))

    # Rename _W132 suffix off raw columns
    names(df) <- stringr::str_replace_all(names(df), "_W132$", "")

    # Rename raw column names to standard
    if ("F_PARTYSUM_FINAL" %in% names(df))
      df <- dplyr::rename(df, party = F_PARTYSUM_FINAL)
    if ("WEIGHT" %in% names(df))
      df <- dplyr::rename(df, weight = WEIGHT)
    if ("F_AGECAT" %in% names(df))
      df <- dplyr::rename(df, age_cat = F_AGECAT)
    if ("F_GENDER" %in% names(df))
      df <- dplyr::rename(df, gender = F_GENDER)
    if ("F_EDUCCAT" %in% names(df))
      df <- dplyr::rename(df, education = F_EDUCCAT)
    if ("F_RACETHNMOD" %in% names(df))
      df <- dplyr::rename(df, race = F_RACETHNMOD)
    if ("F_INC_TIER2" %in% names(df))
      df <- dplyr::rename(df, income = F_INC_TIER2)

    # Rename AIHLPHRT cols: drop _W132 suffix (already done above)
    # Now AIHLPHRT_a_W132 -> AIHLPHRT_a after str_replace above
    # Verify canonical names
    ai_cols_raw <- names(df)[grepl("^AIHLPHRT_[a-h]$", names(df))]

    # -- party_label -----------------------------------------------------------
    # Raw: 1 = Rep/Lean Rep, 2 = Dem/Lean Dem, 9 = DK/Refused
    df <- dplyr::mutate(df,
      party_label = dplyr::case_when(
        party == 1 ~ "Rep/Lean Rep",
        party == 2 ~ "Dem/Lean Dem",
        TRUE        ~ NA_character_
      ),
      party_label = factor(party_label,
                           levels = c("Rep/Lean Rep", "Dem/Lean Dem"))
    )

    # -- AIHLPHRT: recode 99 -> NA ---------------------------------------------
    ai_cols <- paste0("AIHLPHRT_", letters[1:8])
    ai_cols <- intersect(ai_cols, names(df))
    df <- dplyr::mutate(df,
      dplyr::across(
        dplyr::all_of(ai_cols),
        ~ dplyr::if_else(.x == 99, NA_real_, as.numeric(.x))
      )
    )

    # -- Demographics ----------------------------------------------------------
    if ("education" %in% names(df)) {
      df <- dplyr::mutate(df,
        education = dplyr::case_when(
          education == 1  ~ "College graduate+",
          education == 2  ~ "Some College",
          education == 3  ~ "HS or less",
          TRUE            ~ NA_character_
        ),
        education = factor(education,
                           levels = c("College graduate+", "Some College", "HS or less"))
      )
    }

    if ("race" %in% names(df)) {
      df <- dplyr::mutate(df,
        race = dplyr::case_when(
          race == 1 ~ "White NH",
          race == 2 ~ "Black NH",
          race == 3 ~ "Hispanic",
          race == 4 ~ "Other",
          race == 5 ~ "Asian NH",
          TRUE      ~ NA_character_
        ),
        race = factor(race,
                      levels = c("White NH", "Black NH", "Hispanic", "Asian NH", "Other"))
      )
    }

    if ("age_cat" %in% names(df)) {
      df <- dplyr::mutate(df,
        age_cat = dplyr::case_when(
          age_cat == 1 ~ "18-29",
          age_cat == 2 ~ "30-49",
          age_cat == 3 ~ "50-64",
          age_cat == 4 ~ "65+",
          TRUE         ~ NA_character_
        ),
        age_cat = factor(age_cat, levels = c("18-29", "30-49", "50-64", "65+"))
      )
    }

    if ("income" %in% names(df)) {
      df <- dplyr::mutate(df,
        income = dplyr::case_when(
          income == 1 ~ "Lower",
          income == 2 ~ "Middle",
          income == 3 ~ "Upper",
          TRUE        ~ NA_character_
        ),
        income = factor(income, levels = c("Lower", "Middle", "Upper"))
      )
    }
  }

  df
}

df <- load_and_prep()
cat("Data loaded:", nrow(df), "rows,", ncol(df), "columns\n")
cat("Party distribution (before NA filter):\n")
print(table(df$party_label, useNA = "ifany"))


# =============================================================================
# 2. SURVEY DESIGN
# =============================================================================

cat("\n--- Building survey design ---\n")

# Use only rows with valid party and weight
df_svy <- df |>
  dplyr::filter(!is.na(party_label), !is.na(weight), weight > 0)

svy_design <- srvyr::as_survey_design(
  df_svy,
  weights = weight
)

cat("Survey design created. N (partisan subsample):", nrow(df_svy), "\n")


# =============================================================================
# ANALYSIS 1: DOMAIN HELP/HURT PROFILES BY PARTY
# =============================================================================

cat("\n", strrep("-", 60), "\n")
cat("ANALYSIS 1: Domain help/hurt profiles by party\n")
cat(strrep("-", 60), "\n")

ai_cols <- paste0("AIHLPHRT_", letters[1:8])
ai_cols <- intersect(ai_cols, names(df_svy))

# ---- Helper: weighted proportions for one domain by party ------------------

domain_props <- function(svy, var, domain_sfx) {

  svy |>
    dplyr::filter(!is.na(.data[[var]])) |>
    dplyr::mutate(
      response = dplyr::case_when(
        .data[[var]] == 1 ~ "help",
        .data[[var]] == 2 ~ "hurt",
        .data[[var]] == 3 ~ "equal",
        TRUE              ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(response)) |>
    srvyr::group_by(party_label, response) |>
    srvyr::summarise(
      pct = srvyr::survey_mean(vartype = "ci", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(var_suffix = domain_sfx)
}

cat("Computing weighted proportions for all 8 domains...\n")

domain_results <- purrr::map2_dfr(
  ai_cols,
  letters[1:length(ai_cols)],
  ~ domain_props(svy_design, .x, .y)
)

# Join domain labels
domain_results <- domain_results |>
  dplyr::left_join(DOMAIN_MAP, by = "var_suffix") |>
  dplyr::left_join(DOMAIN_SHORT, by = "var_suffix")

cat("Domain proportions computed.\n")


# ---- Compute partisan gaps --------------------------------------------------

gap_tbl <- domain_results |>
  dplyr::select(var_suffix, party_label, response, pct) |>
  tidyr::pivot_wider(names_from = c(party_label, response),
                     values_from = pct,
                     names_sep   = "__") |>
  dplyr::rename_with(~ gsub("[/ ]", "_", .x)) |>
  dplyr::rename_with(~ gsub("__", "_", .x))

# Identify actual column names (they depend on exact factor levels)
cat("Gap table columns:", paste(names(gap_tbl), collapse = ", "), "\n")

# Compute gap in "hurt" pct: Rep hurt – Dem hurt (positive = Rep more pessimistic)
rep_hurt_col <- names(gap_tbl)[grepl("Rep.*hurt|hurt.*Rep", names(gap_tbl))]
dem_hurt_col <- names(gap_tbl)[grepl("Dem.*hurt|hurt.*Dem", names(gap_tbl))]
rep_help_col <- names(gap_tbl)[grepl("Rep.*help|help.*Rep", names(gap_tbl))]
dem_help_col <- names(gap_tbl)[grepl("Dem.*help|help.*Dem", names(gap_tbl))]

cat("Rep hurt col:", rep_hurt_col, "\n")
cat("Dem hurt col:", dem_hurt_col, "\n")

if (length(rep_hurt_col) == 1 && length(dem_hurt_col) == 1) {
  gap_tbl <- gap_tbl |>
    dplyr::mutate(
      hurt_gap = .data[[rep_hurt_col]] - .data[[dem_hurt_col]],
      help_gap = .data[[rep_help_col]] - .data[[dem_help_col]]
    )
}

# Domain order: sort by hurt_gap descending (Rep more pessimistic at top)
# Policing will appear at bottom (negative gap = Rep LESS pessimistic)
domain_order <- gap_tbl |>
  dplyr::arrange(desc(hurt_gap)) |>
  dplyr::pull(var_suffix)

domain_results <- domain_results |>
  dplyr::mutate(
    var_suffix = factor(var_suffix, levels = domain_order),
    domain_label = factor(domain_label,
      levels = DOMAIN_MAP$domain_label[match(domain_order, DOMAIN_MAP$var_suffix)])
  )


# ---- Save summary table -----------------------------------------------------

tbl_profiles <- domain_results |>
  dplyr::select(var_suffix, short_label, domain_label, party_label, response, pct, pct_low, pct_upp) |>
  dplyr::arrange(var_suffix, party_label, response)

readr::write_csv(tbl_profiles,
                 file.path(out_tables, "tbl_07_domain_profiles.csv"))
cat("Saved: tbl_07_domain_profiles.csv\n")


# ---- Figure 7: Butterfly / diverging stacked bar chart ----------------------

# Reshape for diverging chart:
# "hurt" goes LEFT (negative x), "help" goes RIGHT (positive x)
# "equal" straddles center (shown lightly)

butterfly_df <- domain_results |>
  dplyr::mutate(
    # Signed percentage: hurt → negative, help → positive, equal stays for reference
    pct_signed = dplyr::case_when(
      response == "hurt"  ~ -pct * 100,
      response == "help"  ~  pct * 100,
      response == "equal" ~  pct * 100,
      TRUE                ~  0
    ),
    response_f = factor(response, levels = c("hurt", "equal", "help"),
                        labels = c("Mostly hurt", "Equal/Not sure", "Mostly help")),
    party_label = factor(party_label, levels = c("Rep/Lean Rep", "Dem/Lean Dem"))
  )

# Policing annotation flag
butterfly_df <- butterfly_df |>
  dplyr::mutate(is_policing = var_suffix == "g")

# Color fill by response type (same for both parties)
fill_colors <- c(
  "Mostly hurt"     = COL_HURT,
  "Equal/Not sure"  = COL_EQUAL,
  "Mostly help"     = COL_HELP
)

# Build diverging chart: facet by party
fig7 <- butterfly_df |>
  dplyr::filter(response != "equal") |>   # show help/hurt as diverging bars
  ggplot2::ggplot(ggplot2::aes(
    x    = pct_signed,
    y    = var_suffix,
    fill = response_f
  )) +
  ggplot2::geom_col(width = 0.7, alpha = 0.9) +
  # Highlight policing reversal with a box
  ggplot2::geom_rect(
    data = dplyr::filter(butterfly_df, is_policing, response == "help"),
    ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = as.numeric(var_suffix) - 0.45,
                 ymax = as.numeric(var_suffix) + 0.45),
    fill = NA, color = "#F4A582", linewidth = 0.8, linetype = "dashed",
    inherit.aes = FALSE
  ) +
  ggplot2::geom_vline(xintercept = 0, linewidth = 0.5, color = "grey30") +
  ggplot2::scale_x_continuous(
    labels = function(x) paste0(abs(x), "%"),
    breaks = seq(-80, 80, by = 20),
    limits = c(-85, 85)
  ) +
  ggplot2::scale_y_discrete(
    labels = setNames(
      DOMAIN_MAP$domain_label,
      DOMAIN_MAP$var_suffix
    )
  ) +
  ggplot2::scale_fill_manual(values = fill_colors) +
  ggplot2::facet_wrap(~ party_label, ncol = 2) +
  ggplot2::labs(
    title    = "Figure 7. Domain-Specific AI Help/Hurt Perceptions by Party",
    subtitle = paste0(
      "Survey-weighted proportions. Dashed box highlights policing reversal\n",
      "(Republicans more optimistic about AI for police)."
    ),
    x        = expression(phantom(x) %<-% "Mostly hurt" ~ phantom(x) ~ "Mostly help" %->% phantom(x)),
    y        = NULL,
    fill     = NULL,
    caption  = "Source: Pew Research Center ATP Wave 132 (Aug 2023). N \u2248 5,580 with valid responses."
  ) +
  theme_pub() +
  ggplot2::theme(
    legend.position  = "bottom",
    panel.grid.major.y = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(out_figures, "fig7_domain_helphurt_profiles.png"),
  plot     = fig7,
  width    = 9, height = 6, dpi = 300
)
cat("Saved: fig7_domain_helphurt_profiles.png\n")


# ---- Figure 7b: Dumbbell / paired dot plot — % "Mostly help" ----------------

dumbbell_df <- domain_results |>
  dplyr::filter(response == "help") |>
  dplyr::select(var_suffix, domain_label, short_label, party_label, pct, pct_low, pct_upp) |>
  dplyr::mutate(pct = pct * 100, pct_low = pct_low * 100, pct_upp = pct_upp * 100)

# Compute partisan gap for annotation
dumbbell_wide <- dumbbell_df |>
  tidyr::pivot_wider(
    id_cols     = c(var_suffix, short_label, domain_label),
    names_from  = party_label,
    values_from = c(pct, pct_low, pct_upp)
  ) |>
  dplyr::rename_with(~ gsub("[/ ]", "_", .x))

# Identify rep and dem pct columns
rep_pct_col <- names(dumbbell_wide)[grepl("^pct_Rep", names(dumbbell_wide))]
dem_pct_col <- names(dumbbell_wide)[grepl("^pct_Dem", names(dumbbell_wide))]

if (length(rep_pct_col) == 1 && length(dem_pct_col) == 1) {
  dumbbell_wide <- dumbbell_wide |>
    dplyr::mutate(
      gap       = .data[[rep_pct_col]] - .data[[dem_pct_col]],
      midpoint  = (.data[[rep_pct_col]] + .data[[dem_pct_col]]) / 2,
      gap_label = sprintf("%+.1f pp", gap),
      is_policing = var_suffix == "g"
    )
}

# Apply domain ordering
dumbbell_df <- dumbbell_df |>
  dplyr::mutate(var_suffix = factor(var_suffix, levels = domain_order))
dumbbell_wide <- dumbbell_wide |>
  dplyr::mutate(var_suffix = factor(var_suffix, levels = domain_order))

fig7b <- ggplot2::ggplot() +
  # Connecting segment
  ggplot2::geom_segment(
    data = dumbbell_wide,
    ggplot2::aes(
      x    = .data[[dem_pct_col]],
      xend = .data[[rep_pct_col]],
      y    = var_suffix,
      yend = var_suffix,
      color = is_policing
    ),
    linewidth = 1.2
  ) +
  ggplot2::scale_color_manual(
    values = c("TRUE" = "#F4A582", "FALSE" = "grey70"),
    guide  = "none"
  ) +
  # Dem dots
  ggplot2::geom_point(
    data = dplyr::filter(dumbbell_df, party_label == "Dem/Lean Dem"),
    ggplot2::aes(x = pct, y = var_suffix),
    color = COL_DEM, size = 3.5
  ) +
  # Rep dots
  ggplot2::geom_point(
    data = dplyr::filter(dumbbell_df, party_label == "Rep/Lean Rep"),
    ggplot2::aes(x = pct, y = var_suffix),
    color = COL_REP, size = 3.5
  ) +
  # Gap label
  {if (exists("dumbbell_wide") && "gap_label" %in% names(dumbbell_wide))
    ggplot2::geom_text(
      data = dumbbell_wide,
      ggplot2::aes(x = midpoint, y = var_suffix, label = gap_label),
      vjust = -0.8, size = 3.2, color = "grey30"
    )
  } +
  # Policing reversal annotation — reference factor level numerically
  ggplot2::annotate(
    "text",
    x     = Inf,
    y     = which(levels(factor(dumbbell_df$var_suffix,
                                levels = domain_order)) == "g"),
    label = "Policing reversal:\nRep more optimistic",
    hjust = 1.05, vjust = 0.5, size = 3, color = "#B2182B", fontface = "italic"
  ) +
  ggplot2::scale_x_continuous(
    labels = scales::label_percent(scale = 1),
    limits = c(20, 85)
  ) +
  ggplot2::scale_y_discrete(
    labels = setNames(DOMAIN_MAP$domain_label, DOMAIN_MAP$var_suffix)
  ) +
  ggplot2::labs(
    title    = 'Figure 7b. % "Mostly Help": Republican vs. Democrat by Domain',
    subtitle = paste0(
      "Survey-weighted estimates. \u25cf Democrat (blue)  \u25cf Republican (red).\n",
      "Gap labels show Rep \u2212 Dem difference in percentage points."
    ),
    x        = '% saying AI will "mostly help"',
    y        = NULL,
    caption  = "Source: Pew Research Center ATP Wave 132 (Aug 2023)."
  ) +
  theme_pub() +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

ggplot2::ggsave(
  filename = file.path(out_figures, "fig7b_dumbbell_pct_help.png"),
  plot     = fig7b,
  width    = 8, height = 5, dpi = 300
)
cat("Saved: fig7b_dumbbell_pct_help.png\n")


# =============================================================================
# ANALYSIS 2: SURVEY-WEIGHTED CHI-SQUARE TESTS (RAO-SCOTT) + CRAMÉR'S V
# =============================================================================

cat("\n", strrep("-", 60), "\n")
cat("ANALYSIS 2: Rao-Scott chi-square tests + Cramér's V\n")
cat(strrep("-", 60), "\n")

# Cramér's V helper (from observed counts / expected)
cramers_v <- function(observed_table) {
  chi2  <- suppressWarnings(chisq.test(observed_table)$statistic)
  n     <- sum(observed_table)
  k     <- min(nrow(observed_table), ncol(observed_table))
  v     <- sqrt(chi2 / (n * (k - 1)))
  unname(v)
}

chisq_results <- purrr::map_dfr(
  seq_along(ai_cols),
  function(i) {

    col  <- ai_cols[i]
    sfx  <- letters[i]

    sub  <- df_svy |>
      dplyr::filter(!is.na(.data[[col]]), !is.na(party_label), .data[[col]] != 99) |>
      dplyr::mutate(
        response = dplyr::case_when(
          .data[[col]] == 1 ~ "help",
          .data[[col]] == 2 ~ "hurt",
          .data[[col]] == 3 ~ "equal"
        )
      ) |>
      dplyr::filter(!is.na(response))

    svy_sub <- srvyr::as_survey_design(sub, weights = weight)

    # Rao-Scott chi-square via svychisq
    tbl_svy <- survey::svytable(
      stats::as.formula(paste0("~ party_label + response")),
      design = svy_sub
    )

    rs_test <- tryCatch(
      survey::svychisq(
        stats::as.formula(paste0("~ party_label + response")),
        design   = svy_sub,
        statistic = "F"
      ),
      error = function(e) NULL
    )

    chisq_val <- if (!is.null(rs_test)) unname(rs_test$statistic) else NA_real_
    pval      <- if (!is.null(rs_test)) rs_test$p.value                else NA_real_
    df_val    <- if (!is.null(rs_test)) rs_test$parameter["denom"]     else NA_real_

    # Cramér's V from unweighted table for effect size
    raw_tbl <- table(
      sub$party_label,
      sub$response
    )
    v <- tryCatch(cramers_v(raw_tbl), error = function(e) NA_real_)

    # Weighted % help and % hurt by party
    pct_tbl <- svy_sub |>
      srvyr::group_by(party_label, response) |>
      srvyr::summarise(pct = srvyr::survey_mean(vartype = NULL), .groups = "drop") |>
      tidyr::pivot_wider(names_from = c(party_label, response),
                         values_from = pct,
                         names_sep   = "_") |>
      dplyr::rename_with(~ gsub("[/ ]", "_", .x))

    # Extract percentages robustly
    get_pct <- function(df, pattern) {
      col <- names(df)[grepl(pattern, names(df), ignore.case = TRUE)]
      if (length(col) == 1) round(df[[col]] * 100, 1) else NA_real_
    }

    tibble::tibble(
      domain       = DOMAIN_SHORT$short_label[DOMAIN_SHORT$var_suffix == sfx],
      var_suffix   = sfx,
      Rep_pct_help = get_pct(pct_tbl, "Rep.*help"),
      Dem_pct_help = get_pct(pct_tbl, "Dem.*help"),
      Rep_pct_hurt = get_pct(pct_tbl, "Rep.*hurt"),
      Dem_pct_hurt = get_pct(pct_tbl, "Dem.*hurt"),
      help_gap_pp  = Rep_pct_help - Dem_pct_help,
      hurt_gap_pp  = Rep_pct_hurt - Dem_pct_hurt,
      F_stat       = round(chisq_val, 3),
      df_denom     = round(df_val, 1),
      p_value      = round(pval, 4),
      p_fmt        = ifelse(!is.na(pval) & pval < 0.001, "<0.001", sprintf("%.3f", pval)),
      sig          = dplyr::case_when(
                       pval < 0.001 ~ "***",
                       pval < 0.01  ~ "**",
                       pval < 0.05  ~ "*",
                       pval < 0.10  ~ ".",
                       TRUE         ~ ""
                     ),
      cramers_v    = round(v, 3)
    )
  }
)

cat("\nChi-square results:\n")
print(dplyr::select(chisq_results, domain, Rep_pct_help, Dem_pct_help,
                    hurt_gap_pp, F_stat, p_fmt, sig, cramers_v))

readr::write_csv(chisq_results,
                 file.path(out_tables, "tbl_07_chisq_tests.csv"))
cat("Saved: tbl_07_chisq_tests.csv\n")


# =============================================================================
# ANALYSIS 3: DOMAIN CLUSTERS — CORRELATION HEATMAP
# =============================================================================

cat("\n", strrep("-", 60), "\n")
cat("ANALYSIS 3: Domain correlations and clustering\n")
cat(strrep("-", 60), "\n")

# W132 used a SPLIT-SAMPLE design:
#   Form A (items a-d): ~5,590 respondents
#   Form B (items e-h): ~5,549 respondents
# Cross-form correlations are structurally impossible (no respondent answered both).
# Strategy: compute two separate 4x4 weighted correlation matrices, then
# display them side by side and combine into one annotated heatmap.

cat("Note: W132 split-sample design — items a-d and e-h on different respondent halves.\n")
cat("Computing within-form correlation matrices separately.\n")

ai_cols_present <- intersect(paste0("AIHLPHRT_", letters[1:8]), names(df))

# Helper: build weighted correlation matrix for a set of columns
wt_cormat <- function(data, cols, wt_col = "weight") {
  sub <- data |>
    dplyr::select(dplyr::all_of(c(cols, wt_col))) |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(cols), ~ !is.na(.x)),
                  !is.na(.data[[wt_col]]), .data[[wt_col]] > 0) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(cols),
      ~ dplyr::case_when(
          .x == 2           ~ 1L,
          .x %in% c(1L, 3L) ~ 0L,
          TRUE              ~ NA_integer_
        )
    )) |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(cols), ~ !is.na(.x)))

  cat("  N for", paste(cols, collapse = "/"), ":", nrow(sub), "\n")
  if (nrow(sub) < 10) return(diag(length(cols)))

  mat <- as.matrix(sub[, cols])
  wts <- sub[[wt_col]]
  cov_res <- stats::cov.wt(mat, wt = wts, cor = TRUE)
  cov_res$cor
}

# Form A: items a–d
form_a_cols <- intersect(paste0("AIHLPHRT_", letters[1:4]), ai_cols_present)
form_b_cols <- intersect(paste0("AIHLPHRT_", letters[5:8]), ai_cols_present)

cor_a <- wt_cormat(df, form_a_cols)
cor_b <- wt_cormat(df, form_b_cols)

# Label matrices with short names
label_mat <- function(mat, cols) {
  short <- DOMAIN_SHORT$short_label[
    match(gsub("AIHLPHRT_", "", cols), DOMAIN_SHORT$var_suffix)
  ]
  rownames(mat) <- colnames(mat) <- short
  mat
}
cor_a <- label_mat(cor_a, form_a_cols)
cor_b <- label_mat(cor_b, form_b_cols)

# Build a combined 8x8 matrix with NA for cross-form cells
all_short <- DOMAIN_SHORT$short_label[
  match(gsub("AIHLPHRT_", "", ai_cols_present), DOMAIN_SHORT$var_suffix)
]
cor_mat <- matrix(NA_real_, nrow = 8, ncol = 8,
                  dimnames = list(all_short, all_short))
a_idx <- match(rownames(cor_a), all_short)
b_idx <- match(rownames(cor_b), all_short)
cor_mat[a_idx, a_idx] <- cor_a
cor_mat[b_idx, b_idx] <- cor_b

cat("Form A correlation matrix:\n"); print(round(cor_a, 3))
cat("Form B correlation matrix:\n"); print(round(cor_b, 3))

cat("Weighted correlation matrix:\n")
print(round(cor_mat, 3))

# Save correlation matrix
cor_df <- as.data.frame(cor_mat) |>
  tibble::rownames_to_column("domain")

readr::write_csv(cor_df,
                 file.path(out_tables, "tbl_07_domain_correlations.csv"))
cat("Saved: tbl_07_domain_correlations.csv\n")

# ---- Heatmap (two panels — one per form) ------------------------------------
# W132 split-sample: cluster within each 4x4 block separately.

make_heatmap_panel <- function(cor_matrix, form_label) {
  # Hierarchical clustering order within this form
  hc_ord <- hclust(as.dist(1 - cor_matrix))$order
  lev    <- rownames(cor_matrix)[hc_ord]

  cor_matrix |>
    as.data.frame() |>
    tibble::rownames_to_column("domain") |>
    tidyr::pivot_longer(-domain, names_to = "domain2", values_to = "r") |>
    dplyr::mutate(
      domain  = factor(domain,  levels = lev),
      domain2 = factor(domain2, levels = lev)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = domain2, y = domain, fill = r)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", r)),
      size = 3.5, color = "black"
    ) +
    ggplot2::scale_fill_gradient2(
      low      = "#4DAC26",
      mid      = "white",
      high     = "#B2182B",
      midpoint = 0,
      limits   = c(-0.1, 1),
      name     = "r"
    ) +
    ggplot2::labs(title = form_label, x = NULL, y = NULL) +
    theme_pub() +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(angle = 35, hjust = 1),
      panel.grid.major = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(size = 11, face = "bold")
    )
}

panel_a <- make_heatmap_panel(
  cor_a,
  "Form A: Info / Health / Vehicle safety / Customer service"
)
panel_b <- make_heatmap_panel(
  cor_b,
  "Form B: Product search / Privacy / Policing / Doctor quality"
)

fig7c <- panel_a + panel_b +
  patchwork::plot_annotation(
    title    = "Figure 7c. Within-Form Correlation Matrices of AI Help/Hurt Domains",
    subtitle = paste0(
      "Binary hurt indicator (1 = mostly hurt, 0 = help or equal).\n",
      "W132 split-sample design: items a\u2013d and e\u2013h asked to separate respondent halves."
    ),
    caption  = "Source: Pew Research Center ATP Wave 132 (Aug 2023).",
    theme    = theme_pub()
  )

ggplot2::ggsave(
  filename = file.path(out_figures, "fig7c_domain_heatmap.png"),
  plot     = fig7c,
  width    = 10, height = 5, dpi = 300
)
cat("Saved: fig7c_domain_heatmap.png\n")


# =============================================================================
# ANALYSIS 4: THE POLICING REVERSAL — DEEPER LOOK
# =============================================================================

cat("\n", strrep("-", 60), "\n")
cat("ANALYSIS 4: Policing reversal by party x demographics\n")
cat(strrep("-", 60), "\n")

policing_col <- "AIHLPHRT_g"

if (!policing_col %in% names(df_svy)) {
  cat("WARNING: policing column not found, skipping Analysis 4.\n")
} else {

  # Subset to valid policing responses
  df_pol <- df_svy |>
    dplyr::filter(!is.na(.data[[policing_col]]),
                  !is.na(party_label),
                  .data[[policing_col]] != 99) |>
    dplyr::mutate(
      pol_help = as.integer(.data[[policing_col]] == 1),
      pol_hurt = as.integer(.data[[policing_col]] == 2)
    )

  svy_pol <- srvyr::as_survey_design(df_pol, weights = weight)

  # ---- 4a. Party x Education -------------------------------------------------

  pol_educ <- NULL
  if ("education" %in% names(df_pol)) {
    pol_educ <- svy_pol |>
      dplyr::filter(!is.na(education)) |>
      srvyr::group_by(party_label, education) |>
      srvyr::summarise(
        pct_help = srvyr::survey_mean(pol_help, vartype = "ci", na.rm = TRUE),
        pct_hurt = srvyr::survey_mean(pol_hurt, vartype = "ci", na.rm = TRUE),
        n        = srvyr::unweighted(dplyr::n()),
        .groups  = "drop"
      ) |>
      dplyr::mutate(subgroup_type = "Education", subgroup = as.character(education))
    cat("Party x Education computed.\n")
  } else {
    cat("Education variable not available; skipping party x education.\n")
  }

  # ---- 4b. Party x Race ------------------------------------------------------

  pol_race <- NULL
  if ("race" %in% names(df_pol)) {
    pol_race <- svy_pol |>
      dplyr::filter(!is.na(race)) |>
      srvyr::group_by(party_label, race) |>
      srvyr::summarise(
        pct_help = srvyr::survey_mean(pol_help, vartype = "ci", na.rm = TRUE),
        pct_hurt = srvyr::survey_mean(pol_hurt, vartype = "ci", na.rm = TRUE),
        n        = srvyr::unweighted(dplyr::n()),
        .groups  = "drop"
      ) |>
      dplyr::mutate(subgroup_type = "Race/Ethnicity", subgroup = as.character(race))
    cat("Party x Race computed.\n")
  } else {
    cat("Race variable not available; skipping party x race.\n")
  }

  # ---- 4c. Combine and save --------------------------------------------------

  pol_combined <- dplyr::bind_rows(pol_educ, pol_race) |>
    dplyr::select(subgroup_type, subgroup, party_label,
                  pct_help, pct_help_low, pct_help_upp,
                  pct_hurt, pct_hurt_low, pct_hurt_upp, n) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("pct_"), ~ round(.x * 100, 1))
    )

  readr::write_csv(pol_combined,
                   file.path(out_tables, "tbl_07_policing_subgroups.csv"))
  cat("Saved: tbl_07_policing_subgroups.csv\n")

  # ---- 4d. Figure 7d: Policing subgroup chart --------------------------------

  if (!is.null(pol_combined) && nrow(pol_combined) > 0) {

    pol_plot_df <- pol_combined |>
      dplyr::mutate(
        party_label  = factor(party_label,
                              levels = c("Rep/Lean Rep", "Dem/Lean Dem")),
        subgroup     = factor(subgroup),
        subgroup_type = factor(subgroup_type,
                               levels = c("Education", "Race/Ethnicity"))
      )

    fig7d <- pol_plot_df |>
      ggplot2::ggplot(ggplot2::aes(
        x     = pct_help,
        y     = subgroup,
        color = party_label,
        group = party_label
      )) +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = pct_help_low, xmax = pct_help_upp),
        width = 0.2, linewidth = 0.7, alpha = 0.6,
        orientation = "y"
      ) +
      ggplot2::geom_point(size = 3.2) +
      ggplot2::scale_color_manual(
        values = c("Rep/Lean Rep" = COL_REP, "Dem/Lean Dem" = COL_DEM),
        name   = "Party"
      ) +
      ggplot2::scale_x_continuous(
        labels = scales::label_percent(scale = 1),
        limits = c(0, 70)
      ) +
      ggplot2::facet_wrap(~ subgroup_type, scales = "free_y", ncol = 2) +
      ggplot2::labs(
        title    = "Figure 7d. AI for Policing — % 'Mostly Help' by Party and Demographic Subgroup",
        subtitle = paste0(
          "Supplementary figure. 95% CIs shown. Republican optimism about AI\n",
          "for police maintenance varies by education and race/ethnicity."
        ),
        x        = '% saying AI will "mostly help" police maintain public safety',
        y        = NULL,
        caption  = "Source: Pew Research Center ATP Wave 132 (Aug 2023)."
      ) +
      theme_pub() +
      ggplot2::theme(
        legend.position    = "bottom",
        panel.grid.major.y = ggplot2::element_blank()
      )

    ggplot2::ggsave(
      filename = file.path(out_figures, "fig7d_policing_subgroups.png"),
      plot     = fig7d,
      width    = 10, height = 6, dpi = 300
    )
    cat("Saved: fig7d_policing_subgroups.png\n")
  }
}


# =============================================================================
# 5. COMPLETION SUMMARY
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("07_w132_domain_profiles.R — COMPLETE\n")
cat(strrep("=", 65), "\n")

cat("\nOutputs written:\n")
cat("  Figures:\n")
cat("    output/figures/fig7_domain_helphurt_profiles.png\n")
cat("    output/figures/fig7b_dumbbell_pct_help.png\n")
cat("    output/figures/fig7c_domain_heatmap.png\n")
cat("    output/figures/fig7d_policing_subgroups.png\n")
cat("  Tables:\n")
cat("    output/tables/tbl_07_domain_profiles.csv\n")
cat("    output/tables/tbl_07_chisq_tests.csv\n")
cat("    output/tables/tbl_07_domain_correlations.csv\n")
cat("    output/tables/tbl_07_policing_subgroups.csv\n")
cat("\nKey findings:\n")

# Print policing reversal prominently
pol_row <- chisq_results |> dplyr::filter(var_suffix == "g")
if (nrow(pol_row) > 0) {
  help_gap <- pol_row$Rep_pct_help - pol_row$Dem_pct_help
  cat(sprintf(
    "  POLICING REVERSAL: Rep %.1f%% help vs Dem %.1f%% help (gap = %+.1f pp, p = %s%s)\n",
    pol_row$Rep_pct_help, pol_row$Dem_pct_help,
    help_gap,
    pol_row$p_fmt, pol_row$sig
  ))
}

# Print domain with largest partisan hurt gap
if ("hurt_gap_pp" %in% names(chisq_results)) {
  top_hurt <- chisq_results |> dplyr::slice_max(hurt_gap_pp, n = 1)
  cat(sprintf(
    "  LARGEST HURT GAP: %s (Rep %.1f%% hurt vs Dem %.1f%% hurt — gap +%.1f pp)\n",
    top_hurt$domain, top_hurt$Rep_pct_hurt, top_hurt$Dem_pct_hurt,
    top_hurt$hurt_gap_pp
  ))
}
