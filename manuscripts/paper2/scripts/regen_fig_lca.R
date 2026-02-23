#!/usr/bin/env Rscript
# Regenerate LCA profile figures using updated class labels
# Uses tbl_lca_profiles_form_a.csv and tbl_lca_profiles_form_b.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(scales)
})

here_base <- "/Users/hosung/AI_Polarization_Pew/manuscripts/paper2"
dir_tbl   <- file.path(here_base, "output", "tables")
dir_fig   <- file.path(here_base, "output", "figures")

# Load profiles
profiles_a <- read.csv(file.path(dir_tbl, "tbl_lca_profiles_form_a.csv"),
                       stringsAsFactors = FALSE)
profiles_b <- read.csv(file.path(dir_tbl, "tbl_lca_profiles_form_b.csv"),
                       stringsAsFactors = FALSE)

message("Form A profiles: ", nrow(profiles_a), " rows")
message("Form A class_labels: ", paste(sort(unique(profiles_a$class_label)), collapse = ", "))
message("Form B profiles: ", nrow(profiles_b), " rows")
message("Form B class_labels: ", paste(sort(unique(profiles_b$class_label)), collapse = ", "))

# Replace any "Class N (Form X)" labels with proper names
fix_class_labels <- function(df) {
  label_map <- c(
    "Class 1 (Form A)" = "AI-Anxious",
    "Class 2 (Form A)" = "AI-Uninformed",
    "Class 3 (Form A)" = "AI-Advantaged",
    "Class 4 (Form A)" = "AI-Ambivalent",
    "Class 1 (Form B)" = "AI-Anxious",
    "Class 2 (Form B)" = "AI-Uninformed",
    "Class 3 (Form B)" = "AI-Advantaged",
    "Class 4 (Form B)" = "AI-Ambivalent"
  )
  df %>%
    mutate(class_label = if_else(
      class_label %in% names(label_map),
      label_map[class_label],
      class_label
    ))
}

profiles_a <- fix_class_labels(profiles_a)
profiles_b <- fix_class_labels(profiles_b)

message("Fixed Form A labels: ", paste(sort(unique(profiles_a$class_label)), collapse = ", "))
message("Fixed Form B labels: ", paste(sort(unique(profiles_b$class_label)), collapse = ", "))

# Variable labels
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

response_labels_attitude  <- c("1" = "Excited",   "2" = "Concerned",  "3" = "Equal")
response_labels_awareness <- c("1" = "A lot",     "2" = "A little",   "3" = "Nothing")
response_labels_binary    <- c("1" = "Not hurt",  "2" = "Hurt")

get_response_label <- function(var, level) {
  if (var == "lca_attitude")  return(response_labels_attitude[as.character(level)])
  if (var == "lca_awareness") return(response_labels_awareness[as.character(level)])
  return(response_labels_binary[as.character(level)])
}

make_profile_plot <- function(profiles, form_label) {
  n_classes <- length(unique(profiles$class_label))

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

make_heatmap <- function(profiles, form_label) {
  plot_data <- profiles %>%
    mutate(
      var_label  = var_labels[variable],
      resp_label = mapply(get_response_label, variable, response_level),
      cell_label = sprintf("%.0f%%", probability * 100),
      class_f    = factor(paste0(class, "\n", class_label))
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

# Regenerate Form A figures
p_profile_a <- make_profile_plot(profiles_a, "A")
ggsave(file.path(dir_fig, "fig_lca_profiles_form_a.png"),
       p_profile_a, width = 7, height = 5, dpi = 300)
message("Saved: fig_lca_profiles_form_a.png")

p_heat_a <- make_heatmap(profiles_a, "A")
ggsave(file.path(dir_fig, "fig_lca_heatmap_form_a.png"),
       p_heat_a, width = 8, height = 6, dpi = 300)
message("Saved: fig_lca_heatmap_form_a.png")

# Regenerate Form B figures
p_profile_b <- make_profile_plot(profiles_b, "B")
ggsave(file.path(dir_fig, "fig_lca_profiles_form_b.png"),
       p_profile_b, width = 7, height = 5, dpi = 300)
message("Saved: fig_lca_profiles_form_b.png")

p_heat_b <- make_heatmap(profiles_b, "B")
ggsave(file.path(dir_fig, "fig_lca_heatmap_form_b.png"),
       p_heat_b, width = 8, height = 6, dpi = 300)
message("Saved: fig_lca_heatmap_form_b.png")

message("All LCA profile figures regenerated.")
