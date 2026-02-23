#!/usr/bin/env Rscript
# Regenerate fig_cv01_profile_comparison.png only
# Uses pre-computed tbl_cv02_profiles_by_wave.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(scales)
})

here_base <- "/Users/hosung/AI_Polarization_Pew/paper2_ai_divide"
dir_tbl   <- file.path(here_base, "output", "tables")
dir_fig   <- file.path(here_base, "output", "figures")

# Consistent color palette (3 class names across waves)
CLASS_COLORS <- c(
  "AI-Optimistic" = "#2C7BB6",  # blue
  "AI-Skeptic"    = "#D7191C",  # red
  "Ambivalent"    = "#7F7F7F"   # gray
)

WAVE_LABELS <- c(
  "W119" = "W119\n(Dec 2022)",
  "W132" = "W132\n(Aug 2023)",
  "W152" = "W152\n(Aug 2024)"
)

# Class name mapping by wave and class number
CLASS_NAMES <- list(
  W119 = c("1" = "AI-Optimistic",  "2" = "Ambivalent"),
  W132 = c("1" = "AI-Skeptic",     "2" = "Ambivalent"),
  W152 = c("1" = "Ambivalent",     "2" = "AI-Skeptic")
)

# Load profiles table
profiles_all <- read.csv(file.path(dir_tbl, "tbl_cv02_profiles_by_wave.csv"),
                         stringsAsFactors = FALSE)

message("Loaded profiles: ", nrow(profiles_all), " rows")

# Assign class_num: within each wave+indicator group, rows alternate
# 3 rows per class (3 categories), so rows 1-3 = class 1, rows 4-6 = class 2
profiles_all <- profiles_all %>%
  group_by(wave, indicator) %>%
  mutate(
    row_in_group = row_number(),
    class_num = if_else(row_in_group <= 3, 1L, 2L)
  ) %>%
  ungroup() %>%
  mutate(
    class_name = mapply(function(w, cn) CLASS_NAMES[[w]][as.character(cn)],
                        wave, class_num)
  ) %>%
  select(-row_in_group)

message("Assigned class names: ", paste(sort(unique(profiles_all$class_name)), collapse = ", "))

# Category ordering
att_levels <- c("Excited", "Equally", "Concerned")
awr_levels <- c("A lot", "A little", "Nothing")
all_ordered_levels <- c(att_levels, setdiff(awr_levels, att_levels))

plot_profiles <- profiles_all %>%
  mutate(
    indicator_label = case_when(
      indicator == "attitude"  ~ "AI Attitude",
      indicator == "awareness" ~ "AI Awareness",
      TRUE                     ~ indicator
    ),
    category_chr = as.character(category),
    wave_label   = WAVE_LABELS[wave],
    wave_label   = factor(wave_label, levels = WAVE_LABELS)
  ) %>%
  mutate(
    category = factor(category_chr, levels = all_ordered_levels)
  ) %>%
  select(-category_chr)

message("After processing: ", nrow(plot_profiles), " rows")
message("Unique class names: ", paste(sort(unique(plot_profiles$class_name)), collapse = ", "))

# Build color map using named vector
all_class_names <- sort(unique(plot_profiles$class_name))
class_color_map <- CLASS_COLORS[all_class_names]
message("Class color assignments:")
for (nm in all_class_names) {
  message("  ", nm, " -> ", class_color_map[nm])
}

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

out_path <- file.path(dir_fig, "fig_cv01_profile_comparison.png")
ggsave(out_path, fig_cv01, width = 10, height = 6, dpi = 300, bg = "white")
message("Saved: ", out_path)
