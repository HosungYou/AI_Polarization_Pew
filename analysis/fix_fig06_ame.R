#!/usr/bin/env Rscript
# Fix Figure 6: AME predictors with no overlapping labels
# Removes group category annotations that overlap with predictor labels
# Uses clean grouped layout with separator lines instead

library(tidyverse)

# Read AME data
ame <- read_csv("output/tables/tbl_adv03_ames_main.csv", show_col_types = FALSE)

# Create clean labels and group assignments
plot_df <- ame %>%
  mutate(
    group = case_when(
      term == "party" ~ "Party",
      term == "wave_date" ~ "Wave",
      term == "age_cat" ~ "Age",
      term == "gender" ~ "Gender",
      term == "education" ~ "Education",
      term == "race" ~ "Race/Ethnicity",
      term == "income_tier" ~ "Income",
      term == "ai_heard" ~ "AI Awareness"
    ),
    label = case_when(
      # Party
      contrast == "Rep/Lean Rep - Dem/Lean Dem" ~ "Rep/Lean Rep vs. Dem/Lean Dem",
      # Wave
      contrast == "Aug 2023 - Dec 2022" ~ "Aug 2023 vs. Dec 2022",
      contrast == "Aug 2024 - Dec 2022" ~ "Aug 2024 vs. Dec 2022",
      # Age
      contrast == "18-29 - 30-49" ~ "18\u201329 vs. 30\u201349",
      contrast == "50-64 - 30-49" ~ "50\u201364 vs. 30\u201349",
      contrast == "65+ - 30-49" ~ "65+ vs. 30\u201349",
      # Gender
      contrast == "Female - Male" ~ "Female vs. Male",
      contrast == "Other - Male" ~ "Other vs. Male",
      # Education
      contrast == "HS graduate or less - College graduate+" ~ "HS or less vs. College grad+",
      contrast == "Some College - College graduate+" ~ "Some college vs. College grad+",
      # Race
      contrast == "Asian non-Hispanic - White non-Hispanic" ~ "Asian vs. White",
      contrast == "Black non-Hispanic - White non-Hispanic" ~ "Black vs. White",
      contrast == "Hispanic - White non-Hispanic" ~ "Hispanic vs. White",
      contrast == "Other non-Hispanic - White non-Hispanic" ~ "Other race vs. White",
      # Income
      contrast == "Lower - Middle" ~ "Lower vs. Middle income",
      contrast == "Upper - Middle" ~ "Upper vs. Middle income",
      # AI Awareness
      contrast == "A little - A lot" ~ "A little vs. A lot",
      contrast == "Nothing - A lot" ~ "Nothing vs. A lot",
      TRUE ~ contrast
    ),
    sig_flag = p.value < 0.05,
    is_party = term == "party",
    # Significance annotation
    sig_label = case_when(
      term == "party" ~ sprintf("%.1f pp ***", estimate * 100),
      TRUE ~ NA_character_
    )
  )

# Set group order (top to bottom)
group_order <- c("Party", "Wave", "Age", "Gender", "Education",
                 "Race/Ethnicity", "Income", "AI Awareness")
plot_df$group <- factor(plot_df$group, levels = rev(group_order))

# Within each group, order by estimate
plot_df <- plot_df %>%
  arrange(group, estimate) %>%
  mutate(y_pos = row_number())

# Build y-axis with group separators (blank rows between groups)
# Add spacing between groups
spacing <- 0.8
group_sizes <- plot_df %>% count(group) %>% arrange(desc(group))

y_positions <- numeric(nrow(plot_df))
current_y <- 0
for (g in levels(plot_df$group)) {
  rows <- which(plot_df$group == g)
  if (length(rows) > 0) {
    for (i in seq_along(rows)) {
      current_y <- current_y + 1
      y_positions[rows[i]] <- current_y
    }
    current_y <- current_y + spacing  # gap between groups
  }
}
plot_df$y_pos <- y_positions

# Group label positions (centered in each group)
group_labels <- plot_df %>%
  group_by(group) %>%
  summarize(y_mid = mean(y_pos), .groups = "drop")

# Separator lines between groups
group_bounds <- plot_df %>%
  group_by(group) %>%
  summarize(y_min = min(y_pos), y_max = max(y_pos), .groups = "drop") %>%
  arrange(y_min)

sep_lines <- numeric()
for (i in 1:(nrow(group_bounds) - 1)) {
  sep_lines <- c(sep_lines,
                 (group_bounds$y_max[i] + group_bounds$y_min[i + 1]) / 2)
}

# Plot
p <- ggplot(plot_df, aes(x = estimate, y = y_pos)) +
  # Separator lines
  geom_hline(yintercept = sep_lines, color = "grey88", linewidth = 0.3) +
  # Zero line
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  # CI bars
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high,
        alpha = ifelse(sig_flag, 1.0, 0.35)),
    height = 0.3, linewidth = 0.6, color = "grey30"
  ) +
  # Points
  geom_point(
    aes(color = is_party,
        size = is_party,
        alpha = ifelse(sig_flag, 1.0, 0.35)),
    show.legend = FALSE
  ) +
  # Party label annotation
  geom_text(
    data = filter(plot_df, !is.na(sig_label)),
    aes(x = conf.high + 0.008, label = sig_label),
    hjust = 0, size = 3.5, fontface = "bold", color = "#D4A017"
  ) +
  # Group labels on the left margin
  annotate(
    "text",
    x = min(plot_df$conf.low) - 0.02,
    y = group_labels$y_mid,
    label = group_labels$group,
    hjust = 1, size = 2.8, fontface = "italic", color = "grey45"
  ) +
  # Scales
  scale_y_continuous(
    breaks = plot_df$y_pos,
    labels = plot_df$label,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_x_continuous(
    labels = function(x) sprintf("%.0f", x * 100),
    expand = expansion(mult = c(0.22, 0.12))
  ) +
  scale_color_manual(values = c("TRUE" = "#D4A017", "FALSE" = "grey25")) +
  scale_size_manual(values = c("TRUE" = 3.5, "FALSE" = 2.2)) +
  scale_alpha_identity() +
  # Labels
  labs(
    x = "Average Marginal Effect (percentage points)",
    y = NULL,
    caption = paste0(
      "*** p < .001, ** p < .01, * p < .05, ns = not significant. ",
      "Reference: Dem/Lean Dem, Dec 2022, Age 30-49,\n",
      "Male, College grad+, White, Middle income, A lot of AI awareness."
    )
  ) +
  # Theme
  theme_minimal(base_family = "Arial", base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
    axis.text.y = element_text(size = 9, color = "grey20"),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 10, margin = margin(t = 8)),
    plot.caption = element_text(size = 7.5, color = "grey50", hjust = 0,
                                margin = margin(t = 10)),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 100)
  )

# Save
ggsave("output/figures/journal/fig06_ame_predictors.png",
       plot = p, width = 8, height = 6.5, dpi = 300, bg = "white")

cat("Saved: output/figures/journal/fig06_ame_predictors.png\n")
