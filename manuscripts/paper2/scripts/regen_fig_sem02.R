#!/usr/bin/env Rscript
# Regenerate fig_sem02_mediation_diagram.png only
# Uses pre-computed tbl_sem03_mediation_results.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
})

here_base <- "/Users/hosung/AI_Polarization_Pew/manuscripts/paper2"
dir_tbl   <- file.path(here_base, "output", "tables")
dir_fig   <- file.path(here_base, "output", "figures")

# Load mediation results
tbl_med <- read.csv(file.path(dir_tbl, "tbl_sem03_mediation_results.csv"),
                    stringsAsFactors = FALSE)

if (nrow(tbl_med) == 0) {
  stop("No mediation results found.")
}

# Identify class columns (binary outcome variables)
class_cols <- unique(tbl_med$outcome)
message("Available class outcomes: ", paste(class_cols, collapse = ", "))

focal_cls <- if ("class_x3" %in% class_cols) "class_x3" else class_cols[1]
med_focal <- tbl_med |> filter(outcome == focal_cls)

# Map raw variable names to display labels
class_display_labels <- c(
  "class_x1" = "AI-Anxious",
  "class_x2" = "AI-Uninformed",
  "class_x3" = "AI-Advantaged",
  "class_x4" = "AI-Ambivalent"
)
focal_cls_label <- if (focal_cls %in% names(class_display_labels)) {
  class_display_labels[[focal_cls]]
} else {
  pos <- match(focal_cls, class_cols)
  if (!is.na(pos)) {
    c("AI-Anxious", "AI-Uninformed", "AI-Advantaged", "AI-Ambivalent")[pos]
  } else {
    focal_cls
  }
}
message("Focal class label: ", focal_cls_label)

# Key path coefficients
pull_est <- function(lhs_val, op_val, rhs_val, tbl) {
  v <- tbl |>
    filter(lhs == lhs_val, op == op_val, rhs == rhs_val) |>
    pull(est)
  if (length(v) == 0) NA_real_ else round(v[1], 3)
}

a1  <- pull_est("awareness_num", "~", "edu_some_college", med_focal)
a2  <- pull_est("awareness_num", "~", "edu_hs_or_less",   med_focal)
b1  <- pull_est(focal_cls,       "~", "awareness_num",    med_focal)
c1  <- pull_est(focal_cls,       "~", "edu_some_college", med_focal)
c2  <- pull_est(focal_cls,       "~", "edu_hs_or_less",   med_focal)

message(sprintf("a1=%.3f  a2=%.3f  b1=%.3f  c1=%.3f  c2=%.3f",
                a1, a2, b1, c1, c2))

nodes <- tibble(
  label = c("Education\n(SES)", "AI Awareness",
            paste0("Class Membership\n(", focal_cls_label, ")")),
  x     = c(0, 2, 4),
  y     = c(0, 1, 0)
)

arrows_indirect <- tibble(
  x    = c(0,  2),
  y    = c(0,  1),
  xend = c(2,  4),
  yend = c(1,  0),
  lab  = c(
    paste0("a\u2081 = ", a1, " (Some coll.)\na\u2082 = ", a2, " (HS or less)"),
    paste0("b = ", b1)
  ),
  mid_x = c(0.9, 3.1),
  mid_y = c(0.65, 0.65)
)

arrows_direct <- tibble(
  x    = 0,
  y    = 0,
  xend = 4,
  yend = 0,
  lab  = paste0("c\u2081\u2019 = ", c1, " (Some coll.)\nc\u2082\u2019 = ", c2, " (HS or less)"),
  mid_x = 2,
  mid_y = -0.15
)

p2 <- ggplot() +
  # Indirect paths (solid, blue, thick)
  geom_segment(data = arrows_indirect,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
               color = "#1f77b4", linewidth = 1.8) +
  # Direct path (dashed, dark gray, thick)
  geom_segment(data = arrows_direct,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
               color = "#333333", linewidth = 1.5, linetype = "dashed") +
  # Labels for indirect paths
  geom_label(data = arrows_indirect,
             aes(x = mid_x, y = mid_y, label = lab),
             size = 3.5, fill = "white", linewidth = 0.3, color = "#1f77b4") +
  # Label for direct path
  geom_label(data = arrows_direct,
             aes(x = mid_x, y = mid_y, label = lab),
             size = 3.5, fill = "white", linewidth = 0.3, color = "#333333") +
  # Node boxes
  geom_label(data = nodes,
             aes(x = x, y = y, label = label),
             size = 4.5, fill = "#d6e8f7", linewidth = 0.6,
             fontface = "bold", label.padding = unit(0.5, "lines")) +
  # Indirect effects annotation
  annotate("label", x = 2, y = -0.45,
           label = paste0("Indirect: Some college = ", round(a1 * b1, 3),
                          "  |  HS or less = ", round(a2 * b1, 3)),
           size = 4, color = "#1f77b4", fill = "#f0f7ff",
           fontface = "bold", linewidth = 0.3) +
  labs(title    = "Mediation: SES \u2192 AI Awareness \u2192 Class Membership",
       subtitle = paste0("Focal class: ", focal_cls_label,
                         " | Bootstrap SE (n = 1,000)"),
       caption  = "Solid blue = indirect paths (a, b); Dashed gray = direct path (c\u2019); Indirect = a \u00d7 b") +
  coord_cartesian(xlim = c(-0.5, 4.5), ylim = c(-0.65, 1.45)) +
  theme_void(base_size = 13) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
        plot.caption  = element_text(hjust = 0.5, color = "gray50", size = 10),
        plot.margin   = margin(10, 10, 10, 10))

out_path <- file.path(dir_fig, "fig_sem02_mediation_diagram.png")
ggsave(out_path, p2, width = 9, height = 6, dpi = 300, bg = "white")
message("Saved: ", out_path)
