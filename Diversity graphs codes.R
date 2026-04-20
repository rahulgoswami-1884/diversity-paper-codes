getwd()


# Install once if needed:
install.packages(c("readxl", "dplyr", "ggplot2", "patchwork", "grid"))

library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(grid)

# ============================================================
# 1. READ YOUR EXCEL FILE
# ============================================================

file_path <- "diversity mean sd.xlsx"   # change to your file name
sheet_name <- 1                         # or "Sheet1"

df <- read_excel(file_path, sheet = sheet_name)

# ============================================================
# 2. SET X-AXIS ORDER
# ============================================================

df$Group <- factor(
  df$Group,
  levels = c("CH J-M", "TI J-M", "CH A-J", "TI A-J")
)

# ============================================================
# 3. COLORS AND LEGEND LABELS
# ============================================================

cols <- c(
  "CH J-M" = "#C86AD4",
  "TI J-M" = "#FF7A1A",
  "CH A-J" = "#15939A",
  "TI A-J" = "#BDBDBD"
)

legend_labs <- c(
  "CH J-M" = "Chauras, January–March (CH J-M)",
  "TI J-M" = "Tilani, January–March (TI J-M)",
  "CH A-J" = "Chauras, April–June (CH A-J)",
  "TI A-J" = "Tilani, April–June (TI A-J)"
)

# ============================================================
# 4. COMMON THEME
# ============================================================

common_theme <- theme_bw(base_size = 14) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1.2),
    axis.line = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", size = 11),
    axis.text.y = element_text(color = "black", size = 11),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.3),
    legend.position = "none",
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12)
  )

# ============================================================
# 5. FUNCTION TO MAKE EACH PANEL
# ============================================================

make_panel <- function(data, mean_col, sd_col, title_text, digits_mean = 4, digits_sd = 4) {
  
  plot_df <- data.frame(
    Group = data$Group,
    Mean  = as.numeric(data[[mean_col]]),
    SD    = as.numeric(data[[sd_col]])
  )
  
  plot_df <- plot_df %>%
    mutate(
      Label = paste0(
        formatC(Mean, format = "f", digits = digits_mean),
        " \u00B1 ",
        formatC(SD, format = "f", digits = digits_sd)
      ),
      ymax = max(Mean, na.rm = TRUE),
      label_y = Mean + 0.04 * ymax
    )
  
  ggplot(plot_df, aes(x = Group, y = Mean, fill = Group)) +
    geom_col(width = 0.65, color = "grey30", linewidth = 0.30) +
    geom_text(aes(y = label_y, label = Label), size = 3.2, color = "black") +
    scale_fill_manual(values = cols, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.18))) +
    labs(title = title_text) +
    guides(fill = "none") +
    common_theme
}

# ============================================================
# 6. CREATE THE 5 PANELS
# ============================================================

p1 <- make_panel(df, "Margalef_mean",   "Margalef_sd",   "Margalef richness (d)")
p2 <- make_panel(df, "Pielou_mean",     "Pielou_sd",     "Pielou evenness (J\u2032)")
p3 <- make_panel(df, "Shannon_mean",    "Shannon_sd",    "Shannon (H\u2032)")
p4 <- make_panel(df, "SimpsonDiv_mean", "SimpsonDiv_sd", "Simpson diversity (1 \u2212 D)")
p5 <- make_panel(df, "SimpsonDom_mean", "SimpsonDom_sd", "Simpson dominance (D)")

# ============================================================
# 7. SEPARATE LEGEND PANEL
# ============================================================

legend_df <- data.frame(
  Group = factor(
    c("CH J-M", "TI J-M", "CH A-J", "TI A-J"),
    levels = c("CH J-M", "TI J-M", "CH A-J", "TI A-J")
  ),
  x = 1,
  y = c(4, 3, 2, 1)
)

p_legend <- ggplot(legend_df, aes(x = x, y = y, fill = Group)) +
  geom_tile(width = 0.18, height = 0.45, color = "grey30", linewidth = 0.30) +
  geom_text(
    aes(x = 1.22, label = legend_labs[Group]),
    hjust = 0, size = 5.5
  ) +
  xlim(0.75, 2.75) +
  ylim(0.3, 4.7) +
  theme_void(base_size = 14) +
  theme(
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1.2),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "none"
  ) +
  scale_fill_manual(values = cols, drop = FALSE, guide = "none") +
  coord_cartesian(clip = "off")

# ============================================================
# 8. ARRANGE AS 3 x 2 LAYOUT
# Bottom-right = legend box
# ============================================================

final_plot <-
  (p1 | p2) /
  (p3 | p4) /
  (p5 | p_legend)

print(final_plot)

# ============================================================
# 9. SAVE
# ============================================================

ggsave(
  "Fig_Diversity_Indices_5Panel_LegendBox_4decimalSD.png",
  final_plot,
  width = 14,
  height = 10,
  dpi = 600
)

ggsave(
  "Fig_Diversity_Indices_5Panel_LegendBox_4decimalSD.pdf",
  final_plot,
  width = 14,
  height = 10
)