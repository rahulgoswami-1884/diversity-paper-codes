getwd()

# -------------------------------
# 1. INSTALL PACKAGES 
# -------------------------------
install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "stringr", "patchwork", "cowplot"))

# -------------------------------
# 2. LOAD LIBRARIES
# -------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(patchwork)
library(cowplot)
library(grid)

# -------------------------------
# 3. READ EXCEL FILE
# -------------------------------
data <- read_excel("env Data.xlsx", sheet = 1)

# -------------------------------
# 4. RENAME COLUMNS
# -------------------------------
colnames(data)[1] <- "Parameter"
colnames(data) <- c("Parameter", "CH_JM", "TI_JM", "CH_AJ", "TI_AJ", "Unit")

# -------------------------------
# 5. CLEAN TEXT
# -------------------------------
data <- data %>%
  mutate(
    Parameter = trimws(as.character(Parameter)),
    Unit = trimws(as.character(Unit))
  )

# -------------------------------
# 6. FIX MISSING UNITS
# -------------------------------
data <- data %>%
  mutate(
    Unit = case_when(
      Parameter %in% c("Water temperature", "Water Temperature") &
        (is.na(Unit) | Unit == "" | Unit == "NA") ~ "°C",
      Parameter == "pH" &
        (is.na(Unit) | Unit == "" | Unit == "NA") ~ "-",
      TRUE ~ Unit
    )
  )

# -------------------------------
# 7. LONG FORMAT
# -------------------------------
data_long <- data %>%
  pivot_longer(
    cols = c(CH_JM, TI_JM, CH_AJ, TI_AJ),
    names_to = "Group_code",
    values_to = "Mean_SD"
  )

# -------------------------------
# 8. EXTRACT MEAN +_ SD
# -------------------------------
data_long <- data_long %>%
  mutate(
    Mean_SD = as.character(Mean_SD),
    Mean = str_extract(Mean_SD, "^[0-9.]+"),
    SD   = str_extract(Mean_SD, "(?<=±)\\s*[0-9.]+"),
    Mean = as.numeric(trimws(Mean)),
    SD   = as.numeric(trimws(SD))
  )

# -------------------------------
# 9. GROUP LABELS
# -------------------------------
data_long <- data_long %>%
  mutate(
    Group = factor(
      Group_code,
      levels = c("CH_JM", "TI_JM", "CH_AJ", "TI_AJ"),
      labels = c("CH J-M", "TI J-M", "CH A-J", "TI A-J")
    )
  )

# -------------------------------
# 10. LEGEND LABELS
# -------------------------------
legend_labels <- c(
  "CH J-M" = "CH J-M = Chauras January-March",
  "TI J-M" = "TI J-M = Tilani January-March",
  "CH A-J" = "CH A-J = Chauras April-June",
  "TI A-J" = "TI A-J = Tilani April-June"
)

# -------------------------------
# 11. SHORT PARAMETER NAMES
# -------------------------------
data_long <- data_long %>%
  mutate(
    Parameter_short = case_when(
      Parameter == "Total alkalinity" ~ "T. Alkalinity",
      Parameter == "Total hardness"   ~ "T. Hardness",
      Parameter %in% c("Water temperature", "Water Temperature") ~ "Water temperature",
      TRUE ~ Parameter
    )
  )

# -------------------------------
# 12. PANEL TITLES
# -------------------------------
data_long <- data_long %>%
  mutate(
    Panel_Title = case_when(
      Parameter_short == "Water temperature" ~ "Water temperature (°C)",
      Parameter_short == "pH" ~ "pH",
      TRUE ~ paste0(Parameter_short, " (", Unit, ")")
    )
  )

# -------------------------------
# 13. NUMBER FORMAT
# -------------------------------
fmt_num <- function(x) {
  ifelse(
    abs(x - round(x)) < 1e-9,
    as.character(round(x)),
    format(round(x, 2), nsmall = 2, trim = TRUE)
  )
}

data_long <- data_long %>%
  mutate(
    Label = paste0(fmt_num(Mean), " ± ", fmt_num(SD))
  )

# -------------------------------
# 14. COLOR PALETTE
# -------------------------------
group_colors <- c(
  "CH J-M" = "#BA68C8",
  "TI J-M" = "#1F9AA0",
  "CH A-J" = "#FF7A1A",
  "TI A-J" = "#BDBDBD"
)

# -------------------------------
# 15. COMMON THEME
# -------------------------------
base_theme <- theme_bw(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    axis.title = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.margin = margin(4, 4, 4, 4)
  )

# -------------------------------
# 16. FUNCTION TO MAKE EACH PANEL
# -------------------------------
make_panel <- function(df, show_x = FALSE) {
  ggplot(df, aes(x = Group, y = Mean, fill = Group)) +
    geom_col(width = 0.65, color = "black", linewidth = 0.3) +
    geom_text(aes(label = Label), vjust = -0.35, size = 3.0) +
    scale_fill_manual(values = group_colors, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = unique(df$Panel_Title)) +
    base_theme +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
      axis.text.x = if (show_x) element_text(angle = 35, hjust = 1) else element_blank(),
      axis.ticks.x = if (show_x) element_line() else element_blank()
    )
}

# -------------------------------
# 17. SPLIT DATA FOR EACH PANEL
# -------------------------------
df_calcium   <- filter(data_long, Panel_Title == "Calcium (mg/L)")
df_chloride  <- filter(data_long, Panel_Title == "Chloride (mg/L)")
df_color     <- filter(data_long, Panel_Title == "Watercolor (Hazen)")
df_mag       <- filter(data_long, Panel_Title == "Magnesium (mg/L)")
df_nitrate   <- filter(data_long, Panel_Title == "Nitrate (mg/L)")
df_ph        <- filter(data_long, Panel_Title == "pH")
df_alk       <- filter(data_long, Panel_Title == "T. Alkalinity (mg/L)")
df_hard      <- filter(data_long, Panel_Title == "T. Hardness (mg/L)")
df_tds       <- filter(data_long, Panel_Title == "TDS (mg/L)")
df_turb      <- filter(data_long, Panel_Title == "Turbidity (NTU)")
df_temp      <- filter(data_long, Panel_Title == "Water temperature (°C)")

# -------------------------------
# 18. MAKE PANELS
# -------------------------------
p1  <- make_panel(df_calcium,  show_x = FALSE)
p2  <- make_panel(df_chloride, show_x = FALSE)
p3  <- make_panel(df_color,    show_x = FALSE)
p4  <- make_panel(df_mag,      show_x = FALSE)
p5  <- make_panel(df_nitrate,  show_x = FALSE)
p6  <- make_panel(df_ph,       show_x = FALSE)
p7  <- make_panel(df_alk,      show_x = FALSE)
p8  <- make_panel(df_hard,     show_x = FALSE)
p9  <- make_panel(df_tds,      show_x = FALSE)
p10 <- make_panel(df_turb,     show_x = TRUE)
p11 <- make_panel(df_temp,     show_x = TRUE)

# -------------------------------
# 19. BUILD LEGEND PLOT
# -------------------------------
legend_plot <- ggplot(
  data.frame(
    Group = factor(names(group_colors), levels = names(group_colors)),
    x = 1, y = 1
  ),
  aes(x = x, y = y, fill = Group)
) +
  geom_tile() +
  scale_fill_manual(
    name = "Groups",
    values = group_colors,
    labels = legend_labels,
    drop = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    legend.box.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    legend.margin = margin(8, 8, 8, 8),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.6, "cm")
  )

legend_grob <- cowplot::get_legend(legend_plot)

legend_panel <- patchwork::wrap_elements(legend_grob)

# -------------------------------
# 20. FINAL LAYOUT
# -------------------------------
left_col  <- p1 / p3 / p5 / p7 / p9 / p11
right_col <- p2 / p4 / p6 / p8 / p10 / legend_panel

final_plot <- left_col | right_col


print(final_plot)


ggsave(
  "Final_multipanel_plot_with_legend_bottom_right.tiff",
  plot = final_plot,
  width = 10,
  height = 14,
  dpi = 600,
  compression = "lzw"
)

ggsave(
  "Final_multipanel_plot_with_legend_bottom_right.png",
  plot = final_plot,
  width = 10,
  height = 14,
  dpi = 600
)