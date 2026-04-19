getwd()


# ============================================================
# PALMER INDEX: SITE-WISE ONLY (IGNORE SEASONS)
# CH (Chauras) vs TI (Tilani)
# + JOURNAL-STYLE TABLES
# + SITE-WISE BAR GRAPH WITH POINTS
# + GROUPED BAR PLOT BY GENUS
# ============================================================

getwd()

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(writexl)

# ============================================================
# 1. READ DATA
# ============================================================
raw_data <- read_excel("+ and - species.xlsx", sheet = 1)

# ============================================================
# 2. DEFINE COLUMN NAMES
# ============================================================
species_col <- "Species Name"
group_cols  <- c("CH J-M", "TI J-M", "CH A-J", "TI A-J")

# ============================================================
# 3. PALMER GENUS SCORES
# ============================================================
palmer_genus_scores <- data.frame(
  Genus = c(
    "Ankistrodesmus","Chlamydomonas","Chlorella","Closterium",
    "Cyclotella","Euglena","Gomphonema","Lepocinclis",
    "Melosira","Micractinium","Navicula","Nitzschia",
    "Oscillatoria","Pandorina","Phacus","Scenedesmus",
    "Stigeoclonium","Synedra"
  ),
  Palmer_Score = c(
    2,4,3,1,1,5,1,1,1,1,3,3,5,1,2,4,2,2
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 4. SELECT DATA
# ============================================================
data_use <- raw_data %>%
  select(all_of(species_col), all_of(group_cols))

# ============================================================
# 5. CONVERT + / - TO 1 / 0
# ============================================================
data_numeric <- data_use %>%
  mutate(
    across(
      all_of(group_cols),
      ~ case_when(
        as.character(.) %in% c("+", " +") ~ 1,
        TRUE ~ 0
      )
    )
  )

# ============================================================
# 6. WIDE TO LONG
# ============================================================
long_data <- data_numeric %>%
  pivot_longer(
    cols = all_of(group_cols),
    names_to = "Group",
    values_to = "Presence"
  )

# ============================================================
# 7. EXTRACT SITE (IGNORE SEASON)
# ============================================================
long_data <- long_data %>%
  mutate(
    Site = ifelse(str_detect(Group, "^CH"), "Chauras", "Tilani")
  )

# ============================================================
# 8. EXTRACT GENUS
# ============================================================
long_data <- long_data %>%
  mutate(
    Species_Name = str_trim(as.character(.data[[species_col]])),
    Species_Name = str_replace_all(Species_Name, "\\s+", " "),
    Species_Clean = Species_Name,
    Species_Clean = str_remove_all(Species_Clean, "\\b(sp\\.?|cf\\.?|aff\\.?)\\b"),
    Species_Clean = str_remove_all(Species_Clean, "[0-9]+"),
    Species_Clean = str_replace_all(Species_Clean, "[^A-Za-z\\s]", ""),
    Species_Clean = str_squish(Species_Clean),
    Genus = word(Species_Clean, 1),
    Genus = str_to_title(Genus)
  )

# ============================================================
# 9. KEEP ONLY PRESENT SPECIES
# ============================================================
present_data <- long_data %>%
  filter(Presence == 1)

# ============================================================
# 10. MATCH PALMER SCORE
# ============================================================
present_palmer <- present_data %>%
  left_join(palmer_genus_scores, by = "Genus") %>%
  mutate(
    Palmer_Score = ifelse(is.na(Palmer_Score), 0, Palmer_Score),
    Palmer_Tolerant = ifelse(Palmer_Score > 0, "Yes", "No")
  )

# ============================================================
# 11. UNIQUE GENUS PER SITE
# ============================================================
unique_site_genus <- present_palmer %>%
  distinct(Site, Genus, .keep_all = TRUE)

# ============================================================
# 12. SITE SUMMARY
# ============================================================
site_summary <- unique_site_genus %>%
  group_by(Site) %>%
  summarise(
    Total_Unique_Genera = n(),
    Tolerant_Genera = sum(Palmer_Tolerant == "Yes"),
    Total_Palmer_Score = sum(Palmer_Score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    present_palmer %>%
      group_by(Site) %>%
      summarise(Total_Present_Species = n(), .groups = "drop"),
    by = "Site"
  ) %>%
  mutate(
    Interpretation = case_when(
      Total_Palmer_Score >= 20 ~ "Confirmed high organic pollution",
      Total_Palmer_Score >= 15 ~ "Probable high organic pollution",
      Total_Palmer_Score >= 10 ~ "Moderate organic pollution",
      TRUE ~ "Low pollution"
    )
  )

print(site_summary)

# ============================================================
# 13. EXPORT TABLES
# ============================================================
write.csv(site_summary, "Site_Wise_Palmer_Summary.csv", row.names = FALSE)

write_xlsx(
  list(
    Site_Summary = site_summary,
    Unique_Genus = unique_site_genus
  ),
  "Site_Wise_Palmer_Output.xlsx"
)

# ============================================================
# 14. SEPARATE TABLES FOR CHAURAS AND TILANI
# ============================================================
sites <- unique(unique_site_genus$Site)

for(s in sites){
  
  s_file <- gsub("[^A-Za-z0-9]+", "_", s)
  
  genus_tbl <- unique_site_genus %>%
    filter(Site == s) %>%
    select(Site, Genus, Palmer_Score, Palmer_Tolerant) %>%
    arrange(desc(Palmer_Score), Genus)
  
  write.csv(
    genus_tbl,
    paste0("Genus_Table_", s_file, ".csv"),
    row.names = FALSE
  )
}

# ============================================================
# 15. SITE-WISE BAR GRAPH WITH DATA POINTS
# ============================================================
plot_bar <- ggplot(site_summary, aes(x = Site, y = Total_Palmer_Score)) +
  geom_col(width = 0.6, fill = "#9EC5FE", color = "black") +
  geom_point(size = 4, color = "#1D3557") +
  geom_text(
    aes(label = Total_Palmer_Score),
    vjust = -0.6,
    size = 4,
    fontface = "bold"
  ) +
  labs(
    title = "Site-wise Palmer Algal Genus Index",
    x = "Site",
    y = "Palmer Score"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(plot_bar)

ggsave(
  "Plot_Site_Comparison_with_points.png",
  plot = plot_bar,
  width = 4,
  height = 5,
  dpi = 600
)

# ============================================================
# 16. PREPARE DATA FOR SPECIES/GENUS-WISE GROUPED BAR PLOT
#     COMBINING BOTH SEASONS WITHIN EACH SITE
#     REMOVE GENERA ABSENT IN BOTH SITES
# ============================================================
plot_data_site <- present_palmer %>%
  group_by(Site, Genus) %>%
  summarise(
    Palmer_Score = max(Palmer_Score, na.rm = TRUE),
    .groups = "drop"
  )

all_genera <- palmer_genus_scores %>%
  select(Genus, Palmer_Score) %>%
  distinct()

chauras_plot <- all_genera %>%
  select(Genus) %>%
  left_join(
    plot_data_site %>%
      filter(Site == "Chauras") %>%
      select(Genus, Score_CH = Palmer_Score),
    by = "Genus"
  ) %>%
  mutate(Score_CH = ifelse(is.na(Score_CH), 0, Score_CH))

tilani_plot <- all_genera %>%
  select(Genus) %>%
  left_join(
    plot_data_site %>%
      filter(Site == "Tilani") %>%
      select(Genus, Score_TI = Palmer_Score),
    by = "Genus"
  ) %>%
  mutate(Score_TI = ifelse(is.na(Score_TI), 0, Score_TI))

plot_species <- chauras_plot %>%
  left_join(tilani_plot, by = "Genus") %>%
  mutate(
    Microalgae = paste0(Genus, " sp.")
  ) %>%
  select(Microalgae, Chauras = Score_CH, Tilani = Score_TI)

plot_species <- plot_species %>%
  filter(!(Chauras == 0 & Tilani == 0))

plot_species <- bind_rows(
  plot_species,
  data.frame(
    Microalgae = "Total",
    Chauras = sum(plot_species$Chauras, na.rm = TRUE),
    Tilani  = sum(plot_species$Tilani, na.rm = TRUE)
  )
)

plot_long <- plot_species %>%
  pivot_longer(
    cols = c("Chauras", "Tilani"),
    names_to = "Site",
    values_to = "Palmer_Score"
  )

plot_long <- plot_long %>%
  mutate(
    Microalgae = gsub(" sp\\.", "\nsp.", Microalgae)
  )

plot_long$Microalgae <- factor(
  plot_long$Microalgae,
  levels = unique(plot_long$Microalgae)
)

# ============================================================
# 17. GROUPED BAR PLOT (BEST FOR YOUR DATA)
# ============================================================
plot_bar_species <- ggplot(
  plot_long,
  aes(x = Microalgae, y = Palmer_Score, fill = Site)
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6,
    color = "black"
  ) +
  geom_text(
    aes(label = Palmer_Score),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_fill_manual(
    values = c(
      "Chauras" = "#A8DADC",
      "Tilani"  = "#F7C59F"
    )
  ) +
  labs(
    title = "Palmer Pollution Index of Algal Genera (Site Comparison)",
    x = "Algal Genera",
    y = "Palmer Index Score",
    fill = "Sites"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "bottom"
  )

print(plot_bar_species)

ggsave(
  "Best_Palmer_Grouped_Barplot.png",
  plot = plot_bar_species,
  width = 11,
  height = 6.5,
  dpi = 600
)

# ============================================================
# 18. JOURNAL-STYLE PALMER TABLES FOR CH AND TI
#     ONLY PRESENT GENERA
# ============================================================

# genus presence by four seasonal columns
genus_presence <- long_data %>%
  select(Genus, Group, Presence) %>%
  distinct() %>%
  pivot_wider(
    names_from = Group,
    values_from = Presence,
    values_fill = 0
  )

# join Palmer score
palmer_table_data <- genus_presence %>%
  left_join(palmer_genus_scores, by = "Genus")

# function to create site-specific journal table
create_palmer_table <- function(data, col1, col2){
  
  df <- data %>%
    select(Genus, all_of(c(col1, col2)), Palmer_Score) %>%
    filter(.data[[col1]] == 1 | .data[[col2]] == 1) %>%
    mutate(
      Microalgae = paste0(Genus, " sp."),
      !!col1 := ifelse(.data[[col1]] == 1, paste0("+", Palmer_Score), ""),
      !!col2 := ifelse(.data[[col2]] == 1, paste0("+", Palmer_Score), "")
    ) %>%
    arrange(desc(Palmer_Score), Microalgae) %>%
    mutate(`S. No.` = row_number()) %>%
    select(`S. No.`, Microalgae, all_of(c(col1, col2)))
  
  extract_total <- function(x){
    x[x == ""] <- "0"
    sum(as.numeric(gsub("\\+", "", x)), na.rm = TRUE)
  }
  
  total_row <- data.frame(
    `S. No.` = "",
    Microalgae = "Total",
    stringsAsFactors = FALSE
  )
  
  total_row[[col1]] <- extract_total(df[[col1]])
  total_row[[col2]] <- extract_total(df[[col2]])
  
  bind_rows(df, total_row)
}

# create CH and TI tables
ch_table <- create_palmer_table(palmer_table_data, "CH J-M", "CH A-J")
ti_table <- create_palmer_table(palmer_table_data, "TI J-M", "TI A-J")

print(ch_table)
print(ti_table)

# export separate CSVs
write.csv(ch_table, "Palmer_Table_CH.csv", row.names = FALSE)
write.csv(ti_table, "Palmer_Table_TI.csv", row.names = FALSE)

# export all major outputs in one Excel workbook
write_xlsx(
  list(
    Site_Summary = site_summary,
    Unique_Genus = unique_site_genus,
    CH_Palmer_Table = ch_table,
    TI_Palmer_Table = ti_table,
    Plot_Data_By_Genus = plot_species
  ),
  "Palmer_Final_Output_All.xlsx"
)