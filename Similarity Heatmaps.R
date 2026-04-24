# =========================================================
# JACCARD TRIANGULAR HEATMAP
# =========================================================


getwd()
setwd("C:/Users/rahul/OneDrive/Desktop/Pushpendra sir files")

# install.packages(c("readxl", "dplyr", "vegan", "ggplot2"))
library(readxl)
library(dplyr)
library(vegan)
library(ggplot2)

# =========================================================
# 1. READ EXCEL FILE
# =========================================================

file_path <- "Species Data.xlsx"
data_raw <- read_excel(file_path, sheet = 1)

cat("\nOriginal column names:\n")
print(colnames(data_raw))

cat("\nPreview of raw data:\n")
print(data_raw)

# =========================================================
# 2. RENAME FIRST TWO COLUMNS
# =========================================================

colnames(data_raw)[1:2] <- c("Site", "Season")

data_raw$Site   <- trimws(as.character(data_raw$Site))
data_raw$Season <- trimws(as.character(data_raw$Season))

data_raw <- data_raw %>%
  mutate(Sample = paste(Site, Season, sep = " ")) %>%
  select(Sample, Site, Season, everything())

cat("\nData after creating Sample names:\n")
print(data_raw)

# =========================================================
# 3. EXTRACT SPECIES MATRIX
# =========================================================

species_data <- data_raw %>%
  select(-Sample, -Site, -Season)

species_data <- as.data.frame(species_data)
species_data[] <- lapply(species_data, function(x) as.numeric(as.character(x)))
rownames(species_data) <- data_raw$Sample

cat("\nSpecies matrix:\n")
print(species_data)

if (any(is.na(species_data))) {
  warning("There are NA values in species_data. Check Excel for blank or non-numeric cells.")
}

# =========================================================
# 4. HELPER FUNCTION TO CREATE TRIANGULAR HEATMAP
# =========================================================

make_triangular_heatmap <- function(sim_mat,
                                    title_text,
                                    legend_title,
                                    low_col,
                                    mid_col,
                                    high_col,
                                    digits = 4) {
  
  df <- as.data.frame(as.table(sim_mat))
  colnames(df) <- c("Row", "Col", "Similarity")
  
  n <- nrow(sim_mat)
  sample_order <- rownames(sim_mat)
  
  df$Row <- factor(df$Row, levels = rev(sample_order))
  df$Col <- factor(df$Col, levels = sample_order)
  
  df$row_id <- match(df$Row, rev(sample_order))
  df$col_id <- match(df$Col, sample_order)
  
  # keep upper triangle including diagonal
  df_tri <- df[df$col_id >= (n - df$row_id + 1), ]
  df_tri$label_txt <- sprintf(paste0("%.", digits, "f"), df_tri$Similarity)
  
  p <- ggplot(df_tri, aes(x = Col, y = Row, fill = Similarity)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = label_txt), size = 3.8, color = "black") +
    scale_fill_gradientn(
      colours = c(low_col, mid_col, high_col),
      limits = c(0, 1),
      name = legend_title
    ) +
    labs(
      title = title_text,
      x = NULL,
      y = NULL,
      caption = paste(
        "Abbreviations:",
        "CH J-M = Churas January-March |",
        "TI J-M = Tilani January-March |",
        "CH A-J = Chuaras April-June |",
        "TI A-J = Tilani April-June"
      )
    ) +
    coord_fixed(clip = "off") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 17,
        color = "black"
      ),
      axis.text.x = element_text(
        angle = 55,
        hjust = 0,
        vjust = 0,
        size = 11,
        color = "black"
      ),
      axis.text.y = element_text(
        size = 11,
        color = "black"
      ),
      panel.grid = element_blank(),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 1.2
      ),
      plot.background = element_rect(
        fill = "white",
        color = "black",
        linewidth = 1.2
      ),
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),
      legend.position = "right",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      plot.margin = margin(15, 30, 35, 15),
      plot.caption = element_text(
        hjust = 0,
        size = 10,
        color = "black",
        margin = margin(t = 10)
      )
    )
  
  return(p)
}

# =========================================================
# 6. JACCARD ANALYSIS
# =========================================================

species_pa <- ifelse(species_data > 0, 1, 0)
species_pa <- as.data.frame(species_pa)
rownames(species_pa) <- rownames(species_data)

cat("\nPresence-Absence Matrix:\n")
print(species_pa)

jaccard_dist <- vegdist(species_pa, method = "jaccard", binary = TRUE)
jaccard_sim  <- 1 - as.matrix(jaccard_dist)

cat("\nJaccard Similarity Matrix:\n")
print(round(jaccard_sim, 4))

write.csv(round(jaccard_sim, 4), "Jaccard_Similarity_Matrix.csv")
write.csv(as.matrix(jaccard_dist), "Jaccard_Distance_Matrix.csv")

jaccard_plot <- make_triangular_heatmap(
  sim_mat      = jaccard_sim,
  title_text   = "Jaccard Similarity Heatmap",
  legend_title = "Jaccard Similarity Index",
  low_col      = "#EAF3F4",
  mid_col      = "#A9D4D8",
  high_col     = "#0C8A92",
  digits       = 4
)

# PRINT Jaccard heatmap
print(jaccard_plot)

# SAVE Jaccard heatmap
ggsave(
  filename = "Jaccard_Triangular_Heatmap.png",
  plot     = jaccard_plot,
  width    = 10,
  height   = 10,
  units    = "in",
  dpi      = 600,
  device   = "png",
  bg       = "white"
)


# =========================================================
# 8. OPTIONAL NMDS PLOTS WITH LIGHT-COLORED CLUSTERS + LABELS
#    + FINAL TABLE EXTRACTION FOR BOTH NMDS ANALYSES
#    + GGPLOT SAVING WITH GGSAVE()
# =========================================================

library(vegan)
library(ggplot2)
library(dplyr)

# =========================================================
# FUNCTION: assign each species to nearest site
# =========================================================
assign_species_to_nearest_site <- function(site_scores, species_scores) {
  
  site_names <- rownames(site_scores)
  species_names <- rownames(species_scores)
  
  nearest_group <- character(length(species_names))
  
  for (i in seq_along(species_names)) {
    sp_point <- species_scores[i, ]
    
    dists <- apply(site_scores, 1, function(site_point) {
      sqrt(sum((sp_point - site_point)^2))
    })
    
    nearest_group[i] <- names(which.min(dists))
  }
  
  factor(nearest_group, levels = site_names)
}

# =========================================================
# FUNCTION: extract final NMDS tables
# =========================================================
extract_nmds_tables <- function(nmds_obj, analysis_name) {
  
  site_scores <- as.data.frame(scores(nmds_obj, display = "sites"))
  species_scores <- as.data.frame(scores(nmds_obj, display = "species"))
  
  site_scores$Label <- rownames(site_scores)
  species_scores$Label <- rownames(species_scores)
  
  names(site_scores)[1:2] <- c("NMDS1", "NMDS2")
  names(species_scores)[1:2] <- c("NMDS1", "NMDS2")
  
  species_groups <- assign_species_to_nearest_site(
    site_scores[, c("NMDS1", "NMDS2")],
    species_scores[, c("NMDS1", "NMDS2")]
  )
  
  cluster_labels <- c(
    "CH J-M" = "Cluster I",
    "CH A-J" = "Cluster II",
    "TI A-J" = "Cluster III",
    "TI J-M" = "Cluster IV"
  )
  
  site_table <- data.frame(
    Analysis = analysis_name,
    Type = "Site",
    Label = site_scores$Label,
    NMDS1 = round(site_scores$NMDS1, 6),
    NMDS2 = round(site_scores$NMDS2, 6),
    Assigned_Site = site_scores$Label,
    Cluster = unname(cluster_labels[site_scores$Label]),
    stringsAsFactors = FALSE
  )
  
  species_table <- data.frame(
    Analysis = analysis_name,
    Type = "Species",
    Label = species_scores$Label,
    NMDS1 = round(species_scores$NMDS1, 6),
    NMDS2 = round(species_scores$NMDS2, 6),
    Assigned_Site = as.character(species_groups),
    Cluster = unname(cluster_labels[as.character(species_groups)]),
    stringsAsFactors = FALSE
  )
  
  final_table <- rbind(site_table, species_table)
  
  write.csv(site_table,
            paste0(analysis_name, "_Site_Scores.csv"),
            row.names = FALSE)
  
  write.csv(species_table,
            paste0(analysis_name, "_Species_Scores.csv"),
            row.names = FALSE)
  
  write.csv(final_table,
            paste0(analysis_name, "_Final_NMDS_Table.csv"),
            row.names = FALSE)
  
  summary_table <- data.frame(
    Analysis = analysis_name,
    Stress = nmds_obj$stress,
    Dimensions = nmds_obj$ndim,
    Converged = nmds_obj$converged,
    stringsAsFactors = FALSE
  )
  
  write.csv(summary_table,
            paste0(analysis_name, "_NMDS_Summary.csv"),
            row.names = FALSE)
  
  cat("\n=============================\n")
  cat("Final table for", analysis_name, "\n")
  cat("=============================\n")
  print(final_table)
  
  cat("\nSummary for", analysis_name, "\n")
  print(summary_table)
  
  return(list(
    site_table = site_table,
    species_table = species_table,
    final_table = final_table,
    summary_table = summary_table
  ))
}

# =========================================================
# FUNCTION: create cluster polygon points for ggplot
# =========================================================
make_ellipse_polygon <- function(x, y, expand_x = 0.08, expand_y = 0.08, n = 200) {
  
  if (length(x) == 0 || length(y) == 0) return(NULL)
  
  cx <- mean(x, na.rm = TRUE)
  cy <- mean(y, na.rm = TRUE)
  
  rx <- max(abs(x - cx), na.rm = TRUE) + expand_x
  ry <- max(abs(y - cy), na.rm = TRUE) + expand_y
  
  rx <- max(rx, 0.05)
  ry <- max(ry, 0.05)
  
  theta <- seq(0, 2 * pi, length.out = n)
  
  data.frame(
    NMDS1 = cx + rx * cos(theta),
    NMDS2 = cy + ry * sin(theta)
  )
}

# =========================================================
# FUNCTION: build ggplot NMDS plot
# =========================================================
plot_nmds_with_colored_clusters <- function(nmds_obj, main_title) {
  
  site_scores <- as.data.frame(scores(nmds_obj, display = "sites"))
  species_scores <- as.data.frame(scores(nmds_obj, display = "species"))
  
  site_scores$Label <- rownames(site_scores)
  species_scores$Label <- rownames(species_scores)
  
  names(site_scores)[1:2] <- c("NMDS1", "NMDS2")
  names(species_scores)[1:2] <- c("NMDS1", "NMDS2")
  
  species_groups <- assign_species_to_nearest_site(
    site_scores[, c("NMDS1", "NMDS2")],
    species_scores[, c("NMDS1", "NMDS2")]
  )
  
  species_scores$Group <- as.character(species_groups)
  
  cluster_labels <- c(
    "CH J-M" = "Cluster I",
    "CH A-J" = "Cluster II",
    "TI A-J" = "Cluster III",
    "TI J-M" = "Cluster IV"
  )
  
  cluster_colors_border <- c(
    "CH J-M" = "blue",
    "CH A-J" = "darkgreen",
    "TI A-J" = "purple",
    "TI J-M" = "orange"
  )
  
  cluster_colors_fill <- c(
    "CH J-M" = "lightblue",
    "CH A-J" = "lightgreen",
    "TI A-J" = "plum1",
    "TI J-M" = "moccasin"
  )
  
  # Make ellipse polygon dataframe
  ellipse_list <- lapply(unique(species_scores$Group), function(grp) {
    grp_data <- species_scores[species_scores$Group == grp, ]
    
    poly_df <- make_ellipse_polygon(
      x = grp_data$NMDS1,
      y = grp_data$NMDS2,
      expand_x = 0.08,
      expand_y = 0.08,
      n = 200
    )
    
    if (!is.null(poly_df)) {
      poly_df$Group <- grp
      poly_df$Cluster <- cluster_labels[grp]
    }
    
    poly_df
  })
  
  ellipse_df <- do.call(rbind, ellipse_list)
  
  # Cluster label positions
  cluster_centers <- species_scores %>%
    group_by(Group) %>%
    summarise(
      NMDS1 = mean(NMDS1, na.rm = TRUE),
      NMDS2 = mean(NMDS2, na.rm = TRUE) + 0.03,
      .groups = "drop"
    )
  
  cluster_centers$Cluster <- unname(cluster_labels[cluster_centers$Group])
  
  p <- ggplot() +
    
    geom_polygon(
      data = ellipse_df,
      aes(x = NMDS1, y = NMDS2, group = Group, fill = Group, color = Group),
      alpha = 0.25,
      linewidth = 0.8
    ) +
    
    geom_text(
      data = species_scores,
      aes(x = NMDS1, y = NMDS2, label = Label),
      color = "red",
      size = 3
    ) +
    
    geom_text(
      data = site_scores,
      aes(x = NMDS1, y = NMDS2, label = Label),
      color = "black",
      size = 4,
      fontface = "bold"
    ) +
    
    geom_text(
      data = cluster_centers,
      aes(x = NMDS1, y = NMDS2, label = Cluster, color = Group),
      size = 3.5,
      fontface = "bold",
      show.legend = FALSE
    ) +
    
    scale_fill_manual(
      values = cluster_colors_fill,
      labels = c(
        "CH J-M" = "Cluster I (CH J-M)",
        "CH A-J" = "Cluster II (CH A-J)",
        "TI A-J" = "Cluster III (TI A-J)",
        "TI J-M" = "Cluster IV (TI J-M)"
      )
    ) +
    
    scale_color_manual(
      values = cluster_colors_border,
      labels = c(
        "CH J-M" = "Cluster I (CH J-M)",
        "CH A-J" = "Cluster II (CH A-J)",
        "TI A-J" = "Cluster III (TI A-J)",
        "TI J-M" = "Cluster IV (TI J-M)"
      )
    ) +
    
    labs(
      title = main_title,
      x = "NMDS1",
      y = "NMDS2",
      fill = "Clusters",
      color = "Clusters"
    ) +
    
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )
  
  return(p)
}

# =========================================================
# JACCARD NMDS
# =========================================================
set.seed(123)
nmds_jaccard <- metaMDS(
  species_pa,
  distance = "jaccard",
  binary = TRUE,
  k = 2,
  trymax = 100
)

cat("\nNMDS Jaccard result:\n")
print(nmds_jaccard)

# Create ggplot object
p_jaccard <- plot_nmds_with_colored_clusters(
  nmds_obj   = nmds_jaccard,
  main_title = "NMDS Plot (Jaccard)"
)

# Print plot
print(p_jaccard)

# Save with ggsave
ggsave(
  filename = "NMDS_Jaccard_Clusters_600dpi.png",
  plot = p_jaccard,
  width = 10,
  height = 8,
  dpi = 600,
  bg = "white"
)

# Extract tables
jaccard_tables <- extract_nmds_tables(
  nmds_obj = nmds_jaccard,
  analysis_name = "Jaccard"
)