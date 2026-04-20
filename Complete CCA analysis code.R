getwd()

# ========================================================================
# CCA plot analysis with already calculated mean of species and parameters
# ========================================================================

# -----------------------------
# PACKAGES
# -----------------------------
library(readxl)
library(dplyr)
library(vegan)

#install.packages("openxlsx")   #try to install all pacakages
library(openxlsx)

# -----------------------------
# INPUT FILES
# -----------------------------
file_abund <- "Species Data.xlsx"  #try your file location for best result 
file_env   <- "env data.xlsx"

# ============================================================
# READ ABUNDANCE DATA
# ============================================================
abund <- read_excel(file_abund)
names(abund)[1:2] <- c("Site", "Season")
abund <- abund %>%
  mutate(Sample = paste(Site, Season, sep = "_"))
sp_cols <- setdiff(names(abund), c("Site", "Season", "Sample"))

# convert species data to numeric
abund[sp_cols] <- lapply(
  abund[sp_cols],
  function(x) suppressWarnings(as.numeric(as.character(x)))
)

# replace NA in species data with 0
abund[sp_cols][is.na(abund[sp_cols])] <- 0

# community matrix
comm <- abund %>%
  select(Sample, all_of(sp_cols)) %>%
  as.data.frame()

rownames(comm) <- comm$Sample
comm$Sample <- NULL
comm <- as.matrix(comm)

comm <- comm[, colSums(comm) > 0, drop = FALSE]   # remove zero-sum species

# metadata
meta <- abund %>%
  select(Sample, Site, Season) %>%
  distinct()

# ============================================================
# READ ENVIRONMENTAL DATA
# ============================================================
env <- read_excel(file_env)

# rename first two columns
names(env)[1:2] <- c("Site", "Season")

# create sample name
env <- env %>%
  mutate(Sample = paste(Site, Season, sep = "_"))

# keep required columns
env_keep <- c(
  "Sample",
  "CAL", "CHL", "COL", "MAG", "NIT", "pH",
  "TOA", "THA", "TDS", "TUR", "WAT"
)

# check missing columns
miss_env <- setdiff(env_keep, names(env))
if (length(miss_env) > 0) {
  stop(paste("Missing env columns:", paste(miss_env, collapse = ", ")))
}

# build environmental matrix
env_mat <- env %>%
  select(all_of(env_keep)) %>%
  mutate(across(-Sample, ~ suppressWarnings(as.numeric(as.character(.x))))) %>%
  as.data.frame()

rownames(env_mat) <- env_mat$Sample
env_mat$Sample <- NULL

# identify all-NA columns
all_na_cols <- names(env_mat)[sapply(env_mat, function(x) all(is.na(x)))]
if (length(all_na_cols) > 0) {
  message("⚠ These columns became all NA after numeric conversion: ",
          paste(all_na_cols, collapse = ", "))
}

# fill NA with median; if all NA then set 0
for (j in seq_len(ncol(env_mat))) {
  if (anyNA(env_mat[, j])) {
    if (all(is.na(env_mat[, j]))) {
      env_mat[, j] <- 0
    } else {
      env_mat[is.na(env_mat[, j]), j] <- median(env_mat[, j], na.rm = TRUE)
    }
  }
}

# remove zero-variance columns safely
sd_vals <- sapply(env_mat, function(x) sd(x, na.rm = TRUE))
keep_cols <- !is.na(sd_vals) & sd_vals > 0
env_mat <- env_mat[, keep_cols, drop = FALSE]

# ============================================================
# MATCH SAMPLES
# ============================================================
common <- intersect(rownames(comm), rownames(env_mat))

if (length(common) < 3) {
  stop("Need >= 3 matched samples.")
}

comm <- comm[common, , drop = FALSE]
env_mat <- env_mat[common, , drop = FALSE]
meta <- meta %>% filter(Sample %in% common)

# ============================================================
# DEFINE 5 ENVIRONMENTAL GROUPS
# ============================================================
groups <- list(
  G1 = c("CAL", "CHL"),
  G2 = c("COL", "MAG"),
  G3 = c("NIT", "pH"),
  G4 = c("TOA", "THA"),
  G5 = c("TDS", "TUR", "WAT")
)

group_titles <- c(
  G1 = "G1: CAL + CHL",
  G2 = "G2: COL + MAG",
  G3 = "G3: NIT + pH",
  G4 = "G4: TOA + THA",
  G5 = "G5: TDS + TUR + WAT"
)

# ============================================================
# FUNCTION TO RUN CCA FOR ONE GROUP
# ============================================================
run_cca_group <- function(comm, env_mat, vars) {
  
    miss <- setdiff(vars, colnames(env_mat))
  if (length(miss) > 0) {
    stop(paste("Missing env vars:", paste(miss, collapse = ", ")))
  }
  
    env_g <- as.data.frame(scale(env_mat[, vars, drop = FALSE]))
  
  # run cca
  mod <- cca(comm ~ ., data = env_g)
  
  return(mod)
}

# ============================================================
# FUNCTION TO EXTRACT NATURE-STYLE TABLE 3
# ============================================================
make_nature_table <- function(mod, group_name, axes = 4) {
  
  eig_vals <- as.numeric(eigenvals(mod))
  
  total_inertia <- sum(eig_vals, na.rm = TRUE)
  
  # constrained eigenvalues
  canonical <- mod$CCA$eig
  if (is.null(canonical)) canonical <- numeric(0)
  canonical <- as.numeric(canonical)
  
  # species-environment correlations
  sp_env_corr <- tryCatch({
    as.numeric(summary(mod)$cont$correlation)
  }, error = function(e) {
    rep(NA, length(eig_vals))
  })
  
  # percentages for species data
  percent_species <- (eig_vals / total_inertia) * 100
  cum_species <- cumsum(percent_species)
  
  # percentages for species-environment relation
  cum_env <- if (length(canonical) > 0) {
    cumsum((canonical / total_inertia) * 100)
  } else {
    numeric(0)
  }
  
   pad_vals <- function(x, digits = 3, n = axes) {
    x <- round(x, digits)
    x <- as.character(x)
    if (length(x) < n) x <- c(x, rep("", n - length(x)))
    if (length(x) > n) x <- x[1:n]
    x
  }
  
  pad_vals1 <- function(x, n = axes) {
    x <- round(x, 1)
    x <- as.character(x)
    if (length(x) < n) x <- c(x, rep("", n - length(x)))
    if (length(x) > n) x <- x[1:n]
    x
  }
  
  eig_show <- pad_vals(eig_vals, digits = 3, n = axes)
  corr_show <- pad_vals(sp_env_corr, digits = 3, n = axes)
  cum_species_show <- pad_vals1(cum_species, n = axes)
  cum_env_show <- pad_vals1(cum_env, n = axes)
  
  tbl <- data.frame(
    Axes = c(
      "Eigenvalues",
      "Species-environment correlations",
      "Cumulative percentage variance of species data",
      "Cumulative percentage variance of species-environment relation",
      "Sum of all eigenvalues",
      "Sum of all canonical eigenvalues"
    ),
    `1` = c(eig_show[1], corr_show[1], cum_species_show[1], cum_env_show[1], "", ""),
    `2` = c(eig_show[2], corr_show[2], cum_species_show[2], cum_env_show[2], "", ""),
    `3` = c(eig_show[3], corr_show[3], cum_species_show[3], cum_env_show[3], "", ""),
    `4` = c(eig_show[4], corr_show[4], cum_species_show[4], cum_env_show[4], "", ""),
    `Total inertia` = c(
      round(total_inertia, 3),
      "",
      "",
      "",
      round(total_inertia, 3),
      round(sum(canonical), 3)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  list(
    title = paste0("Table 3. Eigenvalues for all axes (", group_name, ")"),
    table = tbl,
    eigenvalues = eig_vals,
    total_inertia = total_inertia,
    canonical_sum = sum(canonical)
  )
}

# ============================================================
# RUN CCA FOR ALL 5 GROUPS
# ============================================================
mod_G1 <- run_cca_group(comm, env_mat, groups$G1)
mod_G2 <- run_cca_group(comm, env_mat, groups$G2)
mod_G3 <- run_cca_group(comm, env_mat, groups$G3)
mod_G4 <- run_cca_group(comm, env_mat, groups$G4)
mod_G5 <- run_cca_group(comm, env_mat, groups$G5)

# ============================================================
# CREATE NATURE-STYLE TABLES
# ============================================================
tab_G1 <- make_nature_table(mod_G1, group_titles["G1"], axes = 4)
tab_G2 <- make_nature_table(mod_G2, group_titles["G2"], axes = 4)
tab_G3 <- make_nature_table(mod_G3, group_titles["G3"], axes = 4)
tab_G4 <- make_nature_table(mod_G4, group_titles["G4"], axes = 4)
tab_G5 <- make_nature_table(mod_G5, group_titles["G5"], axes = 4)

# ============================================================
# PRINT TABLES IN CONSOLE
# ============================================================
cat("\n============================================================\n")
cat(tab_G1$title, "\n")
cat("============================================================\n")
print(tab_G1$table, row.names = FALSE)

cat("\n============================================================\n")
cat(tab_G2$title, "\n")
cat("============================================================\n")
print(tab_G2$table, row.names = FALSE)

cat("\n============================================================\n")
cat(tab_G3$title, "\n")
cat("============================================================\n")
print(tab_G3$table, row.names = FALSE)

cat("\n============================================================\n")
cat(tab_G4$title, "\n")
cat("============================================================\n")
print(tab_G4$table, row.names = FALSE)

cat("\n============================================================\n")
cat(tab_G5$title, "\n")
cat("============================================================\n")
print(tab_G5$table, row.names = FALSE)

# ============================================================
# COMBINED LONG TABLE FOR CSV
# ============================================================
make_long_export <- function(tab_obj, group_name) {
  out <- tab_obj$table
  out$Group <- group_name
  out <- out[, c("Group", "Axes", "1", "2", "3", "4", "Total inertia")]
  out
}

combined_export <- bind_rows(
  make_long_export(tab_G1, group_titles["G1"]),
  make_long_export(tab_G2, group_titles["G2"]),
  make_long_export(tab_G3, group_titles["G3"]),
  make_long_export(tab_G4, group_titles["G4"]),
  make_long_export(tab_G5, group_titles["G5"])
)

write.csv(combined_export, "CCA_Nature_Table3_All_Groups.csv", row.names = FALSE)

# ============================================================
# EXPORT TO EXCEL WITH FORMATTING
# ============================================================
wb <- createWorkbook()

# styles
title_style <- createStyle(
  fontName = "Times New Roman",
  fontSize = 12,
  textDecoration = "bold",
  halign = "center",
  valign = "center"
)

header_style <- createStyle(
  fontName = "Times New Roman",
  fontSize = 11,
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

body_style <- createStyle(
  fontName = "Times New Roman",
  fontSize = 11,
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

left_style <- createStyle(
  fontName = "Times New Roman",
  fontSize = 11,
  halign = "left",
  valign = "center",
  border = "TopBottomLeftRight"
)

write_group_sheet <- function(wb, sheet_name, tab_obj) {
  addWorksheet(wb, sheet_name)
  
  writeData(wb, sheet = sheet_name, x = tab_obj$title, startRow = 1, startCol = 1)
  mergeCells(wb, sheet = sheet_name, cols = 1:6, rows = 1)
  addStyle(wb, sheet = sheet_name, style = title_style, rows = 1, cols = 1:6, gridExpand = TRUE)
  
  writeData(wb, sheet = sheet_name, x = tab_obj$table, startRow = 3, startCol = 1, colNames = TRUE)
  
  addStyle(wb, sheet = sheet_name, style = header_style, rows = 3, cols = 1:6, gridExpand = TRUE)
  
  addStyle(wb, sheet = sheet_name, style = body_style, rows = 4:(nrow(tab_obj$table) + 3), cols = 2:6, gridExpand = TRUE)
  addStyle(wb, sheet = sheet_name, style = left_style, rows = 4:(nrow(tab_obj$table) + 3), cols = 1, gridExpand = TRUE)
  
  setColWidths(wb, sheet = sheet_name, cols = 1, widths = 52)
  setColWidths(wb, sheet = sheet_name, cols = 2:5, widths = 12)
  setColWidths(wb, sheet = sheet_name, cols = 6, widths = 16)
  
  setRowHeights(wb, sheet = sheet_name, rows = 1, heights = 22)
  setRowHeights(wb, sheet = sheet_name, rows = 3:(nrow(tab_obj$table) + 3), heights = 20)
}

write_group_sheet(wb, "G1", tab_G1)
write_group_sheet(wb, "G2", tab_G2)
write_group_sheet(wb, "G3", tab_G3)
write_group_sheet(wb, "G4", tab_G4)
write_group_sheet(wb, "G5", tab_G5)

saveWorkbook(wb, "Nature_Table3_CCA_All_Groups.xlsx", overwrite = TRUE)

# ============================================================
# OPTIONAL: SAVE INDIVIDUAL CSV FILES
# ============================================================
write.csv(tab_G1$table, "Table3_G1_CAL_CHL.csv", row.names = FALSE)
write.csv(tab_G2$table, "Table3_G2_COL_MAG.csv", row.names = FALSE)
write.csv(tab_G3$table, "Table3_G3_NIT_pH.csv", row.names = FALSE)
write.csv(tab_G4$table, "Table3_G4_TOA_THA.csv", row.names = FALSE)
write.csv(tab_G5$table, "Table3_G5_TDS_TUR_WAT.csv", row.names = FALSE)

# ============================================================
# OPTIONAL: SHOW SUMMARY NUMBERS
# ============================================================
summary_df <- data.frame(
  Group = c(group_titles["G1"], group_titles["G2"], group_titles["G3"], group_titles["G4"], group_titles["G5"]),
  Total_Inertia = c(tab_G1$total_inertia, tab_G2$total_inertia, tab_G3$total_inertia, tab_G4$total_inertia, tab_G5$total_inertia),
  Canonical_Eigen_Sum = c(tab_G1$canonical_sum, tab_G2$canonical_sum, tab_G3$canonical_sum, tab_G4$canonical_sum, tab_G5$canonical_sum)
)

cat("\n============================================================\n")
cat("SUMMARY OF TOTAL INERTIA AND CANONICAL EIGENVALUE SUM\n")
cat("============================================================\n")
print(summary_df, row.names = FALSE)

write.csv(summary_df, "CCA_Group_Summary.csv", row.names = FALSE)