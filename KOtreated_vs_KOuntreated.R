# Set the working directory
setwd("/Users/samarthrao/Downloads/Lab_Data/Mikki's_Paper/KOtreated_vs_KOuntreated")

# Load the CSV file into a data frame
KOtreated_vs_KOuntreated_data <- read.csv("./211203_KOtreated_vs_KOuntreated.csv")

# Keep rows where padj is less than 0.05, 0.01, and 0.001
significant_data_0.05 <- subset(KOtreated_vs_KOuntreated_data, padj <= 0.05)
significant_data_0.01 <- subset(KOtreated_vs_KOuntreated_data, padj <= 0.01)
significant_data_0.001 <- subset(KOtreated_vs_KOuntreated_data, padj <= 0.001)

# Function to create log2FoldChange subsets
create_log2fold_subsets <- function(data) {
  list(
    log2fold_0_to_minus5 = subset(data, log2FoldChange <= 0 & log2FoldChange > -5),
    log2fold_minus5_to_minus10 = subset(data, log2FoldChange <= -5 & log2FoldChange > -10),
    log2fold_minus10_to_minus15 = subset(data, log2FoldChange <= -10 & log2FoldChange > -15),
    log2fold_minus15_to_minus20 = subset(data, log2FoldChange <= -15 & log2FoldChange > -20),
    log2fold_0_to_5 = subset(data, log2FoldChange > 0 & log2FoldChange <= 5),
    log2fold_5_to_10 = subset(data, log2FoldChange > 5 & log2FoldChange <= 10),
    log2fold_10_to_15 = subset(data, log2FoldChange > 10 & log2FoldChange <= 15),
    log2fold_15_to_20 = subset(data, log2FoldChange > 15 & log2FoldChange <= 20)
  )
}

# Create subsets for each significant data part
subsets_0.05 <- create_log2fold_subsets(significant_data_0.05)
subsets_0.01 <- create_log2fold_subsets(significant_data_0.01)
subsets_0.001 <- create_log2fold_subsets(significant_data_0.001)

# Extract and view the genes for each subset using dplyr
library(dplyr)

extract_genes <- function(subsets) {
  lapply(subsets, function(df) df %>% select(external_gene_name))
}

genes_0.05 <- extract_genes(subsets_0.05)
genes_0.01 <- extract_genes(subsets_0.01)
genes_0.001 <- extract_genes(subsets_0.001)


