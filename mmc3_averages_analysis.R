# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Set the working directory
setwd("/Users/samarthrao/Downloads/Lab_Data/ScienceDirect_files/raw_data")

# Load the Excel file into a data frame
gene_data <- read_excel("1-s2.0-S221112471731848X-mmc3.xlsx")

# Filter the data for specific genes
stim1 <- gene_data %>% filter(`Gene name...1` == "Stim1")
stim2 <- gene_data %>% filter(`Gene name...1` == "Stim2")
orai1 <- gene_data %>% filter(`Gene name...1` == "Orai1")
orai2 <- gene_data %>% filter(`Gene name...1` == "Orai2")
orai3 <- gene_data %>% filter(`Gene name...1` == "Orai3")

# Function to calculate the mean for specific columns
calculate_mean <- function(data, prefix) {
  columns <- grep(paste0(prefix, "[1-3]"), names(data), value = TRUE)
  data_subset <- data[columns]
  means <- rowMeans(data_subset, na.rm = TRUE)
  return(means)
}

# Calculate the means for each gene
stim1_means <- c(calculate_mean(stim1, '4mo MC'),
                 calculate_mean(stim1, '2yo MC'),
                 calculate_mean(stim1, '4mo SC'),
                 # calculate_mean(stim1, '2yo SC'), # No data for this
                 calculate_mean(stim1, '4mo VC'),
                 calculate_mean(stim1, '2yo VC'),
                 calculate_mean(stim1, '4mo HTH'),
                 calculate_mean(stim1, '2yo HTH'),
                 calculate_mean(stim1, '4mo CB'),
                 calculate_mean(stim1, '2yo CB'))

stim2_means <- c(calculate_mean(stim2, '4mo MC'),
                 calculate_mean(stim2, '2yo MC'),
                 calculate_mean(stim2, '4mo SC'),
                 # calculate_mean(stim2, '2yo SC'), # No data for this
                 calculate_mean(stim2, '4mo VC'),
                 calculate_mean(stim2, '2yo VC'),
                 calculate_mean(stim2, '4mo HTH'),
                 calculate_mean(stim2, '2yo HTH'),
                 calculate_mean(stim2, '4mo CB'),
                 calculate_mean(stim2, '2yo CB'))

orai1_means <- c(calculate_mean(orai1, '4mo MC'),
                 calculate_mean(orai1, '2yo MC'),
                 calculate_mean(orai1, '4mo SC'),
                 # calculate_mean(orai1, '2yo SC'), # No data for this
                 calculate_mean(orai1, '4mo VC'),
                 calculate_mean(orai1, '2yo VC'),
                 calculate_mean(orai1, '4mo HTH'),
                 calculate_mean(orai1, '2yo HTH'),
                 calculate_mean(orai1, '4mo CB'),
                 calculate_mean(orai1, '2yo CB'))

orai2_means <- c(calculate_mean(orai2, '4mo MC'),
                 calculate_mean(orai2, '2yo MC'),
                 calculate_mean(orai2, '4mo SC'),
                 # calculate_mean(orai2, '2yo SC'), # No data for this
                 calculate_mean(orai2, '4mo VC'),
                 calculate_mean(orai2, '2yo VC'),
                 calculate_mean(orai2, '4mo HTH'),
                 calculate_mean(orai2, '2yo HTH'),
                 calculate_mean(orai2, '4mo CB'),
                 calculate_mean(orai2, '2yo CB'))

orai3_means <- c(calculate_mean(orai3, '4mo MC'),
                 calculate_mean(orai3, '2yo MC'),
                 calculate_mean(orai3, '4mo SC'),
                 # calculate_mean(orai3, '2yo SC'), # No data for this
                 calculate_mean(orai3, '4mo VC'),
                 calculate_mean(orai3, '2yo VC'),
                 calculate_mean(orai3, '4mo HTH'),
                 calculate_mean(orai3, '2yo HTH'),
                 calculate_mean(orai3, '4mo CB'),
                 calculate_mean(orai3, '2yo CB'))

# Create a data frame for each gene
stim1_df <- data.frame(
  Condition = c('4mo MC', '2yo MC', '4mo SC', '4mo VC', '2yo VC', 
                '4mo HTH', '2yo HTH', '4mo CB', '2yo CB'),
  Expression = stim1_means
)

stim2_df <- data.frame(
  Condition = c('4mo MC', '2yo MC', '4mo SC', '4mo VC', '2yo VC', 
                '4mo HTH', '2yo HTH', '4mo CB', '2yo CB'),
  Expression = stim2_means
)

orai1_df <- data.frame(
  Condition = c('4mo MC', '2yo MC', '4mo SC', '4mo VC', '2yo VC', 
                '4mo HTH', '2yo HTH', '4mo CB', '2yo CB'),
  Expression = orai1_means
)

orai2_df <- data.frame(
  Condition = c('4mo MC', '2yo MC', '4mo SC', '4mo VC', '2yo VC', 
                '4mo HTH', '2yo HTH', '4mo CB', '2yo CB'),
  Expression = orai2_means
)

orai3_df <- data.frame(
  Condition = c('4mo MC', '2yo MC', '4mo SC', '4mo VC', '2yo VC', 
                '4mo HTH', '2yo HTH', '4mo CB', '2yo CB'),
  Expression = orai3_means
)

# Ensure consistent ordering of levels
levels_order <- c('4mo MC', '2yo MC', '4mo SC', '4mo VC', '2yo VC', 
                  '4mo HTH', '2yo HTH', '4mo CB', '2yo CB')
stim1_df$Condition <- factor(stim1_df$Condition, levels = levels_order)
stim2_df$Condition <- factor(stim2_df$Condition, levels = levels_order)
orai1_df$Condition <- factor(orai1_df$Condition, levels = levels_order)
orai2_df$Condition <- factor(orai2_df$Condition, levels = levels_order)
orai3_df$Condition <- factor(orai3_df$Condition, levels = levels_order)

# Define color mapping for conditions
condition_colors <- c("4mo MC" = "green", "2yo MC" = "blue", "4mo SC" = "green", 
                      "4mo VC" = "green", "2yo VC" = "blue", "4mo HTH" = "green", 
                      "2yo HTH" = "blue", "4mo CB" = "green", "2yo CB" = "blue")

# Plotting function
plot_gene <- function(df, gene_name) {
  ggplot(df, aes(x = Condition, y = Expression, fill = Condition)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = condition_colors) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "black"),
      panel.grid.minor = element_line(color = "black"),
      legend.position = "none"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = paste("Gene Expression in Different Brain Regions -", gene_name),
      x = "Brain Region",
      y = "Mean Expression"
    )
}

# Save the plots
ggsave("Stim1_averages_plot.png", plot_gene(stim1_df, "Stim1"))
ggsave("Stim2_averages_plot.png", plot_gene(stim2_df, "Stim2"))
ggsave("Orai1_averages_plot.png", plot_gene(orai1_df, "Orai1"))
ggsave("Orai2_averages_plot.png", plot_gene(orai2_df, "Orai2"))
ggsave("Orai3_averages_plot.png", plot_gene(orai3_df, "Orai3"))
