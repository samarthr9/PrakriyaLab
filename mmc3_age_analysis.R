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

# Function to reshape the data for plotting
reshape_data <- function(data, gene_name) {
  data_long <- data %>%
    pivot_longer(cols = starts_with(c('4mo', '2yo')),
                 names_to = "Condition",
                 values_to = "Expression") %>%
    mutate(Gene = gene_name) %>%
    filter(!grepl("IN", Condition))  # Remove IN bars
  return(data_long)
}

# Reshape the data for each gene
stim1_long <- reshape_data(stim1, 'Stim1')
stim2_long <- reshape_data(stim2, 'Stim2')
orai1_long <- reshape_data(orai1, 'Orai1')
orai2_long <- reshape_data(orai2, 'Orai2')
orai3_long <- reshape_data(orai3, 'Orai3')

# Combine all data into a single data frame
combined_data <- bind_rows(stim1_long, stim2_long, orai1_long, orai2_long, orai3_long)

# Define the order of brain regions
condition_levels <- c(
  "4mo MC1", "4mo MC2", "4mo MC3",
  "2yo MC1", "2yo MC2", "2yo MC3",
  "4mo SC1", "4mo SC2", "4mo SC3",
  "4mo VC1", "4mo VC2", "4mo VC3",
  "2yo VC1", "2yo VC2", "2yo VC3",
  "4mo HTH1", "4mo HTH2", "4mo HTH3",
  "2yo HTH1", "2yo HTH2", "2yo HTH3",
  "4mo CB1", "4mo CB2", "4mo CB3",
  "2yo CB1", "2yo CB2", "2yo CB3"
)

# Apply the order to the Condition column
combined_data$Condition <- factor(combined_data$Condition, levels = condition_levels)

# Define color mapping for conditions
condition_colors <- c("4mo MC1" = "green", "4mo MC2" = "green", "4mo MC3" = "green",
                      "2yo MC1" = "blue", "2yo MC2" = "blue", "2yo MC3" = "blue",
                      "4mo SC1" = "green", "4mo SC2" = "green", "4mo SC3" = "green",
                      "4mo VC1" = "green", "4mo VC2" = "green", "4mo VC3" = "green",
                      "2yo VC1" = "blue", "2yo VC2" = "blue", "2yo VC3" = "blue",
                      "4mo HTH1" = "green", "4mo HTH2" = "green", "4mo HTH3" = "green",
                      "2yo HTH1" = "blue", "2yo HTH2" = "blue", "2yo HTH3" = "blue",
                      "4mo CB1" = "green", "4mo CB2" = "green", "4mo CB3" = "green",
                      "2yo CB1" = "blue", "2yo CB2" = "blue", "2yo CB3" = "blue")

# Plotting function
plot_gene <- function(df, gene_name) {
  ggplot(df %>% filter(Gene == gene_name), aes(x = Condition, y = Expression, fill = Condition)) +
    geom_bar(stat = "identity", position = position_dodge()) +
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
      y = "Expression"
    )
}

# Save the plots
ggsave("Stim1_plot.png", plot_gene(combined_data, "Stim1"))
ggsave("Stim2_plot.png", plot_gene(combined_data, "Stim2"))
ggsave("Orai1_plot.png", plot_gene(combined_data, "Orai1"))
ggsave("Orai2_plot.png", plot_gene(combined_data, "Orai2"))
ggsave("Orai3_plot.png", plot_gene(combined_data, "Orai3"))
