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
genes <- c("Stim1", "Stim2", "Orai1", "Orai2", "Orai3")
gene_data_filtered <- gene_data %>% filter(`Gene name...1` %in% genes)

# Define the brain regions and conditions
brain_regions <- c("MC", "VC", "HTH", "CB")
conditions <- c("4mo", "2yo")

# Function to calculate the mean for specific columns
calculate_mean <- function(data, prefix) {
  columns <- grep(paste0(prefix, "[1-3]"), names(data), value = TRUE)
  data_subset <- data[columns]
  means <- rowMeans(data_subset, na.rm = TRUE)
  return(means)
}

# Create a function to generate the table and plot for each brain region
generate_region_plot <- function(region) {
  means_list <- list()
  for (gene in genes) {
    gene_data_subset <- gene_data_filtered %>% filter(`Gene name...1` == gene)
    for (condition in conditions) {
      mean_expression <- calculate_mean(gene_data_subset, paste0(condition, " ", region))
      means_list[[paste(gene, condition, region, sep = "_")]] <- mean_expression
    }
  }
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    Condition = rep(paste(rep(genes, each = 2), rep(conditions, times = length(genes)), region, sep = "_"), each = 1),
    Expression = unlist(means_list)
  )
  
  plot_data <- plot_data %>%
    separate(Condition, into = c("Gene", "Age", "Region"), sep = "_")
  
  # Define the order of the conditions on the x-axis
  levels_order <- c('Orai1_4mo', 'Orai1_2yo', 'Orai2_4mo', 'Orai2_2yo', 'Orai3_4mo', 'Orai3_2yo', 'Stim1_4mo', 'Stim1_2yo', 'Stim2_4mo', 'Stim2_2yo')
  
  plot_data$Condition <- factor(paste(plot_data$Gene, plot_data$Age, sep = "_"), levels = levels_order)
  
  # Plot the data
  p <- ggplot(plot_data, aes(x = Condition, y = Expression, fill = Age)) +
    geom_bar(stat = "identity", position = "dodge") +
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
      title = paste("Gene Expression in", region),
      x = "Gene and Age",
      y = "Mean Expression"
    )
  
  # Save the plot
  ggsave(paste0(region, "_averages_plot.png"), p)
}

# Generate the plots for each brain region
for (region in brain_regions) {
  generate_region_plot(region)
}
