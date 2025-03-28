library(tidyverse)
library(readxl)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(wesanderson)

excel_path <-("../phylum.xlsx")

sheet_data <- read.xlsx(excel_path, sheet = "phylum", colNames = TRUE)
# Data with count for 2 samples
data <- data.frame(
  Organisms <- sheet_data[1],
  Sample1 <- sheet_data[2],
  Sample2 <- sheet_data[3]
)

#change data to long format
# Reshape the data into long format

data_long <- data %>%
  gather(key = "Condition", value = "Count", Sample1, Sample2)

# Calculate total counts per condition (Sample1, Sample2)
total_counts <- data_long %>%
  group_by(Condition) %>%
  summarise(Total = sum(Count))

# Merge the total counts with the long-format data
data_long <- data_long %>%
  left_join(total_counts, by = "Condition") %>%
  mutate(Percentage = Count / Total * 100)

# Order organisms within each condition based on their percentage contribution
data_long <- data_long %>%
  group_by(Condition) %>%
  mutate(Organisms = fct_reorder(Organisms, Percentage, .desc = FALSE))

# Create the stacked bar plot
custom_colors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(39)
p <- ggplot(data_long, aes(x = Condition, y = Percentage, fill = Organisms)) +
  geom_bar(stat = "identity")  +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +  # Y-axis starts at 0
  scale_x_discrete(expand = c(0, 0))+     # Ensure no expansion for x-axis
  labs(title = "Taxonomic profile comparison at phylum rank",
       x = "Sample", 
       y = "Number of aligned bases(%)") +
  scale_fill_manual(values = custom_colors)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    legend.text = element_text(size = 9),  # Correct use inside theme()
    legend.title = element_text(size = 12),
    panel.grid = element_blank(),        # Remove grid lines
    axis.line = element_line(color = "black")  # Show x and y axis lines
  ) +labs(fill = "Taxa")

ggsave("figure1.pdf", plot = p, width = 10, height = 8) 