library(tidyverse)
library(readxl)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(wesanderson)
library(cowplot)


excel_path <-("../genus.xlsx")

sheet_data <- read.xlsx(excel_path, sheet = "genus", colNames = TRUE)
# Data with count for 2 different sample types
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
custom_colors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(271)
p<- ggplot(data_long, aes(x = Condition, y = Percentage, fill = Organisms)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +  # Y-axis starts at 0
  scale_x_discrete(expand = c(0, 0))+     # Ensure no expansion for x-axis
  labs(title = "Taxonomic profile comparison at genus rank",
       x = "Sample", 
       y = "Number of aligned bases(%)") +
  scale_fill_manual(values = custom_colors)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none",
        panel.grid = element_blank(),        # Remove grid lines
        axis.line = element_line(color = "black")  # Show x and y axis lines
        )  # Remove the legend from the plot itself

# Create the legend separately
p2 <- ggplot(data_long, aes(x = Condition, y = Percentage, fill = Organisms)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors)+
theme(
  legend.text = element_text(size = 20),  # Correct use inside theme()
  legend.title = element_text(size = 20)
) +
  guides(fill = guide_legend(ncol = 4)) +  # Correctly applied guides()
  labs(fill = "Taxa") 

# Extract the legend from p2 using cowplot's get_legend() function
legend <- get_legend(p2)

# Arrange the plot and the legend side by side (or vertically)
# combined_plot <- plot_grid(p, legend, ncol = 2, rel_widths = c(0.8, 0.2))

# Save the combined plot
#ggsave("stacked_bar_plot_with_separate_legend.pdf", plot = combined_plot, width = 14, height = 8)

ggsave("figure2.pdf", plot = p, width = 8, height = 8) #saves only figure
ggsave("figure2led.pdf", plot = legend, width = 20, height = 20, limitsize = FALSE)# saves only ledgend
