library(tidyverse)
library(readxl)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(wesanderson)
library(cowplot)


excel_path <-("/home/pratima/Insync/pgautam1@umbc.edu/Google Drive/Cusick Lab/Metagenomic_analysis/Megan7_analysis/megan7_bacteria/figure123/genus.xlsx")
#View(excel_path)

sheet_data <- read.xlsx(excel_path, sheet = "genus", colNames = TRUE)
# Data
data <- data.frame(
  Organisms <- sheet_data[1],
  Biofilm <- sheet_data[2],
  Water <- sheet_data[3]
)

#View(data)
#change data to long format
# Reshape the data into long format

data_long <- data %>%
  gather(key = "Condition", value = "Count", Biofilm, Water)

# Calculate total counts per condition (Biofilm, Water)
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

View(data_long)
# Create the stacked bar plot
custom_colors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(271)
p<- ggplot(data_long, aes(x = Condition, y = Percentage, fill = Organisms)) + 
                      #xlim = c(0, 200), ylim = c(0, 100))) +
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
  #theme(legend.position = "right")  # Position legend on the right+
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

#scale_color_gradientn(colours = rainbow(26))
  #scale_fill_manual()
  #scale_fill_discrete()
  #scale_colour_gradient()
#scale_fill_brewer(palette="Accent")