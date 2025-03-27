library(tidyverse)
library(readxl)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(wesanderson)
library(cowplot)

excel_path <-("/home/pratima/Insync/pgautam1@umbc.edu/Google Drive/Cusick Lab/Metagenomic_analysis/Megan7_analysis/megan7_bacteria/figure4/cellenvelope.xlsx")
#View(excel_path)

sheet_data <- read.xlsx(excel_path, sheet = "Sheet1", colNames = TRUE)
# Data
data <- data.frame(
  Category <- sheet_data[1],
  Biofilm <- sheet_data[2],
  Water <- sheet_data[3]
)
View(data)
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
#data_long <- data_long %>%
 # group_by(Condition) %>%
  #mutate(Category = fct_reorder(Category, Percentage, .desc = FALSE))

View(data_long)
# Create the bar plot
p4 <-ggplot(data_long, aes(x = Category, y = Percentage, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge2") +
  labs(
    title = "Cell Envelope Profile",
    x = "Cell envelope",
    y = "Number of aligned bases(%)"
  ) +
  scale_fill_manual(values = c("Biofilm" = "steelblue", "Water" = "#990000")) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle=90, vjust=0))
folder = getwd()
View(folder)
ggsave(path =folder, file="figure4.pdf", plot = p4, width = 12, height = 8) #saves only figure