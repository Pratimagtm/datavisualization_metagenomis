library(tidyverse)
library(readxl)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(wesanderson)

#Excel file includes count for a SEED functional profiling of cell envelope categories for 2 sample types
#Can be applied to any functional groups in the same excel_path within different sheets
excel_path <-("../cellenvelope.xlsx")

sheet_data <- read.xlsx(excel_path, sheet = "Sheet1", colNames = TRUE)
# Data includes count data for systems under cell envelope for Sample1 and Sample 2
data <- data.frame(
  Category <- sheet_data[1],
  Sample1 <- sheet_data[2],
  Sample2 <- sheet_data[3]
)
View(data)
data_long <- data %>%
  gather(key = "Condition", value = "Count", Sample1, Sample2)

# Calculate total counts per condition (Biofilm, Water)
total_counts <- data_long %>%
  group_by(Condition) %>%
  summarise(Total = sum(Count))

# Merge the total counts with the long-format data
data_long <- data_long %>%
  left_join(total_counts, by = "Condition") %>%
  mutate(Percentage = Count / Total * 100)

# Create the bar plot
p4 <-ggplot(data_long, aes(x = Category, y = Percentage, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge2") +
  labs(
    title = "Cell Envelope Profile",
    x = "Cell envelope",
    y = "Number of aligned bases(%)"
  ) +
  scale_fill_manual(values = c("Sample1" = "steelblue", "Sample2" = "#990000")) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle=90, vjust=0))
folder = getwd()
ggsave(path =folder, file="figure4.pdf", plot = p4, width = 12, height = 8) 