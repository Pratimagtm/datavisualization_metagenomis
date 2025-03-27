library(tidyverse)
library(readxl)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(wesanderson)
library(cowplot)


excel_path <-("/home/pratima/Insync/pgautam1@umbc.edu/Google Drive/Cusick Lab/Metagenomic_analysis/Megan7_analysis/megan7_bacteria/figure6/cugenehomologcount.xlsx")
#View(excel_path)

# Get all sheet names
sheet_names <- excel_sheets(excel_path)

# Loop through each sheet
for (sheet in sheet_names) {
sheet_data <- #read_excel(excel_path, sheet = sheet)
  read.xlsx(excel_path, sheet = sheet, colNames = TRUE)
# Data
data <- data.frame(
  CopperProteins <- sheet_data[1],
  Biofilm <- sheet_data[2],
  Water <- sheet_data[3]
)
View(data)
data_long <- data %>%
  gather(key = "Condition", value = "Count", Biofilm, Water)

# Calculate total counts per condition (Biofilm, Water)
#total_counts <- data_long %>%
 # group_by(Condition) %>%
#  summarise(Total = sum(Count))

# Merge the total counts with the long-format data
#data_long <- data_long %>%
#  left_join(total_counts, by = "Condition") %>%
#  Percentage = Count
  #mutate(Percentage = Count / Total * 100)

# Order organisms within each condition based on their percentage contribution
#data_long <- data_long %>%
 # group_by(Condition) %>%
  #mutate(Category = fct_reorder(Category, Percentage, .desc = FALSE))

View(data_long)
# Create the bar plot
p4 <-ggplot(data_long, aes(x = CopperProteins, y = Count, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge2(padding = 0)) +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +  # Y-axis starts at 0
  scale_x_discrete(expand = c(0, 0))+     # Ensure no expansion for x-axis
  labs(
    title = ("Copper associated protein homolog count across samples"),
    x = ("Copper Proteins"),
    y = "Count"
  ) +
  scale_fill_manual(values = c("Biofilm" = "steelblue", "Water" = "#990000")) +
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle=0, vjust=0))+
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid = element_blank(),        # Remove grid lines
        axis.line = element_line(color = "black")  # Show x and y axis lines
  )
folder = getwd()
#View(folder)
ggsave(path =folder, file= paste0(sheet,".pdf"), plot = p4, width = 8, height = 6) #saves only figure
}