library(tidyverse)
library(readxl)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(wesanderson)

excel_path <-("../homologcount.xlsx")

# Get all sheet names
sheet_names <- excel_sheets(excel_path)

# Loop through each sheet
for (sheet in sheet_names) {
sheet_data <- #read_excel(excel_path, sheet = sheet)
  read.xlsx(excel_path, sheet = sheet, colNames = TRUE)
# Data for a protein homology count across two sample types
data <- data.frame(
  Proteins <- sheet_data[1],
  Sample1 <- sheet_data[2],
  Sample2 <- sheet_data[3]
)
View(data)
data_long <- data %>%
  gather(key = "Condition", value = "Count", Sample1, Sample2)

# Create the bar plot
p4 <-ggplot(data_long, aes(x = CopperProteins, y = Count, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge2(padding = 0)) +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +  # Y-axis starts at 0
  scale_x_discrete(expand = c(0, 0))+     # Ensure no expansion for x-axis
  labs(
    title = (".. protein homolog count across samples"),
    x = (".. Proteins"),
    y = "Count"
  ) +
  scale_fill_manual(values = c("Sample1" = "steelblue", "Sample2" = "#990000")) +
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle=0, vjust=0))+
  theme(panel.grid = element_blank(),        # Remove grid lines
        axis.line = element_line(color = "black")  # Show x and y axis lines
  )
folder = getwd()
ggsave(path =folder, file= paste0(sheet,".pdf"), plot = p4, width = 8, height = 6) #saves only figure
}