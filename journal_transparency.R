# Load necessary libraries
library(dplyr)
library(ggplot2)
getwd()
setwd("C:/Users/PC/Desktop/projects/R")

# Import the data from CSV
data <- read.csv("./data.csv")

# Calculate average transparency score by journal
average_transparency <- data %>%
  group_by(Journal) %>%
  summarise(AvgTransparency = round(mean(Transparency.score..1.5.), 2))

# Print the result
print(average_transparency)

# Visualize the result with a bar chart
ggplot(average_transparency, aes(x = reorder(Journal, AvgTransparency), y = AvgTransparency)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Transparency Score by Journal",
       x = "Journal",
       y = "Average Transparency Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
