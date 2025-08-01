# Load necessary libraries
library(dplyr)
library(ggplot2)

# Set the working directory
setwd("C:/Users/PC/Desktop/projects/R")

# Import the data from CSV
data <- read.csv("./data.csv")

# Calculate code sharing rates by journal
code_sharing_rates <- data %>%
  group_by(Journal, `Is.the.code.shared.`) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  filter(!is.na(`Is.the.code.shared.`))

# Print the result
print(code_sharing_rates)

# Visualize the result with a stacked bar chart
ggplot(code_sharing_rates, aes(x = Journal, y = Proportion, fill = `Is.the.code.shared.`)) +
  geom_bar(stat = "identity") +
  labs(title = "Code Sharing Rates by Journal",
       subtitle = "Proportion of Code Shared vs. Not Shared",
       x = "Journal",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 3)