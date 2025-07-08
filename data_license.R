# Load necessary libraries
library(dplyr)
library(ggplot2)

# Set the working directory
setwd("C:/Users/PC/Desktop/projects/R")

# Import the data from CSV
data <- read.csv("./data.csv")

# Filter data to include only fully or partially shared data
data_shared <- data %>%
  filter(`Is.there.data.shared.` == "Fully available" | `Is.there.data.shared.` == "Partially available")

# Calculate license rates by journal
license_rates <- data_shared %>%
  group_by(Journal, `Data.license`) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  filter(!is.na(`Data.license`))

# Print the result
print(license_rates)

# Visualize the result with a stacked bar chart
ggplot(license_rates, aes(x = Journal, y = Proportion, fill = `Data.license`)) +
  geom_bar(stat = "identity") +
  labs(title = "Data License Rates by Journal",
       subtitle = "Proportion of Licenses Provided for Shared Data",
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