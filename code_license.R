# Load necessary libraries
library(dplyr)
library(ggplot2)

# Set the working directory
setwd("C:/Users/PC/Desktop/projects/R")

# Import the data from CSV
data <- read.csv("./data.csv")

# Filter data to include only fully or partially shared code
code_shared <- data %>%
  filter(`Is.the.code.shared.` == "Partially available" | `Is.the.code.shared.` == "Fully available")

# Calculate license rates by journal
license_rates <- code_shared %>%
  group_by(Journal, `Code.license.provided.`) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  filter(!is.na(`Code.license.provided.`))

# Print the result
print(license_rates)

# Visualize the result with a stacked bar chart
ggplot(license_rates, aes(x = Journal, y = Proportion, fill = `Code.license.provided.`)) +
  geom_bar(stat = "identity") +
  labs(title = "Code License Rates by Journal",
       subtitle = "Proportion of Licenses Provided for Shared Code",
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