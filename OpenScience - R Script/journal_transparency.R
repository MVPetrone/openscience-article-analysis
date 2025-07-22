# Load necessary libraries
library(dplyr)
library(ggplot2)

# Import the data from CSV
data <- read.csv("./FINALDATA2.csv")

# Calculate average transparency score by journal
average_transparency <- data %>%
  # First, convert the 'Transparency' column from text to numbers
  mutate(
    Transparency_Numeric = case_when(
      Transparency == "Excellent" ~ 4,
      Transparency == "Good" ~ 3,
      Transparency == "Basic" ~ 2,
      Transparency == "Bad" ~ 1,
      Transparency == "N/A" ~ NA_real_, # Convert "N/A" to R's NA (Not Applicable)
      TRUE ~ NA_real_ # Catch any other unexpected values as NA
    )
  ) %>%
  # Now, group by Journal and calculate the mean of the new numeric column
  group_by(Journal) %>%
  summarise(AvgTransparency = round(mean(Transparency_Numeric, na.rm = TRUE), 2))

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
