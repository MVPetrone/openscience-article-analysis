library(dplyr)
library(ggplot2)

data <- read.csv("FINALDATA2.csv")

# Calculate the percentage
percentage_data_accessible <- data %>%
  # Filter for articles that explicitly have data in the paper
  filter(Is.there.data.explicitly.in.the.paper. == "Yes") %>%
  # Now, from this subset, count those that also share data
  mutate(
    data_shared_and_accessible = ifelse(
      Is.there.data.shared. %in% c("Partially", "Fully"),
      1,
      0
    )
  ) %>%
  # Summarize to get the counts and calculate percentage
  summarise(
    total_articles_with_data = n(),
    articles_with_accessible_data = sum(data_shared_and_accessible, na.rm = TRUE)
  ) %>%
  # Calculate the percentage
  mutate(
    percentage = (
      articles_with_accessible_data / total_articles_with_data
    ) * 100
  )

# Print the result
print(percentage_data_accessible)

# --- Data Preparation for Bar Chart (CRITICAL STEP - DO NOT SKIP THIS) ---
# Filter for articles that explicitly have data in the paper
# Then categorize them based on whether the data is shared (accessible) or not.
plot_data <- data %>%
  filter(Is.there.data.explicitly.in.the.paper. == "Yes") %>%
  mutate(
    Data_Accessibility_Category = case_when(
      # Articles with explicit data that are shared (Partially or Fully)
      Is.there.data.shared. %in% c("Partially", "Fully") ~ "Data Shared (Accessible)",
      # Articles with explicit data that are explicitly NOT shared
      Is.there.data.shared. == "No" ~ "Data Not Shared",
      # Articles with explicit data but sharing status is unknown/NA
      is.na(Is.there.data.shared.) ~ "Data Sharing Status Unknown",
      # Catch any other unexpected values
      TRUE ~ "Other/Unexpected Status"
    )
  ) %>%
  # Group by the new category and count the number of articles in each
  group_by(Data_Accessibility_Category) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  # Calculate percentages for potential labels
  mutate(
    Total_Articles = sum(Count),
    Percentage = (Count / Total_Articles) * 100,
    # Create a formatted label string for text display
    Label = paste0(Count, " (", round(Percentage, 1), "%)")
  ) %>%
  # Optional: Order categories for a more intuitive plot
  arrange(
    factor(
      Data_Accessibility_Category,
      levels = c(
        "Data Shared (Accessible)",
        "Data Not Shared",
        "Data Sharing Status Unknown",
        "Other/Unexpected Status"
      )
    )
  )

# --- Prepare data for barplot() ---
# barplot() typically takes a vector of heights and optionally names
# IMPORTANT: Use 'plot_data' here, not 'data'
bar_heights <- plot_data$Count
names(bar_heights) <- plot_data$Data_Accessibility_Category

# Define custom colors for the bars
bar_colors <- c(
  "Data Shared (Accessible)" = "#4CAF50", # Green
  "Data Not Shared" = "#FFC107", # Amber
  "Data Sharing Status Unknown" = "#9E9E9E", # Grey
  "Other/Unexpected Status" = "#F44336" # Red
)

# Match colors to the order of categories in bar_heights
# This ensures the correct color is applied to the correct bar
ordered_colors <- bar_colors[names(bar_heights)]

# --- Create the Bar Chart using base R's barplot() ---
# Plot the bars and capture the midpoint locations of the bars for text labels
bar_midpoints <- barplot(
  bar_heights,
  main = "Data Accessibility Status of Articles with Explicit Data",
  xlab = "Data Accessibility Category",
  ylab = "Number of Articles",
  col = ordered_colors, # Apply the custom colors
  ylim = c(0, max(bar_heights) * 1.2), # Set y-axis limit to leave space for labels
  cex.names = 0.8, # Reduce font size of x-axis labels if needed
  las = 2 # Rotate x-axis labels vertically for better readability
)

# Add text labels on top of the bars
text(
  x = bar_midpoints,
  y = bar_heights,
  labels = plot_data$Label, # This is correct, it uses plot_data
  pos = 3, # Position the label just above the bar
  cex = 0.8 # Font size of the labels
)

# Add a subtitle using title() for base R plots
title(sub = "Distribution among articles that explicitly mention data in the paper")