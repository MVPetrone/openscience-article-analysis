# Load necessary libraries
library(dplyr)
library(ggplot2)
library(scales)
library(knitr)

# Set the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import the data from CSV
data <- read.csv("./data.csv")

# Calculate data sharing rates
# Create a combined sharing status based on multiple columns
data_analysis <- data %>%
  mutate(
    # Create sharing category based on available columns
    sharing_category = case_when(
      Is.there.data.shared. == "Fully available" & 
        !is.na(Data.accessibility.method) & 
        Data.accessibility.method != "" ~ "within",
      Is.there.a.data.sharing.statement. == "Yes" & 
        (Is.there.data.shared. == "No" | is.na(Is.there.data.shared.)) ~ "statement_only",
      Is.there.data.shared. == "No" ~ "not_shared",
      TRUE ~ "unclear"
    )
  )

# Calculate summary statistics
data_sharing_summary <- data_analysis %>%
  group_by(sharing_category) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    percentage = (count / sum(count)) * 100,
    rate = count / sum(count)
  )

# Calculate specific rates
fully_shared_rate <- data_sharing_summary %>%
  filter(sharing_category == "fully_shared") %>%
  pull(rate)

statement_only_rate <- data_sharing_summary %>%
  filter(sharing_category == "statement_only") %>%
  pull(rate)

not_shared_rate <- data_sharing_summary %>%
  filter(sharing_category == "not_shared") %>%
  pull(rate)

# Additional analysis by data accessibility method
accessibility_summary <- data_analysis %>%
  filter(Is.there.data.shared. == "Yes") %>%
  group_by(Data.accessibility.method) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    percentage = (count / sum(count)) * 100
  ) %>%
  arrange(desc(count))

# Analysis by journal
journal_summary <- data_analysis %>%
  group_by(Journal, sharing_category) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  group_by(Journal) %>%
  mutate(
    total_articles = sum(count),
    percentage = (count / total_articles) * 100
  ) %>%
  filter(sharing_category == "fully_shared") %>%
  arrange(desc(percentage))

# Print the results
cat("Data Sharing Analysis Results:\n")
cat("=============================\n\n")

cat("Overall Summary Statistics:\n")
print(data_sharing_summary)

cat("\nKey Rates:\n")
cat(sprintf("Fully Shared Rate: %.2f%% (%.3f)\n", 
            fully_shared_rate * 100, fully_shared_rate))
cat(sprintf("Statement Only Rate: %.2f%% (%.3f)\n", 
            statement_only_rate * 100, statement_only_rate))
cat(sprintf("Not Shared Rate: %.2f%% (%.3f)\n", 
            not_shared_rate * 100, not_shared_rate))

# Calculate ratio of fully shared vs statement only
if (statement_only_rate > 0) {
  sharing_ratio <- fully_shared_rate / statement_only_rate
  cat(sprintf("\nRatio of Fully Shared to Statement Only: %.2f:1\n", sharing_ratio))
}

cat("\nData Accessibility Methods (for shared data):\n")
print(accessibility_summary)

cat("\nTop 10 Journals by Full Sharing Rate:\n")
print(head(journal_summary, 10))

# Visualize the results
# Main sharing categories bar chart
plot1 <- ggplot(data_sharing_summary, aes(x = sharing_category, y = percentage)) +
  geom_bar(stat = "identity", fill = c("#E74C3C", "#F39C12", "#95A5A6", "#27AE60")) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, size = 4) +
  labs(
    title = "Distribution of Data Sharing Practices",
    x = "Data Sharing Category",
    y = "Percentage (%)",
    caption = "Analysis of author data sharing behavior"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_discrete(labels = c("Fully Shared", "Not Shared", "Statement Only", "Unclear")) +
  scale_y_continuous(limits = c(0, max(data_sharing_summary$percentage) * 1.1))

print(plot1)

# Data accessibility methods visualization
if (nrow(accessibility_summary) > 0) {
  plot2 <- ggplot(accessibility_summary, aes(x = reorder(Data.accessibility.method, count), 
                                             y = percentage)) +
    geom_bar(stat = "identity", fill = "#3498DB") +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), 
              hjust = -0.1, size = 3) +
    labs(
      title = "Data Accessibility Methods",
      x = "Accessibility Method",
      y = "Percentage of Shared Data (%)",
      caption = "Methods used by authors who shared data"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    ) +
    coord_flip()
  
  print(plot2)
}

# Open source format analysis
open_source_summary <- data_analysis %>%
  filter(Is.there.data.shared. == "Yes") %>%
  group_by(Is.the.data.in.open.source.format....enter.type.) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    percentage = (count / sum(count)) * 100
  )

cat("\nOpen Source Format Analysis (for shared data):\n")
print(open_source_summary)

# Time trend analysis if Publication.Date is available
if ("Publication.Date" %in% colnames(data)) {
  # Convert Publication.Date to Date format (adjust format as needed)
  data_analysis$Publication.Date <- as.Date(data_analysis$Publication.Date, format = "%Y-%m-%d")
  
  time_trend <- data_analysis %>%
    filter(!is.na(Publication.Date)) %>%
    mutate(
      year = format(Publication.Date, "%Y")
    ) %>%
    group_by(year, sharing_category) %>%
    summarise(
      count = n(),
      .groups = 'drop'
    ) %>%
    group_by(year) %>%
    mutate(
      total_year = sum(count),
      percentage = (count / total_year) * 100
    ) %>%
    filter(sharing_category == "fully_shared")
  
  plot3 <- ggplot(time_trend, aes(x = year, y = percentage, group = 1)) +
    geom_line(color = "#27AE60", size = 1.2) +
    geom_point(color = "#27AE60", size = 3) +
    labs(
      title = "Trend of Full Data Sharing Over Time",
      x = "Publication Year",
      y = "Percentage of Articles with Full Data Sharing (%)",
      caption = "Temporal analysis of data sharing practices"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(plot3)
}

# Summary table for reporting
summary_table <- data_sharing_summary %>%
  select(sharing_category, count, percentage) %>%
  mutate(
    percentage = paste0(round(percentage, 1), "%"),
    sharing_category = case_when(
      sharing_category == "fully_shared" ~ "Fully Shared",
      sharing_category == "statement_only" ~ "Statement Only",
      sharing_category == "not_shared" ~ "Not Shared",
      sharing_category == "unclear" ~ "Unclear",
      TRUE ~ sharing_category
    )
  )

cat("\nFinal Summary Table:\n")
kable(summary_table, 
      col.names = c("Data Sharing Status", "Count", "Percentage"),
      align = c("l", "c", "c"))

# Additional insights
cat("\nAdditional Insights:\n")
cat("===================\n")

# Data citation analysis
citation_summary <- data_analysis %>%
  filter(Is.there.data.shared. == "Yes") %>%
  group_by(Is.the.data.citable.) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = (count / sum(count)) * 100)

cat("\nData Citation Status (for shared data):\n")
print(citation_summary)

# License analysis
license_summary <- data_analysis %>%
  filter(Is.there.data.shared. == "Yes") %>%
  group_by(Data.license) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(desc(count))

cat("\nData License Distribution (for shared data):\n")
print(license_summary)