library(readr)
library(dplyr)

data <- read.csv("data.csv")

# Percentage of articles with a data sharing statement ==================
total_articles <- nrow(data)
articles_with_statement <- data %>%
  filter(Is.there.a.data.sharing.statement. == "Yes") %>%
  nrow()
percentage_with_statement <- (articles_with_statement / total_articles) * 100
cat("Percentage of articles with a data sharing statement:",
    round(percentage_with_statement, 2), "%\n")

# Percentage of articles that share data and also don't have a data sharing statement ======
filtered_articles <- subset(data, (`Is.there.data.shared.` == "Fully" | `Is.there.data.shared.` == "Partially") & `Is.there.a.data.sharing.statement.` == "No")
count_filtered <- nrow(filtered_articles)
total_articles <- nrow(data)
percentage <- (count_filtered / total_articles) * 100
cat(paste0(round(percentage, 2), "% of the articles share data and don't have a data sharing statement."))

# Percentage of Data Accessibility Methods
filtered_data <- data %>%
  filter(Is.there.data.explicitly.in.the.paper. == "Yes" &
           (Is.there.data.shared. == "Partially" |
              Is.there.data.shared. == "Fully"))

total_filtered_articles <- nrow(filtered_data)

if (total_filtered_articles > 0) {
  accessibility_method_percentages <- filtered_data %>%
    group_by(Data.accessibility.method) %>%
    summarise(count = n()) %>%
    mutate(percentage = (count / total_filtered_articles) * 100)
  
  print(accessibility_method_percentages)
} else {
  cat("No articles match the specified criteria for data accessibility methods.\n")
}