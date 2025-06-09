library(tidyverse)

library_service_usage <- read_csv("~/tmp/daily_practice/library_service_usage.csv")
library_service_usage <- library_service_usage %>% 
    mutate(year2=ymd(str_c(str_sub(year,1,4), "0101")))

# Compute issues per visit
issues_per_visit <- library_service_usage %>%
    filter(category %in% c("issues", "visits")) %>%
    pivot_wider(names_from = category, values_from = count) %>%
    mutate(issues_per_visit = issues / visits)

# Plot trends by branch
ggplot(issues_per_visit, aes(x = year2, y = issues_per_visit, color = branch)) +
    geom_line() +  # Line graph to show trends
    geom_point(size = 2) +  # Highlight actual data points
    scale_x_date(NULL, date_breaks = "1 year", date_labels = "%Y") +
    labs(
        title = "Issues Per Visit Across Library Branches",
        subtitle = "Examining borrowing behavior changes pre and post-COVID",
        x = "Year",
        y = "Issues per Visit"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
