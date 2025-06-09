library(tidyverse)

# Load data
library_service_usage <- read_csv("~/tmp/daily_practice/library_service_usage.csv")

# Process data
dat <- library_service_usage %>%
    mutate(year2 = ymd(str_c(str_sub(year, 1, 4), "0101"))) %>%
    filter(category == "visits") %>%
    group_by(year2) %>%
    summarise(total_visits = sum(count, na.rm = TRUE))

# Define groups
dat <- dat %>%
    mutate(group = case_when(
        year2 %in% ymd(c("2018-01-01", "2019-01-01")) ~ "Early",
        year2 %in% ymd(c("2022-01-01", "2023-01-01")) ~ "Late",
        TRUE ~ NA_character_
    ))

# Perform t-test
t_test_result <- t.test(total_visits ~ group, data = dat, var.equal = TRUE)
print(t_test_result)

ggplot(dat, aes(x = total_visits)) +
    geom_histogram(bins = 10, fill = "blue", alpha = 0.6) +
    facet_wrap(~ group) +
    labs(title = "Distribution of Visit Counts", x = "Total Visits", y = "Frequency") +
    theme_minimal()

ggplot(dat, aes(sample = total_visits)) +
    stat_qq() + stat_qq_line() +
    facet_wrap(~ group) +
    labs(title = "Q-Q Plot of Visit Counts") +
    theme_minimal()
