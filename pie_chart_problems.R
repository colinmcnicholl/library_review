library(tibble)
library(ggplot2)
library(dplyr)

tb <- tibble(
    category = c("A", "B"),
    count = c(51, 49)  # Very close values
)

tb <- tb %>%
    mutate(prop = count / sum(count))  # Calculate proportions

p1 <- ggplot(tb, aes(x = category, y = count, fill = category)) +
    geom_col() +
    labs(
        title = "Quick: For which category is the count greater. A or B?"
    ) +
    theme_minimal()


p2 <- ggplot(tb, aes(x = "", y = prop, fill = category)) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    labs(
        title = "Quick: For which category is the count greater. A or B?"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_blank())

