library(ggplot2)

discrepancies <- tibble(
    Metric = c("In-person issues 2018-19", "Event attendance 2018-19",
               "Computer hours 2019-20", "Event attendance 2019-20",
               "Event attendance 2022-23", "Physical visits 2023-24",
               "Event attendance 2023-24"),
    Reported_Value = c(213924, 15608, 16624, 19454, 16619, 146019, 18521),
    Computed_Value = c(211219, 14036, 16625, 15922, 7930, 148979, 8340),
    Discrepancy = Reported_Value - Computed_Value
)

ggplot(discrepancies, aes(x = Metric, y = Computed_Value, fill = "Computed")) +
    geom_col() +
    geom_col(aes(y = Reported_Value, fill = "Reported"), alpha = 0.6) +
    coord_flip() +  # Horizontal bars for readability
    scale_fill_manual(values = c("Reported" = "red", "Computed" = "blue")) +
    labs(title = "Reported vs Computed Values", x = "Metric", y = "Count") +
    theme_minimal()
