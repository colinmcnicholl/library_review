library(kableExtra)
# library(gt)
library(dplyr)

# Create a data frame with reported vs computed values
discrepancies <- tibble(
    Metric = c("In-person issues 2018-19", "Event attendance 2018-19",
               "Computer hours 2019-20", "Event attendance 2019-20",
               "Event attendance 2022-23", "Physical visits 2023-24",
               "Event attendance 2023-24"),
    Reported_Value = c(213924, 15608, 16624, 19454, 16619, 146019, 18521),
    Computed_Value = c(211219, 14036, 16625, 15922, 7930, 148979, 8340),
    Discrepancy = Reported_Value - Computed_Value
)

# Create a styled table
# discrepancies %>%
#     gt() %>%
#     tab_header(
#         title = "Discrepancies Between Table 1 and Table 2",
#         subtitle = "Errors in reported totals vs computed branch-level sums"
#     ) %>%
#     fmt_number(columns = 2:4, decimals = 0) %>%
#     data_color(
#         columns = Discrepancy,
#         colors = scales::col_bin(c("lightblue", "lightpink"), domain = discrepancies$Discrepancy, bins = 3)
#     ) %>%
#     tab_source_note("Computed values are the sum of branch-level data from Table 2.")


kable(discrepancies, format = "html", caption = "Reported vs Computed Totals") %>%
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
