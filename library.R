library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(lubridate)

library_service_usage <- read_csv("~/tmp/daily_practice/library_service_usage.csv")

library_service_usage <- library_service_usage %>% 
    mutate(year2=ymd(str_c(str_sub(year,1,4), "0101")))

by_year_category <- library_service_usage %>% 
    group_by(year2,category)

usage_by_year_category <- by_year_category %>% 
    summarise(totals = sum(count, na.rm = TRUE))

visits <- filter(library_service_usge, category == 'visits')

lockdown_periods <- data.frame(
    xmin = as.Date(c("2020-03-23", "2020-11-05", "2021-01-05")),
    xmax = as.Date(c("2020-07-04", "2020-12-02", "2021-03-08")),
    ymin = -Inf,
    ymax = Inf
)

p1 <- ggplot(data = visits, aes(x=year2,y=count,color=branch)) +
    geom_point(size=2) +
    scale_x_date(NULL, date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    geom_rect(data = lockdown_periods, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              inherit.aes=FALSE,fill="lightblue", alpha=0.3) +
    annotate("text",x=as.Date(c("2020-05-15","2020-12-10","2021-02-25")),
             y=c(max(visits$count,na.rm = TRUE)*0.95,
                 max(visits$count,na.rm = TRUE)*0.85,
                 max(visits$count,na.rm = TRUE)*0.75),
             label=c("Lockdown1","Lockdown2","Lockdown3"),
             color="blue",size=4,fontface="bold") +
    labs(
        title = paste("Increase in total number of physical visits",
                      "for Waltham"),
        subtitle = paste("Decreases 2018-19 vs 2023-24 for Immingham, Grimsby,",
                         "\nand Cleethorpes of 51%, 49%, and 47% respectively",
                         "\nIncrease of 1% for Waltham"),
        caption = paste("Data from Table 2, Library and Archives Review",
                        "Phase 1 Cabinet Report"),
        y = "Total number of physical visits"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

issues <- filter(library_service_usge, category == 'issues')

p2 <- ggplot(data = issues, aes(x=year2,y=count,color=branch)) +
    geom_point(size=2) +
    scale_x_date(NULL, date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    geom_rect(data = lockdown_periods, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              inherit.aes=FALSE,fill="lightblue", alpha=0.3) +
    annotate("text",x=as.Date(c("2020-05-15","2020-12-10","2021-02-25")),
             y=c(max(visits$count,na.rm = TRUE)*0.95,
                 max(visits$count,na.rm = TRUE)*0.85,
                 max(visits$count,na.rm = TRUE)*0.75),
             label=c("Lockdown1","Lockdown2","Lockdown3"),
             color="blue",size=4,fontface="bold") +
    labs(
        title = paste("Little change in number of in person issues",
                      "for Immingham and Waltham"),
        subtitle = paste("Decreases 2018-19 vs 2023-24 for Grimsby, Cleethorpes,",
                         "\nWaltham, and Immingham, of 27%, 26%, 7% and",
                         "\n3%  respectively."),
        caption = paste("Data from Table 2, Library and Archives Review",
                        "Phase 1 Cabinet Report"),
        y = "Total number of issues"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

computer.hr <- library_service_usage %>% 
    filter(category == "computer-hr")

p3 <- ggplot(data = computer.hr, aes(x=year2,y=count,color=branch)) +
    geom_point(size=2) +
    scale_x_date(NULL, date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    geom_rect(data = lockdown_periods, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              inherit.aes=FALSE,fill="lightblue", alpha=0.3) +
    annotate("text",x=as.Date(c("2020-05-15","2020-12-10","2021-02-25")),
             y=c(max(computer.hr$count,na.rm = TRUE)*0.95,
                 max(computer.hr$count,na.rm = TRUE)*0.85,
                 max(computer.hr$count,na.rm = TRUE)*0.75),
             label=c("Lockdown1","Lockdown2","Lockdown3"),
             color="blue",size=4,fontface="bold") +
    labs(
        title = paste("Total number of computer hours used"),
        subtitle = paste("Decreases 2018-19 vs 2023-24 for Grimsby, Cleethorpes,",
                         "\nImmingham, and Waltham, of 8112 hrs, 3490 hrs, 755 hrs, and 575 hrs.",
                         "\nPercentage decreases of 56%, 73%, 60%, and 78% respectively."),
        caption = paste("Data from Table 2, Library and Archives Review",
                        "Phase 1 Cabinet Report"),
        y = "Total number of computer hours used"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")


event.attendance <- library_service_usage %>% 
    filter(category == "event-attendance")

p4 <- ggplot(data = event.attendance, aes(x=year2,y=count,color=branch)) +
    geom_point(size=2) +
    scale_x_date(NULL, date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    geom_rect(data = lockdown_periods, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              inherit.aes=FALSE,fill="lightblue", alpha=0.3) +
    annotate("text",x=as.Date(c("2020-05-15","2020-12-10","2021-02-25")),
             y=c(max(event.attendance$count,na.rm = TRUE)*0.95,
                 max(event.attendance$count,na.rm = TRUE)*0.85,
                 max(event.attendance$count,na.rm = TRUE)*0.75),
             label=c("Lockdown1","Lockdown2","Lockdown3"),
             color="blue",size=4,fontface="bold") +
    labs(
        title = paste("Total core event and programme attendance"),
        subtitle = paste("Decreases 2018-19 vs 2023-24 for Waltham, Cleethorpes, and Grimsby",
                         "\nof 3805, 1428, and 742.  Increase of 279 for Immingham",
                         "\nPercentage decreases of 65%, 33%, and 30% respectively.",
                         "\nPercentage increase of 22% for Immingham"),
        caption = paste("Data from Table 2, Library and Archives Review",
                        "Phase 1 Cabinet Report"),
        y = "Total core event & programme attendance"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
