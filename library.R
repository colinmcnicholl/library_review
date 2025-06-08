library(tidyverse)
library(lubridate)

library_service_usage <- read_csv("~/tmp/daily_practice/library_service_usage.csv")
View(library_service_usage)

dat <- mutate(library_service_usage,
              year2=ymd(str_c(str_sub(year,1,4), "0101")))

View(dat)

by_year_category <- group_by(dat,year2,category)
usage_by_year_category <- summarise(by_year_category, totals = sum(count, na.rm = TRUE))
View(usage_by_year_category)

visits_dat <- filter(dat, category == 'visits')

lockdown_periods <- data.frame(
    xmin = as.Date(c("2020-03-23", "2020-11-05", "2021-01-05")),
    xmax = as.Date(c("2020-07-04", "2020-12-02", "2021-03-08")),
    ymin = -Inf,
    ymax = Inf
)

p1 <- ggplot(data = visits_dat, aes(x=year2,y=count,color=branch)) +
    geom_point(size=2) +
    scale_x_date(NULL, date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    geom_rect(data = lockdown_periods, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              inherit.aes=FALSE,fill="lightblue", alpha=0.3) +
    annotate("text",x=as.Date(c("2020-05-15","2020-12-10","2021-02-25")),
             y=c(max(visits_dat$count,na.rm = TRUE)*0.95,
                 max(visits_dat$count,na.rm = TRUE)*0.85,
                 max(visits_dat$count,na.rm = TRUE)*0.75),
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
    theme(legend.position = "bottom")

issues_dat <- filter(dat, category == 'issues')

p2 <- ggplot(data = issues_dat, aes(x=year2,y=count,color=branch)) +
    geom_point(size=2) +
    scale_x_date(NULL, date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    geom_rect(data = lockdown_periods, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              inherit.aes=FALSE,fill="lightblue", alpha=0.3) +
    annotate("text",x=as.Date(c("2020-05-15","2020-12-10","2021-02-25")),
             y=c(max(visits_dat$count,na.rm = TRUE)*0.95,
                 max(visits_dat$count,na.rm = TRUE)*0.85,
                 max(visits_dat$count,na.rm = TRUE)*0.75),
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
    theme(legend.position = "bottom")

