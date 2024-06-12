library(tidyverse)
library(ggplot2)
library(readr)

dat <- read_csv("https://byuistats.github.io/M335/data/rcw.csv", 
                col_types = cols(Semester_Date = col_date(format = "%m/%d/%y"), Semester = col_factor(levels = c("Winter", "Spring", "Fall"))))

ggplot(dat, aes(x = Semester_Date, y = Count, color = Department)) +
  geom_line(linewidth = 0.75) +
  scale_x_date(date_label = "%b %Y", 
               breaks = as.Date(c("2016-04-01", "2016-09-01", "2017-01-01", 
                                  "2017-04-01", "2017-09-01", "2018-01-01"))) + 
  labs(
    title = "RC&W Attendance Trends by Department",
    x = "Semester Dates",
    y = "Number of Attendees"
  ) +
  theme_bw()