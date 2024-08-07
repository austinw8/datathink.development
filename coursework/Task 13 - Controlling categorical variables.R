#Task 13 - Controlling categorical variables (factors)

library(tidyverse)
library(forcats)
library(ggplot2)

stocks <- read_csv("../Data Sets/Dart_Expert_Dow_6mo/Dart_Expert_Dow_6month_anova.csv")

stocks_tidy <- 
  stocks |> 
  separate(contest_period, c("start_period", "end_period"), sep = "-") |> 
  separate(end_period, c("month_end", "year_end"), sep = -4) |> 
  mutate(has_year = grepl("\\d{4}$", start_period)) |> 
  mutate(month_year = if_else(has_year, start_period, NA_character_)) |> 
  separate(month_year, into = c("month_start", "year_start"), sep = "(?<=\\D)(?=\\d{4})", fill = "right") |> 
  mutate(month = if_else(is.na(month_start), start_period, month_start)) |> 
  select(-has_year, -start_period, -month_start) |>
  rename(month_start = month) |> 
  relocate(month_start, year_start) |> 
  fill(year_start, .direction = "up")

#show the six-month returns by the month in which the returns are collected using the eight years of data

#convert end_month into factor with correct order
stocks_tidy_ready <- stocks_tidy |> 
  mutate(month_end = str_replace_all(month_end, "^Dec\\.$", "December"),
         month_end = str_replace_all(month_end, "Febuary", "February"),
         month_end = fct(month_end, levels = c(
           "January", "February", "March", "April", "May", "June", 
           "July", "August", "September", "October", "November", "December"
         ))
         ) |> 
#combine month and year
  mutate(date = make_date(year = year_end, month = as.numeric(month_end), day = 1)) |> 
  arrange(date)

#create line plot
ggplot(stocks_tidy_ready, aes(x = date, y = value, color = variable)) +
  geom_line(linewidth = 0.75) +
  labs(
    title = "Stock Returns Over 8 Years",
    x = "Date",
    y = "Stock Return Value", 
    color = "Stock"
  ) +
  theme_bw()





