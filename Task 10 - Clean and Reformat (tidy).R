library(tidyverse)
library(tidyr)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)

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

#Plots---------------------------------------------------

ggplot(stocks_tidy, aes(x = year_end, y = value)) +
  geom_boxplot(color = "forestgreen") +
  labs(
    title = "The 90's showed an overall trend of positive returns from the stock market",
    subtitle = "Notible exceptions being 1990. Additionally, 1992-1994 showed lesser returns, but still positive",
    x = "Year",
    y = "6-month return"
  ) +
  theme_bw()

ggplot(stocks_tidy, aes(x = year_end, y = value)) +
  geom_boxplot(mapping = aes(fill = variable)) +
  labs(
    title = "In the 90s, DJIA shows the safest returns, while PROS had the highest risk/reward difference",
    x = "Year",
    y = "6-month return"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~variable)

#Recreate Final Table in Task 10 ------------------------------------

month_order <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

final_table <- 
  stocks_tidy |> 
  filter(variable == "DJIA") |> 
  select(-month_start, -year_start, -variable) |> 
  mutate(month_end = str_replace_all(month_end, "^Dec\\.$", "December")) |> 
  mutate(month_end = str_replace_all(month_end, "Febuary", "February")) |> 
  pivot_wider(names_from = year_end, values_from = value) |> 
  mutate(month_end = factor(month_end)) |> 
  arrange(factor(month_end, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) |> 
  rename(Month = month_end)