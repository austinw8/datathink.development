#Case Study 5: Human Height over Time

library(tidyverse)
library(tidyr)
library(haven)
library(readr)
library(readxl)
library(foreign)


# Data Import -------------------------------------------------------------

worldwide_height_estimates <- read_excel("../Data Sets/case_study_5_heights/Height.xlsx")
ger_conscripts_19th_century <- read_dta("../Data Sets/case_study_5_heights/germanconscr.dta")
bav_conscripts_19th_century <- read_dta("../Data Sets/case_study_5_heights/germanprison.dta")
ger_soldiers_18th_century <- read.dbf("../Data Sets/case_study_5_heights/B6090.DBF")
BLS_wage_height <- read_csv("../Data Sets/case_study_5_heights/heights.csv")
heights1 <- read_rds("../Data Sets/case_study_5_heights/heights.RDS")
heights2 <- read_sav("../Data Sets/case_study_5_heights/unionhist.sav")


# Data Tidying ------------------------------------------------------------

new_col_names <- 
  as.character(unlist(worldwide_height_estimates[2, ]))

colnames(worldwide_height_estimates) <- new_col_names

tidy_worldwide_height_estimates <- 
  worldwide_height_estimates |> 
  slice(-c(1, 2)) |> 
  pivot_longer(cols = c(`1800`:`2011`), 
               names_to = "year", 
               values_to = "height",
               values_drop_na = TRUE
  ) |> 
  rename(country = `Continent, Region, Country`, 
         height_cm = height) |> 
  mutate(source = "Tubingen Worldwide Height Data") |> 
  mutate(height_in = (height_cm / 2.54)) |> 
  relocate(height_in, .after = height_cm) |> 
  select(-1)

#--------

tidy_ger_conscripts_19th_century <- 
  ger_conscripts_19th_century |> 
  select(-1, -5) |> 
  rename(year = bdec, height_cm = height) |> 
  mutate(
    country = "Germany",
    height_in = (height_cm / 2.54),
    source = "German male conscripts in Bavaria, 19th century") |> 
  relocate(country, year, height_cm, height_in, source) |> 
  select(-6)

#--------------------

tidy_bav_conscripts_19th_century <- 
  bav_conscripts_19th_century |> 
  rename(year = bdec, height_cm = height) |> 
  mutate(
    country = "Germany",
    height_in = (height_cm / 2.54),
    source = "Bavarian conscripts, 19th century"
  ) |> 
  select(-1, -3) |> 
  relocate(country, year, height_cm, height_in, source)

#----------------------

tidy_ger_soldiers_18th_century <- 
  ger_soldiers_18th_century |> 
  select(2, 15) |> 
  rename(year = SJ, height_cm = CMETER) |> 
  mutate(
    country = "Germany", 
    height_in = (height_cm / 2.54),
    source = "German Soldiers 18th Century"
  ) |> 
  relocate(country)

#------------------

tidy_BLS_wage_height <- 
  BLS_wage_height |> 
  select(2) |> 
  rename(height_in = height) |> 
  mutate(
    country = "United States of America",
    year = "1950",
    height_cm = (height_in * 2.54),
    source = "BLS Wage and Height Data"
  ) |> 
  relocate(country, year, height_cm, height_in, source)

#------------------

tidy_heighs1 <- 
  heights1 |> 
  select(3) |> 
  rename(height_in = height) |> 
  mutate(
    country = "United States of America",
    year = "1950",
    height_cm = (height_in * 2.54),
    source = "BLS and NLS cross-section"
  ) |> 
  relocate(country, year, height_cm, height_in, source)

#----------------

all_height_data <- 
  rbind(
    tidy_bav_conscripts_19th_century,
    tidy_BLS_wage_height,
    tidy_ger_conscripts_19th_century,
    tidy_ger_soldiers_18th_century,
    tidy_heighs1,
    tidy_worldwide_height_estimates
  ) |> 
  mutate(year = as.numeric(year)) |> 
  mutate(
    century = (year %/% 100) + 1,
    decade = (year %/% 10) * 10
  ) |> 
  relocate(century, decade, .after = year) |> 
  na.omit()

# Plotting Heights Over Time ----------------------------------------------

plotting_data <- 
  all_height_data |> 
  group_by(decade, century) |> 
  summarize(mean_height_in = mean(height_in, na.rm = TRUE))

ggplot(plotting_data, aes(x = decade, y = mean_height_in)) +
  geom_line(linewidth = 1.25) +
  scale_x_continuous(breaks = seq(1760, 1980, by = 10)) +
  theme(legend.position = "none") +
  labs(
    title = "Average heights worldwide decreased during the industrial revolution",
  #  subtitle = "Average heights worldwide decreased during the 1800s", 
    x = "",
    y = "Average Height (inches)"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(64,71))

germany_heights <- 
  all_height_data |> 
  filter(country == "Germany") |> 
  group_by(decade) |> 
  summarize(mean_height_in = mean(height_in, na.rm = TRUE))

ggplot(plotting_data, aes(x = decade, y = mean_height_in)) +
  geom_line(linewidth = 1.25) +
  scale_x_continuous(breaks = seq(1760, 1980, by = 10)) +
  theme(legend.position = "none") +
  geom_line(data = germany_heights, mapping = aes(x = decade, y = mean_height_in, color = "red"), linewidth = 1.25) +
  labs(
    title = "Germany follows world trend in height decrease, then surpasses pre industrial revolution heights",
    subtitle = "Modern day German height now exceeds that of the world average",
    x = "",
    y = "Average Height (inches)"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(64,71)) +
  annotate("text", x = 1978, y = 70.25, label = "Germany", size = 5) +
  annotate("text", x = 1980, y = 66.75, label = "World", size = 5)




