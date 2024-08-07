library(tidyverse)
library(readr)
library(haven)
library(readxl)
library(downloader)


# Importing Data ----------------------------------------------------------
dta_url <- "https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.dta"

#rds_dat <- read_rds(url("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.RDS"))
rds_data <- Dart_Expert_Dow_6month_anova

#xlsx_data <- read_xlsx("..Data Sets/Dart_Expert_Dow_6mo/Dart_Expert_Dow_6month_anova.xlsx")
xlsx_data <-  read_xlsx("C:/Users/austi/OneDrive/Desktop/R_practice/Data Sets/Dart_Expert_Dow_6mo/Dart_Expert_Dow_6month_anova.xlsx")

temp_dta <- tempfile(fileext = ".dta")
download.file(dta_url, temp_dta, mode = "wb")
dta_data <- read_dta(temp_dta)

csv_data <- read_csv("C:/Users/austi/OneDrive/Desktop/R_practice/Data Sets/Dart_Expert_Dow_6mo/Dart_Expert_Dow_6month_anova.csv")

sav_url <- "https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.sav"
temp_sav <- tempfile(fileext = ".sav")
download.file(sav_url, temp_sav, mode = "wb")
sav_data <- read_sav(temp_sav)


# Verify Equality ---------------------------------------------------------

comparison_results <- list(
  rds_vs_xlsx = all.equal(rds_data, xlsx_data),
  rds_vs_dta = all.equal(rds_data, dta_data),
  rds_vs_csv = all.equal(rds_data, csv_data),
  rds_vs_sav = all.equal(rds_data, sav_data),
  xlsx_vs_dta = all.equal(xlsx_data, dta_data),
  xlsx_vs_csv = all.equal(xlsx_data, csv_data),
  xlsx_vs_sav = all.equal(xlsx_data, sav_data),
  dta_vs_csv = all.equal(dta_data, csv_data),
  dta_vs_sav = all.equal(dta_data, sav_data),
  csv_vs_sav = all.equal(csv_data, sav_data)
)
comparison_results


# Analysis (compare DART, DIJA, and PRO stocks ----------------------------

csv_data
csv_data <- 
  csv_data |> 
  mutate(variable = factor(variable))
csv_data

ggplot(csv_data, aes(x = contest_period, y = value)) +
  geom_line(mapping = aes())

average_returns <- 
  csv_data |> 
  group_by(variable) |> 
  summarise(mean_value = mean(value))

ggplot(csv_data, aes(x = variable, y = value)) +
  geom_boxplot() + 
  geom_jitter(width = 0.1, aes(color = variable), alpha = 0.5) +
  geom_point(average_returns, mapping = aes(x = variable, y = mean_value), 
             color = "red", size = 4, shape = 19) +
  labs(title = "Stock Performance Comparison",
       x = "Stock",
       y = "Return") +
  theme_minimal() +
  theme(legend.position = "none")
