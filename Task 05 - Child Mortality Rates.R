library(tidyverse)
library(ggplot2)
library(ggrepel)

#imoprt data
glimpse(dat)

#filter by region
dat_prepped <- 
  dat |> 
  group_by(Entity, Year) |> 
  filter(Entity %in% c("Europe", "Oceania", "South America", "North America", "Africa", "Asia")) |> 
  mutate()
View(dat_prepped)

#graphing labels
country_labels <- 
  data.frame(label = c("Europe", "Oceania", "South America", "North America", "Africa", "Asia"),
              x = c(2027, 2027.5, 2030, 2030, 2027, 2026),
              y = c(0.9, 0.84, 0.78, 0.7, 0.57, 0.465))

#graphing plot
ggplot(dat_prepped, aes(x = Year, y = Civil.liberties.index..best.estimate..aggregate..average.)) +
  geom_line(aes(
    color = Entity),
    size = 1.25) +
  geom_label(country_labels, mapping = aes(x = x, y = y, label = label)) +
  coord_cartesian(xlim = c(1925, 2033)) +
  scale_x_continuous(breaks = seq(1920, 2025, by = 20)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_color_manual(values = c("#D73C50", "#388917", "#8C4569", "#B16214", "#00875E", "#516E9F")) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    title = "Human Rights Index",
    subtitle = "Based on the expert estimates and index by V-Dem. It captures the extent to which people are free from \ngovernment torture, political killings, and forced labor; and enjoy freedoms of movement, religion, expression, and \nassociation. The variable ranges from 0 to 1 (most rights).",
    caption = "OurWorldInData.org/human-rights | CC BY",
    y = NULL
  )


# Child Mortality Data ----------------------------------------------------

devtools::install_github("drsimonj/ourworldindata")
library(devtools)
library(ourworldindata)
library(dplyr)
library(RColorBrewer)
view(child_mortality)
?child_mortality
display.brewer.all()


child_mortality_summary <- 
  child_mortality |> 
  group_by(country) |> 
  summarize(
    avg_child_mortality = mean(child_mort, na.rm = TRUE), 
    avg_population = mean(population, na.rm = TRUE)) |> 
  na.omit(child_mortality)
view(child_mortality_summary)

most_child_mort_by_country <- 
  child_mortality_summary |> 
  slice_max(avg_child_mortality, n = 10)
view(most_child_mort_by_country)

least_child_mort_by_country <- 
  child_mortality_summary |> 
  slice_min(avg_child_mortality, n = 10)
view(least_child_mort_by_country)

ggplot(most_child_mort_by_country, aes(x = reorder(country, -avg_child_mortality), y = avg_child_mortality)) + 
  geom_col(aes(fill = country)) +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none") +
  labs(
    title = "Countries with the Highest Average Child Mortality Rates",
    subtitle = "Since 1800",
    x = "Country",
    y = "Average Child Mortality Rate"
  )

ggplot(least_child_mort_by_country, aes(x = reorder(country, -avg_child_mortality), y = avg_child_mortality)) + 
  geom_col(aes(fill = country)) +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none") +
  labs(
    title = "Countries with the Lowest Average Child Mortality Rates",
    subtitle = "Since 1800",
    x = "Country",
    y = "Average Child Mortality Rate"
  )

#line graph
child_mortality_summary2 <- 
  child_mortality |> 
  group_by(continent, year) |> 
  summarise(avg_mort_per_continent = mean(child_mort, na.rm = TRUE)) |> 
  na.omit(child_mortality)
view(child_mortality_summary2)

ggplot(child_mortality_summary2, aes(x = year, y = avg_mort_per_continent)) +
  geom_line(aes(color = continent), linewidth = 1.25) +
  coord_cartesian(xlim = c(1800, 2013)) + 
  theme_bw() +
  labs(
    title = "Child Mortality Rates Have Been Declining Since 1800",
    subtitle = "faster decline rates starting in the 1900s",
    x = "Year",
    y = "Average Child Mortality Rate (per 1,000 born)"
  )
