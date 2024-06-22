library(tidyverse)
library(ggplot2)
library(dplyr)
library(zoo)
library(gapminder)
library(patchwork)

gun_deaths <- read_csv("../Data Sets/full_data.csv")
view(gun_deaths)


# FiveThirtyEight data ----------------------------------------------------

#calculate date column
#compute counts of intent by month
gun_deaths_count <- 
  gun_deaths |> 
  mutate(date = make_date(year, month), .after = 3) |> 
  group_by(date, intent) |> 
  summarise(count = table(intent))
view(gun_deaths_count)

#plot intent area over time
ggplot(gun_deaths_count, aes(x = date, fill = intent)) +
  geom_area(alpha = 0.6, mapping = aes(y = count), position = "fill") +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(
    title = "Most gun deaths in the US are suicides (62.7%) \nand homicides (34.9%)",
    x = "Date",
    y = "Percent of gun deaths", 
    fill = "Intent"
  )

percentages <- 
  gun_deaths |> 
  group_by(intent) |> 
  summarise(count = table(intent)) |> 
  mutate(percentage = (count / sum(percentages$count)) * 100)
view(percentages)


# Visualizing Gun Death Stats ---------------------------------------------

gun_deaths_seasons <-  
  gun_deaths |> 
  mutate(season = case_when(
    month == "03" ~ "Spring",
    month == "04" ~ "Spring",
    month == "05" ~ "Spring",
    month == "06" ~ "Summer",
    month == "07" ~ "Summer",
    month == "08" ~ "Summer",
    month == "09" ~ "Fall",
    month == "10" ~ "Fall",
    month == "11" ~ "Fall",
    month == "12" ~ "Winter",
    month == "01" ~ "Winter",
    month == "02" ~ "Winter",
  )) |> 
  na.omit()

season_order <- c("Spring", "Summer", "Fall", "Winter")
gun_deaths_seasons$season <- factor(gun_deaths_seasons$season, levels = season_order)
color_values <- c("#97C63A", "#F5B202", "#BB623E", "#5F99B5")
alpha_values <- c(0.35, 1, 0.35, 0.35)

ggplot(gun_deaths_seasons, aes(x = factor(season, levels = season_order), fill = season)) + 
  geom_bar() +
  scale_fill_manual(values = alpha(color_values, alpha_values)) +
  labs(
    title = "More gun deaths happen in the summer than any other season",
    subtitle = "Gun deaths between 2012 - 2015",
    x = "Season",
    y = "Number of Deaths", 
    fill = "Season"
  )

#ggplot(gun_deaths_seasons |> 
 # mutate(season = fct_reorder(season, reorder_seasons)), aes(x = season, fill = season)) + 

ggplot(gun_deaths_seasons, aes(x = factor(season, levels = season_order), fill = season)) + 
  geom_bar() +
  scale_fill_manual(values = alpha(color_values, alpha_values)) +
  labs(
    title = "Among the most common gun death intents (homicides and suicides), \nsummer remains among the most lethal season",
    subtitle = "Gun deaths between 2012 - 2015",
    x = "Season",
    y = "Number of Deaths", 
    fill = "Season"
  ) +
  facet_wrap(~intent, scales = "free")

#------------------

gun_deaths_subset <- subset(gun_deaths_seasons, intent %in% c("Suicide", "Homicide"))
#gun_deaths_subset$intent <- factor(gun_deaths_subset$intent, levels = c("Homicide", "Suicide"))

ggplot(gun_deaths_subset, aes(x = factor(season), fill = season)) + 
  geom_bar() +
  scale_fill_manual(values = alpha(color_values, alpha_values)) +
  labs(
    title = "Among the most common gun death intents (homicides and suicides), \nsummer remains among the most lethal season",
    subtitle = "Gun deaths between 2012 - 2015",
    x = "Season",
    y = "Number of Deaths",
    fill = "Season"
  ) +
  facet_wrap(~ intent)


# Gun Death Demographics --------------------------------------------------

age_gender_summary <- 
  gun_deaths |> 
  group_by(age, intent, sex) |> 
  summarise(count = n()) |> 
  na.omit()

ggplot(age_gender_summary, aes(x = age, y = count, color = sex)) +
  geom_line(linewidth = 1) +
  labs(title = "Number of Gun Deaths by Age and Gender",
       x = "Age",
       y = "Number of Deaths") +
  facet_wrap(~intent) +
  theme_minimal()

ggplot(age_summary, aes(x = age, y = count)) +
  geom_line(color = "#EE7677", linewidth = 1) +
  labs(
    title = "Number of Gun Deaths by Age",
    x = "Age",
    y = "Number of Deaths"
  ) + 
  facet_wrap(~intent) +
  scale_x_continuous(breaks = seq(0, 100, by = 10))

#-------------------

age_gender_race_summary <- 
  gun_deaths |> 
  group_by(age, intent, sex, race) |> 
  summarise(count = n()) |> 
  na.omit()

ggplot(age_gender_race_summary, aes(x = age, y = count, color = sex)) +
  geom_line() +
  labs(title = "Number of Gun Deaths by Age, Gender, and Race",
       x = "Age",
       y = "Number of Deaths") +
  facet_grid(race ~ intent) +
  theme_minimal()

ggplot(age_gender_race_summary, aes(x = race, y = intent)) +
  geom_count()

#---------------

race_intent_summary <- 
  gun_deaths |> 
  group_by(intent, race) |> 
  summarise(count = n()) |> 
  mutate(race = ifelse(race == "Native American/Native Alaskan", "Native American", race)) |> 
  na.omit()

ggplot(race_intent_summary, aes(x = race, y = intent)) +
  geom_tile(mapping = aes(fill = count)) +
  scale_fill_gradient(low = "#EE7677", high = "#FFFFD1") +
  #scale_fill_gradient(low = "darkred", high = "lightyellow") +
  #scale_fill_brewer(palette = "Reds") +
  labs(
    title = "Majority of gun deaths in US are white suicides, \nfollowed by black homicides",
    x = "Race",
    y = "Intent",
    fill = "Number of deaths"
  )

#------------

black_homicides <- 
  gun_deaths |> 
  filter(intent == "Homicide" & race == "Black") |> 
  group_by(age, sex) |> 
  summarise(count = n()) |> 
  na.omit()

ggplot(black_homicides, aes(x = age, y = count, color = sex)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Reds") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(title = "Homicidal gun deaths are most prevalent among Black men aged 20-30",
       x = "Age",
       y = "Number of Black Homicides",
       color = "Sex") +
  theme_minimal()

white_suicides <- 
  gun_deaths |> 
  filter(intent == "Suicide" & race == "White") |> 
  group_by(age, sex) |> 
  summarise(count = n()) |> 
  na.omit()

ggplot(white_suicides, aes(x = age, y = count, color = sex)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Reds") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(title = "Suicidal gun deaths are most prevalent among White men aged 50-65",
       subtitle = "Followed by White men aged 20-30",
       x = "Age",
       y = "Number of White Suicides",
       color = "Sex") +
  theme_minimal()

