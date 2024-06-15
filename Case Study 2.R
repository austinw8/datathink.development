library(tidyverse)
library(gapminder)
library(dplyr)
view(gapminder)


#filter out Kuwait and add "population per 100k" column
gapminder2 <- 
  gapminder |>
    filter(country != "Kuwait") |> 
    mutate(pop_per_100k = pop / 100000)

#creating life expectancy vs GDP per capita by year graph
ggplot(gapminder2, aes(x = lifeExp, y = gdpPercap)) +
  geom_point(aes(
    x = lifeExp, 
    y = gdpPercap,
    color = continent,
    size = pop_per_100k
    )) +
  facet_grid(~year) +
  theme_bw() +
  scale_y_continuous(trans = "sqrt") +
  labs(
    title = "Life Expectancy and GDP per capita are positively correlated",
    x = "Life Expectancy", 
    y = "GDP per capita",
    size = "Population (100k)",
    color = "Continent"
  )

#create a weighted mean on the gdp
weighted_data <- 
  gapminder2 |> 
  group_by(continent, year) |> 
  summarise(weighted_datamean = weighted.mean(gdpPercap, pop, na.rm = TRUE))
view(weighted_data)

#creating GDP per year growth faceted by continent
ggplot(gapminder2, aes(x = year, y = gdpPercap)) + 
  geom_line(gapminder2, aes(
    x = year,
    y = gdpPercap,
    color = continent, 
    group = country), 
    linewidth = 0.1) +
  geom_point(gapminder2, aes(
    x = year, 
    y = gdpPercap, 
    color = continent, 
    group = country),
    size = 0.5) +
  geom_line(data = weighted_data, aes(
    x = year,
    y = weighted_datamean),
    color = "black") +
  geom_point(data = weighted_data, aes(
    x = year,
    y = weighted_datamean),
    color = "black",
    size = 1.5) +
  scale_y_continuous(trans = "sqrt") +
  facet_wrap(~continent, ncol = 5) + 
  theme_bw() + 
  labs(
    title = "GDP Growth per Year by Continent",
    x = "Year",
    y = "GDP per capita",
    color = "Continent")