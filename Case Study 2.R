library(tidyverse)
library(gapminder)
library(dplyr)
view(gapminder)


# prepping data --------------------------------------------------

gapminder_without_Kuwait <- 
  gapminder |>
    filter(!country == "Kuwait") |> 
    mutate(gapminder_without_Kuwait,
           pop_per_100k = pop / 100000)

# create 1st scatter plot -------------------------------------------------

ggplot(gapminder_without_Kuwait, aes(x = lifeExp, y = gdpPercap)) +
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
    title = "Life Expectancy vs. GDP per Capita by Year",
    x = "Life Expectancy", 
    y = "GDP per capita",
    size = "Population (100k)",
    color = "Continent"
  )

# create 2nd line graph ---------------------------------------------------

gapminder_without_Kuwait |> 
  group_by(continent) |> 
  summarise(avg_gdp_percap = weighted.mean(gdpPercap))

ggplot(gapminder_without_Kuwait, aes(x = year, y = gdpPercap)) + 
  geom_line(aes(
    x = year, 
    y = gdpPercap, 
    color = continent, 
  )) +
  facet_grid(~continent) + 
  theme_bw() + 
  labs(
    title = "GDP Growth per Year by Continent",
    x = "Year",
    y = "GDP per capita",
    color = "Continent"
  )
