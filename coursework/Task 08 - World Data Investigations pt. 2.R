library(devtools)
library(ourworldindata)
library(ggplot2)
install_github("drsimonj/ourworldindata")
theme_set(theme_bw())


# Child Mortality vs. GDP ---------------------------------------------

avg_gdp <- 
  financing_healthcare |> 
  group_by(country, continent) |> 
  summarise(avg_gdp = mean(gdp, na.rm = TRUE), avg_child_mort = mean(child_mort, na.rm = TRUE))

ggplot(avg_gdp, aes(x = avg_gdp, y = avg_child_mort)) +
  geom_point(mapping = aes(color = "darkred")) +
  geom_smooth(method = "lm", color = "red") + 
  labs(
    title = "As GDP rises, child mortailty rates decline",
    x = "Average GDP (USD)",
    y = "Average Child Mortality Rate (per 1,000 born)"
  ) +
  theme(legend.position = "none")


ggplot(avg_gdp, aes(x = avg_gdp, y = avg_child_mort, color = continent)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~continent) +
  labs(
    title = "Each continent has its own GDP vs. child mortality trend",
    x = "Average GDP (USD)",
    y = "Average Child Mortality Rate (per 1,000 born)"
  ) +
  theme(legend.position = "none")



# Child Mortality vs. Life Expectancy ---------------------------------------------

ggplot(financing_healthcare, aes(x = life_expectancy, y = child_mort)) +
  geom_point(mapping = aes(color = "darkred")) +
  labs(
    title = "As life expectency rises, child mortality rates drop",
    x = "Life Expectency",
    y = "Average Child Mortality Rate (per 1,000 born)"
  ) +
  theme(legend.position = "none")
  

ggplot(financing_healthcare, aes(x = life_expectancy, y = child_mort)) +
  geom_point(aes(color = continent)) +
  facet_wrap(~continent) +
  labs(
    title = "As life expectency rises, child mortality rates drop",
    x = "Life Expectency",
    y = "Average Child Mortality Rate (per 1,000 born)"
  ) +
  theme(legend.position = "none")

# Child Mortality by Continent ------------------------------------------

ggplot(data = subset(financing_healthcare, !is.na(continent))) +
  geom_boxplot(aes(x = reorder(continent, child_mort, FUN = median), y = child_mort)) +
  labs(
    title = "Child mortality rates by continent",
    subtitle = "Averages from 1800 - 2015",
    x = "Continent",
    y = "Average Child Mortality Rate (per 1,000 born)"
  )
