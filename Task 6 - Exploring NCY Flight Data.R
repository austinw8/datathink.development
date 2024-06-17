nycflights13::flights
View(flights)
?flights


# Relationship between delays and month of the year --------------

library(nycflights13)
library(ggplot2)
library(tidyverse)
library(dplyr)

avg_delay <- 
  flights |> 
  group_by(month) |> 
  filter(dep_delay > 0) |> 
  summarise(avg_delay_time = mean(dep_delay))
view(avg_delay)

ggplot(avg_delay, aes(x = month, y = avg_delay_time)) +
  geom_smooth(color = "darkblue", linewidth = 1.25) +
  theme_bw() +
  labs(
    title = "Higher Flight Delay Times in Summer Months",
    x = "Month", 
    y = "Average Delay Time (minutes)"
  ) +
  scale_x_continuous(breaks = seq_along(month.name), labels = month.name)



# Distribution of Flights per Month ---------------------------------------------

flights_per_month <- 
  flights |> 
  group_by(month) |> 
  summarise(count = n())
view(flights_per_month)

ggplot(flights_per_month, aes(x = month)) +
  geom_col(aes(x = month, y = count), fill = "darkblue") +
  labs(
    title = "Number of Flights per Month",
    x = "Month", 
    y = "Number of Flights"
  ) +
  theme_bw() +
  scale_x_continuous(breaks = seq_along(month.name), labels = month.name)


# Distribution of Arrival Delays ------------------------------------------

dep_delay_distr <- 
  flights |> 
  filter(dep_delay > 0, dep_delay < 900)

ggplot(dep_delay_distr, aes(x = dep_delay)) +
  geom_histogram(binwidth = 60, color = "white", fill = "darkblue") +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 780, by = 60)) +
  theme_bw() +
  labs(
    title = "Distribution of Delay Times", 
    x = "Departure Delay (minutes)",
    y = "Frequency"
  )


# Bivariate Summary of Months vs Departure Delay --------------------------
