library(nycflights13)
library(tidyverse)
library(ggplot2)
library(quantreg)
library(RColorBrewer)
View(flights)


#1. If I am leaving before noon, which two airlines do you recommend at each airport (JFK, LGA, EWR) that will have the lowest delay time at the 75th percentile? 

flights_filtered <- 
  flights |> 
  filter(sched_dep_time < 1200) |> #find flights before noon
  group_by(origin, carrier) |> 
  summarise(dep_delay_75th = quantile(dep_delay, 0.75, na.rm = TRUE)) |> #delays by carrier and airport
  group_by(origin) |> 
  top_n(2, dep_delay_75th) |> 
  arrange(origin, dep_delay_75th) #top 2 carriers per airport

ggplot(flights_filtered, aes(x = origin, y = dep_delay_75th, fill = carrier)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("blue", "darkblue", "lightblue", "skyblue", "cyan")) +
  labs(
    title = "Top 2 Airports by 75th Percentile Delay", 
    subtitle = "Pre-noon departures",
    x = "Airport",
    y = "75th Percentile Delay (minutes)",
  ) +
  scale_fill_discrete(name = "Airline", labels = c("Endeavor Air", "Atlantic Southeast", "Skywest", "United Airlines", "ACES Colombia")) +
  theme_bw()

#output graph shows top 2 airlines recommended at each airport based on the 75th percentile departure delay for flights leaving before noon. 
#Prioritizes airlines with shorter delays


#2.Which origin airport is best to minimize my chances of a late arrival when I am using Delta Airlines?

flights_filtered2 <- 
  flights |> 
  filter(carrier == "DL") |>  #find Delta flights only
  group_by(origin) |> 
  summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) |> #calculate average arrival delay for each origin airport
  arrange(avg_arr_delay)

ggplot(flights_filtered2, aes(x = origin, y = avg_arr_delay)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(
    title = "Average Arrival Delay of NYC Airports",
    subtitle = "Planes flying from JFK Airport arrive the earliest to their destination",
    x = "Airport",
    y = "Average Arrival Delay (minutes)"
  ) +
  theme_bw()


#3. Which destination airport is the worst (you decide on the metric for worst) airport for arrival time?

flights_filtered3 <- 
  flights |> 
  filter(origin %in% c("JFK", "EWR", "LGA")) |> 
  group_by(dest) |> 
  summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) |> 
  slice_max(avg_arr_delay, n = 10)

ggplot(flights_filtered3, aes(x = reorder(dest, -avg_arr_delay), y = avg_arr_delay)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(
    title = "Average Arrival Delays", 
    subtitle = "Destination Airports",
    x = "Airport",
    y = "Average Arrival Delay (minutes)"
  ) +
  theme_bw()


#TESTS
fivenum(flights$dep_delay)
quantile(flights$dep_delay, na.rm = TRUE)
lapply(flights[4:9], quantile, na.rm = TRUE)

