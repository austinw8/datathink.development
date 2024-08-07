library(tidyverse)
library(nycflights13)
view(nycflights13)
flights
view(flights)

#all flights that had an arrival delay of two or more hours
flights |>
  filter(arr_delay >= 120) |>
  arrange(desc(arr_delay))

#all flights that flew to Houston (IAH or HOU)
flights |>
  filter(dest %in% c("IAH", "HOU"))

#all flights that Were operated by United, American, or Delta
flights |>
  filter(carrier %in% c("UA", "AA,", "DL"))

#all flights that departed in summer (July, August, and September)
flights |>
  filter(month %in% c(7, 8, 9))

#all flights that arrived more than two hours late, but didn’t leave late
flights |>
  filter(arr_delay > 120 & dep_delay <= 0)

#all flights that Were delayed by at least an hour, but made up over 30 minutes in flight
flights |>
  filter(dep_delay >= 60 & dep_delay - arr_delay > 30)

#Sort flights to find the flights with longest departure delays
#Among those, find the flights that left earliest in the morning.
flights |>
  arrange(desc(dep_delay)) |>
  arrange(sched_dep_time) |>
  relocate(dep_delay, sched_dep_time)

#Sort flights to find the fastest flights
flights |>
  mutate(
    speed = distance / (air_time / 60)
    ) |>
  arrange(desc(speed)) |>
  relocate(speed)

#Was there a flight on every day of 2013?
flights |>
  distinct(year, month, day)

#Which flights traveled the farthest distance? 
flights |>
  arrange(desc(distance)) |>
  relocate(distance)

#Which traveled the least distance?
flights |>
  arrange(distance) |>
  relocate(distance)

#Compare dep_time, sched_dep_time, and dep_delay.
#How would you expect those three numbers to be related?
flights |>
  relocate(dep_time, sched_dep_time, dep_delay) |>
  select(1:3)

#Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.
flights |>
  select(dep_time, dep_delay, arr_time, arr_delay)

flights |>
  select(4, 6, 7, 9)

flights |>
  select(starts_with("dep"), starts_with("arr"))

flights |>
  select(dep_time:arr_delay, -contains("sched"))

flights |>
  select(dep_time, dep_time)

variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights |>
  select(any_of(variables))

flights |> 
  select(variables)

flights |> select(contains("TIME"))

flights |> 
  select(contains("TIME", ignore.case = FALSE))

#Rename air_time to air_time_min to indicate units of measurement and move it to the beginning of the data frame.
flights|>
  rename(airtime_min = air_time) |>
  relocate(airtime_min)

flights |> 
  select(tailnum) |> 
  arrange(arr_delay)



