# Task 15 - Does the weather hurt my bottom line? 

library(tidyverse)
library(lubridate)

carwash <- read_csv("https://byuistats.github.io/M335/data/carwash.csv")

carwash_tidy <- carwash |> 
  mutate(date_time_MDT = with_tz(time, tzone = "America/Denver"),
         hour = ceiling_date(date_time_MDT, "hour")) |> 
  group_by(hour) |> 
  summarise(hourly_total = sum(amount, na.rm = TRUE))

library(riem)
weather <- riem_measures(station = "RXE", date_start = "2016-05-13", date_end = "2016-07-18")

weather_tidy <- weather |> 
  mutate(date_time_MDT = with_tz(valid, tzone = "America/Denver"),
         hour = ceiling_date(date_time_MDT, "hour")
         ) |> 
  filter(!is.na(tmpf)) |> 
  select(hour, tmpf:skyc3, skyl1:skyl3, peak_wind_gust:feel) |> 
  relocate(feel, .after = tmpf)

#Join dataframes

full_data <- 
  carwash_tidy |> 
  left_join(weather_tidy, by = "hour")

ggplot(full_data, aes(x = tmpf, y = hourly_total)) +
  geom_point() +
  geom_smooth(aes(method = "lm")) +
  theme_minimal() +
  labs(
    x = "Temperature (degrees Fahrenheit)",
    y = "Hourly Total ($USD)"
  )

