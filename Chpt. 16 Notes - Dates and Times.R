#Chpt. 16 Notes - Dates and Times

library(tidyverse)
library(lubridate)
library(nycflights13)

#get current date and time
today()
now()

#sometimes dttm data is spread across columns
flights |> 
  select(year, month, day, hour, minute)

#to create dttm from this...
flights |> 
  select(year, month, day, hour, minute) |> 
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights |> 
  filter(!is.na(dep_time), !is.na(arr_time)) |> 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))


#view distribution of departure times across year
flights_dt |> 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400)

#or within a single day
flights_dt |> 
  filter(dep_time < ymd(20130102)) |> 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 600)

today()
as_datetime(today())
now()
as_date(now())

ymd(c("2010-10-10", "bananas"))
today()
today(tzone = "GMT")

d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)

datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime) #year
month(datetime) #month (can take label = TRUE)
mday(datetime) #day of month
yday(datetime) #day of year
wday(datetime) # day of week (can take label = TRUE)

flights_dt |> 
  mutate(wday = wday(dep_time, label = TRUE)) |> 
  ggplot(aes(x = wday)) +
  geom_bar()


flights_dt |> 
  count(week = floor_date(dep_time, "week")) |> 
  ggplot(aes(week, n)) +
  geom_line()

flights_dt |> 
  mutate(dep_hour = update(dep_time, yday = 1)) |> 
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)












