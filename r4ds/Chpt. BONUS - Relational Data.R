#Chapter 13 Notes: Relational Data

library(tidyverse)
library(nycflights13)
library(Lahman)

planes |> 
  count(tailnum) |> 
  filter(n > 1)

weather |> 
  count(year, month, day, hour, origin) |> 
  filter(n > 1)

People <- as_tibble(Lahman::People)
Teams <- as_tibble(Lahman::Teams)
Fielding <- as_tibble(Lahman::Fielding)
Pitching <- as_tibble(Lahman::Pitching)
Batting <- as_tibble(Lahman::Batting)

head(People)

#Primary key = respective to each table and uniquely identifies observations in a table
#can be more than one column combined together

People |> 
  count(playerID) |> 
  filter(n > 1)

#People$PlayerID is the foreign key from the perspective of batting

head(Batting)
Batting |> 
  count(playerID) |> 
  filter(n > 1)
#in batting, playerID is NOT a primary key

head(Teams)


# Joins -------------------------------------------------------------------
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2", 
  3, "x3"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2", 
  4, "y4"
)

#inner_join - keeping observations that occur in both x and y
x |> 
  inner_join(y, by = "key")

#select all batting stats for players who were born in the 1980s
nrow(Batting)

People |> 
  filter(between(birthYear, 1980, 1989)) |> 
  inner_join(Batting, by = "playerID")


#left_join
x |> 
  left_join(y, by = "key")

#right_join
x |> 
  right_join(y, by = "key")

#full_join
x |> 
  full_join(y, by = "key")

#---------------------------------------
#add in team name to the batting data frame

Teams |> 
  select(teamID, name, yearID) |> 
  distinct() |> 
  right_join(Batting, by = c("teamID", "yearID")

batting |> 
  left_join()


#list the first name, last name, and team name for every player who played in 2018
Batting |> 
  select(yearID, playerID, teamID) |> 
  filter(yearID == 2018) |> 
  left_join(Teams, by = c("yearID", "teamID")) |> 
  left_join(People, by = "playerID") |> 
  select(nameFirst, nameLast, name) |> 
  distinct()

#filtering joing
#semi_join
x |> 
  semi_join(y, by = "key")

x |> 
  inner_join(y, by = "key")

#anti_join
#what's in x but not in y?
x |> 
  anti_join(y, by = "key")
 

#find the 10 players with the highest number of strikeouts (SO) from the batting table and assign to a new dataframe.
#Join the appropriate data frams to select all players from People for those 10 players

ten_worst <- Batting |> 
  group_by(playerID) |> 
  summarise(count_strikeout = sum(SO, na.rm = TRUE)) |> 
  slice_max(count_strikeout, n = 10)

People |> 
  semi_join(ten_worst, by = "playerID")

#another way to do this... 
ten_worst_vector <- Batting |> 
  group_by(playerID) |> 
  summarise(count_strikeout = sum(SO, na.rm = TRUE)) |> 
  slice_max(count_strikeout, n = 10) |> 
  pull(playerID)

People |> 
  filter(playerID %in% ten_worst_vector)


#when key names don't match....
library(nycflights13)
flights |> 
  left_join(airports, by = c("origin" = "faa"))


#set operation
x <- tribble(
  ~key, ~val,
  1, "a", 
  1, "b",
  2, "a"
)

y <- tribble(
  ~key, ~val,
  1, "a",
  2, "b"
)

#union - all unique rows from x and y
union(x, y)

#intersect - common rows in both x and y, keeping just unique rows
intersect(x, y)

#setdiff - all rows from x which are not also rows in y, keeping just unique rows

#what's in x but not in y?
setdiff(x, y)

#what's in y but not in x?
setdiff(y, x)

