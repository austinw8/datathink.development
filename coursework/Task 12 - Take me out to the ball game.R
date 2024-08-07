# Task 12

library(tidyverse)
library(Lahman)
library(blscrapeR)
library(ggplot2)

people <- as_tibble(Lahman::People)
salaries <- as_tibble(Lahman::Salaries)
teams <- as_tibble(Lahman::Teams)
college_playing <- as_tibble(Lahman::CollegePlaying)
schools <- as.tibble(Lahman::Schools)

#Goal = prove that BYU has the best MLB success compared to other Utah schools

#find primary keys
people |> 
  count(playerID) |> 
  filter(n > 1)

teams |> 
  count(teamID, yearID) |> 
  filter(n > 1)

salaries |> 
  count(playerID, yearID, teamID) |> 
  filter(n > 1)

college_playing |> 
  count(playerID, schoolID, yearID) |> 
  filter(n > 1)

#Combine data frames to show full college name, player name, & annual earnings

combined_stats <- 
  salaries |> 
  left_join(people, by = "playerID") |> 
  select(yearID, playerID, teamID, salary, nameFirst, nameLast) |> 
  left_join(college_playing, by = "playerID", relationship = "many-to-many") |> 
  left_join(schools, by = "schoolID", relationship = "many-to-many") |> 
  na.omit() |> 
  select(yearID.x, playerID, teamID, salary, nameFirst, nameLast, schoolID, name_full, state) |> 
  rename(year = "yearID.x")
 # mutate(salary_adjusted = inflation_adjust(salary, 2017))

dummy <- inflation_adjust(base_date = "1985-01-01") |> 
  group_by(year) |> 
  summarise(adjusted_dollar = (first(adj_dollar_value))) |> 
  mutate(year = as.numeric(year)) |> 
  right_join(combined_stats, by = "year") |> 
  mutate(`1985_adjusted_salay` = (salary / adjusted_dollar))
             
#plotting---------------

plot_data <- 
dummy |> 
  filter(state == "UT") |> 
  group_by(year, name_full, state) |> 
  summarise(average_salary = mean((salary / 1000000), na.rm = TRUE))

byu_players <- 
  dummy |> 
  filter(name_full == "Brigham Young University") |> 
  group_by(year) |> 
  summarise(average_salary = mean(salary / 1000000))

ggplot(plot_data, aes(x = reorder(name_full, -average_salary), y = average_salary, fill = name_full)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("darkblue", "grey", "grey", "grey", "grey", "grey", "grey", "grey")) +
  labs(
    title = "MLB players from BYU earn significantly more than players from any other Utah school",
    subtitle = "Followed (not that closely) by Dixie State",
    x = "",
    y = "Combined Average Salary (million USD)",
  )

ggplot(plot_data, aes(x = reorder(name_full, -average_salary), y = average_salary, fill = name_full)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values=c("darkblue", "grey", "grey", "grey", "grey", "grey", "grey", "grey")) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 6)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme(legend.position = "none") +
  labs(
    title = "MLB players from BYU earn significantly more than players from any other Utah school",
    subtitle = "Followed (not that closely) by Dixie State",
    x = "",
    y = "Average Salary (million USD)",
  )
