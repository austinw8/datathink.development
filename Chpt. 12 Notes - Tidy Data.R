#Chpt. 12 Notes - Tidy Data

library(tidyverse)


# Tidy Data Rules ---------------------------------------------------------
#1. Each variable must have its own column.
#2. Each observation must have its own row.
#3. Each value must have its own cell.

table1 <- table1

table2 <- table2

table3 <- table3

table4a <- table4a 

table4b <- table4b

table2.0 <- 
  table2 |> 
  group_by(type) |> 
  summarise(n = n())
view(table2.0)


# Steps to Tidying Data ---------------------------------------------------

#1. Figure out what your variable and observations are
#2. Resolve these common problems
  #a. One variable might be spread across multiple columns.
  #b. One observation might be scattered across multiple rows.


# Tidying Tables 4a and 4b: Longer -----------------------------------------

tidy4a <- 
  table4a |> 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

tidy4b <- 
  table4b |> 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")

tidy_table4 <- 
  left_join(tidy4a, tidy4b)


# Tidying Table 2: Wider ----------------------------------------------------

tidy2 <- 
  table2 |> 
  pivot_wider(names_from = type, values_from = count)


# Exercises ---------------------------------------------------------------

stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks |> 
  pivot_wider(names_from = year, values_from = return) |> 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")

table4a |> 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

people_tidy <- 
  people |> 
  pivot_wider(names_from = names, values_from = values)

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

tidy_preg <- 
  preg |> 
  pivot_longer(cols = c(male, female), 
               names_to = "gender", 
               values_to = "count",
               values_drop_na = TRUE)


# Tidying Table 3: Separating ----------------------------------------------------

tidy_table3 <- 
  table3 |> 
  separate(rate, into = c("cases", "population"), convert = TRUE)
tidy_table3

#Exercises
tibble1 <- 
  tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) |> 
  separate(x, into = c("1st", "everything_else"), fill = "left")

tibble2 <- 
  tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"))


# Missing Values ----------------------------------------------------------
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

#implicit to explicit
stocks |> 
  pivot_wider(names_from = year, values_from = return)

stocks |> 
  complete(year, qtr)

#explicit to implicit
stocks |> 
  pivot_wider(names_from = year, values_from = return) |> 
  pivot_longer(
    cols = c(`2015`, `2016`),
    names_to = "year", 
    values_to = "return",
    values_drop_na = TRUE
  )

treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

treatment |> 
  fill(person)


# Case Study Example ------------------------------------------------------

view(who)

who1 <- 
  who |> 
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = "key",
    values_to = "cases",
    values_drop_na = TRUE
  )
view(who1)
who1 |> 
  count(key)
?who

who2 <- 
  who1 |> 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))

who3 <- 
  who2 |> 
  separate(key, c("new", "type", "sexage"), sep = "_")

who4 <- 
  who3 |> 
  select(-new, -iso2, -iso3)

who5 <- 
  who4 |> 
  separate(sexage, c("sex", "age"), sep = 1)

who_tidy <- 
  who |> 
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = "key",
    values_to = "cases",
    values_drop_na = TRUE
  ) |> 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) |> 
  separate(key, c("new", "var", "sexage")) |> 
  select(-new, -iso2, -iso3) |> 
  separate(sexage, c("sex", "age"), sep = 1)
