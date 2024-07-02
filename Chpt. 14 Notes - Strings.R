library(tidyverse)
library(babynames)
library(ggplot2)
view(babynames)

string1 <- "This is a string"
string2 <- "Getting stuck in a string sucks"
string3 <- "Practicing with strings and so called \"string stuff\""

double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"

str_view(string1)
str_view(single_quote)
str_view(string3)

raw_string <- r"(I can put anything I want in here \\ " and ' it'll show up \" however I 'type' it)"
str_view(raw_string)

exercise1.0 <- r"(He said "That's amazing!")"
str_view(exercise1.0)
exercise1.1 <- "He said \"That\'s amazing!\""
str_view(exercise1.1)

x <- "\u00a0"
str_view(x)

y <- "This\u00a0is\u00a0tricky"
str_view(y)


# Creating Strings from Data ----------------------------------------------

str_c("x", "y")
str_c("x", "y", "Z")
str_c("Hello ", c("John", "Susan"))

df <- tibble(name = c("Flora", "David", "Terra", NA))

greeting <- df |> 
  mutate(greeting = str_c("Hi ", name, "!"))

df |> 
  mutate(
    greeting1 = str_c("Hi ", coalesce(name, "you"), "!"),
    greeting2 = coalesce(str_c("Hi ", name, "!"), "Hi!")
  )

df |> mutate(greeting = str_glue("Hi {name}!"))

str_flatten(c("x", "y", "z"))
str_flatten(c("x", "y", "z"), ", ")
str_flatten(c("x", "y", "z"), ", ", last = ", and ")

df2 <- tribble(
  ~ name, ~ fruit,
  "Carmen", "banana",
  "Carmen", "apple",
  "Marvin", "nectarine",
  "Terence", "cantaloupe",
  "Terence", "papaya",
  "Terence", "mandarin"
)
df2 |> 
  group_by(name) |> 
  summarise(fruits = str_flatten(fruit, ", "))

# Exercises --------------

str_c("hi ", NA)
str_c(letters[1:2], letters[1:3])

paste0("hi ", NA)
paste0(letters[1:2], letters[1:3])

food <- "banana"
price <- 100
str_c("The price of a ", food, " is $", price)
str_glue("The price of a {food} is ${price}")


# Extracting Data from Strings --------------------------------------------

df1 <- tibble(x = c("a,b,c", "d,e", "f"))
df1 |> 
  separate_longer_delim(x, delim = ",")

df2 <- tibble(x = c("1211", "131", "21"))
df2 |> 
  separate_longer_position(x, width = 1)

df3 <- tibble(x = c("a10.1.2022", "b10.2.2011", "e15.1.2015"))
df3 |> 
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", "edition", "year")
  )

df4 <- tibble(x = c("202215TX", "202122LA", "202325CA")) 
df4 |> 
  separate_wider_position(
    x,
    widths = c(year = 4, age = 2, state = 2)
  )

df5 <- tibble(x = c("1-1-1", "1-1-2", "1-3", "1-3-2", "1"))
df5 |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"), 
    too_few = "debug"
  )
df5 |> 
  separate_wider_delim(
    x, 
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "align_end"
  )

df6 <- tibble(x = c("1-1-1", "1-1-2", "1-3-5-6", "1-3-2", "1-3-5-7-9"))
df6 |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "drop"
  )

df6 |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "merge"
  )


# Letters -----------------------------------------------------------------

str_length(c("a", "R for Data Science", NA))

babynames |> 
  count(length = str_length(name), wt = n)

babynames |> 
  filter(str_length(name) == 15) |> 
  count(name, wt = n, sort = TRUE)

x <- c("Apple", "Banana", "Pear")
str_sub(x, 2, 5)
str_sub(x, -3, -1)

babynames1 <- 
  babynames |> 
  mutate(
    first = str_sub(name, 1, 1),
    last = str_sub(name, -1, -1)
  ) |> 
  count(first, wt = n)

babynames2 <- 
  babynames |> 
  mutate(
    first = str_sub(name, 1, 1),
    last = str_sub(name, -1, -1)
  ) |> 
  count(first)

ggplot(babynames1, aes(x = reorder(first, -n), y = n)) +
  geom_col() + 
  theme_bw() +
  labs(
    title = "More people with 'J' names"
  )

ggplot(babynames2, aes(x = reorder(first, -n), y = n)) +
  geom_col() +
  theme_bw() +
  labs(
    title = "More 'A' names"
  )

#-------------------------------------------------------------------------------

x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")

name <- "Hadley"
time_of_day <- "morning"
birthday <- TRUE
str_c("Good ", time_of_day, " ", name,
      if (birthday) " and HAPPY BIRTHDAY",
      ".")

#turn a vector of strings into a single string
str_c(c("x", "y", "z"), collapse = ", ")

#subsetting
fruit <- c("Apple", "Banana", "Pear")
str_sub(fruit, 1, 3)
str_sub(fruit, -3, -1)

str_to_upper(c("i", "I"))
str_to_upper(c("i", "I"), locale = "tr") #Turkish locale

#paste() and paste0() concatenate strings, similat to str_c()
str_c("x", "y", "z") #no seperation
paste("x", "y", "z") #adds space between strings
paste0()









