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

#-------------------------------------------------------------------

#------------------------------------------------------------------


```{r}
x <- "this is a string"
x
class(x)
y <- "3"
class(y)
as.numeric(y)
as.numeric(x) #doesn't work! 

z <- c("I", "am", "a", "string", "vector")
z
z[1:2]
z[2:5]
z[-5]

#ugly
print(z)
#simplified and interpreted (pretty)
cat(z)

```

escape special characters with \
```{r}
a <- "As Michael Scott said, \"I'm not superstitious, but I am a little stitious.\""
print(a)
cat(a)
```
line break with \n
```{r}
n <- "As Michael Scott said,\n\"I'm not superstitious, but I am a little stitious.\""
print(n)
cat(n)
```
tab with \t
```{r}
t <- "this has\ta tab"
cat(t)
```
special characters and unicode with \u
```{r}
mu <- "\u00b5"
cat(mu)
```
# stringr
r package to deal with strings (grep = base R version)

```{r}
library(tidyverse)

beyonce <- "I am Beyonce, always."
str_length(beyonce)

str_length("I am Beyonce,\n\t\u2022 always.")

```
```{r}
x <- "Would I rather be feared or loved?"
y <- "Easy. Both."
z <- "I want people to be afraid of how much they love me."

str_c(x, y, z, sep = " ")
str_c("Where", "are", "the", "turtles?!", sep = " ")

```
str_sub()
```{r}
bankrupt <- "I... declare... Bankruptcy!"
str_sub(bankrupt, start = 4, end = 10)
str_sub(bankrupt, start = 6, end = 12)
str_sub(bankrupt, start = -10)


phone <- "800-800-8553"
str_sub(phone, start = 1, end = 3)
str_sub(phone, start = -4)

str_sub(bankrupt, end = 1) <- "We"


```
str_replace()
```{r}
bankrupt
str_replace(bankrupt, pattern = "We", replacement = "I")

#replace the first match
str_replace(phone, pattern = "800-", replacement = "")
#replace all matches
str_replace_all(phone, pattern = "800-", replacement = "")
```
Regular Expressions

```{r}
fruit <- c("Apple", "strawberry", "Banana", "Pear", "Blackberry", "*berry")
str_view(fruit, "an")
str_view_all(fruit, "an")
str_view(fruit, "berry")

#. <- any character
str_view(fruit, ".berry")

# \w or [:alpha:] <- any alpha chsaracter
str_view(fruit, "[:alpha:]berry")
str_view(fruit, "\\wberry")

# \\* <- a literal asterisk
str_view(fruit, "\\*berry")


```


Anchoring
beginning: ^
  end: $
  
  ```{r}
str_view(fruit, "^B") #start with B
str_view(fruit, "a$") #ends with a

#replace all 4 letter words that begin with a with "foo
x <- c("apple", "barn", "ape", "cart", "alas", "pain", "ally")
str_replace(x, pattern = "^a...$", replacement = "foo")
str_view(x, pattern = "^a...$")


```
```{r}
phones <- c("Abba: 555-1234", "Anna: 555-0987", "Andy: 555-7654")

# \d <- digit
str_view(phones, pattern = "\\d\\d\\d-\\d\\d\\d\\d")

# \s [:space:] <- whitespace
str_view(phones, pattern = "\\s")

```
Alternates
```{r}
str_view(phones, "A..a")
str_view(phones, "A[bn][bn]a")
str_view(phones, "A[^b]") # ^ inside [] means not
str_view(phones, "An(na|dy)")

str_view(phones, "(?i)an(na|dy)") #(?i <- ignore case)

```


Quantifiers

?, 0 or 1 time
+, 1 or more times
*, 0 or more times

```{r}
str_view(phones, "\\d+")
str_view(phones, "\\d?")

x <- c("A", "AA", "AAA", "AAAA", "B", "BB")
str_view(x, "^A?")
str_view(x, "^A+")
str_view("color and colour", "colou?r")

```
{n}, exactly n
{n,}, n or more
{0,n}, at most, m
{n,m}, between n and m times

```{r}
str_view(phones, "\\d{3}-\\d{4}")
str_view(phones, "\\d*-\\d*")

```
Regular expressions are greedy
```{r}
love <- "Would I rather be feared or loved? Easy. Both. I want people to be afraid of how much they love me."

str_view(love, "love.*")
str_view(love, "love.?")

str_replace(love, pattern = "love[:alpha:]?", replacement = "x")
str_replace_all(love, pattern = "love[:alpha:]?", replacement = "x")

#replace love or loved with x
```

```{r}
#replace strings with 3 consonants in a row at the start of a word
x1 <- c("string", "priority", "value", "distinction")
str_view(x1, "^[^aeiou]{3}")
```
```{r}
cause <- "I have a cause. It is beCAUSE I hate him."
str_to_lower(cause)
str_to_upper(cause)
str_to_sentence(cause)
str_to_title(cause)
```
```{r}
library(Lahman)
data("People")
People <- People |> 
  as_tibble()

People |> 
  filter(str_detect(nameFirst, "^Jo(e|hn)$")) |> 
  select(nameFirst, nameLast) |> 
  head()
```








