# Chapter 11: Data Import

library(tidyverse)
library(janitor)
library(readr)
library(tibble)

students <- read_csv("https://pos.it/r4ds-students-csv")
view(students)
read_csv()
students

#turn written NA into actual NA
students <- 
  students |> 
  read_csv("https://pos.it/r4ds-students-csv", na = c("N/A", ""))

#renaming non-syntactic names
students |> 
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )

#turn character variable type into factors (for categorical data)
#fixing numbers --> character to dbl
students <- 
  students |> 
  clean_names() |> 
  mutate(meal_plan = factor(meal_plan),
  age = parse_number(if_else(age == "five", "5", age)
  ))

# Reading csv column headings 
read_csv(
  "a, b, c
  1, 2, 3
  4, 5, 6"
)

read_csv(
  "The first line of metadata
  The second line of metadata
  x, y, z
  1, 2, 3",
  skip = 2)

read_csv(
  "# A comment I want to skip
  x, y, z
  1, 2, 3",
  comment = "#"
)

read_csv(
  "1, 2, 3
  4, 5, 6", 
  col_names = FALSE
)

read_csv(
  "1, 2, 3
  4, 5, 6", 
  col_names = c("x", "y", "z")
)


?read_csv
?read_tsv
?read_fwf

read_csv(
  "x, y/n1, 'a,b'", 
  col_names = FALSE
)

data <- 
  read_csv(
  "x, y/n1, 'a,b'",
  col_names = FALSE,
  quote = "'")
data <- read_csv("x,y\n1,'a,b'", quote = "'")

read_csv("a,b\n1,2,3\n4,5,6")
#header indicates 2 columns, but there are more than 2 columns in following lines

read_csv("a,b,c\n1,2\n1,2,3,4")
#same as above

read_csv("a,b\n\"1")
#quoting field is incomplete

read_csv("a,b\n1,2\na,b")

read_csv("a;b\n1;3")

#practice with non-syntactic names
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
annoying

variable_1 <- annoying$`1`

ggplot(annoying, aes(x = `1`, y = `2`)) +
  geom_point()

annoying |> 
  mutate(`3` = `2`/`1`) |> 
  rename(
    three = `3`,
    two = `2`,
    one = `1`
  )

#column types and problems
simple_csv <- "
  x
  10
  .
  20
  30"
read_csv(simple_csv)

df <- 
  read_csv(
    simple_csv,
    col_types = list(x = col_double())
  )
problems(df)

read_csv(simple_csv, na = ".")

another_csv <- 
  "x, y, z
  1, 2, 3"

read_csv(
  another_csv,
  col_types = cols(.default = col_character())
)

read_csv(
  another_csv,
  col_types = cols_only(x = col_character())
)


#reading data from multiple files
sales_files <- 
  read_csv(c(
  "https://pos.it/r4ds-01-sales",
  "https://pos.it/r4ds-02-sales",
  "https://pos.it/r4ds-03-sales"), 
  id = "file")
view(sales_files)

sales_files2 <- 
  list.files("data", pattern = "sales\\.csv$", full.names = TRUE)
sales_files2


#writing to a file
write_csv(students, "students.csv")
students
write_csv(students, "students-2.csv")
read_csv("students-2.csv")

write_rds(students, "students.rds")
read_rds("students.rds")

library(arrow)
write_parquet(students, "students.parquet")
read_parquet("students.parquet")


#data entry
tibble(
  x = c(1, 2, 5),
  y = c("h", "m", "g"),
  z = c(0.08, 0.83, 0.60)
)

tribble(
  ~x, ~y, ~z,
  1, "h", 0.08,
  2, "m", 0.83,
  5, "g", 0.60
)
)