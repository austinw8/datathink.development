# Task 14

library(tidyverse)
library(purrr)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

median(df$a)

output <- vector("double", ncol(df)) #output
for (i in seq_along(df)) { #sequence
  output[[i]] <- median(df[[i]]) #body
}
output

#the 3 components of a FOR loop
#(1) output - includes type and length of vector
#(2) sequence
#(3) body

#Practice ---------------

#Compute the mean of every column in mtcars
mtcars
results <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  results[[i]] <- mean(mtcars[[i]])
}

#Determine the type of each column in nycflights13::flights.
nycflights13::flights

#Compute the number of unique values in each column of iris.
iris
unique(iris$Sepal.Length)

unique_counts <- numeric(ncol(iris))
for (i in seq_along(iris)) {
  unique_counts[[i]] <- length(unique(iris[[i]]))
}


out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
letters
#same as...
str_c(letters, collapse = "")


#Alice the camel
humps <- 10
for (i in humps:0) {
  for (j in 1:3) {
    cat(paste("Alice the camel has", i, ifelse(i == 1, "hump.", "humps."), "\n"))
  }
  cat("So go, Alice, go.\n")
  cat("Boom, boom, boom!\n\n")
}

#ten in the bed
ten_in_the_bed <- function(number, structure = "bed") {
  for (i in number:1) {
    cat(paste("There were", i, "in the", structure, "and the little one said,\n"))
    cat("\"Roll over, roll over!\"\n")
    cat("So they all rolled over and one fell out.\n\n")
  }
  cat("There were no more in the", structure, ".\n")
}
ten_in_the_bed(10)
ten_in_the_bed(5)

# Variations --------------------------------------------------------------

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

#the above but as a for loop...
for (i in seq_along(df)) {
  df[[i]] <- rescale
}


# For Loops with Cupcakes -------------------------------------------------
means <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  means[[i]] <- mean(mtcars[[i]], na.rm = TRUE)
}

medians <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  medians[[i]] <- median(mtcars[[i]], na.rm = TRUE)
}

library(purrr)
means <- map_dbl(mtcars, mean)
medians <- map_dbl(mtcars, median)



# Kelsey Codes: Loops and Iterations --------------------------------------

#Vectors: atomic vectors (everything has to be same type) and lists (flexible, contain many data types)
#6 data types: logical (T/F), Integer (1L, 2L, 5L), Double (4.35, -51.6), complex, raw (bye-level data), special cases (factors and dates)

double <- c(1, 10, 2)
double
length(double)
typeof(double)
class(double)
is.double(double)

int <- c(1L, 10L, 2L)
length(int)
is.double(int)
is.integer(int)

char <- c("hi", "hello", "goodmorning")
length(char)
typeof(char)

logi <- c(TRUE, FALSE, FALSE)
is.logical(logi)
length(logi)

x <- c(horse = 7, man = 3, dog = 8)
x
#vector subsetting
x[1]
x[-1]
x[2:4]
logi[1]
x["man"]
x[c("man", "horse")]
x[[1]]
x[2]
x[[2]]
unname(x[1])

#lists
my_first_list <- list(x = "a", y = "1", z = c(1L, 2L, 3L), list("a", 1))
my_first_list
length(my_first_list)
str(my_first_list)

my_first_list[1:2]
my_first_list["y"]
my_first_list["z"]

my_first_list[["y"]]
my_first_list[["z"]]

my_first_list$y

#give name to 4th item (list)
names(my_first_list)[4] <- "a"
names(my_first_list)
my_first_list


#working with logicals
x <- c("I", "like", "turtles")
lvec <- c(TRUE, FALSE, TRUE)
x[lvec]
x[nchar(x) > 2]

#loops
#basic loop syntax
for (variable in vector) {
  do something
}


for(i in 1:10) {
  print(i)
}

for (i in c("cat", "dog", "turtle")) {
  print(i)
}

for (i in x) {
  print(i)
}

mean(double)
double ^ 2


n <- 100
banana <- vector(mode = "double", length = n)
for (i in 1:n){
  banana[i] <- i^2
}
sum(banana)


n <- 100
banana <- vector(mode = "double", length = n)
for (i in seq_along(1:n)){
  banana[i] <- i^2
}
sum(banana)


#total and cumulative total
x <- c(8, 1, 3, 1, 3)
x[1] + x[2] + x[3] + x[4] + x[5]
sum(x)

#best practice is to build loop outside of function to test it, then build function
sumval <- 0

total <- function(vec){
  if(!is.numeric(vec)){
    stop("vec needs to be numeric")
  }
  sumval <- 0
for (i in seq_along(vec)) {
  sumval <- sumval + vec[[i]]
}
  return(sumval)
}
total(x)



cum_total <- function(vec){
  if(!is.numeric(vec)){
    stop("vec needs to be numeric")
  }
  
  cum_val <- vector(mode = "double", length = length(vec))
  for (i in seq_along(vec)) {
    if (i == 1) {
      cum_val [i] <- vec[[i]]
    } else {
      cum_val[i] <- cum_val[i - 1] + vec[[i]]
    }
  }
  return(cum_val)
}
cum_total(x)
cumsum(x)


#while loop

flip <- function(){
  sample(c("T", "H"), 1)
}
flip <- 0
nheads <- 0

while (nheads < 3){
  if (flip() == "H"){
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}


#Purrr

square <- function(x){
  return(x * x)
}
library(tidyverse)
vector1 <- c(2, 4, 5, 7)

map(vector1, square)
map(vector1, cumsum)





















