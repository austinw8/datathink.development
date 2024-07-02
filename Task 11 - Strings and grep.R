#Task 11
library(stringr)
library(tidyverse)

letters <- read_lines("https://byuistats.github.io/M335/data/randomletters.txt")
letters_numbers <- read_lines("https://byuistats.github.io/M335/data/randomletters_wnumbers.txt")

#-----------------
secret_message <- str_c(c("t", strsplit(letters, "")[[1]][seq(1700, nchar(letters), 1700)]), collapse = "")

#----------------
numbers <- str_extract_all(letters_numbers, "[0-9]+")

num_to_let <- function(n) {
  if (n < 1 || n > 26) {stop("Number must be between 1 and 26")}
  if (!is.numeric(n)) {stop("Input must be a number")}
  if (n %% 1 != 0) {stop("Input must be a whole number")}
  char <- switch (n,
                  "1" = "A",
                  "2" = "B",
                  "3" = "C",
                  "4" = "D",
                  "5" = "E",
                  "6" = "F",
                  "7" = "G",
                  "8" = "H",
                  "9" = "I",
                  "10" = "J",
                  "11" = "K",
                  "12" = "L",
                  "13" = "M",
                  "14" = "N",
                  "15" = "O",
                  "16" = "P",
                  "17" = "Q",
                  "18" = "R",
                  "19" = "S",
                  "20" = "T",
                  "21" = "U",
                  "22" = "V",
                  "23" = "W",
                  "24" = "X",
                  "25" = "Y",
                  "26" = "Z"
  )
  return(char)
}

secret_message_3 <- c()
for (n in numbers[[1]]) {
  print(num_to_let(as.numeric(n)))
  secret_message_3 <- c(secret_message_3, num_to_let(as.numeric(n)))
}
str_c(secret_message_3, collapse = "")
print(secret_message_3)

#---------------

longest_vowels <- str_replace_all(letters, '[^:alpha:]', replacement = "")
str_extract_all(longest_vowels, "[aeiou]{8,}")