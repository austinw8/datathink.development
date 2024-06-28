#Task 11
library(stringr)

letters <- read_lines("https://byuistats.github.io/M335/data/randomletters.txt")
letters_numbers <- read_lines("https://byuistats.github.io/M335/data/randomletters_wnumbers.txt")

#-----------------
secret_message <- paste0(strsplit(letters, "")[[1]][seq(1700, nchar(letters), 1700)], collapse = "")

v1 <- strsplit(letters, "")[[1]]
v1[seq(0, by = 1700, length(v1))]

#----------------
ext_numb <- gsub("[^0-9]", "", letters_numbers)

numbers <- gregexpr("[0-9]+", letters_numbers)
result <- regmatches(letters_numbers, numbers)
numeric_result <- as.numeric(unlist(result))

letters_list <- letters[numeric_result]
print(letters_list)

#---------------
