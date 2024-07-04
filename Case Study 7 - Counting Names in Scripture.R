#Case study 7: Counting names in scripture

library(tidyverse)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)
library(stringi)
library(rio)
library(readr)
library(dplyr)
library(DescTools)

# To get the standard works data
scriptures <- rio::import("http://scriptures.nephi.org/downloads/lds-scriptures.csv.zip")

# to get the Savior names
savior_names <-readRDS(gzcon(url("https://byuistats.github.io/M335/data/BoM_SaviorNames.rds")))

#functions that might help with this task
str_detect() - returns TRUE/FALSE if the pattern is found
str_locate_all() - locates position of pattern
str_replace_all() - replaces all occurances of a pattern
str_extract_all() - extracts all matches of a pattern (lays them out for you)
stri_stats_latex - displays character count, words, etc. 

#Find each instance of a Savior name in the Book of Mormon

book_of_mormon <- scriptures |> 
  filter(volume_title == "Book of Mormon") |> 
  select(volume_title, book_title, chapter_number:verse_title)

savior_pattern <- paste(savior_names$name, collapse = "|")

#function to count words between Savior names
words_between <- function(text, pattern) {
  locations <- str_locate_all(text, pattern)[[1]]
  words <- unlist(strsplit(text, "\\s+"))
  distances <- numeric()
  
  if (nrow(locations) > 1) {
    start_indices <- c(1, locations[, 2] + 1)
    end_indices <- c(locations[, 1] - 1, nchar(text))
    
    for (i in 2:length(start_indices)) {
      distance <- sum(str_count(substr(text, start_indices[i-1], end_indices[i-1]), "\\S+"))
      distances <- c(distances, distance)
    }
  }
  
  distances
}

#-----------------------------------------

savior_pattern <- paste(savior_names$name, collapse = "|")
all_bom <- paste(book_of_mormon$scripture_text, collapse = " ")
all_bom

words_between_savior_names_bom <- data.frame(words_between(all_bom, savior_pattern))
typeof(all_bom)

mean(words_between_savior_names$words_between.all_bom..savior_pattern.)
quantile(words_between_savior_names$words_between.all_bom..savior_pattern.)
mode(words_between_savior_names$words_between.all_bom..savior_pattern.)

mode_2 <- function(x) {
  tbl <- table(x)
  mode_value <- as.numeric(names(tbl)[which.max(tbl)])
  return(mode_value)
}

mode_2(words_between_savior_names$words_between.all_bom..savior_pattern.)

ggplot(words_between_savior_names, aes(x = words_between.all_bom..savior_pattern.)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(xlim = c(0, 160)) +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  scale_x_continuous(n.breaks = 10) +
  labs(
    title = "Frequent references to the Savior in the Book of Mormon",
    x = "Number of words between Savior references", 
    y = ""
  )

#----------------------------------
ggplot(words_between_savior_names, aes(x = words_between.all_bom..savior_pattern.)) +
  geom_density() +
  coord_cartesian(xlim = c(0, 200)) +
  theme_minimal() +
  scale_x_continuous(n.breaks = 20) +
  labs(
    title = "The Savior is most often referenced every 34 words in the Book of Mormon",
    x = "Number of words between Savior references", 
    y = "Density of reference"
  )

ggplot(words_between_savior_names_bom, aes(x = words_between.all_bom..savior_pattern.)) +
  geom_histogram(binwidth = 1, color = "white") +
  coord_cartesian(xlim = c(3, 96)) +
  theme_minimal() +
  scale_x_continuous(n.breaks = 20) +
  labs(
    title = "The Savior is most often referenced every 2 words in the Book of Mormon",
    subtitle = "On average, every 65 words",
    x = "Number of words between Savior references", 
    y = "Frequency of reference gap"
  )

# Total references to the savior

book_order <- c("1 Nephi", "2 Nephi", "Jacob", "Enos", "Jarom", "Omni", "Words of Mormon", "Mosiah", "Alma", "Helaman", "3 Nephi", "4 Nephi", "Mormon", "Ether", "Moroni")

bom_references <- book_of_mormon |> 
  mutate(num_ref = str_count(scripture_text, savior_pattern)) |> 
  group_by(book_title) |> 
  summarise(total_ref = sum(num_ref),
            verse_count = n()) |> 
  mutate(book_title = factor(book_title, levels = book_order))
  

ggplot(bom_references, aes(x = book_title, y = total_ref, fill = book_title)) +
  geom_bar(stat = "identity", fill = "#42558C") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(
    title = "References to the Savior by books in the Book of Mormon",
    x = "Book Title", 
    y = "Number of References"
  )

# Compare to other scriptures ---------------------------------------------

# Adding additional names to savior name list -----------------------------

savior_names_filtered <- savior_names |> 
  select(name)

add_names <- data.frame(name = c("shaddai", "elyon", "adonai", "yahweh", "jehovah", "olam", "qanna", "elohim", "the word"))

savior_names <- bind_rows(savior_names, add_names)

#---------------------------------------------------------------------------

#BOM
all_bom <- paste(book_of_mormon$scripture_text, collapse = " ")
avg_ref_gap_bom <- data.frame(mean(words_between(all_bom, savior_pattern))) |> 
  select(mean.words)

#NT
new_testament <- scriptures |> 
  filter(volume_title == "New Testament") |> 
  select(volume_title, book_title, chapter_number:verse_title)
all_nt <- paste(new_testament$scripture_text, collapse = " ")
avg_ref_gap_nt <- data.frame(mean(words_between(all_nt, savior_pattern)))

#OT
old_testament <- scriptures |> 
  filter(volume_title == "Old Testament") |> 
  select(volume_title, book_title, chapter_number:verse_title)
all_ot <- paste(old_testament$scripture_text, collapse = " ")
avg_ref_gap_ot <- data.frame(mean(words_between(all_ot, savior_pattern)))

#DC
doctrine_covenants <- scriptures |> 
  filter(volume_title == "Doctrine and Covenants") |> 
  select(volume_title, book_title, chapter_number:verse_title)
all_dc <- paste(doctrine_covenants$scripture_text, collapse = " ")
avg_ref_gap_dc <- data.frame(mean(words_between(all_dc, savior_pattern)))

#PGP
pearl_great_price <- scriptures |> 
  filter(volume_title == "Pearl of Great Price") |> 
  select(volume_title, book_title, chapter_number:verse_title)
all_pgp <- paste(pearl_great_price$scripture_text, collapse = " ")
avg_ref_gap_pgp <- data.frame(mean(words_between(all_pgp, savior_pattern)))


avg_ref_gap <- data.frame(book_title = c("Book of Mormon", "New Testament", "Old Testament", "Doctrine and Covenants", "Pearl of Great Price"),
                          avg_ref_gap = c(64.81843, 46.55721, 165.8753, 70.70626, 49.49427))
  

ggplot(avg_ref_gap, aes(x = reorder(book_title, -avg_ref_gap), y = avg_ref_gap, fill = book_title)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("#42558C", "#008000", "darkred", "#FFA540", "ivory")) +
  theme_minimal() +
  labs(
    title = "The New Testament References Jesus Christ most often, Old Testament the least", 
    x = "", 
    y = "Average # of Words Between References"
  ) +
  theme(legend.position = "none")

#---------------------------

ggplot(avg_ref_gap, aes(x = reorder(book_title, -avg_ref_gap), y = avg_ref_gap, fill = book_title)) +
  geom_bar(stat = "identity", color = "darkgrey", linewidth = 0.5) +
  scale_fill_manual(values = c("#42558C", "#558c42", "#8c4255", "#daa520", "ivory")) +
  theme_minimal() +
  labs(
    title = "The New Testament References Jesus Christ most often, Old Testament the least", 
    x = "", 
    y = "Average # of Words Between References"
  ) +
  theme(legend.position = "none")
