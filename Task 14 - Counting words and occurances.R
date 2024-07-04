#Task 14

library(purrr)
library(tidyverse)
library(stringr)
library(stringi)
library(janitor)
library(ggplot2)
library(dplyr)
library(tidyr)

scriptures <- read_csv("C:/Users/austi/OneDrive/Desktop/R_practice/Data Sets/lds-scriptures.csv")


# Data Prep ---------------------------------------------------------------

scriptures <- scriptures |> 
  mutate(word_count = str_count(scripture_text),
         jesus_count = str_count(scripture_text, "Jesus"),
         scripture_text = str_to_lower(scripture_text),
         came_to_pass_count = str_count(scripture_text, 'for it came to pass|and it came to pass'))

old_testament <- scriptures |> 
  filter(volume_title == "Old Testament")

new_testament <- scriptures |> 
  filter(volume_title == "New Testament")

book_of_mormon <- scriptures |> 
  filter(volume_title == "Book of Mormon")

doctrine_covenants <- scriptures |> 
  filter(volume_title == "Doctrine & Covenants")

pearl_great_price <- scriptures |> 
  filter(volume_title == "Pearl of Great Price")


#What is the average verse length (number of words) in the New Testament compared to the Book of Mormon?

avg_verse_len_bm <- book_of_mormon |> summarise(avg_verse_length = mean(word_count))
avg_verse_len_nt <- new_testament |> summarise(avg_verse_length = mean(word_count))
avg_verse_len_ot <- old_testament |> summarise(avg_verse_length = mean(word_count))
avg_verse_len_dc <- doctrine_covenants |> summarise(avg_verse_length = mean(word_count))
avg_verse_len_pgp <- pearl_great_price |> summarise(avg_verse_length = mean(word_count))

#How often is the word Jesus in the New Testament compared to the Book of Mormon?

jesus_count_nt <- new_testament |> summarise(total_jesus_count = sum(jesus_count))
jesus_count_bm <- book_of_mormon |> summarise(total_jesus_count = sum(jesus_count))


#How does the word count distribution by verse look for each book in the Book of Mormon?

ggplot(book_of_mormon, aes(x = word_count)) +
  geom_histogram(aes(y = ..density..), binwidth = 25, fill = "lightblue", color = "black", alpha = 0.6) +
  geom_density(alpha = 0.5, fill = "red") +
  theme_minimal() +
  #facet_wrap(~book_title) +
  labs(
    title = "Book of Mormon Word Count Distribution by Verse",
    x = "Word Count",
    y = "Density / Frequency"
  )

ggplot(book_of_mormon, aes(x = word_count, fill = book_title)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(
    title = "Book of Mormon Word Count Distribution by Verse",
    x = "Word Count",
    y = "Density"
  ) +
  theme(legend.position = "bottom")

ggplot(new_testament, aes(x = word_count)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "lightblue", color = "black", alpha = 0.6) +
  geom_density(alpha = 0.5, fill = "red") +
  theme_minimal() +
  #facet_wrap(~book_title) +
  labs(
    title = "New Testament Word Count Distribution by Verse",
    x = "Word Count",
    y = "Density / Frequency"
  )


#Frequency of the Phrase "And It Came to Pass" in the Book of Mormon and Its Impact

total_bom_word_count <- sum(book_of_mormon$word_count)

book_of_mormon_3 <-  book_of_mormon |> 
  mutate(text_without_came_to_pass = str_replace_all(scripture_text, 'for it came to pass|and it came to pass', "")) |> 
  mutate(new_word_count = str_count(text_without_came_to_pass)) |> 
  select(-volume_id, -book_id, -chapter_id, -verse_id, -volume_long_title, -book_long_title, -book_subtitle, -volume_short_title, -volume_lds_url, -book_lds_url)

book_of_mormon_word_count <- book_of_mormon_3 |> 
  group_by(book_title) |> 
  summarise(word_count = sum(word_count),
            Count_without_came_to_pass = sum(new_word_count)) |> 
  #adorn_totals("row") |> 
  mutate(percentage_retained = (Count_without_came_to_pass / word_count) * 100) |> 
  mutate(percentage_lost = ((word_count - Count_without_came_to_pass) / word_count) * 100)

#--------------
book_order <- c("1 Nephi", "2 Nephi", "Jacob", "Enos", "Jarom", "Omni", "Words of Mormon", "Mosiah", "Alma", "Helaman", "3 Nephi", "4 Nephi", "Mormon", "Ether", "Moroni")

book_of_mormon_word_count_2 <- book_of_mormon_word_count |> 
  pivot_longer(cols = c("word_count", "Count_without_came_to_pass"), 
               names_to = "count_type", 
               values_to = "word_count") |> 
  mutate(count_type = fct_recode(count_type,
                                 "WITHOUT phrase" = "Count_without_came_to_pass",
                                 "WITH phrase" = "word_count")) |> 
  mutate(word_count_per_1000 = word_count / 1000)
book_of_mormon_word_count_2$book_title <- factor(book_of_mormon_word_count_2$book_title, levels = book_order)
book_of_mormon_word_count_2

book_of_mormon_word_count_2$count_type <- factor(book_of_mormon_word_count_2$count_type, levels = c("WITH phrase", "WITHOUT phrase"))


ggplot(book_of_mormon_word_count_2, aes(x = book_title, y = word_count_per_1000, fill = count_type)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  theme_minimal() +
  scale_fill_manual(values = c("#024B7A", "#44B7C2")) +
  labs(
    title = "Removing the phrase \"and it came to pass\" by book",
    subtitle = "Doing so only removes 1.23% of the BOM word count",
    x = "Book Title", 
    y = "Word Count (per 1,000)", 
    fill = ""
  ) +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


#-------------

book_of_mormon_word_count_3 <- 
  book_of_mormon_3 |> 
  group_by(volume_title) |> 
  summarise(word_count = sum(word_count),
            Count_without_came_to_pass = sum(new_word_count)
  ) |> 
  pivot_longer(cols = c("word_count", "Count_without_came_to_pass"), 
               names_to = "count_type", 
               values_to = "word_count") |> 
  mutate(count_type = fct_recode(count_type,
                                 "WITHOUT phrase" = "Count_without_came_to_pass",
                                 "WITH phrase" = "word_count")) |> 
  mutate(word_count_per_1000 = word_count / 1000)
  
book_of_mormon_word_count_3$count_type <- factor(book_of_mormon_word_count_3$count_type, levels = c("WITH phrase", "WITHOUT phrase"))


ggplot(book_of_mormon_word_count_3, aes(x = volume_title, y = word_count_per_1000, fill = count_type)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  theme_minimal() +
  scale_fill_manual(values = c("#024B7A", "#44B7C2")) +
  labs(
    title = "Removing the phrase \"and it came to pass\" from the Book of Mormon",
    subtitle = "Doing so only removes 1.23% of the BOM word count",
    x = "", 
    y = "Word Count (per 1,000)", 
    fill = ""
  ) +
  scale_y_continuous(n.breaks = 8)





