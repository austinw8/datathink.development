library(tidyverse)
library(ggplot2)
library(dplyr)

#distribution of categorical variable - use a bar chart
ggplot(diamonds) +
  geom_bar(mapping = aes(x = cut))
diamonds |> 
  count(cut)

#distribution of continuous data - use a histogram
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
diamonds |> 
  count(cut_width(carat, 0.5))

#if overlaying multiple histograms in same plot, use geom_freqpoly()
ggplot(diamonds, aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)
#GOOD QUESTIONS TO ASK
#Why are there more diamonds at whole carats and common fractions of carats?
#Why are there more diamonds slightly to the right of each peak than there are slightly to the left of each peak?
#Why are there no diamonds bigger than 3 carats?


ggplot(faithful, aes(x = eruptions)) +
  geom_histogram(binwidth = 0.25)


#outliers
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
#outliers caught! 

unusual <- 
  diamonds |> 
  filter(y < 3 | y > 20) |> 
  select(price, x, y, z) |> 
  arrange(y)

#good practice to repeat analysis with and w/o outliers


# Exercises ---------------------------------------------------------------

no_outliers <- 
  diamonds |> 
  filter(y > 3 & y < 20) |> 
  filter(z < 20)

ggplot(no_outliers) +
  geom_histogram(mapping = aes(x = x), fill = "blue", alpha = 0.5) +
  geom_histogram(mapping = aes(x = y), fill = "yellow", alpha = 0.5) +
  geom_histogram(mapping = aes(x = z), fill = "green", alpha = 0.5) 
  #coord_cartesian(ylim = c(0, 50))

#x and y are length and width, z must be depth. Diamonds are wider than they are tall

ggplot(diamonds, aes(x = price)) + 
  geom_histogram(binwidth = 100) +
  coord_cartesian(xlim = c(0, 10000))

#it's unusual there are extremely few diamonds priced around $1500
#There is also an interesting dip in price around $3700 and rise around $4000

ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.005) +
  coord_cartesian(xlim = c(0.95, 1.05))

diamonds |> 
  filter(carat == 0.99) |> 
  nrow()
diamonds |> 
  filter(carat == 1.0) |> 
  nrow()

#there are 23 diamonds with 0.9 carat but 1558 diamonds with 1.0 carat
#this might be due to size recording practices at the company, or perhaps a reflection of their precision, since 1 carat diamonds might sell better than 0.99 carat ones

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y)) +
  ylim(0, 50) +
  xlim(0, 60)

#ylim() will delete and not display any values outside the limits, while coord() will simply crop the graph shorter
#without specifying, binwidth resets to a default value
#trying to zoom so only half a bar shows (with xlim()) cuts off the entire bar

#replace unusual values with missing values (creates a modified copy)
diamonds2 <- 
  diamonds |> 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(diamonds2, aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)


#need the info from missing values? 
library(nycflights13)

flights |> 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100, 
    sched_dep_time = sched_hour + sched_min / 60
) |> 
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(mapping = aes(color = cancelled), binwidth = 1/4)


#what happens to missing values in a histogram? 
ggplot(flights, aes(x = arr_time)) +
  geom_histogram(binwidth = 50)
#missing values are ignored, excuded from the calculation

#what happens to missing values in a bar chart? 
ggplot(flights, aes(x = carrier)) +
  geom_bar()
#missing values treated as a separate category



# Covariation -------------------------------------------------------------

ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
#WTF is going on with this plot? Please explain density plots... 

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()


ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

#reorder based on median value of hwy
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()


# Example Exercises -------------------------------------------------------

#1
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

flights |> 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) |> 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_boxplot(mapping = aes(x = cancelled, y = sched_dep_time, colour = cancelled))

#2
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  labs(
    title = "Diamond Price vs. Carat"
  )

ggplot(diamonds) +
  geom_boxplot(mapping = aes(x = reorder(color, price, FUN = median), y = price)) +
  labs(
    title = "Diamond Price vs. Color"
  )

ggplot(diamonds) +
  geom_boxplot(mapping = aes(x = reorder(clarity, price, FUN = median), y = price)) +
  labs(
    title = "Diamond Price vs. Clarity"
  )

#the most important variable for predicting the price of diamond is carat

ggplot(diamonds, aes(x = cut, y = carat)) + 
  geom_boxplot() +
  coord_flip()

library(ggstance)
ggplot(diamonds, aes(x = carat, y = cut)) + 
  geom_boxploth()

library(lvplot)
ggplot(diamonds, aes(x = cut, y = price)) + 
  geom_boxplot()

ggplot(diamonds, aes(x = cut, y = price)) + 
  geom_lv()

ggplot(diamonds, aes(x = cut, y = price)) + 
  geom_violin()

library(ggbeeswarm)
ggplot(diamonds, aes(x = cut, y = price)) + 
  geom_jitter()

ggplot(diamonds, aes(x = cut, y = price)) + 
  geom_jitter()


# Categorical vs. Categorical ---------------------------------------------

ggplot(diamonds) + 
  geom_count(mapping = aes(x = cut, y = color))

diamonds |> 
  count(color, cut) |> 
  ggplot(mapping = aes(x = cut, y = color)) +
  geom_tile(mapping = aes(fill = n))

library(heatmaply)
?heatmaply

ggplot(flights, aes(x = month, y = dest)) +
  geom_tile(mapping = aes(fill = arr_delay))

ggplot(diamonds) + 
  geom_count(mapping = aes(x = cut, y = color))

ggplot(diamonds) + 
  geom_count(mapping = aes(x = color, y = cut))

ggplot(diamonds) +
  geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(diamonds) +
  geom_hex(mapping = aes(x = carat, y = price))

#bin one continuous variable, then plot cont. vs. categorical
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))


library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))
