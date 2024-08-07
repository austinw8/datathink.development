#Chpt 15 Notes - Factors

#factors are for categorical variables! 

library(tidyverse)
library(ggplot2)

x1 <- c("Dec", "Apr", "Jan", "Mar")
sort(x1)


month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1
sort(y1)

y2 <- factor(x2, levels = month_levels)
y2

csv <- "
month, value
Jan,12
Feb, 56
Mar,12" 

df <- read_csv(csv, col_types = cols(month = col_factor(month_levels)))
df$month


# General Social Survey ---------------------------------------------------
gss_cat <- gss_cat

?gss_cat

gss_cat |> 
  count(race)

#most common factor operations? (1) changing level order and (2) changing level values

gss_cat |> 
  count(rincome)

ggplot(gss_cat, aes(x = fct_infreq(rincome)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
gss_cat |> 
  count(relig)

gss_cat |> 
  count(partyid)

gss_cat |> 
  count(denom)

table(gss_cat$relig, gss_cat$denom)

denom_spread <- 
  gss_cat |> 
  count(relig, denom) |> 
  spread(denom, n)


# Modifying Factor Order --------------------------------------------------

relig_summary <- gss_cat |> 
  group_by(relig) |> 
  summarise(
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
ggplot(relig_summary, aes(x = tvhours, y = relig)) +
  geom_point()

ggplot(relig_summary, aes(x = tvhours, y = fct_reorder(relig, tvhours))) +
  geom_point()

#this below is the same plot as the one above, but reorder is moved out of aes()
relig_summary |> 
  mutate(
    relig = fct_reorder(relig, tvhours)
  ) |> 
  ggplot(aes(x = tvhours, y = relig)) +
  geom_point()

#reordering her eisn't a good idea! Because income already has a natural order
rincome_summary <- gss_cat |> 
  group_by(rincome) |> 
  summarise(
    age = mean(age, na.rm = TRUE),
    n = n()
  )

ggplot(rincome_summary, aes(x = age, y = fct_reorder(rincome, age))) +
  geom_point()

ggplot(rincome_summary, aes(x = age, y = rincome)) +
  geom_point()
#move "not applicable" to front with other special levels
ggplot(rincome_summary, aes(x = age, y = fct_relevel(rincome, "Not applicable"))) +
  geom_point()


#------------
by_age <- gss_cat |> 
  filter(!is.na(age)) |> 
  count(age, marital) |> 
  group_by(age) |> 
  mutate(prop = n / sum(n))

ggplot(by_age, aes(x = age, y = prop, color = marital)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1")

#rearrange to make colors at end of line on far right line up with legend
#fct_reorder2() eorders factor by the y values associated with the largest x values
ggplot(by_age, aes(x = age, y = prop, color = fct_reorder2(marital, age, prop))) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "marital")

gss_cat |> 
  mutate(marital = marital |> fct_infreq() |> fct_rev()) |> 
  ggplot(aes(x = marital)) +
  geom_bar()

ggplot(tv_hours, aes(x = tvhours)) +
  geom_bar()


# Modifying Factor Levels -------------------------------------------------

gss_cat |> count(partyid)

gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat"
    )
  ) |>
  count(partyid)

#group multiple old levels to same old level
gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat",
                         "Other"                 = "No answer",
                         "Other"                 = "Don't know",
                         "Other"                 = "Other party"
    )
  ) |> 
  count(partyid)

#collapse lots of levels
gss_cat |> 
  mutate(
    partyid = fct_collapse(partyid,
                           "other" = c("No answer", "Don't know", "Other party"),
                           "rep" = c("Strong republican", "Not str republican"),
                           "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
                           "dem" = c("Not str democrat", "Strong democrat")
                           )) |> 
      count(partyid)

#Exercises

party_time <- 
  gss_cat |> 
  mutate(
    partyid = fct_collapse(partyid,
                           "other" = c("No answer", "Don't know", "Other party"),
                           "rep" = c("Strong republican", "Not str republican"),
                           "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
                           "dem" = c("Not str democrat", "Strong democrat")
    )) |> 
  count(partyid, year) |> 
  group_by(partyid, year)

ggplot(party_time, aes(x = year, y = n, color = partyid)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("black", "red", "green", "blue")) +
  scale_x_continuous(n.breaks = 20)




