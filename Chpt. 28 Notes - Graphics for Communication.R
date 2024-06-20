#Chapter 28 - Graphics for Communication Notes

library(tidyverse)

ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "Fuel efficiency generally decreases with engine size",
       subtitle = "Two seaters (sports cars) are an exception because of their light weight",
       caption = "Data from fueleconomy.gov",
       x = "Engine displacement (L)",
       y = "Highway fuel economy (mpg)",
       color = "Car type")

# good titles summarize the findings, not just describe what the plot is
# axis and legend labels should be detailed and include units

# use math equations as labels instead? Replace "" with quote()

df <- tibble(
  x = runif(10),
  y = runif(10)
)

ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i == 1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )

#Create one plot on the fuel economy data with customised title, subtitle, caption, x, y, and colour labels.

ggplot(mpg, aes(x = fct_infreq(trans))) + 
  geom_bar(aes(fill = trans)) +
  labs(title = "Quantity of different types of transmissions",
       subtitle = "Majority of cars have auto(I4) transmissions",
       caption = "Data from fueleconomy.gov",
       x = "Transmission Type",
       y = "Count",
       fill = "Transmission type")

#The geom_smooth() is somewhat misleading because the hwy for large engines is skewed upwards due to the inclusion of lightweight sports cars with big engines. 
#Use your modelling tools to fit and display a better model.

mpg_no_2seater <-  
  mpg |> 
  filter(class != "2seater")

ggplot(mpg_no_2seater, aes(displ, hwy)) + 
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "Fuel efficiency decreases with engine size",
       subtitle = "Two seaters (sports cars) are removed from the data",
       caption = "Data from fueleconomy.gov",
       x = "Engine displacement (L)",
       y = "Highway fuel economy (mpg)",
       color = "Car type")

#annotations

library(ggrepel)

best_in_class <- mpg |> 
  group_by(class) |> 
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(color = class)) +
  geom_label(aes(label = model), data = best_in_class, nudge_y = 2, alpha = 0.5)

ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(color = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  geom_label_repel(aes(label = model), data = best_in_class)


#Use geom_text() with infinite positions to place text at the four corners of the plot.

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(x = Inf, y = Inf, label = "Top Right", vjust = "top", hjust = "right")) +
  geom_text(aes(x = Inf, y = -Inf, label = "Bottom Right", vjust = "bottom", hjust = "right")) +
  geom_text(aes(x = -Inf, y = Inf, label = "Top Left", vjust = "top", hjust = "left")) + 
  geom_text(aes(x = -Inf, y = -Inf, label = "Bottom Left", vjust = "bottom", hjust = "left")) +
  annotate(geom = "text",
               x = 7,
               y = 23,
               label = "this is a dot")

#How do labels with geom_text() interact with faceting?
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(color = class)) +
  facet_wrap(~class) +
  geom_text(aes(x = Inf, y = Inf, label = "Top Right", vjust = "top", hjust = "right")) +
  labs(title = "Fuel efficiency generally decreases with engine size",
       subtitle = "Two seaters (sports cars) are an exception because of their light weight",
       caption = "Data from fueleconomy.gov",
       x = "Engine displacement (L)",
       y = "Highway fuel economy (mpg)",
       color = "Car type")


#How can you add a label to a single facet? 
#How can you put a different label in each facet?

facet_labels <- data.frame(label = c("1st Graph", "2nd Graph", "3rd Graph"),
                       class = c("2seater", "compact", "midsize"),
                       x = c(3, 4, 6),
                       y = c(20, 30, 40))
  
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class) +
  geom_text(facet_labels, mapping = aes(x = x, y = y, label = label))


#scaling
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)

base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right") # the default
base + theme(legend.position = "none")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4)))
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'


#replacing a scale - log transform
ggplot(diamonds, aes(carat, price)) +
  geom_bin2d()

ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d()

ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() + 
  scale_x_log10() + 
  scale_y_log10()


#replacing color
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv)) +
  scale_colour_brewer(palette = "Set1")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_colour_brewer(palette = "Set1")


#predefined color mapping
presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, colour = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(values = c(Republican = "red", Democratic = "blue"))


#Why doesn’t the following code override the default scale?

library(hexbin)

df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)

ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_colour_gradient(low = "white", high = "red") +
  coord_fixed()

#the color changes when replacing scale_color_gradient with scale_fill_gradient
ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed()


#What is the first argument to every scale? How does it compare to labs()?
#labels and #breaks


#Change the display of the presidential terms by:
#Combining the two variants shown above.
#Improving the display of the y axis.
#Labelling each term with the name of the president.
#Adding informative plot labels.
#Placing breaks every 4 years (this is trickier than it seems!).

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, colour = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(values = c(Republican = "red", Democratic = "blue")) + 
  scale_x_date(breaks = as.Date(c(1950:2020)))