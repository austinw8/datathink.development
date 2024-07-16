#Task 19

library(USA.state.boundaries)
library(ggrepel)
library(sf)
library(ggplot2)
library(tidyverse)
library(maps)
library(data.table)
library(sp)
library(USAboundariesData)
library(usa)
library(stars)
library(ggrepel)
devtools::install_github("ropensci/USAboundariesData")


#counties of North Carolina
nc <- read_sf(system.file("shape/nc.shp", package = "sf"), 
              quiet = TRUE,  
              stringsAsFactors = FALSE
)
states <- sf::st_as_sf(map("state", plot = FALSE, fill = TRUE))
plot(nc$geometry)

ggplot() +
  geom_sf(aes(fill = AREA), data = nc, colour = "white")

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = nc)

ggplot() +
  geom_sf(data = nc) +
  annotate("point", x = -76, y = 35.1, color = "red", size = 4)

ggplot() +
  geom_sf(data = nc) +
  annotate("point", x = -80, y = 35, color = "red", size = 4) +
  coord_sf(xlim = c(-80.3, -79.8), ylim = c(34.8, 35.3))

ggplot() +
  geom_sf(data = states) +
  coord_sf(crs = st_crs(102003))

n <- nc$geometry %>% map_int(length)
table(n)

interesting <- nc$geometry[n == 3][[1]]
plot(interesting)

st_is_longlat(nc)
st_crs(nc)

devtools::install_github("tidyverse/ggplot2")
?geom_sf

#---------------------------------------

sf_points <- sf::st_read(
  dsn = "C:/Users/austi/OneDrive/Desktop/R_practice/Data Sets/elementary_schools_2324/elementary_schools_2324.shp"
)

sf_polygons <- sf::st_read(dsn = "C:/Users/austi/OneDrive/Desktop/R_practice/Data Sets/elementary_boundaries_2324 (1)/elementary_boundaries_2324.shp")

sf_points
sf_polygons

#Transform to longitude and latitude

st_crs(sf_points)

sf_points <- st_transform(x = sf_points, crs = "NAD83")
sf_polygons <- st_transform(x = sf_polygons, crs = "NAD83")

#grab data from an sf object
class(sf_points)
head(x = sf_points, n = 3)

#computing geometry features
st_area(sf_points)
st_area(sf_polygons)
sf_polygons$area <- st_area(sf_polygons)


#plotting shapes
plot(x = sf_polygons)
plot(x = sf_points)

plot(x = sf_polygons[,"area"],
     reset = FALSE)
plot(
  x = sf_points["geometry"],
  add = TRUE,
  pch = 19,
  col = "black"
)

#----------------------
states_1840 <- us_states("1840-03-12")
plot(st_geometry(states_1840))
title("U.S. State Boundaries on March 3, 1840")

states_contemporary <- us_states()
plot(st_geometry(states_contemporary))
title("Comtemporary U.S. State Boundaries")

#---------------------
dtm_harv <- read_stars("../data/neon-geospatial-data/harv/harv_dtmcrop.tif")
dtm_harv

ggplot() +
  geom_stars(data = dtm_harv) +
  scale_fill_viridis_c()

plots_harv <- st_read("../data/neon-geospatial-data/HARV/harv_plots.shp")
boundary_harv <- st_read("../data/neon-geospatial-data/HARV/harv_boundary.shp")
harv_soils <- st_read("../data/neon-geospatial-data/HARV/harv_soils.shp")

ggplot() +
  geom_sf(data = boundary_harv) +
  geom_sf(data = plots_harv, mapping = aes(color = plot_type))

ggplot() +
  geom_sf(data = plots_harv, mapping = aes(color = plot_type)) +
  geom_sf(data = boundary_harv, alpha = 0.5)

ggplot() +
  geom_sf(data = harv_soils, mapping = aes(fill = TYPE_)) +
  scale_fill_viridis_d()

ggplot() +
  geom_sf(data = harv_soils) +
  facet_wrap(~TYPE_)


# Task Assignment ---------------------------------------------------------

capitals_sf <-  us.cities |> 
  filter(capital != 0, 
         !(name %in% c("Honolulu HI", "Juneau AK"))) |> 
  st_as_sf(coords = c("long", "lat"), crs = "NAD83") 
  #mutate(pop_per_mil = pop / 1000)

capitals_coord <- us.cities |> 
  filter(capital != 0, 
         !(name %in% c("Honolulu HI", "Juneau AK"))) |> 
  mutate(long = abs(long),
         lat = abs(lat))

county_data <- st_read("../data/spatial_idaho/shp/County-AK-HI-Moved-USA-Map.shp")
idaho_data <- county_data |> 
  filter(StateName == "Idaho")

ggplot() +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = idaho_data, fill = NA) +
  geom_sf(data = capitals_sf, aes(geometry = geometry, size = pop_per_mil), color = "blue", alpha = 0.6) +
  geom_sf_label(data = capitals_sf, aes(geometry = geometry, label = name), size = 3, nudge_x = 0.5, nudge_y = -0.75) +
  #geom_label_repel(data = capitals_coord, aes(x = lat, y = long, label = name), size = 2)) +
  theme_minimal() +
  labs(
    title = "Population of U.S. Capitals",
    x = "",
    y = "", 
    size = "Population \n(1,000)"
  )



top_1_city <- us.cities |> 
  group_by(country.etc) |> 
  slice_max(order_by = pop, n = 1) |> 
  ungroup() |> 
  st_as_sf(coords = c("long", "lat"), crs = "NAD83") |> 
  filter(country.etc != c("AK", "HI")) |> 
  mutate(pop_per_mil = pop / 1000) |> 
  mutate(name = str_sub(name, 1, -4))

top_2_city <- us.cities |> 
  group_by(country.etc) |> 
  slice_max(order_by = pop, n = 2) |> 
  slice_min(order_by = pop, n = 1) |> 
  ungroup() |> 
  st_as_sf(coords = c("long", "lat"), crs = "NAD83") |> 
  filter(country.etc != c("AK", "HI")) |> 
  mutate(pop_per_mil = pop / 1000)


top_3_city <- us.cities |> 
  group_by(country.etc) |> 
  slice_max(order_by = pop, n = 3) |> 
  slice_min(order_by = pop, n = 1) |> 
  ungroup() |> 
  st_as_sf(coords = c("long", "lat"), crs = "NAD83") |> 
  filter(country.etc != c("AK", "HI")) |> 
  mutate(pop_per_mil = pop / 1000)


ggplot() +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = idaho_data, fill = NA) +
  geom_sf(data = top_3_city, aes(geometry = geometry, size = pop_per_mil), color = "lightblue") +
  geom_sf(data = top_2_city, aes(geometry = geometry, size = pop_per_mil), color = "blue") +
  geom_sf(data = top_1_city, aes(geometry = geometry, size = pop_per_mil), color = "darkblue") +
  geom_sf_label(data = top_1_city, aes(geometry = geometry, label = name), size = 2.2, nudge_x = 0.5, nudge_y = -0.75) +
  labs(
    title = "Population of U.S. Capitals",
    x = "",
    y = "", 
    size = "Population \n(1,000)"
  )


ggplot() +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = idaho_data, fill = NA) +
  geom_sf(data = top_3_city, aes(geometry = geometry, size = pop_per_mil), color = "lightblue") +
  geom_sf(data = top_2_city, aes(geometry = geometry, size = pop_per_mil), color = "blue") +
  geom_sf(data = top_1_city, aes(geometry = geometry, size = pop_per_mil), color = "darkblue") +
  geom_label_repel(data = top_1_city, aes(geometry = geometry, label = name), 
                   size = 2, nudge_x = 0.5, nudge_y = -0.75, 
                   stat = "sf_coordinates") +
  labs(
    title = "Most Populated U.S. Cities",
    x = "",
    y = "", 
    size = "Population \n(1,000)"
  )
