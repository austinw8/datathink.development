#Task 20 - Getting in SHP

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library(sf)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(USAboundaries)
library(maps)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot() +
  geom_sf(data = world)

world_states <- nworldworld_states <- ne_states()
ggplot() +
  geom_sf(data = world_states, fill = NA)

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

theme_set(theme_bw())

water <- st_read("../data/spatial_idaho/water/hyd250.shp")
wells <- st_read("../data/spatial_idaho/wells/wells.shp")
dams <- st_read("../data/spatial_idaho/idaho_dams/dam_safety.shp")
all_county_data <- st_read("../data/spatial_idaho/shp/County-AK-HI-Moved-USA-Map.shp")
id_water <- water |> 
  filter(STATE == "IDAH")

ggplot() +
  geom_sf(data = all_county_data, fill = NA)

rivers <- water |> 
  filter(FEAT_NAME == c("Snake River", "Henrys Fork"))

large_wells <- wells |> 
  filter(Production >= 5000)

large_dams <- dams |> 
  filter(SurfaceAre > 50)

id_county_data <- county_data |> 
  filter(StateName == "Idaho") |> 
  st_transform(all_county_data, crs = "NAD27")

ggplot() +
  geom_sf(data = all_county_data, fill = NA)

ggplot() +
  geom_sf(data = id_county_data, fill = NA) +
  geom_sf(data = rivers, color = "blue", size = 1) +
  geom_sf(data = large_wells, color = "black") +
  geom_sf(data = large_dams, color = "orange")

#-------------------------------------------

wells_2 <- large_wells |> 
  mutate(type = "Wells")

dams_2 <- large_dams |> 
  mutate(type = "Dams")

combined_data <- bind_rows(wells_2, dams_2)

ggplot() +
  geom_sf(data = id_county_data, fill = NA) +
  geom_sf(data = rivers, color = "blue", size = 1) +
  geom_sf(data = combined_data, aes(color = type), size = 1.75) +
  scale_color_manual(values = c("Wells" = "black", "Dams" = "orange"), name = "Feature") +
  labs(
    title = "Idaho water sources", 
    subtitle = "Wells produce > 5,000 gal, dams > 50 acres SA"
  )
  
  
  