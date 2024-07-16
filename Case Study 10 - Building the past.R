#Case Study 10: Building the Past


# Setup -------------------------------------------------------------------

devtools::install_github("hathawayj/buildings")
library(rnaturalearth)
library(tidyverse)
library(devtools)
library(usethis)
library(buildings)
library(USAboundaries)
library(stringr)
library(sf)


# Data Input --------------------------------------------------------------

world_states <- ne_states()

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) |> 
  mutate(ID = str_to_title(ID)) |> 
  rename(name = "ID")

ggplot() +
  geom_sf(data = states)

us_states <- world_states |> 
  filter(geonunit == "United States of America", 
         !woe_name %in% c("Alaska", "Hawaii"))

permits_us <- buildings::permits |> 
  rename(FIPS.state = state,
         FIPS.county = county)

permits_us_single_fam <- buildings::permits |> 
  rename(FIPS.state = state,
         FIPS.county = county) |> 
  filter(variable == "Single Family")

permits_id <- buildings::permits |> 
  filter(StateAbbr == "ID") |> 
  rename(FIPS.state = state,
         FIPS.county = county)

permits_id_single_fam <- buildings::permits |> 
  filter(StateAbbr == "ID",
         variable == "Single Family") |> 
  rename(FIPS.state = state,
         FIPS.county = county)

us_county_data <- st_read("../data/spatial_idaho/shp/County-AK-HI-Moved-USA-Map.shp") |> 
  select(StateFIPSN, CntyFIPSN, StateName, CntyName, geometry) |> 
  rename(FIPS.county = "CntyFIPSN",
         FIPS.state = "StateFIPSN")

id_county_data <- us_county_data |> 
  filter(StateName == "Idaho") |> 
  st_transform(us_county_data, crs = "NAD27")

merged_id_data <- permits_id_single_fam |> 
  left_join(id_county_data, by = c("FIPS.county", "FIPS.state"))

merged_us_data <- permits_us_single_fam |> 
  left_join(us_county_data, by = c("FIPS.county", "FIPS.state"))


# COUNTS ----------------------------------------------------------------

us_permit_count <- st_as_sf(merged_us_data) |> 
  mutate(StateName = str_to_title(StateName)) |> 
  group_by(StateName) |> 
  rename(name = "StateName") |> 
  summarise(permit_count = n()) |> 
  st_join(states, by = "name", left = TRUE) |> 
  rename(geometry = "geom")

ggplot() +
  geom_sf(data = us_permit_count, aes(geometry = geometry, fill = permit_count))


# Percent change from 1980 to 2020 ----------------------------------------

us_permit_count_diff <- merged_us_data |> 
  mutate(StateName = str_to_title(StateName)) |> 
  group_by(StateName, year) |> 
  rename(name = "StateName") |> 
  summarise(permit_count = n()) |> 
  filter(year %in% c("1980", "2010")) |> 
  pivot_wider(names_from = year, values_from = permit_count) |> 
  mutate(percent_change = ((`2010` - `1980`) / `1980`) * 100) |> 
  left_join(states, by = "name") |> 
  rename(geometry = "geom") |> 
  na.omit()

ggplot() +
  geom_sf(data = us_permit_count_diff, aes(geometry = geometry, fill = percent_change)) +
  scale_fill_viridis_b()

#------------------
us_permit_count_diff_2 <- merged_us_data |> 
  mutate(StateName = str_to_title(StateName)) |> 
  summarise(permit_count = n(), .by = c("StateName", "year")) |> 
  rename(name = "StateName") |> 
  filter(year %in% c("1980", "2010")) |> 
  pivot_wider(names_from = year, values_from = permit_count) |> 
  mutate(percent_change = ((`2010` - `1980`) / `1980`) * 100) |> 
  left_join(states, by = "name") |> 
  rename(geometry = "geom") |> 
  na.omit()

# Percent change from 80s to 10s ------------------------------------------

us_permit_count_dec_diff <- merged_us_data |> 
  mutate(StateName = str_to_title(StateName)) |> 
  group_by(StateName, year) |> 
  rename(name = "StateName") |> 
  summarise(permit_count = n()) |> 
  filter(!year %in% c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999")) |> 
  pivot_wider(names_from = year, values_from = permit_count) |> 
  mutate(sum_1980s = sum(`1980`, `1981`, `1982`, `1983`, `1984`, `1985`, `1986`, `1987`, `1988`, `1989`, `1990`),
         sum_2000s = sum(`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`),
         percent_change = ((sum_2000s - sum_1980s) / sum_1980s) * 100
         ) |> 
  left_join(states, by = "name") |> 
  rename(geometry = "geom") |> 
  na.omit()

ggplot() +
  geom_sf(data = us_permit_count_dec_diff, aes(geometry = geometry, fill = percent_change)) +
  scale_fill_viridis_b()


# Percent change from 80-83 to 07-10 --------------------------------------

us_permit_count_3yr_diff <- merged_us_data |> 
  mutate(StateName = str_to_title(StateName)) |> 
  group_by(StateName, year) |> 
  rename(name = "StateName") |> 
  summarise(permit_count = n()) |> 
  filter(!year %in% c("1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006")) |> 
  pivot_wider(names_from = year, values_from = permit_count) |> 
  mutate(sum_1980s = sum(`1980`, `1981`, `1982`, `1983`),
         sum_2000s = sum(`2007`, `2008`, `2009`, `2010`),
         percent_change = ((sum_2000s - sum_1980s) / sum_1980s) * 100
  ) |> 
  left_join(states, by = "name") |> 
  rename(geometry = "geom") |> 
  na.omit()

ggplot() +
  geom_sf(data = us_permit_count_3yr_diff, aes(geometry = geometry, fill = percent_change)) +
  scale_fill_viridis_b()


# VALUES ----------------------------------------------------------------

#not adjusted for inflation
us_permit_values <- merged_us_data |> 
  mutate(StateName = str_to_title(StateName)) |> 
  rename(name = "StateName") |> 
  select(!StateAbbr, !CntyName) |> 
  group_by(name, year, FIPS.state) |> 
  summarise(total_value = sum(value, na.rm = TRUE)) |> 
  filter(year %in% c("1980", "2010")) |> 
  pivot_wider(names_from = year, values_from = total_value) |> 
  mutate(percent_change = ((`2010` - `1980`) / `1980`)) |> 
  left_join(states, by = "name") |> 
  na.omit() |> 
  rename(geometry = "geom")

ggplot() +
  geom_sf(data = us_permit_values, aes(geometry = geometry, fill = percent_change)) +
  scale_fill_gradient(low = "#FF2500", high = "#FFFC74", labels = scales::percent_format()) +
  theme_void() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank()
  ) +
  labs(
    title = "Single Family Housing Price Decline in US from 1980 to 2010",
    subtitle = "",
    fill = "Percent Chance of Price"
  )


#adjusted for inflation
us_permit_values_adj <- merged_us_data |> 
  mutate(StateName = str_to_title(StateName)) |> 
  rename(name = "StateName") |> 
  select(!StateAbbr, !CntyName) |> 
  group_by(name, year, FIPS.state) |> 
  summarise(total_value = sum(value, na.rm = TRUE)) |> 
  filter(year %in% c("1980", "2010")) |> 
  pivot_wider(names_from = year, values_from = total_value) |> 
  mutate(`1980_adj` = (`1980` * 2.785),
         percent_change = ((`2010` - `1980_adj`) / `1980_adj`)) |> 
  left_join(states, by = "name") |> 
  na.omit() |> 
  rename(geometry = "geom")

ggplot() +
  geom_sf(data = us_permit_values_adj, aes(geometry = geometry, fill = percent_change)) +
  scale_fill_gradient(low = "#FF2500", high = "#FFFC74", labels = scales::percent_format()) + 
  theme_void() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank()
  ) +
  labs(
    title = "Single Family Housing Price Decline in US from 1980 to 2010",
    subtitle = "Adjusted for inflation according to BLS",
    fill = "Percent Chance of Price"
  ) +
  coord_sf(crs = "NAD27")

