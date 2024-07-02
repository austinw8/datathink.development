#Case Study 6 - The Collapse of Construction in Idaho

# Setup -------------------------------------------------------------------

library(dplyr)
library(tidyverse)
library(devtools)
library(stringr)
library(ggplot2)
library(scales)
library(forcats)

devtools::install_github("hathawayj/buildings")

climate_zone <- as_tibble(buildings::climate_zone_fips)
buildings <- as_tibble(buildings::buildings0809)
restaurants <- as_tibble(buildings::restaurants)
permits <- as_tibble(buildings::permits)

#predefined groups
not_restaurants <- c("development","Food preperation center", "Food Services center","bakery","Grocery","conceession","Cafeteria", "lunchroom","school","facility"," hall ")
standalone_retail <- c("Wine","Spirits","Liquor","Convenience","drugstore","Flying J", "Rite Aid ","walgreens ","Love's Travel ")
full_service_type <- c("grains of montana", "souper salad", "chadders", "texas roadhouse", "applebee's", "wings", "Tuscanos", "Ristorante","mexican","pizza ","steakhouse","grill","buffet","tavern","bar","waffle","italian","steak house")
quick_service_type <- c("chronic tacos", "panda express", "Mcdonald", "dutch", "Jimmy johns", "starbucks", "subway", "sonic", "coffee","java","Donut","Doughnut","burger", "smashburger", "Ice Cream ","custard ","sandwich ","fast food","bagel")
quick_service_names <- restaurants$Restaurant[restaurants$Type %in% c("coffee","Ice Cream","Fast Food")]
full_service_names <- restaurants$Restaurant[restaurants$Type %in% c("Pizza","Casual Dining","Fast Casual")]


# Data Prep ---------------------------------------------------------------

buildings2 <- 
  buildings |> 
  left_join(climate_zone, by = c("FIPS.county", "FIPS.state")) |> 
  filter(Type == "Food_Beverage_Service") |> 
  mutate(ProjectTitle = str_to_lower(str_trim(ProjectTitle)),
         category = case_when(
           str_detect(ProjectTitle, str_c(tolower(not_restaurants), collapse = "|")) ~ "Not Restaurant",
           str_detect(ProjectTitle, str_c(tolower(standalone_retail), collapse = "|")) ~ "Standalone Retail",
           str_detect(ProjectTitle, str_c(tolower(full_service_type), collapse = "|")) ~ "Full Service Restaurant",
           str_detect(ProjectTitle, str_c(tolower(quick_service_type), collapse = "|")) ~ "Quick Service Restaurant",
           str_detect(ProjectTitle, str_c(tolower(not_restaurants), collapse = "|")) ~ "Not Restaurant",
           SqFt >= 4000 ~ "Full Service Restaurant",
           SqFt < 4000 & str_detect(ProjectTitle, "(?i)NEW") ~ "Quick Service Restaurant",
           TRUE ~ "Unknown")) |> 
  relocate(category, .after = ProjectTitle) |> 
  mutate(county = str_replace(AreaName, " County", ""),
         value_million = (Value1000 / 1000), ) |> 
  select(Year, Month, ProjectTitle, category, City, value_million, county)


# Answering Questions -----------------------------------------------------

#How did full-service restaurant construction compare to quick service restaurant construction across county and years?

buildings2 |> 
  filter(category %in% c("Full Service Restaurant", "Quick Service Restaurant")) |> 
  group_by(county, Year, category) |> 
  summarise(building_value = sum(value_million, na.rm = TRUE)) |> 
  ggplot(buildings2, mapping = aes(x = Year, y = building_value, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "m")) +
  theme_bw() +
  scale_fill_manual(values=c("#D53E4F", "darkred")) +
  labs(
    title = "Construction decreased significantly for Idaho full-service type restaurants between `08-`09",
    subtitle = "Quick service restaurants (fast food) sees a slight increase", 
    x = "",
    y = "Amount spent on construction",
    fill = "Restaurant Type")


buildings2 |> 
  filter(category %in% c("Full Service Restaurant", "Quick Service Restaurant")) |> 
  group_by(county, Year, category) |> 
  summarise(building_value = sum(value_million, na.rm = TRUE)) |> 
  ggplot(buildings2, mapping = aes(x = Year, y = building_value, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "m")) +
  theme_bw() +
  scale_fill_manual(values=c("#D53E4F", "darkred")) +
  facet_wrap(~county) +
  labs(
    title = "No consistency in restaurant construction growth/decline across counties",
    x = "",
    y = "Amount spent on construction",
    fill = "Restaurant Type")


#How did restaurant construction fare compare to the other commercial construction in Idaho?

#Categorizing commercial construction

restaurants <- c("restaurant", "grains of montana", "souper salad", "chadders", "texas roadhouse", "applebee's", "wings", "Tuscanos", "Ristorante","mexican","pizza ","steakhouse","grill","buffet","tavern","bar","waffle","italian","steak house", "chronic tacos", "panda express", "Mcdonald", "dutch", "Jimmy johns", "starbucks", "subway", "sonic", "coffee","java","Donut","Doughnut","burger", "smashburger", "Ice Cream ","custard ","sandwich ","fast food","bagel", "coffee","Ice Cream","Fast Food", "Pizza","Casual Dining","Fast Casual")
retail <- c("bakery", "concession", "conseession", "grocery", "fred meyer", "albertson", "shopping center", "shopping", "nail salon", "mall", "car wash", "retail", "Wine","Spirits","Liquor","Convenience","drugstore","pharmacy", "Flying J", "Rite Aid ","walgreen","Love's Travel ")
wholesale <- c("costco", "wholesale", "warehouse", "winco", "supercenter")
airport <- c("air", "terminal", "hanger", "hangar")
medical <- c("clinic", "medicine", "healing", "medi", "dentist", "assisted living", "senior center", "detoxification", "dental", "eye clinic", "hospice", "paramedic", "medical", "doctor", "hospital", "physical therapy", "weight loss", "nurse", "nursing", "wellness")
hospitality <- c("motel", "hotel", "holiday inn", "marriot")
education <- c("library", "academy", "campus", "learning", "education", "classroom", "school", "college", "university", "research", "educational")
recreation <- c("hockey", "casino", "recreation", "sport", "sports", "swim", "golf", "park", "playground", "pool", "gym", "theater", "theatre", "arena", "zoo")
housing <- c("home", "apts", "town home", "apartment", "townhome", "townhouse", "condominium", "married student housing", "alpine village")
religious <- c("faith", "church", "catholic", "stake center")
financial <- c("bank", "credit union")
industrial <- c("gas", "gas station", "storage", "auto", "lube shop", "railhead", "factory", "industrial", "parking", "garage", "Food preperation center", "Food Services center")
government <- c("county", "national guard", "army", "veteran", "fire station")

commercial_categories <- 
  buildings |> 
  left_join(climate_zone, by = c("FIPS.county", "FIPS.state")) |> 
  mutate(ProjectTitle = str_to_lower(str_trim(ProjectTitle)),
         category = case_when(
           str_detect(ProjectTitle, str_c(tolower(restaurants), collapse = "|")) ~ "Restaurant",
           str_detect(ProjectTitle, str_c(tolower(retail), collapse = "|")) ~ "Retail",
           str_detect(ProjectTitle, str_c(tolower(wholesale), collapse = "|")) ~ "Wholesale",
           str_detect(ProjectTitle, str_c(tolower(airport), collapse = "|")) ~ "Airport",
           str_detect(ProjectTitle, str_c(tolower(medical), collapse = "|")) ~ "Medical",
           str_detect(ProjectTitle, str_c(tolower(education), collapse = "|")) ~ "Education",
           str_detect(ProjectTitle, str_c(tolower(hospitality), collapse = "|")) ~ "Hospitality",
           str_detect(ProjectTitle, str_c(tolower(recreation), collapse = "|")) ~ "Recreation",
           str_detect(ProjectTitle, str_c(tolower(housing), collapse = "|")) ~ "Housing",
           str_detect(ProjectTitle, str_c(tolower(religious), collapse = "|")) ~ "Religious",
           str_detect(ProjectTitle, str_c(tolower(financial), collapse = "|")) ~ "Financial",
           str_detect(ProjectTitle, str_c(tolower(industrial), collapse = "|")) ~ "Industrial",
           str_detect(ProjectTitle, str_c(tolower(government), collapse = "|")) ~ "Government",
           TRUE ~ "Unknown")) |> 
  relocate(category, .after = ProjectTitle) |> 
  select(-Census.FIPS, -FIPS.county, -FIPS.state) |> 
  filter(category != "Unknown")
  
ggplot(commercial_categories, mapping = aes(x = fct_infreq(category), fill = category)) +
  geom_bar() +
  theme_bw() +
  scale_fill_manual(values=c("grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "darkred", "grey", "grey")) +
  theme(legend.position = "none") +
  labs(
    title = "Restaurants are the 4th most common commercial construction project type", 
    subtitle = "Between 2008-2009", 
    x = "Construction category",
    y = "Number of projects"
  )


ggplot(commercial_categories, mapping = aes(x = fct_infreq(category), fill = category)) +
  geom_bar() +
  theme_bw() +
  scale_fill_manual(values=c("grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "darkred", "grey", "grey")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Restaurants are the 4th most common commercial construction project type", 
    subtitle = "Between 2008-2009", 
    x = "Construction category",
    y = "Number of projects"
  )







#--------------------

commercial_categories_2 <- 
  commercial_categories |> 
  group_by(category) |> 
  mutate(value_million = (Value1000 / 1000)) |> 
  summarise(building_value_mil = sum(value_million, na.rm = TRUE))

ggplot(commercial_categories_2, aes(x = reorder(category, -building_value_mil), y = building_value_mil, fill = category)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "m")) +
  scale_fill_manual(values = c("grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "darkred", "grey", "grey")) +
  theme(legend.position = "none") +
  labs(
    title = "Restaurants rank as the 8th highest category in commercial construction spending", 
    subtitle = "Between 2008-2009",
    x = "Construction category",
    y = "Amount spent"
  )

#Which county in Idaho spent the most on fast food construction each year?

buildings2 |> 
  group_by(county, category, Year) |> 
  summarise(value_million = sum(value_million, na.rm = TRUE)) |>
  filter(category == "Quick Service Restaurant") |> 
  ggplot(buildings2, mapping = aes(x = reorder(county, -value_million), y = value_million, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "m")) +
  scale_fill_manual(values = c("darkred", "grey")) +
  theme_bw() +
  labs(
    title = "Ada county spent the most on fast food construction in Idaho", 
    subtitle = "In both 2008 and 2009", 
    x = "County", 
    y = "Spending on fast food construction"
  )


#In that county how did other commercial construction compare?

commercial_categories_2 <- 
  buildings |> 
  left_join(climate_zone, by = c("FIPS.county", "FIPS.state")) |> 
  mutate(ProjectTitle = str_to_lower(str_trim(ProjectTitle)),
         category = case_when(
           str_detect(ProjectTitle, str_c(tolower(restaurants), collapse = "|")) ~ "Restaurant",
           str_detect(ProjectTitle, str_c(tolower(retail), collapse = "|")) ~ "Retail",
           str_detect(ProjectTitle, str_c(tolower(wholesale), collapse = "|")) ~ "Wholesale",
           str_detect(ProjectTitle, str_c(tolower(airport), collapse = "|")) ~ "Airport",
           str_detect(ProjectTitle, str_c(tolower(medical), collapse = "|")) ~ "Medical",
           str_detect(ProjectTitle, str_c(tolower(education), collapse = "|")) ~ "Education",
           str_detect(ProjectTitle, str_c(tolower(hospitality), collapse = "|")) ~ "Hospitality",
           str_detect(ProjectTitle, str_c(tolower(recreation), collapse = "|")) ~ "Recreation",
           str_detect(ProjectTitle, str_c(tolower(housing), collapse = "|")) ~ "Housing",
           str_detect(ProjectTitle, str_c(tolower(religious), collapse = "|")) ~ "Religious",
           str_detect(ProjectTitle, str_c(tolower(financial), collapse = "|")) ~ "Financial",
           str_detect(ProjectTitle, str_c(tolower(industrial), collapse = "|")) ~ "Industrial",
           str_detect(ProjectTitle, str_c(tolower(government), collapse = "|")) ~ "Government",
           TRUE ~ "Unknown")) |> 
  relocate(category, .after = ProjectTitle) |> 
  mutate(county = str_replace(AreaName, " County", "")) |> 
  select(-Census.FIPS, -FIPS.county, -FIPS.state) |> 
  filter(category != "Unknown", county == "Ada") |> 
  mutate(value_million = (Value1000 / 1000)) |> 
  group_by(category) |> 
  summarise(building_value_mil = sum(value_million, na.rm = TRUE))
  

ggplot(commercial_categories_2, aes(x = reorder(category, -building_value_mil), y = building_value_mil, fill = category)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "m")) +
  scale_fill_manual(values = c("grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "darkred", "grey", "grey")) +
  theme(legend.position = "none") +
  labs(
    title = "Restaurant in Ada County, ID remain the 8th highest category in commercial construction spending",
    subtitle = "Between 2008-2009",
    x = "Construction Category", 
    y = "Amount spent"
  )
  




