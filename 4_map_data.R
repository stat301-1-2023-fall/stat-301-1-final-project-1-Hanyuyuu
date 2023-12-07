### Load packages + data

library(tidyverse)
library(tigris)
library(sf)
library(mapview)

traffic_data <- read_rds("data/traffic_data.rds")

chi_map <- zctas(starts_with = c("606", "60706", "60176", "60018", "60707"), 
             state = "IL", 
             year = 2010)

### Find crashes with complete long+lat data and convert to shapefile

traffic_map <- 
  traffic_data |> 
  filter(!is.na(longitude) & !is.na(latitude), 
         date_police_notified >= "2015-01-01",
         most_severe_injury == "FATAL") |>
  select(longitude, latitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

### Plot crashes over map of Chicago Zip codes

ggplot() +
  geom_sf(data = chi_map) +
  geom_sf(data = traffic_map, col = "cornflowerblue", shape = 20) +
  theme_minimal()

### Interactive map

mapview(traffic_map)

### Store data
write_rds(chi_map, "data/chi_map.rds")
write_rds(traffic_map, "data/traffic_map.rds")

#Reference for creating interactive map
#https://map-rfun.library.duke.edu/01_georeference
