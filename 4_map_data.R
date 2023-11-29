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

#Reference for creating interactive map
#https://map-rfun.library.duke.edu/01_georeference











beats <- read_csv("data/raw/PoliceBeatDec2012.csv") |> janitor::clean_names() |> 
  mutate(beat_num := as.double(beat_num))
joined <- traffic |> 
  select(crash_record_id, beat_of_occurrence) |> 
  drop_na() |> 
  left_join(y = beats, join_by(beat_of_occurrence == beat_num)) 
ggplot(beats) +
  geom_polygon()

#ggplot(data = zipcodedf, aes(x = long, y = lat, group = group, fill = continuous_var)) +
#  geom_polygon() +

  joined |> 
  group_by(beat_of_occurrence) |> 
  mutate(count = n()) |> 
  ggplot(aes(fill = count)) +
  geom_sf()
