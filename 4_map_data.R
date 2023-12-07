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

### Codebooks
traffic_map_codebook <- tibble(variable = "geometry",
                               description = "A geometric object encoding the site of a crash.")
chi_map_codebook <- tibble(variable = c("STATEFP10",
                                        "ZCTA5CE10",
                                        "GEOID10",
                                        "CLASSFP10",
                                        "MTFCC10",
                                        "FUNCSTAT10",
                                        "ALAND10",
                                        "AWATER10",
                                        "INTPLTLAT10",
                                        "INTPTLON10",
                                        "PARTFLG10",
                                        "GEOMETRY"),
                           description = c("State FIPS (Federal Information Processing Standards) code.",
                                           "ZIP Code Tabulation Area (ZCTA) code.",
                                           "Unique identifier for a geographic entity.",
                                           "Classification code for the type of geographic entity.",
                                           "MAF/TIGER Feature Class Code, used in the U.S. Census Bureau's MAF/TIGER database.",
                                           "Functional status of the geographic entity (e.g., 'S' for statistical).",
                                           "Land area.",
                                           "Water area.",
                                           "Latitude of the internal point.",
                                           "Longitude of the internal point.",
                                           "Participation flag indicating whether the geographic entity is part of another entity.",
                                           "Attribute related to the spatial representation of the geographic entity.")
                           )
write_rds(chi_map_codebook, "data/chi_map_codebook.rds")
write_rds(traffic_map_codebook, "data/traffic_map_codebook.rds")
#Reference for creating interactive map
#https://map-rfun.library.duke.edu/01_georeference
