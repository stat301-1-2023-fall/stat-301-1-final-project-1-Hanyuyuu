### Read in data
library(tidyverse)
traffic <- read_csv("data/raw/traffic_crashes.csv") |>
  janitor::clean_names() 

### Convert appropriate variables to factor
convert_to_factor <- c("traffic_control_device", 
                       "device_condition", 
                       "weather_condition", 
                       "lighting_condition",
                       "first_crash_type", 
                       "trafficway_type",
                       "alignment",
                       "roadway_surface_cond",
                       "road_defect",
                       "report_type",
                       "crash_type",
                       "damage",
                       "prim_contributory_cause",
                       "sec_contributory_cause",
                       "work_zone_type",
                       "most_severe_injury"
)

for(var in convert_to_factor){
  traffic <- traffic |> 
    mutate({{var}} := as.factor(!!sym(var)))
}

### Convert appropriate variables to boolean
convert_to_bool <- traffic |> select((ends_with("_i"))) |> colnames()

for(var in convert_to_bool){
  traffic <- traffic |> 
    mutate({{var}} := if_else(!!sym(var) == "Y", TRUE, FALSE, missing = NA))
}

### Convert appropriate variables to datetime
convert_to_datetime <- c("crash_date", "date_police_notified")

for(var in convert_to_datetime){
  traffic <- traffic |> 
    mutate({{var}} := parse_date_time(!!sym(var), orders = "%m/%d/%Y %I:%M:%S %p"))
}

### Create delay variable
traffic <- traffic |> 
  mutate(delay = (date_police_notified - crash_date)/ddays(1),
         delay_bins = cut(delay, 
                          breaks = c(0, 1, 7, 30, 365, 730, 1095, 1460, 1825, 2190, 2555, 2920, 3285),
                          labels = c("day", "week", "month", "year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years"),
                          ordered_result = T,
                          include.lowest = T)) 
### Store cleaned data
write_rds(traffic, "data/traffic_data.rds")
traffic <- read_rds("data/traffic_data.rds")