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

### Create delay and delay_bins variable
traffic <- traffic |> 
  mutate(delay = (date_police_notified - crash_date)/ddays(1),
         delay_bins = cut(delay, 
                          breaks = c(0, 1, 7, 30, 365, 730, 1095, 1460, 1825, 2190, 2555, 2920, 3285),
                          labels = c("day", "week", "month", "year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years"),
                          ordered_result = T,
                          include.lowest = T)) 

### Subset injuries data
injuries <- 
  traffic |> 
  select(crash_record_id, contains("injuries")) |> 
  pivot_longer(cols = contains("injuries"),
               names_to = "injury_type",
               names_pattern = "injuries_(.*)",
               values_to = "value")

### Store cleaned data
write_rds(traffic, "data/traffic_data.rds")
write_rds(injuries, "data/injuries.rds")

### Write codebooks
traffic_codebook <- tibble(variable = c("crash_record_id",
                                        "rd_no",
                                        "crash_date_est_i",
                                        "crash_date",
                                        "posted_speed_limit",
                                        "traffic_control_device",
                                        "device_condition",
                                        "weather_condition",
                                        "lighting_condition",
                                        "first_crash_type",
                                        "trafficway_type",
                                        "lane_cnt",
                                        "alignment",
                                        "roadway_surface_cond",
                                        "road_defect",
                                        "report_type",
                                        "crash_type",
                                        "intersection_related_i",
                                        "not_right_of_way_i",
                                        "hit_and_run_i",
                                        "damage",
                                        "date_police_notified",
                                        "prim_contributory_cause",
                                        "sec_contributory_cause",
                                        "street_no",
                                        "street_direction",
                                        "street_name",
                                        "beat_of_occurrence",
                                        "photos_taken_i",
                                        "statements_taken_i",
                                        "dooring_i",
                                        "work_zone_i",
                                        "work_zone_type",
                                        "workers_present_i",
                                        "num_units",
                                        "most_severe_injury",
                                        "injuries_total",
                                        "injuries_fatal",
                                        "injuries_incapacitating",
                                        "injuries_non_incapacitatin",
                                        "injuries_reported_not_evident",
                                        "injuries_no_indication",
                                        "injuries_unknown",
                                        "crash_hour",
                                        "crash_day_of_week",
                                        "crash_month",
                                        "latitude",
                                        "longitude",
                                        "location",
                                        "delay",
                                        "delay_bins"
                                        ),
                            description = c("This number can be used to link to the same crash in the Vehicles and People datasets. This number also serves as a unique ID in this dataset.",
                                            "Chicago Police Department report number",
                                            "Crash date estimated by desk officer or reporting party (only used in cases where crash is reported at police station days after the crash)",
                                            "Date and time of crash as entered by the reporting officer",
                                            "Posted speed limit, as determined by reporting officer",
                                            "Traffic control device present at crash location, as determined by reporting officer",
                                            "Condition of traffic control device, as determined by reporting officer",
                                            "Weather condition at time of crash, as determined by reporting officer",
                                            "Light condition at time of crash, as determined by reporting officer",
                                            "Type of first collision in crash",
                                            "Trafficway type, as determined by reporting officer",
                                            "Total number of through lanes in either direction, excluding turn lanes, as determined by reporting officer (0 = intersection)",
                                            "Street alignment at crash location, as determined by reporting officer",
                                            "Road surface condition, as determined by reporting officer",
                                            "Road defects, as determined by reporting officer",
                                            "Administrative report type (at scene, at desk, amended)",
                                            "A general severity classification for the crash. Can be either Injury and/or Tow Due to Crash or No Injury / Drive Away",
                                            "A field observation by the police officer whether an intersection played a role in the crash. Does not represent whether or not the crash occurred within the intersection.",
                                            "Whether the crash begun or first contact was made outside of the public right-of-way.",
                                            "Crash did/did not involve a driver who caused the crash and fled the scene without exchanging information and/or rendering aid",
                                            "A field observation of estimated damage.",
                                            "Calendar date on which police were notified of the crash",
                                            "The factor which was most significant in causing the crash, as determined by officer judgment",
                                            "The factor which was second most significant in causing the crash, as determined by officer judgment",
                                            "Street address number of crash location, as determined by reporting officer",
                                            "Street address direction (N,E,S,W) of crash location, as determined by reporting officer",
                                            "Street address name of crash location, as determined by reporting officer",
                                            "Chicago Police Department Beat ID. Boundaries available at https://data.cityofchicago.org/d/aerh-rz74",
                                            "Whether the Chicago Police Department took photos at the location of the crash",
                                            "Whether statements were taken from unit(s) involved in crash",
                                            "Whether crash involved a motor vehicle occupant opening a door into the travel path of a bicyclist, causing a crash",
                                            "Whether the crash occurred in an active work zone",
                                            "The type of work zone, if any",
                                            "Whether construction workers were present in an active work zone at crash location",
                                            "Number of units involved in the crash. A unit can be a motor vehicle, a pedestrian, a bicyclist, or another non-passenger roadway user. Each unit represents a mode of traffic with an independent trajectory.",
                                            "Most severe injury sustained by any person involved in the crash",
                                            "Total persons sustaining fatal, incapacitating, non-incapacitating, and possible injuries as determined by the reporting officer",
                                            "Total persons sustaining fatal injuries in the crash",
                                            "Total persons sustaining incapacitating/serious injuries in the crash as determined by the reporting officer. Any injury other than fatal injury, which prevents the injured person from walking, driving, or normally continuing the activities they were capable of performing before the injury occurred. Includes severe lacerations, broken limbs, skull or chest injuries, and abdominal injuries.",
                                            "Total persons sustaining non-incapacitating injuries in the crash as determined by the reporting officer. Any injury, other than fatal or incapacitating injury, which is evident to observers at the scene of the crash. Includes lump on head, abrasions, bruises, and minor lacerations.",
                                            "Total persons sustaining possible injuries in the crash as determined by the reporting officer. Includes momentary unconsciousness, claims of injuries not evident, limping, complaint of pain, nausea, and hysteria.",
                                            "Total persons sustaining no injuries in the crash as determined by the reporting officer",
                                            "Total persons for whom injuries sustained, if any, are unknown",
                                            "The hour of the day component of CRASH_DATE.",
                                            "The day of the week component of CRASH_DATE. Sunday=1",
                                            "The month component of CRASH_DATE.",
                                            "The latitude of the crash location, as determined by reporting officer, as derived from the reported address of crash",
                                            "The longitude of the crash location, as determined by reporting officer, as derived from the reported address of crash",
                                            "The crash location, as determined by reporting officer, as derived from the reported address of crash, in a column type that allows for mapping and other geographic analysis in the data portal software",
                                            "The amount of time surpassed between crash_date and date_police_notified",
                                            "Delay variable as measured in the unit of days."))
injuries_codebook <- tibble(variable = c("crash_record_id", "type", "value"),
                            description = c("Unique identifier for each traffic crash", "type or severity of injury", "the number of each type of injury associated with the respective crash"))

write_rds(traffic_codebook, "data/traffic_data_codebook.rds")
write_rds(injuries_codebook, "data/injuries_codebook.rds")
  