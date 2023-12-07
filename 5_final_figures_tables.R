### 5 - Finalized Figures and Tables #####

library(tidyverse)
traffic <- read_rds("data/traffic_data.rds")
injuries <- read_rds("data/injuries.rds")

### At a Glance - Most Severe Injury Distribution
most_severe_injury_distribution <- ggplot(traffic, aes(most_severe_injury)) +
  geom_bar() +
  scale_x_discrete(limits = rev(c("NO INDICATION OF INJURY",
                                  "NONINCAPACITATING INJURY",
                                  "REPORTED, NOT EVIDENT",
                                  "INCAPACITATING INJURY",
                                  "FATAL",
                                  NA))) +
  coord_flip() +
  labs(y = "Count",
       x = "Most Severe Injury")

### At a Glance - Device, Roadway Surface, Lighting, and Weather Conditions

weather <- ggplot(traffic, aes(weather_condition)) +
  geom_bar() +
  scale_x_discrete(limits = rev(c("CLEAR",                    
                                  "RAIN",                      
                                  "UNKNOWN",                   
                                  "SNOW",                      
                                  "CLOUDY/OVERCAST",           
                                  "OTHER",                      
                                  "FREEZING RAIN/DRIZZLE",      
                                  "FOG/SMOKE/HAZE",             
                                  "SLEET/HAIL",                  
                                  "BLOWING SNOW",                
                                  "SEVERE CROSS WIND GATE",     
                                  "BLOWING SAND, SOIL, DIRT"))) +
  coord_flip() +
  labs(y = "Count",
       x = NULL,
       title = "a) Weather Condition") +
  theme(axis.text.y = element_text(angle = 15, vjust = 0, hjust = 1))

# Lighting Condition

lighting <- ggplot(traffic, aes(x = lighting_condition)) +
  geom_bar() +
  scale_x_discrete(limits = rev(c("DAYLIGHT",              
                                  "DARKNESS, LIGHTED ROAD",
                                  "DARKNESS",              
                                  "UNKNOWN",               
                                  "DUSK",                  
                                  "DAWN" ))) +
  coord_flip() +
  labs(y = "Count",
       x = NULL,
       title = "b) Lighting Condition") +
  theme(axis.text.y = element_text(angle = 15, vjust = 0, hjust = 1))

# Roadway conditions
road <- ggplot(traffic, aes(roadway_surface_cond)) +
  geom_bar() +
  scale_x_discrete(limits = rev(c("DRY",                 
                                  "WET",                 
                                  "UNKNOWN",             
                                  "SNOW OR SLUSH",       
                                  "ICE",                 
                                  "OTHER",               
                                  "SAND, MUD, DIRT"))) +
  coord_flip() +
  labs(y = "Count",
       x = NULL,
       title = "c) Road Surface\nCondition") +
  theme(axis.text.y = element_text(angle = 15, vjust = 0, hjust = 1))

# Device Condition

device <- ggplot(traffic, aes(device_condition)) +
  geom_bar() +
  scale_x_discrete(limits = rev(c("NO CONTROLS",
                                  "FUNCTIONING PROPERLY",
                                  "UNKNOWN",
                                  "OTHER",
                                  "FUNCTIONING IMPROPERLY",
                                  "NOT FUNCTIONING",
                                  "WORN REFLECTIVE MATERIAL",
                                  "MISSING"))) +
  coord_flip() +
  labs(y = "Count",
       x = NULL,
       title = "d) Traffic Control\nDevice Condition") +
  theme(axis.text.y = element_text(angle = 15, vjust = 0, hjust = 1))

### At a Glance - First Crash Type Distribution
first_crash <- ggplot(traffic, aes(first_crash_type)) +
  geom_bar() +
  scale_x_discrete(limits = rev(c("PARKED MOTOR VEHICLE",        
                              "REAR END",                    
                              "SIDESWIPE SAME DIRECTION",    
                              "TURNING",                     
                              "ANGLE",                       
                              "FIXED OBJECT",                
                              "PEDESTRIAN",                  
                              "PEDALCYCLIST",               
                              "SIDESWIPE OPPOSITE DIRECTION",
                              "OTHER OBJECT",                
                              "REAR TO FRONT",              
                              "HEAD ON",                     
                              "REAR TO SIDE",                
                              "OTHER NONCOLLISION",         
                              "REAR TO REAR",                
                              "ANIMAL",            
                              "OVERTURNED",                  
                              "TRAIN"))) +
  coord_flip() +
  labs(x = "First Crash Type",
       y = "Count")

### At a Glance - Primary and Secondary Contributory Causes Tables
prim_cause <- traffic |> 
  count(prim_contributory_cause) |> 
  arrange(desc(n)) |> 
  knitr::kable() |> 
  kableExtra::kable_styling(full_width = FALSE, position = "float_left")

sec_cause <- traffic |> 
  count(sec_contributory_cause) |> 
  arrange(desc(n)) |> 
  knitr::kable() |> 
  kableExtra::kable_styling(full_width = FALSE, position = "left")

### At a Glance - Crash day of week, hour, and month
crash_day <- traffic |> 
  select(crash_day_of_week) |> 
  mutate(crash_day_of_week := wday(crash_day_of_week, label = T, abbr = T)) |> 
  ggplot(aes(crash_day_of_week)) +
  geom_bar() +
  labs(y = "Count",
       x = "Day of the Week")

crash_hour <- ggplot(traffic, aes(crash_hour)) +
  geom_bar() +
  labs(y = "Count",
       x = "Hour of Crash (24 Hour Cycle)")

crash_month <- traffic |> 
  select(crash_month) |> 
  mutate(crash_month := month(crash_month, label = T)) |> 
  ggplot(aes(crash_month)) +
  geom_bar() +
  labs(y = "Count",
       x = "Month of Crash")

### At a Glance - Injuries
injuries |> 
  filter(injury_type != "total") |> 
  group_by(injury_type) |> 
  summarize(sum = sum(value, na.rm = T)) |> 
  arrange(desc(sum)) |> 
  ggplot(aes(injury_type, sum)) +
  geom_col() +
  scale_x_discrete(limits = rev(c("no_indication", 
                                  "non_incapacitating",
                                  "reported_not_evident",
                                  "incapacitating",
                                  "fatal",
                                  "unknown"))) +
  coord_flip() +
  labs(y = "Sum",
       x = "Injury Type")

### Looking deeper - What causes of crashes produce the greatest number of injuries?
cause_v_injury <- traffic |> 
  filter(injuries_total > 0) |> 
  mutate(prim_contributory_cause =  
           fct_collapse(prim_contributory_cause, 
                        "Distraction" = c("DISTRACTION - FROM INSIDE VEHICLE",                                               
                                          "DISTRACTION - FROM OUTSIDE VEHICLE",                                              
                                          "DISTRACTION - OTHER ELECTRONIC DEVICE (NAVIGATION DEVICE, DVD PLAYER, ETC.)",  
                                          "TEXTING",
                                          "CELL PHONE USE OTHER THAN TEXTING"), 
                        "Disregarding signs and markings" = c("DISREGARDING OTHER TRAFFIC SIGNS",
                                                              "DISREGARDING ROAD MARKINGS",
                                                              "DISREGARDING STOP SIGN",
                                                              "DISREGARDING TRAFFIC SIGNALS",
                                                              "DISREGARDING YIELD SIGN"),
                        "Improper Driving" = c("EXCEEDING AUTHORIZED SPEED LIMIT",                                                
                                               "EXCEEDING SAFE SPEED FOR CONDITIONS",                                             
                                               "FAILING TO REDUCE SPEED TO AVOID CRASH",                                          
                                               "FAILING TO YIELD RIGHT-OF-WAY",                                                   
                                               "FOLLOWING TOO CLOSELY",
                                               "IMPROPER TURNING/NO SIGNAL",
                                               "IMPROPER OVERTAKING/PASSING",
                                               "IMPROPER LANE USAGE",
                                               "IMPROPER BACKING",
                                               "DRIVING SKILLS/KNOWLEDGE/EXPERIENCE",
                                               "DRIVING ON WRONG SIDE/WRONG WAY"),
                        "Influence of Substances" = c("HAD BEEN DRINKING (USE WHEN ARREST IS NOT MADE)" ,
                                                      "UNDER THE INFLUENCE OF ALCOHOL/DRUGS (USE WHEN ARREST IS EFFECTED)"),
                        "No Information" = c("UNABLE TO DETERMINE", "NOT APPLICABLE"),
                        "Construction-related" = c("ROAD ENGINEERING/SURFACE/MARKING DEFECTS", "ROAD CONSTRUCTION/MAINTENANCE"),
                        "Bus-related" = c("RELATED TO BUS STOP",
                                          "PASSING STOPPED SCHOOL BUS"),
                        "Driver Condition" = c("OPERATING VEHICLE IN ERRATIC, RECKLESS, CARELESS, NEGLIGENT OR AGGRESSIVE MANNER",
                                               "PHYSICAL CONDITION OF DRIVER"),
                        "Advancing on Red Light" = c("MOTORCYCLE ADVANCING LEGALLY ON RED LIGHT",
                                                     "BICYCLE ADVANCING LEGALLY ON RED LIGHT"),
                        "Response to External Obstruction" = c("ANIMAL", 
                                                               "EVASIVE ACTION DUE TO ANIMAL, OBJECT, NONMOTORIST", 
                                                               "OBSTRUCTED CROSSWALKS"),
                        "Weather" = "WEATHER",
                        "Vision Obscured" = "VISION OBSCURED (SIGNS, TREE LIMBS, BUILDINGS, ETC.)",
                        "Turning Right on Red" = "TURNING RIGHT ON RED",
                        "Vehicle Condition" = "EQUIPMENT - VEHICLE CONDITION")
         
  ) |> 
  ggplot(aes(prim_contributory_cause, injuries_total)) +
  geom_point(alpha = 0.5, 
             position = position_jitter(width = 0.3, height = .5)) +
  scale_x_discrete(limits = rev(c("Improper Driving", 
                                  "Disregarding signs and markings",
                                  "Driver Condition",
                                  "Distraction",
                                  "Influence of Substances",
                                  "Weather",
                                  "Vehicle Condition",
                                  "Vision Obscured",
                                  "Response to External Obstruction",
                                  "Construction-related",
                                  "Turning Right on Red",
                                  "Bus-related",
                                  "Advancing on Red Light",
                                  "No Information"))) +
  coord_flip() +
  labs(x = "Primary Contributory Cause",
       y = "Total Number of Injuries")

ggsave("looking_deeper_cause_v_injury_final.png",
       cause_v_injury,
       path = "./plots/final",
       width = 6)


### Looking deeper - How do crash severity and type correlate?
crash_type_v_most_severe_injury <- traffic %>% 
  filter(most_severe_injury %in% c("NO INDICATION OF INJURY", 
                                   "INCAPACITATING INJURY", 
                                   "FATAL")) |> 
  count(most_severe_injury, first_crash_type) |>   
  mutate(prop = n / sum(n), .by = most_severe_injury) |> 
  ggplot(mapping = aes(x = most_severe_injury, y = first_crash_type)) +
  geom_tile(mapping = aes(fill = prop)) +
  scale_x_discrete(limits = c("NO INDICATION OF INJURY",
                              "INCAPACITATING INJURY", 
                              "FATAL")) +
  scale_y_discrete(limits = rev(c("PARKED MOTOR VEHICLE",
                                  "REAR END", 
                                  "SIDESWIPE SAME DIRECTION",
                                  "TURNING", 
                                  "ANGLE", 
                                  "FIXED OBJECT",
                                  "SIDESWIPE OPPOSITE DIRECTION",
                                  "REAR TO FRONT",
                                  "OTHER OBJECT",
                                  "HEAD ON",
                                  "REAR TO SIDE",
                                  "PEDALCYCLIST",
                                  "PEDESTRIAN",
                                  "OTHER NONCOLLISION",
                                  "REAR TO REAR",
                                  "ANIMAL",
                                  "OVERTURNED",
                                  "TRAIN"))) +
  theme(axis.text.x = element_text(angle = 15, vjust = 1, hjust = 1)) +
  labs(y = "First Crash Type",
       x = "Most Severe Injury",
       fill = "Proportion of Crash Type\nwithin Injury Category")

ggsave("looking_deeper_crash_type_v_most_severe_injury_final.png",
       crash_type_v_most_severe_injury,
       path = "plots/final",
       width = 6, 
       height = 4)

### Looking deeper - Winter crash conditions
winter_crashes <- traffic |> 
  mutate(prim_contributory_cause = fct_collapse(prim_contributory_cause,
                                                "other" = c("ANIMAL",
                                                            "BICYCLE ADVANCING LEGALLY ON RED LIGHT", 
                                                            "CELL PHONE USE OTHER THAN TEXTING"                                               
                                                            , "DISREGARDING OTHER TRAFFIC SIGNS"                                                
                                                            , "DISREGARDING ROAD MARKINGS"                                                      
                                                            , "DISREGARDING STOP SIGN"                                                          
                                                            , "DISREGARDING TRAFFIC SIGNALS"                                                    
                                                            , "DISREGARDING YIELD SIGN"                                                         
                                                            , "DISTRACTION - FROM INSIDE VEHICLE"                                               
                                                            , "DISTRACTION - FROM OUTSIDE VEHICLE"                                              
                                                            , "DISTRACTION - OTHER ELECTRONIC DEVICE (NAVIGATION DEVICE, DVD PLAYER, ETC.)"     
                                                            , "DRIVING ON WRONG SIDE/WRONG WAY"                                                 
                                                            , "DRIVING SKILLS/KNOWLEDGE/EXPERIENCE"                                             
                                                            , "EVASIVE ACTION DUE TO ANIMAL, OBJECT, NONMOTORIST"                               
                                                            , "EXCEEDING AUTHORIZED SPEED LIMIT"                                                
                                                            , "EXCEEDING SAFE SPEED FOR CONDITIONS"                                             
                                                            , "FAILING TO REDUCE SPEED TO AVOID CRASH"                                          
                                                            , "FAILING TO YIELD RIGHT-OF-WAY"                                                   
                                                            , "FOLLOWING TOO CLOSELY"                                                           
                                                            , "HAD BEEN DRINKING (USE WHEN ARREST IS NOT MADE)"                                 
                                                            , "IMPROPER BACKING"                                                                
                                                            , "IMPROPER LANE USAGE"                                                             
                                                            , "IMPROPER OVERTAKING/PASSING"                                                     
                                                            , "IMPROPER TURNING/NO SIGNAL"                                                      
                                                            , "MOTORCYCLE ADVANCING LEGALLY ON RED LIGHT"                                       
                                                            , "NOT APPLICABLE"                                                                  
                                                            , "OBSTRUCTED CROSSWALKS"                                                           
                                                            , "OPERATING VEHICLE IN ERRATIC, RECKLESS, CARELESS, NEGLIGENT OR AGGRESSIVE MANNER"
                                                            , "PASSING STOPPED SCHOOL BUS"                                                      
                                                            , "PHYSICAL CONDITION OF DRIVER"                                                    
                                                            , "RELATED TO BUS STOP"                                                             
                                                            , "ROAD ENGINEERING/SURFACE/MARKING DEFECTS"                                        
                                                            , "TEXTING"                                                                         
                                                            , "TURNING RIGHT ON RED"                                                            
                                                            , "UNABLE TO DETERMINE"                                                             
                                                            , "UNDER THE INFLUENCE OF ALCOHOL/DRUGS (USE WHEN ARREST IS EFFECTED)")
  )) |> 
  ggplot(aes(crash_month)) +
  geom_bar(aes(fill = prim_contributory_cause)) +
  scale_x_discrete(limits = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "June", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")) +
  labs(x = "Crash Month",
       y = "Count",
       fill = "Primary Contributory Cause")

ggsave("looking_deeper_winter_crashes_final.png",
       winter_crashes,
       path = "plots/final",
       width = 10,
       height = 4)

### Looking deeper - Is there a difference in injury severity that is dependent upon weather? 

weather_severity_all <- traffic |> 
  filter(!(is.na(most_severe_injury))) |> 
  count(most_severe_injury, weather_condition) |> 
  mutate(prop = n / sum(n), .by = most_severe_injury) |> 
  ggplot(aes(most_severe_injury, weather_condition)) +
  geom_tile(mapping = aes(fill = prop)) 

ggsave("looking_deeper_weather_all_final.png",
       weather_severity_all,
       path = "plots/final")

weather_severity_other <- traffic |> 
  filter(!(is.na(most_severe_injury)),
         weather_condition != "CLEAR",
         weather_condition != "RAIN",
         weather_condition != "UNKNOWN",
         weather_condition != "SNOW",
         weather_condition != "CLOUDY/OVERCAST") |> 
  count(most_severe_injury, weather_condition) |> 
  mutate(prop = n / sum(n), .by = most_severe_injury) |> 
  ggplot(aes(most_severe_injury, weather_condition)) +
  geom_tile(mapping = aes(fill = prop)) 

ggsave("looking_deeper_weather_other_final.png",
       weather_severity_other,
       path = "plots/final")

### Looking deeper - Are there differences in cause between daytime and nighttime crashes?
night <- traffic |> 
  mutate(prim_contributory_cause =  
           fct_collapse(prim_contributory_cause, 
                        "Distraction" = c("DISTRACTION - FROM INSIDE VEHICLE",                                               
                                          "DISTRACTION - FROM OUTSIDE VEHICLE",                                              
                                          "DISTRACTION - OTHER ELECTRONIC DEVICE (NAVIGATION DEVICE, DVD PLAYER, ETC.)",  
                                          "TEXTING",
                                          "CELL PHONE USE OTHER THAN TEXTING"), 
                        "Disregarding signs and markings" = c("DISREGARDING OTHER TRAFFIC SIGNS",
                                                              "DISREGARDING ROAD MARKINGS",
                                                              "DISREGARDING STOP SIGN",
                                                              "DISREGARDING TRAFFIC SIGNALS",
                                                              "DISREGARDING YIELD SIGN"),
                        "Improper Driving" = c("EXCEEDING AUTHORIZED SPEED LIMIT",                                                
                                               "EXCEEDING SAFE SPEED FOR CONDITIONS",                                             
                                               "FAILING TO REDUCE SPEED TO AVOID CRASH",                                          
                                               "FAILING TO YIELD RIGHT-OF-WAY",                                                   
                                               "FOLLOWING TOO CLOSELY",
                                               "IMPROPER TURNING/NO SIGNAL",
                                               "IMPROPER OVERTAKING/PASSING",
                                               "IMPROPER LANE USAGE",
                                               "IMPROPER BACKING",
                                               "DRIVING SKILLS/KNOWLEDGE/EXPERIENCE",
                                               "DRIVING ON WRONG SIDE/WRONG WAY"),
                        "Influence of Substances" = c("HAD BEEN DRINKING (USE WHEN ARREST IS NOT MADE)" ,
                                                      "UNDER THE INFLUENCE OF ALCOHOL/DRUGS (USE WHEN ARREST IS EFFECTED)"),
                        "No Information" = c("UNABLE TO DETERMINE", "NOT APPLICABLE"),
                        "Construction-related" = c("ROAD ENGINEERING/SURFACE/MARKING DEFECTS", "ROAD CONSTRUCTION/MAINTENANCE"),
                        "Bus-related" = c("RELATED TO BUS STOP",
                                          "PASSING STOPPED SCHOOL BUS"),
                        "Driver Condition" = c("OPERATING VEHICLE IN ERRATIC, RECKLESS, CARELESS, NEGLIGENT OR AGGRESSIVE MANNER",
                                               "PHYSICAL CONDITION OF DRIVER"),
                        "Advancing on Red Light" = c("MOTORCYCLE ADVANCING LEGALLY ON RED LIGHT",
                                                     "BICYCLE ADVANCING LEGALLY ON RED LIGHT"),
                        "Response to External Obstruction" = c("ANIMAL", 
                                                               "EVASIVE ACTION DUE TO ANIMAL, OBJECT, NONMOTORIST", 
                                                               "OBSTRUCTED CROSSWALKS"),
                        "Weather" = "WEATHER",
                        "Vision Obscured" = "VISION OBSCURED (SIGNS, TREE LIMBS, BUILDINGS, ETC.)",
                        "Turning Right on Red" = "TURNING RIGHT ON RED",
                        "Vehicle Condition" = "EQUIPMENT - VEHICLE CONDITION")
         
  ) |> 
  filter(prim_contributory_cause %in% c("Distraction",
                                        "Improper Driving",
                                        "Influence of Substances",
                                        "Driver Condition",
                                        "Construction-related",
                                        "Weather",
                                        "Vision Obscured")) |>
  ggplot(aes(crash_hour)) +
  geom_bar(aes(fill = prim_contributory_cause), 
           position = "fill") +
  labs(x = "Crash Hour",
       y = "Count",
       fill = "Primary\nContributory\nCause")

ggsave("looking_deeper_night_final.png",
       night,
       path = "plots/final")


# How do crashes at different speeds involving different numbers of units differ in injury severity? 

units_v_speed <- ggplot(traffic |> filter(!is.na(most_severe_injury)), 
                        aes(posted_speed_limit, num_units)) +
  geom_point(position = position_jitter(width = 3, height = 0.5)) +
  facet_wrap(~most_severe_injury) +
  labs(x = "Posted Speed Limit",
       y = "Number of Units Involved")

ggsave("looking_deeper_speed_v_units_final.png",
       units_v_speed,
       path = "plots/final")

### Aside 
dy <- traffic |> 
  filter(delay_bins == "day") |> 
  ggplot(aes(delay)) +
  geom_histogram() +
  labs(x = "Delay within 1 day",
       y = "Count")

ggsave("aside_delay_by_day.png",
       dy,
       path = "plots/final")

wk <- traffic |> 
  filter(delay_bins == "week") |> 
  ggplot(aes(delay)) +
  geom_histogram() +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) +
  labs(x = "Delay by number of days",
       y = "Count")

ggsave("aside_delay_by_wk.png",
       wk,
       path = "plots/final")

ggsave("aside_delay_by_wk.png",
       wk,
       path = "plots/final")

all <- ggplot(traffic, aes(delay)) +
  geom_histogram(bins = 250) +
  coord_cartesian(ylim = c(0, 25)) +
  labs(x = "Duration of Delay (in days)",
       y = "Count")

ggsave("aside_delay_all.png",
       all,
       path = "plots/final")

### Where do crashes occur?
traffic |> 
  mutate(address = paste(street_no, street_direction, street_name, sep = " "),
         .keep = "used") |> 
  count(address) |> 
  arrange(desc(n)) |> 
  slice_head(n = 10) |> 
  knitr::kable(align = "c",
               col.names = c("Address", "Number of Crashes"))

