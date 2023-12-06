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
