### 3 - Bivariate Analysis #####
library(tidyverse)
traffic <- read_rds("data/traffic_data.rds")

# Plots with interesting univariate distributions
## factor:device_condition, first_crash_type, lighting_condition, 
         #most_severe_injury, prim/sec_contributory_cause, roadway_surface_cond, 
         #weather_condition
## numeric: crash day of week, crash hour, month, injuries
## logical: hit and run, intersection related, not right of way 

# What causes of crashes produce the greatest number of injuries?
traffic |> 
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

# How do crash severity and type correlate?
traffic %>% 
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
       x = "Most_Severe_Injury",
       fill = "Proportion of Crash Type\nwithin Injury Category")

# There are fewer crashes are during the winter, but are there more due to road conditions?
traffic |> 
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

 
# Are there differences in cause between daytime and nighttime crashes?
traffic |> 
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
  filter(prim_contributory_cause %in% c("Disregarding signs and markings",
                                        "Improper Driving",
                                        "Vehicle Condition",
                                        "Influence of Substances",
                                        "Driver Condition",
                                        "Construction-related",
                                        "Vision Obscured")) |> 
  ggplot(aes(crash_hour)) +
  geom_bar(aes(fill = prim_contributory_cause), 
           position = "fill")

# Is there a difference in injury severity that is dependent upon weather? 
traffic |> 
  filter(most_severe_injury %in% c("NO INDICATION OF INJURY", "INCAPACITATING INJURY", "FATAL"),
         weather_condition != "CLEAR",
         weather_condition != "RAIN") |> 
  count(most_severe_injury, weather_condition) |> 
  mutate(prop = n / sum(n), .by = most_severe_injury) |> 
  ggplot(aes(most_severe_injury, weather_condition)) +
  geom_tile(mapping = aes(fill = prop)) 

# How do crashes at different speeds involving different numbers of units differ in injury severity? 
test <- traffic |> slice_head(n = 100)
ggplot(traffic, aes(posted_speed_limit, num_units)) +
  geom_point(position = position_jitter(width = 3, height = 0.5)) +
  facet_wrap(~most_severe_injury)

