### 3 - Bivariate Analysis #####
library(tidyverse)
traffic <- read_rds("data/traffic_data.rds")

# Plots with interesting univariate distributions
## factor:device_condition, first_crash_type, lighting_condition, 
          #most_severe_injury, prim/sec_contributory_cause, roadway_surface_cond, weather_condition
## numeric: crash day of week, crash hour, month, injuries?
## logical: hit and run, intersection related, not right of way 

traffic |> 
  filter(most_severe_injury %in% c("NO INDICATION OF INJURY", "INCAPACITATING INJURY", "FATAL")) |> 
ggplot(aes(most_severe_injury, prim_contributory_cause)) +
  geom_tile() +
  facet_wrap(~roadway_surface_cond)

# What causes of crashes produce the greatest number of injuries?
traffic |> 
  filter(injuries_total > 0) |> 
  ggplot(aes(prim_contributory_cause, injuries_total)) +
  geom_point(alpha = 0.5, 
             position = position_jitter(width = 0.3, height = .5)) +
  coord_flip()

# How do crash severity and type correlate?
traffic %>% 
  filter(most_severe_injury %in% c("NO INDICATION OF INJURY", 
                                  "INCAPACITATING INJURY", 
                                  "FATAL")) |> 
  count(most_severe_injury, first_crash_type) %>%  
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

# most crashes are on Friday, but does that mean they are more severe?
traffic |> 
  ggplot(aes(crash_day_of_week)) +
  geom_bar(aes(fill = most_severe_injury))

# fewer crashes are during the winter, but are they mostly due to road conditions?
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
                                                            , "EQUIPMENT - VEHICLE CONDITION"                                                   
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
                                                            , "ROAD CONSTRUCTION/MAINTENANCE"                                                   
                                                            , "ROAD ENGINEERING/SURFACE/MARKING DEFECTS"                                        
                                                            , "TEXTING"                                                                         
                                                            , "TURNING RIGHT ON RED"                                                            
                                                            , "UNABLE TO DETERMINE"                                                             
                                                            , "UNDER THE INFLUENCE OF ALCOHOL/DRUGS (USE WHEN ARREST IS EFFECTED)")
                                                )) |> 
  filter(weather_condition != "CLEAR") |> # %in% c("VISION OBSCURED (SIGNS, TREE LIMBS, BUILDINGS, ETC.)", 
                                        #"WEATHER")) |> 
  ggplot(aes(crash_month)) +
  geom_bar(aes(fill = prim_contributory_cause)) +
  labs(x = "Crash Month",
       y = "Count",
       fill = "Weather Condition")

 
# I thought crashes would occur more often at night; is there a confounding variable?
#Are crashes at night more severe?
ggplot(traffic, aes(crash_hour)) +
  geom_bar() +
  facet_wrap(~street_direction, nrow = 1)

ggplot(traffic)



### correlation matrices
traffic |> 
  filter(most_severe_injury == "FATAL") |> 
  select(where(is.numeric), -lane_cnt, -(injuries_total:injuries_unknown), -beat_of_occurrence) |> 
  cor() |> 
  round(digits = 3) |> 
  knitr::kable()
