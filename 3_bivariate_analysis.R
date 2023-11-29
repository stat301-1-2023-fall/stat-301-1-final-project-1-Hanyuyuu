### 3 - Bivariate Analysis #####
library(tidyverse)
traffic <- read_rds("data/traffic_data.rds")
traffic

colnames(traffic)
## factor: crash type, device_condition, first_crash_type, lighting_condition, most_severe_injury, prim/sec_contributory_cause, roadway_surface_cond, weather_condition
## numeric: crash day of week, crash hour, month, injuries?
## logical: hit and run, intersection related, not right of way 


