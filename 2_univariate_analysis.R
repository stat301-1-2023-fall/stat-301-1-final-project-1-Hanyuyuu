### 2 - Univariate Analysis #####
# This step utilizes a series of for loops to generate univariate distribution plots for all 
# variables in the dataset\

library(tidyverse)
traffic <- read_rds("data/traffic_data.rds")

# list of variables, organized by type, from 1_data_cleaning.R
convert_to_factor <- c("traffic_control_device", "device_condition", "weather_condition", 
                       "lighting_condition", "first_crash_type", "trafficway_type",
                       "alignment", "roadway_surface_cond", "road_defect", "report_type",
                       "crash_type", "damage", "prim_contributory_cause", "sec_contributory_cause",
                       "work_zone_type", "most_severe_injury")
convert_to_bool <- traffic |> select((ends_with("_i"))) |> colnames()

convert_to_datetime <- c("crash_date", "date_police_notified")

### Numeric variable plots

for (var in traffic |> select(is.numeric) |> colnames()) {
  label <- rlang::englue("A histogram of {{var}}")
  numeric_plot <- 
    ggplot(traffic, aes(x = !!sym(var))) +
    geom_histogram(bins = 20)
  ggsave(paste(var, "_distribution.png", sep = ""), 
         numeric_plot,
         path = "./plots/numeric")
}

### Factor variable plots

for (var in convert_to_factor) {
  label <- rlang::englue("A barchart of {{var}}")
  factor_plot <- 
    ggplot(traffic, aes(x = !!sym(var))) +
    geom_bar() +
    coord_flip() +
    labs(title = label)
  ggsave(paste(var, "_distribution.png", sep = ""),
         factor_plot,
         path = "./plots/factor")
}

### Logical variable plots
for (var in convert_to_bool) {
  label <- rlang::englue("A barchart of {{var}}")
  lgl_plot <- 
    ggplot(traffic, aes(x = !!sym(var))) +
    geom_bar() +
    coord_flip() +
    labs(title = label)
  ggsave(paste(var, "_distribution.png", sep = ""),
         lgl_plot,
         path = "./plots/logical")
}

### Datetime variable plots

for (interval in levels(traffic$delay_bins)){
  delay_plot <- traffic |> 
    filter(delay_bins == interval) |> 
    ggplot(aes(delay)) +
    geom_histogram()
  ggsave(paste(interval, "_distribution.png", sep = ""),
         delay_plot,
         path = "./plots/datetime")
}

### Distribution of all delay between time of crash and time of report to police 

all_delay <- ggplot(traffic, aes(delay)) +
  geom_histogram(bins = 250) +
  coord_cartesian(ylim = c(0, 25))
ggsave("delay_distribution.png",
       all_delay,
       path = "./plots/datetime")

### Distribution of delay grouped by day, week, month
traffic |> 
  filter(delay_bins == "day") |> 
  ggplot(aes(delay)) +
  geom_histogram() +
  labs(title = "Day")

traffic |> 
  filter(delay_bins == "week") |> 
  ggplot(aes(delay)) +
  geom_histogram() +
  labs(title = "Week")

### Distribution of injuries all together
injury_dist <- injuries |> 
  group_by(injury_type) |> 
  summarize(sum = sum(value, na.rm = T)) |> 
  ggplot(aes(injury_type, sum)) +
  geom_col() +
  coord_flip()

ggsave("sum_injuries.png",
       injury_dist,
       path = "./plots/numeric")
