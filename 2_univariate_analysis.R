### 2 - Univariate Analysis #####
library(tidyverse)
traffic <- read_rds("data/traffic_data.rds")
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

## Datetime variable plots

for (interval in levels(traffic$delay_bins)){
  delay_plot <- traffic |> 
    filter(delay_bins == interval) |> 
    ggplot(aes(delay)) +
    geom_histogram()
  ggsave(paste(interval, "_distribution.png", sep = ""),
         delay_plot,
         path = "./plots/datetime")
}

#ggplot(traffic, aes(delay)) +
#  geom_histogram(bins = 250) +
#  coord_cartesian(ylim = c(0, 25))
