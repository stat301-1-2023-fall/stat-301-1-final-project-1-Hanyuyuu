## Datasets

The data used in this project comes from the City of Chicago. Raw data can be found at:

https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if

Included in this folder are cleaned datasets used in data wrangling and visualization, as well as codebooks to explain variables found in each.

traffic_data.rds contains all the original columns with the appropriate types affixed, as well as a few mutated variables used in data exploration.

injuries.rds is a subset of the original traffic_data.rds, which contained different measures of injuries recorded at each crash in separate columns. For ease of visualization, these variables were subsetted out of traffic_data.rds and pivoted to create a longer separate dataset.


