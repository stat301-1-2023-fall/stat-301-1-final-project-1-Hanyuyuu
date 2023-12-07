## Datasets

The data used in this project comes from the City of Chicago. Raw data can be found at:

https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if

Included in this folder are cleaned datasets used in data wrangling and visualization, as well as codebooks to explain variables found in each.

traffic_data.rds contains all the original columns with the appropriate types affixed, as well as a few mutated variables used in data exploration.

injuries.rds is a subset of the original traffic_data.rds, which contained different measures of injuries recorded at each crash in separate columns. For ease of visualization, these variables were subsetted out of traffic_data.rds and pivoted to create a longer separate dataset.

chi_map.rds contains map data of selected zipcodes in the City of Chicago from the tigris package. This is used in Section 4.8 of the Final Report.

traffic_map.rds contains location data for all traffic crashes in the traffic_data.rds with longitude and latitude data, encoded as a geometric object used for plotting on a map.

traffic_data_codebook and chi_map codebook adapted respectively from codebook given by the City of Chicago for the raw traffic_data (sourced from link at top) and shapefile documentation provided by census.gov (https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2019/TGRSHP2019_TechDoc.pdf, page 30 of pdf)
