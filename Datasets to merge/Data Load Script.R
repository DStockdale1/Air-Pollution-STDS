library(tidyverse)
library(readxl)

# This script loads the data sets we will be using (except the zoning until we find that)

Population_Data <- read_csv("Raw Data/Population_Data.csv", 
                            col_types = cols(MEASURE = col_skip(), 
                                             Measure = col_skip(), REGIONTYPE = col_skip(), 
                                             `Geography Level` = col_skip(), FREQUENCY = col_skip(), 
                                             Frequency = col_skip(), TIME = col_skip(), 
                                             `Flag Codes` = col_skip(), Flags = col_skip()))

Traffic_Data <- read_csv("Raw Data/Traffic_Volume_Viewer_2007-2021.csv", 
                         col_types = cols(the_geom = col_skip()))

{
  load("Raw Data/Pollutants_Data.Rda") # Pollution Data
  Pollutants_Data <- Pollutants_Data %>% 
    mutate(Year = str_sub(report_year, -4, -1))
} # Load Pollutants Data


{
  load("Datasets to merge/Raw Data/AQI_Data.Rda")
  load("Datasets to merge/Raw Data/AQI Data/Air Quality Site Details.Rda")
} # Load AQI Data

AQI_Summarized_Data <- AQI_Summarized_Data %>% 
  merge(df_site_details)



# This is a demonstration of how to join the data so you can easily compare the different datasets


# To calculate distance between points:

Rozelle <- df_site_details %>% 
  + filter(SiteName == "ROZELLE") %>% 
  + select(SiteName, Longitude, Latitude)

Randwick <- df_site_details %>% 
  + filter(SiteName == "RANDWICK") %>% 
  + select(SiteName, Longitude, Latitude)

distHaversine(c(Randwick$Longitude, Randwick$Latitude), c(Rozelle$Longitude, Rozelle$Latitude)) # Result is in metres
