library(tidyverse)
library(readxl)

# This script loads the data sets we will be using (except the zoning until we find that)

Population_Data <- read_csv("Datasets to merge/Raw Data/Population_Data.csv", 
                            col_types = cols(MEASURE = col_skip(), 
                                             Measure = col_skip(), REGIONTYPE = col_skip(), 
                                             `Geography Level` = col_skip(), FREQUENCY = col_skip(), 
                                             Frequency = col_skip(), TIME = col_skip(), 
                                             `Flag Codes` = col_skip(), Flags = col_skip()))

Traffic_Data <- read_csv("Datasets to merge/Raw Data/Traffic_Volume_Viewer_2007-2021.csv", 
                         col_types = cols(the_geom = col_skip()))

load("Datasets to merge/Raw Data/Pollutants_Data.Rda")

{load("Datasets to merge/Raw Data/AQI Data/2015.Rda")
  aqi_2015 <- df_observations
  load("Datasets to merge/Raw Data/AQI Data/2016.Rda")
  aqi_2016 <- df_observations
  load("Datasets to merge/Raw Data/AQI Data/2017.Rda")
  aqi_2017 <- df_observations
  load("Datasets to merge/Raw Data/AQI Data/2018.Rda")
  aqi_2018 <- df_observations
  load("Datasets to merge/Raw Data/AQI Data/2019.Rda")
  aqi_2019 <- df_observations
  load("Datasets to merge/Raw Data/AQI Data/2020.Rda")
  aqi_2020 <- df_observations
  load("Datasets to merge/Raw Data/AQI Data/2021.Rda")
  aqi_2021 <- Obs_2021
  
  AQI_Data <- rbind(aqi_2015, rbind(aqi_2016, rbind(aqi_2017, rbind(aqi_2018, rbind(aqi_2019, rbind(aqi_2020, aqi_2021))))))
  
  remove(aqi_2015, aqi_2016, aqi_2017, aqi_2018, aqi_2019, aqi_2020, aqi_2021, Obs_2021, df_observations)} # Load AQI Data. Will simplify and average to monthly this afternoon


#This is a demonstration of how to join the data so you can easily compare the different datasets

Pollutants_Data <- Pollutants_Data %>% 
  filter(jurisdiction_code %in% c("NSW", "ACT", "COM"))