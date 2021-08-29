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
  load("Raw Data/AQI Data/2015.Rda")
  aqi_2015 <- df_observations
  load("Raw Data/AQI Data/2016.Rda")
  aqi_2016 <- df_observations
  load("Raw Data/AQI Data/2017.Rda")
  aqi_2017 <- df_observations
  load("Raw Data/AQI Data/2018.Rda")
  aqi_2018 <- df_observations
  load("Raw Data/AQI Data/2019.Rda")
  aqi_2019 <- df_observations
  load("Raw Data/AQI Data/2020.Rda")
  aqi_2020 <- df_observations
  load("Raw Data/AQI Data/2021.Rda")
  aqi_2021 <- Obs_2021
  
  load("Raw Data/AQI Data/Air Quality Site Details.Rda")
  AQI_Site_Details <- df_site_details
  
  AQI_Data <- rbind(aqi_2015, rbind(aqi_2016, rbind(aqi_2017, rbind(aqi_2018, rbind(aqi_2019, rbind(aqi_2020, aqi_2021))))))
  
  remove(aqi_2015, aqi_2016, aqi_2017, aqi_2018, aqi_2019, aqi_2020, aqi_2021, Obs_2021, df_observations, df_site_details)
} # Load AQI Data. Will simplify and average to monthly this afternoon


# This is a demonstration of how to join the data so you can easily compare the different datasets

AQI_Mod_Data <- AQI_Data %>% 
  mutate(Year = str_sub(Date, 1, 4)) %>% 
  select(Site_Id, Value, Parameter.ParameterCode, Parameter.ParameterDescription, Parameter.Units, Year) %>% 
  group_by(Site_Id, Value, Parameter.ParameterCode, Parameter.ParameterDescription, Parameter.Units, Year) %>% 
  summarise(Value = mean(Value)) %>% 
  inner_join(AQI_Site_Details)


# To calculate distance between points:

Rozelle <- df_site_details %>% 
  + filter(SiteName == "ROZELLE") %>% 
  + select(SiteName, Longitude, Latitude)

Randwick <- df_site_details %>% 
  + filter(SiteName == "RANDWICK") %>% 
  + select(SiteName, Longitude, Latitude)

distHaversine(c(Randwick$Longitude, Randwick$Latitude), c(Rozelle$Longitude, Rozelle$Latitude)) # Result is in metres
