library(tidyverse)
library(readxl)
library(geosphere)
library(leaflet)

# This script loads the data sets we will be using (except the zoning until we find that)

setwd("C:/Users/NASA/Desktop/UTS-MDSI/36103 Statistical Thinking for Data Science/AT2/Air-Pollution/Datasets to merge/Raw Data")

Population_Data <- read_csv("Population_Data.csv", 
                            col_types = cols(MEASURE = col_skip(), 
                                             Measure = col_skip(), REGIONTYPE = col_skip(), 
                                             `Geography Level` = col_skip(), FREQUENCY = col_skip(), 
                                             Frequency = col_skip(), TIME = col_skip(), 
                                             `Flag Codes` = col_skip(), Flags = col_skip()))

Traffic_Data <- read_csv("Traffic_Volume_Viewer_2007-2021.csv", 
                         col_types = cols(the_geom = col_skip()))

{
  load("Pollutants_Data.Rda") # Pollution Data
  Pollutants_Data <- Pollutants_Data %>% 
    mutate(Year = str_sub(report_year, -4, -1))
} # Load Pollutants Data


{
<<<<<<< HEAD
  load("Raw Data/AQI_Data.Rda")
  load("Raw Data/AQI Data/Air Quality Site Details.Rda")
=======
<<<<<<< HEAD
  load("AQI_Data.Rda")
=======
  load("Datasets to merge/Raw Data/AQI_Data.Rda")
  load("Datasets to merge/Raw Data/AQI Data/Air Quality Site Details.Rda")
>>>>>>> f6ab82fafe7937dd41781c0a7b3a2be482bf3046
>>>>>>> 427b875c1cd8eaa085a1de918780b454de949a61
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

distHaversine(c(AQI_Summarized_Data$Longitude, AQI_Summarized_Data$Latitude), c(Traffic_Data$wgs84_longitude, Traffic_Data$wgs84_latitude))

AQI_Sites <- AQI_Summarized_Data %>% 
  transmute(AQI_Site_Id = Site_Id, AQI_Long = Longitude, AQI_Lat = Latitude) %>% 
  distinct(AQI_Site_Id, AQI_Long, AQI_Lat)

Traffic_Sites <- Traffic_Data %>%
  transmute(Traffic_Site_Id = station_id, Traff_Long = wgs84_longitude, Traff_Lat = wgs84_latitude) %>% 
  distinct(Traffic_Site_Id, Traff_Long, Traff_Lat)

my_distm <- function(long1, lat1, long2, lat2)
  distm(c(long1, lat1), c(long2, lat2), fun=distHaversine)

Crossjoined_AQI_Traffic <- merge(AQI_Sites, Traffic_Sites) %>% 
  arrange(AQI_Site_Id) %>% 
  rowwise() %>% 
  mutate(distance = my_distm(AQI_Long, AQI_Lat, Traff_Long, Traff_Lat)/1000)

boxplot(Crossjoined_AQI_Traffic$distance)



