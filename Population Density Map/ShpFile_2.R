# Libraries ---------------------------------------------------------------
library(rgdal)
library(tidyverse)
library(readxl)
library(httr)
library(dplyr)
library(leaflet)


# Setup -------------------------------------------------------------------

shp <- readOGR(dsn = "./Population Density Map", layer = "LGA_2016_AUST")

Population_Data <- read_csv("Population Density Map/Population_Data.csv", 
                            col_types = cols(MEASURE = col_skip(), 
                                             Measure = col_skip(), REGIONTYPE = col_skip(), 
                                             `Geography Level` = col_skip(), FREQUENCY = col_skip(), 
                                             Frequency = col_skip(), TIME = col_skip(), 
                                             `Flag Codes` = col_skip(), Flags = col_skip())) %>% 
  filter(Region != "New South Wales", Time == 2016, LGA_2019 < 19000, !is.na(Value)) %>% 
  rename("LGA_CODE16" = LGA_2019)

m <- merge(shp, Population_Data, by = "LGA_CODE16", type = "inner")

m@data <- m@data %>% 
  mutate(POP_DENS = Value / AREASQKM16, LOG_POP_DENS = log(Value / AREASQKM16))

m <- m[!is.na(m@data$Value), ]

Test = m@data

mypalette <- colorNumeric(palette="plasma", domain=m@data$LOG_POP_DENS, na.color="transparent")

map <- leaflet(m) %>% 
  addTiles() %>%
  
  addPolygons(
    fillColor   = ~mypalette(LOG_POP_DENS),    
    weight      = 1,
    opacity     = 1,
    color       = "white",
    dashArray   = "3",
    fillOpacity = 0.7,
    highlight   = highlightOptions(
      weight       = 4,
      color        = "#white",
      dashArray    = "",
      bringToFront = TRUE),
    label        = c(m$LGA_NAME),
    popup = m$LGA_NAME16) %>%
  
  addLegend(pal      = mypalette, 
            values   = ~LOG_POP_DENS, 
            opacity  = 0.7, 
            title    = "Log10 of Population Density log(People/km)",
            position = "bottomright") %>% 
  addTiles() %>%
  addMarkers(aqi_site_unique$Longitude, 
             aqi_site_unique$Latitude, 
             popup = aqi_site_unique$site_year)

map


aqi_site_unique <- AQI_Summarized_Data %>%
  select(SiteName, Year, Longitude, Latitude) %>%
  unique()

aqi_site_unique <- aqi_site_year_unique %>%
  mutate(site_year = paste(SiteName)) %>%
  group_by(Latitude, Longitude)

# adding markers
aqi_map <- leaflet() %>%
  addTiles() %>%
  addMarkers(aqi_site_unique$Longitude, 
             aqi_site_unique$Latitude, 
             popup = aqi_site_unique$site_year) 
aqi_map



aqi_Site_LGA_Key <- aqi_site_unique %>% 
  select(SiteName, Longitude, Latitude) %>% 
  unique()

write.csv(aqi_Site_LGA_Key, "C:/Users/willi/OneDrive/Desktop/AQI_Site_LGA_Key.csv", row.names = FALSE)
