# Libraries-----------------------------------------------------------------
library(revgeo)


# Sample --------------------------------------------------------------------
revgeo(longitude=-151.1575433, latitude=-33.8227679, provider = 'photon', output='frame')


# Code --------------------------------------------------------------------

lat_long <- unique(AQI_Summarized_Data[,8:9])

reverse <- lat_long %>% 
  reverse_geocode(lat = Latitude, long = Longitude, method = 'osm', 
                  address = address_found, full_results = TRUE) %>% 
  select(-address_found, -place_id, -license, -osm_type, -osm_id, -osm_lat, -osm_lon, -district)

suburb_lat_long <- as.data.frame(reverse) %>% 
  select(Longitude, Latitude, suburb, city, municipality, state) %>% 
  mutate(municipality = ifelse(grepl("Council", city, ignore.case = TRUE), city, municipality))

write.csv(lat_long, file="C:/Users/willi/OneDrive/Desktop/Longitude-Latitude.csv", row.names = FALSE)
