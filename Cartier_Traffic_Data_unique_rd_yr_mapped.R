library(magrittr)
library(leaflet)

# traffic data  unique road_name and year combinations
traffic_road_year_unique <- Traffic_Data %>%
  select(road_name, year, wgs84_latitude, wgs84_longitude) %>%
  unique()

# defining the bounds of the map 
lat_range <- range(traffic_road_year_unique$wgs84_latitude)
long_range <- range(traffic_road_year_unique$wgs84_longitude)

bounds_lat1 <-  floor(lat_range[1])
bounds_long1 <- floor(long_range[1])
bounds_lat2 <- ceiling(lat_range[2])
bounds_long2 <- ceiling(long_range[2])

# creating the map
map <- leaflet() %>%
  addTiles() %>%
  fitBounds(bounds_long1, bounds_lat1, bounds_long2, bounds_lat2)
map

# new road_year = concatenated road and year
traffic_road_year_unique <- traffic_road_year_unique %>%
  mutate(road_year = paste(road_name, year)) %>%
  group_by(wgs84_latitude, wgs84_longitude)

# transferring group data 
map <- leaflet() %>%
  addTiles() %>%
  addMarkers(traffic_road_year_unique$wgs84_longitude, 
             traffic_road_year_unique$wgs84_latitude, 
             popup = traffic_road_year_unique$road_year) 
map
