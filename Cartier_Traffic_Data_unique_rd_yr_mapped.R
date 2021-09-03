library(magrittr)
library(leaflet)

####################################################
#### Traffic Volume data #### 
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
traffic_map <- leaflet() %>%
  addTiles() %>%
  fitBounds(bounds_long1, bounds_lat1, bounds_long2, bounds_lat2)
traffic_map

# new road_year = concatenated road and year
traffic_road_year_unique <- traffic_road_year_unique %>%
  mutate(road_year = paste(road_name, year)) %>%
  group_by(wgs84_latitude, wgs84_longitude)

# adding markers
traffic_map <- leaflet() %>%
  addTiles() %>%
  addMarkers(traffic_road_year_unique$wgs84_longitude, 
             traffic_road_year_unique$wgs84_latitude, 
             popup = traffic_road_year_unique$road_year) 
traffic_map

# adding clusters
traffic_map_clustered <- leaflet() %>%
  addTiles() %>%
  fitBounds(bounds_long1, bounds_lat1, bounds_long2, bounds_lat2) 
traffic_map_clustered <- leaflet() %>%
  addTiles() %>%
  addMarkers(traffic_road_year_unique$wgs84_longitude, 
             traffic_road_year_unique$wgs84_latitude, 
             popup = traffic_road_year_unique$road_year,
             clusterOptions = markerClusterOptions()
             )
traffic_map_clustered

####################################################
#### AQI data #### 
# AQI data  unique site_name and year combinations
aqi_site_year_unique <- AQI_Summarized_Data %>%
  select(SiteName, Year, Longitude, Latitude) %>%
  unique()

# defining the bounds of the aqi_map 
aqi_lat_range <- range(aqi_site_year_unique$Latitude)
aqi_long_range <- range(aqi_site_year_unique$Longitude)

aqi_bounds_lat1 <-  floor(aqi_lat_range[1])
aqi_bounds_long1 <- floor(aqi_long_range[1])
aqi_bounds_lat2 <- ceiling(aqi_lat_range[2])
aqi_bounds_long2 <- ceiling(aqi_long_range[2])

# creating the aqi_map
aqi_map <- leaflet() %>%
  addTiles() %>%
  fitBounds(aqi_bounds_long1, aqi_bounds_lat1, aqi_bounds_long2, aqi_bounds_lat2)
aqi_map

# new site_year = concatenated site and year
aqi_site_year_unique <- aqi_site_year_unique %>%
  mutate(site_year = paste(SiteName, Year)) %>%
  group_by(Latitude, Longitude)

# adding markers
aqi_map <- leaflet() %>%
  addTiles() %>%
  addMarkers(aqi_site_year_unique$Longitude, 
             aqi_site_year_unique$Latitude, 
             popup = aqi_site_year_unique$site_year) 
aqi_map

# adding clusters
aqi_map_clustered <- leaflet() %>%
  addTiles() %>%
  fitBounds(aqi_bounds_long1, aqi_bounds_lat1, aqi_bounds_long2, aqi_bounds_lat2) 
aqi_map_clustered <- leaflet() %>%
  addTiles() %>%
  addMarkers(aqi_site_year_unique$Longitude, 
             aqi_site_year_unique$Latitude, 
             popup = aqi_site_year_unique$site_year,
             clusterOptions = markerClusterOptions()
  )
aqi_map_clustered

####################################################
#### Traffic and AQI #### 
# creating the map
traffic_aqi_map_clustered <- leaflet() %>%
  addTiles() %>%
  fitBounds(aqi_bounds_long1, aqi_bounds_lat1, aqi_bounds_long2, aqi_bounds_lat2
            )
traffic_aqi_map_clustered

# adding traffic volume clusters (and blue pins)
traffic_aqi_map_clustered <- leaflet() %>%
  addTiles() %>%
  addMarkers(traffic_road_year_unique$wgs84_longitude, 
             traffic_road_year_unique$wgs84_latitude, 
             clusterOptions = markerClusterOptions()
  )
traffic_aqi_map_clustered

#adding red AQI markers
red_icon <- awesomeIcons(
  icon = 'ios-close',
  library = 'ion',
  markerColor = "red",
  iconColor = "black",
  squareMarker = TRUE
)


traffic_aqi_map_clustered <- leaflet() %>%
  addTiles() %>%
  addMarkers(traffic_road_year_unique$wgs84_longitude, 
             traffic_road_year_unique$wgs84_latitude, 
             clusterOptions = markerClusterOptions()
             ) %>%
  addAwesomeMarkers(aqi_site_year_unique$Longitude, 
             aqi_site_year_unique$Latitude, 
             icon = red_icon,
             popup = aqi_site_year_unique$site_year)
traffic_aqi_map_clustered
