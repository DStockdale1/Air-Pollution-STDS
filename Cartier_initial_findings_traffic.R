library(magrittr)
library(ggplot2)
library(dplyr)

#### load traffic data ####
Traffic_Data <- read.csv("Traffic_Volume_Viewer_2007-2021.csv")

#### deleting geom variable ####
Traffic_Data <- Traffic_Data %>%
  select(-the_geom)
head(Traffic_Data)

#### splitting the traffic data ####
# new traffic data location object (latitude and longitude, suburb, road_name and station id)
Traffic_Data_location <- Traffic_Data[1:3] # station_id, road, suburb 
Traffic_Data_location <- cbind(Traffic_Data_location, Traffic_Data[9:10]) # adding longitude and latitude
head(Traffic_Data_location)

#### new traffic data excluding location info ####
Traffic_Data <- Traffic_Data[4:8] # new traffic data excl location data
Traffic_Data <- cbind(Traffic_Data, Traffic_Data_location[1]) # adding station id back
head(Traffic_Data)

#### initial look ####
summary(Traffic_Data)
str(Traffic_Data)
head(Traffic_Data)
sum(is.na(Traffic_Data))

#### Understanding the Variables ####
DirectionName <- ggplot(Traffic_Data, aes(cardinal_direction_name, fill = cardinal_direction_name)) + 
  geom_bar(size = 0.5) + scale_fill_brewer(palette = "Paired") + labs( title = "Count of Traffic Flow Direction ")
DirectionName 

Class_Type <- ggplot(Traffic_Data, aes(classification_type, fill = classification_type)) + 
  geom_bar(size = 0.5) + scale_fill_brewer(palette = "Paired") + labs( title = "Traffic Classification Count")
Class_Type

Year <- ggplot(Traffic_Data, aes(year, fill = year)) + 
  geom_bar(size = 0.5) + scale_fill_brewer(palette = "Paired") + labs( title = "Count of Traffic Data Entries of each Year")
Year

Period <- ggplot(Traffic_Data, aes(period, fill = period)) + 
  geom_bar(size = 0.5) + scale_fill_brewer(palette = "Paired") + labs( title = "Traffic Period Count")
Period

Traffic_Count <- ggplot(Traffic_Data, aes(x = year, y = log(traffic_count), group = year)) + 
  geom_boxplot() + scale_fill_brewer(palette = "Paired") + labs( title = "Traffic Count across the Years") + stat_summary(fun = median, geom = "pointrange", size = .2, color = "red")
Traffic_Count

#### Relationships ####

# graphing direction and traffic count
Traffic_DirectionCount <- ggplot(Traffic_Data, aes(x = cardinal_direction_name, y = traffic_count)) +
  geom_boxplot() + scale_fill_brewer(palette = "Paired") + stat_summary(fun = median, geom = "pointrange", size = .2, color = "red") +
  labs(title = "Traffic Count for Road Directions")
Traffic_DirectionCount

# graph of traffic count and period
Traffic_PeriodCount <- ggplot(Traffic_Data, aes(x = period, y = traffic_count)) +
  geom_boxplot() + scale_fill_brewer(palette = "Paired") + stat_summary(fun = median, geom = "pointrange", size = .2, color = "red") +
  labs(title = "Traffic Count for Periods")
Traffic_PeriodCount

# graph of traffic count and year
Traffic_YearCount <- ggplot(Traffic_Data, aes(x = year, y = traffic_count)) +
  geom_boxplot() + scale_fill_brewer(palette = "Paired") + stat_summary(fun = median, geom = "pointrange", size = .2, color = "red") +
  labs(title = "Traffic Count for Years")
Traffic_YearCount

# graph of traffic count and class
Traffic_ClassCount <- ggplot(Traffic_Data, aes(x = classification_type, y = traffic_count, group = classification_type)) +
  geom_boxplot() + scale_fill_brewer(palette = "Paired") + stat_summary(fun = median, geom = "pointrange", size = .2, color = "red") +
  labs(title = "Traffic Count for Classification Type")
Traffic_ClassCount

# graph of traffic count and class
Traffic_ClassCount <- ggplot(Traffic_Data, aes(x = classification_type, y = traffic_count, group = classification_type)) +
  geom_boxplot() + scale_fill_brewer(palette = "Paired") + stat_summary(fun = median, geom = "pointrange", size = .2, color = "red") +
  labs(title = "Traffic Count for Classification Type")
Traffic_ClassCount
