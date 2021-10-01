library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(xlsx)
library(tidyr)
library(lubridate)
library(zoo)
library(corrplot)
library(scales)
library(mgsub)
############Preparing AQI merged Data######################
##This codes copy from 'cartier21_sept_at1report to load AQI merged Data to manage population issue


"%notin%" <- Negate("%in%")

# Data Data ---------------------------------------------------------------

Population_Data <- read_csv("Datasets to merge/Raw Data/Population_Data.csv", 
                            col_types = cols(MEASURE = col_skip(), 
                                             Measure = col_skip(), REGIONTYPE = col_skip(), 
                                             `Geography Level` = col_skip(), FREQUENCY = col_skip(), 
                                             Frequency = col_skip(), TIME = col_skip(), 
                                             `Flag Codes` = col_skip(), Flags = col_skip()))

Traffic_Data <- read_csv("Traffic_Volume_Viewer_2007-2021.csv", 
                         col_types = cols(the_geom = col_skip()))


Suburb_LGA <- read_xlsx("Suburb_and_LGA_PercentageArea.xlsx")

load("Datasets to merge/Raw Data/AQI_Data.Rda")
AQI_Site_LGA_Key <- read_csv("AQI_Data/AQI_Site_LGA_Key.csv")
load("Datasets to merge/Raw Data/AQI Data/Air Quality Site Details.Rda")



# Population -> Suburb_lga ----------------------------------------------------

# remove NSW total population
Population_Data1 <- Population_Data %>%
  filter(Region != "New South Wales")

# keep only LGA name in uppercase
Population_Data1$Region <- c(toupper(mgsub(Population_Data1$Region, c(' [(].*'),
                                           c(''))))

# Suburb_LGA data remove council
Suburb_LGA$lganame <- c(mgsub(Suburb_LGA$lganame, c(' COUNCIL.*'), c('')))

# population data rename to "lganame"
Population_Data1 <- rename(Population_Data1, lganame = Region)

# unique LGA in both datasets
unique(Suburb_LGA$lganame)
unique(Population_Data1$lganame)

# append matching LGA of both datasets and find no match lga
a <- Suburb_LGA$lganame
b <- Population_Data1$lganame

matching_lga <- Suburb_LGA[which(a %in% b), ]
no_match_lga <- Suburb_LGA[which(a %notin% b), ]

#unique values in suburb lga data and population data with no match
unique(no_match_lga$lganame)
no_match_population_lga <- Population_Data1[-which(b %in% a), ]$lganame
unique(no_match_population_lga)

# match LGA values
{Population_Data1["lganame"][Population_Data1["lganame"] == "ALBURY"] <- "ALBURY CITY"
  Population_Data1["lganame"][Population_Data1["lganame"] == "LITHGOW"] <- "LITHGOW CITY"
  Population_Data1["lganame"][Population_Data1["lganame"] == "PARRAMATTA"] <- "CITY OF PARRAMATTA"
  Population_Data1["lganame"][Population_Data1["lganame"] == "NAMBUCCA"] <- "NAMBUCCA VALLEY"
  Population_Data1["lganame"][Population_Data1["lganame"] == "UPPER HUNTER SHIRE"] <- "UPPER HUNTER"
  Population_Data1["lganame"][Population_Data1["lganame"] == "WARRUMBUNGLE SHIRE"] <- "WARRUMBUNGLE"
  Population_Data1["lganame"][Population_Data1["lganame"] == "UNCORPORATED NSW" | 
                                "lganame" == "NO USUAL ADDRESS" | 
                                "lganame" == "MIGRATORY - OFFSHORE - SHIPPING"] <- "UNINCORPORATED"
  Suburb_LGA["lganame"][Suburb_LGA["lganame"] == "LORD HOWE ISLAND - UNINCORPORATED AREA"] <- "UNINCORPORATED"}

# removal of NAs
sum(is.na(Suburb_LGA$lganame))
Suburb_LGA <- Suburb_LGA[-c(which(is.na(Suburb_LGA$lganame))), ]

# merge population and suburb + LGA data 
Population_Data_merged <- merge(x = Suburb_LGA, y = Population_Data1, 
                                by = "lganame", all = TRUE)

# rename population columns
Population_Data_merged <- rename(Population_Data_merged, 
                                 population_number = Value, LGA = lganame, Year = Time)


Population_Data_merged_1 <- Population_Data_merged %>% 
  mutate(population_Test = round((population_number * percentage / 100), 0)) %>% 
  select(LGA, Year, population_Test) %>% 
  group_by(LGA, Year) %>% 
  summarise(population_Test = max(population_Test))


AQI_merged_Data <- AQI_Summarized_Data %>% 
  filter(Parameter.ParameterCode == "CO" |
           Parameter.ParameterCode == "NO" |
           Parameter.ParameterCode == "NO2" |
           Parameter.ParameterCode == "OZONE" |
           Parameter.ParameterCode == "TEMP" |
           Parameter.ParameterCode == "PM10") %>% 
  merge(df_site_details) %>% 
  merge(AQI_Site_LGA_Key) %>% 
  mutate(LGA = toupper(mgsub(LGA, c(' [(].*'),c(''))), Year = as.integer(Year)) %>% 
  group_by(LGA, Year, Parameter.ParameterCode, Parameter.ParameterDescription, Parameter.Units) %>% 
  summarize(Value = mean(Value)) %>% 
  arrange(Parameter.ParameterCode, LGA, Year) %>% 
  inner_join(Population_Data_merged_1, by = c("LGA", "Year"))

#################################################################

########################## Codes for Reggresion over PM10 & TEMP ####################

##Data summary
summary(AQI_merged_Data)

##Filtering PM10 & TEMP
#PM10
PM10_Data <- AQI_merged_Data %>%
  filter(Parameter.ParameterCode =="PM10")
#TEMP
TEMP_Data <- AQI_merged_Data %>%
  filter(Parameter.ParameterCode =="TEMP")

##removing unused columns
#PM10
pm10<- aggregate(cbind(PM10_value=PM10_Data$Value,population=PM10_Data$population_Test), 
                 list(LGA=PM10_Data$LGA, Year=PM10_Data$Year), sum)
#TEMP
temp<- aggregate(cbind(TEMP_value=TEMP_Data$Value,population=TEMP_Data$population_Test),
                 list(LGA=TEMP_Data$LGA, Year=TEMP_Data$Year), sum)

##check for Na's
#PM10
pm10[which(is.na(pm10$population)),]
pm10[which(is.na(pm10$PM10_value)),]

#TEMP
temp[which(is.na(temp$population)),]
temp[which(is.na(temp$TEMP_value)),]

##Scatter plot
#PM10
ggplot(pm10, aes(population, PM10_value)) +
  geom_point() +
  geom_smooth(method = lm)+
  labs(title = "   Relationship between population and Particulate matter (PM10)",
       x = "population",
       y = "PM10 (µg/m³)")


#TEMP
ggplot(temp, aes(population, TEMP_value)) +
  geom_point() +
  geom_smooth(method = lm)+
  labs(title = "   Relationship between population and temperature(°C)",
       x = "population",
       y = "Temperature (°C)")



##EDA on variables

# divide graph area in 2 columns
par(mfrow=c(2,2)) 

# box plot for 'PM10 Value'
boxplot(pm10$PM10_value, main="PM10 value")  

# box plot for 'Temperature value'
boxplot(temp$TEMP_value, main="Temperature value")  

# box plot for 'population'
boxplot(pm10$population, main="population") 

# divide graph area in 2 columns
par(mfrow=c(1, 2)) 

# density plot for 'PM10 Value'
plot(density(pm10$PM10_value), 
     main="Density Plot: PM10 Value", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(pm10$PM10_value), 2)))  
polygon(density(pm10$PM10_value), col="red")

# density plot for 'TEMP_value'
plot(density(temp$TEMP_value),
     main="Density Plot: TEMP_value", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(temp$TEMP_value), 2)))  
polygon(density(temp$TEMP_value), col="red")


## Correlations
# divide graph area in 2 columns
par(mfrow=c(1, 2)) 
#calculate correlation between PM10_value and population 
cor_pm <- cor(pm10[2:4])
corrplot(cor_pm) 
cor(pm10$PM10_value,pm10$population)
#calculate correlation between TEMP_value and population 
cor_temp <- cor(temp[2:4])
corrplot(cor_temp) 
cor(temp$TEMP_value,temp$population)



## build linear regression model on full data 
#PM10
linearMod_pm <- lm(PM10_value ~ population, data=pm10)  # build linear regression model on full data
print(linearMod_pm)
# model summary
summary(linearMod_pm)
#AIC
AIC(linearMod_pm)

#TEMP
linearMod_temp <- lm(TEMP_value ~ population, data=temp)  
print(linearMod_temp)
# model summary
summary(linearMod_temp)  
#AIC
AIC(linearMod_temp)


##Plot linear regression model
par(mfrow=c(2,2))  
plot(linearMod_pm)
plot(linearMod_temp)
par(mfrow=c(1,1)) 

#Close look to outliers
pm10 %>% slice(191)
temp %>% slice(138,144,189)

##Check Variance 
summary(pm10)
summary(temp)
var(pm10$PM10_value)
var(temp$TEMP_value)

##Model GLM poisson
#PM10
glmp.pm10<- glm(PM10_value ~ population, data = pm10, family = poisson(link = "log"))
summary(glmp.pm10)
#TEMP
glmp.temp<- glm(TEMP_value ~ population, data = temp, family = poisson(link = "log"))
summary(glmp.temp)

##Model GLM quasipoisson
#PM10
glmp.pm10<- glm(PM10_value ~ population, data = pm10, family = quasipoisson(link = "log"))
summary(glmp.pm10)
#TEMP
glmp.temp<- glm(TEMP_value ~ population, data = temp, family = quasipoisson(link = "log"))
summary(glmp.temp)
