library(tidyverse)
library(readxl)
library(xlsx)
library(tidyr)
library(dplyr)
library(lubridate)
library(zoo)
library(corrplot)
library(ggplot2)
library(scales)
library(reshape)





str(AQI_Summarized_Data)
particles <- AQI_Summarized_Data %>%  filter(Parameter.ParameterCode == c("NO","NO2","OZONE","PM10","CO"))
particles<- arrange(particles,Parameter.ParameterCode )


getwd()

write.csv(particles,"C:/Users/NASA/Desktop/UTS-MDSI/36103 Statistical Thinking for Data Science/AT2/Air-Pollution\\particles.csv", row.names = FALSE)


#Convert pphm and µg/m³ to ppm

particles$Value <-ifelse(particles$Parameter.Units =="µg/m³",particles$Value *(0.02455/1450),ifelse(particles$Parameter.Units =="pphm",particles$Value *0.01,particles$Value*1))

particles$Parameter.Units[particles$Parameter.Units %in% "µg/m³"] <- "ppm"
particles$Parameter.Units[particles$Parameter.Units %in% "pphm"] <- "ppm"


#Heatmap by Region
ggp <- ggplot(particles, aes(Parameter.ParameterCode, Region)) +                          
  geom_tile(aes(fill = Value))
ggp 

#Heatmap by Year
ggp <- ggplot(particles, aes(Parameter.ParameterCode, Year)) +                          
  geom_tile(aes(fill = Value))
ggp

#Heatmap by SiteName
ggp <- ggplot(particles, aes(Parameter.ParameterCode, SiteName)) +                          
  geom_tile(aes(fill = Value))
ggp


  