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


#Get working directory
getwd()

#Read CSV file
LGA<-read.csv("ABS_LGA_Population_2001_2019.csv")


#Check columns' name
colnames(LGA)

#Drop unuseable Variables
LGA<- LGA[, !(colnames(LGA) %in% c("ï..MEASURE","Geography.Level","FREQUENCY","Frequency" ,"TIME","Flag.Codes","Flags"))]


#Get data summary
str(LGA)
summary(LGA)

#Check unique Region
unique(LGA$Region)
unique(LGA$LGA_2019)



#Visualize Region column

g <- ggplot(LGA[-c(1:19),], aes(Time, Value))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(from = 2000, to = 2020, by = 1))+
  labs(title="Bar Chart", 
       subtitle="NSW population per Year" ) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


for (i in unique(LGA$Region)){
  print(ggplot(LGA[LGA$Region==i,], aes(Time, Value))+
    geom_bar(stat="identity", width = 0.5, fill="tomato2") +
      facet_wrap( ~ Region)+
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks = seq(from = 2000, to = 2020, by = 1))+
    labs(title="population per year at", 
         subtitle=i ) +
    theme(axis.text.x = element_text(angle=65, vjust=0.6)))
}




