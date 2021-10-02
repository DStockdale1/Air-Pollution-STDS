library(tidyverse)
library(dplyr)
library(ggplot2)
library(MASS)
library(magrittr)
library(olsrr)
library(lmtest)

setwd("C:/Users/Declan/Documents/DataScience/Air-Pollution-STDS")
load("Full_Data_Merged.Rda")

##### NO Data ####
# Population NO
NO_Population<-Full_Data_merged%>%
  filter(Parameter.ParameterCode =="NO") %>%
  dplyr::select(LGA, Year, Value,population_Test)

#Sites with only a few recordings
table((NO_Population$LGA))
#1 - COFFS GOULBURN, SYDNEY
#2 - GUNNEDAH, NTH SYDNEY PORT MACQUARIE
#3 - RYDE

# Check for Na's -  No missing values

NO_Population[which(is.na(NO_Population$population_Test)),]
NO_Population[which(is.na(NO_Population$Value)),]


# Singleton and Musselbrook 2011 will be changed to mean of other Singleton recordings
NO_Population[145,3]<-0.57019896
NO_Population[89,3]<-0.98951836375

# concat LGA names for graphs 
NO_Population %<>%
  mutate(concat_LGA = substr(LGA, 1, 4))

# matrices
# Correlations and relationships
cor_matrix <- cor(NO_Population[2:4])
cor_matrix
corrplot::corrplot(cor_matrix) 
# marginal pos corr between value with pop 
# marginal neg corr between value with year

#scatter correlation matrix
pairs(NO_Population[2:4]) 

#### NO plots ####
ggplot(NO_Population, aes(x = reorder(concat_LGA, Value), y = Value)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  labs(title = "   Relationship between LGA and Nitrogen Monoxide for Population",
       x = "LGA",
       y = "Nitrogen Monoxide (ppm)")

ggplot(NO_Population, aes(x = Year, y = Value)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  labs(title = "   Relationship between LGA and Nitrogen Monoxide for Population",
       x = "LGA",
       y = "Nitrogen Monoxide (ppm)")

# scatter plot CO vs pop coloured with time
ggplot(NO_Population, aes(x = population_Test, y = Value)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Population vs Nitrogen Monoxide",
       x = "Population",
       y = "Nitrogen Monoxide (ppm)")

#Nth sydney is the outlier - removing it
NO_Population_removed_NS <- NO_Population[-c(which(NO_Population["LGA"] == "NORTH SYDNEY")),]

#### NO after removing North Sydney ####
cor_matrix <- cor(NO_Population_removed_NS[2:4])
cor_matrix
corrplot::corrplot(cor_matrix) 
# marginal pos corr between value with pop 
# marginal neg corr between value with year

#scatter correlation matrix
pairs(NO_Population_removed_NS[2:4]) 

#### NO plots ####
ggplot(NO_Population_removed_NS, aes(x = reorder(concat_LGA, Value), y = Value)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  labs(title = "   Relationship between LGA and Nitrogen Monoxide for Population",
       x = "LGA",
       y = "Nitrogen Monoxide (ppm)")

#### THIS PLOT
# scatter plot CO vs pop coloured with time
ggplot(NO_Population_removed_NS, aes(x = population_Test, y = Value)) + 
  geom_point() +
  geom_smooth(method = 'lm')+
  #theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      # panel.grid.major = element_blank(),
       # panel.grid.minor = element_blank(),
        #panel.background = element_blank())+
        labs(title = "Relationship between Population and Nitrogen Monoxide",
       x = "Population",
       y = "Nitrogen Monoxide (ppm)")

#### THIS MODEL
#### NO regression using OLSRR NS removed ####
NO_removed_model <- lm(Value ~LGA+Year+population_Test, data = NO_Population_removed_NS)
k <- ols_step_all_possible(NO_removed_model)
k

lm.NO_population_removed_1 <- lm((Value) ~LGA+Year+population_Test,  data = NO_Population_removed_NS)
summary(lm.NO_population_removed_1)
par(mfrow = c(2, 2))
plot(lm.NO_population_removed_1) # automatically removed datapoint 45,152
bptest(lm.NO_population_removed_1)
#shapiro.test(NO_Population_removed_NS$Year)



#### NO removed sites ####
#Removd sites with less than 3 years
#1 - COFFS, GOULBURN, SYDNEY
#2 - GUNNEDAH, NTH SYDNEY PORT MACQUARIE
#3 - RYDE

NO_Population_removed <- NO_Population_removed[-c(which(NO_Population_removed["LGA"] == "COFFS HARBOUR")),]
NO_Population_removed <- NO_Population_removed[-c(which(NO_Population_removed["LGA"] == "GOULBURN MULWAREE")),]
NO_Population_removed <- NO_Population_removed[-c(which(NO_Population_removed["LGA"] == "GUNNEDAH")),]
NO_Population_removed <- NO_Population_removed[-c(which(NO_Population_removed["LGA"] == "SYDNEY")),]
NO_Population_removed <- NO_Population_removed[-c(which(NO_Population_removed["LGA"] == "RYDE")),]
NO_Population_removed <- NO_Population_removed[-c(which(NO_Population_removed["LGA"] == "PORT MACQUARIE-HASTINGS")),]

#### NO Removed Plots ####

ggplot(NO_Population_removed, aes(x = reorder(concat_LGA, Value), y = Value)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  labs(title = "   Relationship between LGA and Nitrogen Monoxide for Population",
       subtitle = "Removed incomplete sites",
       x = "LGA",
       y = "Nitrogen Monoxide (ppm)")

ggplot(NO_Population_removed, aes(x = Year, y = Value)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "   Relationship between LGA and Nitrogen Monoxide for Population",
       subtitle = "Removed incomplete sites",
       x = "LGA",
       y = "Nitrogen Monoxide (ppm)")

# scatter plot CO vs pop coloured with time
ggplot(NO_Population_removed, aes(x = population_Test, y = Value,col=Year)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  labs(title = "Relationship between Population and Nitrogen Monoxide for Population",
       subtitle = "Removed incomplete sites",
       x = "Population",
       y = "Nitrogen Monoxide (ppm)")

#### NO removed regression ####
# using OLSRR
model_NO_removed_1 <- lm(Value ~., data = NO_Population_removed)
k <- ols_step_all_possible(model_NO_removed_1)
k
#plot(k)# best adj R square population_Test LGA population_Test:LGA
lm.NO_population_removed_1 <- lm(Value ~population_Test*LGA+population_Test*Year,  data = NO_Population_removed)#CO_Population
summary(lm.NO_population_removed_1)
summary(lm.NO_population_removed_1)$sigma 
summary(lm.NO_population_removed_1)$r.squared
summary(lm.NO_population_removed_1)$coefficients 
par(mfrow = c(2, 2))
plot(lm.NO_population_removed_1)