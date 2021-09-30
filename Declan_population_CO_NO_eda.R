library(tidyverse)
library(dplyr)
library(ggplot2)
library(MASS)
library(magrittr)
library(olsrr)

setwd("C:/Users/Declan/Documents/DataScience/Air-Pollution-STDS")
load("Full_Data_Merged.Rda")

full_dataset<- Full_Data_merged

##### CO Data ####
# CO data

CO_Population<-full_dataset%>%
  filter(Parameter.ParameterCode =="CO") %>%
  dplyr::select(LGA, Year, Value, population_Test)

table((CO_Population$LGA))
#1 - COFFS,, SYDNEY
#2 - NTH SYDNEY PORT MACQUARIE
#3 - RYDE

#check for Na's
#No missing values
CO_Population[which(is.na(CO_Population$population_Test)),]
CO_Population[which(is.na(CO_Population$Value)),]

# bar chart for LGAs concat to shorten
CO_Population %<>%
  mutate(concat_LGA = substr(LGA, 1, 4))

# Correlations and relationships
cor_matrix <- cor(CO_Population[2:4])
corrplot::corrplot(cor_matrix) 
# marginal neg corr between value with pop 
# small neg corr between value with year

#scatter correlation matrix
pairs(CO_Population[2:4]) 

CO_Population %<>%
  mutate(concat_LGA = substr(LGA, 1, 4))

#### CO Plots ####
ggplot(CO_Population, aes(x = reorder(concat_LGA, Value), y = Value)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  labs(title = "   Relationship between LGA and Carbon Monoxide for Population",
       x = "LGA",
       y = "Carbon Monoxide (ppm)")

str(CO_Population)
# scatter plot CO vs pop coloured with time
ggplot(CO_Population, aes(x = population_Test, y = Value,col=Year)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  #scale_color_brewer(palette="Dark2")+
  labs(title = "   Relationship between Population and Carbon Monoxide for Population",
       x = "Population",
       y = "Carbon Monoxide (ppm)")



#### CO Regression ####

##### CO Model 1 - everything #####
lm.CO_population_1 <- lm(Value ~., data = CO_Population)#CO_Population
summary(lm.CO_population_1)
summary(lm.CO_population_1)$sigma 
summary(lm.CO_population_1)$r.squared
summary(lm.CO_population_1)$coefficients 
par(mfrow = c(2, 2))
plot(lm.CO_population_1)

##### CO Model 2- stepwise #####
step.CO_population_2 <- stepAIC(CO_population_1, direction = "both", 
                      trace = TRUE)

summary(step.CO_population_2)
#summary(step.CO_population_2)$sigma 
#summary(step.CO_population_2)$r.squared
#summary(step.CO_population_2)$coefficients 

#### CO Model 3 ####
lm.CO_population_3 <- lm(log(Value) ~population_Test*LGA+population_Test,  data= CO_Population)#CO_Population
summary(lm.CO_population_3)
#summary(lm.CO_population_3)$sigma 
#summary(lm.CO_population_3)$r.squared
#summary(lm.CO_population_3)$coefficients 
par(mfrow = c(2, 2))
plot(lm.CO_population_3)


#### CO Model 4 using OLSRR package ####
CO_model <- lm(Value ~population_Test*LGA+population_Test, data = CO_Population)
k <- ols_step_all_possible(CO_model)
k# best adj R square population_Test LGA population_Test:LGA

#saved plot is here
lm.CO_population_4 <- lm(Value ~population_Test*LGA+population_Test,  data = CO_Population)#CO_Population
summary(lm.CO_population_4)
summary(lm.CO_population_4)$sigma 
summary(lm.CO_population_4)$r.squared
summary(lm.CO_population_4)$coefficients 
par(mfrow = c(2, 2))
plot(lm.CO_population_4)

#### CO Removed Data ####

# CO Removing sites with 3 or less readings over the ten years
#1 - COFFS HARBOUR, SYDNEY
#2 - NTH SYDNEY, PORT MACQUARIE-HASTINGS
#3 - RYDE

#CO_Population
CO_Population_removed <- CO_Population
CO_Population_removed <- CO_Population_removed[-c(which(CO_Population_removed["LGA"] == "COFFS HARBOUR")),]
CO_Population_removed <- CO_Population_removed[-c(which(CO_Population_removed["LGA"] == "SYDNEY")),]
CO_Population_removed <- CO_Population_removed[-c(which(CO_Population_removed["LGA"] == "NORTH SYDNEY")),]
CO_Population_removed <- CO_Population_removed[-c(which(CO_Population_removed["LGA"] == "PORT MACQUARIE-HASTINGS")),]
CO_Population_removed <- CO_Population_removed[-c(which(CO_Population_removed["LGA"] == "RYDE")),]

#### CO Removed Plots ####

ggplot(CO_Population_removed, aes(x = reorder(concat_LGA, Value), y = Value)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  labs(title = "   Relationship between LGA and Carbon Monoxide for Population",
       subtitle = "Removed incomplete sites",
       x = "LGA",
       y = "Carbon Monoxide (ppm)")

ggplot(CO_Population_removed, aes(x = Year, y = Value)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "   Relationship between LGA and Carbon Monoxide for Population",
       subtitle = "Removed incomplete sites",
       x = "LGA",
       y = "Carbon Monoxide (ppm)")

# scatter plot CO vs pop coloured with time
ggplot(CO_Population_removed, aes(x = population_Test, y = Value,col=Year)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  labs(title = "Relationship between Population and Carbon Monoxide for Population",
       subtitle = "Removed incomplete sites",
       x = "Population",
       y = "Carbon Monoxide (ppm)")

#### CO removed regression ####
# using OLSRR
CO_removed_model <- lm(Value ~., data = CO_Population_removed)
k <- ols_step_all_possible(CO_removed_model)
k
plot(k)# best adj R square population_Test LGA population_Test:LGA

lm.CO_population_removed_1 <- lm(Value ~population_Test*LGA+population_Test,  data = CO_Population_removed)#CO_Population
summary(lm.CO_population_removed_1)
summary(lm.CO_population_removed_1)$sigma 
summary(lm.CO_population_removed_1)$r.squared
summary(lm.CO_population_removed_1)$coefficients 
par(mfrow = c(2, 2))
plot(lm.CO_population_removed_1)

##### NO Data ####
# Population NO
NO_Population<-full_dataset%>%
  filter(Parameter.ParameterCode =="NO") %>%
  dplyr::select(LGA, Year, Value,population_Test)

table((NO_Population$LGA))
#1 - COFFS GOULBURN, SYDNEY
#2 - GUNNEDAH, NTH SYDNEY PORT MACQUARIE
#3 - RYDE

# Check for Na's
# No missing values
NO_Population[which(is.na(NO_Population$population_Test)),]
NO_Population[which(is.na(NO_Population$Value)),]
# concat LGA names for plotting
NO_Population %<>%
  mutate(concat_LGA = substr(LGA, 1, 4))

# matrices
# Correlations and relationships
cor_matrix <- cor(NO_Population[2:4])
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
ggplot(NO_Population, aes(x = population_Test, y = Value,col=Year)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  labs(title = "Relationship between Population and Nitrogen Monoxide for Population",
       x = "Population",
       y = "Nitrogen Monoxide (ppm)")

#Nth sydney is the outlier - removing it
NO_Population_removed <- NO_Population[-c(which(NO_Population["LGA"] == "NORTH SYDNEY")),]

#### NO after removing North Sydney ####

cor_matrix <- cor(NO_Population_removed[2:4])
corrplot::corrplot(cor_matrix) 
# marginal pos corr between value with pop 
# marginal neg corr between value with year

#scatter correlation matrix
pairs(NO_Population_removed[2:4]) 

#### NO plots ####
ggplot(NO_Population_removed, aes(x = reorder(concat_LGA, Value), y = Value)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  labs(title = "   Relationship between LGA and Nitrogen Monoxide for Population",
       x = "LGA",
       y = "Nitrogen Monoxide (ppm)")

ggplot(NO_Population_removed, aes(x = Year, y = Value)) + 
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
ggplot(NO_Population_removed, aes(x = population_Test, y = Value,col=Year)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())+
  #scale_color_brewer(palette="Dark2")+
  labs(title = "Relationship between Population and Nitrogen Monoxide for Population",
       x = "Population",
       y = "Nitrogen Monoxide (ppm)")

#### NO regression using OLSRR ####
NO_removed_model <- lm(Value ~.-concat_LGA, data = NO_Population_removed)
k <- ols_step_all_possible(NO_removed_model)
k

lm.NO_population_removed_1 <- lm(log(Value) ~LGA+Year+log(population_Test),  data = NO_Population_removed)#CO_Population
summary(lm.NO_population_removed_1)
summary(lm.NO_population_removed_1)$sigma 
summary(lm.NO_population_removed_1)$r.squared
summary(lm.NO_population_removed_1)$coefficients 
par(mfrow = c(2, 2))
plot(lm.NO_population_removed_1)


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