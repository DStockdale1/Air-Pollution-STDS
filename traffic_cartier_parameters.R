library(magrittr)
library(ggplot2)
setwd("/Users/cartierzhi/Documents/MDSI/s2/STDS/AT2/Air-Pollution-STDS")

load("Full_Data_merged.Rda")
######################### CO cleaning  #########################################
Traffic_CO <- Full_Data_merged %>%
  filter(Parameter.ParameterCode == "CO") %>%
  select(LGA, Year, Value, Traffic_Test
         ) 
  

# finding NAs
Traffic_CO[which(is.na(Traffic_CO$Traffic_Test)),]

# replacing NAs: assume same rate as other years of same LGA or 
# average rate of all observations if only single observation for LGA
filter(Traffic_CO, LGA == "COFFS HARBOUR")
TrafficTest_mean <- mean(Traffic_CO[-which(is.na(Traffic_CO$Traffic_Test)),]$Traffic_Test)
ValueMean <- mean(Traffic_CO$Value)
Traffic_CO[45,4] <- TrafficTest_mean/ValueMean*0.3248000

# Traffic_CO North sydney
filter(Traffic_CO, LGA == "NORTH SYDNEY")
Traffic_CO[76,4] <- ceiling(23231/0.3484948 * 0.2978417)

# Traffic_CO PORT MACQUARIE-HASTINGS
filter(Traffic_CO, LGA == "PORT MACQUARIE-HASTINGS")
Traffic_CO[79,4] <- ceiling(17724/0.01762819 * 0.38023958)

##########################CO EDA############################################
# Correlations and relationships
cor_matrix <- cor(Traffic_CO[2:4])
corrplot::corrplot(cor_matrix) # positive rel between traffic and CO
                                # negative rel between year with CO and traffic 
pairs(Traffic_CO[2:4])



# bar chart for LGAs
ggplot(Traffic_CO, aes(x = reorder(LGA, Value, FUN = median), y = Value)) + 
  geom_boxplot() +
  labs(title = "Relationship Between LGA and Carbon Monoxide",
     x = "LGA",
     y = "Carbon Monoxide (parts per million/ppm)")

# scatter plot
Traffic_CO %<>%
mutate(concat_LGA = substr(LGA, 1, 4))


ggplot(Traffic_CO, aes(x = Traffic_Test, y = Value)) +
  geom_point() +
  labs(title = "Relationship Between Traffic Volume and Carbon Monoxide",
       x = "Traffic Volume (number of vehicles)",
       y = "Carbon Monoxide (parts per million/ppm)") +
  geom_text(label = Traffic_CO$concat_LGA) +
  geom_smooth(method = lm)
################################ NO cleaning  ##################################
Traffic_NO <- Full_Data_merged %>%
  filter(Parameter.ParameterCode == "NO") %>%
  select(LGA, Year, Value, Traffic_Test
  ) 

# finding NAs
Traffic_NO[which(is.na(Traffic_NO$Traffic_Test)),]

# # replacing NAs: assume same rate as other years of same LGA or 
# # average rate of all observations if only single observation for LGA

# 1 entry: COFFS HARBOUR
TrafficTest_meanNO <- mean(Traffic_NO[-which(is.na(Traffic_NO$Traffic_Test)),]$Traffic_Test)
ValueMeanNO <- mean(Traffic_NO$Value)
Traffic_NO[45,4] <- TrafficTest_meanNO/ValueMeanNO*0.03628942

# 10 entry: Traffic_NO HAWKESBURY
HawksburyNO_NA <-Traffic_NO %>%
  filter(LGA == "HAWKESBURY") 
Hawksburymean <- mean(HawksburyNO_NA[2:9,4])

HAWKESBURYvalueMean <- mean(HawksburyNO_NA[2:9,3])
Traffic_NO[49,4] <- ceiling(Hawksburymean/HAWKESBURYvalueMean * 0.09271424)
Traffic_NO[58,4] <- ceiling(Hawksburymean/HAWKESBURYvalueMean * 0.13119231)
 
# 2 entry: Traffic_NO PORT MACQUARIE-HASTINGS
filter(Traffic_NO, LGA == "PORT MACQUARIE-HASTINGS")
Traffic_NO[121,4] <- ceiling(17724/0.3632899 * 0.4261314)

# 2 entry: Traffic_NO NORTH SYDNEY
filter(Traffic_NO, LGA == "NORTH SYDNEY")
Traffic_NO[108,4] <- ceiling(23231/5.276848 * 4.916940)

# 10 entry: Traffic_NO RANDWICK
filter(Traffic_NO, LGA == "RANDWICK")

RANDWICKNO_NA <- Traffic_NO %>%
  filter(LGA == "RANDWICK") %>%
  na.omit(Traffic_Test)
  
RANDWICKmean <- mean(RANDWICKNO_NA[,4])

RANDWICKvalueMean <- mean(RANDWICKNO_NA[,3])

Traffic_NO[126,4] <- ceiling(RANDWICKmean/RANDWICKvalueMean * 0.7749436)
Traffic_NO[127,4] <- ceiling(RANDWICKmean/RANDWICKvalueMean * 0.6390228)
Traffic_NO[128,4] <- ceiling(RANDWICKmean/RANDWICKvalueMean * 0.5151662)
Traffic_NO[129,4] <- ceiling(RANDWICKmean/RANDWICKvalueMean * 0.5116049)
Traffic_NO[131,4] <- ceiling(RANDWICKmean/RANDWICKvalueMean * 0.5165597)

# 10 entry: SHELLHARBOUR
filter(Traffic_NO, LGA == "SHELLHARBOUR")

SHELLHARBOURNO_NA <- Traffic_NO %>%
  filter(LGA == "SHELLHARBOUR") %>%
  na.omit(Traffic_Test)

SHELLHARBOURmean <- mean(SHELLHARBOURNO_NA[,4])

SHELLHARBOURvalueMean <- mean(SHELLHARBOURNO_NA[,3])
Traffic_NO[144,4] <- ceiling(SHELLHARBOURmean/SHELLHARBOURvalueMean * 0.11899081)

# WOLLONDILLY
filter(Traffic_NO, LGA == "WOLLONDILLY")

WOLLONDILLYNO_NA <- Traffic_NO %>%
  filter(LGA == "WOLLONDILLY") %>%
  na.omit(Traffic_Test)

WOLLONDILLYmean <- mean(WOLLONDILLYNO_NA[,4])

WOLLONDILLYvalueMean <- mean(WOLLONDILLYNO_NA[,3])
Traffic_NO[158,4] <- ceiling(WOLLONDILLYmean/WOLLONDILLYvalueMean * 0.1882504)

# 10 entry: Traffic_NO MAITLAND
filter(Traffic_NO, LGA == "MAITLAND")
MAITLANDNO_NA <- Traffic_NO %>%
  filter(LGA == "MAITLAND") %>%
  na.omit(Traffic_Test)
MAITLANDmean <- mean(MAITLANDNO_NA[,4])
MAITLANDvalueMean <- mean(MAITLANDNO_NA[,3])

Traffic_NO[88,4] <- ceiling(MAITLANDmean/MAITLANDvalueMean * 0.96699359)

# check for NA
Traffic_NO[c(which(is.na(Traffic_NO$Traffic_Test))),]

##########################NO EDA############################################
# Correlations and relationships
cor_matrix <- cor(Traffic_NO[2:4])
corrplot::corrplot(cor_matrix) # positive rel between value with traffic and year
# negative rel between year with traffic 

pairs(Traffic_NO[2:4]) # outliers

# bar chart for LGAs
Traffic_NO %<>%
  mutate(concat_LGA = substr(LGA, 1, 4))

ggplot(Traffic_NO, aes(x = reorder(concat_LGA, Value, FUN = median), y = Value)) + 
  geom_boxplot() +
  labs(title = "Relationship Between LGA and Nitric oxide",
       x = "LGA",
       y = "Nitric oxide (parts per million/ppm)")

# scatter plot
ggplot(Traffic_NO, aes(x = Traffic_Test, y = Value)) +
  geom_point() +
  labs(title = "Relationship Between Traffic Volume and Nitric oxide",
       x = "Traffic Volume (number of vehicles)",
       y = "Nitric oxide (parts per million/ppm)") +
  geom_text(label = Traffic_NO$concat_LGA) +
  geom_smooth(method = lm)

#removing outliers
Traffic_NO <- Traffic_NO[-c(which(Traffic_NO["LGA"] == "NORTH SYDNEY")),]

##################NO2 Cleaning#################################################
Traffic_NO2 <- Full_Data_merged %>%
  filter(Parameter.ParameterCode == "NO2") %>%
  select(LGA, Year, Value, Traffic_Test
  ) 

# finding NAs
Traffic_NO2[which(is.na(Traffic_NO2$Traffic_Test)),]

# # replacing NAs: assume same rate as other years of same LGA or 
# # average rate of all observations if only single observation for LGA

# 1 entry: COFFS HARBOUR
TrafficTest_meanNO2 <- mean(Traffic_NO2[-which(is.na(Traffic_NO2$Traffic_Test)),]$Traffic_Test)
ValueMeanNO2 <- mean(Traffic_NO2$Value)
Traffic_NO2[45,4] <- TrafficTest_meanNO2/ValueMeanNO2*0.2801797

# 10 entry: Traffic_NO2 HAWKESBURY
HawksburyNO2_NA <-Traffic_NO2 %>%
  filter(LGA == "HAWKESBURY") %>%
  na.omit(Traffic_Test)
Hawksburymean <- mean(HawksburyNO2_NA[,4])

HAWKESBURYvalueMean <- mean(HawksburyNO2_NA[,3])
Traffic_NO2[49,4] <- ceiling(Hawksburymean/HAWKESBURYvalueMean * 0.5368935)
Traffic_NO2[58,4] <- ceiling(Hawksburymean/HAWKESBURYvalueMean * 0.4663120)

# 2 entry: Traffic_NO2 PORT MACQUARIE-HASTINGS
filter(Traffic_NO2, LGA == "PORT MACQUARIE-HASTINGS")
Traffic_NO2[121,4] <- ceiling(17724/0.0939313 * 0.5985025)

# 2 entry: Traffic_NO2 NORTH SYDNEY
filter(Traffic_NO2, LGA == "NORTH SYDNEY")
Traffic_NO2[108,4] <- ceiling(23231/2.545677 * 2.344626)

# 10 entry: Traffic_NO2 RANDWICK
filter(Traffic_NO2, LGA == "RANDWICK")

RANDWICKNO2_NA <- Traffic_NO2 %>%
  filter(LGA == "RANDWICK") %>%
  na.omit(Traffic_Test)

RANDWICKmean <- mean(RANDWICKNO2_NA[,4])

RANDWICKvalueMean <- mean(RANDWICKNO2_NA[,3])

Traffic_NO2[126,4] <- ceiling(RANDWICKmean/RANDWICKvalueMean * 0.5881403)
Traffic_NO2[127,4] <- ceiling(RANDWICKmean/RANDWICKvalueMean * 0.8469609)
Traffic_NO2[128,4] <- ceiling(RANDWICKmean/RANDWICKvalueMean * 0.8012651)
Traffic_NO2[129,4] <- ceiling(RANDWICKmean/RANDWICKvalueMean * 0.6787424)
Traffic_NO2[131,4] <- ceiling(RANDWICKmean/RANDWICKvalueMean * 0.6515051)

# 10 entry: SHELLHARBOUR
filter(Traffic_NO2, LGA == "SHELLHARBOUR")

SHELLHARBOURNO2_NA <- Traffic_NO2 %>%
  filter(LGA == "SHELLHARBOUR") %>%
  na.omit(Traffic_Test)

SHELLHARBOURmean <- mean(SHELLHARBOURNO2_NA[,4])

SHELLHARBOURvalueMean <- mean(SHELLHARBOURNO2_NA[,3])
Traffic_NO2[144,4] <- ceiling(SHELLHARBOURmean/SHELLHARBOURvalueMean * 0.3814035)

# WOLLONDILLY
filter(Traffic_NO2, LGA == "WOLLONDILLY")

WOLLONDILLYNO2_NA <- Traffic_NO2 %>%
  filter(LGA == "WOLLONDILLY") %>%
  na.omit(Traffic_Test)

WOLLONDILLYmean <- mean(WOLLONDILLYNO2_NA[,4])

WOLLONDILLYvalueMean <- mean(WOLLONDILLYNO2_NA[,3])
Traffic_NO2[158,4] <- ceiling(WOLLONDILLYmean/WOLLONDILLYvalueMean * 0.3620133)

# 10 entry: Traffic_NO2 MAITLAND
filter(Traffic_NO2, LGA == "MAITLAND")
MAITLANDNO2_NA <- Traffic_NO2 %>%
  filter(LGA == "MAITLAND") %>%
  na.omit(Traffic_Test)
MAITLANDmean <- mean(MAITLANDNO2_NA[,4])
MAITLANDvalueMean <- mean(MAITLANDNO2_NA[,3])

Traffic_NO2[88,4] <- ceiling(MAITLANDmean/MAITLANDvalueMean * 0.7780095)

# check for na
Traffic_NO2[c(which(is.na(Traffic_NO2$Traffic_Test))),]

##########################NO2 EDA############################################
# Correlations and relationships
cor_matrix <- cor(Traffic_NO2[2:4])
corrplot::corrplot(cor_matrix) # positive rel between value with traffic 
# negative rel between year with traffic 

pairs(Traffic_NO2[2:4]) # outliers

# bar chart for LGAs
Traffic_NO2 %<>%
  mutate(concat_LGA = substr(LGA, 1, 4))

ggplot(Traffic_NO2, aes(x = reorder(concat_LGA, Value, FUN = median), y = Value)) + 
  geom_boxplot() +
  labs(title = "Relationship Between LGA and Nitrogen Dioxide ",
       x = "LGA",
       y = "Nitrogen Dioxide  (parts per million/ppm)")

# scatter plot
ggplot(Traffic_NO2, aes(x = Traffic_Test, y = Value)) +
  geom_point() +
  labs(title = "Relationship Between Traffic Volume and Nitrogen Dioxide ",
       x = "Traffic Volume (number of vehicles)",
       y = "Nitrogen Dioxide  (parts per million/ppm)") +
  geom_text(label = Traffic_NO2$concat_LGA) +
  geom_smooth(method = lm)

#removing outliers
Traffic_NO2 <- Traffic_NO2[-c(which(Traffic_NO2["LGA"] == "NORTH SYDNEY")),]

###############merging traffic_co traffic_NO and Traffic_NO2################
Traffic_CO <- rename(Traffic_CO, CO_value = Value)
Traffic_NO <- rename(Traffic_NO, NO_value = Value)
Traffic_NO2 <- rename(Traffic_NO2, NO2_value = Value)

Traffic_NO_NO2 <- cbind(Traffic_NO, Traffic_NO2$NO2_value)
Traffic_3parameters <- merge(Traffic_NO_NO2, Traffic_CO, all.x = TRUE)

######################regression################################
CO_traffic <- lm(Value ~ ., data = Traffic_CO)
summary(CO_traffic)$sigma # RSE
summary(CO_traffic)$r.squared # R ^ 2
summary(CO_traffic)$coefficients 
par(mfrow = c(2, 2))
plot(CO_traffic)

CO_traffic1 <- lm(Value ~ Year*Traffic_Test + LGA, data = Traffic_CO)
summary(CO_traffic1)
summary(CO_traffic1)$sigma # RSE
summary(CO_traffic1)$r.squared # R ^ 2
summary(CO_traffic1)$coefficients 

CO_traffic2 <- lm(Value ~ Year*Traffic_Test + Year*LGA, data = Traffic_CO)
summary(CO_traffic2)
summary(CO_traffic2)$sigma # RSE
summary(CO_traffic2)$r.squared # R ^ 2
summary(CO_traffic2)$coefficients 


CO_traffic3 <- lm(log(Value) ~ log(Year*Traffic_Test) + Year*LGA - Traffic_Test, data = Traffic_CO)
summary(CO_traffic3)
summary(CO_traffic3)$sigma # RSE
summary(CO_traffic3)$r.squared # R ^ 2
summary(CO_traffic3)$coefficients 

par(mfrow = c(2, 2))
plot(CO_traffic3)
