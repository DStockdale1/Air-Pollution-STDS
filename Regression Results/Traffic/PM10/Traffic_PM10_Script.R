library(tidyverse)
library(magrittr)

load("Full_Data_Merged.Rda")

PM10_Filtered_Data <- Full_Data_merged %>% 
  filter(!is.na(Traffic_Test), Parameter.ParameterCode == "PM10") %>% 
  select(-population_Test)

ggplot(PM10_Filtered_Data, aes(x = Traffic_Test, y = Value)) +
  geom_point()

cor(PM10_Filtered_Data$Value, PM10_Filtered_Data$Traffic_Test)

PM10_slm <- lm(Value ~ Traffic_Test, data = PM10_Filtered_Data)

ggplot(data = PM10_Filtered_Data, aes(x = Traffic_Test, y = Value)) + geom_point()+ 
  geom_smooth(method = 'lm') +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "PM10 vs Recorded Traffic", y = expression(PM10~(~mu~g/m^{"3"})), x = "Recorded Traffic (count)")

plot(PM10_slm) #Scale location!!!


