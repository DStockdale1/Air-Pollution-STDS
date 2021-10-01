library(tidyverse)
library(magrittr)

load("Full_Data_Merged.Rda")

OZONE_Filtered_Data <- Full_Data_merged %>% 
  filter(!is.na(Traffic_Test), Parameter.ParameterCode == "OZONE") %>% 
  select(-population_Test)

ggplot(OZONE_Filtered_Data, aes(x = Traffic_Test, y = Value)) +
  geom_point()

cor(OZONE_Filtered_Data$Value, OZONE_Filtered_Data$Traffic_Test)

OZONE_slm <- lm(Value ~ Traffic_Test, data = OZONE_Filtered_Data)

ggplot(data = OZONE_Filtered_Data, aes(x = Traffic_Test, y = Value)) + geom_point()+ 
  geom_smooth(method = 'lm') +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Ozone vs Recorded Traffic", x = "Recorded Traffic (count)", y = "OZONE (pphm)")

plot(OZONE_slm) #Scale location!!!