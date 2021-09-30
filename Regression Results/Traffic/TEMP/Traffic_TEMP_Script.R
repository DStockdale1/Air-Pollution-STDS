library(tidyverse)
library(magrittr)

load("Full_Data_Merged.Rda")

TEMP_Filtered_Data <- Full_Data_merged %>% 
  filter(!is.na(Traffic_Test), Parameter.ParameterCode == "TEMP") %>% 
  select(-population_Test)

ggplot(TEMP_Filtered_Data, aes(x = Traffic_Test, y = Value)) +
  geom_point()

cor(TEMP_Filtered_Data$Value, TEMP_Filtered_Data$Traffic_Test)

TEMP_slm <- lm(Value ~ Traffic_Test, data = TEMP_Filtered_Data)

ggplot(data = TEMP_Filtered_Data, aes(x = Traffic_Test, y = Value)) + geom_point()+ 
  geom_smooth(method = 'lm') +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Temperature vs Recorded Traffic", x = "Recorded Traffic", y = expression("Temperature " ( degree*C)))

plot(TEMP_slm) #Scale location!!!
