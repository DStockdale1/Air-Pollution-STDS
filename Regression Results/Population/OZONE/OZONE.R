install.packages("dplyr")
install.packages("readr")
install.packages("Amelia")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("AER")

library("dplyr")
library("readr")
library("Amelia")
library("tidyverse")
library("magrittr")
library("AER")

#Load and filter dataset
Full_Data_merged
Full_Data_merged_OZONE = filter(Full_Data_merged, Full_Data_merged$Parameter.ParameterCode == "OZONE")

#----------------ozone/population/LGA categorical regression----------------------------------

str(Full_Data_merged_OZONE)
Full_Data_merged_OZONE$LGA = as.factor(Full_Data_merged_OZONE$LGA)

# Delete some outliers
boxplot(Full_Data_merged_OZONE$Value)
Full_Data_merged_OZONE = filter(Full_Data_merged_OZONE, Full_Data_merged_OZONE$Value > 1 &
                                                        Full_Data_merged_OZONE$Value < 3 )
# -0.1610036
cor(Full_Data_merged_OZONE$Value , Full_Data_merged_OZONE$population_Test)

# Multiple R-squared:  0.9325,	Adjusted R-squared:  0.922 
plot(Full_Data_merged_OZONE$Value , Full_Data_merged_OZONE$population_Test, main = "OZONE vs Population")

OZONE_linear_regression <- lm(Value ~ population_Test + LGA, data = Full_Data_merged_NO2)
OZONE_linear_regression
summary(OZONE_linear_regression)
abline(OZONE_linear_regression, col="red", lwd=2)

par(mfrow= c(2,2))
plot(OZONE_linear_regression)

par(mfrow= c(1,1))

df <- data.frame(x.y = 250050050)
res <- predict(ozone_linear_regression, df)
cat("\nPredicted value of OZONE in population = 250050050")
print(res)



Ozone_regression <- ggplot(OZONEP, mapping = aes(x= x.y , y= x.x))+  
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE)+ 
  labs(x="Population",y="OZONE Value") + 
  theme_classic()
Ozone_regression









































