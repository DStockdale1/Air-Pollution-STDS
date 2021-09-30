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
Full_Data_merged_NO2 = filter(Full_Data_merged, Full_Data_merged$Parameter.ParameterCode == "NO2")

str(Full_Data_merged_NO2)
Full_Data_merged_NO2$LGA = as.factor(Full_Data_merged_NO2$LGA)

# Delete some outliers
boxplot(Full_Data_merged_NO2$Value)
Full_Data_merged_NO2 = filter(Full_Data_merged_NO2, Full_Data_merged_NO2$Value < 2)

cor(Full_Data_merged_NO2$Value , Full_Data_merged_NO2$population_Test)

plot(Full_Data_merged_NO2$Value , Full_Data_merged_NO2$population_Test, main = "NO2 vs Population Regression model")

NO2_linear_regression <- lm(Value ~ population_Test + LGA, data = Full_Data_merged_NO2)
NO2_linear_regression
summary(NO2_linear_regression)
abline(NO2_linear_regression, col="red", lwd=2)

par(mfrow= c(2,2))
plot(NO2_linear_regression)

par(mfrow= c(1,1))


#--------
df <- data.frame(x.y = 250050050)
res <- predict(NO2_linear_regression, df)
cat("\nPredicted value of NO2 in population = 250050050")
print(res)



NO2_regression <- ggplot(Full_Data_merged_NO2, mapping = aes(x= population_Test  , y= Value))+  
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE, col = "red")+ 
  labs(x="Population",y="NO2 Value") + 
  theme_classic()
NO2_regression
