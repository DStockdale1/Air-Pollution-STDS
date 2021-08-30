library(tidyverse)
library(readxl)
library(xlsx)
library(tidyr)
library(dplyr)
library(lubridate)
library(zoo)
library(corrplot)

# Reading excel file

ghg <- read_excel("nggi-quarterly-update-december-2020-data-sources.xlsx", sheet = "Data Table 1A")
summary(ghg)
colnames(ghg)
ghg <- ghg[-1]
colnames(ghg) <- c("date_quarter","electricity_energy","stationary_energy","transport_energy","fugitive_emissions_energy","industrial_processes","agriculture","waste","total_excl_LULUCF","LULUCF","total_national_inventory")
ghg <- slice(ghg,-c(1:5, 84:85))
str(ghg)

# Change character to date format

ghg$date_quarter <- as.numeric(ghg$date_quarter)

ghg$date_quarter <- as.Date(ghg$date_quarter, origin="1899-12-30")

##ghg$date_quarter <- format(as.Date(ghg$date_quarter, origin="1899-12-30"),"%b-%Y")##


head(ghg$date_quarter)

str(ghg)

ghg[[2]] <- as.numeric(ghg[[2]])
ghg[[3]] <- as.numeric(ghg[[3]])
ghg[[4]] <- as.numeric(ghg[[4]])
ghg[[5]] <- as.numeric(ghg[[5]])
ghg[[6]] <- as.numeric(ghg[[6]])
ghg[[7]] <- as.numeric(ghg[[7]])
ghg[[8]] <- as.numeric(ghg[[8]])
ghg[[9]] <- as.numeric(ghg[[9]])
ghg[[10]] <- as.numeric(ghg[[10]])
ghg[[11]] <- as.numeric(ghg[[11]])

str(ghg)

mean <- data.frame(col1=c("electricity_energy","stationary_energy","transport_energy","fugitive_emissions_energy","industrial_processes","agriculture","waste","LULUCF"),
                   col2=c(mean(ghg$electricity_energy),
                          mean(ghg$stationary_energy),
                          mean(ghg$transport_energy),
                          mean(ghg$fugitive_emissions_energy),
                          mean(ghg$industrial_processes),
                          mean(ghg$agriculture),
                          mean(ghg$waste),
                          mean(ghg$LULUCF)))

n <- ggplot(mean,aes(col1,col2))+ geom_point(colour="red",size=1, stroke=2)+
  
  theme(axis.text.x = element_text(angle=65, vjust=0.6, size=10), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
        
  )


n + xlab("Sector") + ylab("Emissions (Mt CO2-e)
 Mean")


corr <- cor(ghg[-c(1,9,11)])
corrplot(corr, method = "ellipse", type="upper", tl.cex=.7)


k <- ggplot(ghg, aes(x=date_quarter)) + 
  geom_line(aes(y = electricity_energy, colour = "electricity_energy")) + 
  geom_line(aes(y = stationary_energy, colour = "stationary_energy")) +
  geom_line(aes(y = transport_energy, colour = "transport_energy")) +
  geom_line(aes(y = fugitive_emissions_energy, colour = "fugitive_emissions_energy")) +
  geom_line(aes(y = industrial_processes, colour = "industrial_processes")) +
  geom_line(aes(y = agriculture, colour = "agriculture")) +
  geom_line(aes(y = waste, colour = "waste")) +
  geom_line(aes(y = LULUCF, colour = "LULUCF")) +
  scale_x_date(date_breaks  ="3 months",date_labels = "%b-%y")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
  )

k+ ylab("Emissions (Mt CO2-e)")













ghg[,-c(1,9,11)] %>% mutate_all(funs(sum), na.rm = TRUE) %>%
  gather(key=Sector, value=Emissions) %>% 
  ggplot(aes(x= Sector,fill=Sector)) +
  geom_bar(aes(x =reorder(Sector,-Emissions), y = Emissions), position = "dodge", stat = "identity") +
  labs(title = "GHG Emissions by Sector") + ylab("Emissions (Mt CO2-e)")
