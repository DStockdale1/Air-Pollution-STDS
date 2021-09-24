library(magrittr)
library(mgsub)
"%notin%" <- Negate("%in%")
#######################population: combining with suburb_lga####################
# remove NSW total population
Population_Data1 <- Population_Data %>%
  filter(Region != "New South Wales")

# keep only LGA name in uppercase
Population_Data1$Region <- c(toupper(mgsub(Population_Data1$Region, c(' [(].*'),
                                           c(''))))

# Suburb_LGA data remove council
Suburb_LGA$lganame <- c(mgsub(Suburb_LGA$lganame, c(' COUNCIL.*'), c('')))

# population data rename to "lganame"
Population_Data1 <- rename(Population_Data1, lganame = Region)

# unique LGA in both datasets
unique(Suburb_LGA$lganame)
unique(Population_Data1$lganame)

# append matching LGA of both datasets and find no match lga
a <- Suburb_LGA$lganame
b <- Population_Data1$lganame

matching_lga <- Suburb_LGA[which(a %in% b), ]
no_match_lga <- Suburb_LGA[which(a %notin% b), ]

#unique values in suburb lga data and population data with no match
unique(no_match_lga$lganame)
no_match_population_lga <- Population_Data1[-which(b %in% a), ]$lganame
unique(no_match_population_lga)

# match LGA values
Population_Data1["lganame"][Population_Data1["lganame"] == "ALBURY"] <- "ALBURY CITY"
Population_Data1["lganame"][Population_Data1["lganame"] == "LITHGOW"] <- "LITHGOW CITY"
Population_Data1["lganame"][Population_Data1["lganame"] == "PARRAMATTA"] <- "CITY OF PARRAMATTA"
Population_Data1["lganame"][Population_Data1["lganame"] == "NAMBUCCA"] <- "NAMBUCCA VALLEY"
Population_Data1["lganame"][Population_Data1["lganame"] == "UPPER HUNTER SHIRE"] <- "UPPER HUNTER"
Population_Data1["lganame"][Population_Data1["lganame"] == "WARRUMBUNGLE SHIRE"] <- "WARRUMBUNGLE"
Population_Data1["lganame"][Population_Data1["lganame"] == "UNCORPORATED NSW" | 
                   "lganame" == "NO USUAL ADDRESS" | 
                   "lganame" == "MIGRATORY - OFFSHORE - SHIPPING"] <- "UNINCORPORATED"
Suburb_LGA["lganame"][Suburb_LGA["lganame"] == "LORD HOWE ISLAND - UNINCORPORATED AREA"] <- "UNINCORPORATED"

# removal of NAs
sum(is.na(Suburb_LGA$lganame))
Suburb_LGA <- Suburb_LGA[-c(which(is.na(Suburb_LGA$lganame))), ]

# merge population and suburb + LGA data 
Population_Data_merged <- merge(x = Suburb_LGA, y = Population_Data1, 
                                by = "lganame", all = TRUE)

# rename population columns
Population_Data_merged <- rename(Population_Data_merged, 
                                 population_number = Value)

# unique obversations
Population_Suburb <- Population_Data_merged %>%
  select(suburbname, Time, percentage, population_number) %>%
  unique()

###############################TRAFFIC##########################################
# using the chosen parameters for traffic
Traffic_Data %<>%
  filter(cardinal_direction_name == "BOTH" | 
           cardinal_direction_name == "NORTHBOUND AND SOUTHBOUND" |
           cardinal_direction_name == "EASTBOUND AND WESTBOUND") %>%
  filter(classification_type == "ALL VEHICLES" |
           classification_type == "UNCLASSIFIED") %>%
  filter(period == "ALL DAYS")

# upper suburb
Traffic_Data$suburb <- toupper(Traffic_Data$suburb)

# selecting relevant variables
Traffic_Data %<>% 
  select(- station_id, 
         - road_name,  
         - period)

# number of unique suburbs  in traffic data
summary(unique(Traffic_Data$suburb))

############################AQI######################################
# selecting relevant variables
AQI_Summarized_Data %<>%
  select(- Site_Id, 
         - Region, 
         - Parameter.ParameterDescription, 
         - Parameter.Units) 

# selecting chosen parameters
AQI_Summarized_Data %<>%
  filter(Parameter.ParameterCode == "CO" |
           Parameter.ParameterCode == "NO" |
           Parameter.ParameterCode == "NO2" |
           Parameter.ParameterCode == "OZONE" |
           Parameter.ParameterCode == "TEMP" |
           Parameter.ParameterCode == "PM10")

# rename sitename to suburb
AQI_Summarized_Data <- rename(AQI_Summarized_Data, suburb = SiteName)

# unique suburbs and year in AQI
unique(AQI_Summarized_Data$Year) #12
c <- unique(AQI_Summarized_Data$suburb) #56

# string year to numeric
AQI_Summarized_Data$Year <- as.numeric(AQI_Summarized_Data$Year)

# match AQI suburbs to suburb_lga
c <- unique(AQI_Summarized_Data$suburb)
d <- unique(Suburb_LGA$suburbname)

matching_sub <- AQI_Summarized_Data[which(c %in% d), ]
no_match_sub <- as.data.frame(unique(AQI_Summarized_Data[!(which(c %in% d)), ]$suburb))

matching_lga <- Suburb_LGA[which(a %in% b), ]
no_match_lga <- Suburb_LGA[which(a %notin% b), ]


