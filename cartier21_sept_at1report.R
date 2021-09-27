library(magrittr)
library(mgsub)
"%notin%" <- Negate("%in%")

# Data Data ---------------------------------------------------------------

Population_Data <- read_csv("Datasets to merge/Raw Data/Population_Data.csv", 
                            col_types = cols(MEASURE = col_skip(), 
                                             Measure = col_skip(), REGIONTYPE = col_skip(), 
                                             `Geography Level` = col_skip(), FREQUENCY = col_skip(), 
                                             Frequency = col_skip(), TIME = col_skip(), 
                                             `Flag Codes` = col_skip(), Flags = col_skip()))

Traffic_Data <- read_csv("Traffic_Volume_Viewer_2007-2021.csv", 
                         col_types = cols(the_geom = col_skip()))


Suburb_LGA <- read_xlsx("Suburb_and_LGA_PercentageArea.xlsx")

load("Datasets to merge/Raw Data/AQI_Data.Rda")
AQI_Site_LGA_Key <- read_csv("AQI_Data/AQI_Site_LGA_Key.csv")
load("Datasets to merge/Raw Data/AQI Data/Air Quality Site Details.Rda")



# Population -> Suburb_lga ----------------------------------------------------

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
{Population_Data1["lganame"][Population_Data1["lganame"] == "ALBURY"] <- "ALBURY CITY"
Population_Data1["lganame"][Population_Data1["lganame"] == "LITHGOW"] <- "LITHGOW CITY"
Population_Data1["lganame"][Population_Data1["lganame"] == "PARRAMATTA"] <- "CITY OF PARRAMATTA"
Population_Data1["lganame"][Population_Data1["lganame"] == "NAMBUCCA"] <- "NAMBUCCA VALLEY"
Population_Data1["lganame"][Population_Data1["lganame"] == "UPPER HUNTER SHIRE"] <- "UPPER HUNTER"
Population_Data1["lganame"][Population_Data1["lganame"] == "WARRUMBUNGLE SHIRE"] <- "WARRUMBUNGLE"
Population_Data1["lganame"][Population_Data1["lganame"] == "UNCORPORATED NSW" | 
                   "lganame" == "NO USUAL ADDRESS" | 
                   "lganame" == "MIGRATORY - OFFSHORE - SHIPPING"] <- "UNINCORPORATED"
Suburb_LGA["lganame"][Suburb_LGA["lganame"] == "LORD HOWE ISLAND - UNINCORPORATED AREA"] <- "UNINCORPORATED"}

# removal of NAs
sum(is.na(Suburb_LGA$lganame))
Suburb_LGA <- Suburb_LGA[-c(which(is.na(Suburb_LGA$lganame))), ]

# merge population and suburb + LGA data 
Population_Data_merged <- merge(x = Suburb_LGA, y = Population_Data1, 
                                by = "lganame", all = TRUE)

# rename population columns
Population_Data_merged <- rename(Population_Data_merged, 
                                 population_number = Value, LGA = lganame, Year = Time)


Population_Data_merged_1 <- Population_Data_merged %>% 
  mutate(population_Test = round((population_number * percentage / 100), 0)) %>% 
  select(LGA, Year, population_Test) %>% 
  group_by(LGA, Year) %>% 
  summarise(population_Test = sum(population_Test))


AQI_merged_Data <- AQI_Summarized_Data %>% 
  filter(Parameter.ParameterCode == "CO" |
           Parameter.ParameterCode == "NO" |
           Parameter.ParameterCode == "NO2" |
           Parameter.ParameterCode == "OZONE" |
           Parameter.ParameterCode == "TEMP" |
           Parameter.ParameterCode == "PM10") %>% 
  merge(df_site_details) %>% 
  merge(AQI_Site_LGA_Key) %>% 
  mutate(LGA = toupper(mgsub(LGA, c(' [(].*'),c(''))), Year = as.integer(Year)) %>% 
  group_by(LGA, Year, Parameter.ParameterCode, Parameter.ParameterDescription, Parameter.Units) %>% 
  summarize(Value = mean(Value)) %>% 
  arrange(Parameter.ParameterCode, LGA, Year) %>% 
  inner_join(Population_Data_merged_1, by = c("LGA", "Year"))



# unique obversations
#Population_Suburb <- Population_Data_merged %>%
#  select(suburbname, Time, percentage, population_number) %>%
#  unique()

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

Traffic_Data_LGA <- Traffic_Data %>% 
  select(suburb, year, traffic_count) %>%
  rename(suburbname = suburb) %>% 
  inner_join(Suburb_LGA) %>% 
  mutate(Traffic_Test = round((traffic_count * percentage) / 100, 0)) %>% 
  select(lganame, year, Traffic_Test) %>% 
  rename(LGA = lganame, Year = year) %>% 
  group_by(LGA, Year) %>% 
  summarise(Traffic_Test = sum(Traffic_Test))


Full_Data_merged <- AQI_merged_Data %>% 
  merge(Traffic_Data_LGA, by = c("LGA", "Year"), all = TRUE)
  

# number of unique suburbs  in traffic data
summary(unique(Traffic_Data$suburb))

save(Full_Data_merged, file="C:/Users/willi/OneDrive/Desktop/Air-Pollution/Full_Data_Merged.Rda")



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


