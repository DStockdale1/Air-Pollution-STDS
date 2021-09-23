library(magrittr)
library(mgsub)

# remove NSW total population
Population_Data1 <- Population_Data %>%
  filter(Region != "New South Wales")

# keep only LGA name in uppercase
Population_Data1$Region <- c(toupper(mgsub(Population_Data1$Region, c(' [(].*'), c(''))))

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

matching_lga <- Suburb_LGA[which(a %in% b),]
no_match_Lga <- Suburb_LGA[-which(a %in% b),]

#unique values in suburb lga data and population data with no match
unique(no_match_Lga$lganame)
no_match_population_Lga <- Population_Data1[-which(b %in% a),]$lganame
unique(no_match_population_Lga)

# match LGA values
Population_Data1["lganame"][Population_Data1["lganame"] == "ALBURY"] <- "ALBURY CITY"
Population_Data1["lganame"][Population_Data1["lganame"] == "LITHGOW"] <- "LITHGOW CITY"
Population_Data1["lganame"][Population_Data1["lganame"] == "PARRAMATTA"] <- "CITY OF PARRAMATTA"
Population_Data1["lganame"][Population_Data1["lganame"] == "NAMBUCCA"] <- "NAMBUCCA VALLEY"
Population_Data1["lganame"][Population_Data1["lganame"] == "UPPER HUNTER SHIRE"] <- "UPPER HUNTER"
Population_Data1["lganame"][Population_Data1["lganame"] == "UNCORPORATED NSW" | 
                   "lganame" == "NO USUAL ADDRESS" | 
                   "lganame" == "MIGRATORY - OFFSHORE - SHIPPING"] <- "UNINCORPORATED"
Suburb_LGA["lganame"][Suburb_LGA["lganame"] == "LORD HOWE ISLAND - UNINCORPORATED AREA"] <- "UNINCORPORATED"

# removal of NAs
sum(is.na(Suburb_LGA$lganame))
Suburb_LGA <- Suburb_LGA[-c(which(is.na(Suburb_LGA$lganame))),]

# merge population and suburb + LGA data 
Population_Data_merged <- merge(x = Suburb_LGA, y = Population_Data1, by = "lganame", all = TRUE)

# rename population columns
Population_Data_merged <- rename(Population_Data_merged, population_number = Value)

# unique obversations
Population_Suburb <- Population_Data_merged %>%
  select(suburbname, Time, percentage, population_number) %>%
  unique()

# unique obvs  
toupper(Traffic_Data_location)

