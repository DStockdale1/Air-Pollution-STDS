# Air Quality Data Connection

# This is not to be used for downloading the full dataset. When loading all sites and all parameters it takes 
# ~70 seconds / month of data and so will take ~7 hours to download. I will periodically upload the full dataset 
# as an RDA file. If you wish to make smaller queries this can be used and this can be used to familiarise yourself
# with the structure of the data before I can fully download the full dataset.


# Packages ---------------------------------------------------------------
library(tidyverse)
library(httr)
library(jsonlite)
library(svDialogs)
library(qdapTools)
options(scipen = 100)


# Site Information --------------------------------------------------------
path_site_details <- 'https://data.airquality.nsw.gov.au/api/Data/get_SiteDetails' # URL for obtaining the Site Details table

df_site_details <- {path_site_details %>% 
    GET(body = list(),
        add_headers(`Content-Type`="application/json")) %>% 
    content(as = "text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE) %>% 
    data.frame()} # Loading and processing the JSON into a data frame
rm(path_site_details)

# Parameter Information ---------------------------------------------------
path_param_details <- 'https://data.airquality.nsw.gov.au/api/Data/get_ParameterDetails' # URL for obtaining the Parameter Details table

df_param_details <- {path_param_details %>% 
    GET(body = list(),
        add_headers(`Content-Type`="application/json")) %>% 
    content(as = "text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE) %>% 
    data.frame()}

rm(path_param_details)

# Observations ------------------------------------------------------------
path_historical_obs <- 'https://data.airquality.nsw.gov.au/api/Data/get_observations' # URL for obtaining the observations table

{
  Parameters <- unique(df_param_details$ParameterCode) # Possible Values for Parameters
  Sites <- unique(df_site_details$SiteName) # Possible Values for Sites
} # Possible Values for selections

{
  Parameters <- {dlg_list(
    Parameters,
    preselect = Parameters,
    multiple = TRUE,
    title = NULL,
    gui = .GUI
  )$res} # Popup to select parameters
  Sites <- {data.frame(dlg_list(
    Sites,
    preselect = Sites,
    multiple = TRUE,
    title = NULL,
    gui = .GUI)$res) %>% 
      rename("SiteName" = dlg_list.Sites..preselect...Sites..multiple...TRUE..title...NULL..) %>% 
      lookup(key.match = df_site_details[,2:1])} # Popup to select Sites
  StartDate <- {dlg_input(message = "Enter Start Date [yyyy-mm-dd] (>= 1995-01-25)", default = "1995-01-25", gui = .GUI)$res
  } # Popup to select Start Date
  EndDate <- {dlg_input(message = paste("Enter End Date [yyyy-mm-dd] (<= ", as.Date(Sys.Date()), ")"), default = as.Date(Sys.Date()), gui = .GUI)$res
  } # Popup to select End Date
} # Popups to select values


Query <- {paste('{"Parameters": [ "', paste(Parameters, collapse = "\", \""), '" ],
                       "Sites": [ ', paste(Sites, collapse = ", "), ' ],
                       "StartDate": "', StartDate, '", 
                       "EndDate": "', EndDate, '", 
                       "Categories": [ "Averages" ],
                       "SubCategories": [ "Hourly" ],
                       "Frequency": [ "Hourly average" ]}', 
                sep = "")} # query sent to source in 'body' argument  

df_observations <- {path_historical_obs %>% 
    POST(body = Query,
         add_headers(c(`accept` = 'application/json',
                       `Content-Type` = 'application/json'))) %>% 
    content(as = "text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE) %>% 
    data.frame()} # Load JSON as data frame

head(df_observations)