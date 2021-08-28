library(tidyverse)

Population_Data <- read_csv("Datasets to merge/Raw Data/Population_Data.csv", 
                            col_types = cols(MEASURE = col_skip(), 
                            Measure = col_skip(), REGIONTYPE = col_skip(), 
                            `Geography Level` = col_skip(), FREQUENCY = col_skip(), 
                            Frequency = col_skip(), TIME = col_skip(), 
                            `Flag Codes` = col_skip(), Flags = col_skip()))

