

#essential libraries
library(tidyverse)
library(janitor)
library(lubridate)
#additional libraries 
library(sf)
library(tigris)
library(tidycensus)
library(censusxy)


## census api key
census_api_key("0105753aba54d204fb6c33c44044ae59449b3773", install = "TRUE", overwrite = TRUE)


police_shootings <- read_csv("data/fatal-police-shootings-data.csv") %>%
  filter(!is.na(longitude)) 

## ok so it seems like we can only do one lat long at a time so... for loop? idk lets try

## first create empty data frame

police_shootings_sf <- tibble()

#for (row_number in 1:nrow(police_shootings)) {
for (row_number in 3501:6414) {  
  #this makes a dataframe for each
  row_df<- police_shootings %>%
    slice(row_number)
  
  #store lat and long values
  longitude <- row_df$longitude
  latitude <- row_df$latitude
  census_results <- cxy_geography(longitude, latitude) %>%
    select(Census.Tracts.GEOID) %>%
    clean_names()
  
  row_df <- row_df %>%
    bind_cols(census_results) 
  
  
  #inding some rows
  police_shootings_sf <- police_shootings_sf %>%
    bind_rows(row_df) 
  
  print(paste0("finished ", row_number, " ", Sys.time()))
  
  if (row_number%%500 == 0) {
    filepath <- paste0("data/geocoded_results_", row_number, ".rds")
    write_rds(police_shootings_sf, filepath)
    police_shootings_sf <- as_tibble()
    
  }
  
  
}
