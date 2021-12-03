#essential libraries
library(tidyverse)
library(janitor)
library(lubridate)
#additional libraries 
library(sf)
library(tigris)
library(tidycensus)
library(censusxy)
library(parallel)


## census api key
census_api_key("82c33dd2db316b78367d3476bcb90d2abe65a69a", install = "TRUE", overwrite = TRUE)


police_shootings <- read_csv("data/data-police-shootings-master/fatal-police-shootings-data.csv") %>%
  filter(!is.na(longitude)) 


# Rows 
rows <- c(1:nrow(police_shootings))

#rows <- c(1:2)
## Define function to get geocodes

get_geocodes <- function(row_number) {
  
  row_df <- police_shootings %>%
    slice(row_number)
  
  #store lat and long values
  longitude <- row_df$longitude
  latitude <- row_df$latitude
  
  # Get census results
  census_results <- cxy_geography(longitude, latitude, benchmark = "Public_AR_Current",vintage = "ACS2019_Current") 
  
  if (!is.null(census_results)) {
    
    census_results <- census_results %>%
      select(Census.Tracts.GEOID) %>%
      clean_names()
    
    row_df <- row_df %>%
      bind_cols(census_results) 

    print(paste0("finished ", row_number, " ", Sys.time()))
    
    filepath <- paste0("data_new_geocodes/geocoded_results_", str_pad(row_number, side="left",width=5, pad="0"), ".rds")
    write_rds(row_df, filepath)
    print(paste0("finished ",row_number))
      
      
  } else {
    print(paste0("No geocode found", row_number, " ", Sys.time()))
    
  } 
}

mclapply(rows,get_geocodes,mc.cores=8)

# Bind data together
geocoded_rds_files_list <- list.files("data_new_geocodes/")

all_geocoded_shootings <- tibble()

bind_geocoded_rows <- function(file) {
  
  temp_df <- read_rds(paste0("data_new_geocodes/", file))
  
  all_geocoded_shootings <<- all_geocoded_shootings %>%
    bind_rows(temp_df)
}

lapply(geocoded_rds_files_list, bind_geocoded_rows)
  
write_rds(all_geocoded_shootings,"all_geocoded_shootings.rds")  
