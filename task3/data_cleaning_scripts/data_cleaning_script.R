library(tidyverse)
library(janitor)
library(here)
library(readr)
library(readxl)
library(dplyr)

#Testing where the top level of the project directory is
here::here()

#Reading in the raw seabird data
raw_seabirds_data <- read_xls(here("raw_data/seabirds.xls"))

#List the sheet names contained in the .xls file
excel_sheets("raw_data/seabirds.xls")

#Extracting the "Bird data by record ID" sheet from .xls file and cleaning the names
bird_data <- read_excel("raw_data/seabirds.xls", sheet = "Bird data by record ID") %>% clean_names()

#Getting the variable names and types for bird_data
names(bird_data)
glimpse(bird_data)
dim(bird_data)

#Extracting the "Ship data by record ID" sheet from .xls file and cleaning the names
ship_data <- read_excel("raw_data/seabirds.xls", sheet = "Ship data by record ID") %>% clean_names()

#Getting the variable names and types for ship_data
names(ship_data)
glimpse(ship_data)
dim(ship_data)

#Joining the two sheets "Ship data by record ID" and "Bird data by record ID" together so all rows of both sheets are kept
seabirds_data <- full_join(ship_data, bird_data, by = "record_id") %>% 
  #Renaming column names
  rename(bird_common_name = species_common_name_taxon_age_sex_plumage_phase,
         bird_scientific_name = species_scientific_name_taxon_age_sex_plumage_phase)

#Keeping the following columns: bird's common name, scientific name, species abbreviation, latitude, longitude, record id, 





