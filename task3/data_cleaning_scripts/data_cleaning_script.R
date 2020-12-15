library(tidyverse)
library(janitor)
library(here)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(assertr)


#List the sheet names contained in the .xls file
excel_sheets("raw_data/seabirds.xls")

#Reading in the raw seabird data, extracting the "Bird data by record ID" sheet and cleaning the names
bird_data <- read_excel("raw_data/seabirds.xls", sheet = "Bird data by record ID") %>% clean_names()

#Looking at the variable names and types for bird_data
names(bird_data)
glimpse(bird_data)
dim(bird_data)

#Extracting the "Ship data by record ID" sheet from .xls file and cleaning the names
ship_data <- read_excel("raw_data/seabirds.xls", sheet = "Ship data by record ID") %>% clean_names()

#Looking at the variable names and types for ship_data
names(ship_data)
glimpse(ship_data)
dim(ship_data)

#Joining the two sheets "Ship data by record ID" and "Bird data by record ID" together so all rows of both sheets are kept
seabirds_data <- full_join(ship_data, bird_data, by = "record_id") %>% 
  #Renaming column names
  rename(bird_common_name = species_common_name_taxon_age_sex_plumage_phase,
         bird_scientific_name = species_scientific_name_taxon_age_sex_plumage_phase,
         number_of_birds = count)

#An overall view shows that the data is about sightings of bird species by ships between 1969 and 1990.

names(seabirds_data)

seabirds_data <- seabirds_data %>% 
#Keeping the following columns: bird's common name, scientific name, species abbreviation, latitude, record id.
select(record_id, bird_common_name, bird_scientific_name, species_abbreviation, lat, long, number_of_birds, date) 

#Finding any missing values - 1st Stage
seabirds_data %>% 
  summarise(across(.fns = ~ sum(is.na(.x))))

#Checking for missing values specifically in bird_scientific_name
missing_values <- seabirds_data %>% 
  filter(is.na(bird_scientific_name))

#Delete records where bird_common_name states "[NO BIRDS RECORDED]"
seabirds_data <- 
filter(seabirds_data, bird_common_name != "[NO BIRDS RECORDED]")

#Finding any missing values - 2nd Stage
seabirds_data %>% 
  summarise(across(.fns = ~ sum(is.na(.x))))

missing_values <- seabirds_data %>% 
  filter(is.na(bird_scientific_name))

# Replace NA's in bird_scientific_name column with "Unknown"
seabirds_data <-  seabirds_data %>% 
  mutate(bird_scientific_name = replace_na(bird_scientific_name, "Unknown")) %>% 
#Replacing missing values with zeros for latitude and longitude columns
  mutate(lat = coalesce(lat, 0),
         long = coalesce(long, 0)) %>% 
#Putting in checks using assertive programming to ensure latitude and longitude are valid values. (Latitude between -90 and 90, longitude between -180 and 180).
verify(lat >= -90 & lat <= 90) %>% 
  verify(long >= -180 & long <= 180)

#Finding any missing values - 3rd Stage
seabirds_data %>% 
  summarise(across(.fns = ~ sum(is.na(.x))))
#The above check showed there are missing values in count column. 
#Taking a closer look.
missing_values <- seabirds_data %>% 
  filter(is.na(number_of_birds))
#There are many rows of missing count values - drop these.
seabirds_data <- seabirds_data %>% 
  drop_na()

#Writing the cleaned data to a csv file.
write_csv(seabirds_data, "clean_data/seabirds_data.csv")
