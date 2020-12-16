library(tidyverse)
library(dplyr)
library(readxl)
library(janitor)
library(stringr)

#List the sheet names contained in the .xls files
excel_sheets("raw_data/boing-boing-candy-2015.xlsx")
excel_sheets("raw_data/boing-boing-candy-2016.xlsx")
excel_sheets("raw_data/boing-boing-candy-2017.xlsx")
#Looks like each file only has one sheet.

#Reading in data set files and cleaning names
candy_2015_raw <- read_excel("raw_data/boing-boing-candy-2015.xlsx") %>% clean_names() %>% 
#Renaming the column names for 2015
  rename(age = how_old_are_you) %>% 
rename(trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself)

candy_2016_raw <- read_excel("raw_data/boing-boing-candy-2016.xlsx") %>% clean_names() %>% 
  rename(age = how_old_are_you) %>% 
  rename(trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself) %>% 
  rename(gender = your_gender) %>% 
  rename(country = which_country_do_you_live_in) 

candy_2017_raw <- read_excel("raw_data/boing-boing-candy-2017.xlsx") %>% clean_names() %>% 
  str_replace_all("q[0-9]+_", "", names(candy_2017_raw)) %>% 
  rename(trick_or_treating = q1_going_out) %>% 
  rename(gender = q2_gender) %>% 
  rename(age = q3_age) %>% 
  rename(country = q4_country)