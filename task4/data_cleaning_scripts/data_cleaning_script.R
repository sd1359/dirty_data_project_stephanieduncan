library(tidyverse)
library(dplyr)
library(readxl)
library(janitor)
library(tidyr)
library(stringr)
library(assertr)
library(here)

#Reading in data set files and cleaning names
candy_2015_raw <- read_excel(here("raw_data/boing-boing-candy-2015.xlsx")) %>% clean_names() %>% 
#Renaming the column names for 2015
  rename(age = how_old_are_you) %>% 
rename(trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself) %>% mutate(year = 2015) 

#Reading in data set files and cleaning names
candy_2016_raw <- read_excel(here("raw_data/boing-boing-candy-2016.xlsx")) %>% clean_names() %>% 
  #Renaming the column names for 2016
  rename(age = how_old_are_you) %>% 
  rename(trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself) %>%
  rename(gender = your_gender) %>% 
  rename(country = which_country_do_you_live_in) %>% 
  mutate(year = 2016)

#Reading in data set files and cleaning names
candy_2017_raw <- read_excel(here("raw_data/boing-boing-candy-2017.xlsx")) %>% 
  clean_names() %>% 
  #Renaming the column names for 2017
  mutate(year = 2017) %>% 
  rename(trick_or_treating = q1_going_out) %>% 
  rename(gender = q2_gender) %>% 
  rename(age = q3_age) %>% 
  rename(country = q4_country) 

#Removing question numbers at the beginning of each column name.
names(candy_2017_raw) <- str_remove_all(colnames(candy_2017_raw), "q[0-9]_")

#Using pivot longer on 2015 dataset and putting candy names into column "candy" and ratings under column "rating"
candy_15 <- candy_2015_raw %>% 
  pivot_longer(butterfinger:york_peppermint_patties, names_to = "candy", values_to = "rating") %>%
  #Adding missing gender and country columns which are required for analysis
  mutate(gender = NA,
         country = NA) %>% 
#Selecting relevant columns
  select(year, trick_or_treating, age, gender, country, candy, rating)

#Using pivot longer on 2016 dataset and putting candy names into column "candy" and ratings under column "rating"
  candy_16 <- candy_2016_raw %>% 
    pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy", values_to = "rating") %>% 
    select(year, trick_or_treating, age, gender, country, candy, rating)
  
  #Using pivot longer on 2017 dataset and putting candy names into column "candy" and ratings under column "rating"
  candy_17 <- candy_2017_raw %>% 
    pivot_longer("100_grand_bar":"york_peppermint_patties", names_to = "candy", values_to = "rating") %>% 
    #selecting relevant columns
    select(year, trick_or_treating, age, gender, country, candy, rating)
  
  #Joining all three datasets
  halloween_candy <- rbind(candy_15, candy_16, candy_17) %>% 
  #Converting country, gender and rating values to lowercase
    mutate(country = str_to_lower(country),
           gender = str_to_lower(gender),
           rating = str_to_lower(rating))
  
  #Get a list of all country data
  #countries_list <- halloween_candy_clean %>% 
    #distinct(country)
  
  #Clean countries to a uniform format
  country_usa_raw <- c(
    "united states of america",
    "united states",
    "us",
    "ussa",
    "u.s.a.",
    "murica",
    "usa!",
    "usa (i think but it's an election year so who can really tell)",
    "u.s.",
    "america",
    "units states",
    "usa usa usa",
    "the best one - usa",
    "usa! usa! usa!",
    "the yoo ess of aaayyyyyy",
    "usa!!!!!!",
    "usa! usa!",
    "united sates",
    "trumpistan",
    "merica",
    "united stetes",
    "usa usa usa usa",
    "the republic of cascadia",
    "united  states of america",
    "united state",
    "united staes",
    "usausausa",
    "us of a",
    "unites states",
    "the united states",
    "north carolina",
    "unied states",
    "u s",
    "the united states of america",
    "unite states",
    "usa? hard to tell anymore..",
    "'merica",
    "usas",
    "pittsburgh",
    "new york",
    "california",
    "i pretend to be from canada, but i am really from the united states.",
    "united stated",
    "ahem....amerca",
    "new jersey",
    "united ststes",
    "united statss",
    "murrika",
    "usaa",
    "alaska",
    "n. america",
    "n. america",
    "u s a",
    "united statea",
    "usa usa usa!!!!",
    "cascadia",
    "panama",
    #assuming eua is acronym for "Estados Unidos da América"
    "eua"
  )
  
country_uk_raw <- c(
  "england",
  "united kingdom",
  "scotland",
  "endland",
  "u.k.",
  "united kindom"
)

unknown_countries <- c(
  "a tropical island south of the equator",
  "neverland",
  "this one",
  "usa (i think but it's an election year so who can really tell)",
  "there isn't one for old men",
  "one of the best ones",
  "somewhere",
  "god's country",
  "sub-canadian north america... 'merica",
  "see above",
  "not the usa or canada",
  "denial",
  "unhinged states",
  "earth",
  "insanity lately",
  "a",
  "can",
  "canae",
  "ud",
  "narnia",
  "subscribe to dm4uz3 on youtube",
  "i don't know anymore",
  "fear and loathing",
  "europe",
  "korea",
  "soviet canuckistan",
  "atlantis"
)

#Recoding the country column data
halloween_candy_clean <- halloween_candy %>% 
  mutate(
    country = if_else(country == "españa", "spain", country),
    country = if_else(country == "the netherlands", "netherlands", country),
    country = if_else(country %in% country_uk_raw, "uk", country),
    country = if_else(country %in% unknown_countries, "unknown", country),
    country = if_else(country %in% country_usa_raw, "usa", country),
    country = if_else(country == "brasil", "brazil", country),
    #Convert numeric data in country column to "unknown"
    country = if_else(str_detect(country, "[0-9]"), "unknown", country)
  ) 

#Convert unknown countries to na
halloween_candy_clean <- halloween_candy_clean %>% 
  mutate(
    country = na_if(country, "unknown")
  )

#Checking country list
#country_list <- halloween_candy_clean %>% 
#distinct(country)

#Checking values for trick_or_treating column
#halloween_candy_clean %>% 
  #distinct(trick_or_treating)

#Converting age column from character to numeric
halloween_candy_clean <- halloween_candy_clean %>% 
  mutate(age = as.numeric(age))

#Checking if there are values for ages which are outwith reasonable range using assertive programming
halloween_candy_clean %>% 
  verify(age <= 122 & age > 2)

#Removing unreasonable ages from data
halloween_candy_clean <- halloween_candy_clean %>% 
  filter(age <= 122 & age > 2)

#Removing unknown candy data by creating a list
unknown_candy <- c(
  "anonymous_brown_globs_that_come_in_black_and_orange_wrappers",
  "vials_of_pure_high_fructose_corn_syrup_for_main_lining_into_your_vein",
  "candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants",
  "cash_or_other_forms_of_legal_tender",
  "dental_paraphenalia",
  "generic_brand_acetaminophen",
  "broken_glow_stick",
  "creepy_religious_comics_chick_tracts",
  "hugs_actual_physical_hugs",
  "lapel_pins",
  "minibags_of_chips",
  "joy_joy_mit_iodine",
  "nown_laters",
  "mint_juleps",
  "chick_o_sticks_we_don_t_know_what_that_is",
  "swedish_fish",
  "vicodin",
  "white_bread",
  "whole_wheat_anything",
  "bonkers_the_board_game",
  "third_party_m_ms",
  "person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes",
  "sourpatch_kids_i_e_abominations_of_nature",
  "green_party_m_ms",
  "independent_m_ms",
  "abstained_from_m_ming",
  "real_housewives_of_orange_county_season_9_blue_ray",
  "sandwich_sized_bags_filled_with_boo_berry_crunch",
  "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes",
  "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes",
  "mary_janes",
  "peterson_brand_sidewalk_chalk",
  "chardonnay",
  "sweetums_a_friend_to_diabetes",
  "spotted_dick"
)
  
#Removing unknown candy data and renaming existing candy values
halloween_candy_clean <- halloween_candy_clean %>% 
  mutate(
    candy = if_else(candy == "x100_grand_bar", "100_grand_bar", candy),
    candy = if_else(candy %in% unknown_candy, "unknown", candy),
    candy = if_else(candy %in% c("jolly_rancher_bad_flavor", "jolly_ranchers_good_flavor"), "jolly_rancher", candy),
    candy = if_else(candy %in% c("smarties_american", "smarties_commonwealth"), "smarties", candy),
    candy = if_else(candy == "tolberone_something_or_other", "toblerone", candy)
  )

#Convert unknown candy to na
halloween_candy_clean <- halloween_candy_clean %>% 
  mutate(
    candy = na_if(candy, "unknown")
  ) %>% 
  filter(!is.na(candy))

#Writing to csv file
write_csv(halloween_candy_clean, "clean_data/halloween_candy.csv")

