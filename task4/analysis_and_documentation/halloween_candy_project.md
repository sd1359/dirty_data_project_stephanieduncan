# Halloween Candy Project

## Analysing Data on Halloween Candy Ratings from 2015 to 2017

## The .R File
The objective of this project was to perform analysis on data obtained from a survey on Halloween candy between 2015 and 2017.

The main points of interest which were investigated are outlined below:

1. The total number of candy ratings given across the three years.
2. The average age of people who are going out trick or treating and the average age of people not going trick or treating?
3. For each of joy, despair and meh, which candy bar revived the most of these ratings?
4. How many people rated Starburst as despair?
5. What was the most popular candy bar by this rating system for each gender in the dataset?
6. What was the most popular candy bar in each year?
7. What was the most popular candy bar by this rating for people in US, Canada, UK and all other countries?


###Reading in 2015 data set files and cleaning names
`candy_2015_raw <- read_excel(here("raw_data/boing-boing-candy-2015.xlsx")) %>% clean_names() %>% `

###Renaming the column names for 2015
`  rename(age = how_old_are_you) %>% 
rename(trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself) %>% mutate(year = 2015) 
`
###Reading in 2016 data set files and cleaning names
`candy_2016_raw <- read_excel(here("raw_data/boing-boing-candy-2016.xlsx")) %>% clean_names() %>% 
  #Renaming the column names for 2016
  rename(age = how_old_are_you) %>% 
  rename(trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself) %>%
  rename(gender = your_gender) %>% 
  rename(country = which_country_do_you_live_in) %>% 
  mutate(year = 2016)`

###Reading in data set files and cleaning names
`candy_2017_raw <- read_excel(here("raw_data/boing-boing-candy-2017.xlsx")) %>% 
  clean_names() %>% 
  #Renaming the column names for 2017
  mutate(year = 2017) %>% 
  rename(trick_or_treating = q1_going_out) %>% 
  rename(gender = q2_gender) %>% 
  rename(age = q3_age) %>% 
  rename(country = q4_country) `

###Removing question numbers at the beginning of each column name.
`names(candy_2017_raw) <- str_remove_all(colnames(candy_2017_raw), "q[0-9]_")`

###Using pivot longer on 2015 dataset and putting candy names into column "candy" and ratings under column "rating"
`candy_15 <- candy_2015_raw %>% 
  pivot_longer(butterfinger:york_peppermint_patties, names_to = "candy", values_to = "rating") %>%
  #Adding missing gender and country columns which are required for analysis
  mutate(gender = NA,
         country = NA) %>% `
###Selecting relevant columns
  `select(year, trick_or_treating, age, gender, country, candy, rating)`

###Using pivot longer on 2016 dataset and putting candy names into column "candy" and ratings under column "rating"
  `candy_16 <- candy_2016_raw %>% 
    pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy", values_to = "rating") %>% 
    select(year, trick_or_treating, age, gender, country, candy, rating)`
  
###Using pivot longer on 2017 dataset and putting candy names into column "candy" and ratings under column "rating"
  `candy_17 <- candy_2017_raw %>% 
    pivot_longer("100_grand_bar":"york_peppermint_patties", names_to = "candy", values_to = "rating") %>% 
    #selecting relevant columns
    select(year, trick_or_treating, age, gender, country, candy, rating)`
  
###Joining all three datasets
  `halloween_candy <- rbind(candy_15, candy_16, candy_17) %>% `
###Converting country, gender and rating values to lowercase
    `mutate(country = str_to_lower(country),
           gender = str_to_lower(gender),
           rating = str_to_lower(rating))`
  
###Get a list of all country data
 ` countries_list <- halloween_candy_clean %>% 
    distinct(country)`
  
###Clean countries to a uniform format
  ``country_usa_raw <- c(
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
`    #assuming eua is acronym for "Estados Unidos da América"
  `  "eua"
  )`
  
``country_uk_raw <- c(
  "england",
  "united kingdom",
  "scotland",
  "endland",
  "u.k.",
  "united kindom"
)`

`unknown_countries <- c(
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
)`
`
###Recoding the country column data
``halloween_candy_clean <- halloween_candy %>% 
  mutate(
    country = if_else(country == "españa", "spain", country),
    country = if_else(country == "the netherlands", "netherlands", country),
    country = if_else(country %in% country_uk_raw, "uk", country),
    country = if_else(country %in% unknown_countries, "unknown", country),
    country = if_else(country %in% country_usa_raw, "usa", country),
    country = if_else(country == "brasil", "brazil", country),`
    #Convert numeric data in country column to "unknown"
    `country = if_else(str_detect(country, "[0-9]"), "unknown", country)
  ) `
`
###Convert unknown countries to na
`halloween_candy_clean <- halloween_candy_clean %>% 
  mutate(
    country = na_if(country, "unknown")
  )`

###Checking country list
`country_list <- halloween_candy_clean %>% 
distinct(country)`

###Checking values for trick_or_treating column
`halloween_candy_clean %>% 
  distinct(trick_or_treating)`

###Converting age column from character to numeric
`halloween_candy_clean <- halloween_candy_clean %>% 
  mutate(age = as.numeric(age))`

###Checking if there are values for ages which are outwith reasonable range using assertive programming
`halloween_candy_clean %>% 
  verify(age <= 122 & age > 2)`

###Removing unreasonable ages from data
`halloween_candy_clean <- halloween_candy_clean %>% 
  filter(age <= 122 & age > 2)`
  
###Removing unknown candy data by creating a list
`unknown_candy <- c(
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
)`
  
###Removing unknown candy data and renaming existing candy values
`halloween_candy_clean <- halloween_candy_clean %>% 
  mutate(
    candy = if_else(candy == "x100_grand_bar", "100_grand_bar", candy),
    candy = if_else(candy %in% unknown_candy, "unknown", candy),
    candy = if_else(candy %in% c("jolly_rancher_bad_flavor", "jolly_ranchers_good_flavor"), "jolly_rancher", candy),
    candy = if_else(candy %in% c("smarties_american", "smarties_commonwealth"), "smarties", candy),
    candy = if_else(candy == "tolberone_something_or_other", "toblerone", candy)
  )`

###Converting unknown candy to na
`halloween_candy_clean <- halloween_candy_clean %>% 
  mutate(
    candy = na_if(candy, "unknown")
  ) %>% 
  filter(!is.na(candy))`

###Writing to csv file
`write_csv(halloween_candy_clean, "clean_data/halloween_candy.csv")`


## The .Rmd File
###Reading the cleaned data into R
###Testing where the top level of the project directory is
`here::here()`
### Setting the path to the data file
`halloween_candy <- read_csv(here("clean_data/halloween_candy.csv"),`
###Parsing failures error solution for gender and country columns
`col_types = cols(gender = col_character(), country = col_character()))`

##Carrying out analysis
###1. What is the total number of candy ratings given across the three years. (number of candy ratings, not number of raters. Don’t count missing values)
  `total_ratings <-
halloween_candy %>% 
  filter(!is.na(rating)) %>% 
  summarise(total_ratings = n())`

###There are 548814 total ratings.

###2. What was the average age of people who are going out trick or treating and the average age of people not going trick or treating?
`average_age_trick_or_treating <- halloween_candy %>% 
  group_by(trick_or_treating) %>%
 drop_na() %>% 
  summarise(avg_age = mean(age))`
  
| trick_or_treating | average_age |
|-------------------|-------------|
| No                | 42          |
| Yes               | 39          |



###The average age of people who are trick or treating is 39 compared to an average age of 42 for those not trick or treating.

###3. For each of joy, despair and meh, which candy bar received the most of these ratings?

###Firstly, find the ratings total for each bar for each rating
`most_rated_candy <- halloween_candy %>% 
   select(candy, rating) %>% 
  filter(rating == c("joy", "despair", "meh")) %>% 
  group_by(candy, rating) %>% 
  count() `

####Show the ratings 
`most_rated_candy <- most_rated_candy %>% 
  group_by(rating) %>% 
  slice_max(n) %>% 
  arrange(desc(n))`
  
| candy                      | rating  | ratings_count |
|----------------------------|---------|---------------|
| reese_s_peanut_butter_cups | joy     | 3307          |
| gum_from_baseball_cards    | despair | 3265          |
| chiclets                   | meh     | 879           |

### 4. How many people rated Starburst as despair?
`starbust_despair_rating <- halloween_candy %>% 
  filter(candy == "starburst",
         rating == "despair") %>% 
  summarise(total = n())`
###1865 people

###For the next three questions, count despair as -1, joy as +1 and meh as 0.
`halloween_candy <- halloween_candy %>% 
  filter(rating == c("despair", "joy", "meh")) %>% 
  mutate(rating = case_when(
    rating == "despair" ~ -1, 
    rating == "joy" ~ +1, 
    rating == "meh" ~ 0
  ))`
  
###5. What was the most popular candy bar by this rating system for each gender in the dataset?
  `popular_candy_by_gender <- halloween_candy %>% 
  select(gender, candy, rating) %>% 
  drop_na() %>% 
  group_by(gender, candy) %>% 
  summarise(total = sum(rating)) %>% 
  slice_max(total) %>% 
  arrange(desc(total))`
  
| gender             | candy | total |
|--------------------|-------|-------|
| male               | twix  | 992   |
| female             | twix  | 574   |
| I'd rather not say | twix  | 41    |
| other              | twix  | 24    |
  
###6. What was the most popular candy bar in each year?
`most_popular_candy_by_year <-
halloween_candy %>% 
  select(year, rating, candy) %>% 
  drop_na() %>% 
  group_by(year, candy) %>% 
  summarise(total = sum(rating)) %>% 
  slice_max(total)`
  
| year | candy                    | total |
|------|--------------------------|-------|
| 2015 | any_full_sized_candy_bar | 1458  |
| 2016 | any_full_sized_candy_bar | 322   |
| 2017 | twix                     | 1376  |
  
###7. What was the most popular candy bar by this rating for people in US, Canada, UK and all other countries?
`  most_popular_candy_by_countries <-
halloween_candy %>% 
  drop_na(country) %>% 
  select(country, rating, candy) %>% 
    mutate(country = case_when(
      country == "usa" ~ "usa",
      country == "canada" ~ "canada",
      country == "uk" ~ "uk",
      TRUE ~ "other"
    )) %>% 
  group_by(country, candy) %>% 
  summarise(total = sum(rating)) %>% 
  slice_max(total) %>% 
  arrange(desc(total))`
  
| country | candy        | total |
|---------|--------------|-------|
| usa     | twix         | 1437  |
| canada  | mars         | 150   |
| other   | butterfinger | 31    |
| other   | twix         | 31    |
| uk      | twix         | 24    |
 
