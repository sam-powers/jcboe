## ---------------------------
## Script name: data_pull.R
##
## Author:Sam Powers
## Date Created: 2021-07-07
##
## ---------------------------
## Purpose of script: To pull data from the ACS in a programatic and repeatable manner
##   
##
## ---------------------------
## set working directory

library(rstudioapi)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

## ---------------------------
## load up the packages we will need:

library(tidyverse)
library(extrafont)
library(tidycensus)
library(readxl)

loadfonts() ## Load in the fonts I want to use

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation


# Tidycensus Data Pull ----------------------------------------------------

## Set Jersey City Geographic Area
# "https://data.jerseycitynj.gov/explore/dataset/jersey_city_censustract_geojson/export/"
jc_geo <- read_excel("data/jersey_city_censustract_geojson.xlsx") %>%
  pull(geoid) %>%
  unique()

missing_tracts <- read_csv("data/missing_tracts.csv")

missing_tracts %>%
  filter(geoid %in% jc_geo) # there is no overlap

jc_geo <- c(jc_geo, missing_tracts$geoid)

## Median Household Income
"S1901_C01_012"   # hhinc

hhinc_load  <- get_acs(geography = "tract",
                        variables = "S1901_C01_012",
                        state = "NJ", 
                        county = "Hudson County", 
                        survey = "acs5",
                        year = 2019, 
                        output = "wide") 

hhinc <-  hhinc_load %>% 
  filter(GEOID %in% jc_geo) %>%
  select(GEOID, hhinc = 3)


## Educational Attaiment
acs5 <- load_variables(2019, "acs5", cache = TRUE)

ed_attain_load <- get_acs(
  geography = "tract",
  table = "B15003",
  state = "NJ",
  county = "Hudson County",
  survey = "acs5",
  year = 2019
)  %>%
  left_join(acs5, by = c("variable" = "name")) %>%
  select( -concept, -NAME, -variable, -moe) %>%
  mutate(label = str_replace_all(label, "Estimate!!Total:", ""),
         label = str_replace_all(label, "!!|,|:|'", ""),
         label = str_replace_all(label, " ", "_")
         )

ed_attain <-
ed_attain_load %>%
  mutate(
    label = 
      case_when(
        label == "" ~ "Total",
        TRUE ~   str_to_lower(label)
      )
  ) %>%
  spread(label, estimate) %>%
  mutate(
    across(
      everything(), 
      ~
        case_when(
          is.na(.x) ~ 0, 
          TRUE ~ as.numeric(.x))
      )
    ) %>% 
  transmute(
    GEOID = as.character(GEOID),
    less_than_hs = `no_schooling_completed` + `nursery_school` + `kindergarten` + `1st_grade` + `2nd_grade` + `3rd_grade` + `4th_grade` + `5th_grade` 
                               + `6th_grade` + `7th_grade` + `8th_grade` + `9th_grade` 
                               + `10th_grade` + `11th_grade` + `12th_grade_no_diploma`,
    hs = `ged_or_alternative_credential` + `regular_high_school_diploma`,
    some_college = `associates_degree` + `some_college_1_or_more_years_no_degree` + `some_college_less_than_1_year`,
    college_deg = `bachelors_degree`,
    grad_deg = masters_degree + doctorate_degree + professional_school_degree,
    total = Total
  ) %>%
  filter(GEOID %in% jc_geo) 
  

## okay, we've got it
ed_attain 


## Single Parent Households
## Variable pulled is %of Children under Age 18 in two v. single parent households

sing_par_load <- get_acs(
  geography = "tract",
  table = "B09005",
  state = "NJ",
  county = "Hudson County",
  survey = "acs5",
  year = 2019
)  %>%
  left_join(acs5, by = c("variable" = "name")) %>%
  select( -concept, -NAME, -variable, -moe) %>%
  mutate(label = str_replace_all(label, "Estimate!!Total:", ""),
         label = str_replace_all(label, "!!|,|:|'|-|\\/", ""),
         label = str_replace_all(label, " ", "_")
  )

sing_par <-
sing_par_load %>%
  mutate(
    label = 
      case_when(
        label == "" ~ "Total",
        TRUE ~   str_to_lower(label)
      )
  ) %>%
  spread(label, estimate) %>%
  mutate(
    across(
      everything(), 
      ~
        case_when(
          is.na(.x) ~ 0, 
          TRUE ~ as.numeric(.x))
    )
  ) %>% 
  transmute(
    GEOID = as.character(GEOID),
    two_parent = cohabiting_couple_household + marriedcouple_household,
    single_parent = in_female_householder_no_spousepartner_present_household + in_male_householder_no_spousepartner_present_household,
    total = Total,
  ) %>%
  transmute(
    GEOID = GEOID,
    single_parent = (single_parent/total*100)
  ) %>%
  filter(GEOID %in% jc_geo) 


## Owner Occupied Homes
"B25003_002" # owner-occupied housing units
own_occ_load <- get_acs(
  geography = "tract",
  table = "B25003",
  state = "NJ",
  county = "Hudson County",
  survey = "acs5",
  year = 2019
)  %>%
  left_join(acs5, by = c("variable" = "name"))

own_occ <-
own_occ_load %>%
select( -concept, -NAME, -variable, -moe) %>%
  mutate(label = str_replace_all(label, "Estimate!!Total:", ""),
         label = str_replace_all(label, "!!|,|:|'|-|\\/", ""),
         label = str_replace_all(label, " ", "_")
  ) %>%
  mutate(
    label = 
      case_when(
        label == "" ~ "Total",
        TRUE ~   str_to_lower(label)
      )
  ) %>%
  spread(label, estimate) %>%
  mutate(
    across(
      everything(), 
      ~ case_when(
          is.na(.x) ~ 0, 
          TRUE ~ as.numeric(.x))
    )
  ) %>%
  transmute(
    GEOID = as.character(GEOID),
    own_occ = owner_occupied/Total*100
) %>%
  filter(GEOID %in% jc_geo) 

View(own_occ)

## Non-English Language

oth_lang_load <- get_acs(
  geography = "tract",
  table = "B99162",
  state = "NJ",
  county = "Hudson County",
  survey = "acs5",
  year = 2019
)  %>%
  left_join(acs5, by = c("variable" = "name"))

oth_lang <-
oth_lang_load %>%
  select( -concept, -NAME, -variable, ) %>%
  mutate(label = str_replace_all(label, "Estimate!!Total:", ""),
         label = str_replace_all(label, "!!|,|:|'|-|\\/", ""),
         label = str_replace_all(label, " ", "_")
  ) %>%
  mutate(
    label = 
      case_when(
        label == "" ~ "Total",
        TRUE ~   str_to_lower(label)
      )
  ) %>%
  spread(label, estimate) %>%
  mutate(
    across(
      everything(), 
      ~ case_when(
        is.na(.x) ~ 0, 
        TRUE ~ as.numeric(.x))
    )
  ) %>% 
  transmute(
    GEOID = as.character(GEOID),
    oth_lang = speak_other_languages/Total*100
  ) %>%
  filter(GEOID %in% jc_geo) 


## Number School Aged Children

school_age_children <-
sing_par_load %>%
  filter(label == "") %>%
  select(GEOID, children = estimate) %>%
  filter(GEOID %in% jc_geo) 


## Rent Burdened

total_households_load <- get_acs(geography = "tract", 
                         table = "B19051", 
                         state = "NJ",
                         county = "Hudson County",
                         survey = "acs5",
                         year = 2019,
                         cache_table = TRUE) %>%
  filter(variable == "B19051_001") %>%
  select(GEOID, total_housholds = estimate)



rent_pct_load <- get_acs(geography = "tract", 
                              table = "B25070", 
                              state = "NJ",
                              county = "Hudson County",
                              survey = "acs5",
                              year = 2019,
                              cache_table = TRUE)

rent_burdened <-
rent_pct_load %>%
  left_join(acs5 %>% rename(variable = name)) %>%
  separate(label, c(NA, "Total", "level"), sep = "!!")  %>%
  mutate(level = case_when(is.na(level) ~ "Total",
                           TRUE ~ level)) %>%
  select(estimate, level, GEOID)  %>%
  spread(level, estimate) %>%
  rename_with( ~ str_replace_all(
    tolower(
      str_replace_all(.x, " ", "_")
    ),
    "_percent",
    "")
  ) %>%
mutate(
  Burdened = `30.0_to_34.9` + `35.0_to_39.9` + `40.0_to_49.9`,
) %>%
  select(GEOID = geoid, Burdened) %>%
  left_join(total_households_load) %>%
  mutate(
    rent_burdened = Burdened/total_housholds*100
  ) %>%
  filter(GEOID %in% jc_geo) %>%
  select(GEOID, rent_burdened)

summary(rent_burdened$rent_burdened)


## Poverty & CPoverty
varlist_s = c("S1701_C03_001", # povrate
              "S1701_C03_002"   # cpovrate
)

tract_data_s <- get_acs(geography = "tract",
                        variables = varlist_s,
                        state = "NJ", 
                        county = "Hudson County", 
                        survey = "acs5",
                        year = 2019, 
                        output = "wide")

names(tract_data_s) = c("GEOID", "NAME",
                        "povrate", "povrateM",
                        "cpovrate", "cpovrateM")

tract_data_s %>%
  select(povrate, cpovrate) %>%
  arrange(cpovrateE, povrateE) %>%
  View()



# Gini Index --------------------------------------------------------------

varlist_b = c(
              "B19083_001"  # gini
              )  

tract_gini <- get_acs(geography = "tract",
                        variables = varlist_b,
                        state = "NJ", 
                        county = "Hudson County", 
                        survey = "acs5",
                        year = 2019, 
                        output = "wide")


names(tract_gini) = c("GEOID", "NAME",  
                        "gini", "giniM"
                        )


# Employment --------------------------------------------------------------

tract_employ <- get_acs(geography = "tract",
                      table = "B23025",
                      state = "NJ", 
                      county = "Hudson County", 
                      survey = "acs5",
                      year = 2019, 
                      output = "wide")

names(tract_employ) = c("GEOID", "NAME",  
                    "totalE", "totalM",
                    "labforceE", "labforceM",
                    "civlabforceE", "civlabforceM",
                    "employedE", "employedM",
                    "unemployedE", "unemployedM",
                    "ArmedE", "ArmedM",
                    "nolabforceE", "nolabforceM"
                    
) 

tract_employ <- 
tract_employ %>%
  mutate(unemp =  round(unemployedE/labforceE*100,2)) %>%
  select(GEOID, unemp)

# Put data together -------------------------------------------------------
indicator_data <- 
hhinc %>%
  left_join(ed_attain %>%
              select(-total)) %>%
  left_join(sing_par) %>%
  left_join(own_occ) %>%
  left_join(oth_lang) %>%
  left_join(school_age_children)


write_csv(indicator_data, file = "data/indicator_data.csv")

# Validation criteria -----------------------------------------------------
## Percent Black, White, Hispanic, Asian. 

tract_race <- get_acs(geography = "tract", 
                      table = "DP05", 
                      state = "NJ", 
                      county = "Hudson County", 
                      survey = "acs5",
                      year = 2019)

tract_white <- tract_race %>% 
  filter(variable == "DP05_0077P") %>% 
  rename(whiteE = estimate,
         whiteM = moe) %>% 
  select(-variable)

tract_black <- tract_race %>% 
  filter(variable == "DP05_0078P") %>% 
  rename(blackE = estimate,
         blackM = moe) %>% 
  select(-variable)

tract_indig <- tract_race %>% 
  filter(variable == "DP05_0079P") %>% 
  rename(indigE = estimate,
         indigM = moe) %>% 
  select(-variable)

tract_asian <- tract_race %>% 
  filter(variable == "DP05_0080P") %>% 
  rename(asianE = estimate,
         asianM = moe) %>% 
  select(-variable)

tract_othrace <- tract_race %>% 
  filter(variable %in% c("DP05_0081P", "DP05_0082P")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(othraceE = sum(estimate),
            othraceM = moe_sum(moe = moe, estimate = estimate))

tract_multi <- tract_race %>% 
  filter(variable == "DP05_0083P") %>% 
  rename(multiE = estimate,
         multiM = moe) %>% 
  select(-variable)

tract_ltnx <- tract_race %>% 
  filter(variable == "DP05_0071P") %>% 
  rename(ltnxE = estimate,
         ltnxM = moe) %>% 
  select(-variable)

tract_demos <- 
tract_white %>%
  left_join(
    tract_black
  ) %>%
  left_join(tract_indig) %>%
  left_join(tract_asian) %>%
  left_join(tract_othrace) %>%
  left_join(tract_multi) %>%
  left_join(tract_ltnx) %>%
  select(GEOID, contains("E")) %>%
  select(-NAME, -othraceM, -whiteM) %>%
  rename_with(~str_sub(.x, 1, -2), .cols = everything()) %>%
  rename(GEOID = 1) %>%
  filter(GEOID %in% jc_geo) 

write_csv(tract_demos, file = "data/tract_demographics.csv")



# Complete Community Level Data -------------------------------------------

complete_data <- 
  hhinc %>%
  left_join(ed_attain %>%
              rename(over_25 = total)) %>%
  left_join(sing_par) %>%
  left_join(own_occ) %>%
  left_join(oth_lang) %>%
  left_join(school_age_children) %>%
  left_join(rent_burdened) %>%
  left_join(
    tract_data_s %>%
      select(GEOID, -contains("M"))
  ) %>%
  left_join(tract_demos) %>%
  left_join(
    tract_gini %>% select(-contains("M"))
  ) %>%
  left_join(tract_employ) %>%
  left_join(tract_data_s %>% select(-contains("M")))


write_csv(complete_data, file = "data/community_data.csv")


