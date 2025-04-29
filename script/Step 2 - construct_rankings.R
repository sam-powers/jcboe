## ---------------------------
## Script name: construct_rankings.R
##
## Author:Sam Powers
## Date Created: 2021-07-08
##
## ---------------------------
## Purpose of script: To construct the ranking system for JCBOE after the style of chicago
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
library(psych)

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation


# read in data ------------------------------------------------------------

indicator_data <- read_csv("data/indicator_data.csv") %>%
  filter(!GEOID == "34017980100" ) # Take out the one tract that is actually a park

View(indicator_data)
# Make each indicator -----------------------------------------------------
## Income
names(indicator_data)

hhinc_comp <-
indicator_data %>%
  select(GEOID, hhinc, children) %>%
  mutate(hhinc = case_when(is.na(hhinc) ~ 0, TRUE ~ hhinc)) %>%
  arrange(hhinc) %>%
  mutate(
    cum_school_child = cumsum(children),
    tot_school_child = sum(children),
    hhinc_pct = cum_school_child/tot_school_child *100
  )   %>%
  select(GEOID, hhinc_pct)
hhinc_comp

## Educational Attainment
ed_attain_comp <-
indicator_data %>%
  select(GEOID, less_than_hs, hs, some_college, college_deg, grad_deg, children) %>%
  mutate(
    tot_ed = less_than_hs + hs + some_college + college_deg + grad_deg,
    across( c("less_than_hs", "hs", "some_college", "college_deg", "grad_deg"), ~.x/tot_ed*100),
    ed_attain = .2*less_than_hs + .4*hs + .6*some_college + .8*college_deg + grad_deg,   ## Dont round in the creation of this score. 
    ed_attain = case_when(is.na(ed_attain) ~ 0, TRUE ~ ed_attain)
  ) %>% 
  arrange(ed_attain) %>% 
  mutate(
    cum_school_child = cumsum(children),
    tot_school_child = sum(children),
    ed_attain_pct = cum_school_child/tot_school_child *100
  ) %>%
  select(GEOID, ed_attain, ed_attain_pct, children)

ed_attain_comp

## Children in single parent households
## Reverse the polarity on single parents. 
sing_par_comp <-
indicator_data %>%
  select(GEOID, single_parent, children) %>%
  arrange(desc(single_parent)) %>% 
  mutate(
    cum_school_child = cumsum(children),
    tot_school_child = sum(children),
    single_parent_pct  = cum_school_child/tot_school_child *100
  )  %>%
  select(GEOID, single_parent_pct)


## Owner Occupied Households
own_occ_comp <- 
indicator_data %>%
  select(GEOID, own_occ, children) %>%
  arrange(own_occ) %>% 
  mutate(
    cum_school_child = cumsum(children),
    tot_school_child = sum(children),
    own_occ_pct = cum_school_child/tot_school_child *100
  )  %>%
  select(GEOID, own_occ_pct)

## Other Language 
oth_lang_comp <-
indicator_data %>%
  select(GEOID, oth_lang,  children) %>%
  arrange(desc(oth_lang)) %>% ## reverse polarity
  mutate(
    cum_school_child = cumsum(children),
    tot_school_child = sum(children),
    oth_lang_pct = cum_school_child/tot_school_child *100
  )  %>%
  select(GEOID, oth_lang_pct)


# put it all together -----------------------------------------------------

calculated_quartiles <- 
hhinc_comp %>%
  left_join(ed_attain_comp %>%
              select(GEOID, ed_attain_pct, children)
            ) %>%
  left_join(sing_par_comp) %>%
  left_join(own_occ_comp) %>%
  left_join(oth_lang_comp) %>%
  mutate(
    overall_ses_score = (hhinc_pct + ed_attain_pct + single_parent_pct + own_occ_pct + oth_lang_pct)/5
  ) %>%
  arrange(
    overall_ses_score
  ) %>%
  mutate(
    pct_children = cumsum(children)/sum(children)*100,
    quartile =  case_when(
      pct_children >= 76 ~ 4,
      pct_children >= 51 ~ 3,
      pct_children >= 26 ~ 2,
      TRUE ~ 1
    )
  ) %>%
  group_by(
    quartile
  ) %>%
  mutate(tot_children = sum(children)) 


# Output Table ------------------------------------------------------------


final_table <-
indicator_data %>%
  left_join(
    ed_attain_comp %>%
      select(GEOID, ed_attain)
  ) %>%
  left_join(
    calculated_quartiles %>%
      select(GEOID, contains("pct"), overall_ses_score, quartile)
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 1)))


write_csv(final_table, file = "data/final_table.csv")















