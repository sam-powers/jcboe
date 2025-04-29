## ---------------------------
## Script name: chicago_deconstruct
##
## Author:Sam Powers
## Date Created: 2021-07-06
##
## ---------------------------
## Purpose of script: Deconstruct the chicago schools' index to see how they make theirs and to investigate its internal and external validity. 
##   
##
## ---------------------------
## set working directory
library(rstudioapi)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

## load up the packages we will need:

library(tidyverse)
library(extrafont)
library(pdftools)
library(psych)



options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation 


# Read in Data ------------------------------------------------------------

cps_file <- "background_docs/Tier_2021-2022_Summary Information.pdf" # The File

cps_pdf_structure <- pdf_data(cps_file) 

cps_clean <- 
map_df(cps_pdf_structure,
~ tibble(numb =  1:1619, fill = "top") %>%
  spread(numb, fill) %>%
  bind_rows(
  .x %>%
  arrange(x, y) %>%
  pivot_wider(id_cols = y, names_from = x, values_from = text) %>%
  arrange(y)
  ) %>%
  unite(col = "tract", `1`:`65`, sep = "", remove = TRUE, na.rm = TRUE) %>%
  unite(col = "hh_inc", `66`:`130`, sep = "", remove = TRUE, na.rm = TRUE) %>%
  unite(col = "less_than_hs", `131`:`211`, sep = "", remove = TRUE, na.rm = TRUE) %>%
  unite(col = "hs", `212`:`265`, sep = "", remove = TRUE, na.rm = TRUE) %>%
  unite(col = "some_college", `266`:`310`, sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "college_grad", `311`:`370`, sep = "", remove = TRUE, na.rm = TRUE) %>%
  unite(col = "grad_school", `371`:`420`, sep = "", remove = TRUE, na.rm = TRUE) %>%
  unite(col = "ed_attain", `421`:`480`, sep = "", remove = TRUE, na.rm = TRUE) %>%
  unite(col = "sing_par", `481`:`557`, sep = "", remove = TRUE, na.rm = TRUE) %>%
  unite(col = "own_occ", `558`:`627`, sep = "", remove = TRUE, na.rm = TRUE) %>%
  unite(col = "oth_lang", `628`:`759`, sep = "", remove = TRUE, na.rm = TRUE) %>%
  unite(col = "performance_score", `760`:`850`, sep = "", remove = TRUE, na.rm = TRUE)  %>% 
  unite(col = "numb_school_child", `851`:`908`, sep = "", remove = TRUE, na.rm = TRUE)  %>% 
  unite(col = "hh_inc_comp", `909`:`989`, sep = "", remove = TRUE, na.rm = TRUE)  %>% 
  unite(col = "ed_attain_comp", `990`:`1053`, sep = "", remove = TRUE, na.rm = TRUE)  %>% 
  unite(col = "sing_par_comp", `1054`:`1108`, sep = "", remove = TRUE, na.rm = TRUE)  %>% 
  unite(col = "own_occ_comp", `1109`:`1178`, sep = "", remove = TRUE, na.rm = TRUE)  %>% 
  unite(col = "oth_lang_comp", `1179`:`1305`, sep = "", remove = TRUE, na.rm = TRUE)  %>% 
  unite(col = "performance_score_comp", `1306`:`1431`, sep = "", remove = TRUE, na.rm = TRUE)  %>% 
  unite(col = "combined_score", `1432`:`1518`, sep = "", remove = TRUE, na.rm = TRUE)    %>%
  unite(col = "tier", `1519`:`1619`, sep = "", remove = TRUE, na.rm = TRUE)    %>% 
  mutate(across(everything(), ~ as.numeric(str_replace_all(.x, "%|,|\\$", "")))) %>% 
  select(-y) %>%
  filter(!is.na(tract)) 
)
View(cps_clean)

write_csv(cps_clean, file = "data/cps_clean.csv")

cps_clean <- read_csv("data/cps_clean.csv")
  
# Deconstruct the indicator -----------------------------------------------

## Income
cps_clean %>%
  select(hh_inc, hh_inc_comp, numb_school_child) %>%
  mutate(
    range_norm =  (hh_inc - min(hh_inc))/ (max(hh_inc)  - min(hh_inc)),
    range_norm_inv = 1- range_norm
  ) %>%
  arrange(hh_inc) %>%
  mutate(
    cum_school_child = cumsum(numb_school_child),
    tot_school_child = sum(numb_school_child),
    pct = round(cum_school_child/tot_school_child *100)
  )  
         
## Lesson Learned. 
### Step 1: Rank on the desired component
### Step 2: Take the cumulative prop of school children less than and equal to that component
### Step 3: Multiply by 100 and round


## Ed Attainment
cps_clean %>%
  select(less_than_hs, hs, some_college, college_grad, grad_school, ed_attain, ed_attain_comp, numb_school_child) %>%
  mutate(
    tot_ed = less_than_hs + hs + some_college + college_grad + grad_school,
    across( c("less_than_hs", "hs", "some_college", "college_grad", "grad_school"), ~.x/tot_ed*100),
    ed_attain_synth = .2*less_than_hs + .4*hs + .6*some_college + .8*college_grad + grad_school,   ## Dont round in the creation of this score. 
    ed_attain_synth = case_when(is.na(ed_attain_synth) ~ 0, TRUE ~ ed_attain_synth)
    ) %>%
  arrange(ed_attain_synth) %>% 
  mutate(
    cum_school_child = cumsum(numb_school_child),
    tot_school_child = sum(numb_school_child),
    pct = round(cum_school_child/tot_school_child *100)
  )  %>% View()


## Single Parent Households
## Reverse the polarity on single parents. 
cps_clean %>%
  select(sing_par, sing_par_comp, numb_school_child) %>%
  mutate(sing_par_use = 100 - sing_par, 
         sing_par_use = case_when(is.na(sing_par_use) ~ 0, TRUE ~ sing_par_use)) %>%
  arrange(desc(sing_par)) %>% 
  mutate(
    cum_school_child = cumsum(numb_school_child),
    tot_school_child = sum(numb_school_child),
    pct = round(cum_school_child/tot_school_child *100)
  )  %>% View()

## NA's go to 0, but true 0's go to 100. That is not something I can recover from this data. 


## Owner Occupied Households
cps_clean %>%
  select(own_occ, own_occ_comp, numb_school_child) %>%
  arrange(own_occ) %>% 
  mutate(
    cum_school_child = cumsum(numb_school_child),
    tot_school_child = sum(numb_school_child),
    pct = round(cum_school_child/tot_school_child *100)
  )  %>% View()
## Recovered, but we need non -rounded data


## Other Language 
cps_clean %>%
  select(oth_lang, oth_lang_comp, numb_school_child) %>%
  arrange(desc(oth_lang)) %>% 
  mutate(
    cum_school_child = cumsum(numb_school_child),
    tot_school_child = sum(numb_school_child),
    pct = round(cum_school_child/tot_school_child *100)
  )  %>% View()
## Another reversed polarity with NA's at bottom & true 0's at top


## 6 Factor econ score. 
cps_clean %>%
  select(contains("comp"), combined_score, numb_school_child, tier) %>%
  mutate(
    total = hh_inc_comp + ed_attain_comp + sing_par_comp + own_occ_comp + oth_lang_comp + performance_score_comp,
    avg_total = round(total/6) ## its just an average of the components
  ) %>%
  arrange(avg_total) %>%
  mutate(
    cum_school_child = cumsum(numb_school_child),
    tot_school_child = sum(numb_school_child),
    pct = round(cum_school_child/tot_school_child *100),
    tier_synth = ceiling(pct/25)
  )  %>%
  select(tier, tier_synth) %>% View()

## A few were fiddled with. But by and large, this is very right. 


components <-
cps_clean %>%
  select(contains("comp"))



alpha(components, check.keys = TRUE) # The reliability is fine! But the other language component trends negatively
principal(components, nfactors=1, rotate="none", scores=TRUE) ## A PCA only gets 56% of the variance, though. 
# And it looks like, in general, that the other language component trends negatively and actually reduces this variance. 
principal(components, nfactors=2, rotate="none", scores=TRUE) ## eh,

fa(components, nfactors=1, rotate="promax", fm="ml", SMC=TRUE) # horrid rmsea

fa(components, nfactors=1, rotate="promax", fm="wls", SMC=TRUE) # we were going heywood for a bit there. Owner occupied housing is weak as is other languages
fa(components, nfactors=2, rotate="promax", fm="wls", SMC=TRUE) 
fa(components, nfactors=3, rotate="promax", fm="wls", SMC=TRUE) # educational attainment is really split. 


# the chicago metric is statistically quite poor. Its not measuring a latent at all. 







