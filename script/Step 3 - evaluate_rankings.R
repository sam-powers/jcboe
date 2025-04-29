## ---------------------------
## Script name: evaluating_rankings
##
## Author:Sam Powers
## Date Created: 2021-07-08
##
## ---------------------------
## Purpose of script: To investigate the internal and external validity of the JCBOE chicago-style ranking system
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

options(scipen = 6, digits = 4) # I prefer to vie

# load in data ------------------------------------------------------------

rankings <- read_csv("data/final_table.csv") %>%
  select(GEOID, contains("pct"), overall_ses_score, children, quartile) %>%
  select(-pct_children)
 
demos <- read_csv("data/tract_demographics.csv")


# Internal validity -------------------------------------------------------

fa.dat <-
rankings %>%
  select(contains("pct"))


pairs(fa.dat, upper.panel = NULL)
?pairs
alpha(fa.dat, check.keys = TRUE) # Raw alpha of .62. Dropping owner occupied would increase reliability. Other language is negatively correlated. 

principal(fa.dat, nfactors=1, rotate="none", scores=TRUE) ## A PCA only gets 49% of the variance. 
# Other language is negatively correlated to the construct. And owner occupied pct is not related at all. 
principal(fa.dat, nfactors=2, rotate="none", scores=TRUE) ## We can get up to 24% with a second, orthogonal, dimension. But that doesn't really help the cause


fa(fa.dat, nfactors=1, rotate="promax", fm="ml", SMC=TRUE)  # horrid rmsea
fa(fa.dat, nfactors=2, rotate="promax", fm="ml", SMC=TRUE)  # Two! Factors! Does! So! Well! Its actually a bit suspect, tbgh. 

fa(fa.dat, nfactors=2, rotate="promax", fm="wls", SMC=TRUE) # Ow. Heywood. 
fa(fa.dat, nfactors=3, rotate="promax", fm="wls", SMC=TRUE) # And yea... owner occupied and other language should be separate dimensions. 
                                                            # They dont really correlate either. 


# external validity -------------------------------------------------------

rankings %>%
  select(overall_ses_score, GEOID) %>%
  left_join(demos) %>%
  select(-GEOID) %>%
  cor()

## Okay, so the rankings do correlate with what you want it to correlate with. 

rankings %>%
  select(quartile, GEOID) %>%
  left_join(demos) %>%
  group_by(quartile) %>%
  summarize(across(where(is.numeric), median))

rankings %>%
  select(quartile, GEOID) %>%
  left_join(demos) %>%
  group_by(quartile) %>%
  summarize(across(where(is.numeric), min))

rankings %>%
  select(quartile, GEOID) %>%
  left_join(demos) %>%
  group_by(quartile) %>%
  summarize(across(where(is.numeric), max))

rankings %>%
  select(quartile, GEOID) %>%
  left_join(demos) %>%
  group_by(quartile) %>%
  summarize(across(where(is.numeric), mean))

rankings %>%
  select(quartile, GEOID) %>%
  left_join(demos) %>%
  group_by(quartile) %>%
  summarize(across(where(is.numeric), sd))



# maybe try a model -------------------------------------------------------
ses_score_predict <- lm(cbind(white, black, ltnx) ~ overall_ses_score , data = 
             rankings %>%
             select(overall_ses_score, GEOID) %>%
             left_join(demos) )

coef(ses_score_predict)
summary(ses_score_predict)


quartile_predict <- lm(cbind(white, black, ltnx) ~ quartile , data = 
             rankings %>%
             select(quartile, GEOID) %>%
             left_join(demos) )

coef(quartile_predict)

# --- I could structural model a cfa between the constructs... but chicago model still does not seem worth it. 








