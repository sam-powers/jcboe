## ---------------------------
## Script name: expand_rankings.R
##
## Author:Sam Powers
## Date Created: 2021-08-30
##
## ---------------------------
## Purpose of script: To expand and explore other factors to include in the ranking system for JCBOE
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

complete_data <- read_csv("data/community_data.csv") %>%
  filter(!GEOID == "34017980100" ) # Take out the one tract that is actually a parks

View(complete_data)
# Make each indicator -----------------------------------------------------
## Income
names(complete_data)

hhinc_comp <-
  complete_data %>%
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
  complete_data %>%
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
  complete_data %>%
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
  complete_data %>%
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
  complete_data %>%
  select(GEOID, oth_lang,  children) %>%
  arrange(desc(oth_lang)) %>% 
  mutate(
    cum_school_child = cumsum(children),
    tot_school_child = sum(children),
    oth_lang_pct = cum_school_child/tot_school_child *100
  )  %>%
  select(GEOID, oth_lang_pct)


## Rent Burden

rent_burden_comp <-
  complete_data %>%
  select(GEOID, rent_burdened,  children) %>%
  arrange(desc(rent_burdened)) %>% 
  mutate(
    cum_school_child = cumsum(children),
    tot_school_child = sum(children),
    rent_burdened_pct = cum_school_child/tot_school_child *100
  )  %>%
  select(GEOID, rent_burdened_pct)

## Unemployed
employ_comp <-
  complete_data %>%
  select(GEOID, unemp,  children) %>%
  arrange(desc(unemp)) %>% 
  mutate(
    cum_school_child = cumsum(children),
    tot_school_child = sum(children),
    unemp_perc = cum_school_child/tot_school_child *100
  )  %>%
  select(GEOID, unemp_perc) %>%
  mutate(GEOID = as.numeric(GEOID))


# put it all together -----------------------------------------------------

comp_table <- 
  hhinc_comp %>%
  left_join(ed_attain_comp %>%
              select(GEOID, ed_attain_pct, children)
  ) %>%
  left_join(sing_par_comp) %>%
 left_join(employ_comp) %>%
 # left_join(oth_lang_comp) %>%
  left_join(rent_burden_comp) %>%
  left_join(
    complete_data %>% select(GEOID, gini ) %>% mutate(GEOID = as.numeric(GEOID))
  )



# Explore it a bit --------------------------------------------------------

fa.dat <- comp_table[,c(2,3, 5, 7)]
pairs(fa.dat, upper.panel = NULL)

alpha(fa.dat, check.keys = TRUE) # Raw alpha of .87. Dropping rent burden occupied would increase reliability but not by much. This is really good

principal(fa.dat, nfactors=1, rotate="none", scores=TRUE) ## A PCA gets 72% of the variance
principal(fa.dat, nfactors=2, rotate="none", scores=TRUE) ## A portion of the rent burden variance is oblique to the remainder of the factors. 


fa(fa.dat, nfactors=1, rotate="promax", fm="ml", SMC=TRUE)  # Good RSEA, Scary low, actually
fa(fa.dat, nfactors=2, rotate="promax", fm="ml", SMC=TRUE)  # Their correlation is so close that I'm not really sure it changes anything to add a second
fa(fa.dat, nfactors=1, rotate="promax", fm="wls", SMC=TRUE) # Erg, so the confidence band on the RMSEA is very wide. 
fa(fa.dat, nfactors=2, rotate="promax", fm="wls", SMC=TRUE) # The lack of stability is a bit bothersome between the ml and the wls. But it is still a correlation of .73. I could get behind this as unidimensional


fa.dat2 <- comp_table[,c(2,3, 5, 6, 7)] # add unemployment to make 5 factors
pairs(fa.dat2, upper.panel = NULL)

alpha(fa.dat2, check.keys = TRUE) # Raw alpha of .88. Dropping rent burden occupied would increase reliability but not by much

principal(fa.dat2, nfactors=1, rotate="none", scores=TRUE) ## A PCA gets 69% of the variance
principal(fa.dat2, nfactors=2, rotate="none", scores=TRUE) ## A portion of the rent burden variance is oblique to the rest, but it is only 12% of the overall vatriance

scree(fa.dat2) # oh, yes, this is incredibly unidimensional

fa(fa.dat2, nfactors=1, rotate="promax", fm="ml", SMC=TRUE)  # Good RSEA, Scary low, actually, 62% of variance. Lowerish uniquenesses. rent burden does have less communality. We could maybe drop it but I don't see the need. It is easy to explain 
fa(fa.dat2, nfactors=2, rotate="promax", fm="ml", SMC=TRUE)  # Their correlation is so close that I'm not really sure it changes anything.

fa(fa.dat2, nfactors=1, rotate="promax", fm="wls", SMC=TRUE) # This RMSEA is pretty stable. Great 90% CI, great BIC (the best so far) This could really work. 
fa(fa.dat2, nfactors=2, rotate="promax", fm="wls", SMC=TRUE) # It isnt two factors at all. Its definitely unidimensional. This is really great news. 


# Output Table ------------------------------------------------------------

final_table <-
  complete_data %>%
  left_join(
    ed_attain_comp %>%
      select(GEOID, ed_attain)
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

write_csv(final_table, file = "data/demographic_table.csv")


### Next steps, take script from Step 2 to construct rankings based on these factors. 

# Explore the Gini Coefficient --------------------------------------------

hist(complete_data$gini)

tract_geo <- get_acs(
  geography = "tract",
  table = "B99162",
  state = "NJ",
  county = "Hudson County",
  survey = "acs5",
  year = 2019, 
  geometry = "true"
) 


tract_geo_dat <-
tract_geo %>%
  select(GEOID, geometry, NAME) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  filter(GEOID %in% complete_data$GEOID) %>%
  left_join(complete_data)


cols1 <- c("#f0dbe2", "#b02c58")

library("ggspatial")

ggplot(tract_geo_dat) +
  geom_sf( aes(fill = gini), color = "black") +
  scale_fill_steps(
    low = cols1[1],
    high = cols1[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 5
 #   labels = percent
    
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Gini Coefficient in Jersey City Census Tracts") +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br",
                         which_north = "true",
                         pad_x = unit(0.0, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal(
                           line_width = 1,
                           line_col = "black",
                           fill = "black",
                           text_col = "black",
                           text_family = "",
                           text_face = NULL,
                           text_size = 0
                         )) +
  theme(
    legend.position = "top",
    legend.title = element_text(),
    panel.border = element_rect(color  = "black",
                                fill = NA,
                                size = 1),
    plot.margin = margin(l =  .1, r = .1, t = 1, b =1, "cm")
    
  )







