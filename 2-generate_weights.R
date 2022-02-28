library(weights)
library(anesrake)
library(tidyverse)
library(ipumsr)
library(survey)
# read in survey data
temp <- read.csv("survey_recoded.csv")

# read in acs 2019 5 year data
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package.
                             # It can be installed using the following command: 
                             # install.packages('ipumsr')")

ddi <- read_ipums_ddi("usa_00004.xml")
ipums_data <- read_ipums_micro(ddi)
head(ipums_data)

# subset data to citizens over 18
citizen_df <- ipums_data %>% subset(CITIZEN %in% c(0,1,2) & AGE >= 18)
citizen_df %>% group_by(CITIZEN) %>%
  summarise(weighted_n = sum(PERWT)) %>%
  mutate(
    CITIZEN_SIMP = c(1, 1, 1)
  ) %>%
  group_by(CITIZEN_SIMP) %>% summarise(Freq = sum(weighted_n))

# store the number of citizens over 18
n_citizen = 231017583

# ============================
# PART 1: CREATE MARGIN TABLES
# ============================
# the margin tables calculate the frequency each category should appear in 
# the survey sample by using the proportions for the respective category in the
# acs data

# create a new race variable
citizen_df <- citizen_df %>% mutate(
  RACE_2 = case_when(HISPAN %in% c(1,2,3,4) ~ "Hispanic or Latino",
                     RACE == 1 ~ "White",
                     RACE == 2 ~ "Black or African American",
                     RACE %in% c(3,4,5,6) ~ "Asian or Pacific Islander",
                     TRUE ~ "Other"))
# race margin table
race.margins.fr <- citizen_df %>% group_by(RACE_2) %>%
  summarise(Freq = sum(PERWT)) %>%
  rename(race = RACE_2) %>%
  mutate(
    Freq = Freq/n_citizen * nrow(temp)
)

# education
educ.margins.fr <- citizen_df %>% group_by(EDUCD) %>%
  summarise(weighted_n = sum(PERWT))%>% 
  mutate(
    education = c(rep("Associate or less", 20), rep("Bachelor or higher", 4))
  ) %>%
  group_by(education) %>% summarise(Freq = sum(weighted_n) ) %>% 
  mutate(education = as.factor(education)) %>%
  mutate(
    Freq = Freq/n_citizen * nrow(temp)
)

# party 
party.margins.fr <- data.frame(party=c("Democrat", "Republican", "Other"), 
                               Freq=round(c(.33, .29, .34)*n_citizen)) %>%
  mutate(party = as.factor(party))%>%
  mutate(
    Freq = Freq/n_citizen * nrow(temp)
)

# age
age.margins.fr <- citizen_df %>% mutate(
  AGE_BINNED = case_when(AGE >= 18 & AGE < 25 ~ "18 to 24",
                         AGE >= 25 & AGE < 40 ~ "25 to 39",
                         AGE >= 40 & AGE < 60 ~ "40 to 59",
                         AGE >= 60 ~ "60+"
                      )) 

age.margins.fr <- age.margins.fr %>% group_by(AGE_BINNED) %>%
  summarise(Freq = sum(PERWT)) %>%
  rename(age_binned = AGE_BINNED) %>%
  mutate(age_binned = as.factor(age_binned))%>%
  mutate(
    Freq = Freq/n_citizen * nrow(temp)
)

# raking
temp <- temp %>%
  mutate(age_binned = as.factor(age_binned),
         education = as.factor(education),
         race = as.factor(race),
         party = as.factor(party))

vars.to.include <- list(~education, ~race, ~party, ~age_binned)
svyz.fr.sd <- svydesign(~1, data = temp)
rake.out.fr <- rake(svyz.fr.sd, vars.to.include, 
                    list(
                         educ.margins.fr,
                         race.margins.fr,
                         party.margins.fr,
                         age.margins.fr
                         ))


rake.out.fr <- trimWeights(rake.out.fr, upper = 6)
temp$weight <- weights(rake.out.fr)
summary(temp$weight)

write.csv(temp,"survey_recoded_weighted.csv")
