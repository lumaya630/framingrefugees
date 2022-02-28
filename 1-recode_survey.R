library(tidyverse)
library(sjPlot)
# =======================
# PART 1: RECODE RAW DATA
# =======================
# read in data
dat <- read.csv("survey_responses_final.csv")  %>%
  subset(DistributionChannel == "anonymous") %>%
  subset(!duplicated(AmazonId)) %>%
  subset(Q10 != "I am not a citizen of the United States and cannot vote")

# recode
temp <- dat %>%  mutate(
  Q1 = recode(Q1, "I agree to participate" = 1, 
              "I do not agree to participate" = 0),
  
  Q3 = recode(Q3, 
               "Not famiiar" = 0,
               "Somewhat familiar" = 1,
               "Very familiar" = 2,
               .default = NA_real_),
  
  Q5 = recode(Q5, 
              "The United States should greatly increase admissions of refugees and asylum seekers." = 2,
              "The United States should somewhat increase admissions of refugees and asylum seekers." = 1,
              "The United States should keep the same level of admissions of refugees and asylum seekers." = 0,
              "The United States should somewhat decrease admissions of refugees and asylum seekers." = -1,
              "The United States should greatly decrease admissions of refugees and asylum seekers." = -2,
              .default = NA_real_),
  
  Q6 = recode(Q6,
              "My city of residence should resettle significantly less refugees and asylum seekers." = -2,
              "My city of residence should resettle significantly more refugees and asylum seekers." = 2,
              "My city of residence should resettle somewhat less refugees and asylum seekers." = -1,
              "My city of residence should resettle somewhat more refugees and asylum seekers." = 1,
              "My city of residence should resettle the same number of refugees and asylum seekers." = 0,
              .default = NA_real_),
  
  Q7 = recode(Q7,
              "Refugees/asylum seekers are somewhat less dangerous than the average resident in the US." = 1,
              "Refugees/asylum seekers are somewhat more dangerous than the average resident in the US." = -1,
              "Refugees/asylum seekers are significantly less dangerous than the average resident in the US." = 2,
              "Refugees/asylum seekers are significantly more dangerous than the average resident in the US." = -2,
              "Refugees/asylum seekers pose the same level of danger compared to the average resident in the US." = 0,
              .default = NA_real_),
  
  Q8 = recode(Q8,
               "Strongly agree" = 2,
               "Agree" = 1,
               "Neither agree nor disagree" = 0,
               "Disagree" = -1,
               "Strongly disagree" = -2,
               .default = NA_real_),
  
  Q9 = recode(Q9,
               "Strongly agree" = 2,
               "Agree" = 1,
               "Neither agree nor disagree" = 0,
               "Disagree" = -1,
               "Strongly disagree" = -2,
               .default = NA_real_),
  
  Q10 = recode(Q10,
    "Significantly more likely" = 2,
    "More likely" = 1,
    "No change" = 0, 
    "Less likely" = -1,
    "Significantly less likely" = -2,
    .default = NA_real_),
  
  Q11 = recode(Q11,
               "Very Liberal" = -2,
               "Liberal" = -1,
               "Moderate" = 0,
               "Conservative" = 1,
               "Very Conservative" = 2,
               .default = NA_real_),
  
 Q12 = recode(Q12,
              "Independent" = "Other",
              "Democratic" = "Democrat",
              .default = Q12),
  
  Q13 = recode(Q13,
               "Immigrant (foreign born)" = 1,
               "Not an immigrant, but at least one parent is an immigrant"= 0,
               "Not an immigrant, parents are not immigrants, but at least one grandparent is an immigrant" = 0,
               "Not an immigrant, parents and grandparents are not immigrants" = 0,
               .default = NA_real_),
  
  Q14_1 = recode(Q14,
               "High school or equivalent" = "High school or less",
               "Some college coursework completed" = "Some college/Associates",
               "Technical or occupational certificate" = "Some college/Associates",
               "Associate degree" = "Some college/Associates",
               "Bachelor's degree" = "Bachelor's degree",
               "Master's degree" = "Postgraduate degree",
               "Professional" = "Postgraduate degree",
               "Doctorate" = "Postgraduate degree",
               .default = "High school or less"
               ),
 
 Q14_2 = recode(Q14,
                "High school or equivalent" = "Associate or less",
                "Some college coursework completed" = "Associate or less",
                "Technical or occupational certificate" = "Associate or less",
                "Associate degree" = "Associate or less",
                "Bachelor's degree" = "Bachelor or higher",
                "Master's degree" = "Bachelor or higher",
                "Professional" = "Bachelor or higher",
                "Doctorate" = "Bachelor or higher",
                .default = "Associate or less"
 ),
 
 
 Q15 = recode(Q15, 
              "Asian or Pacific Islander, not Hispanic/Latino" = "Asian or Pacific Islander",
              "White, not Hispanic/Latino" = "White",
              "Black or African American, not Hispanic/Latino" = "Black or African American",
              .default = Q15),

  Q16 = as.numeric(Q16),
 
  age_binned = case_when(Q16 >= 18 & Q16 < 25 ~ "18 to 24",
                         Q16 >= 25 & Q16 < 40 ~ "25 to 39",
                         Q16 >= 40 & Q16 < 60 ~ "40 to 59",
                         Q16 >= 60 ~ "60+"),
  
 Q18 = recode(Q18,
              "Male" = "Male",
              "Female" = "Female",
              .default = "Other"),

  treatment = recode(treatment,
                     "There is an ongoing discussion on how to reform asylum and refugee policy." = "control",
                     "Some policymakers highlight chaos at the southern border. They say the overwhelming influx of illegal immigrants and asylum seekers trying to enter the country has caused loss of control of the border, delegitimizing our asylum system." = "border security",
                     "Some policymakers highlight the dangers of accepting refugees and asylum seekers. They say there is a risk of the refugee or asylum seeker being a member of a gang or terrorist group, and increasing admissions would increase crime in the United States." = "traditional security",
                     .default = ""),
 
 treatment = factor(treatment, levels = c("control", "traditional security", "border security", ""))
 
  # rename
  ) %>%  subset(Q1 == 1) %>% rename(
    consent = Q1, 
    base_knowledge = Q3,
    admission = Q5, 
    acceptance = Q6,
    crime = Q7,
    influx = Q8,
    threat_perception = Q9,
    voting = Q10,
    ideology = Q11,
    party = Q12,
    immigrant_status = Q13,
    educ_d = Q14_1,
    education = Q14_2,
    race = Q15,
    age = Q16,
    income = Q17,
    gender = Q18,
    religion = Q19
  )

# =======================================
# PART 2: DICHOTOMIZE DEPENDENT VARIABLES
# =======================================
# dichotomized
temp <- temp %>% mutate(
  treatment = factor(treatment, 
                     levels = c("control", "traditional security", "border security")),
  
  admission_dichotimized = case_when(admission >= 0 ~ 0,
                                     admission < 0 ~ 1,
                                     TRUE ~ -1),
  acceptance_dichotimized = case_when(acceptance >= 0 ~ 0,
                                      acceptance < 0 ~ 1,
                                      TRUE ~ -1),
  crime_dichotimized = case_when(crime >= 0 ~ 0,
                                 crime < 0 ~ 1,
                                 TRUE ~ -1),
  influx_dichotimized = case_when(influx >= 0 ~ 0,
                                  influx < 0 ~ 1,
                                  TRUE ~ -1),
  threat_dichotimized = case_when(threat_perception >= 0 ~ 0,
                                  threat_perception < 0 ~ 1,
                                  TRUE ~ -1),
  voting_dichotimized = case_when(voting >= 0 ~ 0,
                                  voting < 0 ~ 1,
                                  TRUE ~ -1)
)


# =======================================
# PART 3: SAVE CSV
# =======================================
write.csv(temp, "survey_recoded.csv")

