library("tidyverse")

temp <- read.csv("survey_recoded_weighted.csv") 
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
)

# ===================
# OVERALL POPULATION
# ===================

summary(lm(admission_dichotimized ~ treatment + 
             party + immigrant_status + ideology + educ_d + 
             age + income + race + base_knowledge + religion, 
           dat = temp, 
           weights = weight))

summary(lm(acceptance_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + base_knowledge + religion, 
           dat = temp, 
           weights = weight))

summary(lm(crime_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + base_knowledge + religion, 
           dat = temp, weights = weight))

summary(lm(influx_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + base_knowledge + religion, 
           dat = temp, 
           weights = weight))

summary(lm(threat_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + base_knowledge + religion, 
           dat = temp, weights = weight))

summary(lm(voting_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + base_knowledge + religion, 
           dat = temp, 
           weights = weight))

 # ========================================
# BROKEN DOWN BY SELF REPORTED FAMILIARITY
# =========================================
not_familiar <- temp %>% subset(base_knowledge < 2)

# self reported as "not familiar" or "somewhat familiar"

summary(lm(admission_dichotimized ~ treatment + 
             party + immigrant_status + ideology + educ_d + 
             age + income + race + religion, 
           weights = weight, dat = not_familiar ))

summary(lm(acceptance_dichotimized ~ treatment +
             party + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = not_familiar))

summary(lm(crime_dichotimized ~ treatment +
             party + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = not_familiar))

summary(lm(influx_dichotimized ~ treatment+
             party + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = not_familiar))

summary(lm(threat_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = not_familiar))

summary(lm(voting_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = not_familiar))

# self reported as "very familiar"
familiar <- temp %>% subset(base_knowledge == 2)

summary(lm(admission_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + religion, 
           weights = weight, dat = familiar ))

summary(lm(acceptance_dichotimized ~ treatment +
             party + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = familiar))

summary(lm(crime_dichotimized ~ treatment +
             party + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = familiar))

summary(lm(influx_dichotimized ~ treatment+
             party + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = familiar))

summary(lm(threat_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = familiar))

summary(lm(voting_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = familiar))

# ====================
# BROKEN DOWN BY PARTY
# ====================
rep <- temp %>% subset(party == "Republican")
summary(lm(admission_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = rep ))

summary(lm(acceptance_dichotimized ~ treatment+
             base_knowledge + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = rep))

summary(lm(crime_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = rep))

summary(lm(influx_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + education + 
             age + income + race + religion, 
           weights = weight, dat = rep))

summary(lm(threat_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = rep))

summary(lm(voting_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = rep))

dem <- temp %>% subset(party == "Democrat")
summary(lm(admission_dichotimized ~ treatment+
             base_knowledge + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = dem ))
summary(lm(acceptance_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = dem))
summary(lm(crime_dichotimized ~ treatment + 
             base_knowledge + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = dem))
summary(lm(influx_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = dem))
summary(lm(threat_dichotimized ~ treatment + 
             base_knowledge + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = dem))
summary(lm(voting_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + education + 
             age + income + race + religion,
           weights = weight, dat = dem))

party_other <- temp %>% subset(party == "Other")


# ====================
# BROKEN DOWN BY EDUCATION
# ====================
associate_less <- temp %>% subset(education == "Associate or less")
summary(lm(admission_dichotimized ~ treatment+
             base_knowledge + immigrant_status + ideology + party + 
             age + income + race + religion,
           weights = weight, dat = associate_less ))
summary(lm(acceptance_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + party + 
             age + income + race + religion,
           weights = weight, dat = associate_less))
summary(lm(crime_dichotimized ~ treatment + 
             base_knowledge + immigrant_status + ideology + party + 
             age + income + race + religion,
           weights = weight, dat = associate_less))
summary(lm(influx_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + party + 
             age + income + race + religion,
           weights = weight, dat = associate_less))
summary(lm(threat_dichotimized ~ treatment + 
             base_knowledge + immigrant_status + ideology + party + 
             age + income + race + religion,
           weights = weight, dat = associate_less))
summary(lm(voting_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + party + 
             age + income + race + religion,
           weights = weight, dat = associate_less))

bachelor_higher <- temp %>% subset(education == "Bachelor or higher")
summary(lm(admission_dichotimized ~ treatment+
             base_knowledge + immigrant_status + ideology + party + 
             age + income + race + religion,
           weights = weight, dat = bachelor_higher ))
summary(lm(acceptance_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + party + 
             age + income + race + religion,
           weights = weight, dat = bachelor_higher))
summary(lm(crime_dichotimized ~ treatment + 
             base_knowledge + immigrant_status + ideology + party + 
             age + income + race + religion,
           weights = weight, dat = bachelor_higher))
summary(lm(influx_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + party + 
             age + income + race + religion,
           weights = weight, dat = bachelor_higher))
summary(lm(threat_dichotimized ~ treatment + 
             base_knowledge + immigrant_status + ideology + party + 
             age + income + race + religion,
           weights = weight, dat = bachelor_higher))
summary(lm(voting_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + party + 
             age + income + race + religion,
           weights = weight, dat = bachelor_higher))

# ===================
# RACE
# ===================
white <- temp %>% subset(race == "White")
summary(lm(admission_dichotimized ~ treatment+
             base_knowledge + immigrant_status + ideology + party + 
             age + income + educ_d + religion,
           weights = weight, dat = white ))
summary(lm(acceptance_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + party + 
             age + income + educ_d + religion,
           weights = weight, dat = white))
summary(lm(crime_dichotimized ~ treatment + 
             base_knowledge + immigrant_status + ideology + party + 
             age + income + educ_d + religion,
           weights = weight, dat = white))
summary(lm(influx_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + party + 
             age + income + educ_d + religion,
           weights = weight, dat = white))
summary(lm(threat_dichotimized ~ treatment + 
             base_knowledge + immigrant_status + ideology + party + 
             age + income + educ_d + religion,
           weights = weight, dat = white))
summary(lm(voting_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + party + 
             age + income + educ_d + religion,
           weights = weight, dat = white))

black <- temp %>% subset(race == "Black or African American")
summary(lm(admission_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + party + 
             age + income + educ_d + religion,
           weights = weight, dat = black ))
summary(lm(acceptance_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + party + 
             age + income + educ_d + religion,
           weights = weight, dat = black))
summary(lm(crime_dichotimized ~ treatment + 
             base_knowledge + immigrant_status + ideology + party + 
             age + income + educ_d + religion,
           weights = weight, dat = black))
summary(lm(influx_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + party + 
             age + income + educ_d + religion,
           weights = weight, dat = black))
summary(lm(threat_dichotimized ~ treatment + 
             base_knowledge + immigrant_status + ideology + party + 
             age + income + educ_d + religion,
           weights = weight, dat = black))
summary(lm(voting_dichotimized ~ treatment +
             base_knowledge + immigrant_status + ideology + party + 
             age + income + educ_d + religion,
           weights = weight, dat = black))
