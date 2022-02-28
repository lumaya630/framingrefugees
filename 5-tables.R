library(stargazer)
temp <- read.csv("survey_recoded_weighted.csv")

# DEMOGRAPHIC TABLE
demographic <- c("Asian", "Black/African American", "Hispanic/Latino",
                 "White", "Other", 
                 "Associate or Less", "Bachelor or Higher",
                 "Democrat", "Republican", "Other",
                 "18 to 24", "25 to 39", "40 to 59", "60+")
population <- c(4.47,
                12.8,
                12.4,
                76.4,
                6.35,
                70.1,
                29.9,
                27,
                31,
                41,
                12.4,
                25.1,
                32.9,
                29.6
)

sample <- c(5.77,
            10.58,
            5.00,
            77.98,
            0.67,
            16.83,
            83.17,
            64.13,
            11.73,
            24.13,
            3.08,
            63.56,
            28.75,
            4.62
            )
tab <- data.frame(demographic, population, sample)
stargazer(tab, summary = F, rownames = F) 
rm(demographic, population, sample)

# REGRESSION TABLES
# store models
mod_admission <- lm(admission_dichotimized ~ treatment + 
             party + immigrant_status + ideology + educ_d + 
             age + income + race + base_knowledge + religion, 
           dat = temp, 
           weights = weight)

mod_acceptance<- lm(acceptance_dichotimized ~ treatment + 
                      party + immigrant_status + ideology + educ_d + 
                      age + income + race + base_knowledge + religion, 
                    dat = temp, 
                    weights = weight)
mod_crime<- lm(crime_dichotimized ~ treatment + 
                      party + immigrant_status + ideology + educ_d + 
                      age + income + race + base_knowledge + religion, 
                    dat = temp, 
                    weights = weight)
mod_influx<- lm(influx_dichotimized ~ treatment + 
                      party + immigrant_status + ideology + educ_d + 
                      age + income + race + base_knowledge + religion, 
                    dat = temp, 
                    weights = weight)
mod_threat<- lm(threat_dichotimized ~ treatment + 
                      party + immigrant_status + ideology + educ_d + 
                      age + income + race + base_knowledge + religion, 
                    dat = temp, 
                    weights = weight)
mod_voting<- lm(voting_dichotimized ~ treatment + 
                      party + immigrant_status + ideology + educ_d + 
                      age + income + race + base_knowledge + religion, 
                    dat = temp, 
                    weights = weight)

# policy preference
stargazer(mod_admission, mod_acceptance, mod_voting, single.row = TRUE, font.size = "tiny")

# threat perception
stargazer(mod_crime, mod_influx, mod_threat, single.row = TRUE, font.size = "tiny")


# party breakdown
# acceptance
mod_admission_dem <- lm(admission_dichotimized ~ treatment+
                                   base_knowledge + immigrant_status + ideology + education + 
                                   age + income + race + religion,
                                 weights = weight, dat = dem )

mod_admission_rep <- lm(admission_dichotimized ~ treatment+
                           base_knowledge + immigrant_status + ideology + education + 
                           age + income + race + religion,
                         weights = weight, dat = rep )

mod_admission_ind <- lm(admission_dichotimized ~ treatment+
                           base_knowledge + immigrant_status + ideology + education + 
                           age + income + race + religion,
                         weights = weight, dat = party_other)

stargazer(mod_admission_dem, mod_admission_rep, mod_admission_ind, 
          mod_acceptance_dem,mod_acceptance_rep,mod_acceptance_ind, 
          single.row = FALSE, font.size = "tiny", keep = c("treatment"))

# admission
mod_acceptance_dem <- lm(acceptance_dichotimized ~ treatment+
                           base_knowledge + immigrant_status + ideology + education + 
                           age + income + race + religion,
                         weights = weight, dat = dem )

mod_acceptance_rep <- lm(acceptance_dichotimized ~ treatment+
                           base_knowledge + immigrant_status + ideology + education + 
                           age + income + race + religion,
                         weights = weight, dat = rep )

mod_acceptance_ind <- lm(acceptance_dichotimized ~ treatment+
                           base_knowledge + immigrant_status + ideology + education + 
                           age + income + race + religion,
                         weights = weight, dat = party_other)

stargazer(mod_acceptance_dem,mod_acceptance_rep,mod_acceptance_ind, 
          single.row = FALSE, font.size = "tiny", keep = c("treatment"))

# threat
mod_crime_dem <- lm(crime_dichotimized ~ treatment+
                           base_knowledge + immigrant_status + ideology + education + 
                           age + income + race + religion,
                         weights = weight, dat = dem )
mod_crime_rep <- lm(crime_dichotimized ~ treatment+
                           base_knowledge + immigrant_status + ideology + education + 
                           age + income + race + religion,
                         weights = weight, dat = rep )
mod_crime_ind <- lm(crime_dichotimized ~ treatment+
                           base_knowledge + immigrant_status + ideology + education + 
                           age + income + race + religion,
                         weights = weight, dat = party_other)
mod_influx_dem <- lm(influx_dichotimized ~ treatment+
                      base_knowledge + immigrant_status + ideology + education + 
                      age + income + race + religion,
                    weights = weight, dat = dem )
mod_influx_rep <- lm(influx_dichotimized ~ treatment+
                      base_knowledge + immigrant_status + ideology + education + 
                      age + income + race + religion,
                    weights = weight, dat = rep )
mod_influx_ind <- lm(influx_dichotimized ~ treatment+
                      base_knowledge + immigrant_status + ideology + education + 
                      age + income + race + religion,
                    weights = weight, dat = party_other)

stargazer(mod_crime_dem,mod_crime_rep,mod_crime_ind, 
          mod_influx_dem,mod_influx_rep,mod_influx_ind, 
          single.row = FALSE, font.size = "tiny", keep = c("treatment"))

mod_threat_dem <- lm(threat_dichotimized ~ treatment+
                       base_knowledge + immigrant_status + ideology + education + 
                       age + income + race + religion,
                     weights = weight, dat = dem )
mod_threat_rep <- lm(threat_dichotimized ~ treatment+
                       base_knowledge + immigrant_status + ideology + education + 
                       age + income + race + religion,
                     weights = weight, dat = rep )
mod_threat_ind <- lm(threat_dichotimized ~ treatment+
                       base_knowledge + immigrant_status + ideology + education + 
                       age + income + race + religion,
                     weights = weight, dat = party_other)
mod_voting_dem <- lm(voting_dichotimized ~ treatment+
                       base_knowledge + immigrant_status + ideology + education + 
                       age + income + race + religion,
                     weights = weight, dat = dem )
mod_voting_rep <- lm(voting_dichotimized ~ treatment+
                       base_knowledge + immigrant_status + ideology + education + 
                       age + income + race + religion,
                     weights = weight, dat = rep )
mod_voting_ind <- lm(voting_dichotimized ~ treatment+
                       base_knowledge + immigrant_status + ideology + education + 
                       age + income + race + religion,
                     weights = weight, dat = party_other)

stargazer(mod_threat_dem,mod_threat_rep,mod_threat_ind, 
          mod_voting_dem,mod_voting_rep,mod_voting_ind, 
          single.row = FALSE, font.size = "tiny", keep = c("treatment"))
