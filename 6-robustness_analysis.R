
# ===================
# OVERALL POPULATION
# ===================

summary(lm(admission_dichotimized ~ treatment + 
             party + immigrant_status + ideology + educ_d + 
             age + income + race + base_knowledge + religion, 
           dat = temp))

summary(lm(acceptance_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + base_knowledge + religion, 
           dat = temp))

summary(lm(crime_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + base_knowledge + religion, 
           dat = temp))

summary(lm(influx_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + base_knowledge + religion, 
           dat = temp))

summary(lm(threat_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + base_knowledge + religion, 
           dat = temp))

summary(lm(voting_dichotimized ~ treatment + 
             party + immigrant_status + ideology + education + 
             age + income + race + base_knowledge + religion, 
           dat = temp))

# store models
mod_admission_unweighted <- lm(admission_dichotimized ~ treatment + 
                      party + immigrant_status + ideology + educ_d + 
                      age + income + race + base_knowledge + religion, 
                    dat = temp)

mod_acceptance_unweighted<- lm(acceptance_dichotimized ~ treatment + 
                      party + immigrant_status + ideology + educ_d + 
                      age + income + race + base_knowledge + religion, 
                    dat = temp)

mod_crime_unweighted<- lm(crime_dichotimized ~ treatment + 
                 party + immigrant_status + ideology + educ_d + 
                 age + income + race + base_knowledge + religion, 
               dat = temp)

mod_influx_unweighted<- lm(influx_dichotimized ~ treatment + 
                  party + immigrant_status + ideology + educ_d + 
                  age + income + race + base_knowledge + religion, 
                dat = temp)

mod_threat_unweighted<- lm(threat_dichotimized ~ treatment + 
                  party + immigrant_status + ideology + educ_d + 
                  age + income + race + base_knowledge + religion, 
                dat = temp)

mod_voting_unweighted<- lm(voting_dichotimized ~ treatment + 
                  party + immigrant_status + ideology + educ_d + 
                  age + income + race + base_knowledge + religion, 
                dat = temp)

# plots
plot_acceptance <- plot_model(mod_acceptance_unweighted, show.p = TRUE,
                              show.values = TRUE,
                              vline.color = "gray",
                              title = "Local Policy Preferences",
                              terms = c("treatmenttraditional security", "treatmentborder security")) +
  ylim(c(-0.4,0.4)) +
  geom_hline(yintercept = 0, color = "gray", size = 1.5, alpha = 0.5) + 
  theme_simple() +
  theme(text=element_text(family="lmroman")) 

plot_admission <- plot_model(mod_admission_unweighted, show.p = TRUE,
                              show.values = TRUE,
                              vline.color = "gray",
                              title = "National Policy Preferences",
                              terms = c("treatmenttraditional security", "treatmentborder security")) +
  ylim(c(-0.4,0.4)) +
  geom_hline(yintercept = 0, color = "gray", size = 1.5, alpha = 0.5) + 
  theme_simple() +
  theme(text=element_text(family="lmroman")) 

plot_crime <- plot_model(mod_crime_unweighted, show.p = TRUE,
                             show.values = TRUE,
                             vline.color = "gray",
                             title = "Danger Perception",
                             terms = c("treatmenttraditional security", "treatmentborder security")) +
  ylim(c(-0.4,0.4)) +
  geom_hline(yintercept = 0, color = "gray", size = 1.5, alpha = 0.5) + 
  theme_simple() +
  theme(text=element_text(family="lmroman")) 

plot_influx <- plot_model(mod_influx_unweighted, show.p = TRUE,
                         show.values = TRUE,
                         vline.color = "gray",
                         title = "Border Safety",
                         terms = c("treatmenttraditional security", "treatmentborder security")) +
  ylim(c(-0.4,0.4)) +
  geom_hline(yintercept = 0, color = "gray", size = 1.5, alpha = 0.5) + 
  theme_simple() +
  theme(text=element_text(family="lmroman")) 

plot_threat <- plot_model(mod_threat_unweighted, show.p = TRUE,
                         show.values = TRUE,
                         vline.color = "gray",
                         title = "Threat Perception",
                         terms = c("treatmenttraditional security", "treatmentborder security")) +
  ylim(c(-0.4,0.4)) +
  geom_hline(yintercept = 0, color = "gray", size = 1.5, alpha = 0.5) + 
  theme_simple() +
  theme(text=element_text(family="lmroman")) 

plot_voting <- plot_model(mod_voting_unweighted, show.p = TRUE,
                         show.values = TRUE,
                         vline.color = "gray",
                         title = "Voting Behavior",
                         terms = c("treatmenttraditional security", "treatmentborder security")) +
  ylim(c(-0.4,0.4)) +
  geom_hline(yintercept = 0, color = "gray", size = 1.5, alpha = 0.5) + 
  theme_simple() +
  theme(text=element_text(family="lmroman")) 

grid.arrange(plot_admission, plot_acceptance, plot_crime, plot_influx, plot_threat, plot_voting, nrow = 3)

