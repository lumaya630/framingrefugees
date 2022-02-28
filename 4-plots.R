library(gridExtra)
library(tidyverse)
library(sjPlot)
library(showtext)
library(extrafont)

temp <- read.csv("survey_recoded_weighted.csv")

# add lm roman font
font_add(family = "lmroman", 
         regular = "Library/Fonts/lmroman10-regular.otf")
showtext_auto()

# helper function for plotting estimated effects
plot_effects <- function(mods, title) {
  # empty data frame
  effects = data.frame(var = rep(NA,12), treatment = rep(NA, 12), 
                       value = rep(NA, 12), se = rep(NA, 12))
  
  # fill in data frame
  for (i in 1:6){
    var_order <- c("National Policy Preference", "Local Policy Preference",
                   "Danger Perception", "Border Safety", "Threat Perception",
                   "Voting Behavior")
    treatment_order <- c("Traditional Security", "Border Security")
    
    for (j in 1:2){
      idx = ((i-1) * 2) + j
      mod = mods[[i]]
      
      # label to dependent var for the observation
      effects$var[idx] = var_order[i]
      
      # label the treatment group for the observation
      effects$treatment[idx] = treatment_order[j]
      
      # fill in the values and standard error
      effects$value[idx] = mod[j, 1]
      effects$se[idx] = mod[j, 2]
    }
  }
  
  # plot
  effects$var <- factor(effects$var, levels=unique(effects$var))
  ggplot(effects, aes(x = fct_rev(var), y = value, color = treatment)) +
    geom_pointrange(aes( x = var, y = value, ymin = value - 2*se, ymax = value + 2*se),
                    position = position_dodge(0.5) 
    ) +
    geom_hline(yintercept = 0, lty= "dashed", alpha = 0.5) +
    xlab("") +
    ylab("\nTreatment Effect") + 
    ggtitle(title) +
    theme(text=element_text(family="lmroman"),
          legend.key = element_rect(fill = "white")) +
    guides(col=guide_legend("Treatment Group")) +
    theme_simple() +
    coord_flip() + 
    scale_x_discrete(limits = rev)
}


e <- new.env()
create_mod_list <- function(dat, excluded_var = "none", weighted = T){
# general population
  if (weighted == F){
    e$weights = NULL
  }else{e$weights = dat$weight}

  # get control variables
  controls <- c("party", "immigrant_status", "ideology", "educ_d",
                  "age", "income", "race", "base_knowledge", "religion")
  controls <- controls[controls != excluded_var]
  
  x = "treatment"
  mod_admission <- lm(paste("admission_dichotimized", 
                            paste(c(x, controls), collapse="+"), sep="~"),
                      data=dat, weights = eval(parse(text = "weights"), envir=e))
  
  mod_acceptance<- lm(paste("acceptance_dichotimized", 
                                  paste(c(x, controls), collapse="+"), sep="~"),
                            data=dat, weights = eval(parse(text = "weights"), envir=e))
  
  mod_crime<-lm(paste("crime_dichotimized", 
                          paste(c(x, controls), collapse="+"), sep="~"),
                    data=dat, weights = eval(parse(text = "weights"), envir=e))
  
  mod_influx<- lm(paste("influx_dichotimized", 
                        paste(c(x, controls), collapse="+"), sep="~"),
                  data=dat, weights = eval(parse(text = "weights"), envir=e))
  
  mod_threat<- lm(paste("threat_dichotimized", 
                            paste(c(x, controls), collapse="+"), sep="~"),
                      data=dat, weights = eval(parse(text = "weights"), envir=e))
  
  mod_voting<- lm(paste("voting_dichotimized", 
                            paste(c(x, controls), collapse="+"), sep="~"),
                      data=dat, weights = eval(parse(text = "weights"), envir=e))
  
  mods <- list(summary(mod_admission)$coefficients[2:3, c(1:2,4)],
               summary(mod_acceptance)$coefficients[2:3, c(1:2,4)],
               summary(mod_crime)$coefficients[2:3, c(1:2,4)],
               summary(mod_influx)$coefficients[2:3, c(1:2,4)],
               summary(mod_threat)$coefficients[2:3, c(1:2,4)],
               summary(mod_voting)$coefficients[2:3, c(1:2,4)])
  
  return(mods)
}


# general pop
mods_general <- create_mod_list(temp)
plot_effects(mods_general, "Effects of Frames on the US Population")

# democrats
dem <- temp %>% subset(party == "Democrat")
mods_dem <- create_mod_list(dem, "party")
plot_effects(mods_dem, "Effects of Frames on Democrats")

# republicans
rep <- temp %>% subset(party == "Republican")
mods_rep <- create_mod_list(rep, "party")
plot_effects(mods_rep, "Effects of Frames on Republicans")

# independents
ind <- temp %>% subset(party == "Other")
mods_ind <- create_mod_list(ind, "party")
plot_effects(mods_ind, "Effects of Frames on Independents")

# familiarity
familiar <- temp %>% subset(base_knowledge == 2)
mods_familiar <- create_mod_list(familiar, "base_knowledge")
plot_effects(mods_familiar, "Effects of Frames on Those Who Identify as Very Familiar with Refugees/Asylum Seekers")

# unfamiliar
unfamiliar <- temp %>% subset(base_knowledge %in% c(0,1))
mods_unfamiliar <- create_mod_list(unfamiliar, "base_knowledge")
plot_effects(mods_unfamiliar, "Effects of Frames as Not Familiar or Somewhat Familiar with Refugees/Asylum Seekers")

# education
bach_higher <- temp %>% subset(education == "Bachelor or higher")
mods_bachelor <- create_mod_list(bach_higher, "education")
plot_effects(mods_bachelor, "Effects of Frames on those with Bachelor's Degree or Higher")

# education
assoc_less <- temp %>% subset(education == "Associate or less")
mods_associate <- create_mod_list(assoc_less, "education")
plot_effects(mods_associate, "Effects of Frames on those with an Associate Degree or Less")

# unweighted
mods_unweighted <- create_mod_list(temp, weighted = F)
plot_effects(mods_unweighted, "Effects of Frames on the US Population")

#age
older <- temp %>% subset(age_binned == "40 to 59")
mods_older <- create_mod_list(older, "age_binned")
plot_effects(mods_older, "Effects of Frames on Older Population")

younger <- temp %>% subset(age < 30)
mods_younger <- create_mod_list(younger, "age_binned")
plot_effects(mods_younger, "Effects of Frames on Younger Population")

# liberal
liberal <- temp %>% subset(ideology < 0)
mods_liberal <- create_mod_list(liberal, "ideology")
plot_effects(mods_liberal, "Effects of Frames on Liberal Population")

# conservative
conservative <- temp %>% subset(ideology > 0)
mods_convservative <- create_mod_list(conservative, "ideology")
plot_effects(mods_convservative, "Effects of Frames on Conservative Population")

table(dem$base_knowledge)/sum(table(dem$base_knowledge))
table(ind$base_knowledge)/sum(table(ind$base_knowledge))

#without control
no_control <- temp %>% subset(treatment != "control")
mod_no_control <- lm(admission_dichotimized ~ treatment, data = no_control, weights = weight)
summary(mod_no_control)
mods_no_control <- create_mod_list(no_control)
plot_effects(mods_no_control, title = "")

stargazer(mod_no_control,
          single.row = FALSE, font.size = "small", keep = c("treatment"),
          report = "vcs*p")

