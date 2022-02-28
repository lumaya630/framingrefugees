library(readxl)
library(tidyverse)

# add lm roman font
font_add(family = "lmroman", 
         regular = "Library/Fonts/lmroman10-regular.otf")
showtext_auto()

# read in total population of refugees (source: UNHCR)
population <- read.csv("population.csv", skip=14) %>% 
  select(c("Year", "Refugees.under.UNHCR.s.mandate")) %>% 
  rename(
    year = Year,
    population = "Refugees.under.UNHCR.s.mandate"
  )

# read in us_admissions
us_admissions <- read_excel("PRM Refugee ADmissions Report as of 31 Dec 2021.xlsx", 
           sheet = "Cumulative Summary", skip = 10) %>%
  select(c(Year, Total)) %>%
  mutate(Year = as.integer(Year)) %>%
  subset(!is.na(Year))

# plot
colors <- c("US Admissions" = "blue", "Global Population" = "black")

ggplot(data = population, aes(x = year, y = population)) + 
  geom_line(aes(color = "Global Population")) +
  geom_line(data = us_admissions, aes(x = as.integer(Year), y = (Total * 100), color = "US Admissions")) +
  scale_y_continuous(
    name = "Global Refugee Population",
    sec.axis = sec_axis(~./ 100 , name = "US Admission of Refugees")) + 
  
  scale_color_manual("Population", values = colors) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(text=element_text(family="lmroman")) +
  theme_simple() 
  
  

