#world's happeniess 
# Kholoud Sindi
# 09.10.2020

#load pacakges 
library(tidyverse)
library(ggplot2)
library(RColorBrewer)


report <- read_csv("2019.csv")


# get familiar with our data
summary(report)

# display the names:
names(report)

# structure 
glimpse(report)

#How many countries have a %>%  score lower than 5?
report %>% 
  filter(Score <= 5) %>% 
  count()

#How many countries have a score higher than 7?
report %>% 
  filter(Score >= 7) %>% 
  count() 

#Which country has the lowest corruption? 
report %>% 
  group_by(`Country or region`) %>%
  filter(`Perceptions of corruption` == min(report$`Perceptions of corruption`))

#Which country has the highest social support?
report %>% 
  group_by(`Country or region`) %>%
  filter(`Social support` == max(report$`Social support`))

#arrange countries from lowest to highest based on Healthy life expectancy


#Does the level of freedom has an effect on the happiness score?
ggplot(report, aes(x= `Freedom to make life choices`, y= Score)) +
  geom_jitter()