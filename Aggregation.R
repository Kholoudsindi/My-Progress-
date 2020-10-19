# irrigation analysis
# kholoud sindi
# 01.10.2020
# a small case study

# laod packges 
library(tidyverse)

# begin with widee "messy" format:
irrigation <- read_csv("data/irrigation_wide.csv")
  
  
# examine the data 
glimpse(irrigation)
summary(irrigation)  

#in 2007 what is the total area under ittigation?
#for only the americas
irrigation %>% 
  filter(year == "2007") %>% 
  select(ends_with("erica"))


irrigation %>% 
  filter(year == "2007") %>% 
  select('N.America', 'S.America')


# use numbers for culomns and sum the value 
irrigation %>% 
  filter(year == "2007") %>% 
  select(4, 5) %>% 
  sum()

# to anser followeing questions you need a tidy data
irrigation_t <- irrigation %>% 
  pivot_longer(-year, names_to = "region")

#whats the total area under irrigation in each year?
irrigation_t %>% 
  group_by(year) %>% 
  summarise(total = sum(value))

#what is the rate-of-change in each region?
xxx <- c(1, 1.2, 1.6, 1.1)
xxx

#there are the absolute diffrinces:
c(0, diff(xxx))
(1.2-1)/1
(1.6-1.2)/1.2
(1.1-1.6)/1.6

diff(xxx)/xxx[-length(xxx)]

# how about the proportional: 


irrigation_t <-  irrigation_t %>% 
  arrange(region) %>% 
  group_by(region) %>% 
  mutate(rate = c(0, diff(value)/value[-length(value)]))
 



#where is the lowest and highest?
irrigation_t[which.max(irrigation_t$rate), ]
irrigation_t[which.min(irrigation_t$rate),] 


irrigation_t %>% 
  slice_max(rate, n = 1)
  
# because the tibble is still a group_df
# to get the global answer wh have to ungroup()

# highest:
irrigation_t %>% 
  ungroup() %>% 
  slice_max(rate, n = 1)

#lowest: 
irrigation_t %>% 
  ungroup() %>% 
  slice_min(rate, n = 1)


#standrize against 1980 (relative change over 1980)
irrigation_t %>% 
  group_by(region) %>% 
  summarise(diff = value[year == 2007] - value[year == 1980])


#arrange from lowest to highest
# with mines - it arranges from highest to lowest 
irrigation_t %>% 
  group_by(region) %>% 
  summarise(diff = value[year == 2007] - value[year == 1980]) %>% 
  arrange(-diff) %>% 
  slice(1:2) #if i write 1:2 it gives me the first 2

#our use top_n
irrigation_t %>% 
  group_by(region) %>% 
  summarise(diff = value[year == 2007] - value[year == 1980]) %>% 
  arrange(-diff) %>% 
  slice_max(diff, n = 2)


#plot area over time for each region?


#which region increased most 1980 to 2007?



