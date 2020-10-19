# diamond analysis 
# kholoud sindi 
# 29.09.2020
# a small case study for EDA and stat 

# load packages 
library(tidyverse)

# read the data (csv format):
# newer methods from the tidyr patch
  jems <- read_csv("data/diamonds.csv")
  read_

  getwd() # tell me what project i'm in
  
# super convenient way 
  library(rio)
  jems2 <- import("data/diamonds.csv")

# get familiar with our data
  summary(jems)

# display the names:
  names(jems)
  
# structure 
  glimpse(jems)
  
#more details 
  attributes(jems)
  typeof(jems)

#reminder of what group b is 
  jems %>% 
    group_by(cut, color)
  
#basic filtering 
#are there any diamonds with vvs2( clarity) & good (cut)
  
jems %>%
  filter(clarity == "VVS2" & cut == "Good")

# How many diamonds with a clarity of category "IF" are present in the data-set?

clarity <- jems %>%
  filter(clarity == "IF") %>% 
  nrow(clarity)

sum(jems$clarity == "IF")


#What fraction of the total do they represent?
nrow(clarity/nrow(jems))

  
#What proportion of the whole is made up of each category of clarity?
jems %>% 
  group_by(clarity) %>% 
  count() %>% 
  mutate(prop = n/nrow(jems))


# What is the cheapest diamond price overall? 
min(jems$price)

# what are the cheapest diamonds (2-way tie)
jems %>% 
  filter(price == min(jems$price))
  
# What is the range of diamond prices? 
range(jems$price)

jems %>% 
  filter(price == range(jems$price))

# What is the average diamond price in each category of cut and color?
jems %>% 
  group_by(cut, color) %>% 
  summarise(avg = mean(price))


# Make a scatter plot that shows the price of a diamond as described
#by another continuous variable, like the carat
ggplot(jems, aes(x = carat, y = price)) + 
  geom_point()


#transformation from the tidyverse:

jems <- jems %>% 
  mutate(carat_log10 = log10(carat), 
         price_log10 = log10(price))


# alternitave old school way 
jems$price_log10 <- log10(jems$price)
jems$carat_log10 <- log10(jems$carat)

# plot the transformed variables 
ggplot(jems, aes(x = carat_log10, y = price_log10)) + 
geom_point()


# produce our model: y ~ x 
jems_lm <- lm(price_log10 ~ carat_log10 , data = jems)
jems_lm  
jems_lm$coefficint 

#plot 
ggplot(jems, aes(x = carat_log10, y = price_log10)) + 
  geom_point()+
  geom_smooth(method = 'lm',
              se = FALSE, 
              colour = "red")


ggplot(jems, aes(y = price_log10, x = carat_log10)) + 
geom_smooth(price_log10 ~ carat_log10)
