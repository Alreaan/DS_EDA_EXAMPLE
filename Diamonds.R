
# Title: Case-Study: Diamonds
# Name: Abdullah Alreaan
# lastUpdate: Nov19,2020
# Description: Diamonds dataset contains information about Diamonds' cuts and  prices with other features/attributes with size of almost 54,000 diamond.

library(tidyverse)
library(ggplot2)
library(dplyr)

#import data and save
jems = read.csv("Data/diamonds.csv")

#also we can import first the save the diamonds data as an object called jems
jems = as_tibble(diamonds)
glimpse(diamonds)

#type of data in each column 
str(jems)
#how many clarity diamonds with category "IF" and their fraction. 
jems %>% 
  group_by(clarity) %>% 
  filter (clarity == "IF") %>% 
  summarise (counts=n()/nrow(jems))

# proportion for each category of clarity
jems %>% 
  group_by(clarity) %>% 
  summarise(counts=n())

#cheapest diamond
min(jems$price)
#range of prices
range(jems$price)
#averge diamond price in cut and color
jems %>% 
  group_by(cut, color) %>% 
  summarise(n=n(),
             mean= mean(price))

#scatter plot
ggplot(aes(carat,price),data=jems)+
  geom_point()

ggplot(jems,aes(log10(carat), log10(price)))+
  geom_point()


#create Model
jems_lm <- lm(carat ~ price, data = jems)
jems_lm
#plot with new layer
ggplot(jems,aes(log10(carat), log10(price)))+
  geom_point()+
  geom_smooth(method = 'lm',formula = y ~ x)
 
 #save to file

write.csv(jems, "diamondsCaseStudy")

