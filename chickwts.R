
#Title: chickwts Analysis
# Name: Abdullah Alreaan
# lastUpdate: Oct20,2020
# Description: Chickwts dataset contains data about chickens' food and weight as a way to conduct an experiment to see and compare the effectiveness of different supplements on the growth rate of chickens

library(tidyverse)
library(ggplot2)


data("chickwts")
chickwts
glimpse(chickwts)

summary(chickwts)

chickwts = as_tibble(chickwts)

chiktbl <- chickwts %>%
            group_by(feed) %>%
            summarise(n = n(),
            average = mean(weight),
            SD = sd(weight))
chiktbl

ggplot (chickwts, aes(feed, weight))+
  geom_boxplot()

x <- ggplot(chickwts, aes(feed, weight))


  x +
  geom_jitter(width = 0.2, shape = 16, alpha = 0.75) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), col = "red")
  
  
  chickwts.lm <- lm(weight ~ feed, data=chickwts)
  chickwts.ano <- anova(chickwts.lm)
  
  chickwts.ano
  
  chickwts.av <- aov(weight ~ feed, data = chickwts)
  tukeyTest <- TukeyHSD(chickwts.av)
  
  datatable(tukeyTest$feed)
  
  
  




