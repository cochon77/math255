coots <- read.csv("http://math.carleton.edu/kstclair/data/coots.csv")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(survey)

## dependence within cluster
ggplot(coots, aes(x=csize, y=volume)) + geom_point() + 
  labs(x = "Clutch Size (M_i)", title = "Larger clutch sizes have larget egg volumes")
ggplot(coots, aes(x=clutch, y=volume)) + geom_point() +
  labs(x = "Cluster ID", title = "Volume of effes within the same clutch are similar")

## summary stats
str(coots)
clutch.summary <- coots %>% group_by(clutch) %>% 
  summarize(Mi = first(csize), mi = n(), vol.mean = mean(volume), vol.sd = sd(volume))
summary(clutch.summary)

## svydesign
coots$elem.id <- 1:nrow(coots) ## unique id for each unique element (egg)
coots <- coots %>% group_by(clutch) %>% mutate(mi = n()) %>% ungroup()
coots$wts <- coots$csize/coots$mi ## since N is unknown, give relative weights Mi/mi
coots.design <- svydesign(id = ~clutch+elem.id, weights = ~wts, data = coots)
summary(coots.design) 
mn.obj <- svymean(~volume, coots.design, deff=T)
mn.obj

## N is known
n_distinct(coots$clutch) ## stage 1 sample size
coots$N <- 1000
coots$wts2 <- (coots$N*coots$csize)/(n_distinct(coots$clutch)*coots$mi)
coots.design2 <- svydesign(id=~clutch+elem.id, fpc=~N+csize, weights=~wts2, data = coots)
mn.obj2 <- svymean(~volume, coots.design2, deff=T)
mn.obj2
confint(mn.obj, df = degf(coots.design2))
svytotal(~volume, coots.design2, deff=T)
sum(coots$volume*coots$wts2)

## example 5.11
aov(volume~factor(clutch), data = coots)
summary(aov(volume~factor(clutch), data = coots))
msw <- 1.09008/194
s2 <- var(coots$volume)
1-msw/s2
summary(lm(volume~as.factor(clutch),data = coots))$adj.r.squared

