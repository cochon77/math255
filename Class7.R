library(survey)
library(SDaA)
str(agstrat)
levels(agstrat$region)

table(agstrat$region)
tapply(agstrat$acres92, agstrat$region, summary)
tapply(agstrat$acres92, agstrat$region, sd)
library(dplyr)
agstrat %>% group_by(region) %>% summarize(mean = mean(acres92), sd = sd(acres92))

boxplot(acres92/1000000~region, ylab = "millions of acres",xlab = "region", data = agstrat)

levels(agstrat$region)

agstrat$N <- recode(agstrat$region, NC = 1054, NE = 220, S = 1382, W = 422)
agstrat %>% group_by(region) %>% summarize(min(N), max(N))

agstrat <- agstrat %>% group_by(region) %>% mutate(n = n())
agstrat %>% group_by(region) %>% summarize(min(n),max(n)) # check
agstrat$wts <- agstrat$N/agstrat$n
agstrat %>% group_by(region) %>% summarize(min(wts),max(wts)) #check

design.strat <- svydesign(id=~1, fpc=~N, weights =~wts, strata = ~region, data=agstrat)
summary(design.strat)

svytotal(~acres92, design.strat)
confint(svytotal(~acres92, design.strat))
confint(svytotal(~acres92, design.strat), df = degf(design.strat))

svymean(~acres92, design.strat)
confint(svymean(~acres92, design.strat))
confint(svymean(~acres92, design.strat), df = degf(design.strat))

lt200k92 <- ifelse(agstrat$acres92 < 200000, 1,0)
design.strat <- update(design.strat, lt200k92 = lt200k92)
svymean(~lt200k92, design.strat)
confint(svymean(~lt200k92, design.strat), df = degf(design.strat))

# No pop size (fpc)
design.strat2 <- svydesign(id=~1, strata = ~region, weights =~wts, data = agstrat)
summary(design.strat2)

## No pop size (fpc) and weights
design.strat3 <- svydesign(id=~1, strata = ~region, data = agstrat)
summary(design.strat3)
svytotal(~acres92, design.strat3) # pretty off
svymean(~acres92, design.strat3) # close but not correct estimate
mean(agstrat$acres92)
sum(agstrat$acres92)

# Testing for Precision
svytotal(~acres92, design.strat, deff=T)
50417248^2/58169381^2 ## different from above, we will see

# Within Strata
region.mean <- svyby(~acres92, ~region, design.strat, svymean)
region.mean
confint(region.mean, df = degf(design.strat))

region.total <- svyby(~acres92, ~region, design.strat, svytotal)
region.total
confint(region.total, df = degf(design.strat))

# NA
dim(agpop)
sum(complete.cases(agpop)) ## seems complete?
length(which(agpop==-99)) ## we are missing 59 values (NAs)

agpop[agpop==-99] <- NA
sum(complete.cases(agpop))
which(agpop==-99)
