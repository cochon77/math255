agsrs <- read.csv("http://math.carleton.edu/kstclair/data/agsrs.csv")
library(survey)
agsrs$n <- nrow(agsrs)
agsrs$N <- 3078
agsrs$wts <- agsrs$N/agsrs$n
design.srs <- svydesign(id=~1, fpc=~N, wegiths=~wts, data=agsrs)
region.tot <- svyby(~acres92, ~region, design.srs, svytotal)
region.tot
confint(region.tot, df=degf(design.srs))
region.mean <- svyby(~acres92, ~region, design.srs, svymean)
region.mean
confint(region.mean, df=degf(design.srs))

west.ind <- ifelse(agsrs$region == "W", 1, 0)
design.srs <- update(design.srs, west.ind = west.ind)
svyby(~acres92, ~west.ind, design.srs, svymean)

svymean(~acres92, design.srs)
svytotal(~acres92, design.srs)

levels(agsrs$region)
pop.ps <- data.frame(region=c("NC","NE","S","W"), N.str = c(1054,220,1382,422))
pop.ps

design.post<-postStratify(design.srs, strata=~region, population = pop.ps)
svymean(~acres92, design.post)
svytotal(~acres92, design.post)
table(agsrs$region)
