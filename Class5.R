## Lohr Example 2.5

library(SDaA)
str(agsrs)
summary(agsrs$acres92)
sd(agsrs$acres92)
hist(agsrs$acres92, main = "Millions of acres per county", xlab = "")

agsrs$lt200k92 <- ifelse(agsrs$acres92 < 200000, 1, 0) 
agsrs$lt200k92[1:20]
agsrs$acres92[1:20]
mean(agsrs$lt200k92)

agsrs$N <- 3078
agsrs$wts <- agsrs$N/nrow(agsrs)
str(agsrs)

library(survey)
design.srs <- svydesign(id = ~1, fpc =  ~N, weights = ~ wts, data = agsrs)
summary(design.srs)

tot.obj <- svytotal(~acres92, design.srs)
tot.obj

cv(tot.obj)
confint(tot.obj)
confint(tot.obj, df = degf(design.srs))

mean.obj <- svymean(~acres92+lt200k92, design.srs)
mean.obj

cv(mean.obj)
confint(mean.obj)
confint(mean.obj, df = degf(design.srs))

t.test(agsrs$acres92)

agsrs$lt200k92 <- ifelse(agsrs$acres92 < 200000, "less than 200k", "greater than 200k")
table(agsrs$lt200k92)

design.srs <- svydesign(id = ~1, fpc = ~N, weights = ~wts, data = agsrs)
svymean(~lt200k92, design.srs)

## Don't Know N, but know weights
design2.srs <- svydesign(id = ~1, weights = ~wts, data = agsrs)
summary(design2.srs)
## Don't know N and weights
design3.srs <- svydesign(id = ~1, data = agsrs)
summary(design3.srs)

svytotal(~acres92, design.srs)
svytotal(~acres92, design2.srs) ## if FPC not specified, it assumes the sampling design is with replacement (so SE is big)
svytotal(~acres92, design3.srs) ## we can't get total estimate (because we don't know N)

svymean(~acres92, design.srs)
svymean(~acres92, design2.srs) ## if FPC not specified, it assumes the sampling design is with replacement (so SE is big)
svymean(~acres92, design3.srs) ## mean estimate and SE are correct
sd(agsrs$acres92)/sqrt(300) ## SEs for (2) and (3) are just sd/sqrt(n): with replacement assumption

acres.diff <- agsrs$acres92-agsrs$acres87
hist(acres.diff/1000, main="Acreage Difference (thousands) 1992-1987", xlab="")
design.srs <- update(design.srs, acres.diff = acres.diff)
diff.obj <- svytotal(~acres.diff, design.srs)
diff.obj
confint(diff.obj, df = degf(design.srs))

new.acres <- agsrs$acres92
is.na(new.acres)[1:10]
sum(is.na(new.acres)[1:10])

new.acres[1] <- NA
is.na(new.acres)[1:10]
sum(is.na(new.acres)[1:10])

mean(agsrs$acres92)
mean(new.acres) # fails because of NA
mean(new.acres, na.rm=T) ## mean of 299 counties

design.srs <- update(design.srs, new.acres = new.acres)
svymean(~acres92+new.acres, design.srs) #question: why Inf in SE?
svymean(~acres92+new.acres, design.srs, na.rm = T)
svymean(~acres92, design.srs)

## svymean uses fpc = (1-300/3078) instead of (1-299/3078)

sqrt((1-300/3078)/299)*sd(new.acres,na.rm = T) # SE using na.rm = T (svymean)
sqrt((1-299/3078)/299)*sd(new.acres,na.rm = T) # SE using n = 299 instead of 300
sqrt((1-300/3078)/300)*sd(agsrs$acres92) #SE with no missing
