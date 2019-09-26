library(survey)
library(SDaA)

# C and D

y <- c(24, 245)
probs <- matrix(c(0.5393,0.4567,0.4567, 0.9002), nrow=2, byrow=T)
probs
pi <- diag(probs)
store <- data.frame(y,pi,probs)
store

store.design <- svydesign(id = ~1, fpc = ~pi, data = store, pps=ppsmat(probs))
svytotal(y, store.design)
store.design <- svydesign(id = ~1, fpc = ~pi, data = store, pps=ppsmat(probs), variance = "YG")
svytotal(y, store.design)
store.design <- svydesign(id=~1, fpc=~pi, data = store)
svytotal(y,store.design)

# A and B
probs <- matrix(c(0.1900, 0.0173, 0.0173, 0.3705), nrow=2, byrow=T)
pi <- diag(probs)
y <- c(11,20)
store <- data.frame(y, pi, probs)
store

store.design <- svydesign(id = ~1, fpc = ~pi, data = store, pps=ppsmat(probs))
svytotal(y, store.design)
store.design <- svydesign(id = ~1, fpc = ~pi, data = store, pps=ppsmat(probs), variance = "YG")
svytotal(y, store.design)
store.design <- svydesign(id=~1, fpc=~pi, data = store)
svytotal(y,store.design)

# AG data

agpps <- read.csv("http://math.carleton.edu/kstclair/data/agpps.csv")
dim(agpps)
head(agpps)

n <- 15
incl.mat <- as.matrix(agpps[,10:24])
diag(incl.mat) <- agpps$pii
ag.pps <- svydesign(id=~1, fpc=~pii, data=agpps, pps=ppsmat(incl.mat))
svytotal(~acres92, ag.pps, deff = T)

ag.pps <- svydesign(id=~1, fpc=~pii, data=agpps, pps=ppsmat(incl.mat), variance="YG")
svytotal(~acres92, ag.pps, deff = T)

ag.pps <- svydesign(id=~1, fpc=~pii, data=agpps)
svytotal(~acres92, ag.pps, deff = T)

agsrs$n <- nrow(agsrs)
agsrs$N <- 3078
agsrs$wts <- agsrs$N/agsrs$n
agsrs.design <- svydesign(id=~1, fpc=~N, weights=~wts, data=agsrs)
svytotal(~acres92, agsrs.design, deff=T)
