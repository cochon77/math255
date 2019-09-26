dorm <- read.csv("http://math.carleton.edu/kstclair/data/Dorm_Cluster.csv")
dorm$N <- 100
dorm$n <- 5
dorm$wts <- dorm$N/dorm$n
library(survey)
design.clus <- svydesign(id=~room, fpc=~N, wegiths = ~wts, data=dorm)
svymean(~gpa, design.clus, deff=T)
confint(svymean(~gpa, design.clus), df = degf(design.clus))

algebra <- read.csv("http://math.carleton.edu/kstclair/data/algebra.csv")
algebra$N <- 187
nrow(algebra)
unique(algebra$class)
algebra$n <- n_distinct(algebra$class)
algebra$wts <- algebra$N/algebra$n
alg.design <- svydesign(id=~class, fpc=~N, weights=~wts, data=algebra)
svymean(~score, alg.design)
confint(svymean(~score, alg.design), df = degf(alg.design))
degf(alg.design)
