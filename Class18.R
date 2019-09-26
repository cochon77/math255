library(survey)
library(dplyr)
library(ggplot2)

data(api)
sizes <- apiclus2 %>% group_by(dname) %>% summarize(M = first(fpc2)) %>% ungroup()
sd(sizes$M)
sizes <- apiclus2 %>% group_by(dname) %>% summarize(M = first(fpc2), m = n(), fraction = m/M) %>% ungroup()
max(sizes$fraction)
min(sizes$fraction)

boxplot(api00~dnum, data = apiclus2)
boxplot(growth~dnum, data = apiclus2)
summary(lm(api00 ~ factor(dnum), data=apiclus2))$adj.r.squared
summary(lm(growth ~ factor(dnum), data=apiclus2))$adj.r.squared

api.clus <- svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, weights =~pw, data = apiclus2)
svymean(~api00+growth+sch.wide, api.clus, deff=T)

mean(apipop$api00)
confint(svymean(~api00, api.clus), df = degf(api.clus))
mean(apiclus2$api00)
t.test(apiclus2$api00)$conf.int

ggplot(apiclus2, aes(x=pw, y=api00)) + geom_point() + scale_x_log10()

apiclus2$income <- ifelse(apiclus2$meals >= 50, "low","high")
api.clus <- update(api.clus, income = apiclus2$income)

svyby(~growth, ~income, api.clus, svymean, vartaype=c("se","ci"))
svyttest(growth ~ income, api.clus)

t.test(growth~income, data=apiclus2)
