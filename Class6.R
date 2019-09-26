pop <- read.csv("http://math.carleton.edu/kstclair/data/StatsClassSurvey.csv")
nrow(pop)
y <- pop$Height
sd(y)
mean((y-mean(y))^3)/sd(y)^3

28 + 25*(mean((y-mean(y))^3)/sd(y)^3)^2

y <- pop$Exercise
y <- y[y<4000]
length(y)
sd(y, na.rm=T)
mean((y-mean(y, na.rm = T))^3, na.rm=T)

28 + 25*(mean((y-mean(y, na.rm = T))^3, na.rm=T)/sd(y, na.rm=T)^3)^2
