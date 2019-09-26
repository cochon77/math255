dorm <-  read.csv("http://math.carleton.edu/kstclair/data/Dorm_Cluster.csv")

summary(aov(gpa~as.factor(room), data=dorm))

1-4/3*((300*0.185)/(99*0.5639+300*0.1850))

0.5639/((99*0.5639+300*0.1850)/399)


M <-c(10,15,18,22,17)

tot1 <- c(1100,1020,972,704,714)
sum((tot1-(4510/82)*M)^2)/4
sqrt((1-5/400)*(sum((tot1-(4510/82)*M)^2)/4)/(5*(82/5)^2))


tot<-c(8,5,7,15,3)
sum((tot-(38/82)*M)^2)/4
sqrt((1-5/400)*(sum((tot-(38/82)*M)^2)/4)/(5*(82/5)^2))

