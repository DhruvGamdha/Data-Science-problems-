titanic <- read.csv("Titanic Data.csv",TRUE,",")
View(titanic)
dim(titanic,1)
nrow(titanic)

table(titanic$Survived)
mean(titanic$Survived)

mytable<-xtabs(~Survived+Pclass,data = titanic)
mytable

prop.table(mytable)*100

fem1stSurv <- xtabs(~Survived+Sex+Pclass,data = titanic)
fem1stSurv

table(titanic$Survived,titanic$Sex)

prop.table(table(titanic$Survived,titanic$Sex),2)*100

library(vcd)
chisq.test(table(titanic$Survived,titanic$Sex))