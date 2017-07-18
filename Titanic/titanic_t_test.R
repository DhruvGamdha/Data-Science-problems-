
titanic = read.csv("Titanic Data.csv",TRUE,",")
View(titanic)

titanic$Survived = as.factor(titanic$Survived)

aggregate(titanic$Age, by=list(titanic$Survived),FUN = mean)

t.test(titanic$Age~titanic$Survived)

