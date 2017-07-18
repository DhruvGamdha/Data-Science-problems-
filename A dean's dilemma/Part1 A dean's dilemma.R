dilemma = read.csv("Data - Deans Dilemma.csv", TRUE, ",")
View(dilemma)

library(psych)
describe(dilemma)

median(dilemma$Salary)
options(digits = 2)
mean(dilemma$Placement_B)*100

placed = dilemma[which(dilemma$Placement=='Placed'), ]
placed

median(placed$Salary)

aggregate(Salary ~ Gender,data = dilemma,mean)

hist(placed$Percent_MBA, breaks = 3, xlab = "MBA Percentage",ylab = "count",main = "MBA Perforamnce of placed students")

notplaced = dilemma[which(dilemma$Placement=="Not Placed"), ]

par(mfrow=c(1, 2))
hist(notplaced$Percent_MBA, breaks = 3, xlab = "MBA Percentage",ylab = "count",main = "MBA Perforamnce of not placed students")
hist(placed$Percent_MBA, breaks = 3, xlab = "MBA Percentage",ylab = "count",main = "MBA Perforamnce of placed students")
par(mfrow=c(1, 1))



boxplot(placed$Salary~placed$Gender, horizontal = TRUE)

placedET = dilemma[which(dilemma$Placement == "Placed" & dilemma$S.TEST == 1), ]

library(car)
scatterplot.matrix(~Salary+Percent_MBA+Percentile_ET, data = placedET, main = "Scatter Plot Matrix")
