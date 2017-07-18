dilemma = read.csv("Data - Deans Dilemma.csv", TRUE, ",")

placed = dilemma[which(dilemma$Placement == "Placed"),]
aggregate(placed$Salary,by = list(placed$Gender) ,FUN= mean)

t.test(placed$Salary~placed$Gender,var.equal = TRUE)
