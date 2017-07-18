# Analysis of MBA SALARIES
# NAME: Dhruv Gamdha
# COLLEGE / COMPANY: IIT Roorkee


## reading data into R
MBAStartSal = read.csv("MBA Starting Salaries Data.csv", TRUE,',')
View(MBAStartSal)
attach(MBAStartSal)

## summary of dataset 
library(psych)
describe(MBAStartSal)

## creating new dataframe with only those students who mentioned their salary(salMen)  
salMen = subset(MBAStartSal,salary != 999)
salMen = subset(salMen,salary != 998)



## converting sex variable into factor 
MBAStartSal$sex = factor(MBAStartSal$sex, levels = c("1","2"))
salMen$sex = factor(salMen$sex, levels = c("1","2"))


## few plots to understand data better 

boxplot(salMen$salary, main ="starting salary")
boxplot(age, main ="age - in years")
hist(sex  ,main="1=Male; 2=Female")
boxplot(gmat_tot, main ="total GMAT score")
boxplot(work_yrs,  main="years of work experience")
hist(frstlang , main="first language (1=English; 2=other)")
plot(salMen$gmat_vpc,salMen$salary, main = "Gmat verbal percentile vs salary")
plot(salMen$salary~jitter(salMen$age,1), main = "salar vs age")
plot(salMen$salary~jitter(salMen$gmat_tot,1), main="salary vs Gmat total score")



## aggregating salary on the basis of sex

aggregate(salMen$salary~salMen$sex,FUN=mean)
aggregate(salMen$salary~salMen$sex,FUN=sd)





library(corrgram)

## Correlation matrix of salMen 
corrgram(salMen, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of students who's salary is mentioned")




## Dividing salMen on the basis of placed vs notPlaced students 
placed = subset(salMen, salary != 0)
notPlaced = subset(salMen, salary == 0)


## describing placed and non-placed students 
describe(placed)
describe(notPlaced)


## boxplot of the salary of students placed 
boxplot(placed$salary, main ="placed students starting salary")


## partitioning the plot window into 2 
par(mfrow=c(1,2))



## Setting a common range of y-axis for age graphs(placed and not placed students) so as to analyse graphs easily
lmtsAge <- range(placed$age,notPlaced$age)

##boxplot of age(placed and not placed students) 
boxplot(placed$age, main ="placed age - in years ",ylim=lmtsAge)
boxplot(notPlaced$age, main ="not placed age - in years ",ylim=lmtsAge)



## converting sex variable into factor 
placed$sex = factor(placed$sex, levels = c("1","2"))
notPlaced$sex = factor(notPlaced$sex, levels = c("1","2"))



##plot of sex(placed and not placed students) 
plot(placed$sex  ,main="placed 1=Male; 2=Female")
plot(notPlaced$sex  ,main="not placed 1=Male; 2=Female")


## Setting a common range of y-axis for gmat_tot graphs(placed and not placed students) so as to analyse graphs easily
lmtsGmat_tot <- range(placed$gmat_tot,notPlaced$gmat_tot)


##boxplot of gmat_tot(placed and not placed students) 

boxplot(placed$gmat_tot, main ="placed total GMAT score",ylim = lmtsGmat_tot)
boxplot(notPlaced$gmat_tot, main ="not placed total GMAT score", ylim = lmtsGmat_tot)



## Setting a common range of y-axis for gmat_vpc graphs(placed and not placed students) so as to analyse graphs easily
lmtsGmat_vpc <- range(placed$gmat_vpc,notPlaced$gmat_vpc)

##boxplot of gmat_vpc(placed and not placed students) 
boxplot(placed$gmat_vpc, main ="placed verbal GMAT percentile",ylim = lmtsGmat_vpc)
boxplot(notPlaced$gmat_vpc, main ="not placed verbal GMAT percentile",ylim = lmtsGmat_vpc)



## Setting a common range of y-axis for s_avg graphs(placed and not placed students) so as to analyse graphs easily
lmtsS_avg <- range(placed$s_avg,notPlaced$s_avg)


##boxplot of s_avg(placed and not placed students) 
boxplot(placed$s_avg, main= "placed spring MBA average", ylim = lmtsS_avg)
boxplot(notPlaced$s_avg, main= "not placed spring MBA average",ylim = lmtsS_avg)




## Setting a common range of y-axis for f_avg graphs(placed and not placed students) so as to analyse graphs easily
lmtsF_avg <- range(placed$f_avg,notPlaced$f_avg)

##boxplot of f_avg(placed and not placed students) 
boxplot(placed$f_avg, main = "placed fall MBA average", ylim = lmtsF_avg)
boxplot(notPlaced$f_avg, main = "not placed fall MBA average", ylim = lmtsF_avg)



## converting frstlang variable into factor 
placed$frstlang = factor(placed$frstlang, levels = c("1","2"))
notPlaced$frstlang = factor(notPlaced$frstlang, levels = c("1","2"))


plot(placed$frstlang , main="placed first language (1=English; 2=other)" )
plot(notPlaced$frstlang , main="not placed first language (1=English; 2=other)" )


mean(placed$satis)
mean(notPlaced$satis)
sd(placed$satis)
sd(notPlaced$satis)



## contigency tables 

notPlacedsexAndFrstlang = xtabs(~sex+frstlang , data = notPlaced)
notPlacedsexAndFrstlang
chisq.test(notPlacedsexAndFrstlang)


## plots to visualize variation of Gmat score with salary
plot(placed$gmat_tot, placed$salary)
plot(placed$gmat_vpc, placed$salary)


## T-test on gmat_tot and gmat_vpc of placed and not placed students 
t.test(placed$gmat_tot,notPlaced$gmat_tot)
t.test(placed$gmat_vpc,notPlaced$gmat_vpc)


## correlation matrix of placed students 
corrgram(placed, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of placed students")

## Regression models of placed students dataset 

placedfit1 = lm(salary~., data = placed ) 
summary(placedfit1)


placedfit2 = lm(salary~work_yrs+age+quarter+s_avg + f_avg+gmat_qpc + gmat_tot + gmat_tpc + gmat_vpc, data = placed ) 
summary(placedfit2)

anova(placedfit1,placedfit2)

placedfit3 = lm(salary~. + gmat_tot:s_avg + gmat_tot:f_avg + work_yrs:frstlang + satis:age + satis:sex+ work_yrs:satis , data = placed ) 
summary(placedfit3)

anova(placedfit1,placedfit3)



## Best fitting model out of above two is Placedfit3 

placedfit3
