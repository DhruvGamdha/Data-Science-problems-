#Exercise 1

data(state)

state77 = as.data.frame(state.x77)
View(state77)

names(state77)[4] = "Life.Exp"
names(state77)[6] = "HS.Grad"

##Exercise 2

model = lm(Life.Exp ~ ., data = state77) 
summary(model)

##Exercise 3
model2 = update(model,.~. - Income - Area - Illiteracy)
summary(model2)

##Exercise 4
model3 = lm(Life.Exp ~ HS.Grad+Murder, data = state77)
summary(model3)

##Exercise 5
model4 = lm(Life.Exp ~ HS.Grad + Murder + HS.Grad:Murder, data = state77)
summary(model4)

model4 = lm(Life.Exp ~ HS.Grad*Murder, data = state77)
summary(model4)

model4 = lm(Life.Exp ~ (HS.Grad+Murder)^2, data = state77)
summary(model4)
##Exercise 6 

confint(model3, level = 0.95)

##Exercise 7
sumModel3 = summary(model3)$coefficients
lifeExpec = sumModel3[1,1] + sumModel3[2,1]*55 + sumModel3[3,1]*8

## alternate Exercise 7 

predict(model3, data.frame(HS.Grad = 55, Murder = 8))

##Exercise 8 

predict(model3, data.frame(HS.Grad = 55, Murder = 8), interval = "confidence", level = 0.98)



