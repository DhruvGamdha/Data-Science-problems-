
store = read.csv("Store24 (A).csv", TRUE,",") 
summary(store)

View(store)

mean(store$Profit)
sd(store$Profit)

mean(store$MTenure)
sd(store$MTenure)

mean(store$CTenure)
sd(store$CTenure)


attach(store)

## creating new dataframe sortedStore 
## sorting store data on the basis of increasing/decreasing profit's 
sortedStore = store[order(Profit),]

sortedStore[1:10,1:5]

sortedStore = store[order(-Profit),]

sortedStore[1:10,1:5]

##Ploting Tenure(Manager and Crew) VS Profit
plot(MTenure,Profit)

plot(CTenure,Profit)
