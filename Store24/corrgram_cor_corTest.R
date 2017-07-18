store = read.csv("Store24 (A).csv",TRUE,',')

## Correlation matrix
cor(store, method="spearman")

mean(store$Profit) ## mean profit  

##Visualizing correlations among store variables  
library(corrgram)
corrgram(store, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of store variables")

View(store)

cor(store[,c("Profit")],store[,c("MTenure")]) ## Correlation between Profit and MTenure
cor(store[,c("Profit")],store[,c("CTenure")]) ## Correlation between Profit and CTenure


cor.test(store[,"Profit"], store[,"MTenure"])
cor.test(store[,"Profit"], store[,"CTenure"])

attach(store)
fit = lm(Profit~MTenure+CTenure+Comp+Pop+PedCount+Res+Hours24+Visibility)

summary(fit)


