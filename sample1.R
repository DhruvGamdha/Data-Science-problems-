library(UsingR)
View(galton)
library(reshape)
long = melt(galton)

dev.off()
g = ggplot(long, aes(x=value,fill=variable))
g = g + geom_histogram(colour = "black", binwidth = 1)
g = g + facet_grid(. ~ variable)
g

dev.off()
ggplot(galton, aes(x=parent, y = child)) + geom_point()

library(dplyr)

freqData = as.data.frame(table(galton$child,galton$parent))
names(freqData) = c("child","parent","freq")
View(freqData)

freqData$child = as.numeric(as.character(freqData$child))
freqData$parent = as.numeric(as.character(freqData$parent))

g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))

g <- g + scale_size(range = c(2, 20), guide = "none" )

g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))

## Warning: Ignoring unknown aesthetics: show_guide

g <- g + geom_point(aes(colour=freq, size = freq))

g <- g + scale_colour_gradient(low = "lightblue", high="white")

g