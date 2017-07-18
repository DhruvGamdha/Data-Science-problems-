library(UsingR)

data(father.son)

y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)

x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)

rho <- cor(x, y)

library(ggplot2)

g = ggplot(data.frame(x, y), aes(x = x, y = y))

g = g + geom_point(size = 5, alpha = .2, colour = "black")

g = g + geom_point(size = 4, alpha = .2, colour = "red")

g = g + geom_vline(xintercept = 0)

g = g + geom_hline(yintercept = 0)

g = g + geom_abline(position = "identity")

## Warning: Ignoring unknown parameters: position

g = g + geom_abline(intercept = 0, slope = rho, size = 2)

g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)

g = g + xlab("Father's height, normalized")

g = g + ylab("Son's height, normalized")

g
