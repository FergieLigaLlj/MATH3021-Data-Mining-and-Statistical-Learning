library(MASS)
Boston
?Boston
nrow(Boston)
ncol(Boston)
str(Boston)
par(mfrow= c(2,2))
plot(Boston$dis,Boston$nox)
plot(Boston$age,Boston$nox)
plot(Boston$rm,Boston$lstat)
plot(Boston$lstat,Boston$rm)


par(mfrow=c(2,2))
plot(Boston$crim,Boston$zn)
plot(Boston$crim,Boston$indus)
plot(Boston$crim,Boston$chas)
plot(Boston$crim,Boston$nox)
par(mfrow=c(2,2))
plot(Boston$crim,Boston$rm)
plot(Boston$crim,Boston$age)
plot(Boston$crim,Boston$dis)
plot(Boston$crim,Boston$rad)
par(mfrow=c(2,2))
plot(Boston$crim,Boston$tax)
plot(Boston$crim,Boston$ptratio)
plot(Boston$crim,Boston$black)
plot(Boston$crim,Boston$lstat)



library(ggplot2)
par(mfrow=c(1,3))
plot(Boston$crim,Boston$dis,type = 'p', pch=16)
range(Boston$crim)
plot(Boston$tax,Boston$dis,type = 'p', pch=16)
range(Boston$tax)
plot(Boston$ptratio,Boston$dis,type = 'p', pch=16)
range(Boston$ptratio)


Boston$chas <- as.factor(as.numeric(Boston$chas))
summary(Boston$chas)
?wilcox.test

x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
wilcox.test(x, y, paired = TRUE, alternative = "greater")
wilcox.test(y - x, alternative = "less")
wilcox.test(y - x, alternative = "less",
            exact = FALSE, correct = FALSE)