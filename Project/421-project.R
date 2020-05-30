library(AlgDesign)
rm(list=ls(all=T))
design1 <- gen.factorial(c(2,2,2,2,2), varNames = c("A","B","C","D","E"))
attach(design1)
y <- c(74.6, 94.4, 85, 93.1, 83.3, 85, 76.7, 79.9, 70.1, 86.2, 77.5, 85.1, 69.5, 74.5, 76.1, 79.6,
       59.9, 71.9, 62.4, 59, 60.4, 66.9, 54.8, 59.9, 58.9, 63.7, 62.5, 71.4, 69.8, 67.2, 57, 55.6)
Block <- B*D
lm1 <- lm(y~A+B+C+D+E+A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+C:E+D:E + Block)
summary.aov(lm1)


eff1 <- lm1$coefficients[-1]
eff1 <- na.omit(eff1)
qqnorm(eff1, ylim=c(-10, 10))
abline(median(eff1), 1, col=2)

lm2 <- lm(y~A+B+C+D+E+A:C+B:C+B:E+D:E + Block)
summary.aov(lm2)

##cbind(A,B,C,D,E,BL)
##data <- rnorm(32)
##temp <- as.matrix(summary.aov(lm(data~A*B*C*D*E))[[1]])
##cbind(temp,BL[-1])

par(mfrow=c(2,2))

boxplot(y[A==1 & C==1 ],y[A==-1 & C==1],ylim=c(50,100) , main="Interaction AC")
boxplot(y[A==1 & C==-1 ],y[A==-1 & C==-1], add=2,col=2, boxwex = 0.5)

boxplot(y[B==1 & C==1 ],y[B==-1 & C==1],ylim=c(50,100) , main="Interaction BC")
boxplot(y[B==1 & C==-1 ],y[B==-1 & C==-1], add=2,col=2, boxwex = 0.5)

boxplot(y[B==1 & E==1 ],y[B==-1 & E==1],ylim=c(50,100) , main="Interaction BE")
boxplot(y[B==1 & E==-1 ],y[B==-1 & E==-1], add=2,col=2, boxwex = 0.5)

boxplot(y[D==1 & E==1 ],y[D==-1 & E==1],ylim=c(50,100) , main="Interaction DE")
boxplot(y[D==1 & E==-1 ],y[D==-1 & E==-1], add=2,col=2, boxwex = 0.5)

