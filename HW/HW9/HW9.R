## lect 25-2
## a
library(AlgDesign)
design1 <- gen.factorial(c(3,2,10), varNames = c('Rep', 'Ope', 'Part'), factors = 'all')
attach(design1)
y <- c(50, 49, 50, 50, 48, 51, 52, 52, 51, 51, 51, 51, 53, 50, 50, 54, 52, 51,
         49, 51, 50, 48, 50, 51, 48, 49, 48, 48, 49, 48, 52, 50, 50, 52, 50, 50, 
         51, 51, 51, 51, 50, 50, 52, 50, 49, 53, 48, 50, 50, 51, 50, 51, 48, 49,
         47, 46, 49, 46, 47, 48)
lm1 <- lm(y~Ope*Part)
summary.aov(lm1)
MSA <- summary.aov(lm1)[[1]][1,3]
MSB <- summary.aov(lm1)[[1]][2,3]
MSAB <- summary.aov(lm1)[[1]][3,3]
MSE <- summary.aov(lm1)[[1]][4,3]

## b
a <- 2
b <- 10
n <- 3
F_A <- MSA/MSAB
F_B <- MSB/MSAB
F_AB <- MSAB/MSE
p_A <- pf(F_A, df1=a-1, df2=(a-1)*(b-1), lower.tail = F)
p_B <- pf(F_B, df1=b-1, df2=(a-1)*(b-1), lower.tail = F)
p_AB <- pf(F_AB, df1=(a-1)*(b-1), df2=a*b*n-a*b, lower.tail = F)

c(F_A, F_B, F_AB)
c(p_A, p_B, p_AB)

## c
est_sigmaA <- 1/(n*b) * (MSA - MSAB)
est_sigmaB <- 1/(n*a) * (MSB - MSAB)
est_sigmaAB <- 1/n * (MSAB - MSE)
est_sigmaE <- MSE

## d
sigmaY <- var(y)
sum(est_sigmaA, est_sigmaB, est_sigmaAB, est_sigmaE)




