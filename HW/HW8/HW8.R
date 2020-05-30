library(AlgDesign)

## lect 21-4
## a
rm(list=ls(all=T))
y <- c(7,9,34,55,16,20,40,60,
       8,10,32,50,18,21,44,61,
       8,12,35,52,15,22,45,65,
       6,10,30,53,15,20,41,63)
design <- gen.factorial(c(2,2,2,2,2),varNames = c('A','B','C','D','E'))
attach(design)
ABD <- A*B*D
ACE <- A*C*E
y2 <- y[ABD == 1 & ACE == 1]
A <- as.factor(A[ABD == 1 & ACE == 1])
B <- as.factor(B[ABD == 1 & ACE == 1])
C <- as.factor(C[ABD == 1 & ACE == 1])
D <- as.factor(D[ABD == 1 & ACE == 1])
E <- as.factor(E[ABD == 1 & ACE == 1])
lm1 <- lm(y2~A*B*C*D*E)

## b
contr <- as.character("contr.helmert")
lm2 <- lm(y2~A*B*C*D*E, contrasts = list(A=contr,B=contr,C=contr,D=contr,E=contr)) 
eff <- 2 * lm2$coefficients[-1]
na.omit(eff)

## lect 22-2
## a
rm(list=ls(all=T))
rep1 <- c(90,74,81,83,77,81,88,73,98,72,87,85,99,79,87,80)
rep2 <- c(93,78,85,80,78,80,82,70,95,76,83,86,90,75,84,80)
y <- c(rep1, rep2)
design1 <- gen.factorial(c(2,2,2,2,2), varNames = c('A','B','C','D','Rep'))
attach(design1)
lm1 <- lm(y~A*B*C*D)
summary.aov(lm1)

## b
lm2 <- lm(y~A*B*C*D + Rep)
summary.aov(lm2)

## c
rm(list=ls(all=T))
rep1 <- c(90,74,81,83,77,81,88,73,98,72,87,85,99,79,87,80)
y <- rep1
design1 <- gen.factorial(c(2,2,2,2), varNames = c('A','B','C','D'))
attach(design1)
lm1 <- lm(y~A*B*C*D)
summary.aov(lm1)

## d
rm(list=ls(all=T))
rep1 <- c(90,74,81,83,77,81,88,73,98,72,87,85,99,79,87,80)
rep2 <- c(93,78,85,80,78,80,82,70,95,76,83,86,90,75,84,80)
y <- c(rep1, rep2)
design1 <- gen.factorial(c(2,2,2,2,2), varNames = c('A','B','C','D','Rep'))
attach(design1)
ABCD <- A*B*C*D
y <- y[ABCD==1]
A <- A[ABCD==1]
B <- B[ABCD==1]
C <- C[ABCD==1]
D <- D[ABCD==1]
Rep <- Rep[ABCD==1]
lm1 <- lm(y~A*B*C*D + Rep)
summary.aov(lm1)

## e
rm(list=ls(all=T))
rep1 <- c(90,74,81,83,77,81,88,73,98,72,87,85,99,79,87,80)
y <- rep1
design1 <- gen.factorial(c(2,2,2,2), varNames = c('A','B','C','D'))
attach(design1)
ABCD <- A*B*C*D
y <- y[ABCD==1]
A <- A[ABCD==1]
B <- B[ABCD==1]
C <- C[ABCD==1]
D <- D[ABCD==1]
lm1 <- lm(y~A*B*C*D)
summary.aov(lm1)

## f
BL <- A*C
lm2 <- lm(y~A*B*C*D + BL)
summary.aov(lm2)




