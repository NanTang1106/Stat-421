library(AlgDesign)

## lect 18-3
## a
rm(list=ls(all=T))
design1 <- gen.factorial(c(2,2,2), varNames = c('A','B','C'))
attach(design1)
A <- as.factor(A)
B <- as.factor(B)
C <- as.factor(C)
y <- c(22, 32, 35, 55, 44, 40, 60, 39)

contr <- as.character("contr.helmert")
lm1 <- lm(y~A*B*C, contrasts = list(A=contr,B=contr,C=contr))
summary.aov(lm1)
eff1 <- 2 * (lm1$coefficients)[-1]
qqnorm(eff1)
abline(median(eff1), 7)

## b
lm2 <- lm(y~A+B+C+A:C)
summary.aov(lm2)

## c
rm(list=ls(all=T))
y <- c(22, 32, 35, 55, 44, 40, 60, 39)
design3 <- gen.factorial(c(2,2,2), varNames = c('A','B','C'))
attach(design3)
ABC <- A*B*C
A <- as.factor(A)
B <- as.factor(B)
C <- as.factor(C)
Block <- as.factor(ABC)

contr <- as.character("contr.helmert")
lm3 <- lm(y~A*B*C + Block, contrasts = list(A=contr,B=contr,C=contr, Block=contr))
summary.aov(lm3)

## d
contr <- as.character("contr.helmert")
lm3 <- lm(y~A*B*C + Block, contrasts = list(A=contr,B=contr,C=contr, Block=contr))
eff3 <- 2 * (lm3$coefficients)[-1]
qqnorm(eff3)

lm4 <- lm(y~A+B+C+A:C+Block)
summary.aov(lm4)

## e
rm(list=ls(all=T))
y <- c(22, 32, 35, 55, 44, 40, 60, 39)
design4 <- gen.factorial(c(2,2,2), varNames = c('A','B','C'))
attach(design4)
AC <- A*C
BC <- B*C
Block <- numeric(8)
for (i in 1:8) {
  if (AC[i] == -1 & BC[i] == -1) {
    Block[i] = 1
  } else if (AC[i] == 1 & BC[i] == -1) {
    Block[i] = 2
  } else if (AC[i] == -1 & BC[i] == 1) {
    Block[i] = 3
  } else {
    Block[i] = 4
  }
}
A <- as.factor(A)
B <- as.factor(B)
C <- as.factor(C)
Block <- as.factor(Block)

lm4 <- lm(y~A*B*C + Block)
summary.aov(lm4)

## lect 19-1
## a
rm(list=ls(all=TRUE))
design = gen.factorial(2,4,varNames=c("A","B","C","D"), factors="all")
attach(design)
y = c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96) 
BL = as.factor((c(A) + c(B) + c(C) + c(D)) %% 2 )
A <- as.factor(A)
B <- as.factor(B)
C <- as.factor(C)
D <- as.factor(D)
contr <- as.character("contr.helmert")
lm1 <- lm(y~A*B*C*D*BL, contrasts = list(A=contr,B=contr,C=contr, D=contr, BL=contr))
summary.aov(lm1)
eff <- 2*lm1$coef
eff <- eff[2:16]
ss = summary.aov(lm1) [[1]][,2]

## lect 19-2 2^5
## a
rm(list=ls(all=TRUE))
design = gen.factorial(2,5,varNames=c("A","B","C","D",'E'), factors="all")
attach(design)
y <- c(7,9,34,55,16,20,40,60,8,10,32,50,18,21,44,61,
       8,12,35,52,15,22,45,65,6,10,30,53,15,20,41,63)
A <- as.factor(A)
B <- as.factor(B)
C <- as.factor(C)
D <- as.factor(D)
E <- as.factor(E)
contr <- as.character("contr.helmert")
lm1 <- lm(y~A*B*C*D*E, contrasts = list(A=contr,B=contr,C=contr, D=contr, E=contr))
summary.aov(lm1)

## b
rm(list=ls(all=TRUE))
design = gen.factorial(2,5,varNames=c("A","B","C","D",'E'))
attach(design)
y <- c(7,9,34,55,16,20,40,60,8,10,32,50,18,21,44,61,
       8,12,35,52,15,22,45,65,6,10,30,53,15,20,41,63)
ABC = A*B*C 
CDE = C*D*E
BL = numeric(16)
BL[ABC==-1 & CDE==-1] = 1
BL[ABC==+1 & CDE==-1] = 2
BL[ABC==-1 & CDE==+1] = 3
BL[ABC==+1 & CDE==+1] = 4
A <- as.factor(A)
B <- as.factor(B)
C <- as.factor(C)
D <- as.factor(D)
E <- as.factor(E)
BL <- as.factor(BL)
lm2 <- lm(y~A*B*C*D*E+BL)
summary.aov(lm2)

## lect 19-5
## a
rm(list=ls(all=TRUE))
design = gen.factorial(2,4,varNames=c("A","B","C","D"))
attach(design)
y = c(23,15, 16, 18, 25, 16, 17, 26, 28, 16, 18, 21, 36, 24, 33, 34)
BL <- A*B*C
lm1 <- lm(y~A*B*C*D + BL)
summary.aov(lm1)

## b
rm(list=ls(all=TRUE))
design = gen.factorial(2,4,varNames=c("A","B","C","D"), factors = 'all')
attach(design)
y = c(23,15, 16, 18, 25, 16, 17, 26, 28, 16, 18, 21, 36, 24, 33, 34)
BL = as.factor((c(A) + c(B) + c(C)) %% 2 )
lm1 <- lm(y~A*B*C*D + BL)
summary.aov(lm1)

## c
rm(list=ls(all=TRUE))
design = gen.factorial(2,4,varNames=c("A","B","C","D"), factors = 'all')
attach(design)
y = c(23,15, 16, 18, 25, 16, 17, 26, 28, 16, 18, 21, 36, 24, 33, 34)

BL1 <- as.factor((c(A)+c(B))%%2)
BL2 <- as.factor((c(C)+c(D))%%2)
BL <- numeric(length(y))
BL[BL1==0 & BL2==0] <- 1
BL[BL1==1 & BL2==0] <- 2
BL[BL1==0 & BL2==1] <- 3
BL[BL1==1 & BL2==1] <- 4
BL <- as.factor(BL)
lm2 <- lm(y~A*B*C*D + BL)
summary.aov(lm2)

## lect 20-2
rm(list=ls(all=TRUE))
design <- gen.factorial(2,3,varNames = c('A','B','C'))
attach(design)
y = c(43, 71, 48, 104, 68, 86, 70, 65)
D <- -A*B*C
A <- as.factor(A)
B <- as.factor(B)
C <- as.factor(C)
D <- as.factor(D)
contr = as.character("contr.helmert")
lm1 = lm(y~A*B*C*D, contrasts = list(A=contr,B=contr,C=contr,D=contr))
eff = 2 * lm1$coefficients
eff = eff[2:8]

## lect 20-3
## c
rm(list=ls(all=TRUE))
design <- gen.factorial(2,4,varNames = c('A','B','C','E'))
design1 <- rbind(design, design, design)
attach(design1)
rep1 <- c(7.78,8.15,7.50,7.59,7.54,7.69,7.56,7.56,7.50,7.88,7.50,7.63,7.32,7.56,7.18,7.81)
rep2 <- c(7.78,8.18,7.56,7.56,8.00,8.09,7.52,7.81,7.25,7.88,7.56,7.75,7.44,7.69,7.18,7.50)
rep3 <- c(7.81,7.88,7.50,7.75,7.88,8.06,7.44,7.69,7.12,7.44,7.50,7.56,7.44,7.62,7.25,7.59)
y <- c(rep1, rep2, rep3)
D <- A*B*C
A <- as.factor(A)
B <- as.factor(B)
C <- as.factor(C)
D <- as.factor(D)
E <- as.factor(E)
contr = as.character("contr.helmert")
lm1 = lm(y~A*B*C*D*E, contrasts = list(A=contr,B=contr,C=contr,D=contr,E=contr))
summary.aov(lm1)

## d
eff <- as.matrix(2 * lm1$coefficients[-1])
eff <- as.matrix(na.omit(eff))
qqnorm(eff[,1])
abline(median(eff[,1]), 0.08, col=2)
