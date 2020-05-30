## lect 15-4
## a
library(AlgDesign)
rm(list=ls(all=TRUE))
design <- gen.factorial(c(2,2,4), varNames = c('A', 'B', 'Replication'), factors = 'all')
attach(design)
y <- c(18.2,27.2,15.9,41,18.9,24,14.5,43.9,12.9,22.4,15.1,36.3,14.4,22.5,14.2,39.9)

lm1 <- lm(y~A + B + A:B)
summary.aov(lm1)

## b
n <- 4
data1 <- data.frame(cbind(y, A, B, Replication))
yate_1 <- sum(data1[A==1 & B==1,1])
yate_a <- sum(data1[A==2 & B==1,1])
yate_b <- sum(data1[A==1 & B==2,1])
yate_ab <- sum(data1[A==2 & B==2,1])

SSA <- (-yate_1 + yate_a - yate_b + yate_ab)^2 / (4 * n)
SSB <- (-yate_1 - yate_a + yate_b + yate_ab)^2 / (4 * n)
SSAB <- (yate_1 - yate_a - yate_b + yate_ab)^2 / (4 * n)
SST <- sum((y - mean(y))^2)
SSE <- SST - SSA - SSB - SSAB
c(SSA, SSB, SSAB, SSE)

## c
par(mfrow=c(1,2))
ylimit <- c(min(y), max(y))
boxplot(data1[A==1,1], data1[A==2,1], ylim=ylimit, xlab='A')
boxplot(data1[B==1,1], data1[B==2,1], ylim=ylimit, xlab='B')

par(mfrow=c(1,1))
boxplot(data1[A==1 & B==1,1],data1[A==2 & B==1,1], ylim=ylimit, 
        xlab='A-/A+, left/right', main='B-/B+, white/red')
boxplot(data1[A==1 & B==2,1],data1[A==2 & B==2,1],add=T,col='red',boxwex=0.5)


## lect-16-4
## a
rm(list=ls(all=TRUE))
design2 <- gen.factorial(c(2,2,2,2,2), varNames = c('A','B', 'C', 'D','Rep'))
attach(design2)
y2 <- c(7.037, 14.707, 11.635, 17.273, 10.403, 4.368, 9.360, 13.440, 8.561, 16.867, 13.876, 
        19.824, 11.846, 6.125, 11.190, 15.653, 6.376, 15.219, 12.089, 17.815, 10.151, 4.098, 
        9.253, 12.923, 8.951, 17.052, 13.658, 19.639, 12.337, 5.904, 10.935, 15.053)
data2 <- data.frame(y2,A,B,C,D)
lm2 = lm(y2~A*B*C*D)
anovatable <- summary.aov(lm2)[[1]]
MSE <- anovatable$`Mean Sq`[16]
summary.aov(lm2)

## b
contr <- as.character("contr.helmert")
lm3 <- lm(y2~A*B*C*D, contrasts = list(A=contr,B=contr,C=contr,D=contr))
summary.lm(lm3) 
all_effects <- 2 * (lm3$coefficients)[-1]

## c
contrasts_15 <- apply(y2*cbind(A, B, C, D, A*B, A*C, A*D,
                            B*C,B*D, C*D, A*B*C, A*B*D, A*C*D, B*C*D, A*B*C*D), 2, sum)
contrasts_15

## d
k <- 4
n <- 2
p <- 15 + 1
effects_15 <- 1/(2^(k-1)*n)*contrasts_15
t_obs_15 <- effects_15 / sqrt(MSE/(2^(k-2)*n))
p_values_15 <- 2*pt(abs(t_obs_15), df=2^k*n - p, lower.tail = F)
t_tests_byhand <- cbind(effects_15, t_obs_15, p_values_15)
colnames(t_tests_byhand) <- c('effects', 't-stats', 'p-values')
rownames(t_tests_byhand) <- c('A','B','C','D','AB','AC','AD','BC','
                              BD','CD','ABC','ABD','ACD','BCD','ABCD')
t_tests_byhand

## e
contr <- as.character("contr.helmert")
lm3 <- lm(y2~A*B*C*D, contrasts = list(A=contr,B=contr,C=contr,D=contr))
summary.lm(lm3) 
effs <- 2 * (lm3$coefficients)[-1]
as.matrix(effs,col=1)

qqnorm(effs)
abline(median(effs), MSE, lty=2, col=2)


## lect 17-2
## a
rm(list=ls(all=T))
design4 <- gen.factorial(c(2,2,3), varNames = c('A','B','RR'))
attach(design4)
rep1 <- c(28,36,18,31)
rep2 <- c(25,32,19,30)
rep3 <- c(27,32,23,29)
y4 <- c(rep1,rep2,rep3)
contr <- as.character("contr.helmert")
A <- as.factor(A)
B <- as.factor(B)
lm4 <- lm(y4~A*B, contrasts = list(A=contr, B=contr))

summary.aov(lm4)

effects4 <- 2 * lm4$coefficients[-1]

## b
RR <- RR + 2
BL <- as.factor(RR)
lm5 <- lm(y4~A + B + A*B + BL, contrasts = list(A=contr,B=contr, BL=contr))

summary.aov(lm5)

effects5 <- 2 * lm5$coefficients[-1]
effects5

## d
sse_obs <- summary.aov(lm5)[[1]][5,2]
y.m <- matrix(c(rep1,rep2,rep3), ncol=3, byrow=F)
ntrials <- 5000
set.seed(123)
sse_sample <- numeric(ntrials)
for (i in 1:ntrials) {
  new_y.m <- t(apply(t(y.m),2,sample))
  lm_temp <- lm(as.vector(new_y.m)~A+B+A*B+BL)
  sse_temp <- summary.aov(lm_temp)[[1]][5,2]
  sse_sample[i] <- sse_temp
}
hist(sse_sample, main='Empirical Distribution of SSE under H0', 
     xlab='Sample SSE', probability = T)

## e
sse_p_value <- length(sse_sample[sse_sample <= sse_obs]) / ntrials

## f
rm(list=ls(all=TRUE))
rep1 <- c(28,36,18,31)
rep2 <- c(25,32,19,30)
y6 <- c(rep1, rep2)
design6 <- gen.factorial(c(2,2,2), varNames = c('A','B','RR'))
attach(design6)
A <- as.factor(A)
B <- as.factor(B)
lm6 <- lm(y6~A*B)
summary.aov(lm6)

## g
BL <- numeric(length(y6))
BL[B==-1] <- 1
BL[B==1] <- 2
BL <- as.factor(BL)

lm7 <- lm(y6~A+B+A*B + BL)
summary.aov(lm7)


