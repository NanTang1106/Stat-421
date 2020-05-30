## lect 12-4
## a
p <- 5
y.m <- matrix(0, ncol = 5, nrow = 5)
y.m[1,] <- c(8, 7, 1, 7, 3)
y.m[2,] <- c(11, 2, 7, 3, 8)
y.m[3,] <- c(4, 9, 10, 1, 5)
y.m[4,] <- c(6, 8, 6, 6, 10)
y.m[5,] <- c(4, 2, 3, 8, 8)
treatment <- matrix('A', ncol = 5, nrow = 5)
treatment[1,] <- c('A', 'B', 'D', 'C', 'E')
treatment[2,] <- c('C', 'E', 'A', 'D', 'B')
treatment[3,] <- c('B', 'A', 'C', 'E', 'D')
treatment[4,] <- c('D', 'C', 'E', 'B', 'A')
treatment[5,] <- c('E', 'D', 'B', 'A', 'C') 

bar_yi.. <- apply(y.m, 1, mean)
bar_y..k <- apply(y.m, 2, mean)
bar_y.j. <- as.vector(c(mean(y.m[treatment=='A']), mean(y.m[treatment=='B']),
                        mean(y.m[treatment=='C']), mean(y.m[treatment=='D']), 
                        mean(y.m[treatment=='E']) ))
grand_mean <- mean(y.m)
SSA <- p * sum((bar_yi.. - grand_mean)^2)
SSB <- p * sum((bar_y.j. - grand_mean)^2)
SSC <- p * sum((bar_y..k - grand_mean)^2)
SST <- sum((as.vector(y.m) - grand_mean)^2)
SSE <- SST - SSA - SSB - SSC

c(SSA, SSB, SSC, SSE, SST)

F_ratio_A <- (SSA / (p-1)) / (SSE / ((p-1)* (p-2)))
p_value_A <- pf(F_ratio_A, df1=(p-1), df2=(p-1) * (p-2), lower.tail = F)
c(F_ratio_A, p_value_A)

F_ratio_B <- (SSB / (p-1)) / (SSE / ((p-1)* (p-2)))
p_value_B <- pf(F_ratio_B, df1=(p-1), df2=(p-1) * (p-2), lower.tail = F)
c(F_ratio_B, p_value_B)

F_ratio_C <- (SSC / (p-1)) / (SSE / ((p-1)* (p-2)))
p_value_C <- pf(F_ratio_C, df1=(p-1), df2=(p-1) * (p-2), lower.tail = F)
c(F_ratio_C, p_value_C)

## b
y <- as.vector(t(y.m))
A <- as.factor(rep(c(1:p), each=p))
C <- as.factor(rep(c(1:p), times=p))
B <- as.factor(as.vector(t(treatment)))
summary.aov(lm(y~A+B+C))

## lect 12-5
p <- 4
y.m2 <- matrix(0, ncol = 4, nrow = 4)
y.m2[1,] <- c(11, 10, 14, 8)
y.m2[2,] <- c(8, 12, 10, 12)
y.m2[3,] <- c(9, 11, 7, 15)
y.m2[4,] <- c(9, 8, 18, 6)

treatment1 <- matrix(0, ncol = 4, nrow = 4)
treatment1[1,] = c(3,2,4,1)
treatment1[2,] = c(2,3,1,4)
treatment1[3,] = c(1,4,2,3)
treatment1[4,] = c(4,1,3,2)

treatment2 = matrix(0, ncol = 4, nrow = 4)
treatment2[1,] = c(2,3,4,1)
treatment2[2,] = c(1,4,3,2)
treatment2[3,] = c(4,1,2,3)
treatment2[4,] = c(3,2,1,4)

y <- as.vector(t(y.m2))
A <- as.factor(rep(c(1:p), each=p))
B <- as.factor(rep(c(1:p), times=p))
t1 <- as.factor(as.vector(t(treatment1)))
t2 <- as.factor(as.vector(t(treatment2)))

summary.aov(lm(y~A+B+t1+t2))

## lect 14-3
y <- c(6,5,6,5,3,2,4,1,10,9,11,11,10,9,9,10)
FF <- as.factor(rep(c(1,2), each=8))
BB <- as.factor(rep(c(1,1,2,2), time=4))
WW <- as.factor(rep(c(rep(1,time=4), rep(2,time=4)), time=2))
## a
summary.aov(lm(y~FF + BB + WW + FF:BB + FF:WW + BB:WW + FF:BB:WW))
## b
predicts <- predict(lm(y~FF + BB + WW + FF:BB + FF:WW + BB:WW + FF:BB:WW))
residuals <- y - predicts

qqnorm(residuals, ylab='Residuals')

par(mfrow=c(2,2))
plot(x=predicts, y=residuals, xlab='Predicted value')
abline(h=0, lty=2)
plot(as.vector(x=FF), y=residuals, xlab='Factor one')
abline(h=0, lty=2)
plot(as.vector(x=BB), y=residuals, xlab='Factor two')
abline(h=0, lty=2)
plot(as.vector(x=WW), y=residuals, xlab='Factor three')
abline(h=0, lty=2)

## c
y2 <- y^1.7
predicts2 <- predict(lm(y2~FF + BB + WW + FF:BB + FF:WW + BB:WW + FF:BB:WW))
residuals2 <- y2 - predicts2

qqnorm(residuals2)

par(mfrow=c(2,2))
plot(x=predicts2, y=residuals2, xlab='Predicted value')
abline(h=0, lty=2)
plot(as.vector(x=FF), y=residuals2, xlab='Factor one')
abline(h=0, lty=2)
plot(as.vector(x=BB), y=residuals2, xlab='Factor two')
abline(h=0, lty=2)
plot(as.vector(x=WW), y=residuals2, xlab='Factor three')
abline(h=0, lty=2)

par(mfrow=c(1,1))


## lect 14-4
## a
rm(list=ls(all=TRUE))
y <- c(-35,-45,-40,17,-65,20,-39,-55,15,110,-10,80,55,-55,110,90,-28,110,
       4,-40,31,-23,-64,-20,-30,-61,54)
factors <- gen.factorial(c(3,3,3), varNames = c('Speed', 'Nozzle_Type', 'Pressure'), factors = 'all')
attach(factors)
lm2 <- lm(y~Nozzle_Type + Speed + Pressure + Nozzle_Type:Speed + Nozzle_Type:Pressure + Speed:Pressure)
summary.aov(lm2)

## b
data_full <- data.frame(y, Nozzle_Type, Speed, Pressure)
Nozzle_Type2 <- as.factor(rep(c(1,2,3), time=3))
Speed2 <- as.factor(rep(c(1,2,3), each=3))
Pressure2 <- as.factor(c(1,2,3,2,3,1,3,1,2))
y2 <- numeric(9)
for (i in 1:9) {
  index1 <- as.numeric(as.vector(Nozzle_Type2)[i])
  index2 <- as.numeric(as.vector(Speed2)[i])
  index3 <- as.numeric(as.vector(Pressure2)[i])
  y2[i] <- data_full[Nozzle_Type==index1 & Speed==index2 & Pressure==index3,1]
}

lm3 <- lm(y2~Nozzle_Type2 + Speed2 + Pressure2)
summary.aov(lm3)


## c
lm4 <- lm(y2~Nozzle_Type2 + Speed2 + Pressure2 + Speed2:Pressure2)
summary.aov(lm4)



