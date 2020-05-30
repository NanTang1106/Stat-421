## lect 10-4
## a
b = 5
a = 4
data = matrix(0, ncol = b, nrow = a)
data[1,] = c(73, 68, 74, 71, 67)
data[2,] = c(73, 67, 75, 72, 70)
data[3,] = c(75, 68, 78, 73, 68)
data[4,] = c(73, 71, 75, 75, 69)

y <- as.vector(t(data))
A <- as.factor(rep(c(1:a), each=b))
B <- as.factor(rep(c(1:b), times=a))

SSE <- summary.aov(lm(y~A+B))[[1]]$`Sum Sq`[3]
F_block <- summary.aov(lm(y~A+B))[[1]]$`F value`[2]
F_treat <- summary.aov(lm(y~A+B))[[1]]$`F value`[1]

## b
lm1 <- summary.aov(lm(y~B))[[1]]
SSE1 <- lm1$`Sum Sq`[2]

## c 
lm2 <- summary.aov(lm(y~A))[[1]]
SSE2 <- lm2$`Sum Sq`[2]

## d
((SSE1 - SSE)/(a-1)) / (SSE/((a-1)*(b-1)))

((SSE2 - SSE)/(b-1)) / (SSE/((a-1)*(b-1)))

## lect 11-1
## a
b = 5
a = 4
data = matrix(0, ncol = b, nrow = a)
data[1,] = c(73, 68, 74, 71, 67)
data[2,] = c(73, 67, 75, 72, 70)
data[3,] = c(75, 68, 78, 73, 68)
data[4,] = c(73, 71, 75, 75, 69)


## B <- as.factor(rep(c(1:b), times=a))

y <- as.vector(t(data))
grand_mean <- mean(y)
SSTr <- sum((apply(data, 1, mean) - grand_mean)^2) * b
SSE <- sum(apply(data, 1, var)) * (b-1)
F_ratio <- (SSTr / (a-1)) / (SSE / (a*b - a))
p_val <- pf(F_ratio, df1 = a-1, df2=a*b - a, lower.tail = F)

## b
A <- as.factor(rep(c(1:a), each=b))
B <- as.factor(rep(c(1:b), times=a))

yi. <- apply(data, 1, mean)
y.j <- apply(data, 2, mean)
SStr <- b * sum((yi. - grand_mean)^2)
SSbl <- a * sum((y.j - grand_mean)^2)
SST <- sum((y - grand_mean)^2)
SSE <- SST - SStr - SSbl
F_ratio <- (SStr / (a-1)) / (SSE / ((a-1)*(b-1)))
p_val <- pf(F_ratio, df1=a-1, df2=(a-1) * (b-1), lower.tail = F)

## c
y_hat <- rep(yi., each=b)
resid <- y - y_hat
par(mfrow=c(1,2))
plot(x=y_hat, y=resid, xlab='Y-hat', ylab='Residuals')
abline(h=0, lty=2)
plot(x=y, y=resid, xlab='Y', ylab='Residuals')

## d
y_hat <- rep(yi., each=b) + rep(y.j, times=a) - grand_mean
resid <- y - y_hat
par(mfrow=c(1,2))
plot(x=y_hat, y=resid, xlab='Y-hat', ylab='Residuals')
abline(h=0, lty=2)
plot(x=y, y=resid, xlab='Y', ylab='Residuals')

## e
y_hat1 <- rep(yi., each=b)
resid1 <- y - y_hat1
y_hat2 <- rep(yi., each=b) + rep(y.j, times=a) - grand_mean
resid2 <- y - y_hat2
par(mfrow=c(1,2))
qqnorm(resid1, main='CRD Residuals QQplot')
qqnorm(resid2, main='RCBD Residuals QQplot')




