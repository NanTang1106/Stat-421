## Lec 6-1
## a
type_one <- c(65, 81, 57, 66, 82, 82, 67, 59, 75, 70)
type_two <- c(64, 71, 83, 59, 65, 56, 69, 74, 82, 79)
n1 <- length(type_one)
n2 <- length(type_two)
s1 <- sd(type_one)
s2 <- sd(type_two)

mean(type_one) - mean(type_two)
sp <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1 + n2 - 2))
se <- sp * sqrt(1/n1 + 1/n2)
t_obs <- (mean(type_one) - mean(type_two)) / se

alpha <- 0.05
low_cv <- qt(alpha/2, df = (n1 + n2 - 2))
up_cv <- qt(1 - alpha/2, df = (n1 + n2 - 2))

CI_low <- (mean(type_one) - mean(type_two)) - qt(1 - alpha/2, df = (n1 + n2 - 2)) * se
CI_up <- (mean(type_one) - mean(type_two)) + qt(1 - alpha/2, df = (n1 + n2 - 2)) * se

p_val <- (1 - pt(t_obs, df=n1+n2-2)) * 2

## b
F_obs <- s1^2 / s2^2
p_val <- pf(F_obs, df1=n1-1, df2=n2-1) +
  (1 - pf(1 + abs(F_obs - 1), df1=n1-1, df2=n2-1))

CI_low <- s1^2 / s2^2 * 1 / qf(1 - alpha/2, df1=n1-1, df2=n2-1)
CI_up <- s1^2 / s2^2 * 1 / qf(alpha/2, df1=n1-1, df2=n2-1)

low_cv <- qf(alpha/2, df1=n1-1, df2=n2-1)
up_cv <- qf(1 - alpha/2, df1=n1-1, df2=n2-1)

## Lect 7-1
y = matrix(nrow=4,ncol=10)
y[1,] = c( -2.10552316, 1.89491371, -1.52919682, -0.99265143, -0.45911960, 1.09271028, -1.54680778, 0.13890677,
           0.06240357,
           -0.09273045)
y[2,] = c( 4.25667943, 4.36518096, 4.42108835, 3.77229146, 2.22264903, 3.95354759, 6.29377745, 3.58501081,
           3.12457306, 3.04360597)
y[3,] = c( 3.64209745, 2.76932242, 1.46001019, 0.23739519, 0.27629510, 2.83897173, 2.99999590, 3.54657820,
           2.03955378, 1.28515784)
y[4,] = c( 3.18088593, 5.44976665, 5.87116946, 4.01275036, 6.00826692, 5.19220036, 6.17338313, 4.88846073,
           4.49330445, 4.83224707)

## a
tr_level <- as.factor(rep(c(1,2,3,4), each=10))
pool_dt <- data.frame(tr_level, as.vector(t(y)))
plot(pool_dt, xlab='Treatment Levels', ylab='Y values')
  
## b
grand_mean <- mean(pool_dt[,2])
new_dt <- data.frame(tr_level, pool_dt[,2] - grand_mean)
plot(new_dt, xlab='Treatment Levels', ylab='Yij - Grand Mean')

## c
y_bar <- apply(y, 1, mean)
y_effect <- y_bar - grand_mean

## d
y_sd <- apply(y, 1, sd)
y_se <- y_sd / sqrt(10)

## e
one_sd_bd <- c(y_effect + y_se, y_effect - y_se)
tr_level <- as.factor(rep(c(1,2,3,4), 2))
effect_est <- data.frame(tr_level, one_sd_bd)
plot(effect_est, xlab='Treatment Levels', ylab='Effects')
abline(h=0, lty=2)

## f
SStr <- 10 * sum((y_bar - grand_mean)^2)
SSe <- (10 - 1) * sum(y_sd^2)
F_obs <- (SStr / (4-1)) / (SSe / (40 - 4))

qf(0.95, df1=3, df2=36, lower.tail = T)

## Lect 7-4
## a
y_m <- matrix(nrow=3, ncol=20)
y_m[1,] <- c(5.32, 6, 5.12, 7.08, 5.48, 6.52, 4.09, 6.28, 7.77, 
           5.68, 8.47, 4.58, 4.11, 5.72, 5.91, 6.89, 6.99, 4.98, 9.94, 6.38)
y_m[2,] <- c(4.73, 5.81, 5.69, 3.86, 4.06, 6.56, 8.34, 3.01, 6.71, 
           6.51, 1.70, 5.89, 6.55, 5.34, 5.88, 7.50, 3.28, 5.38, 7.30, 5.46)
y_m[3,] <- c(7.03, 4.65, 6.65, 5.49, 6.98, 4.85, 7.26, 5.92, 5.58,
            7.91, 4.90, 4.54, 8.18, 5.42, 6.03, 7.04, 5.17, 7.60, 7.90, 7.91)
A <- as.factor(rep(c(1, 2, 4), each=20))
y <- c(y_m[1,] , y_m[2,], y_m[3,]  )
lm_1 <- lm(y~A)
summary.aov(lm_1)

## b
y_bars <- apply(y_m, 1, mean)
plot(x=rep(y_bars, each=20), y=lm_1$residuals, xlab='Y Bar', ylab='Residuals', pch=1)
abline(h=0, lty=2)

## c
dt_1 <- data.frame(A, lm_1$residuals)
qqnorm(dt_1[dt_1$A==1,]$lm_1.residuals, ylim=c(-4, 4))
qqnorm(dt_1[dt_1$A==2,]$lm_1.residuals, ylim=c(-4, 4))
qqnorm(dt_1[dt_1$A==4,]$lm_1.residuals, ylim=c(-4, 4))

## d
alpha <- 0.05
n <- 20
y1_mean <- mean(y_m[1,])
y_sds <- apply(y_m, 1, sd)
MSE <- sum(y_sds^2) / 3
cv_low <- y1_mean + sqrt(MSE / n) * qt(alpha/2, df=(60-3))
cv_high <- y1_mean - sqrt(MSE / n) * qt(alpha/2, df=(60-3))

## e
y2_sd <- sd(y_m[2,])
y3_sd <- sd(y_m[3,])
F_obs <- y2_sd^2 / y3_sd^2
p_val <- pf(1 - (F_obs-1), df1=n-1, df2=n-1) +
  (1 - pf(F_obs, df1=n-1, df2=n-1))

y2_mean <- mean(y_m[2,])
y3_mean <- mean(y_m[3,])
cv_low <- y2_mean - y3_mean + sqrt(2 * MSE / n) * qt(alpha/2, df=(60-3))
cv_high <- y2_mean - y3_mean - sqrt(2 * MSE / n) * qt(alpha/2, df=(60-3))

## f
c <- c(1, -1/2, -1/2)
gamma_est <- sum(y_bars * c)
MSE <- sum(y_sds^2) / 3
t <- gamma_est / (sqrt(MSE * sum(c^2) / n))
low_cv <- qt(alpha/2, df=n*3 - 3) 
up_cv <- qt(alpha/2, df=n*3 - 3) 

## g
ci_low <- gamma_est + (sqrt(MSE * sum(c^2) / n)) * qt(alpha/2, df=n*3 - 3)
ci_up <- gamma_est - (sqrt(MSE * sum(c^2) / n)) * qt(alpha/2, df=n*3 - 3)


## lect 8-1
## a
a <- 3
n <- 5
ct_m <- matrix(nrow=a, ncol=n)
ct_m[1,] <- c(9 ,12, 10, 8, 15)
ct_m[2,] <- c(20, 21, 23, 17, 30)
ct_m[3,] <- c(6, 5, 8, 16, 7)
y_means <- apply(ct_m, 1, mean)
grand_mean <- mean(ct_m)
y_sds <- apply(ct_m, 1, sd)
SStr <- n * sum((y_means - grand_mean)^2)
SSe <- (n-1) * sum(y_sds^2)
F_obs <- (SStr / (a-1)) / (SSe / (n*a - a))
p_val <- pf(F_obs, df1=a-1, df2=a*n - a, lower.tail = F)

## b
A <- as.factor(rep(c(1,2,3), each=n))
ct <- as.vector(t(ct_m))
lm_2 <- lm(ct~A)
aov(ct~A)
summary.aov(lm_2)

## c
y1_sd <- sd(ct_m[1,])
y3_sd <- sd(ct_m[3,])
F_obs <- y1_sd^2 / y3_sd^2
p_val <- (1 - pf(1 + (1 - F_obs), df1=n-1, df2=n-1)) +
  pf(F_obs, df1=n-1, df2=n-1)

MSE <- sum(y_sds^2) / a
y1_bar <- mean(ct_m[1,])
y3_bar <- mean(ct_m[3,])
ci_low <- (y1_bar - y3_bar) + qt(alpha/2, df=a*(n-1)) * sqrt(2 * MSE / n)
ci_up <- (y1_bar - y3_bar) - qt(alpha/2, df=a*(n-1)) * sqrt(2 * MSE / n)

## d
t_obs <- (y1_bar - y3_bar) / sqrt(2 * MSE / n)
p_val <- 2 * (1 - pt(t_obs, df=n*a - a))

## e
c <- c(1, 0, -1)
d <- c(1, -2, 1)
c_ssc <- (sum(c * y_means))^2 / (sum(c^2) / n)
d_ssc <- (sum(d * y_means))^2 / (sum(d^2) / n)
c(c_ssc, d_ssc)
sum(c(c_ssc + d_ssc))

## f
c <- c(1, 0, -1)
MSE <- sum(y_sds^2) / a
c_gamma <- sum(c * y_means)
t_obs <- c_gamma / sqrt(MSE * sum(c^2) / n)
p_val <- 2 * (1 - pt(t_obs, df=n * (a-1)))

d <- c(1, -2, 1)
d_gamma <- sum(d * y_means)
t_obs <- d_gamma / sqrt(MSE * sum(d^2) / n)
p_val <- 2 * pt(t_obs, df=n * (a-1))

## lect 8-3
## a
a = 4
n = 4
y_m <- matrix(nrow=a, ncol=n)
y_m[1,] = c(143, 141, 150, 146)
y_m[2,] = c(152, 149, 137, 143)
y_m[3,] = c(134, 136, 132, 127)
y_m[4,] = c(129, 127, 132, 129)

c <- c(1, 1, -1, -1)
d <- c(1, -1, 0, 0)
e <- c(0, 0, 1, -1)

y_bars <- apply(y_m, 1, mean)
c_ssc <- (sum(c * y_bars))^2 / (sum(c^2) / n)
d_ssc <- (sum(d * y_bars))^2 / (sum(d^2) / n)
e_ssc <- (sum(e * y_bars))^2 / (sum(e^2) / n)
sum(c(c_ssc, d_ssc, e_ssc))

## b
f <- c(1, -1, 1, -1)
g <- c(0, 1, 0, -1) 
h <- c(1, 0, -1, 0)
f_ssc <- (sum(f * y_bars))^2 / (sum(f^2) / n)
g_ssc <- (sum(g * y_bars))^2 / (sum(g^2) / n)
h_ssc <- (sum(h * y_bars))^2 / (sum(h^2) / n)
c(f_ssc, g_ssc, h_ssc)
sum(c(f_ssc, g_ssc, h_ssc))

## c
u <- c(-1, 0, 1, 0)
v <- c(-1, 2, -1, 0)
w <- c(-1, -1, -1, 3)
u_ssc <- (sum(u * y_bars))^2 / (sum(u^2) / n)
v_ssc <- (sum(v * y_bars))^2 / (sum(v^2) / n)
w_ssc <- (sum(w * y_bars))^2 / (sum(w^2) / n)
sum(c(u_ssc, v_ssc, w_ssc))

