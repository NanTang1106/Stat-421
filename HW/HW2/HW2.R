## lec 3-1
## a
set.seed(123)
nsample <- 100
x_obs <- rnorm(nsample,0,1)
y_obs <- rnorm(nsample,0,1)
delta_obs <- mean(x_obs) - mean(y_obs)

sigma_theo <- sqrt(1/nsample + 1/nsample)

1 - pnorm(delta_obs, 0, sigma_theo)

## b
pool <- c(x_obs, y_obs)
ntrial <- 1000
rand_delta <- numeric(ntrial)

for (i in 1:ntrial) {
 z = c(1:length(pool))
 samp_index <- sample(z, nsample, replace=F)
 samp_x <- pool[samp_index]
 samp_y <- pool[-samp_index]
 rand_delta[i] <- mean(samp_x) - mean(samp_y)
}

## c
hist(rand_delta, breaks = 20, xlab='Difference in Sample Means', 
    main = 'Randomization Sampling Distribution')
abline(v = delta_obs, col=2, lwd=2, lty=2)

rand_p <- length(rand_delta[rand_delta >= delta_obs]) / ntrial

## lec 4-3
sigma2 <- 1
s2 <- 1.459
n = 31
test_stat <- (n-1)*s2 / sigma2
1 - pchisq(test_stat, df=n-1)

## lec 5-1
n1 <- 21
n2 <- 25
1 - pf(2.33, df1=n1-1, df2=n2-1)








 