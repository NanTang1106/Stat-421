# lec 2-2
m1 <- c(0.73, 0.62, 0.62, 0.82, 0.68)
m2 <- c(0.75, 0.55, 0.64, 0.00, 0.30)
m3 <- c(0.65, 0.46, 0.52, 0.48, 0.64)
m4 <- c(0.71, 0.49, 0.56, 0.66, 0.99)
m5 <- c(0.61, 0.28, 0.35, 0.62, 0.52)
m6 <- c(0.75, 0.34, 0.89, 0.66, 0.80)
m7 <- c(0.08, 0.27, 0.28, 0.49, 0.81)
m8 <- c(0.87, 0.97, 0.78, 0.98, 0.75)

all_m <- list(m1,m2,m3,m4,m5,m6,m7,m8)

boxplot(all_m, range=0, xlab="Model Type", ylab="Accuracy")

# lec 2-3
trial <- 1000
x <- c(1, 2, 3)
x_prob <- c(1/2, 1/4, 1/4)
geo_mean <- numeric(1000)
for (i in 1:trial) {
  samp <- sample(x, size=2, prob = x_prob, replace=T)
  geo_mean[i] = sqrt(samp[1] * samp[2])
}
geo_mean_prob <- as.vector(table(geo_mean)) / trial

#hist(geo_mean, probability = F, main="Distribution of Sample Geometric Mean",
#    ylab="Geometric Mean", breaks = 15)

x_val <- c(1, sqrt(2), sqrt(3), 2, sqrt(6), 3)

plot(x_val, geo_mean_prob, type='h', lwd=5, xlab='Geometric Means', ylab='Density',
     main='Sampling Distribution')

y_val <- c(1/4, 1/4, 1/4, 1/16, 1/8, 1/16)

points(x_val, y_val, cex=2, pch=20, col=2)




