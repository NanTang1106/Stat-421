(57/276)^2 + 4.3*0.001 ## 0.04695123

x = 3
x ## 3
y = 1 + 1
y ## 2
log10(y) ## 0.30103
log2(y) ## 1
sqrt(y) ## 1.414214

is.vector(x) ## TRUE

x = 1:5 
x ## 1 2 3 4 5
y = seq(from=0, to=10, by=2)
y ## 0  2  4  6  8 10
y = c(34, 30, 41, 35, 21)
y ## 34 30 41 35 21

mean(y) ## 32.2
median(y) ## 34
range(y) ## 21 41
range(y)[1] ## 21
range(y)[2] ## 41
min(y) ## 21
max(y) ## 41
length(y) ## 5
sort(y) ## 21 30 34 35 41
sd(y) ## 7.395945

data(cars)
?cars
cars
names(cars) ## "speed" "dist"
dim(cars) ## 50 2
cars$speed
cars[, 1]
x = cars$speed
x
mean(x) ## 15.4
sd(x) ## 5.287644

dat = read.table("https://www.stat.washington.edu/marzban/421/autumn19/hist_dat.txt", header=F)
dat
x = dat[, 1]
x
par(mfrow=c(3, 3))
hist(x, breaks = 2)
hist(x, breaks = 3)
hist(x, breaks = 4)
hist(x, breaks = 5)
hist(x, breaks = 10)
hist(x, breaks = 20)
hist(x, breaks = 30)
hist(x, breaks = 100)
hist(x, breaks = 10000)

hist(x, breaks = seq(-3, 7, by=1))

setwd("/Users/nantang/Google Drive/STAT 421/HW/HW1/")
pdf("hello.pdf")
par(mfrow=c(1,2))
hist(x)
hist(x, freq=F)
dev.off()

dbinom(0, 100, 0.005) ## 0.6057704
dbinom(0:3, 100, 0.005) ## 0.60577044 0.30440725 0.07571939 0.01242965
sum(dbinom(0:3, 100, 0.005)) ## 0.9983267
plot(0:3, dbinom(0:3, 100, 0.005))

x = 0:10
plot(x, dpois(x, 1), type="b")
lines(x, dpois(x, 4), type="b", col=2)
lines(x, dpois(x, 6), type="b", col=3)

x = 0:100
plot(x, dnorm(x, 40, 10))

rbinom(200, 10, 0.5)
rpois(100, 4)
x = rnorm(10000, 0, 1)
hist(x, breaks=200)

boxplot(x, range=0)
boxplot(x, x)

quantile(x, prob=c(0, 0.25, 0.5, 0.75, 1))

x = rnorm(500, 0, 1)
hist(x)
qqnorm(x)

x = rexp(500, 1)
hist(x)
qqnorm(x)

library(lattice)
x = rexp(500, 1)
hist(x)
qqmath(x, dist=qexp)
