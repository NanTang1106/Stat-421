# question a
trial <- 10000
size <- 100
sample_trials <- numeric(trial)

for(i in 1:trial) {
  sample <- rnorm(size, 1, 2)
  sample_trials[i] = mean(sample)
}
qqnorm(sample_trials)
hist(sample_trials)


# question b
x = rnorm(1000, 0, 1)
eql_prob <- seq(0, 1, length.out = 1000)
sample_qtl <- quantile(x, probs=eql_prob)
theoretic_qtl <- qnorm(eql_prob,0,1)
plot(x=qnorm(eql_prob,0,1), y=sample_qtl)

qqnorm(x)
points(x=theoretic_qtl, y=sample_qtl, col=2, cex=0.5)


# question c
x = rexp(1000, 1)
samp_qtl <- quantile(x, probs=eql_prob)
theo_qtl <- qexp(eql_prob, 1)
plot(samp_qtl, theo_qtl)


# question d
y <- rexp(1000, 1)
x <- rnorm(2000, 0, 1)
qqplot(x, y)


# question e
## if the 1-sample normal qqplot resembles the plot in d, the distribution of sample data in 
## this 1-sample case should be less denser than standard normal on two tails 





