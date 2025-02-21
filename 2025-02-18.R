install.packages("mosaic")

library(mosaic)


mu <- 10 # for "mean" parameter
sigma <- 2 # for "sd" parameter
plotDist("norm", mean=mu, sd=sigma, xlab="x", ylab="Frequency")


s1 <- rnorm(n = 10, mean = 10, sd = 2)
mean(s1)
sd(s1)

s2 <- rnorm(n = 1000, mean = 10, sd = 2)
mean(s2)
sd(s2)

histogram(s1)
histogram(s2)


# sampling distribution - repeat distrbution of samples

reps <- 500 #replicates

samp_dist_mean <-
  do(reps) * mean(rnorm(n = 1000, mean = 10, sd = 2))

str(samp_dist_mean) #turns it to a vector

histogram(samp_dist_mean$mean)

mean(samp_dist_mean$mean)

samp_dist_median <-
  do(reps) * median(rnorm(n = 1000, mean = 10, sd = 2))
str(samp_dist_median)

histogram(samp_dist_median$median)

meadian(samp_dist_median$median)

# sample size gets larger, error bars get more narrow




#Standard error, is a standard deviation of a summary distribution
#Standard deviation, is applied to any vector

se <- sd(x)/sqrt(length(x)) # good for one sample SE

se_mean <- sd(samp_dist_mean$mean)
se_mean <- sd(samp_dist_median$median)


sd(samp_dist_mean$mean) # good for if you have access to all sample SE

x<- rnorm(n = 10, mean = 10, sd = 2)



s2 <- rpois(n = 10, lambda = 10)
mean(s2)
sd(s2)

# increase sample size will have mean more closer to where it should be


# if the confidence interval doesn't include 0 let's people know if something is significant


x<- rnorm(n= 100, mean= 2, sd= 4)

histogram(x)

mean(x)
sd(x)
se <- sd(x)/sqrt(length(x))

reps <- 1000

x <-
  do(reps) * mean(rnorm(n = 100, mean = 2, sd = 4))

se <- sd(x$mean)

histogram(x$mean)


plotDist("t", df=99, xlab="x", ylab="Frequency", col="red")

plotDist("t", df = 50, add = TRUE)


plotDist("beta", shape1 = 0.3 , shape2 = 4)

x = rbeta(n = 100, shape1 = 0.3, shape2 = 4)

histogram(x)

reps <- 500

s <- do(reps) * mean (rbeta(n=20, .3, 4))

histogram(s$mean)
sd(s$mean)

se = sd(x)/sqrt(length(x))

c <- qnorm(c(0.025, 0.50, 0.975), mean = 0, sd =1)


d = qnorm(p = c(0.025, 0.975), mean = 0, sd = 1)

plotDist("beta", shape1 =2, shape2 = 4)
c = qbeta(p = c(0.025, 0.975), shape1 = 2, shape2 = 4)

# confidience interval:
# +/- ctricial value x the standard error of the statistics

x = c(2.9, 4.8, 8.9, -3.2, 9.1, -2.5, -0.9, -0.1, 2.8, -1.7)
m = mean(x)
se = sd(x)/sqrt(length(x))
ci = m + qnorm(c(0.025, 0.975)) * se
ci = m + c(qnorm(0.025), qnorm(0.975)) * se

percent_ci = 0.95
alpha = 1 - percnet_ci/100
ci = m + qnorm(c(alpha/2, (1 - (alpha/2)))) * se


#Central Limit Theorem (CLT) - basis

#Z value is understood as the critical value

#T value is crtical value for small sample size

#degrees of freedom is sample size minus 1 / T value is only possible if your data set is less than 30

#marticarlo simulation

n_boot = 10000
boot = vector(length=n_boot)
n = length(x)
for (i in 1:n_boot){
  boot[[i]] = mean(sample(x, n, replace=TRUE))
}
ci = quantile(boot, probs = c(0.025, 0.975))

histogram(boot)
hist(boot)
