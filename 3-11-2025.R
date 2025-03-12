library(tidyverse)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main"



#review module 1 - module 19 over spring break
#deep dive into literature
#coding assignment due on sunday
#no class thursday
#submit abstract by friday
#prepare for journal club friday
#schedule doctor's appointment wednesday
#contact kia about policy buyout wednesday
#contact kia about gap coverage refund wednesday

# chase
# 2000 vs 500
# 1500 vs 1000

# capitol
# 2800 vs 500
# 1500 vs 1800

# navy
# 500

500+1500+1000+1500+1800

6300-800






d <- tibble(val = rnorm(50, mean = 50, sd = 10))

ggplot(d) +
  geom_histogram(aes(x = val, y = after_stat(density))) +
  stat_function(fun = function(x) dnorm(x, mean = 50, sd = 10), color = "red", linewidth = 1) +
  stat_function(fun = function(x) dnorm(x, mean = 65, sd = 10), color = "blue", linewidth =1)

(mean <- mean(d$val))
(sd <- sd(d$val))


val <- c(70, 75, 50)
mean <- 65
sd <- 10

(likelihood <- 1/sqrt(2*pi*sd^2) *
    exp((-(val - mean)^2)/(2*sd^2)))
# or
(likelihood <- dnorm(val, mean, sd))

nll <- -1 * log(likelihood) # natural log
(summed_nll <- sum(nll))


verbose_nll <- function(val, mu, sigma) {
  likelihood <- 0
  ll <- 0
  for (i in 1:length(val)){
    likelihood[[i]] = dnorm(val[[i]], mean = mu, sd = sigma)
    ll[[i]] <- log(likelihood[[i]])
    message(paste0(val[[i]], " ", mean, " ", sd, " ", ll[[i]] ))
  }
  nll <- -1 * sum(ll)
  return(nll)
}

verbose_nll(val, mean, sd)

simple_nll <- function(mu, sigma, verbose = FALSE)
{
  ll = sum(dnorm(val, mean = mu, sd = sigma, log = TRUE))
  nll <- -1 * ll
  if (verbose == TRUE) {
    message(paste0("mean=", mu, " sigma=", sd, " nll=", nll))
  }
  return(nll)
  }
}

simple_nll(mean, sd)

install.packages("bbmle")
library(bbmle)


val <- rnorm(100, mean = 50, sd = 10)
simple_nll(50, 10)
simple_nll(51, 10)
simple_nll(51, 11)
simple_nll(49, 10)
simple_nll(100, 10)


val <- rnorm(50, 50, 10)

mle_norm <- bbmle::mle2(
  minuslogl = simple_nll,
  start = list(mu = 0, sigma = 1),
  method = "SANN",
  trace = TRUE
)

mle_norm






