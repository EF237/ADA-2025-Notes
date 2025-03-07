install.packages("tidyverse")
library(tidyverse)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"
d <- read_csv(f, col_names = TRUE)

lm(formula = height ~ weight, data = d)

plot(x = d$weight, y = d$height)

m <- lm(height ~ weight, data = d)

summary(m) #summary of linear model

names(m) #objects within model

m$coefficients
m$model
head(m$model)
m$fitted.values # expectation of height for each value of weight
m$coefficients
hist(m$residuals)
plot(m)
qqplot(m)

broom::tidy(m)
confint(m)
broom::glance(m)

abline(ceof(m1), col = "blue")

install.packages("lmodel2")
library(lmodel2)

m2 = lmodel2(height~weight, data = d,
             range.y = "relative", range.x = "relative",
             nperm = 1000)

m2

plot(x=d$weight, y=d$height)

betas <- broom::tidy(m2) |>
  filter(method == "OLS") |>
  pull(estimate)
abline(betas, col = "blue")

betas <- broom::tidy(m2)|>
  filter(method == "RMA") |>
  pull(estimate)
abline(betas, col = "red")

betas <- broom::tidy(m2)|>
  filter(method == "MA") |>
  pull(estimate)
abline(betas, col = "green")

or

plot(m2, "OLS")
plot(m2, "MA")
plot(m2, "RMA")

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/Street_et_al_2017.csv"
d <- read_csv(f, col_names = TRUE)

m = lm(formula = ECV ~ Group_size, data = d)

m = lm(ECV ~ Group_size, d)

par(mfrow = c(2,2))

plot(d$Group_size, d$ECV) 
plot(d$Longevity, d$ECV) 
plot(d$Weaning, d$ECV) 
plot(d$Repro_lifespan, d$ECV) 

m1 = lm(formula = ECV~ Group_size, data = d)
m2 = lm(formula = ECV~ Longevity, data = d)
m3 = lm(formula = ECV~ Weaning, data = d)
m4 = lm(formula = ECV~ Repro_lifespan, data = d)

broom::tidy(m1)
confint(m1)
broom::tidy(m2)
confint(m2)
broom::tidy(m3)
confint(m3)
broom::tidy(m4)
confint(m4)


c = d |>
  filter (Taxonomic_group == "Catarrhini")

p = d |>
  filter (Taxonomic_group == "Platyrrhini")

s = d |>
  filter (Taxonomic_group == "Strepsirhini")


