library(tidyverse)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/KamilarAndCooperData.csv"

d <- read_csv(f, col_names = TRUE) # creates a "tibble"

mean(d$Brain_Size_Species_Mean, na.rm = TRUE)

attach(d)

mean(Brain_Size_Species_Mean, na.rm = TRUE)

detach(d)

with(d, 
     mean(Brain_Size_Species_Mean, na.rm = TRUE)
     detach(d)
     with(
       d,
       mean(Body_mass_male_mean, na.rm = TRUE)
     ))

summary(d)

install.packages("skimr")
library(skimr)
skim(d)

boxplot(log(d$Body_mass_female_mean))
stripchart(log(d$Body_mass_female_mean), 
           method = "jitter",
           col = "blue",
           vertical = TRUE,
           add = TRUE)

boxplot(log(d$Body_mass_female_mean) ~ Family)
stripchart(log(d$Body_mass_female_mean) ~
             Family, 
           method = "jitter",
           col = "blue",
           vertical = TRUE,
           add = TRUE)

# ggplot (data, mapping, aes, geom, theme)

p <- ggplot(
  data = d,
  aes(x = "", y = log(Body_mass_female_mean))
)+
  geom_boxplot(na.rm = TRUE)

p <- p + geom_boxplot(na.rm = TRUE)

p <- p + geom_jitter(
  color = "blue",
  width = 0.5
)

p <- ggplot(
  data = d,
  aes(x = Family, y = log(Body_mass_female_mean)))+
  geom_boxplot(na.rm=TRUE) +
  geom_jitter(
    color = "red",
    width = 0.5) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("log(Female Body Mass)")

p


           
