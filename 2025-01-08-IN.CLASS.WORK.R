library(tidyverse)


 f <- file.choose()
# f <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/Country-Data-2016.csv"
d <- read_csv(f, col_names = TRUE)
d

class(d)

d$country

# vairalbes = 16
# observations = 248
# countries = 248
names(d)
meidan(d$population, na.rm = TRUE)
median(d$area, na.rm = TRUE)

d$density <- d$population/d$area

v <- sort(d$density, decreasing = TRUE)

d <- d %>% arrange(desc(density))

str(d)
structure(d)

d$country

d[ ,1]

structure(d)

d <- as.data.frame(d)
