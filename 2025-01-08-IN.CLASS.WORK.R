f <- file.choose()
d <- read.csv(f)
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

order(d$density)
sort