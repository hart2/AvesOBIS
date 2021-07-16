library(anytime)
library(readr)
Aves <- read_csv("Aves.csv")

Aves$eventDate_2 <- as.POSIXct(Aves$date_mid/1000, origin="1970-01-01", tz="UTC")

# times
# Aves$eventDate[1:10]
# ?as.POSIXct
# as.matrix(as.Date(times[,1], origin="1970-01-01"))

