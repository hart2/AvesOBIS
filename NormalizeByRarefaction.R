# This script plots LME's and observations inside LME's
# Written by E. Montes (2019), edited by S. Hartman
# August 3, 2020

install.packages("maptools")
library(robis)
library(rgdal) # for `ogrInfo()` and `readOGR()`
library(tools) # for `file_path_sans_ext()`
library(dplyr) # for `inner_join()`, `filter()`, `summarise()`, and the pipe operator (%>%)
library(ggplot2) # for `fortify()` and for plotting
library(sp) # for `point.in.polygon()` and `spDists()`
library(tidyr) # for `gather()`
library(readr) # for `write_tsv()`
library(leaflet)
library(lubridate)
library(sp)
library(sf)
library(iNEXT)

# First view the shapefile, get a feel for it
lme <- st_read(
  "LME66/LMEs66.shp")
ggplot() + 
  geom_sf(data = lme, color = "black", fill = "cyan1") + 
  coord_sf()


# Provide the function fortify.shape(), which puts the shapefile data in the object class data.frame, 
# so that it can be used by ggplot2

fortify.shape <- function(x){
  x@data$id <- rownames(x@data)
  x.f <- fortify(x, region = "id")
  x.join <- inner_join(x.f, x@data, by = "id")
}

# extract portions of the data (from the fortified data.frame object) for a smaller domain

subset.shape <- function(x, domain){
  x.subset <- filter(x, long > domain[1] & 
                       long < domain[2] & 
                       lat > domain[3] & 
                       lat < domain[4])
  x.subset
}

# Plotting the coastline and some animal observations
# Specify the local directory and name of the Natural Earth shapefile (previously downloaded) 
# and read its contents (global coastline data)
path.lme.coast <- ("LME66")
fnam.lme.coast <- "LMEs66.shp"
dat.coast <- readOGR(dsn = path.lme.coast, 
                     layer = file_path_sans_ext(fnam.lme.coast))

# fortify the global data and then extract domain
dat.coast <- fortify.shape(dat.coast) 
# Specify the desired LME:
# See numbers here: http://lme.edc.uri.edu/index.php/lme-introduction
dat.sel_1 <- subset(dat.coast, LME_NUMBER == 17) # North Brazil Shelf 
dat.sel_2 <- subset(dat.coast, LME_NUMBER == 3) # California Current
dat.sel_3 <- subset(dat.coast, LME_NUMBER == 4) # Gulf of California
dat.sel_4 <- subset(dat.coast, LME_NUMBER == 5) # Gulf of Mexico
dat.sel_5 <- subset(dat.coast, LME_NUMBER == 6) # Southeast U.S. Continental Shelf
dat.sel_6 <- subset(dat.coast, LME_NUMBER == 7) # Northeast U.S. Continental Shelf
dat.sel_7 <- subset(dat.coast, LME_NUMBER == 8) # Scotian Shelf
dat.sel_8 <- subset(dat.coast, LME_NUMBER == 11) # Pacific Central-American
dat.sel_9 <- subset(dat.coast, LME_NUMBER == 12) # Caribbean Sea
dat.sel_10 <- subset(dat.coast, LME_NUMBER == 13) # Humboldt Current
dat.sel_11 <- subset(dat.coast, LME_NUMBER == 14) # Patagonian Shelf
dat.sel_12 <- subset(dat.coast, LME_NUMBER == 15) # South Brazil Shelf
dat.sel_13 <- subset(dat.coast, LME_NUMBER == 16) # East Brazil Shelf
dat.sel_14 <- subset(dat.coast, LME_NUMBER == 9) # Newfoundland-Labrador Shelf
