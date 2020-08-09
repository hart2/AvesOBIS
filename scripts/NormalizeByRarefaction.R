# This script plots LME's and observations inside LME's
# Written by E. Montes (2019), edited by S. Hartman
# August 3, 2020

install.packages("maptools")
install.packages("rgeos")
install.packages("rgdal")
install.packages("maps")
install.packages("mapproj")

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
library(maptools)
library(maps)
library(mapproj)


# LME Subregions ----------------------------------------------------------

# First view the shapefile, get a feel for it
lme <- st_read(
  "data/LME66/LMEs66.shp") # found http://lme.edc.uri.edu/index.php/digital-data
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
# Specify the local directory and name of the shapefile 
# and read its contents (global coastline data)
path.lme.coast <- ("data/LME66") #localdirectory
fnam.lme.coast <- "LMEs66.shp" #shapefile
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

xlims <- c(-148, -18) #longitude
ylims <- c(-60,55)    #latitude

# World map
mapWorld <- borders(database = "world", colour="gray50", fill="gray50")
# Generate a base map with the coastline:

p0 <- ggplot() + theme(text = element_text(size=15),plot.title = element_text(hjust = 0.5),
                       plot.caption = element_text(hjust = 0.6)) + 
  geom_path(data = dat.coast, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.25) + 
  coord_map(projection = "mercator") + 
  scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
  scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
  labs(title = "Large Marine Ecosystems", x = "Longitude", y = "Latitude", 
       caption = "Figure 1. Large Marine Ecosystems as subregions for rarefaction 
       analysis. Generated from NOAA shapefiles (http://www.lme.noaa.gov/).")

p0 # view map

# highlight LME of interest
# p.sel <- p0  + mapWorld +
# geom_path(data = dat.sel_1,
#           aes(x = long, y = lat, group = group),
#           colour = "goldenrod2", size = 0.75) +
# geom_path(data = dat.sel_2,
#           aes(x = long, y = lat, group = group),
#           colour = "coral3", size = 0.75) +
# geom_path(data = dat.sel_3,
#           aes(x = long, y = lat, group = group),
#           colour = "coral", size = 0.75) +
# geom_path(data = dat.sel_4,
#           aes(x = long, y = lat, group = group),
#           colour = "chocolate4", size = 0.75) +
# geom_path(data = dat.sel_5,
#           aes(x = long, y = lat, group = group),
#           colour = "chocolate1", size = 0.75) +
# geom_path(data = dat.sel_6,
#           aes(x = long, y = lat, group = group),
#           colour = "chartreuse4", size = 0.75) +
# geom_path(data = dat.sel_7,
#           aes(x = long, y = lat, group = group),
#           colour = "chartreuse", size = 0.75) +
# geom_path(data = dat.sel_8,
#           aes(x = long, y = lat, group = group),
#           colour = "cadetblue1", size = 0.75) +
# geom_path(data = dat.sel_9,
#           aes(x = long, y = lat, group = group),
#           colour = "cadetblue4", size = 0.75) +
# geom_path(data = dat.sel_10,
#           aes(x = long, y = lat, group = group),
#           colour = "brown3", size = 0.75) +
# geom_path(data = dat.sel_11,
#           aes(x = long, y = lat, group = group),
#           colour = "purple", size = 0.75) +
#   geom_path(data = dat.sel_12,
#             aes(x = long, y = lat, group = group),
#             colour = "blue", size = 0.75) +
#   geom_path(data = dat.sel_13,
#             aes(x = long, y = lat, group = group),
#             colour = "green", size = 0.75) +
#   geom_path(data = dat.sel_14,
#             aes(x = long, y = lat, group = group),
#             colour = "red", size = 0.75)
# p.sel #view


# Separate Data by Subregion ----------------------------------------------

# want to filter out the necessary data from each region using polygons dat.sel_#
aves1 <- Aves_EEZnd %>% 
  filter() # North Brazil shelf

aves2 <- Aves_EEZnd %>% 
  filter() # California Current

aves3 <- Aves_EEZnd %>% 
  filter() # Gulf of California

aves4 <- Aves_EEZnd %>% 
  filter() # Gulf of Mexico

aves5 <- Aves_EEZnd %>% 
  filter() # Southeast U.S. Continental Shelf

aves6 <- Aves_EEZnd %>% 
  filter() # Northeast U.S. Continental Shelf

aves7 <- Aves_EEZnd %>% 
  filter() # Scotian Shelf

aves8 <- Aves_EEZnd %>% 
  filter() # Pacific Central-American

aves9 <- Aves_EEZnd %>% 
  filter() # Caribbean Sea

aves10 <- Aves_EEZnd %>% 
  filter() # Humboldt Current

aves11 <- Aves_EEZnd %>% 
  filter() # Patagonian Shelf

aves12 <- Aves_EEZnd %>% 
  filter() # South Brazil Shelf

aves13 <- Aves_EEZnd %>% 
  filter() # East Brazil Shelf

aves14 <- Aves_EEZnd %>% 
  filter() # Newfoundland-Labrador Shelf
