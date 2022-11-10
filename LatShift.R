# Code written by Savannah Hartman, August 2022

# Latitudinal shift in Atlantic and Pacific Oceans
# Using centroid points see what environmental data is causing shift
#   Use vegan package, classical rda w/ aic
# Need environmental data for the specific points in time (Atlantic -> 
#   care about environmental parameters for the Gulf Stream and Northern Atlantic 
#   (SST, SSH, Surf Currents, Sa, ML)
#   Pacific -> northern shift due to temperature)

# input table for Phoebastria nigripes, Puffinus griseus, Gavia immer
  # this csv presence/absence

library(vegan)
library(tidyverse)
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
# library(rgdal) # package for geospatial analysis
# library(ggplot2) # package for plotting

# response variables
species            <- read.csv("C:/Users/savan/Desktop/Chapter 2/Species_latlon.csv")
nigripes           <- as.data.frame(species[1:10,2:3]) # Species' occurrence lat/lon
immer              <- as.data.frame(species[11:26,2:3])
griseus            <- as.data.frame(species[27:nrow(species),2:3])
row.names(immer)   <- c(1:16)         # Modify row names 
row.names(griseus) <- c(1:25)
new                <- c("West", "West", "West", "West", "West", "West", "West", "West", "West", "West",
                        "West", "West", "East", "East", "East", "East", "East", "East", "East", "East",
                        "East", "East", "East", "East", "East")
griseus['coast']   <-(new)

# explanatory variables
# Extracted environmental data for specific lat/lon via arcgis, input as tif

# P. griseus species, make raster into data frame then remove zeroes and outliers which indicate NA data
# Eastern current velocity P. griseus east-----------------

grisEV_eastfall90s <- raster("Extract_EVgriseus_eastfall90s.tif")
grisEV_eastfall90s <- as.data.frame(grisEV_eastfall90s, xy = TRUE)
x <- grisEV_eastfall90s%>%
  filter(Extract_EVgriseus_eastfall90s >-20)
av <- mean(x$Extract_EVgriseus_eastfall90s) #number of occurrence points
av
mx <- max(x$Extract_EVgriseus_eastfall90s)
mx
mn <- min(x$Extract_EVgriseus_eastfall90s)
mn

grisEV_eastfall00s <- raster("Extract_EVgriseus_eastfall00s.tif")
grisEV_eastfall00s <- as.data.frame(grisEV_eastfall00s, xy = TRUE)
x <- grisEV_eastfall00s%>%
  filter(Extract_EVgriseus_eastfall00s >-20)
av <- mean(x$Extract_EVgriseus_eastfall00s) #number of occurrence points
av
mx <- max(x$Extract_EVgriseus_eastfall00s)
mx
mn <- min(x$Extract_EVgriseus_eastfall00s)
mn

grisEV_eastfall10s <- raster("Extract_EVgriseus_eastfall10s.tif")
grisEV_eastfall10s <- as.data.frame(grisEV_eastfall10s, xy = TRUE)
x <- grisEV_eastfall10s%>%
  filter(Extract_EVgriseus_eastfall10s >-20)
av <- mean(x$Extract_EVgriseus_eastfall10s) #number of occurrence points
av
mx <- max(x$Extract_EVgriseus_eastfall10s)
mx
mn <- min(x$Extract_EVgriseus_eastfall10s)
mn

grisEV_eastwinter10s <- raster("Extract_EVgriseus_eastwinter10s.tif")
grisEV_eastwinter10s <- as.data.frame(grisEV_eastwinter10s, xy = TRUE)
x <- grisEV_eastwinter10s%>%
  filter(Extract_EVgriseus_eastwinter10s >-20)
av <- mean(x$Extract_EVgriseus_eastwinter10s) #number of occurrence points
av
mx <- max(x$Extract_EVgriseus_eastwinter10s)
mx
mn <- min(x$Extract_EVgriseus_eastwinter10s)
mn

grisEV_eastspring90s <- raster("Extract_EVgriseus_eastspring90s.tif")
grisEV_eastspring90s <- as.data.frame(grisEV_eastspring90s, xy = TRUE)
x <- grisEV_eastspring90s%>%
  filter(Extract_EVgriseus_eastspring90s >-20)
av <- mean(x$Extract_EVgriseus_eastspring90s) #number of occurrence points
av
mx <- max(x$Extract_EVgriseus_eastspring90s)
mx
mn <- min(x$Extract_EVgriseus_eastspring90s)
mn

grisEV_eastspring00s <- raster("Extract_EVgriseus_eastspring00s.tif")
grisEV_eastspring00s <- as.data.frame(grisEV_eastspring00s, xy = TRUE)
x <- grisEV_eastspring00s%>%
  filter(Extract_EVgriseus_eastspring00s >-20)
av <- mean(x$Extract_EVgriseus_eastspring00s) #number of occurrence points
av
mx <- max(x$Extract_EVgriseus_eastspring00s)
mx
mn <- min(x$Extract_EVgriseus_eastspring00s)
mn

grisEV_eastspring10s <- raster("Extract_EVgriseus_eastspring10s.tif")
grisEV_eastspring10s <- as.data.frame(grisEV_eastspring10s, xy = TRUE)
x <- grisEV_eastspring10s%>%
  filter(Extract_EVgriseus_eastspring10s >-20)
av <- mean(x$Extract_EVgriseus_eastspring10s) #number of occurrence points
av
mx <- max(x$Extract_EVgriseus_eastspring10s)
mx
mn <- min(x$Extract_EVgriseus_eastspring10s)
mn

grisEV_eastsummer90s <- raster("Extract_EVgriseus_eastsummer90s.tif")
grisEV_eastsummer90s <- as.data.frame(grisEV_eastsummer90s, xy = TRUE)
x <- grisEV_eastsummer90s%>%
  filter(Extract_EVgriseus_eastsummer90s >-20)
av <- mean(x$Extract_EVgriseus_eastsummer90s) #number of occurrence points
av
mx <- max(x$Extract_EVgriseus_eastsummer90s)
mx
mn <- min(x$Extract_EVgriseus_eastsummer90s)
mn

grisEV_eastsummer00s <- raster("Extract_EVgriseus_eastsummer00s.tif")
grisEV_eastsummer00s <- as.data.frame(grisEV_eastsummer00s, xy = TRUE)
x <- grisEV_eastsummer00s%>%
  filter(Extract_EVgriseus_eastsummer00s >-20)
av <- mean(x$Extract_EVgriseus_eastsummer00s) #number of occurrence points
av
mx <- max(x$Extract_EVgriseus_eastsummer00s)
mx
mn <- min(x$Extract_EVgriseus_eastsummer00s)
mn

grisEV_eastsummer10s <- raster("Extract_EVgriseus_eastsummer10s.tif")
grisEV_eastsummer10s <- as.data.frame(grisEV_eastsummer10s, xy = TRUE)
x <- grisEV_eastsummer10s%>%
  filter(Extract_EVgriseus_eastsummer10s >-20)
av <- mean(x$Extract_EVgriseus_eastsummer10s) #number of occurrence points
av
mx <- max(x$Extract_EVgriseus_eastsummer10s)
mx
mn <- min(x$Extract_EVgriseus_eastsummer10s)
mn

# Eastern current velocity P. griseus west -----

grisEV_westfall90s <- raster("Extract_EVgriseus_westfall90s.tif")
grisEV_westfall90s <- as.data.frame(grisEV_westfall90s, xy = TRUE)
x <- grisEV_westfall90s%>%
  filter(Extract_EVgriseus_westfall90s >-20)
av <- mean(x$Extract_EVgriseus_westfall90s) 
av
mx <- max(x$Extract_EVgriseus_westfall90s)
mx
mn <- min(x$Extract_EVgriseus_westfall90s)
mn

grisEV_westwinter00s <- raster("Extract_EVgriseus_westwinter00s.tif")
grisEV_westwinter00s <- as.data.frame(grisEV_westwinter00s, xy = TRUE)
x <- grisEV_westwinter00s%>%
  filter(Extract_EVgriseus_westwinter00s >-20)
av <- mean(x$Extract_EVgriseus_westwinter00s) 
av
mx <- max(x$Extract_EVgriseus_westwinter00s)
mx
mn <- min(x$Extract_EVgriseus_westwinter00s)
mn

grisEV_westspring90s <- raster("Extract_EVgriseus_westspring90s.tif")
grisEV_westspring90s <- as.data.frame(grisEV_westspring90s, xy = TRUE)
x <- grisEV_westspring90s%>%
  filter(Extract_EVgriseus_westspring90s >-20)
av <- mean(x$Extract_EVgriseus_westspring90s) 
av
mx <- max(x$Extract_EVgriseus_westspring90s)
mx
mn <- min(x$Extract_EVgriseus_westspring90s)
mn

grisEV_westsummer90s <- raster("Extract_EVgriseus_westsummer90s.tif")
grisEV_westsummer90s <- as.data.frame(grisEV_westsummer90s, xy = TRUE)
x <- grisEV_westsummer90s%>%
  filter(Extract_EVgriseus_westsummer90s >-20)
av <- mean(x$Extract_EVgriseus_westsummer90s) 
av
mx <- max(x$Extract_EVgriseus_westsummer90s)
mx
mn <- min(x$Extract_EVgriseus_westsummer90s)
mn

grisEV_westsummer00s <- raster("Extract_EVgriseus_westsummer00s.tif")
grisEV_westsummer00s <- as.data.frame(grisEV_westsummer00s, xy = TRUE)
x <- grisEV_westsummer00s%>%
  filter(Extract_EVgriseus_westsummer00s >-20)
av <- mean(x$Extract_EVgriseus_westsummer00s) 
av
mx <- max(x$Extract_EVgriseus_westsummer00s)
mx
mn <- min(x$Extract_EVgriseus_westsummer00s)
mn

grisEV_westsummer10s <- raster("Extract_EVgriseus_westsummer10s.tif")
grisEV_westsummer10s <- as.data.frame(grisEV_westsummer10s, xy = TRUE)
x <- grisEV_westsummer10s%>%
  filter(Extract_EVgriseus_westsummer10s >-20)
av <- mean(x$Extract_EVgriseus_westsummer10s) 
av
mx <- max(x$Extract_EVgriseus_westsummer10s)
mx
mn <- min(x$Extract_EVgriseus_westsummer10s)
mn

# Mixed layer depth P. griseus east----------------------
grisML_eastfall90s <- raster("Extract_MLgriseus_eastfall90s.tif")
grisML_eastfall90s <- as.data.frame(grisML_eastfall90s, xy = TRUE)
x <- grisML_eastfall90s%>%
  filter(Extract_MLgriseus_eastfall90s >=0)
av <- mean(x$Extract_MLgriseus_eastfall90s) #number of occurrence points
av
mx <- max(x$Extract_MLgriseus_eastfall90s)
mx
mn <- min(x$Extract_MLgriseus_eastfall90s)
mn

grisML_eastfall00s <- raster("Extract_MLgriseus_eastfall00s.tif")
grisML_eastfall00s <- as.data.frame(grisML_eastfall00s, xy = TRUE)
x <- grisML_eastfall00s%>%
  filter(Extract_MLgriseus_eastfall00s >0)
av <- mean(x$Extract_MLgriseus_eastfall00s) #number of occurrence points
mx <- max(x$Extract_MLgriseus_eastfall00s)
mn <- min(x$Extract_MLgriseus_eastfall00s)

grisML_eastfall10s <- raster("Extract_MLgriseus_eastfall10s.tif")
grisML_eastfall10s <- as.data.frame(grisML_eastfall10s, xy = TRUE)
x <- grisML_eastfall10s%>%
  filter(Extract_MLgriseus_eastfall10s >0)
av <- mean(x$Extract_MLgriseus_eastfall10s) #number of occurrence points
mx <- max(x$Extract_MLgriseus_eastfall10s)
mn <- min(x$Extract_MLgriseus_eastfall10s)

grisML_eastwinter10s <- raster("Extract_MLgriseus_eastwinter10s.tif")
grisML_eastwinter10s <- as.data.frame(grisML_eastwinter10s, xy = TRUE)
av <- mean(grisML_eastwinter10s$Extract_MLgriseus_eastwinter10s) #number of occurrence points
mx <- max(grisML_eastwinter10s$Extract_MLgriseus_eastwinter10s)
mn <- min(grisML_eastwinter10s$Extract_MLgriseus_eastwinter10s)

grisML_eastspring90s <- raster("Extract_MLgriseus_eastspring90s.tif")
grisML_eastspring90s <- as.data.frame(grisML_eastspring90s, xy = TRUE)
av <- mean(grisML_eastspring90s$Extract_MLgriseus_eastspring90s) #number of occurrence points
av
mx <- max(grisML_eastspring90s$Extract_MLgriseus_eastspring90s)
mx
mn <- min(grisML_eastspring90s$Extract_MLgriseus_eastspring90s)
mn

grisML_eastspring00s <- raster("Extract_MLgriseus_eastspring00s.tif")
grisML_eastspring00s <- as.data.frame(grisML_eastspring00s, xy = TRUE)
x <- grisML_eastspring00s%>%
  filter(Extract_MLgriseus_eastspring00s >=0)
av <- mean(x$Extract_MLgriseus_eastspring00s) #number of occurrence points
av
mx <- max(x$Extract_MLgriseus_eastspring00s)
mx
mn <- min(x$Extract_MLgriseus_eastspring00s)
mn

grisML_eastspring10s <- raster("Extract_MLgriseus_eastspring10s.tif")
grisML_eastspring10s <- as.data.frame(grisML_eastspring10s, xy = TRUE)
x <- grisML_eastspring10s%>%
  filter(Extract_MLgriseus_eastspring10s >=0)
av <- mean(x$Extract_MLgriseus_eastspring10s) #number of occurrence points
av
mx <- max(x$Extract_MLgriseus_eastspring10s)
mx
mn <- min(x$Extract_MLgriseus_eastspring10s)
mn

grisML_eastsummer90s <- raster("Extract_MLgriseus_eastsummer90s.tif")
grisML_eastsummer90s <- as.data.frame(grisML_eastsummer90s, xy = TRUE)
x <- grisML_eastsummer90s%>%
  filter(Extract_MLgriseus_eastsummer90s >=0)
av <- mean(x$Extract_MLgriseus_eastsummer90s) #number of occurrence points
av
mx <- max(x$Extract_MLgriseus_eastsummer90s)
mx
mn <- min(x$Extract_MLgriseus_eastsummer90s)
mn

grisML_eastsummer00s <- raster("Extract_MLgriseus_eastsummer00s.tif")
grisML_eastsummer00s <- as.data.frame(grisML_eastsummer00s, xy = TRUE)
x <- grisML_eastsummer00s%>%
  filter(Extract_MLgriseus_eastsummer00s >=0)
av <- mean(x$Extract_MLgriseus_eastsummer00s) #number of occurrence points
av
mx <- max(x$Extract_MLgriseus_eastsummer00s)
mx
mn <- min(x$Extract_MLgriseus_eastsummer00s)
mn

grisML_eastsummer10s <- raster("Extract_MLgriseus_eastsummer10s.tif")
grisML_eastsummer10s <- as.data.frame(grisML_eastsummer10s, xy = TRUE)
x <- grisML_eastsummer10s%>%
  filter(Extract_MLgriseus_eastsummer10s >=0)
av <- mean(x$Extract_MLgriseus_eastsummer10s) #number of occurrence points
av
mx <- max(x$Extract_MLgriseus_eastsummer10s)
mx
mn <- min(x$Extract_MLgriseus_eastsummer10s)
mn

#basic usage, X is a matrix or data frame of explanatory variables
#my_rda <- rda(Y ~ x1 + x2 + x3, data = X)

# rdaNigripes <- rda(nigripes ~ ML_Spring2000s +	SST_Spring2000s +	Sa_Spring2000s +
#                      NV_Spring2000s	+ EV_Spring2000s +	SSH_Spring2000s	+ 
#                      ML_Spring2010s	+ SST_Spring2010s	+ Sa_Spring2010s +
#                      NV_Spring2010s +	EV_Spring2010s + SSH_Spring2010s +
#                      ML_Summer1990s	+ SST_Summer1990s	+ Sa_Summer1990s +
#                      NV_Summer1990s	+ EV_Summer1990s +	SSH_Summer1990s	+ 
#                      ML_Summer2000s	+ SST_Summer2000s	+ Sa_Summer2000s	+ 
#                      NV_Summer2000s	+ EV_Summer2000s +	SSH_Summer2000s	+ 
#                      ML_Summer2010s	+ SST_Summer2010s	+ Sa_Summer2010s +
#                      NV_Summer2010s	+ EV_Summer2010s +	SSH_Summer2010s+
#                      ML_Fall1990s ... , data = envNigripes)
#BUT use the shorter, concise code for seasonal decade
rdaNigripes1 <- rda(nigripes ~., data = envNigripes[( 1:6 )])    #fall1990s

# Mixed layer depth P. griseus west ----

grisML_westfall90s <- raster("Extract_MLgriseus_westfall90s.tif")
grisML_westfall90s <- as.data.frame(grisML_westfall90s, xy = TRUE)
x <- grisML_westfall90s%>%
  filter(Extract_MLgriseus_westfall90s >-20)
av <- mean(x$Extract_MLgriseus_westfall90s) 
av
mx <- max(x$Extract_MLgriseus_westfall90s)
mx
mn <- min(x$Extract_MLgriseus_westfall90s)
mn

grisML_westwinter00s <- raster("Extract_MLgriseus_westwinter00s.tif")
grisML_westwinter00s <- as.data.frame(grisML_westwinter00s, xy = TRUE)
x <- grisML_westwinter00s%>%
  filter(Extract_MLgriseus_westwinter00s >-20)
av <- mean(x$Extract_MLgriseus_westwinter00s) 
av
mx <- max(x$Extract_MLgriseus_westwinter00s)
mx
mn <- min(x$Extract_MLgriseus_westwinter00s)
mn

grisML_westspring90s <- raster("Extract_MLgriseus_westspring90s.tif")
grisML_westspring90s <- as.data.frame(grisML_westspring90s, xy = TRUE)
x <- grisML_westspring90s%>%
  filter(Extract_MLgriseus_westspring90s >-20)
av <- mean(x$Extract_MLgriseus_westspring90s) 
av
mx <- max(x$Extract_MLgriseus_westspring90s)
mx
mn <- min(x$Extract_MLgriseus_westspring90s)
mn

grisML_westsummer90s <- raster("Extract_MLgriseus_westsummer90s.tif")
grisML_westsummer90s <- as.data.frame(grisML_westsummer90s, xy = TRUE)
x <- grisML_westsummer90s%>%
  filter(Extract_MLgriseus_westsummer90s >-20)
av <- mean(x$Extract_MLgriseus_westsummer90s) 
av
mx <- max(x$Extract_MLgriseus_westsummer90s)
mx
mn <- min(x$Extract_MLgriseus_westsummer90s)
mn

grisML_westsummer00s <- raster("Extract_MLgriseus_westsummer00s.tif")
grisML_westsummer00s <- as.data.frame(grisML_westsummer00s, xy = TRUE)
x <- grisML_westsummer00s%>%
  filter(Extract_MLgriseus_westsummer00s >-20)
av <- mean(x$Extract_MLgriseus_westsummer00s) 
av
mx <- max(x$Extract_MLgriseus_westsummer00s)
mx
mn <- min(x$Extract_MLgriseus_westsummer00s)
mn

grisML_westsummer10s <- raster("Extract_MLgriseus_westsummer10s.tif")
grisML_westsummer10s <- as.data.frame(grisML_westsummer10s, xy = TRUE)
x <- grisML_westsummer10s%>%
  filter(Extract_MLgriseus_westsummer10s >-20)
av <- mean(x$Extract_MLgriseus_westsummer10s) 
av
mx <- max(x$Extract_MLgriseus_westsummer10s)
mx
mn <- min(x$Extract_MLgriseus_westsummer10s)
mn

# Northern current velocity P. griseus east ----------------

grisNV_eastfall90s <- raster("Extract_NVgriseus_eastfall90s.tif")
grisNV_eastfall90s <- as.data.frame(grisNV_eastfall90s, xy = TRUE)
x <- grisNV_eastfall90s%>%
  filter(Extract_NVgriseus_eastfall90s >-20)
av <- mean(x$Extract_NVgriseus_eastfall90s) #number of occurrence points
av
mx <- max(x$Extract_NVgriseus_eastfall90s)
mx
mn <- min(x$Extract_NVgriseus_eastfall90s)
mn

grisNV_eastfall00s <- raster("Extract_NVgriseus_eastfall00s.tif")
grisNV_eastfall00s <- as.data.frame(grisNV_eastfall00s, xy = TRUE)
x <- grisNV_eastfall00s%>%
  filter(Extract_NVgriseus_eastfall00s >-20)
av <- mean(x$Extract_NVgriseus_eastfall00s) #number of occurrence points
av
mx <- max(x$Extract_NVgriseus_eastfall00s)
mx
mn <- min(x$Extract_NVgriseus_eastfall00s)
mn

grisNV_eastfall10s <- raster("Extract_NVgriseus_eastfall10s.tif")
grisNV_eastfall10s <- as.data.frame(grisNV_eastfall10s, xy = TRUE)
x <- grisNV_eastfall10s%>%
  filter(Extract_NVgriseus_eastfall10s >-20)
av <- mean(x$Extract_NVgriseus_eastfall10s) #number of occurrence points
av
mx <- max(x$Extract_NVgriseus_eastfall10s)
mx
mn <- min(x$Extract_NVgriseus_eastfall10s)
mn

grisNV_eastwinter10s <- raster("Extract_NVgriseus_eastwinter10s.tif")
grisNV_eastwinter10s <- as.data.frame(grisNV_eastwinter10s, xy = TRUE)
x <- grisNV_eastwinter10s%>%
  filter(Extract_NVgriseus_eastwinter10s >-20)
av <- mean(x$Extract_NVgriseus_eastwinter10s) #number of occurrence points
av
mx <- max(x$Extract_NVgriseus_eastwinter10s)
mx
mn <- min(x$Extract_NVgriseus_eastwinter10s)
mn

grisNV_eastspring90s <- raster("Extract_NVgriseus_eastspring90s.tif")
grisNV_eastspring90s <- as.data.frame(grisNV_eastspring90s, xy = TRUE)
x <- grisNV_eastspring90s%>%
  filter(Extract_NVgriseus_eastspring90s >-20)
av <- mean(x$Extract_NVgriseus_eastspring90s) #number of occurrence points
av
mx <- max(x$Extract_NVgriseus_eastspring90s)
mx
mn <- min(x$Extract_NVgriseus_eastspring90s)
mn

grisNV_eastspring00s <- raster("Extract_NVgriseus_eastspring00s.tif")
grisNV_eastspring00s <- as.data.frame(grisNV_eastspring00s, xy = TRUE)
x <- grisNV_eastspring00s%>%
  filter(Extract_NVgriseus_eastspring00s >-20)
av <- mean(x$Extract_NVgriseus_eastspring00s) #number of occurrence points
av
mx <- max(x$Extract_NVgriseus_eastspring00s)
mx
mn <- min(x$Extract_NVgriseus_eastspring00s)
mn

grisNV_eastspring10s <- raster("Extract_NVgriseus_eastspring10s.tif")
grisNV_eastspring10s <- as.data.frame(grisNV_eastspring10s, xy = TRUE)
x <- grisNV_eastspring10s%>%
  filter(Extract_NVgriseus_eastspring10s >-20)
av <- mean(x$Extract_NVgriseus_eastspring10s) #number of occurrence points
av
mx <- max(x$Extract_NVgriseus_eastspring10s)
mx
mn <- min(x$Extract_NVgriseus_eastspring10s)
mn

grisNV_eastsummer90s <- raster("Extract_NVgriseus_eastsummer90s.tif")
grisNV_eastsummer90s <- as.data.frame(grisNV_eastsummer90s, xy = TRUE)
x <- grisNV_eastsummer90s%>%
  filter(Extract_NVgriseus_eastsummer90s >-20)
av <- mean(x$Extract_NVgriseus_eastsummer90s) #number of occurrence points
av
mx <- max(x$Extract_NVgriseus_eastsummer90s)
mx
mn <- min(x$Extract_NVgriseus_eastsummer90s)
mn

grisNV_eastsummer00s <- raster("Extract_NVgriseus_eastsummer00s.tif")
grisNV_eastsummer00s <- as.data.frame(grisNV_eastsummer00s, xy = TRUE)
x <- grisNV_eastsummer00s%>%
  filter(Extract_NVgriseus_eastsummer00s >-20)
av <- mean(x$Extract_NVgriseus_eastsummer00s) #number of occurrence points
av
mx <- max(x$Extract_NVgriseus_eastsummer00s)
mx
mn <- min(x$Extract_NVgriseus_eastsummer00s)
mn

grisNV_eastsummer10s <- raster("Extract_NVgriseus_eastsummer10s.tif")
grisNV_eastsummer10s <- as.data.frame(grisNV_eastsummer10s, xy = TRUE)
x <- grisNV_eastsummer10s%>%
  filter(Extract_NVgriseus_eastsummer10s >-20)
av <- mean(x$Extract_NVgriseus_eastsummer10s) #number of occurrence points
av
mx <- max(x$Extract_NVgriseus_eastsummer10s)
mx
mn <- min(x$Extract_NVgriseus_eastsummer10s)
mn
# Northern current velocity P. griseus west ----

grisNV_westfall90s <- raster("Extract_NVgriseus_westfall90s.tif")
grisNV_westfall90s <- as.data.frame(grisNV_westfall90s, xy = TRUE)
x <- grisNV_westfall90s%>%
  filter(Extract_NVgriseus_westfall90s >-20)
av <- mean(x$Extract_NVgriseus_westfall90s) 
av
mx <- max(x$Extract_NVgriseus_westfall90s)
mx
mn <- min(x$Extract_NVgriseus_westfall90s)
mn

grisNV_westwinter00s <- raster("Extract_NVgriseus_westwinter00s.tif")
grisNV_westwinter00s <- as.data.frame(grisNV_westwinter00s, xy = TRUE)
x <- grisNV_westwinter00s%>%
  filter(Extract_NVgriseus_westwinter00s >-20)
av <- mean(x$Extract_NVgriseus_westwinter00s) 
av
mx <- max(x$Extract_NVgriseus_westwinter00s)
mx
mn <- min(x$Extract_NVgriseus_westwinter00s)
mn

grisNV_westspring90s <- raster("Extract_NVgriseus_westspring90s.tif")
grisNV_westspring90s <- as.data.frame(grisNV_westspring90s, xy = TRUE)
x <- grisNV_westspring90s%>%
  filter(Extract_NVgriseus_westspring90s >-20)
av <- mean(x$Extract_NVgriseus_westspring90s) 
av
mx <- max(x$Extract_NVgriseus_westspring90s)
mx
mn <- min(x$Extract_NVgriseus_westspring90s)
mn

grisNV_westsummer90s <- raster("Extract_NVgriseus_westsummer90s.tif")
grisNV_westsummer90s <- as.data.frame(grisNV_westsummer90s, xy = TRUE)
x <- grisNV_westsummer90s%>%
  filter(Extract_NVgriseus_westsummer90s >-20)
av <- mean(x$Extract_NVgriseus_westsummer90s) 
av
mx <- max(x$Extract_NVgriseus_westsummer90s)
mx
mn <- min(x$Extract_NVgriseus_westsummer90s)
mn

grisNV_westsummer00s <- raster("Extract_NVgriseus_westsummer00s.tif")
grisNV_westsummer00s <- as.data.frame(grisNV_westsummer00s, xy = TRUE)
x <- grisNV_westsummer00s%>%
  filter(Extract_NVgriseus_westsummer00s >-20)
av <- mean(x$Extract_NVgriseus_westsummer00s) 
av
mx <- max(x$Extract_NVgriseus_westsummer00s)
mx
mn <- min(x$Extract_NVgriseus_westsummer00s)
mn

grisNV_westsummer10s <- raster("Extract_NVgriseus_westsummer10s.tif")
grisNV_westsummer10s <- as.data.frame(grisNV_westsummer10s, xy = TRUE)
x <- grisNV_westsummer10s%>%
  filter(Extract_NVgriseus_westsummer10s >-20)
av <- mean(x$Extract_NVgriseus_westsummer10s) 
av
mx <- max(x$Extract_NVgriseus_westsummer10s)
mx
mn <- min(x$Extract_NVgriseus_westsummer10s)
mn

# Salinity P. griseus east ----------------

grisSa_eastfall90s <- raster("Extract_Sagriseus_eastfall90s.tif")
grisSa_eastfall90s <- as.data.frame(grisSa_eastfall90s, xy = TRUE)
x <- grisSa_eastfall90s%>%
  filter(Extract_Sagriseus_eastfall90s >0)
av <- mean(x$Extract_Sagriseus_eastfall90s) #number of occurrence points
av
mx <- max(x$Extract_Sagriseus_eastfall90s)
mx
mn <- min(x$Extract_Sagriseus_eastfall90s)
mn

grisSa_eastfall00s <- raster("Extract_Sagriseus_eastfall00s.tif")
grisSa_eastfall00s <- as.data.frame(grisSa_eastfall00s, xy = TRUE)
x <- grisSa_eastfall00s%>%
  filter(Extract_Sagriseus_eastfall00s >0)
av <- mean(x$Extract_Sagriseus_eastfall00s) #number of occurrence points
av
mx <- max(x$Extract_Sagriseus_eastfall00s)
mx
mn <- min(x$Extract_Sagriseus_eastfall00s)
mn

grisSa_eastfall10s <- raster("Extract_Sagriseus_eastfall10s.tif")
grisSa_eastfall10s <- as.data.frame(grisSa_eastfall10s, xy = TRUE)
x <- grisSa_eastfall10s%>%
  filter(Extract_Sagriseus_eastfall10s >0)
av <- mean(x$Extract_Sagriseus_eastfall10s) #number of occurrence points
av
mx <- max(x$Extract_Sagriseus_eastfall10s)
mx
mn <- min(x$Extract_Sagriseus_eastfall10s)
mn

grisSa_eastwinter10s <- raster("Extract_Sagriseus_eastwinter10s.tif")
grisSa_eastwinter10s <- as.data.frame(grisSa_eastwinter10s, xy = TRUE)
x <- grisSa_eastwinter10s%>%
  filter(Extract_Sagriseus_eastwinter10s >0)
av <- mean(x$Extract_Sagriseus_eastwinter10s) #number of occurrence points
av
mx <- max(x$Extract_Sagriseus_eastwinter10s)
mx
mn <- min(x$Extract_Sagriseus_eastwinter10s)
mn

grisSa_eastspring90s <- raster("Extract_Sagriseus_eastspring90s.tif")
grisSa_eastspring90s <- as.data.frame(grisSa_eastspring90s, xy = TRUE)
x <- grisSa_eastspring90s%>%
  filter(Extract_Sagriseus_eastspring90s >0)
av <- mean(x$Extract_Sagriseus_eastspring90s) #number of occurrence points
av
mx <- max(x$Extract_Sagriseus_eastspring90s)
mx
mn <- min(x$Extract_Sagriseus_eastspring90s)
mn

grisSa_eastspring00s <- raster("Extract_Sagriseus_eastspring00s.tif")
grisSa_eastspring00s <- as.data.frame(grisSa_eastspring00s, xy = TRUE)
x <- grisSa_eastspring00s%>%
  filter(Extract_Sagriseus_eastspring00s >0)
av <- mean(x$Extract_Sagriseus_eastspring00s) #number of occurrence points
av
mx <- max(x$Extract_Sagriseus_eastspring00s)
mx
mn <- min(x$Extract_Sagriseus_eastspring00s)
mn

grisSa_eastspring10s <- raster("Extract_Sagriseus_eastspring10s.tif")
grisSa_eastspring10s <- as.data.frame(grisSa_eastspring10s, xy = TRUE)
x <- grisSa_eastspring10s%>%
  filter(Extract_Sagriseus_eastspring10s >0)
av <- mean(x$Extract_Sagriseus_eastspring10s) #number of occurrence points
av
mx <- max(x$Extract_Sagriseus_eastspring10s)
mx
mn <- min(x$Extract_Sagriseus_eastspring10s)
mn

grisSa_eastsummer90s <- raster("Extract_Sagriseus_eastsummer90s.tif")
grisSa_eastsummer90s <- as.data.frame(grisSa_eastsummer90s, xy = TRUE)
x <- grisSa_eastsummer90s%>%
  filter(Extract_Sagriseus_eastsummer90s >0)
av <- mean(x$Extract_Sagriseus_eastsummer90s) #number of occurrence points
av
mx <- max(x$Extract_Sagriseus_eastsummer90s)
mx
mn <- min(x$Extract_Sagriseus_eastsummer90s)
mn

grisSa_eastsummer00s <- raster("Extract_Sagriseus_eastsummer00s.tif")
grisSa_eastsummer00s <- as.data.frame(grisSa_eastsummer00s, xy = TRUE)
x <- grisSa_eastsummer00s%>%
  filter(Extract_Sagriseus_eastsummer00s >0)
av <- mean(x$Extract_Sagriseus_eastsummer00s) #number of occurrence points
av
mx <- max(x$Extract_Sagriseus_eastsummer00s)
mx
mn <- min(x$Extract_Sagriseus_eastsummer00s)
mn

grisSa_eastsummer10s <- raster("Extract_Sagriseus_eastsummer10s.tif")
grisSa_eastsummer10s <- as.data.frame(grisSa_eastsummer10s, xy = TRUE)
x <- grisSa_eastsummer10s%>%
  filter(Extract_Sagriseus_eastsummer10s >0)
av <- mean(x$Extract_Sagriseus_eastsummer10s) #number of occurrence points
av
mx <- max(x$Extract_Sagriseus_eastsummer10s)
mx
mn <- min(x$Extract_Sagriseus_eastsummer10s)
mn

# Salinity P. griseus west -----

grisSa_westfall90s <- raster("Extract_Sagriseus_westfall90s.tif")
grisSa_westfall90s <- as.data.frame(grisSa_westfall90s, xy = TRUE)
x <- grisSa_westfall90s%>%
  filter(Extract_Sagriseus_westfall90s >0)
av <- mean(x$Extract_Sagriseus_westfall90s) 
av
mx <- max(x$Extract_Sagriseus_westfall90s)
mx
mn <- min(x$Extract_Sagriseus_westfall90s)
mn

grisSa_westwinter00s <- raster("Extract_Sagriseus_westwinter00s.tif")
grisSa_westwinter00s <- as.data.frame(grisSa_westwinter00s, xy = TRUE)
x <- grisSa_westwinter00s%>%
  filter(Extract_Sagriseus_westwinter00s >0)
av <- mean(x$Extract_Sagriseus_westwinter00s) 
av
mx <- max(x$Extract_Sagriseus_westwinter00s)
mx
mn <- min(x$Extract_Sagriseus_westwinter00s)
mn

grisSa_westspring90s <- raster("Extract_Sagriseus_westspring90s.tif")
grisSa_westspring90s <- as.data.frame(grisSa_westspring90s, xy = TRUE)
x <- grisSa_westspring90s%>%
  filter(Extract_Sagriseus_westspring90s >0)
av <- mean(x$Extract_Sagriseus_westspring90s) 
av
mx <- max(x$Extract_Sagriseus_westspring90s)
mx
mn <- min(x$Extract_Sagriseus_westspring90s)
mn

grisSa_westsummer90s <- raster("Extract_Sagriseus_westsummer90s.tif")
grisSa_westsummer90s <- as.data.frame(grisSa_westsummer90s, xy = TRUE)
x <- grisSa_westsummer90s%>%
  filter(Extract_Sagriseus_westsummer90s >0)
av <- mean(x$Extract_Sagriseus_westsummer90s) 
av
mx <- max(x$Extract_Sagriseus_westsummer90s)
mx
mn <- min(x$Extract_Sagriseus_westsummer90s)
mn

grisSa_westsummer00s <- raster("Extract_Sagriseus_westsummer00s.tif")
grisSa_westsummer00s <- as.data.frame(grisSa_westsummer00s, xy = TRUE)
x <- grisSa_westsummer00s%>%
  filter(Extract_Sagriseus_westsummer00s >0)
av <- mean(x$Extract_Sagriseus_westsummer00s) 
av
mx <- max(x$Extract_Sagriseus_westsummer00s)
mx
mn <- min(x$Extract_Sagriseus_westsummer00s)
mn

grisSa_westsummer10s <- raster("Extract_Sagriseus_westsummer10s.tif")
grisSa_westsummer10s <- as.data.frame(grisSa_westsummer10s, xy = TRUE)
x <- grisSa_westsummer10s%>%
  filter(Extract_Sagriseus_westsummer10s >0)
av <- mean(x$Extract_Sagriseus_westsummer10s) 
av
mx <- max(x$Extract_Sagriseus_westsummer10s)
mx
mn <- min(x$Extract_Sagriseus_westsummer10s)
mn

# Sea surface temperature P. griseus east ------

grisSST_eastfall90s <- raster("Extract_SSTgriseus_eastfall90s.tif")
grisSST_eastfall90s <- as.data.frame(grisSST_eastfall90s, xy = TRUE)
x <- grisSST_eastfall90s%>%
  filter(Extract_SSTgriseus_eastfall90s >0)
av <- mean(x$Extract_SSTgriseus_eastfall90s) #number of occurrence points
av
mx <- max(x$Extract_SSTgriseus_eastfall90s)
mx
mn <- min(x$Extract_SSTgriseus_eastfall90s)
mn

grisSST_eastfall00s <- raster("Extract_SSTgriseus_eastfall00s.tif")
grisSST_eastfall00s <- as.data.frame(grisSST_eastfall00s, xy = TRUE)
x <- grisSST_eastfall00s%>%
  filter(Extract_SSTgriseus_eastfall00s >0)
av <- mean(x$Extract_SSTgriseus_eastfall00s) #number of occurrence points
av
mx <- max(x$Extract_SSTgriseus_eastfall00s)
mx
mn <- min(x$Extract_SSTgriseus_eastfall00s)
mn

grisSST_eastfall10s <- raster("Extract_SSTgriseus_eastfall10s.tif")
grisSST_eastfall10s <- as.data.frame(grisSST_eastfall10s, xy = TRUE)
x <- grisSST_eastfall10s%>%
  filter(Extract_SSTgriseus_eastfall10s >0)
av <- mean(x$Extract_SSTgriseus_eastfall10s) #number of occurrence points
av
mx <- max(x$Extract_SSTgriseus_eastfall10s)
mx
mn <- min(x$Extract_SSTgriseus_eastfall10s)
mn

grisSST_eastwinter10s <- raster("Extract_SSTgriseus_eastwinter10s.tif")
grisSST_eastwinter10s <- as.data.frame(grisSST_eastwinter10s, xy = TRUE)
x <- grisSST_eastwinter10s%>%
  filter(Extract_SSTgriseus_eastwinter10s >0)
av <- mean(x$Extract_SSTgriseus_eastwinter10s) #number of occurrence points
av
mx <- max(x$Extract_SSTgriseus_eastwinter10s)
mx
mn <- min(x$Extract_SSTgriseus_eastwinter10s)
mn


grisSST_eastspring90s <- raster("Extract_SSTgriseus_eastspring90s.tif")
grisSST_eastspring90s <- as.data.frame(grisSST_eastspring90s, xy = TRUE)
x <- grisSST_eastspring90s%>%
  filter(Extract_SSTgriseus_eastspring90s >0)
av <- mean(x$Extract_SSTgriseus_eastspring90s) #number of occurrence points
av
mx <- max(x$Extract_SSTgriseus_eastspring90s)
mx
mn <- min(x$Extract_SSTgriseus_eastspring90s)
mn

grisSST_eastspring00s <- raster("Extract_SSTgriseus_eastspring00s.tif")
grisSST_eastspring00s <- as.data.frame(grisSST_eastspring00s, xy = TRUE)
x <- grisSST_eastspring00s%>%
  filter(Extract_SSTgriseus_eastspring00s >0)
av <- mean(x$Extract_SSTgriseus_eastspring00s) #number of occurrence points
av
mx <- max(x$Extract_SSTgriseus_eastspring00s)
mx
mn <- min(x$Extract_SSTgriseus_eastspring00s)
mn

grisSST_eastspring10s <- raster("Extract_SSTgriseus_eastspring10s.tif")
grisSST_eastspring10s <- as.data.frame(grisSST_eastspring10s, xy = TRUE)
x <- grisSST_eastspring10s%>%
  filter(Extract_SSTgriseus_eastspring10s >0)
av <- mean(x$Extract_SSTgriseus_eastspring10s) #number of occurrence points
av
mx <- max(x$Extract_SSTgriseus_eastspring10s)
mx
mn <- min(x$Extract_SSTgriseus_eastspring10s)
mn

grisSST_eastsummer90s <- raster("Extract_SSTgriseus_eastsummer90s.tif")
grisSST_eastsummer90s <- as.data.frame(grisSST_eastsummer90s, xy = TRUE)
x <- grisSST_eastsummer90s%>%
  filter(Extract_SSTgriseus_eastsummer90s >0)
av <- mean(x$Extract_SSTgriseus_eastsummer90s) #number of occurrence points
av
mx <- max(x$Extract_SSTgriseus_eastsummer90s)
mx
mn <- min(x$Extract_SSTgriseus_eastsummer90s)
mn

grisSST_eastsummer00s <- raster("Extract_SSTgriseus_eastsummer00s.tif")
grisSST_eastsummer00s <- as.data.frame(grisSST_eastsummer00s, xy = TRUE)
x <- grisSST_eastsummer00s%>%
  filter(Extract_SSTgriseus_eastsummer00s >0)
av <- mean(x$Extract_SSTgriseus_eastsummer00s) #number of occurrence points
av
mx <- max(x$Extract_SSTgriseus_eastsummer00s)
mx
mn <- min(x$Extract_SSTgriseus_eastsummer00s)
mn

grisSST_eastsummer10s <- raster("Extract_SSTgriseus_eastsummer10s.tif")
grisSST_eastsummer10s <- as.data.frame(grisSST_eastsummer10s, xy = TRUE)
x <- grisSST_eastsummer10s%>%
  filter(Extract_SSTgriseus_eastsummer10s >0)
av <- mean(x$Extract_SSTgriseus_eastsummer10s) #number of occurrence points
av
mx <- max(x$Extract_SSTgriseus_eastsummer10s)
mx
mn <- min(x$Extract_SSTgriseus_eastsummer10s)
mn
# Sea surface temperature P. griseus west ----

grisSST_westfall90s <- raster("Extract_SSTgriseus_westfall90s.tif")
grisSST_westfall90s <- as.data.frame(grisSST_westfall90s, xy = TRUE)
x <- grisSST_westfall90s%>%
  filter(Extract_SSTgriseus_westfall90s >0)
av <- mean(x$Extract_SSTgriseus_westfall90s) 
av
mx <- max(x$Extract_SSTgriseus_westfall90s)
mx
mn <- min(x$Extract_SSTgriseus_westfall90s)
mn

grisSST_westwinter00s <- raster("Extract_SSTgriseus_westwinter00s.tif")
grisSST_westwinter00s <- as.data.frame(grisSST_westwinter00s, xy = TRUE)
x <- grisSST_westwinter00s%>%
  filter(Extract_SSTgriseus_westwinter00s >0)
av <- mean(x$Extract_SSTgriseus_westwinter00s) 
av
mx <- max(x$Extract_SSTgriseus_westwinter00s)
mx
mn <- min(x$Extract_SSTgriseus_westwinter00s)
mn

grisSST_westspring90s <- raster("Extract_SSTgriseus_westspring90s.tif")
grisSST_westspring90s <- as.data.frame(grisSST_westspring90s, xy = TRUE)
x <- grisSST_westspring90s%>%
  filter(Extract_SSTgriseus_westspring90s >0)
av <- mean(x$Extract_SSTgriseus_westspring90s) 
av
mx <- max(x$Extract_SSTgriseus_westspring90s)
mx
mn <- min(x$Extract_SSTgriseus_westspring90s)
mn

grisSST_westsummer90s <- raster("Extract_SSTgriseus_westsummer90s.tif")
grisSST_westsummer90s <- as.data.frame(grisSST_westsummer90s, xy = TRUE)
x <- grisSST_westsummer90s%>%
  filter(Extract_SSTgriseus_westsummer90s >0)
av <- mean(x$Extract_SSTgriseus_westsummer90s) 
av
mx <- max(x$Extract_SSTgriseus_westsummer90s)
mx
mn <- min(x$Extract_SSTgriseus_westsummer90s)
mn

grisSST_westsummer00s <- raster("Extract_SSTgriseus_westsummer00s.tif")
grisSST_westsummer00s <- as.data.frame(grisSST_westsummer00s, xy = TRUE)
x <- grisSST_westsummer00s%>%
  filter(Extract_SSTgriseus_westsummer00s >0)
av <- mean(x$Extract_SSTgriseus_westsummer00s) 
av
mx <- max(x$Extract_SSTgriseus_westsummer00s)
mx
mn <- min(x$Extract_SSTgriseus_westsummer00s)
mn

grisSST_westsummer10s <- raster("Extract_SSTgriseus_westsummer10s.tif")
grisSST_westsummer10s <- as.data.frame(grisSST_westsummer10s, xy = TRUE)
x <- grisSST_westsummer10s%>%
  filter(Extract_SSTgriseus_westsummer10s >0)
av <- mean(x$Extract_SSTgriseus_westsummer10s) 
av
mx <- max(x$Extract_SSTgriseus_westsummer10s)
mx
mn <- min(x$Extract_SSTgriseus_westsummer10s)
mn

# Sea surface height P. griseus east ----

grisSSH_eastfall90s <- raster("Extract_SSHgriseus_eastfall90s.tif")
grisSSH_eastfall90s <- as.data.frame(grisSSH_eastfall90s, xy = TRUE)
x <- grisSSH_eastfall90s
av <- mean(x$Extract_SSHgriseus_eastfall90s) #number of occurrence points
av
mx <- max(x$Extract_SSHgriseus_eastfall90s)
mx
mn <- min(x$Extract_SSHgriseus_eastfall90s)
mn

grisSSH_eastfall00s <- raster("Extract_SSHgriseus_eastfall00s.tif")
grisSSH_eastfall00s <- as.data.frame(grisSSH_eastfall00s, xy = TRUE)
x <- grisSSH_eastfall00s%>%
  filter(Extract_SSHgriseus_eastfall00s >-10)
av <- mean(x$Extract_SSHgriseus_eastfall00s) #number of occurrence points
av
mx <- max(x$Extract_SSHgriseus_eastfall00s)
mx
mn <- min(x$Extract_SSHgriseus_eastfall00s)
mn

grisSSH_eastfall10s <- raster("Extract_SSHgriseus_eastfall10s.tif")
grisSSH_eastfall10s <- as.data.frame(grisSSH_eastfall10s, xy = TRUE)
x <- grisSSH_eastfall10s%>%
  filter(Extract_SSHgriseus_eastfall10s >-10)
av <- mean(x$Extract_SSHgriseus_eastfall10s) #number of occurrence points
av
mx <- max(x$Extract_SSHgriseus_eastfall10s)
mx
mn <- min(x$Extract_SSHgriseus_eastfall10s)
mn

grisSSH_eastwinter10s <- raster("Extract_SSHgriseus_eastwinter10s.tif")
grisSSH_eastwinter10s <- as.data.frame(grisSSH_eastwinter10s, xy = TRUE)
x <- grisSSH_eastwinter10s%>%
  filter(Extract_SSHgriseus_eastwinter10s >-10)
av <- mean(x$Extract_SSHgriseus_eastwinter10s) #number of occurrence points
av
mx <- max(x$Extract_SSHgriseus_eastwinter10s)
mx
mn <- min(x$Extract_SSHgriseus_eastwinter10s)
mn

grisSSH_eastspring90s <- raster("Extract_SSHgriseus_eastspring90s.tif")
grisSSH_eastspring90s <- as.data.frame(grisSSH_eastspring90s, xy = TRUE)
x <- grisSSH_eastspring90s
av <- mean(x$Extract_SSHgriseus_eastspring90s) #number of occurrence points
av
mx <- max(x$Extract_SSHgriseus_eastspring90s)
mx
mn <- min(x$Extract_SSHgriseus_eastspring90s)
mn

grisSSH_eastspring00s <- raster("Extract_SSHgriseus_eastspring00s.tif")
grisSSH_eastspring00s <- as.data.frame(grisSSH_eastspring00s, xy = TRUE)
x <- grisSSH_eastspring00s%>%
  filter(Extract_SSHgriseus_eastspring00s >-10)
av <- mean(x$Extract_SSHgriseus_eastspring00s) #number of occurrence points
av
mx <- max(x$Extract_SSHgriseus_eastspring00s)
mx
mn <- min(x$Extract_SSHgriseus_eastspring00s)
mn

grisSSH_eastspring10s <- raster("Extract_SSHgriseus_eastspring10s.tif")
grisSSH_eastspring10s <- as.data.frame(grisSSH_eastspring10s, xy = TRUE)
x <- grisSSH_eastspring10s%>%
  filter(Extract_SSHgriseus_eastspring10s >-10)
av <- mean(x$Extract_SSHgriseus_eastspring10s) #number of occurrence points
av
mx <- max(x$Extract_SSHgriseus_eastspring10s)
mx
mn <- min(x$Extract_SSHgriseus_eastspring10s)
mn

grisSSH_eastsummer90s <- raster("Extract_SSHgriseus_eastsummer90s.tif")
grisSSH_eastsummer90s <- as.data.frame(grisSSH_eastsummer90s, xy = TRUE)
x <- grisSSH_eastsummer90s %>%
  filter(Extract_SSHgriseus_eastsummer90s >-10)
av <- mean(x$Extract_SSHgriseus_eastsummer90s) #number of occurrence points
av
mx <- max(x$Extract_SSHgriseus_eastsummer90s)
mx
mn <- min(x$Extract_SSHgriseus_eastsummer90s)
mn

grisSSH_eastsummer00s <- raster("Extract_SSHgriseus_eastsummer00s.tif")
grisSSH_eastsummer00s <- as.data.frame(grisSSH_eastsummer00s, xy = TRUE)
x <- grisSSH_eastsummer00s %>%
  filter(Extract_SSHgriseus_eastsummer00s >-10)
av <- mean(x$Extract_SSHgriseus_eastsummer00s) #number of occurrence points
av
mx <- max(x$Extract_SSHgriseus_eastsummer00s)
mx
mn <- min(x$Extract_SSHgriseus_eastsummer00s)
mn

grisSSH_eastsummer10s <- raster("Extract_SSHgriseus_eastsummer10s.tif")
grisSSH_eastsummer10s <- as.data.frame(grisSSH_eastsummer10s, xy = TRUE)
x <- grisSSH_eastsummer10s %>%
  filter(Extract_SSHgriseus_eastsummer10s >-10)
av <- mean(x$Extract_SSHgriseus_eastsummer10s) #number of occurrence points
av
mx <- max(x$Extract_SSHgriseus_eastsummer10s)
mx
mn <- min(x$Extract_SSHgriseus_eastsummer10s)
mn
# Sea surface height P. griseus west ----

grisSSH_westfall90s <- raster("Extract_SSHgriseus_westfall90s.tif")
grisSSH_westfall90s <- as.data.frame(grisSSH_westfall90s, xy = TRUE)
x <- grisSSH_westfall90s%>%
  filter(Extract_SSHgriseus_westfall90s >-10)
av <- mean(x$Extract_SSHgriseus_westfall90s) 
av
mx <- max(x$Extract_SSHgriseus_westfall90s)
mx
mn <- min(x$Extract_SSHgriseus_westfall90s)
mn

grisSSH_westwinter00s <- raster("Extract_SSHgriseus_westwinter00s.tif")
grisSSH_westwinter00s <- as.data.frame(grisSSH_westwinter00s, xy = TRUE)
x <- grisSSH_westwinter00s%>%
  filter(Extract_SSHgriseus_westwinter00s >-10)
av <- mean(x$Extract_SSHgriseus_westwinter00s) 
av
mx <- max(x$Extract_SSHgriseus_westwinter00s)
mx
mn <- min(x$Extract_SSHgriseus_westwinter00s)
mn

grisSSH_westspring90s <- raster("Extract_SSHgriseus_westspring90s.tif")
grisSSH_westspring90s <- as.data.frame(grisSSH_westspring90s, xy = TRUE)
x <- grisSSH_westspring90s%>%
  filter(Extract_SSHgriseus_westspring90s >-10)
av <- mean(x$Extract_SSHgriseus_westspring90s) 
av
mx <- max(x$Extract_SSHgriseus_westspring90s)
mx
mn <- min(x$Extract_SSHgriseus_westspring90s)
mn

grisSSH_westsummer90s <- raster("Extract_SSHgriseus_westsummer90s.tif")
grisSSH_westsummer90s <- as.data.frame(grisSSH_westsummer90s, xy = TRUE)
x <- grisSSH_westsummer90s%>%
  filter(Extract_SSHgriseus_westsummer90s >-10)
av <- mean(x$Extract_SSHgriseus_westsummer90s) 
av
mx <- max(x$Extract_SSHgriseus_westsummer90s)
mx
mn <- min(x$Extract_SSHgriseus_westsummer90s)
mn

grisSSH_westsummer00s <- raster("Extract_SSHgriseus_westsummer00s.tif")
grisSSH_westsummer00s <- as.data.frame(grisSSH_westsummer00s, xy = TRUE)
x <- grisSSH_westsummer00s%>%
  filter(Extract_SSHgriseus_westsummer00s >-10)
av <- mean(x$Extract_SSHgriseus_westsummer00s) 
av
mx <- max(x$Extract_SSHgriseus_westsummer00s)
mx
mn <- min(x$Extract_SSHgriseus_westsummer00s)
mn

grisSSH_westsummer10s <- raster("Extract_SSHgriseus_westsummer10s.tif")
grisSSH_westsummer10s <- as.data.frame(grisSSH_westsummer10s, xy = TRUE)
x <- grisSSH_westsummer10s%>%
  filter(Extract_SSHgriseus_westsummer10s >-10)
av <- mean(x$Extract_SSHgriseus_westsummer10s) 
av
mx <- max(x$Extract_SSHgriseus_westsummer10s)
mx
mn <- min(x$Extract_SSHgriseus_westsummer10s)
mn