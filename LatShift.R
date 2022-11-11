# Code written by Savannah Hartman, August 2022

# Latitudinal shift in Atlantic and Pacific Oceans
# Using centroid points see what environmental data is causing shift
#   Use vegan package, classical rda w/ aic
# Need environmental data for the specific points in time (Atlantic -> 
#   care about environmental parameters for the Gulf Stream and Northern Atlantic 
#   (SST, SSH, Surf Currents, Sa, ML)
#   Pacific -> northern shift due to temperature)

library(vegan)
library(tidyverse)
library(ncdat4) # package for netcdat manipulation
library(raster) # package for raster manipulation
# library(rgdal) # package for geospatial analysis
# library(ggplot2) # package for plotting

# response variables
# lat/lon of species 

# explanatory variables
# Extracted environmental data for specific lat/lon via arcgis, input as tif

# P. griseus species, make raster into data frame then remove zeroes and outliers which indicate NA data
# Eastern current velocity P. griseus east-----------------

grisEV_eastfall90s <- raster("Extract_EVgriseus_eastfall90s.tif")
grisEV_eastfall90s <- as.data.frame(grisEV_eastfall90s, xy = TRUE)
grisEV_eastfall90s <- grisEV_eastfall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(grisEV_eastfall90s) <- c("x","y","Season","Decade","EV")

grisEV_eastfall00s <- raster("Extract_EVgriseus_eastfall00s.tif")
grisEV_eastfall00s <- as.data.frame(grisEV_eastfall00s, xy = TRUE)
grisEV_eastfall00s <- grisEV_eastfall00s %>%
  add_column(Season = "Fall",
             Decade = "00s",
             .after = "y")
colnames(grisEV_eastfall00s) <- c("x","y","Season","Decade","EV")

grisEV_eastfall10s <- raster("Extract_EVgriseus_eastfall10s.tif")
grisEV_eastfall10s <- as.data.frame(grisEV_eastfall10s, xy = TRUE)
grisEV_eastfall10s <- grisEV_eastfall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(grisEV_eastfall10s) <- c("x","y","Season","Decade","EV")

grisEV_eastwinter10s <- raster("Extract_EVgriseus_eastwinter10s.tif")
grisEV_eastwinter10s <- as.data.frame(grisEV_eastwinter10s, xy = TRUE)
grisEV_eastwinter10s <- grisEV_eastwinter10s %>%
  add_column(Season = "Winter",
             Decade = "10s",
             .after = "y")
colnames(grisEV_eastwinter10s) <- c("x","y","Season","Decade","EV")

grisEV_eastspring90s <- raster("Extract_EVgriseus_eastspring90s.tif")
grisEV_eastspring90s <- as.data.frame(grisEV_eastspring90s, xy = TRUE)
grisEV_eastspring90s <- grisEV_eastspring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(grisEV_eastspring90s) <- c("x","y","Season","Decade","EV")

grisEV_eastspring00s <- raster("Extract_EVgriseus_eastspring00s.tif")
grisEV_eastspring00s <- as.data.frame(grisEV_eastspring00s, xy = TRUE)
grisEV_eastspring00s <- grisEV_eastspring00s %>%
  add_column(Season = "Spring",
             Decade = "00s",
             .after = "y")
colnames(grisEV_eastspring00s) <- c("x","y","Season","Decade","EV")

grisEV_eastspring10s <- raster("Extract_EVgriseus_eastspring10s.tif")
grisEV_eastspring10s <- as.data.frame(grisEV_eastspring10s, xy = TRUE)
grisEV_eastspring10s <- grisEV_eastspring10s %>%
  add_column(Season = "Spring",
             Decade = "10s",
             .after = "y")
colnames(grisEV_eastspring10s) <- c("x","y","Season","Decade","EV")

grisEV_eastsummer90s <- raster("Extract_EVgriseus_eastsummer90s.tif")
grisEV_eastsummer90s <- as.data.frame(grisEV_eastsummer90s, xy = TRUE)
grisEV_eastsummer90s <- grisEV_eastsummer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(grisEV_eastsummer90s) <- c("x","y","Season","Decade","EV")

grisEV_eastsummer00s <- raster("Extract_EVgriseus_eastsummer00s.tif")
grisEV_eastsummer00s <- as.data.frame(grisEV_eastsummer00s, xy = TRUE)
grisEV_eastsummer00s <- grisEV_eastsummer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(grisEV_eastsummer00s) <- c("x","y","Season","Decade","EV")

grisEV_eastsummer10s <- raster("Extract_EVgriseus_eastsummer10s.tif")
grisEV_eastsummer10s <- as.data.frame(grisEV_eastsummer10s, xy = TRUE)
grisEV_eastsummer10s <- grisEV_eastsummer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(grisEV_eastsummer10s) <- c("x","y","Season","Decade","EV")

# Eastern current velocity P. griseus west -----

grisEV_westfall90s <- raster("Extract_EVgriseus_westfall90s.tif")
grisEV_westfall90s <- as.data.frame(grisEV_westfall90s, xy = TRUE)
grisEV_westfall90s <- grisEV_westfall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(grisEV_westfall90s) <- c("x","y","Season","Decade","EV")

grisEV_westwinter00s <- raster("Extract_EVgriseus_westwinter00s.tif")
grisEV_westwinter00s <- as.data.frame(grisEV_westwinter00s, xy = TRUE)
grisEV_westwinter00s <- grisEV_westwinter00s %>%
  add_column(Season = "Winter",
             Decade = "00s",
             .after = "y")
colnames(grisEV_westwinter00s) <- c("x","y","Season","Decade","EV")

grisEV_westspring90s <- raster("Extract_EVgriseus_westspring90s.tif")
grisEV_westspring90s <- as.data.frame(grisEV_westspring90s, xy = TRUE)
grisEV_westspring90s <- grisEV_westspring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(grisEV_westspring90s) <- c("x","y","Season","Decade","EV")

grisEV_westsummer90s <- raster("Extract_EVgriseus_westsummer90s.tif")
grisEV_westsummer90s <- as.data.frame(grisEV_westsummer90s, xy = TRUE)
grisEV_westsummer90s <- grisEV_westsummer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(grisEV_westsummer90s) <- c("x","y","Season","Decade","EV")

grisEV_westsummer00s <- raster("Extract_EVgriseus_westsummer00s.tif")
grisEV_westsummer00s <- as.data.frame(grisEV_westsummer00s, xy = TRUE)
grisEV_westsummer00s <- grisEV_westsummer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(grisEV_westsummer00s) <- c("x","y","Season","Decade","EV")

grisEV_westsummer10s <- raster("Extract_EVgriseus_westsummer10s.tif")
grisEV_westsummer10s <- as.data.frame(grisEV_westsummer10s, xy = TRUE)
grisEV_westsummer10s <- grisEV_westsummer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(grisEV_westsummer10s) <- c("x","y","Season","Decade","EV")

# Eastern current velocity nigripes ----

nigEV_fall90s <- raster("Extract_EVnigripes_fall90s.tif")
nigEV_fall90s <- as.data.frame(nigEV_fall90s, xy = TRUE)
nigEV_fall90s <- nigEV_fall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(nigEV_fall90s) <- c("x","y","Season","Decade","EV")

nigEV_fall10s <- raster("Extract_EVnigripes_fall10s.tif")
nigEV_fall10s <- as.data.frame(nigEV_fall10s, xy = TRUE)
nigEV_fall10s <- nigEV_fall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(nigEV_fall10s) <- c("x","y","Season","Decade","EV")

nigEV_spring90s <- raster("Extract_EVnigripes_spring90s.tif")
nigEV_spring90s <- as.data.frame(nigEV_spring90s, xy = TRUE)
nigEV_spring90s <- nigEV_spring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(nigEV_spring90s) <- c("x","y","Season","Decade","EV")

nigEV_summer90s <- raster("Extract_EVnigripes_summer90s.tif")
nigEV_summer90s <- as.data.frame(nigEV_summer90s, xy = TRUE)
nigEV_summer90s <- nigEV_summer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(nigEV_summer90s) <- c("x","y","Season","Decade","EV")

nigEV_summer00s <- raster("Extract_EVnigripes_summer00s.tif")
nigEV_summer00s <- as.data.frame(nigEV_summer00s, xy = TRUE)
nigEV_summer00s <- nigEV_summer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(nigEV_summer00s) <- c("x","y","Season","Decade","EV")

nigEV_summer10s <- raster("Extract_EVnigripes_summer10s.tif")
nigEV_summer10s <- as.data.frame(nigEV_summer10s, xy = TRUE)
nigEV_summer10s <- nigEV_summer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(nigEV_summer10s) <- c("x","y","Season","Decade","EV")

# Eastern current velocity immer-----

imEV_fall90s <- raster("Extract_EVimmer_fall90s.tif")
imEV_fall90s <- as.data.frame(imEV_fall90s, xy = TRUE)
imEV_fall90s <- imEV_fall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(imEV_fall90s) <- c("x","y","Season","Decade","EV")

imEV_fall00s <- raster("Extract_EVimmer_fall00s.tif")
imEV_fall00s <- as.data.frame(imEV_fall00s, xy = TRUE)
imEV_fall00s <- imEV_fall00s %>%
  add_column(Season = "Fall",
             Decade = "00s",
             .after = "y")
colnames(imEV_fall00s) <- c("x","y","Season","Decade","EV")

imEV_fall10s <- raster("Extract_EVimmer_fall10s.tif")
imEV_fall10s <- as.data.frame(imEV_fall10s, xy = TRUE)
imEV_fall10s <- imEV_fall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(imEV_fall10s) <- c("x","y","Season","Decade","EV")

imEV_winter90s <- raster("Extract_EVimmer_winter90s.tif")
imEV_winter90s <- as.data.frame(imEV_winter90s, xy = TRUE)
imEV_winter90s <- imEV_winter90s %>%
  add_column(Season = "Winter",
             Decade = "90s",
             .after = "y")
colnames(imEV_winter90s) <- c("x","y","Season","Decade","EV")

imEV_winter00s <- raster("Extract_EVimmer_winter00s.tif")
imEV_winter00s <- as.data.frame(imEV_winter00s, xy = TRUE)
imEV_winter00s <- imEV_winter00s %>%
  add_column(Season = "Winter",
             Decade = "00s",
             .after = "y")
colnames(imEV_winter00s) <- c("x","y","Season","Decade","EV")

imEV_winter10s <- raster("Extract_EVimmer_winter10s.tif")
imEV_winter10s <- as.data.frame(imEV_winter10s, xy = TRUE)
imEV_winter10s <- imEV_winter10s %>%
  add_column(Season = "Winter",
             Decade = "10s",
             .after = "y")
colnames(imEV_winter10s) <- c("x","y","Season","Decade","EV")

imEV_spring90s <- raster("Extract_EVimmer_spring90s.tif")
imEV_spring90s <- as.data.frame(imEV_spring90s, xy = TRUE)
imEV_spring90s <- imEV_spring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(imEV_spring90s) <- c("x","y","Season","Decade","EV")

imEV_spring00s <- raster("Extract_EVimmer_spring00s.tif")
imEV_spring00s <- as.data.frame(imEV_spring00s, xy = TRUE)
imEV_spring00s <- imEV_spring00s %>%
  add_column(Season = "Spring",
             Decade = "00s",
             .after = "y")
colnames(imEV_spring00s) <- c("x","y","Season","Decade","EV")

imEV_spring10s <- raster("Extract_EVimmer_spring10s.tif")
imEV_spring10s <- as.data.frame(imEV_spring10s, xy = TRUE)
imEV_spring10s <- imEV_spring10s %>%
  add_column(Season = "Spring",
             Decade = "10s",
             .after = "y")
colnames(imEV_spring10s) <- c("x","y","Season","Decade","EV")

imEV_summer00s <- raster("Extract_EVimmer_summer00s.tif")
imEV_summer00s <- as.data.frame(imEV_summer00s, xy = TRUE)
imEV_summer00s <- imEV_summer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(imEV_summer00s) <- c("x","y","Season","Decade","EV")

imEV_summer10s <- raster("Extract_EVimmer_summer10s.tif")
imEV_summer10s <- as.data.frame(imEV_summer10s, xy = TRUE)
imEV_summer10s <- imEV_summer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(imEV_summer10s) <- c("x","y","Season","Decade","EV")

# Mixed layer depth P. griseus east----------------------
grisML_eastfall90s <- raster("Extract_MLgriseus_eastfall90s.tif")
grisML_eastfall90s <- as.data.frame(grisML_eastfall90s, xy = TRUE)
grisML_eastfall90s <- grisML_eastfall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(grisML_eastfall90s) <- c("x","y","Season","Decade","ML")

grisML_eastfall00s <- raster("Extract_MLgriseus_eastfall00s.tif")
grisML_eastfall00s <- as.data.frame(grisML_eastfall00s, xy = TRUE)
grisML_eastfall00s <- grisML_eastfall00s %>%
  add_column(Season = "Fall",
             Decade = "00s",
             .after = "y")
colnames(grisML_eastfall00s) <- c("x","y","Season","Decade","ML")

grisML_eastfall10s <- raster("Extract_MLgriseus_eastfall10s.tif")
grisML_eastfall10s <- as.data.frame(grisML_eastfall10s, xy = TRUE)
grisML_eastfall10s <- grisML_eastfall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(grisML_eastfall10s) <- c("x","y","Season","Decade","ML")

grisML_eastwinter10s <- raster("Extract_MLgriseus_eastwinter10s.tif")
grisML_eastwinter10s <- as.data.frame(grisML_eastwinter10s, xy = TRUE)
grisML_eastwinter10s <- grisML_eastwinter10s %>%
  add_column(Season = "Winter",
             Decade = "10s",
             .after = "y")
colnames(grisML_eastwinter10s) <- c("x","y","Season","Decade","ML")

grisML_eastspring90s <- raster("Extract_MLgriseus_eastspring90s.tif")
grisML_eastspring90s <- as.data.frame(grisML_eastspring90s, xy = TRUE)
grisML_eastspring90s <- grisML_eastspring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(grisML_eastspring90s) <- c("x","y","Season","Decade","ML")

grisML_eastspring00s <- raster("Extract_MLgriseus_eastspring00s.tif")
grisML_eastspring00s <- as.data.frame(grisML_eastspring00s, xy = TRUE)
grisML_eastspring00s <- grisML_eastspring00s %>%
  add_column(Season = "Spring",
             Decade = "00s",
             .after = "y")
colnames(grisML_eastspring00s) <- c("x","y","Season","Decade","ML")

grisML_eastspring10s <- raster("Extract_MLgriseus_eastspring10s.tif")
grisML_eastspring10s <- as.data.frame(grisML_eastspring10s, xy = TRUE)
grisML_eastspring10s <- grisML_eastspring10s %>%
  add_column(Season = "Spring",
             Decade = "10s",
             .after = "y")
colnames(grisML_eastspring10s) <- c("x","y","Season","Decade","ML")

grisML_eastsummer90s <- raster("Extract_MLgriseus_eastsummer90s.tif")
grisML_eastsummer90s <- as.data.frame(grisML_eastsummer90s, xy = TRUE)
grisML_eastsummer90s <- grisML_eastsummer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(grisML_eastsummer90s) <- c("x","y","Season","Decade","ML")

grisML_eastsummer00s <- raster("Extract_MLgriseus_eastsummer00s.tif")
grisML_eastsummer00s <- as.data.frame(grisML_eastsummer00s, xy = TRUE)
grisML_eastsummer00s <- grisML_eastsummer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(grisML_eastsummer00s) <- c("x","y","Season","Decade","ML")

grisML_eastsummer10s <- raster("Extract_MLgriseus_eastsummer10s.tif")
grisML_eastsummer10s <- as.data.frame(grisML_eastsummer10s, xy = TRUE)
grisML_eastsummer10s <- grisML_eastsummer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(grisML_eastsummer10s) <- c("x","y","Season","Decade","ML")

# Mixed layer depth P. griseus west ----

grisML_westfall90s <- raster("Extract_MLgriseus_westfall90s.tif")
grisML_westfall90s <- as.data.frame(grisML_westfall90s, xy = TRUE)
grisML_westfall90s <- grisML_westfall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(grisML_westfall90s) <- c("x","y","Season","Decade","ML")

grisML_westwinter00s <- raster("Extract_MLgriseus_westwinter00s.tif")
grisML_westwinter00s <- as.data.frame(grisML_westwinter00s, xy = TRUE)
grisML_westwinter00s <- grisML_westwinter00s %>%
  add_column(Season = "Winter",
             Decade = "00s",
             .after = "y")
colnames(grisML_westwinter00s) <- c("x","y","Season","Decade","ML")

grisML_westspring90s <- raster("Extract_MLgriseus_westspring90s.tif")
grisML_westspring90s <- as.data.frame(grisML_westspring90s, xy = TRUE)
grisML_westspring90s <- grisML_westspring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(grisML_westspring90s) <- c("x","y","Season","Decade","ML")

grisML_westsummer90s <- raster("Extract_MLgriseus_westsummer90s.tif")
grisML_westsummer90s <- as.data.frame(grisML_westsummer90s, xy = TRUE)
grisML_westsummer90s <- grisML_westsummer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(grisML_westsummer90s) <- c("x","y","Season","Decade","ML")

grisML_westsummer00s <- raster("Extract_MLgriseus_westsummer00s.tif")
grisML_westsummer00s <- as.data.frame(grisML_westsummer00s, xy = TRUE)
grisML_westsummer00s <- grisML_westsummer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(grisML_westsummer00s) <- c("x","y","Season","Decade","ML")

grisML_westsummer10s <- raster("Extract_MLgriseus_westsummer10s.tif")
grisML_westsummer10s <- as.data.frame(grisML_westsummer10s, xy = TRUE)
grisML_westsummer10s <- grisML_westsummer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(grisML_westsummer10s) <- c("x","y","Season","Decade","ML")

# Mixed layer depth nigripes -----

nigML_fall90s <- raster("Extract_MLnigripes_fall90s.tif")
nigML_fall90s <- as.data.frame(nigML_fall90s, xy = TRUE)
x <- nigML_fall90s%>%
  filter(Extract_MLnigripes_fall90s >-20)
av <- mean(x$Extract_MLnigripes_fall90s) 
av
mx <- max(x$Extract_MLnigripes_fall90s)
mx
mn <- min(x$Extract_MLnigripes_fall90s)
mn

nigML_fall10s <- raster("Extract_MLnigripes_fall10s.tif")
nigML_fall10s <- as.data.frame(nigML_fall10s, xy = TRUE)
x <- nigML_fall10s %>%
  filter(Extract_MLnigripes_fall10s >-20)
av <- mean(x$Extract_MLnigripes_fall10s) 
av
mx <- max(x$Extract_MLnigripes_fall10s)
mx
mn <- min(x$Extract_MLnigripes_fall10s)
mn

nigML_spring90s <- raster("Extract_MLnigripes_spring90s.tif")
nigML_spring90s <- as.data.frame(nigML_spring90s, xy = TRUE)
x <- nigML_spring90s%>%
  filter(Extract_MLnigripes_spring90s >-20)
av <- mean(x$Extract_MLnigripes_spring90s) 
av
mx <- max(x$Extract_MLnigripes_spring90s)
mx
mn <- min(x$Extract_MLnigripes_spring90s)
mn

nigML_summer90s <- raster("Extract_MLnigripes_summer90s.tif")
nigML_summer90s <- as.data.frame(nigML_summer90s, xy = TRUE)
x <- nigML_summer90s%>%
  filter(Extract_MLnigripes_summer90s >-20)
av <- mean(x$Extract_MLnigripes_summer90s) 
av
mx <- max(x$Extract_MLnigripes_summer90s)
mx
mn <- min(x$Extract_MLnigripes_summer90s)
mn

nigML_summer00s <- raster("Extract_MLnigripes_summer00s.tif")
nigML_summer00s <- as.data.frame(nigML_summer00s, xy = TRUE)
x <- nigML_summer00s %>%
  filter(Extract_MLnigripes_summer00s >-20)
av <- mean(x$Extract_MLnigripes_summer00s) 
av
mx <- max(x$Extract_MLnigripes_summer00s)
mx
mn <- min(x$Extract_MLnigripes_summer00s)
mn

nigML_summer10s <- raster("Extract_MLnigripes_summer10s.tif")
nigML_summer10s <- as.data.frame(nigML_summer10s, xy = TRUE)
x <- nigML_summer10s%>%
  filter(Extract_MLnigripes_summer10s >-20)
av <- mean(x$Extract_MLnigripes_summer10s) 
av
mx <- max(x$Extract_MLnigripes_summer10s)
mx
mn <- min(x$Extract_MLnigripes_summer10s)
mn

# Mixed layer depth immer ------

nigML_fall90s <- raster("Extract_MLnigripes_fall90s.tif")
nigML_fall90s <- as.data.frame(nigML_fall90s, xy = TRUE)
nigML_fall90s <- nigML_fall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(nigML_fall90s) <- c("x","y","Season","Decade","ML")

nigML_fall10s <- raster("Extract_MLnigripes_fall10s.tif")
nigML_fall10s <- as.data.frame(nigML_fall10s, xy = TRUE)
nigML_fall10s <- nigML_fall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(nigML_fall10s) <- c("x","y","Season","Decade","ML")

nigML_spring90s <- raster("Extract_MLnigripes_spring90s.tif")
nigML_spring90s <- as.data.frame(nigML_spring90s, xy = TRUE)
nigML_spring90s <- nigML_spring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(nigML_spring90s) <- c("x","y","Season","Decade","ML")

nigML_summer90s <- raster("Extract_MLnigripes_summer90s.tif")
nigML_summer90s <- as.data.frame(nigML_summer90s, xy = TRUE)
nigML_summer90s <- nigML_summer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(nigML_summer90s) <- c("x","y","Season","Decade","ML")

nigML_summer00s <- raster("Extract_MLnigripes_summer00s.tif")
nigML_summer00s <- as.data.frame(nigML_summer00s, xy = TRUE)
nigML_summer00s <- nigML_summer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(nigML_summer00s) <- c("x","y","Season","Decade","ML")

nigML_summer10s <- raster("Extract_MLnigripes_summer10s.tif")
nigML_summer10s <- as.data.frame(nigML_summer10s, xy = TRUE)
nigML_summer10s <- nigML_summer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(nigML_summer10s) <- c("x","y","Season","Decade","ML")

# Northern current velocity P. griseus east ----------------

grisNV_eastfall90s <- raster("Extract_NVgriseus_eastfall90s.tif")
grisNV_eastfall90s <- as.data.frame(grisNV_eastfall90s, xy = TRUE)
grisNV_eastfall90s <- grisNV_eastfall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(grisNV_eastfall90s) <- c("x","y","Season","Decade","NV")

grisNV_eastfall00s <- raster("Extract_NVgriseus_eastfall00s.tif")
grisNV_eastfall00s <- as.data.frame(grisNV_eastfall00s, xy = TRUE)
grisNV_eastfall00s <- grisNV_eastfall00s %>%
  add_column(Season = "Fall",
             Decade = "00s",
             .after = "y")
colnames(grisNV_eastfall00s) <- c("x","y","Season","Decade","NV")

grisNV_eastfall10s <- raster("Extract_NVgriseus_eastfall10s.tif")
grisNV_eastfall10s <- as.data.frame(grisNV_eastfall10s, xy = TRUE)
grisNV_eastfall10s <- grisNV_eastfall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(grisNV_eastfall10s) <- c("x","y","Season","Decade","NV")

grisNV_eastwinter10s <- raster("Extract_NVgriseus_eastwinter10s.tif")
grisNV_eastwinter10s <- as.data.frame(grisNV_eastwinter10s, xy = TRUE)
grisNV_eastwinter10s <- grisNV_eastwinter10s %>%
  add_column(Season = "Winter",
             Decade = "10s",
             .after = "y")
colnames(grisNV_eastwinter10s) <- c("x","y","Season","Decade","NV")

grisNV_eastspring90s <- raster("Extract_NVgriseus_eastspring90s.tif")
grisNV_eastspring90s <- as.data.frame(grisNV_eastspring90s, xy = TRUE)
grisNV_eastspring90s <- grisNV_eastspring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(grisNV_eastspring90s) <- c("x","y","Season","Decade","NV")

grisNV_eastspring00s <- raster("Extract_NVgriseus_eastspring00s.tif")
grisNV_eastspring00s <- as.data.frame(grisNV_eastspring00s, xy = TRUE)
grisNV_eastspring00s <- grisNV_eastspring00s %>%
  add_column(Season = "Spring",
             Decade = "00s",
             .after = "y")
colnames(grisNV_eastspring00s) <- c("x","y","Season","Decade","NV")

grisNV_eastspring10s <- raster("Extract_NVgriseus_eastspring10s.tif")
grisNV_eastspring10s <- as.data.frame(grisNV_eastspring10s, xy = TRUE)
grisNV_eastspring10s <- grisNV_eastspring10s %>%
  add_column(Season = "Spring",
             Decade = "10s",
             .after = "y")
colnames(grisNV_eastspring10s) <- c("x","y","Season","Decade","NV")

grisNV_eastsummer90s <- raster("Extract_NVgriseus_eastsummer90s.tif")
grisNV_eastsummer90s <- as.data.frame(grisNV_eastsummer90s, xy = TRUE)
grisNV_eastsummer90s <- grisNV_eastsummer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(grisNV_eastsummer90s) <- c("x","y","Season","Decade","NV")

grisNV_eastsummer00s <- raster("Extract_NVgriseus_eastsummer00s.tif")
grisNV_eastsummer00s <- as.data.frame(grisNV_eastsummer00s, xy = TRUE)
grisNV_eastsummer00s <- grisNV_eastsummer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(grisNV_eastsummer00s) <- c("x","y","Season","Decade","NV")

grisNV_eastsummer10s <- raster("Extract_NVgriseus_eastsummer10s.tif")
grisNV_eastsummer10s <- as.data.frame(grisNV_eastsummer10s, xy = TRUE)
grisNV_eastsummer10s <- grisNV_eastsummer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(grisNV_eastsummer10s) <- c("x","y","Season","Decade","NV")

# Northern current velocity P. griseus west ----

grisNV_westfall90s <- raster("Extract_NVgriseus_westfall90s.tif")
grisNV_westfall90s <- as.data.frame(grisNV_westfall90s, xy = TRUE)
grisNV_westfall90s <- grisNV_westfall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(grisNV_westfall90s) <- c("x","y","Season","Decade","NV")

grisNV_westwinter00s <- raster("Extract_NVgriseus_westwinter00s.tif")
grisNV_westwinter00s <- as.data.frame(grisNV_westwinter00s, xy = TRUE)
grisNV_westwinter00s <- grisNV_westwinter00s %>%
  add_column(Season = "Winter",
             Decade = "00s",
             .after = "y")
colnames(grisNV_westwinter00s) <- c("x","y","Season","Decade","NV")

grisNV_westspring90s <- raster("Extract_NVgriseus_westspring90s.tif")
grisNV_westspring90s <- as.data.frame(grisNV_westspring90s, xy = TRUE)
grisNV_westspring90s <- grisNV_westspring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(grisNV_westspring90s) <- c("x","y","Season","Decade","NV")

grisNV_westsummer90s <- raster("Extract_NVgriseus_westsummer90s.tif")
grisNV_westsummer90s <- as.data.frame(grisNV_westsummer90s, xy = TRUE)
grisNV_westsummer90s <- grisNV_westsummer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(grisNV_westsummer90s) <- c("x","y","Season","Decade","NV")

grisNV_westsummer00s <- raster("Extract_NVgriseus_westsummer00s.tif")
grisNV_westsummer00s <- as.data.frame(grisNV_westsummer00s, xy = TRUE)
grisNV_westsummer00s <- grisNV_westsummer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(grisNV_westsummer00s) <- c("x","y","Season","Decade","NV")

grisNV_westsummer10s <- raster("Extract_NVgriseus_westsummer10s.tif")
grisNV_westsummer10s <- as.data.frame(grisNV_westsummer10s, xy = TRUE)
grisNV_westsummer10s <- grisNV_westsummer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(grisNV_westsummer10s) <- c("x","y","Season","Decade","NV")

# Northern current velocity nigripes ----

nigNV_fall90s <- raster("Extract_NVnigripes_fall90s.tif")
nigNV_fall90s <- as.data.frame(nigNV_fall90s, xy = TRUE)
nigNV_fall90s <- nigNV_fall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(nigNV_fall90s) <- c("x","y","Season","Decade","NV")

nigNV_fall10s <- raster("Extract_NVnigripes_fall10s.tif")
nigNV_fall10s <- as.data.frame(nigNV_fall10s, xy = TRUE)
nigNV_fall10s <- nigNV_fall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(nigNV_fall10s) <- c("x","y","Season","Decade","NV")

nigNV_spring90s <- raster("Extract_NVnigripes_spring90s.tif")
nigNV_spring90s <- as.data.frame(nigNV_spring90s, xy = TRUE)
nigNV_spring90s <- nigNV_spring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(nigNV_spring90s) <- c("x","y","Season","Decade","NV")

nigNV_summer90s <- raster("Extract_NVnigripes_summer90s.tif")
nigNV_summer90s <- as.data.frame(nigNV_summer90s, xy = TRUE)
nigNV_summer90s <- nigNV_summer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(nigNV_summer90s) <- c("x","y","Season","Decade","NV")

nigNV_summer00s <- raster("Extract_NVnigripes_summer00s.tif")
nigNV_summer00s <- as.data.frame(nigNV_summer00s, xy = TRUE)
nigNV_summer00s <- nigNV_summer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(nigNV_summer00s) <- c("x","y","Season","Decade","NV")

nigNV_summer10s <- raster("Extract_NVnigripes_summer10s.tif")
nigNV_summer10s <- as.data.frame(nigNV_summer10s, xy = TRUE)
nigNV_summer10s <- nigNV_summer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(nigNV_summer10s) <- c("x","y","Season","Decade","NV")

# Northern current velocity immer ----

imNV_fall90s <- raster("Extract_NVimmer_fall90s.tif")
imNV_fall90s <- as.data.frame(imNV_fall90s, xy = TRUE)
imNV_fall90s <- imNV_fall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(imNV_fall90s) <- c("x","y","Season","Decade","NV")

imNV_fall00s <- raster("Extract_NVimmer_fall00s.tif")
imNV_fall00s <- as.data.frame(imNV_fall00s, xy = TRUE)
imNV_fall00s <- imNV_fall00s %>%
  add_column(Season = "Fall",
             Decade = "00s",
             .after = "y")
colnames(imNV_fall00s) <- c("x","y","Season","Decade","NV")

imNV_fall10s <- raster("Extract_NVimmer_fall10s.tif")
imNV_fall10s <- as.data.frame(imNV_fall10s, xy = TRUE)
imNV_fall10s <- imNV_fall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(imNV_fall10s) <- c("x","y","Season","Decade","NV")

imNV_winter90s <- raster("Extract_NVimmer_winter90s.tif")
imNV_winter90s <- as.data.frame(imNV_winter90s, xy = TRUE)
imNV_winter90s <- imNV_winter90s %>%
  add_column(Season = "Winter",
             Decade = "90s",
             .after = "y")
colnames(imNV_winter90s) <- c("x","y","Season","Decade","NV")

imNV_winter00s <- raster("Extract_NVimmer_winter00s.tif")
imNV_winter00s <- as.data.frame(imNV_winter00s, xy = TRUE)
imNV_winter00s <- imNV_winter00s %>%
  add_column(Season = "Winter",
             Decade = "00s",
             .after = "y")
colnames(imNV_winter00s) <- c("x","y","Season","Decade","NV")

imNV_winter10s <- raster("Extract_NVimmer_winter10s.tif")
imNV_winter10s <- as.data.frame(imNV_winter10s, xy = TRUE)
imNV_winter10s <- imNV_winter10s %>%
  add_column(Season = "Winter",
             Decade = "10s",
             .after = "y")
colnames(imNV_winter10s) <- c("x","y","Season","Decade","NV")

imNV_spring90s <- raster("Extract_NVimmer_spring90s.tif")
imNV_spring90s <- as.data.frame(imNV_spring90s, xy = TRUE)
imNV_spring90s <- imNV_spring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(imNV_spring90s) <- c("x","y","Season","Decade","NV")

imNV_spring00s <- raster("Extract_NVimmer_spring00s.tif")
imNV_spring00s <- as.data.frame(imNV_spring00s, xy = TRUE)
imNV_spring00s <- imNV_spring00s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(imNV_spring00s) <- c("x","y","Season","Decade","NV")

imNV_spring10s <- raster("Extract_NVimmer_spring10s.tif")
imNV_spring10s <- as.data.frame(imNV_spring10s, xy = TRUE)
imNV_spring10s <- imNV_spring10s %>%
  add_column(Season = "Spring",
             Decade = "10s",
             .after = "y")
colnames(imNV_spring10s) <- c("x","y","Season","Decade","NV")

imNV_summer00s <- raster("Extract_NVimmer_summer00s.tif")
imNV_summer00s <- as.data.frame(imNV_summer00s, xy = TRUE)
imNV_summer00s <- imNV_summer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(imNV_summer00s) <- c("x","y","Season","Decade","NV")

imNV_summer10s <- raster("Extract_NVimmer_summer10s.tif")
imNV_summer10s <- as.data.frame(imNV_summer10s, xy = TRUE)
imNV_summer10s <- imNV_summer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(imNV_summer10s) <- c("x","y","Season","Decade","NV")

# Salinity P. griseus east ----------------
grisSa_eastfall90s <- raster("Extract_Sagriseus_eastfall90s.tif")
grisSa_eastfall90s <- as.data.frame(grisSa_eastfall90s, xy = TRUE)
grisSa_eastfall90s <- grisSa_eastfall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(grisSa_eastfall90s) <- c("x","y","Season","Decade","Sa")

grisSa_eastfall00s <- raster("Extract_Sagriseus_eastfall00s.tif")
grisSa_eastfall00s <- as.data.frame(grisSa_eastfall00s, xy = TRUE)
grisSa_eastfall00s <- grisSa_eastfall00s %>%
  add_column(Season = "Fall",
             Decade = "00s",
             .after = "y")
colnames(grisSa_eastfall00s) <- c("x","y","Season","Decade","Sa")

grisSa_eastfall10s <- raster("Extract_Sagriseus_eastfall10s.tif")
grisSa_eastfall10s <- as.data.frame(grisSa_eastfall10s, xy = TRUE)
grisSa_eastfall10s <- grisSa_eastfall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(grisSa_eastfall10s) <- c("x","y","Season","Decade","Sa")

grisSa_eastwinter10s <- raster("Extract_Sagriseus_eastwinter10s.tif")
grisSa_eastwinter10s <- as.data.frame(grisSa_eastwinter10s, xy = TRUE)
grisSa_eastwinter10s <- grisSa_eastwinter10s %>%
  add_column(Season = "Winter",
             Decade = "10s",
             .after = "y")
colnames(grisSa_eastwinter10s) <- c("x","y","Season","Decade","Sa")

grisSa_eastspring90s <- raster("Extract_Sagriseus_eastspring90s.tif")
grisSa_eastspring90s <- as.data.frame(grisSa_eastspring90s, xy = TRUE)
grisSa_eastspring90s <- grisSa_eastspring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(grisSa_eastspring90s) <- c("x","y","Season","Decade","Sa")

grisSa_eastspring00s <- raster("Extract_Sagriseus_eastspring00s.tif")
grisSa_eastspring00s <- as.data.frame(grisSa_eastspring00s, xy = TRUE)
grisSa_eastspring00s <- grisSa_eastspring00s %>%
  add_column(Season = "Spring",
             Decade = "00s",
             .after = "y")
colnames(grisSa_eastspring00s) <- c("x","y","Season","Decade","Sa")

grisSa_eastspring10s <- raster("Extract_Sagriseus_eastspring10s.tif")
grisSa_eastspring10s <- as.data.frame(grisSa_eastspring10s, xy = TRUE)
grisSa_eastspring10s <- grisSa_eastspring10s %>%
  add_column(Season = "Spring",
             Decade = "10s",
             .after = "y")
colnames(grisSa_eastspring10s) <- c("x","y","Season","Decade","Sa")

grisSa_eastsummer90s <- raster("Extract_Sagriseus_eastsummer90s.tif")
grisSa_eastsummer90s <- as.data.frame(grisSa_eastsummer90s, xy = TRUE)
grisSa_eastsummer90s <- grisSa_eastsummer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(grisSa_eastsummer90s) <- c("x","y","Season","Decade","Sa")

grisSa_eastsummer00s <- raster("Extract_Sagriseus_eastsummer00s.tif")
grisSa_eastsummer00s <- as.data.frame(grisSa_eastsummer00s, xy = TRUE)
grisSa_eastsummer00s <- grisSa_eastsummer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(grisSa_eastsummer00s) <- c("x","y","Season","Decade","Sa")

grisSa_eastsummer10s <- raster("Extract_Sagriseus_eastsummer10s.tif")
grisSa_eastsummer10s <- as.data.frame(grisSa_eastsummer10s, xy = TRUE)
grisSa_eastsummer10s <- grisSa_eastsummer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(grisSa_eastsummer10s) <- c("x","y","Season","Decade","Sa")

# Salinity P. griseus west -----

grisSa_westfall90s <- raster("Extract_Sagriseus_westfall90s.tif")
grisSa_westfall90s <- as.data.frame(grisSa_westfall90s, xy = TRUE)
grisSa_westfall90s <- grisSa_westfall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(grisSa_westfall90s) <- c("x","y","Season","Decade","Sa")

grisSa_westwinter00s <- raster("Extract_Sagriseus_westwinter00s.tif")
grisSa_westwinter00s <- as.data.frame(grisSa_westwinter00s, xy = TRUE)
grisSa_westwinter00s <- grisSa_westwinter00s %>%
  add_column(Season = "Winter",
             Decade = "00s",
             .after = "y")
colnames(grisSa_westwinter00s) <- c("x","y","Season","Decade","Sa")

grisSa_westspring90s <- raster("Extract_Sagriseus_westspring90s.tif")
grisSa_westspring90s <- as.data.frame(grisSa_westspring90s, xy = TRUE)
grisSa_westspring90s <- grisSa_westspring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(grisSa_westspring90s) <- c("x","y","Season","Decade","Sa")

grisSa_westsummer90s <- raster("Extract_Sagriseus_westsummer90s.tif")
grisSa_westsummer90s <- as.data.frame(grisSa_westsummer90s, xy = TRUE)
grisSa_westsummer90s <- grisSa_westsummer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(grisSa_westsummer90s) <- c("x","y","Season","Decade","Sa")

grisSa_westsummer00s <- raster("Extract_Sagriseus_westsummer00s.tif")
grisSa_westsummer00s <- as.data.frame(grisSa_westsummer00s, xy = TRUE)
grisSa_westsummer00s <- grisSa_westsummer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(grisSa_westsummer00s) <- c("x","y","Season","Decade","Sa")

grisSa_westsummer10s <- raster("Extract_Sagriseus_westsummer10s.tif")
grisSa_westsummer10s <- as.data.frame(grisSa_westsummer10s, xy = TRUE)
grisSa_westsummer10s <- grisSa_westsummer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(grisSa_westsummer10s) <- c("x","y","Season","Decade","Sa")

# Salinity nigripes -----

nigSa_fall90s <- raster("Extract_Sanigripes_fall90s.tif")
nigSa_fall90s <- as.data.frame(nigSa_fall90s, xy = TRUE)
nigSa_fall90s <- nigSa_fall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(nigSa_fall90s) <- c("x","y","Season","Decade","Sa")

nigSa_fall10s <- raster("Extract_Sanigripes_fall10s.tif")
nigSa_fall10s <- as.data.frame(nigSa_fall10s, xy = TRUE)
nigSa_fall10s <- nigSa_fall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(nigSa_fall10s) <- c("x","y","Season","Decade","Sa")

nigSa_spring90s <- raster("Extract_Sanigripes_spring90s.tif")
nigSa_spring90s <- as.data.frame(nigSa_spring90s, xy = TRUE)
nigSa_spring90s <- nigSa_spring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(nigSa_spring90s) <- c("x","y","Season","Decade","Sa")

nigSa_summer90s <- raster("Extract_Sanigripes_summer90s.tif")
nigSa_summer90s <- as.data.frame(nigSa_summer90s, xy = TRUE)
nigSa_summer90s <- nigSa_summer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(nigSa_summer90s) <- c("x","y","Season","Decade","Sa")

nigSa_summer00s <- raster("Extract_Sanigripes_summer00s.tif")
nigSa_summer00s <- as.data.frame(nigSa_summer00s, xy = TRUE)
nigSa_summer00s <- nigSa_summer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(nigSa_summer00s) <- c("x","y","Season","Decade","Sa")

nigSa_summer10s <- raster("Extract_Sanigripes_summer10s.tif")
nigSa_summer10s <- as.data.frame(nigSa_summer10s, xy = TRUE)
nigSa_summer10s <- nigSa_summer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(nigSa_summer10s) <- c("x","y","Season","Decade","Sa")

# Salinity immer -----

imSa_fall90s <- raster("Extract_Saimmer_fall90s.tif")
imSa_fall90s <- as.data.frame(imSa_fall90s, xy = TRUE)
imSa_fall90s <- imSa_fall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(imSa_fall90s) <- c("x","y","Season","Decade","Sa")

imSa_fall00s <- raster("Extract_Saimmer_fall00s.tif")
imSa_fall00s <- as.data.frame(imSa_fall00s, xy = TRUE)
imSa_fall00s <- imSa_fall00s %>%
  add_column(Season = "Fall",
             Decade = "00s",
             .after = "y")
colnames(imSa_fall00s) <- c("x","y","Season","Decade","Sa")

imSa_fall10s <- raster("Extract_Saimmer_fall10s.tif")
imSa_fall10s <- as.data.frame(imSa_fall10s, xy = TRUE)
imSa_fall10s <- imSa_fall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(imSa_fall10s) <- c("x","y","Season","Decade","Sa")

imSa_winter90s <- raster("Extract_Saimmer_winter90s.tif")
imSa_winter90s <- as.data.frame(imSa_winter90s, xy = TRUE)
imSa_winter90s <- imSa_winter90s %>%
  add_column(Season = "Winter",
             Decade = "90s",
             .after = "y")
colnames(imSa_winter90s) <- c("x","y","Season","Decade","Sa")

imSa_winter00s <- raster("Extract_Saimmer_winter00s.tif")
imSa_winter00s <- as.data.frame(imSa_winter00s, xy = TRUE)
imSa_winter00s <- imSa_winter00s %>%
  add_column(Season = "Winter",
             Decade = "00s",
             .after = "y")
colnames(imSa_winter00s) <- c("x","y","Season","Decade","Sa")

imSa_winter10s <- raster("Extract_Saimmer_winter10s.tif")
imSa_winter10s <- as.data.frame(imSa_winter10s, xy = TRUE)
imSa_winter10s <- imSa_winter10s %>%
  add_column(Season = "Winter",
             Decade = "10s",
             .after = "y")
colnames(imSa_winter10s) <- c("x","y","Season","Decade","Sa")

imSa_spring90s <- raster("Extract_Saimmer_spring90s.tif")
imSa_spring90s <- as.data.frame(imSa_spring90s, xy = TRUE)
imSa_spring90s <- imSa_spring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(imSa_spring90s) <- c("x","y","Season","Decade","Sa")

imSa_spring00s <- raster("Extract_Saimmer_spring00s.tif")
imSa_spring00s <- as.data.frame(imSa_spring00s, xy = TRUE)
imSa_spring00s <- imSa_spring00s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(imSa_spring00s) <- c("x","y","Season","Decade","Sa")

imSa_spring10s <- raster("Extract_Saimmer_spring10s.tif")
imSa_spring10s <- as.data.frame(imSa_spring10s, xy = TRUE)
imSa_spring10s <- imSa_spring10s %>%
  add_column(Season = "Spring",
             Decade = "10s",
             .after = "y")
colnames(imSa_spring10s) <- c("x","y","Season","Decade","Sa")

imSa_summer00s <- raster("Extract_Saimmer_summer00s.tif")
imSa_summer00s <- as.data.frame(imSa_summer00s, xy = TRUE)
imSa_summer00s <- imSa_summer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(imSa_summer00s) <- c("x","y","Season","Decade","Sa")

imSa_summer10s <- raster("Extract_Saimmer_summer10s.tif")
imSa_summer10s <- as.data.frame(imSa_summer10s, xy = TRUE)
imSa_summer10s <- imSa_summer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(imSa_summer10s) <- c("x","y","Season","Decade","Sa")

# Sea surface temperature P. griseus east ------
grisSST_eastfall90s <- raster("Extract_SSTgriseus_eastfall90s.tif")
grisSST_eastfall90s <- as.data.frame(grisSST_eastfall90s, xy = TRUE)
grisSST_eastfall90s <- grisSST_eastfall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(grisSST_eastfall90s) <- c("x","y","Season","Decade","SST")

grisSST_eastfall00s <- raster("Extract_SSTgriseus_eastfall00s.tif")
grisSST_eastfall00s <- as.data.frame(grisSST_eastfall00s, xy = TRUE)
grisSST_eastfall00s <- grisSST_eastfall00s %>%
  add_column(Season = "Fall",
             Decade = "00s",
             .after = "y")
colnames(grisSST_eastfall00s) <- c("x","y","Season","Decade","SST")

grisSST_eastfall10s <- raster("Extract_SSTgriseus_eastfall10s.tif")
grisSST_eastfall10s <- as.data.frame(grisSST_eastfall10s, xy = TRUE)
grisSST_eastfall10s <- grisSST_eastfall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(grisSST_eastfall10s) <- c("x","y","Season","Decade","SST")

grisSST_eastwinter10s <- raster("Extract_SSTgriseus_eastwinter10s.tif")
grisSST_eastwinter10s <- as.data.frame(grisSST_eastwinter10s, xy = TRUE)
grisSST_eastwinter10s <- grisSST_eastwinter10s %>%
  add_column(Season = "Winter",
             Decade = "10s",
             .after = "y")
colnames(grisSST_eastwinter10s) <- c("x","y","Season","Decade","SST")

grisSST_eastspring90s <- raster("Extract_SSTgriseus_eastspring90s.tif")
grisSST_eastspring90s <- as.data.frame(grisSST_eastspring90s, xy = TRUE)
grisSST_eastspring90s <- grisSST_eastspring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(grisSST_eastspring90s) <- c("x","y","Season","Decade","SST")

grisSST_eastspring00s <- raster("Extract_SSTgriseus_eastspring00s.tif")
grisSST_eastspring00s <- as.data.frame(grisSST_eastspring00s, xy = TRUE)
grisSST_eastspring00s <- grisSST_eastspring00s %>%
  add_column(Season = "Spring",
             Decade = "00s",
             .after = "y")
colnames(grisSST_eastspring00s) <- c("x","y","Season","Decade","SST")

grisSST_eastspring10s <- raster("Extract_SSTgriseus_eastspring10s.tif")
grisSST_eastspring10s <- as.data.frame(grisSST_eastspring10s, xy = TRUE)
grisSST_eastspring10s <- grisSST_eastspring10s %>%
  add_column(Season = "Spring",
             Decade = "10s",
             .after = "y")
colnames(grisSST_eastspring10s) <- c("x","y","Season","Decade","SST")

grisSST_eastsummer90s <- raster("Extract_SSTgriseus_eastsummer90s.tif")
grisSST_eastsummer90s <- as.data.frame(grisSST_eastsummer90s, xy = TRUE)
grisSST_eastsummer90s <- grisSST_eastsummer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(grisSST_eastsummer90s) <- c("x","y","Season","Decade","SST")

grisSST_eastsummer00s <- raster("Extract_SSTgriseus_eastsummer00s.tif")
grisSST_eastsummer00s <- as.data.frame(grisSST_eastsummer00s, xy = TRUE)
grisSST_eastsummer00s <- grisSST_eastsummer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(grisSST_eastsummer00s) <- c("x","y","Season","Decade","SST")

grisSST_eastsummer10s <- raster("Extract_SSTgriseus_eastsummer10s.tif")
grisSST_eastsummer10s <- as.data.frame(grisSST_eastsummer10s, xy = TRUE)
grisSST_eastsummer10s <- grisSST_eastsummer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(grisSST_eastsummer10s) <- c("x","y","Season","Decade","SST")

# Sea surface temperature P. griseus west ----

grisSST_westfall90s <- raster("Extract_SSTgriseus_westfall90s.tif")
grisSST_westfall90s <- as.data.frame(grisSST_westfall90s, xy = TRUE)
grisSST_westfall90s <- grisSST_westfall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(grisSST_westfall90s) <- c("x","y","Season","Decade","SST")

grisSST_westwinter00s <- raster("Extract_SSTgriseus_westwinter00s.tif")
grisSST_westwinter00s <- as.data.frame(grisSST_westwinter00s, xy = TRUE)
grisSST_westwinter00s <- grisSST_westwinter00s %>%
  add_column(Season = "Winter",
             Decade = "00s",
             .after = "y")
colnames(grisSST_westwinter00s) <- c("x","y","Season","Decade","SST")

grisSST_westspring90s <- raster("Extract_SSTgriseus_westspring90s.tif")
grisSST_westspring90s <- as.data.frame(grisSST_westspring90s, xy = TRUE)
grisSST_westspring90s <- grisSST_westspring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(grisSST_westspring90s) <- c("x","y","Season","Decade","SST")

grisSST_westsummer90s <- raster("Extract_SSTgriseus_westsummer90s.tif")
grisSST_westsummer90s <- as.data.frame(grisSST_westsummer90s, xy = TRUE)
grisSST_westsummer90s <- grisSST_westsummer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(grisSST_westsummer90s) <- c("x","y","Season","Decade","SST")

grisSST_westsummer00s <- raster("Extract_SSTgriseus_westsummer00s.tif")
grisSST_westsummer00s <- as.data.frame(grisSST_westsummer00s, xy = TRUE)
grisSST_westsummer00s <- grisSST_westsummer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(grisSST_westsummer00s) <- c("x","y","Season","Decade","SST")

grisSST_westsummer10s <- raster("Extract_SSTgriseus_westsummer10s.tif")
grisSST_westsummer10s <- as.data.frame(grisSST_westsummer10s, xy = TRUE)
grisSST_westsummer10s <- grisSST_westsummer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(grisSST_westsummer10s) <- c("x","y","Season","Decade","SST")

# Sea surface temperature nigripes----

nigSST_fall90s <- raster("Extract_SSTnigripes_fall90s.tif")
nigSST_fall90s <- as.data.frame(nigSST_fall90s, xy = TRUE)
nigSST_fall90s <- nigSST_fall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(nigSST_fall90s) <- c("x","y","Season","Decade","SST")

nigSST_fall10s <- raster("Extract_SSTnigripes_fall10s.tif")
nigSST_fall10s <- as.data.frame(nigSST_fall10s, xy = TRUE)
nigSST_fall10s <- nigSST_fall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(nigSST_fall10s) <- c("x","y","Season","Decade","SST")

nigSST_spring90s <- raster("Extract_SSTnigripes_spring90s.tif")
nigSST_spring90s <- as.data.frame(nigSST_spring90s, xy = TRUE)
nigSST_spring90s <- nigSST_spring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(nigSST_spring90s) <- c("x","y","Season","Decade","SST")

nigSST_summer90s <- raster("Extract_SSTnigripes_summer90s.tif")
nigSST_summer90s <- as.data.frame(nigSST_summer90s, xy = TRUE)
nigSST_summer90s <- nigSST_summer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(nigSST_summer90s) <- c("x","y","Season","Decade","SST")

nigSST_summer00s <- raster("Extract_SSTnigripes_summer00s.tif")
nigSST_summer00s <- as.data.frame(nigSST_summer00s, xy = TRUE)
nigSST_summer00s <- nigSST_summer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(nigSST_summer00s) <- c("x","y","Season","Decade","SST")

nigSST_summer10s <- raster("Extract_SSTnigripes_summer10s.tif")
nigSST_summer10s <- as.data.frame(nigSST_summer10s, xy = TRUE)
nigSST_summer10s <- nigSST_summer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(nigSST_summer10s) <- c("x","y","Season","Decade","SST")

# Sea surface temperature immer ----

imSST_fall90s <- raster("Extract_SSTimmer_fall90s.tif")
imSST_fall90s <- as.data.frame(imSST_fall90s, xy = TRUE)
imSST_fall90s <- imSST_fall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(imSST_fall90s) <- c("x","y","Season","Decade","SST")

imSST_fall00s <- raster("Extract_SSTimmer_fall00s.tif")
imSST_fall00s <- as.data.frame(imSST_fall00s, xy = TRUE)
imSST_fall00s <- imSST_fall00s %>%
  add_column(Season = "Fall",
             Decade = "00s",
             .after = "y")
colnames(imSST_fall00s) <- c("x","y","Season","Decade","SST")

imSST_fall10s <- raster("Extract_SSTimmer_fall10s.tif")
imSST_fall10s <- as.data.frame(imSST_fall10s, xy = TRUE)
imSST_fall10s <- imSST_fall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(imSST_fall10s) <- c("x","y","Season","Decade","SST")

imSST_winter90s <- raster("Extract_SSTimmer_winter90s.tif")
imSST_winter90s <- as.data.frame(imSST_winter90s, xy = TRUE)
imSST_winter90s <- imSST_winter90s %>%
  add_column(Season = "Winter",
             Decade = "90s",
             .after = "y")
colnames(imSST_winter90s) <- c("x","y","Season","Decade","SST")

imSST_winter00s <- raster("Extract_SSTimmer_winter00s.tif")
imSST_winter00s <- as.data.frame(imSST_winter00s, xy = TRUE)
imSST_winter00s <- imSST_winter00s %>%
  add_column(Season = "Winter",
             Decade = "00s",
             .after = "y")
colnames(imSST_winter00s) <- c("x","y","Season","Decade","SST")

imSST_winter10s <- raster("Extract_SSTimmer_winter10.tif")
imSST_winter10s <- as.data.frame(imSST_winter10s, xy = TRUE)
imSST_winter10s <- imSST_winter10s %>%
  add_column(Season = "Winter",
             Decade = "10s",
             .after = "y")
colnames(imSST_winter10s) <- c("x","y","Season","Decade","SST")

imSST_spring90s <- raster("Extract_SSTimmer_spring90s.tif")
imSST_spring90s <- as.data.frame(imSST_spring90s, xy = TRUE)
imSST_spring90s <- imSST_spring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(imSST_spring90s) <- c("x","y","Season","Decade","SST")

imSST_spring00s <- raster("Extract_SSTimmer_spring00.tif")
imSST_spring00s <- as.data.frame(imSST_spring00s, xy = TRUE)
imSST_spring00s <- imSST_spring00s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(imSST_spring00s) <- c("x","y","Season","Decade","SST")

imSST_spring10s <- raster("Extract_SSTimmer_spring10.tif")
imSST_spring10s <- as.data.frame(imSST_spring10s, xy = TRUE)
imSST_spring10s <- imSST_spring10s %>%
  add_column(Season = "Spring",
             Decade = "10s",
             .after = "y")
colnames(imSST_spring10s) <- c("x","y","Season","Decade","SST")

imSST_summer00s <- raster("Extract_SSTimmer_summer00.tif")
imSST_summer00s <- as.data.frame(imSST_summer00s, xy = TRUE)
imSST_summer00s <- imSST_summer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(imSST_summer00s) <- c("x","y","Season","Decade","SST")

imSST_summer10s <- raster("Extract_SSTimmer_summer10.tif")
imSST_summer10s <- as.data.frame(imSST_summer10s, xy = TRUE)
imSST_summer10s <- imSST_summer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(imSST_summer10s) <- c("x","y","Season","Decade","SST")

# Sea surface height P. griseus east ----

grisSSH_eastfall90s <- raster("Extract_SSHgriseus_eastfall90s.tif")
grisSSH_eastfall90s <- as.data.frame(grisSSH_eastfall90s, xy = TRUE)
grisSSH_eastfall90s <- grisSSH_eastfall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(grisSSH_eastfall90s) <- c("x","y","Season","Decade","SSH")

grisSSH_eastfall00s <- raster("Extract_SSHgriseus_eastfall00s.tif")
grisSSH_eastfall00s <- as.data.frame(grisSSH_eastfall00s, xy = TRUE)
grisSSH_eastfall00s <- grisSSH_eastfall00s %>%
  add_column(Season = "Fall",
             Decade = "00s",
             .after = "y")
colnames(grisSSH_eastfall00s) <- c("x","y","Season","Decade","SSH")

grisSSH_eastfall10s <- raster("Extract_SSHgriseus_eastfall10s.tif")
grisSSH_eastfall10s <- as.data.frame(grisSSH_eastfall10s, xy = TRUE)
grisSSH_eastfall10s <- grisSSH_eastfall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(grisSSH_eastfall10s) <- c("x","y","Season","Decade","SSH")

grisSSH_eastwinter10s <- raster("Extract_SSHgriseus_eastwinter10s.tif")
grisSSH_eastwinter10s <- as.data.frame(grisSSH_eastwinter10s, xy = TRUE)
grisSSH_eastwinter10s <- grisSSH_eastwinter10s %>%
  add_column(Season = "Winter",
             Decade = "10s",
             .after = "y")
colnames(grisSSH_eastwinter10s) <- c("x","y","Season","Decade","SSH")

grisSSH_eastspring90s <- raster("Extract_SSHgriseus_eastspring90s.tif")
grisSSH_eastspring90s <- as.data.frame(grisSSH_eastspring90s, xy = TRUE)
grisSSH_eastspring90s <- grisSSH_eastspring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(grisSSH_eastspring90s) <- c("x","y","Season","Decade","SSH")

grisSSH_eastspring00s <- raster("Extract_SSHgriseus_eastspring00s.tif")
grisSSH_eastspring00s <- as.data.frame(grisSSH_eastspring00s, xy = TRUE)
grisSSH_eastspring00s <- grisSSH_eastspring00s %>%
  add_column(Season = "Spring",
             Decade = "00s",
             .after = "y")
colnames(grisSSH_eastspring00s) <- c("x","y","Season","Decade","SSH")

grisSSH_eastspring10s <- raster("Extract_SSHgriseus_eastspring10s.tif")
grisSSH_eastspring10s <- as.data.frame(grisSSH_eastspring10s, xy = TRUE)
grisSSH_eastspring10s <- grisSSH_eastspring10s %>%
  add_column(Season = "Spring",
             Decade = "10s",
             .after = "y")
colnames(grisSSH_eastspring10s) <- c("x","y","Season","Decade","SSH")

grisSSH_eastsummer90s <- raster("Extract_SSHgriseus_eastsummer90s.tif")
grisSSH_eastsummer90s <- as.data.frame(grisSSH_eastsummer90s, xy = TRUE)
grisSSH_eastsummer90s <- grisSSH_eastsummer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(grisSSH_eastsummer90s) <- c("x","y","Season","Decade","SSH")

grisSSH_eastsummer00s <- raster("Extract_SSHgriseus_eastsummer00s.tif")
grisSSH_eastsummer00s <- as.data.frame(grisSSH_eastsummer00s, xy = TRUE)
grisSSH_eastsummer00s <- grisSSH_eastsummer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(grisSSH_eastsummer00s) <- c("x","y","Season","Decade","SSH")

grisSSH_eastsummer10s <- raster("Extract_SSHgriseus_eastsummer10s.tif")
grisSSH_eastsummer10s <- as.data.frame(grisSSH_eastsummer10s, xy = TRUE)
grisSSH_eastsummer10s <- grisSSH_eastsummer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(grisSSH_eastsummer10s) <- c("x","y","Season","Decade","SSH")

# Sea surface height P. griseus west ----

grisSSH_westfall90s <- raster("Extract_SSHgriseus_westfall90s.tif")
grisSSH_westfall90s <- as.data.frame(grisSSH_westfall90s, xy = TRUE)
grisSSH_westfall90s <- grisSSH_westfall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(grisSSH_westfall90s) <- c("x","y","Season","Decade","SSH")

grisSSH_westwinter00s <- raster("Extract_SSHgriseus_westwinter00s.tif")
grisSSH_westwinter00s <- as.data.frame(grisSSH_westwinter00s, xy = TRUE)
grisSSH_westwinter00s <- grisSSH_westwinter00s %>%
  add_column(Season = "Winter",
             Decade = "00s",
             .after = "y")
colnames(grisSSH_westwinter00s) <- c("x","y","Season","Decade","SSH")

grisSSH_westspring90s <- raster("Extract_SSHgriseus_westspring90s.tif")
grisSSH_westspring90s <- as.data.frame(grisSSH_westspring90s, xy = TRUE)
grisSSH_westspring90s <- grisSSH_westspring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(grisSSH_westspring90s) <- c("x","y","Season","Decade","SSH")

grisSSH_westsummer90s <- raster("Extract_SSHgriseus_westsummer90s.tif")
grisSSH_westsummer90s <- as.data.frame(grisSSH_westsummer90s, xy = TRUE)
grisSSH_westsummer90s <- grisSSH_westsummer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(grisSSH_westsummer90s) <- c("x","y","Season","Decade","SSH")

grisSSH_westsummer00s <- raster("Extract_SSHgriseus_westsummer00s.tif")
grisSSH_westsummer00s <- as.data.frame(grisSSH_westsummer00s, xy = TRUE)
grisSSH_westsummer00s <- grisSSH_westsummer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(grisSSH_westsummer00s) <- c("x","y","Season","Decade","SSH")

grisSSH_westsummer10s <- raster("Extract_SSHgriseus_westsummer10s.tif")
grisSSH_westsummer10s <- as.data.frame(grisSSH_westsummer10s, xy = TRUE)
grisSSH_westsummer10s <- grisSSH_westsummer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(grisSSH_westsummer10s) <- c("x","y","Season","Decade","SSH")

# Sea surface height nigripes ----

nigSSH_fall90s <- raster("Extract_SSHnigripes_fall90s.tif")
nigSSH_fall90s <- as.data.frame(nigSSH_fall90s, xy = TRUE)
nigSSH_fall90s <- nigSSH_fall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(nigSSH_fall90s) <- c("x","y","Season","Decade","SSH")

nigSSH_fall10s <- raster("Extract_SSHnigripes_fall10s.tif")
nigSSH_fall10s <- as.data.frame(nigSSH_fall10s, xy = TRUE)
nigSSH_fall10s <- nigSSH_fall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(nigSSH_fall10s) <- c("x","y","Season","Decade","SSH")

nigSSH_spring90s <- raster("Extract_SSHnigripes_spring90s.tif")
nigSSH_spring90s <- as.data.frame(nigSSH_spring90s, xy = TRUE)
nigSSH_spring90s <- nigSSH_spring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(nigSSH_spring90s) <- c("x","y","Season","Decade","SSH")

nigSSH_summer90s <- raster("Extract_SSHnigripes_summer90s.tif")
nigSSH_summer90s <- as.data.frame(nigSSH_summer90s, xy = TRUE)
nigSSH_summer90s <- nigSSH_summer90s %>%
  add_column(Season = "Summer",
             Decade = "90s",
             .after = "y")
colnames(nigSSH_summer90s) <- c("x","y","Season","Decade","SSH")

nigSSH_summer00s <- raster("Extract_SSHnigripes_summer00s.tif")
nigSSH_summer00s <- as.data.frame(nigSSH_summer00s, xy = TRUE)
nigSSH_summer00s <- nigSSH_summer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(nigSSH_summer00s) <- c("x","y","Season","Decade","SSH")

nigSSH_summer10s <- raster("Extract_SSHnigripes_summer10s.tif")
nigSSH_summer10s <- as.data.frame(nigSSH_summer10s, xy = TRUE)
nigSSH_summer10s <- nigSSH_summer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(nigSSH_summer10s) <- c("x","y","Season","Decade","SSH")

# Sea surface height immer ----

imSSH_fall90s <- raster("Extract_SSHimmer_fall90s.tif")
imSSH_fall90s <- as.data.frame(imSSH_fall90s, xy = TRUE)
imSSH_fall90s <- imSSH_fall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")
colnames(imSSH_fall90s) <- c("x","y","Season","Decade","SSH")

imSSH_fall00s <- raster("Extract_SSHimmer_fall00s.tif")
imSSH_fall00s <- as.data.frame(imSSH_fall00s, xy = TRUE)
imSSH_fall00s <- imSSH_fall00s %>%
  add_column(Season = "Fall",
             Decade = "00s",
             .after = "y")
colnames(imSSH_fall00s) <- c("x","y","Season","Decade","SSH")

imSSH_fall10s <- raster("Extract_SSHimmer_fall10s.tif")
imSSH_fall10s <- as.data.frame(imSSH_fall10s, xy = TRUE)
imSSH_fall10s <- imSSH_fall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")
colnames(imSSH_fall10s) <- c("x","y","Season","Decade","SSH")

imSSH_winter90s <- raster("Extract_SSHimmer_winter90s.tif")
imSSH_winter90s <- as.data.frame(imSSH_winter90s, xy = TRUE)
imSSH_winter90s <- imSSH_winter90s %>%
  add_column(Season = "Winter",
             Decade = "90s",
             .after = "y")
colnames(imSSH_winter90s) <- c("x","y","Season","Decade","SSH")

imSSH_winter00s <- raster("Extract_SSHimmer_winter00s.tif")
imSSH_winter00s <- as.data.frame(imSSH_winter00s, xy = TRUE)
imSSH_winter00s <- imSSH_winter00s %>%
  add_column(Season = "Winter",
             Decade = "00s",
             .after = "y")
colnames(imSSH_winter00s) <- c("x","y","Season","Decade","SSH")

imSSH_winter10s <- raster("Extract_SSHimmer_winter10.tif")
imSSH_winter10s <- as.data.frame(imSSH_winter10s, xy = TRUE)
imSSH_winter10s <- imSSH_winter10s %>%
  add_column(Season = "Winter",
             Decade = "10s",
             .after = "y")
colnames(imSSH_winter10s) <- c("x","y","Season","Decade","SSH")

imSSH_spring90s <- raster("Extract_SSHimmer_spring90.tif")
imSSH_spring90s <- as.data.frame(imSSH_spring90s, xy = TRUE)
imSSH_spring90s <- imSSH_spring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(imSSH_spring90s) <- c("x","y","Season","Decade","SSH")

imSSH_spring00s <- raster("Extract_SSHimmer_spring00.tif")
imSSH_spring00s <- as.data.frame(imSSH_spring00s, xy = TRUE)
imSSH_spring00s <- imSSH_spring00s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")
colnames(imSSH_spring00s) <- c("x","y","Season","Decade","SSH")

imSSH_spring10s <- raster("Extract_SSHimmer_spring10.tif")
imSSH_spring10s <- as.data.frame(imSSH_spring10s, xy = TRUE)
imSSH_spring10s <- imSSH_spring10s %>%
  add_column(Season = "Spring",
             Decade = "10s",
             .after = "y")
colnames(imSSH_spring10s) <- c("x","y","Season","Decade","SSH")

imSSH_summer00s <- raster("Extract_SSHimmer_summer00.tif")
imSSH_summer00s <- as.data.frame(imSSH_summer00s, xy = TRUE)
imSSH_summer00s <- imSSH_summer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")
colnames(imSSH_summer00s) <- c("x","y","Season","Decade","SSH")

imSSH_summer10s <- raster("Extract_SSHimmer_summer10.tif")
imSSH_summer10s <- as.data.frame(imSSH_summer10s, xy = TRUE)
imSSH_summer10s <- imSSH_summer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")
colnames(imSSH_summer10s) <- c("x","y","Season","Decade","SSH")

# Import summary data THHAT I THINK IS NOW IRRELEVANT------
# immerSummary    <- read.csv("C:/Users/savan/Documents/github/AvesOBIS/immerSummary.csv")
# immerSummary    <- immerSummary %>% 
#   column_to_rownames(var="X")
# nigSummary      <- read.csv("C:/Users/savan/Documents/github/AvesOBIS/nigSummary.csv")
# nigSummary      <- nigSummary %>% 
#   column_to_rownames(var="X")
# grisWestSummary <- read.csv("C:/Users/savan/Documents/github/AvesOBIS/grisWestSummary.csv")
# grisWestSummary    <- grisWestSummary %>% 
#   column_to_rownames(var="X")
# grisEastSummary <- read.csv("C:/Users/savan/Documents/github/AvesOBIS/grisEastSummary.csv")
# grisEastSummary    <- grisEastSummary %>% 
#   column_to_rownames(var="X")
# 
# # Parse out the lat/lon and env characteristics and place them in data frames:
# grisWestEnv     <- grisWestSummary[,3:ncol(grisWestSummary)]    # Enviro variables
# grisWestLatLon  <- grisWestSummary[,1:2]                        # Lat/long variables
# grisEastEnv     <- grisEastSummary[,3:ncol(grisEastSummary)]
# grisEastLatLon  <- grisEastSummary[,1:2]
# nigEnv          <- nigSummary[,3:ncol(nigSummary)]
# nigLatLon       <- nigSummary[,1:2]
# immerEnv        <- immerSummary[,3:ncol(immerSummary)]
# immerLatLon     <- immerSummary[,1:2]

# Visually investigate the data standardization.
# nigripes ------------------------------------------------------------------

#bind_rows together all data frames

nigEV <- bind_rows(nigEV_fall90s,nigEV_fall10s)%>%
  bind_rows(nigEV_spring90s)%>%
  bind_rows(nigEV_summer90s)%>%
  bind_rows(nigEV_summer00s)%>%
  bind_rows(nigEV_summer10s)

nigNV <- bind_rows(nigNV_fall90s,nigNV_fall10s)%>%
  bind_rows(nigNV_spring90s)%>%
  bind_rows(nigNV_summer90s)%>%
  bind_rows(nigNV_summer00s)%>%
  bind_rows(nigNV_summer10s)

nigML <- bind_rows(nigML_fall90s,nigML_fall10s)%>%
  bind_rows(nigML_spring90s)%>%
  bind_rows(nigML_summer90s)%>%
  bind_rows(nigML_summer00s)%>%
  bind_rows(nigML_summer10s)

nigSa <- bind_rows(nigSa_fall90s,nigSa_fall10s)%>%
  bind_rows(nigSa_spring90s)%>%
  bind_rows(nigSa_summer90s)%>%
  bind_rows(nigSa_summer00s)%>%
  bind_rows(nigSa_summer10s)

nigSST <- bind_rows(nigSST_fall90s,nigSST_fall10s)%>%
  bind_rows(nigSST_spring90s)%>%
  bind_rows(nigSST_summer90s)%>%
  bind_rows(nigSST_summer00s)%>%
  bind_rows(nigSST_summer10s)

nigSSH <- bind_rows(nigSSH_fall90s,nigSSH_fall10s)%>%
  bind_rows(nigSSH_spring90s)%>%
  bind_rows(nigSSH_summer90s)%>%
  bind_rows(nigSSH_summer00s)%>%
  bind_rows(nigSSH_summer10s)

nig <- left_join(nigSa,nigML)%>%
  left_join(nigSSH)%>%
  left_join(nigSST)%>%
  left_join(nigEV)%>%
  left_join(nigNV)
nig[is.na(nig)] <- 0

nig <- nig %>%
  filter(Sa > 0)

nigLatLon <- nig[,1:2]
nigEnv    <- nig[,5:ncol(nig)]

# Use an AIC-based stepwise variable selection procedure with a RDA approach
# to build an optimal (most parsimonious) model depicting the relationship
# between the fish community and the set of associated environmental variables.

# Create the "null" and "full" models described above:
null      <- rda(nigLatLon ~ 1, nigEnv)
full      <- rda(nigLatLon ~ ., nigEnv)

# Now perform the AIC stepwise selection process:
optNig  <- step(object = null, scope = formula(full)) #optimal model selected by AIC selection

# Perform RDA and plot ------------------------------------------------------------

permutest(x = optNig, permutations = 9999) 
# very significant P value (Pr(>F))
# model is F ratio 304.21 (304x more variability explained than by random chance)

# Perform RDA:
rdaNig        <- rda(X = nigLatLon, Y = nigEnv)

# Produce and display the summary results:
optNig_sum    <- summary(rdaNig)
optNig_sum

# Create the ordination diagram.

# Extract the percent of the variability explained by each axis:
pct_explNig  <- 100 * optNig_sum$cont$importance[2,]

scores_Nig <- scores(optNig)
dat <- cbind(nig, scores_Nig$sites)

ggplot()+
  geom_point(data = dat, aes(x = RDA1,
                             y = RDA2,
                             color = Decade,
                             shape = Season)) +
  labs(title = "Redundancy Analysis for P. nigripes",
       x = paste("RDA1 (variance explained: ",
                 round(pct_explNig[1], digits = 1),
                 "%)",
                 sep =""),
       y = paste("RDA2 (variance explained: ",
                 round(pct_explNig[2], digits = 1),
                 "%)",
                 sep =""))+
  geom_text(data = as.data.frame(scores(rdaNig)$species),
            aes(x = RDA1/optNig_sum$cont$importance[1,1], 
                y = RDA2/optNig_sum$cont$importance[1,2],
                label = c("Long","Lat")))+
  stat_ellipse(data = dat, aes(x = RDA1,
                               y = RDA2,
                               color = Decade),
               size = 1,
               level = 0.90,
               alpha = 0.65,
               show.legend = FALSE)+
  geom_segment(data = as.data.frame(optNig_sum$biplot),
               aes(x = 0,
                   y = 0, 
                   xend = RDA1,
                   yend = RDA2),
               arrow = arrow(length=unit(0.3, "cm")),
               color = "gray30")+
  geom_label(data = as.data.frame(optNig_sum$biplot),
             aes(x = RDA1,
                 y = RDA2,
                 label = rownames(optNig_sum$biplot)))

# immer ---------------

#bind_rows together all data frames

immerEV <- bind_rows(imEV_fall90s,imEV_fall00s)%>%
  bind_rows(imEV_fall10s)%>%
  bind_rows(imEV_winter90s)%>%
  bind_rows(imEV_winter00s)%>%
  bind_rows(imEV_winter10s)%>%
  bind_rows(imEV_spring90s)%>%
  bind_rows(imEV_spring00s)%>%
  bind_rows(imEV_spring10s)%>%
  bind_rows(imEV_summer00s)%>%
  bind_rows(imEV_summer10s)

immerNV <- bind_rows(imNV_fall90s,imNV_fall00s)%>%
  bind_rows(imNV_fall10s)%>%
  bind_rows(imNV_winter90s)%>%
  bind_rows(imNV_winter00s)%>%
  bind_rows(imNV_winter10s)%>%
  bind_rows(imNV_spring90s)%>%
  bind_rows(imNV_spring00s)%>%
  bind_rows(imNV_spring10s)%>%
  bind_rows(imNV_summer00s)%>%
  bind_rows(imNV_summer10s)

immerML <- bind_rows(imML_fall90s,imML_fall00s)%>%
  bind_rows(imML_fall10s)%>%
  bind_rows(imML_winter90s)%>%
  bind_rows(imML_winter00s)%>%
  bind_rows(imML_winter10s)%>%
  bind_rows(imML_spring90s)%>%
  bind_rows(imML_spring00s)%>%
  bind_rows(imML_spring10s)%>%
  bind_rows(imML_summer00s)%>%
  bind_rows(imML_summer10s)

immerSa <- bind_rows(imSa_fall90s,imSa_fall00s)%>%
  bind_rows(imSa_fall10s)%>%
  bind_rows(imSa_winter90s)%>%
  bind_rows(imSa_winter00s)%>%
  bind_rows(imSa_winter10s)%>%
  bind_rows(imSa_spring90s)%>%
  bind_rows(imSa_spring00s)%>%
  bind_rows(imSa_spring10s)%>%
  bind_rows(imSa_summer00s)%>%
  bind_rows(imSa_summer10s)

immerSST <- bind_rows(imSST_fall90s,imSST_fall00s)%>%
  bind_rows(imSST_fall10s)%>%
  bind_rows(imSST_winter90s)%>%
  bind_rows(imSST_winter00s)%>%
  bind_rows(imSST_winter10s)%>%
  bind_rows(imSST_spring90s)%>%
  bind_rows(imSST_spring00s)%>%
  bind_rows(imSST_spring10s)%>%
  bind_rows(imSST_summer00s)%>%
  bind_rows(imSST_summer10s)

immerSSH <- bind_rows(imSSH_fall90s,imSSH_fall00s)%>%
  bind_rows(imSSH_fall10s)%>%
  bind_rows(imSSH_winter90s)%>%
  bind_rows(imSSH_winter00s)%>%
  bind_rows(imSSH_winter10s)%>%
  bind_rows(imSSH_spring90s)%>%
  bind_rows(imSSH_spring00s)%>%
  bind_rows(imSSH_spring10s)%>%
  bind_rows(imSSH_summer00s)%>%
  bind_rows(imSSH_summer10s)

immer <- left_join(immerSa,immerML)%>%
  left_join(immerSSH)%>%
  left_join(immerSST)%>%
  left_join(immerEV)%>%
  left_join(immerNV)
immer[is.na(immer)] <- 0

immer <- immer %>%
  filter(Sa > 0)

immerLatLon <- immer[,1:2]
immerEnv    <- immer[,5:ncol(immer)]

# Use an AIC-based stepwise variable selection procedure with a RDA approach
# to build an optimal (most parsimonious) model depicting the relationship
# between the fish community and the set of associated environmental variables.

# Create the "null" and "full" models described above:
null      <- rda(immerLatLon ~ 1, immerEnv)
full      <- rda(immerLatLon ~ ., immerEnv)

# Now perform the AIC stepwise selection process:
optImmer  <- step(object = null, scope = formula(full)) #optimal model selected by AIC selection

# Perform RDA and plot ------------------------------------------------------------

permutest(x = optImmer, permutations = 9999) 
# very significant P value (Pr(>F))
# model is F ratio 753.25 (753x more variability explained than by random chance)

# Perform RDA:
rdaImmer        <- rda(X = immerLatLon, Y = immerEnv)

# Produce and display the summary results:
optImmer_sum    <- summary(rdaImmer)
optImmer_sum

# Create the ordination diagram.

# Extract the percent of the variability explained by each axis:
pct_explImmer  <- 100 * optImmer_sum$cont$importance[2,]

scores_Immer <- scores(optImmer)
dat <- cbind(immer, scores_Immer$sites)

ggplot()+
  geom_point(data = dat, aes(x = RDA1,
                             y = RDA2,
                             color = Decade,
                             shape = Season)) +
  labs(title = "Redundancy Analysis for G. immer",
       x = paste("RDA1 (variance explained: ",
                 round(pct_explImmer[1], digits = 1),
                 "%)",
                 sep =""),
       y = paste("RDA2 (variance explained: ",
                 round(pct_explImmer[2], digits = 1),
                 "%)",
                 sep =""))+
  geom_text(data = as.data.frame(scores(rdaImmer)$species),
            aes(x = RDA1/optImmer_sum$cont$importance[1,1], 
                y = RDA2/optImmer_sum$cont$importance[1,2],
                label = c("Long","Lat")))+
  stat_ellipse(data = dat, aes(x = RDA1,
                               y = RDA2,
                               color = Decade),
               size = 1,
               level = 0.90,
               alpha = 0.65,
               show.legend = FALSE)+
  geom_segment(data = as.data.frame(optImmer_sum$biplot),
               aes(x = 0,
                   y = 0, 
                   xend = RDA1,
                   yend = RDA2),
               arrow = arrow(length=unit(0.3, "cm")),
               color = "gray30")+
  geom_label(data = as.data.frame(optImmer_sum$biplot),
             aes(x = RDA1,
                 y = RDA2,
                 label = rownames(optImmer_sum$biplot)))

# grisWest ----

#bind_rows together all data frames

grisWestEV <- bind_rows(grisEV_westfall90s,grisEV_westwinter00s)%>%
  bind_rows(grisEV_westspring90s)%>%
  bind_rows(grisEV_westsummer00s)%>%
  bind_rows(grisEV_westsummer10s)%>%
  bind_rows(grisEV_westsummer90s)

grisWestNV <- bind_rows(grisNV_westfall90s,grisNV_westwinter00s)%>%
  bind_rows(grisNV_westspring90s)%>%
  bind_rows(grisNV_westsummer00s)%>%
  bind_rows(grisNV_westsummer10s)%>%
  bind_rows(grisNV_westsummer90s)

grisWestML <- bind_rows(grisML_westfall90s,grisML_westwinter00s)%>%
  bind_rows(grisML_westspring90s)%>%
  bind_rows(grisML_westsummer00s)%>%
  bind_rows(grisML_westsummer10s)%>%
  bind_rows(grisML_westsummer90s)


grisWestSa <- bind_rows(grisSa_westfall90s,grisSa_westwinter00s)%>%
  bind_rows(grisSa_westspring90s)%>%
  bind_rows(grisSa_westsummer00s)%>%
  bind_rows(grisSa_westsummer10s)%>%
  bind_rows(grisSa_westsummer90s)

grisWestSST <- bind_rows(grisSST_westfall90s,grisSST_westwinter00s)%>%
  bind_rows(grisSST_westspring90s)%>%
  bind_rows(grisSST_westsummer00s)%>%
  bind_rows(grisSST_westsummer10s)%>%
  bind_rows(grisSST_westsummer90s)

grisWestSSH <- bind_rows(grisSSH_westfall90s,grisSSH_westwinter00s)%>%
  bind_rows(grisSSH_westspring90s)%>%
  bind_rows(grisSSH_westsummer00s)%>%
  bind_rows(grisSSH_westsummer10s)%>%
  bind_rows(grisSSH_westsummer90s)

grisWest <- left_join(grisWestSa,grisWestML)%>%
  left_join(grisWestSSH)%>%
  left_join(grisWestSST)%>%
  left_join(grisWestEV)%>%
  left_join(grisWestNV)
grisWest[is.na(grisWest)] <- 0

grisWest <- grisWest %>%
  filter(Sa > 0)

grisWestLatLon <- grisWest[,1:2]
grisWestEnv    <- grisWest[,5:ncol(grisWest)]

# Use an AIC-based stepwise variable selection procedure with a RDA approach
# to build an optimal (most parsimonious) model depicting the relationship
# between the fish community and the set of associated environmental variables.

# Create the "null" and "full" models described above:
null      <- rda(grisWestLatLon ~ 1, grisWestEnv)
full      <- rda(grisWestLatLon ~ ., grisWestEnv)

# Now perform the AIC stepwise selection process:
optGrisWest       <- step(object = null, scope = formula(full)) #optimal model selected by AIC selection

# All variables gives model with lowest AIC 
# Step:  AIC=-321.24
# grisWestLatLon ~ Sa + SSH + NV + ML + EV + SST
# 
# Df     AIC
# <none>    -321.24
# - SST   1 -314.15
# - EV    1 -308.30
# - ML    1 -308.09
# - SSH   1 -225.82
# - NV    1 -213.61
# - Sa    1  356.99

# Perform RDA and plot ------------------------------------------------------------

permutest(x = optGrisWest, permutations = 9999) 
# very significant P value (Pr(>F))
# model is F ratio 2838 (2838x more variability explained than by random chance)

# Perform RDA:
rdaGrisWest        <- rda(X = grisWestLatLon, Y = grisWestEnv)

# Produce and display the summary results:
optGrisWest_sum    <- summary(rdaGrisWest)
optGrisWest_sum

# Create the ordination diagram.

# Extract the percent of the variability explained by each axis:
pct_explGrisWest  <- 100 * optGrisWest_sum$cont$importance[2,]

scores_GrisWest <- scores(optGrisWest)
dat <- cbind(grisWest, scores_GrisWest$sites)

ggplot()+
  geom_point(data = dat, aes(x = RDA1,
                             y = RDA2,
                             color = Decade,
                             shape = Season)) +
  labs(title = "Redundancy Analysis for P. griseus (West)",
       x = paste("RDA1 (variance explained: ",
                 round(pct_explGrisWest[1], digits = 1),
                 "%)",
                 sep =""),
       y = paste("RDA2 (variance explained: ",
                 round(pct_explGrisWest[2], digits = 1),
                 "%)",
                 sep =""))+
  geom_text(data = as.data.frame(scores(rdagrisWest)$species),
            aes(x = RDA1/optGrisWest_sum$cont$importance[1,1], 
                y = RDA2/optGrisWest_sum$cont$importance[1,2],
                label = c("Long","Lat")))+
  stat_ellipse(data = dat, aes(x = RDA1,
                               y = RDA2,
                               color = Decade),
               size = 1,
               level = 0.90,
               alpha = 0.65,
               show.legend = FALSE)+
  geom_segment(data = as.data.frame(optGrisWest_sum$biplot),
               aes(x = 0,
                   y = 0, 
                   xend = RDA1,
                   yend = RDA2),
               arrow = arrow(length=unit(0.3, "cm")),
               color = "gray30")+
  geom_label(data = as.data.frame(optGrisWest_sum$biplot),
             aes(x = RDA1,
                 y = RDA2,
                 label = rownames(optGrisWest_sum$biplot)))

# grisEast ----

#bind_rows together all data frames
grisEastEV <- bind_rows(grisEV_eastfall90s, grisEV_eastfall00s)%>%
  bind_rows(grisEV_eastfall10s)%>%
  bind_rows(grisEV_eastwinter10s)%>%
  bind_rows(grisEV_eastspring00s)%>%
  bind_rows(grisEV_eastspring10s)%>%
  bind_rows(grisEV_eastspring90s)%>%
  bind_rows(grisEV_eastsummer00s)%>%
  bind_rows(grisEV_eastsummer10s)%>%
  bind_rows(grisEV_eastsummer90s)
  
grisEastNV <- bind_rows(grisNV_eastfall90s,grisNV_eastfall00s)%>%
  bind_rows(grisNV_eastfall10s)%>%
  bind_rows(grisNV_eastwinter10s)%>%
  bind_rows(grisNV_eastspring00s)%>%
  bind_rows(grisNV_eastspring10s)%>%
  bind_rows(grisNV_eastspring90s)%>%
  bind_rows(grisNV_eastsummer00s)%>%
  bind_rows(grisNV_eastsummer10s)%>%
  bind_rows(grisNV_eastsummer90s)

grisEastML <- bind_rows(grisML_eastfall90s,grisML_eastfall00s)%>%
  bind_rows(grisML_eastfall10s)%>%
  bind_rows(grisML_eastwinter10s)%>%
  bind_rows(grisML_eastspring00s)%>%
  bind_rows(grisML_eastspring10s)%>%
  bind_rows(grisML_eastspring90s)%>%
  bind_rows(grisML_eastsummer00s)%>%
  bind_rows(grisML_eastsummer10s)%>%
  bind_rows(grisML_eastsummer90s)

grisEastSa <- bind_rows(grisSa_eastfall90s,grisSa_eastfall00s)%>%
  bind_rows(grisSa_eastfall10s)%>%
  bind_rows(grisSa_eastwinter10s)%>%
  bind_rows(grisSa_eastspring00s)%>%
  bind_rows(grisSa_eastspring10s)%>%
  bind_rows(grisSa_eastspring90s)%>%
  bind_rows(grisSa_eastsummer00s)%>%
  bind_rows(grisSa_eastsummer10s)%>%
  bind_rows(grisSa_eastsummer90s)

grisEastSST <- bind_rows(grisSST_eastfall90s, grisSST_eastfall00s)%>%
  bind_rows(grisSST_eastfall10s)%>%
  bind_rows(grisSST_eastwinter10s)%>%
  bind_rows(grisSST_eastspring00s)%>%
  bind_rows(grisSST_eastspring10s)%>%
  bind_rows(grisSST_eastspring90s)%>%
  bind_rows(grisSST_eastsummer00s)%>%
  bind_rows(grisSST_eastsummer10s)%>%
  bind_rows(grisSST_eastsummer90s)

grisEastSSH <- bind_rows(grisSSH_eastfall90s, grisSSH_eastfall00s)%>%
  bind_rows(grisSSH_eastfall10s)%>%
  bind_rows(grisSSH_eastwinter10s)%>%
  bind_rows(grisSSH_eastspring00s)%>%
  bind_rows(grisSSH_eastspring10s)%>%
  bind_rows(grisSSH_eastspring90s)%>%
  bind_rows(grisSSH_eastsummer00s)%>%
  bind_rows(grisSSH_eastsummer10s)%>%
  bind_rows(grisSSH_eastsummer90s)
  
grisEast <- left_join(grisEastSa,grisEastML)%>%
  left_join(grisEastSSH)%>%
  left_join(grisEastSST)%>%
  left_join(grisEastEV)%>%
  left_join(grisEastNV)
grisEast[is.na(grisEast)] <- 0

grisEast <- grisEast %>%
  filter(Sa > 0)

grisEastLatLon <- grisEast[,1:2]
grisEastEnv    <- dat[,5:ncol(grisEast)]

# Use an AIC-based stepwise variable selection procedure with a RDA approach
# to build an optimal (most parsimonious) model depicting the relationship
# between the fish community and the set of associated environmental variables.

# Create the "null" and "full" models described above:
null      <- rda(grisEastLatLon ~ 1, grisEastEnv)
full      <- rda(grisEastLatLon ~ ., grisEastEnv) 
#Error in qr.fitted(Q, Y) : 'qr' and 'y' must have the same number of rows <- WHAT?

# Now perform the AIC stepwise selection process:
optGrisEast       <- step(object = null, scope = formula(full)) #optimal model selected by AIC selection

# Perform RDA and plot -------------------------------------------------------------
permutest(x = optGrisEast, permutations = 9999) 
# very significant P value (Pr(>F))
# model is F ratio 148 (148x more variability explained than by random chance)

# Perform RDA:
rdaGrisEast        <- rda(X = grisEastLatLon, Y = grisEastEnv)

# Produce and display the summary results:
optGrisEast_sum    <- summary(rdaGrisEast)
optGrisEast_sum

# Create the ordination diagram.

# Extract the percent of the variability explained by each axis:
pct_explGrisEast  <- 100 * optgrisEast_sum$cont$importance[2,]

scores_GrisEast <- scores(optGrisEast)
dat <- cbind(grisEast, scores_GrisEast$sites)

ggplot()+
     geom_point(data = dat, aes(x = RDA1,
                                y = RDA2,
                                color = Decade,
                                shape = Season)) +
      labs(title = "Redundancy Analysis for P. griseus (East)",
           x = paste("RDA1 (variance explained: ",
                 round(pct_explGrisEast[1], digits = 1),
                 "%)",
                 sep =""),
           y = paste("RDA2 (variance explained: ",
                 round(pct_explGrisEast[2], digits = 1),
                 "%)",
                 sep =""))+
      geom_text(data = as.data.frame(scores(rdagrisEast)$species),
             aes(x = RDA1/optGrisEast_sum$cont$importance[1,1], 
                 y = RDA2/optGrisEast_sum$cont$importance[1,2],
                 label = c("Long","Lat")))+
      stat_ellipse(data = dat, aes(x = RDA1,
                                y = RDA2,
                                color = Decade),
                size = 1,
                level = 0.90,
                alpha = 0.65,
                show.legend = FALSE)+
      geom_segment(data = as.data.frame(opt_sum$biplot),
               aes(x = 0,
                   y = 0, 
                   xend = dbRDA1,
                   yend = dbRDA2),
               arrow = arrow(length=unit(0.3, "cm")),
               color = "gray30")+
      geom_label(data = as.data.frame(opt_sum$biplot),
             aes(x = dbRDA1,
                 y = dbRDA2,
                 label = rownames(opt_sum$biplot)))
