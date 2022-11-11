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

grisEV_westwinter00s <- raster("Extract_EVgriseus_westwinter00s.tif")
grisEV_westwinter00s <- as.data.frame(grisEV_westwinter00s, xy = TRUE)
grisEV_westwinter00s <- grisEV_westwinter00s %>%
  add_column(Season = "Winter",
             Decade = "00s",
             .after = "y")

grisEV_westspring90s <- raster("Extract_EVgriseus_westspring90s.tif")
grisEV_westspring90s <- as.data.frame(grisEV_westspring90s, xy = TRUE)
grisEV_westspring90s <- grisEV_westspring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")

grisEV_westsummer90s <- raster("Extract_EVgriseus_westsummer90s.tif")
grisEV_westsummer90s <- as.data.frame(grisEV_westsummer90s, xy = TRUE)
grisEV_westsummer90s <- grisEV_westsummer90s %>%
  add_column(Season = "Winter",
             Decade = "00s",
             .after = "y")

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

# Eastern current velocity nigripes ----

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

nigEV_fall10s <- raster("Extract_EVnigripes_fall10s.tif")
nigEV_fall10s <- as.data.frame(nigEV_fall10s, xy = TRUE)
x <- nigEV_fall10s%>%
  filter(Extract_EVnigipes_fall10s >-20)
av <- mean(x$Extract_EVnigripes_fall0s) 
av
mx <- max(x$Extract_EVnigripes_fall10s)
mx
mn <- min(x$Extract_EVnigripes_fall10s)
mn

nigEV_spring90s <- raster("Extract_EVnigripes_spring90s.tif")
nigEV_spring90s <- as.data.frame(nigEV_spring90s, xy = TRUE)
x <- nigEV_spring90s%>%
  filter(Extract_EVnigripes_spring90s >-20)
av <- mean(x$Extract_EVnigripes_spring90s) 
av
mx <- max(x$Extract_EVnigripes_spring90s)
mx
mn <- min(x$Extract_EVnigripes_spring90s)
mn

nigEV_summer90s <- raster("Extract_EVnigripes_summer90s.tif")
nigEV_summer90s <- as.data.frame(nigEV_summer90s, xy = TRUE)
x <- nigEV_summer90s%>%
  filter(Extract_EVnigripes_summer90s >-20)
av <- mean(x$Extract_EVnigripes_summer90s) 
av
mx <- max(x$Extract_EVnigripes_summer90s)
mx
mn <- min(x$Extract_EVnigripes_summer90s)
mn

nigEV_summer00s <- raster("Extract_EVnigripes_summer00s.tif")
nigEV_summer00s <- as.data.frame(nigEV_summer00s, xy = TRUE)
x <- nigEV_summer00s %>%
  filter(Extract_EVnigripes_summer00s >-20)
av <- mean(x$Extract_EVnigripes_summer00s) 
av
mx <- max(x$Extract_EVnigripes_summer00s)
mx
mn <- min(x$Extract_EVnigripes_summer00s)
mn

nigEV_summer10s <- raster("Extract_EVnigripes_summer10s.tif")
nigEV_summer10s <- as.data.frame(nigEV_summer10s, xy = TRUE)
x <- nigEV_summer10s%>%
  filter(Extract_EVnigripes_summer10s >-20)
av <- mean(x$Extract_EVnigripes_summer10s) 
av
mx <- max(x$Extract_EVnigripes_summer10s)
mx
mn <- min(x$Extract_EVnigripes_summer10s)
mn

# Eastern current velocity immer-----

imEV_fall90s <- raster("Extract_EVimmer_fall90s.tif")
imEV_fall90s <- as.data.frame(imEV_fall90s, xy = TRUE)
imEV_fall90s <- imEV_fall90s %>%
  add_column(Season = "Fall",
             Decade = "90s",
             .after = "y")

imEV_fall00s <- raster("Extract_EVimmer_fall00s.tif")
imEV_fall00s <- as.data.frame(imEV_fall00s, xy = TRUE)
imEV_fall00s <- imEV_fall00s %>%
  add_column(Season = "Fall",
             Decade = "00s",
             .after = "y")

imEV_fall10s <- raster("Extract_EVimmer_fall10s.tif")
imEV_fall10s <- as.data.frame(imEV_fall10s, xy = TRUE)
imEV_fall10s <- imEV_fall10s %>%
  add_column(Season = "Fall",
             Decade = "10s",
             .after = "y")

imEV_winter90s <- raster("Extract_EVimmer_winter90s.tif")
imEV_winter90s <- as.data.frame(imEV_winter90s, xy = TRUE)
imEV_winter90s <- imEV_winter90s %>%
  add_column(Season = "Winter",
             Decade = "90s",
             .after = "y")

imEV_winter00s <- raster("Extract_EVimmer_winter00s.tif")
imEV_winter00s <- as.data.frame(imEV_winter00s, xy = TRUE)
imEV_winter00s <- imEV_winter00s %>%
  add_column(Season = "Winter",
             Decade = "00s",
             .after = "y")

imEV_winter10s <- raster("Extract_EVimmer_winter10s.tif")
imEV_winter10s <- as.data.frame(imEV_winter10s, xy = TRUE)
imEV_winter10s <- imEV_winter10s %>%
  add_column(Season = "Winter",
             Decade = "10s",
             .after = "y")

imEV_spring90s <- raster("Extract_EVimmer_spring90s.tif")
imEV_spring90s <- as.data.frame(imEV_spring90s, xy = TRUE)
imEV_spring90s <- imEV_spring90s %>%
  add_column(Season = "Spring",
             Decade = "90s",
             .after = "y")

imEV_spring00s <- raster("Extract_EVimmer_spring00s.tif")
imEV_spring00s <- as.data.frame(imEV_spring00s, xy = TRUE)
imEV_spring00s <- imEV_spring00s %>%
  add_column(Season = "Spring",
             Decade = "00s",
             .after = "y")

imEV_spring10s <- raster("Extract_EVimmer_spring10s.tif")
imEV_spring10s <- as.data.frame(imEV_spring10s, xy = TRUE)
imEV_spring10s <- imEV_spring10s %>%
  add_column(Season = "Spring",
             Decade = "10s",
             .after = "y")

imEV_summer00s <- raster("Extract_EVimmer_summer00s.tif")
imEV_summer00s <- as.data.frame(imEV_summer00s, xy = TRUE)
imEV_summer00s <- imEV_summer00s %>%
  add_column(Season = "Summer",
             Decade = "00s",
             .after = "y")

imEV_summer10s <- raster("Extract_EVimmer_summer10s.tif")
imEV_summer10s <- as.data.frame(imEV_summer10s, xy = TRUE)
imEV_summer10s <- imEV_summer10s %>%
  add_column(Season = "Summer",
             Decade = "10s",
             .after = "y")

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

imML_fall90s <- raster("Extract_MLimmer_fall90s.tif")
imML_fall90s <- as.data.frame(imML_fall90s, xy = TRUE)
x <- imML_fall90s%>%
  filter(Extract_MLimmer_fall90s >=0)
av <- mean(x$Extract_MLimmer_fall90s) #number of occurrence points
av
mx <- max(x$Extract_MLimmer_fall90s)
mx
mn <- min(x$Extract_MLimmer_fall90s)
mn

imML_fall00s <- raster("Extract_MLimmer_fall00s.tif")
imML_fall00s <- as.data.frame(imML_fall00s, xy = TRUE)
x <- imML_fall00s%>%
  filter(Extract_MLimmer_fall00s >=0)
av <- mean(x$Extract_MLimmer_fall00s) #number of occurrence points
av
mx <- max(x$Extract_MLimmer_fall00s)
mx
mn <- min(x$Extract_MLimmer_fall00s)
mn

imML_fall10s <- raster("Extract_MLimmer_fall10s.tif")
imML_fall10s <- as.data.frame(imML_fall10s, xy = TRUE)
x <- imML_fall10s%>%
  filter(Extract_MLimmer_fall10s >=0)
av <- mean(x$Extract_MLimmer_fall10s) #number of occurrence points
av
mx <- max(x$Extract_MLimmer_fall10s)
mx
mn <- min(x$Extract_MLimmer_fall10s)
mn

imML_winter90s <- raster("Extract_MLimmer_winter90s.tif")
imML_winter90s <- as.data.frame(imML_winter90s, xy = TRUE)
x <- imML_winter90s%>%
  filter(Extract_MLimmer_winter90s >=0)
av <- mean(x$Extract_MLimmer_winter90s) #number of occurrence points
av
mx <- max(x$Extract_MLimmer_winter90s)
mx
mn <- min(x$Extract_MLimmer_winter90s)
mn

imML_winter00s <- raster("Extract_MLimmer_winter00s.tif")
imML_winter00s <- as.data.frame(imML_winter00s, xy = TRUE)
x <- imML_winter00s%>%
  filter(Extract_MLimmer_winter00s >=0)
av <- mean(x$Extract_MLimmer_winter00s) #number of occurrence points
av
mx <- max(x$Extract_MLimmer_winter00s)
mx
mn <- min(x$Extract_MLimmer_winter00s)
mn

imML_winter10s <- raster("Extract_MLimmer_winter10s.tif")
imML_winter10s <- as.data.frame(imML_winter10s, xy = TRUE)
x <- imML_winter10s%>%
  filter(Extract_MLimmer_winter10s >=0)
av <- mean(x$Extract_MLimmer_winter10s) #number of occurrence points
av
mx <- max(x$Extract_MLimmer_winter10s)
mx
mn <- min(x$Extract_MLimmer_winter10s)
mn

imML_spring90s <- raster("Extract_MLimmer_spring90s.tif")
imML_spring90s <- as.data.frame(imML_spring90s, xy = TRUE)
x <- imML_spring90s%>%
  filter(Extract_MLimmer_spring90s >=0)
av <- mean(x$Extract_MLimmer_spring90s) #number of occurrence points
av
mx <- max(x$Extract_MLimmer_spring90s)
mx
mn <- min(x$Extract_MLimmer_spring90s)
mn

imML_spring00s <- raster("Extract_MLimmer_spring00s.tif")
imML_spring00s <- as.data.frame(imML_spring00s, xy = TRUE)
x <- imML_spring00s%>%
  filter(Extract_MLimmer_spring00s >=0)
av <- mean(x$Extract_MLimmer_spring00s) #number of occurrence points
av
mx <- max(x$Extract_MLimmer_spring00s)
mx
mn <- min(x$Extract_MLimmer_spring00s)
mn

imML_spring10s <- raster("Extract_MLimmer_spring10s.tif")
imML_spring10s <- as.data.frame(imML_spring10s, xy = TRUE)
x <- imML_spring10s%>%
  filter(Extract_MLimmer_spring10s >=0)
av <- mean(x$Extract_MLimmer_spring10s) #number of occurrence points
av
mx <- max(x$Extract_MLimmer_spring10s)
mx
mn <- min(x$Extract_MLimmer_spring10s)
mn

imML_summer00s <- raster("Extract_MLimmer_summer00s.tif")
imML_summer00s <- as.data.frame(imML_summer00s, xy = TRUE)
x <- imML_summer00s%>%
  filter(Extract_MLimmer_summer00s >=0)
av <- mean(x$Extract_MLimmer_summer00s) #number of occurrence points
av
mx <- max(x$Extract_MLimmer_summer00s)
mx
mn <- min(x$Extract_MLimmer_summer00s)
mn

imML_summer10s <- raster("Extract_MLimmer_summer10s.tif")
imML_summer10s <- as.data.frame(imML_summer10s, xy = TRUE)
x <- imML_summer10s%>%
  filter(Extract_MLimmer_summer10s >=0)
av <- mean(x$Extract_MLimmer_summer10s) #number of occurrence points
av
mx <- max(x$Extract_MLimmer_summer10s)
mx
mn <- min(x$Extract_MLimmer_summer10s)
mn

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

# Northern current velocity nigripes ----

nigNV_fall90s <- raster("Extract_NVnigripes_fall90s.tif")
nigNV_fall90s <- as.data.frame(nigNV_fall90s, xy = TRUE)
x <- nigNV_fall90s%>%
  filter(Extract_NVnigripes_fall90s >-20)
av <- mean(x$Extract_NVnigripes_fall90s) 
av
mx <- max(x$Extract_NVnigripes_fall90s)
mx
mn <- min(x$Extract_NVnigripes_fall90s)
mn

nigNV_fall10s <- raster("Extract_NVnigripes_fall10s.tif")
nigNV_fall10s <- as.data.frame(nigNV_fall10s, xy = TRUE)
x <- nigNV_fall10s %>%
  filter(Extract_NVnigripes_fall10s >-20)
av <- mean(x$Extract_NVnigripes_fall10s) 
av
mx <- max(x$Extract_NVnigripes_fall10s)
mx
mn <- min(x$Extract_NVnigripes_fall10s)
mn

nigNV_spring90s <- raster("Extract_NVnigripes_spring90s.tif")
nigNV_spring90s <- as.data.frame(nigNV_spring90s, xy = TRUE)
x <- nigNV_spring90s%>%
  filter(Extract_NVnigripes_spring90s >-20)
av <- mean(x$Extract_NVnigripes_spring90s) 
av
mx <- max(x$Extract_NVnigripes_spring90s)
mx
mn <- min(x$Extract_NVnigripes_spring90s)
mn

nigNV_summer90s <- raster("Extract_NVnigripes_summer90s.tif")
nigNV_summer90s <- as.data.frame(nigNV_summer90s, xy = TRUE)
x <- nigNV_summer90s%>%
  filter(Extract_NVnigripes_summer90s >-20)
av <- mean(x$Extract_NVnigripes_summer90s) 
av
mx <- max(x$Extract_NVnigripes_summer90s)
mx
mn <- min(x$Extract_NVnigripes_summer90s)
mn

nigNV_summer00s <- raster("Extract_NVnigripes_summer00s.tif")
nigNV_summer00s <- as.data.frame(nigNV_summer00s, xy = TRUE)
x <- nigNV_summer00s %>%
  filter(Extract_NVnigripes_summer00s >-20)
av <- mean(x$Extract_NVnigripes_summer00s) 
av
mx <- max(x$Extract_NVnigripes_summer00s)
mx
mn <- min(x$Extract_NVnigripes_summer00s)
mn

nigNV_summer10s <- raster("Extract_NVnigripes_summer10s.tif")
nigNV_summer10s <- as.data.frame(nigNV_summer10s, xy = TRUE)
x <- nigNV_summer10s%>%
  filter(Extract_NVnigripes_summer10s >-20)
av <- mean(x$Extract_NVnigripes_summer10s) 
av
mx <- max(x$Extract_NVnigripes_summer10s)
mx
mn <- min(x$Extract_NVnigripes_summer10s)
mn

# Northern current velocity immer ----

imNV_fall90s <- raster("Extract_NVimmer_fall90s.tif")
imNV_fall90s <- as.data.frame(imNV_fall90s, xy = TRUE)
x <- imNV_fall90s%>%
  filter(Extract_NVimmer_fall90s >-20)
av <- mean(x$Extract_NVimmer_fall90s) #number of occurrence points
av
mx <- max(x$Extract_NVimmer_fall90s)
mx
mn <- min(x$Extract_NVimmer_fall90s)
mn

imNV_fall00s <- raster("Extract_NVimmer_fall00s.tif")
imNV_fall00s <- as.data.frame(imNV_fall00s, xy = TRUE)
x <- imNV_fall00s%>%
  filter(Extract_NVimmer_fall00s >-20)
av <- mean(x$Extract_NVimmer_fall00s) #number of occurrence points
av
mx <- max(x$Extract_NVimmer_fall00s)
mx
mn <- min(x$Extract_NVimmer_fall00s)
mn

imNV_fall10s <- raster("Extract_NVimmer_fall10s.tif")
imNV_fall10s <- as.data.frame(imNV_fall10s, xy = TRUE)
x <- imNV_fall10s%>%
  filter(Extract_NVimmer_fall10s >-20)
av <- mean(x$Extract_NVimmer_fall10s) #number of occurrence points
av
mx <- max(x$Extract_NVimmer_fall10s)
mx
mn <- min(x$Extract_NVimmer_fall10s)
mn

imNV_winter90s <- raster("Extract_NVimmer_winter90s.tif")
imNV_winter90s <- as.data.frame(imNV_winter90s, xy = TRUE)
x <- imNV_winter90s%>%
  filter(Extract_NVimmer_winter90s >-20)
av <- mean(x$Extract_NVimmer_winter90s) #number of occurrence points
av
mx <- max(x$Extract_NVimmer_winter90s)
mx
mn <- min(x$Extract_NVimmer_winter90s)
mn

imNV_winter00s <- raster("Extract_NVimmer_winter00s.tif")
imNV_winter00s <- as.data.frame(imNV_winter00s, xy = TRUE)
x <- imNV_winter00s%>%
  filter(Extract_NVimmer_winter00s >-20)
av <- mean(x$Extract_NVimmer_winter00s) #number of occurrence points
av
mx <- max(x$Extract_NVimmer_winter00s)
mx
mn <- min(x$Extract_NVimmer_winter00s)
mn

imNV_winter10s <- raster("Extract_NVimmer_winter10s.tif")
imNV_winter10s <- as.data.frame(imNV_winter10s, xy = TRUE)
x <- imNV_winter10s%>%
  filter(Extract_NVimmer_winter10s >-20)
av <- mean(x$Extract_NVimmer_winter10s) #number of occurrence points
av
mx <- max(x$Extract_NVimmer_winter10s)
mx
mn <- min(x$Extract_NVimmer_winter10s)
mn

imNV_spring90s <- raster("Extract_NVimmer_spring90s.tif")
imNV_spring90s <- as.data.frame(imNV_spring90s, xy = TRUE)
x <- imNV_spring90s%>%
  filter(Extract_NVimmer_spring90s >-20)
av <- mean(x$Extract_NVimmer_spring90s) #number of occurrence points
av
mx <- max(x$Extract_NVimmer_spring90s)
mx
mn <- min(x$Extract_NVimmer_spring90s)
mn

imNV_spring00s <- raster("Extract_NVimmer_spring00s.tif")
imNV_spring00s <- as.data.frame(imNV_spring00s, xy = TRUE)
x <- imNV_spring00s%>%
  filter(Extract_NVimmer_spring00s >-20)
av <- mean(x$Extract_NVimmer_spring00s) #number of occurrence points
av
mx <- max(x$Extract_NVimmer_spring00s)
mx
mn <- min(x$Extract_NVimmer_spring00s)
mn

imNV_spring10s <- raster("Extract_NVimmer_spring10s.tif")
imNV_spring10s <- as.data.frame(imNV_spring10s, xy = TRUE)
x <- imNV_spring10s%>%
  filter(Extract_NVimmer_spring10s >-20)
av <- mean(x$Extract_NVimmer_spring10s) #number of occurrence points
av
mx <- max(x$Extract_NVimmer_spring10s)
mx
mn <- min(x$Extract_NVimmer_spring10s)
mn

imNV_summer00s <- raster("Extract_NVimmer_summer00s.tif")
imNV_summer00s <- as.data.frame(imNV_summer00s, xy = TRUE)
x <- imNV_summer00s%>%
  filter(Extract_NVimmer_summer00s >-20)
av <- mean(x$Extract_NVimmer_summer00s) #number of occurrence points
av
mx <- max(x$Extract_NVimmer_summer00s)
mx
mn <- min(x$Extract_NVimmer_summer00s)
mn

imNV_summer10s <- raster("Extract_NVimmer_summer10s.tif")
imNV_summer10s <- as.data.frame(imNV_summer10s, xy = TRUE)
x <- imNV_summer10s%>%
  filter(Extract_NVimmer_summer10s >-20)
av <- mean(x$Extract_NVimmer_summer10s) #number of occurrence points
av
mx <- max(x$Extract_NVimmer_summer10s)
mx
mn <- min(x$Extract_NVimmer_summer10s)
mn

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

# Salinity nigripes -----

nigSa_fall90s <- raster("Extract_Sanigripes_fall90s.tif")
nigSa_fall90s <- as.data.frame(nigSa_fall90s, xy = TRUE)
x <- nigSa_fall90s%>%
  filter(Extract_Sanigripes_fall90s >0)
av <- mean(x$Extract_Sanigripes_fall90s) 
av
mx <- max(x$Extract_Sanigripes_fall90s)
mx
mn <- min(x$Extract_Sanigripes_fall90s)
mn

nigSa_fall10s <- raster("Extract_Sanigripes_fall10s.tif")
nigSa_fall10s <- as.data.frame(nigSa_fall10s, xy = TRUE)
x <- nigSa_fall10s %>%
  filter(Extract_Sanigripes_fall10s >0)
av <- mean(x$Extract_Sanigripes_fall10s) 
av
mx <- max(x$Extract_Sanigripes_fall10s)
mx
mn <- min(x$Extract_Sanigripes_fall10s)
mn

nigSa_spring90s <- raster("Extract_Sanigripes_spring90s.tif")
nigSa_spring90s <- as.data.frame(nigSa_spring90s, xy = TRUE)
x <- nigSa_spring90s%>%
  filter(Extract_Sanigripes_spring90s >0)
av <- mean(x$Extract_Sanigripes_spring90s) 
av
mx <- max(x$Extract_Sanigripes_spring90s)
mx
mn <- min(x$Extract_Sanigripes_spring90s)
mn

nigSa_summer90s <- raster("Extract_Sanigripes_summer90s.tif")
nigSa_summer90s <- as.data.frame(nigSa_summer90s, xy = TRUE)
x <- nigSa_summer90s%>%
  filter(Extract_Sanigripes_summer90s >0)
av <- mean(x$Extract_Sanigripes_summer90s) 
av
mx <- max(x$Extract_Sanigripes_summer90s)
mx
mn <- min(x$Extract_Sanigripes_summer90s)
mn

nigSa_summer00s <- raster("Extract_Sanigripes_summer00s.tif")
nigSa_summer00s <- as.data.frame(nigSa_summer00s, xy = TRUE)
x <- nigSa_summer00s %>%
  filter(Extract_Sanigripes_summer00s >0)
av <- mean(x$Extract_Sanigripes_summer00s) 
av
mx <- max(x$Extract_Sanigripes_summer00s)
mx
mn <- min(x$Extract_Sanigripes_summer00s)
mn

nigSa_summer10s <- raster("Extract_Sanigripes_summer10s.tif")
nigSa_summer10s <- as.data.frame(nigSa_summer10s, xy = TRUE)
x <- nigSa_summer10s%>%
  filter(Extract_Sanigripes_summer10s >0)
av <- mean(x$Extract_Sanigripes_summer10s) 
av
mx <- max(x$Extract_Sanigripes_summer10s)
mx
mn <- min(x$Extract_Sanigripes_summer10s)
mn

# Salinity immer -----

imSa_fall90s <- raster("Extract_Saimmer_fall90s.tif")
imSa_fall90s <- as.data.frame(imSa_fall90s, xy = TRUE)
x <- imSa_fall90s%>%
  filter(Extract_Saimmer_fall90s >0)
av <- mean(x$Extract_Saimmer_fall90s) #number of occurrence points
av
mx <- max(x$Extract_Saimmer_fall90s)
mx
mn <- min(x$Extract_Saimmer_fall90s)
mn

imSa_fall00s <- raster("Extract_Saimmer_fall00s.tif")
imSa_fall00s <- as.data.frame(imSa_fall00s, xy = TRUE)
x <- imSa_fall00s%>%
  filter(Extract_Saimmer_fall00s >0)
av <- mean(x$Extract_Saimmer_fall00s) #number of occurrence points
av
mx <- max(x$Extract_Saimmer_fall00s)
mx
mn <- min(x$Extract_Saimmer_fall00s)
mn

imSa_fall10s <- raster("Extract_Saimmer_fall10s.tif")
imSa_fall10s <- as.data.frame(imSa_fall10s, xy = TRUE)
x <- imSa_fall10s%>%
  filter(Extract_Saimmer_fall10s >0)
av <- mean(x$Extract_Saimmer_fall10s) #number of occurrence points
av
mx <- max(x$Extract_Saimmer_fall10s)
mx
mn <- min(x$Extract_Saimmer_fall10s)
mn

imSa_winter90s <- raster("Extract_Saimmer_winter90s.tif")
imSa_winter90s <- as.data.frame(imSa_winter90s, xy = TRUE)
x <- imSa_winter90s %>%
   filter(Extract_Saimmer_winter90s >0)
av <- mean(x$Extract_Saimmer_winter90s) #cannot have negative salinity, all 0s
av
mx <- max(x$Extract_Saimmer_winter90s)
mx
mn <- min(x$Extract_Saimmer_winter90s)
mn

imSa_winter00s <- raster("Extract_Saimmer_winter00s.tif")
imSa_winter00s <- as.data.frame(imSa_winter00s, xy = TRUE)
x <- imSa_winter00s %>%
  filter(Extract_Saimmer_winter00s >0)
av <- mean(x$Extract_Saimmer_winter00s) 
av
mx <- max(x$Extract_Saimmer_winter00s)
mx
mn <- min(x$Extract_Saimmer_winter00s)
mn

imSa_winter10s <- raster("Extract_Saimmer_winter10s.tif")
imSa_winter10s <- as.data.frame(imSa_winter10s, xy = TRUE)
x <- imSa_winter10s%>%
  filter(Extract_Saimmer_winter10s >0)
av <- mean(x$Extract_Saimmer_winter10s) 
av
mx <- max(x$Extract_Saimmer_winter10s)
mx
mn <- min(x$Extract_Saimmer_winter10s)
mn

imSa_spring90s <- raster("Extract_Saimmer_spring90s.tif")
imSa_spring90s <- as.data.frame(imSa_spring90s, xy = TRUE)
x <- imSa_spring90s %>%
  filter(Extract_Saimmer_spring90s >0)
av <- mean(x$Extract_Saimmer_spring90s) 
av
mx <- max(x$Extract_Saimmer_spring90s)
mx
mn <- min(x$Extract_Saimmer_spring90s)
mn

imSa_spring00s <- raster("Extract_Saimmer_spring00s.tif")
imSa_spring00s <- as.data.frame(imSa_spring00s, xy = TRUE)
x <- imSa_spring00s%>%
  filter(Extract_Saimmer_spring00s >0)
av <- mean(x$Extract_Saimmer_spring00s) #number of occurrence points
av
mx <- max(x$Extract_Saimmer_spring00s)
mx
mn <- min(x$Extract_Saimmer_spring00s)
mn

imSa_spring10s <- raster("Extract_Saimmer_spring10s.tif")
imSa_spring10s <- as.data.frame(imSa_spring10s, xy = TRUE)
x <- imSa_spring10s%>%
  filter(Extract_Saimmer_spring10s >0)
av <- mean(x$Extract_Saimmer_spring10s) #number of occurrence points
av
mx <- max(x$Extract_Saimmer_spring10s)
mx
mn <- min(x$Extract_Saimmer_spring10s)
mn

imSa_summer00s <- raster("Extract_Saimmer_summer00s.tif")
imSa_summer00s <- as.data.frame(imSa_summer00s, xy = TRUE)
x <- imSa_summer00s%>%
  filter(Extract_Saimmer_summer00s >0)
av <- mean(x$Extract_Saimmer_summer00s) #number of occurrence points
av
mx <- max(x$Extract_Saimmer_summer00s)
mx
mn <- min(x$Extract_Saimmer_summer00s)
mn

imSa_summer10s <- raster("Extract_Saimmer_summer10s.tif")
imSa_summer10s <- as.data.frame(imSa_summer10s, xy = TRUE)
x <- imSa_summer10s%>%
  filter(Extract_Saimmer_summer10s >0)
av <- mean(x$Extract_Saimmer_summer10s) #number of occurrence points
av
mx <- max(x$Extract_Saimmer_summer10s)
mx
mn <- min(x$Extract_Saimmer_summer10s)
mn

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

# Sea surface temperature nigripes----

nigSST_fall90s <- raster("Extract_SSTnigripes_fall90s.tif")
nigSST_fall90s <- as.data.frame(nigSST_fall90s, xy = TRUE)
x <- nigSST_fall90s%>%
  filter(Extract_SSTnigripes_fall90s >0)
av <- mean(x$Extract_SSTnigripes_fall90s) 
av
mx <- max(x$Extract_SSTnigripes_fall90s)
mx
mn <- min(x$Extract_SSTnigripes_fall90s)
mn

nigSST_fall10s <- raster("Extract_SSTnigripes_fall10s.tif")
nigSST_fall10s <- as.data.frame(nigSST_fall10s, xy = TRUE)
x <- nigSST_fall10s %>%
  filter(Extract_SSTnigripes_fall10s >0)
av <- mean(x$Extract_SSTnigripes_fall10s) 
av
mx <- max(x$Extract_SSTnigripes_fall10s)
mx
mn <- min(x$Extract_SSTnigripes_fall10s)
mn

nigSST_spring90s <- raster("Extract_SSTnigripes_spring90s.tif")
nigSST_spring90s <- as.data.frame(nigSST_spring90s, xy = TRUE)
x <- nigSST_spring90s%>%
  filter(Extract_SSTnigripes_spring90s >0)
av <- mean(x$Extract_SSTnigripes_spring90s) 
av
mx <- max(x$Extract_SSTnigripes_spring90s)
mx
mn <- min(x$Extract_SSTnigripes_spring90s)
mn

nigSST_summer90s <- raster("Extract_SSTnigripes_summer90s.tif")
nigSST_summer90s <- as.data.frame(nigSST_summer90s, xy = TRUE)
x <- nigSST_summer90s%>%
  filter(Extract_SSTnigripes_summer90s >0)
av <- mean(x$Extract_SSTnigripes_summer90s) 
av
mx <- max(x$Extract_SSTnigripes_summer90s)
mx
mn <- min(x$Extract_SSTnigripes_summer90s)
mn

nigSST_summer00s <- raster("Extract_SSTnigripes_summer00s.tif")
nigSST_summer00s <- as.data.frame(nigSST_summer00s, xy = TRUE)
x <- nigSST_summer00s %>%
  filter(Extract_SSTnigripes_summer00s >0)
av <- mean(x$Extract_SSTnigripes_summer00s) 
av
mx <- max(x$Extract_SSTnigripes_summer00s)
mx
mn <- min(x$Extract_SSTnigripes_summer00s)
mn

nigSST_summer10s <- raster("Extract_SSTnigripes_summer10s.tif")
nigSST_summer10s <- as.data.frame(nigSST_summer10s, xy = TRUE)
x <- nigSST_summer10s%>%
  filter(Extract_SSTnigripes_summer10s >0)
av <- mean(x$Extract_SSTnigripes_summer10s) 
av
mx <- max(x$Extract_SSTnigripes_summer10s)
mx
mn <- min(x$Extract_SSTnigripes_summer10s)
mn

# Sea surface temperature immer ----

imSST_fall90s <- raster("Extract_SSTimmer_fall90s.tif")
imSST_fall90s <- as.data.frame(imSST_fall90s, xy = TRUE)
x <- imSST_fall90s%>%
  filter(Extract_SSTimmer_fall90s >0)
av <- mean(x$Extract_SSTimmer_fall90s) #number of occurrence points
av
mx <- max(x$Extract_SSTimmer_fall90s)
mx
mn <- min(x$Extract_SSTimmer_fall90s)
mn

imSST_fall00s <- raster("Extract_SSTimmer_fall00s.tif")
imSST_fall00s <- as.data.frame(imSST_fall00s, xy = TRUE)
x <- imSST_fall00s%>%
  filter(Extract_SSTimmer_fall00s >0)
av <- mean(x$Extract_SSTimmer_fall00s) #number of occurrence points
av
mx <- max(x$Extract_SSTimmer_fall00s)
mx
mn <- min(x$Extract_SSTimmer_fall00s)
mn

imSST_fall10s <- raster("Extract_SSTimmer_fall10s.tif")
imSST_fall10s <- as.data.frame(imSST_fall10s, xy = TRUE)
x <- imSST_fall10s%>%
  filter(Extract_SSTimmer_fall10s >0)
av <- mean(x$Extract_SSTimmer_fall10s) #number of occurrence points
av
mx <- max(x$Extract_SSTimmer_fall10s)
mx
mn <- min(x$Extract_SSTimmer_fall10s)
mn

imSST_winter90s <- raster("Extract_SSTimmer_winter90s.tif")
imSST_winter90s <- as.data.frame(imSST_winter90s, xy = TRUE)
x <- imSST_winter90s%>%
  filter(Extract_SSTimmer_winter90s >0)
av <- mean(x$Extract_SSTimmer_winter90s) #number of occurrence points
av
mx <- max(x$Extract_SSTimmer_winter90s)
mx
mn <- min(x$Extract_SSTimmer_winter90s)
mn

imSST_winter00s <- raster("Extract_SSTimmer_winter00s.tif")
imSST_winter00s <- as.data.frame(imSST_winter00s, xy = TRUE)
x <- imSST_winter00s%>%
  filter(Extract_SSTimmer_winter00s >0)
av <- mean(x$Extract_SSTimmer_winter00s) #number of occurrence points
av
mx <- max(x$Extract_SSTimmer_winter00s)
mx
mn <- min(x$Extract_SSTimmer_winter00s)
mn

imSST_winter10s <- raster("Extract_SSTimmer_winter10.tif")
imSST_winter10s <- as.data.frame(imSST_winter10s, xy = TRUE)
x <- imSST_winter10s%>%
  filter(Extract_SSTimmer_winter10 >0)
av <- mean(x$Extract_SSTimmer_winter10) #number of occurrence points
av
mx <- max(x$Extract_SSTimmer_winter10)
mx
mn <- min(x$Extract_SSTimmer_winter10)
mn

imSST_spring90s <- raster("Extract_SSTimmer_spring90s.tif")
imSST_spring90s <- as.data.frame(imSST_spring90s, xy = TRUE)
x <- imSST_spring90s%>%
  filter(Extract_SSTimmer_spring90s >0)
av <- mean(x$Extract_SSTimmer_spring90s) #number of occurrence points
av
mx <- max(x$Extract_SSTimmer_spring90s)
mx
mn <- min(x$Extract_SSTimmer_spring90s)
mn

imSST_spring00s <- raster("Extract_SSTimmer_spring00.tif")
imSST_spring00s <- as.data.frame(imSST_spring00s, xy = TRUE)
x <- imSST_spring00s%>%
  filter(Extract_SSTimmer_spring00 >0)
av <- mean(x$Extract_SSTimmer_spring00) #number of occurrence points
av
mx <- max(x$Extract_SSTimmer_spring00)
mx
mn <- min(x$Extract_SSTimmer_spring00)
mn

imSST_spring10s <- raster("Extract_SSTimmer_spring10.tif")
imSST_spring10s <- as.data.frame(imSST_spring10s, xy = TRUE)
x <- imSST_spring10s%>%
  filter(Extract_SSTimmer_spring10 >0)
av <- mean(x$Extract_SSTimmer_spring10) #number of occurrence points
av
mx <- max(x$Extract_SSTimmer_spring10)
mx
mn <- min(x$Extract_SSTimmer_spring10)
mn

imSST_summer00s <- raster("Extract_SSTimmer_summer00.tif")
imSST_summer00s <- as.data.frame(imSST_summer00s, xy = TRUE)
x <- imSST_summer00s%>%
  filter(Extract_SSTimmer_summer00 >0)
av <- mean(x$Extract_SSTimmer_summer00) #number of occurrence points
av
mx <- max(x$Extract_SSTimmer_summer00)
mx
mn <- min(x$Extract_SSTimmer_summer00)
mn

imSST_summer10s <- raster("Extract_SSTimmer_summer10.tif")
imSST_summer10s <- as.data.frame(imSST_summer10s, xy = TRUE)
x <- imSST_summer10s%>%
  filter(Extract_SSTimmer_summer10 >0)
av <- mean(x$Extract_SSTimmer_summer10) #number of occurrence points
av
mx <- max(x$Extract_SSTimmer_summer10)
mx
mn <- min(x$Extract_SSTimmer_summer10)
mn

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
# Sea surface height nigripes ----

nigSSH_fall90s <- raster("Extract_SSHnigripes_fall90s.tif")
nigSSH_fall90s <- as.data.frame(nigSSH_fall90s, xy = TRUE)
x <- nigSSH_fall90s%>%
  filter(Extract_SSHnigripes_fall90s >-10)
av <- mean(x$Extract_SSHnigripes_fall90s) 
av
mx <- max(x$Extract_SSHnigripes_fall90s)
mx
mn <- min(x$Extract_SSHnigripes_fall90s)
mn

nigSSH_fall10s <- raster("Extract_SSHnigripes_fall10s.tif")
nigSSH_fall10s <- as.data.frame(nigSSH_fall10s, xy = TRUE)
x <- nigSSH_fall10s %>%
  filter(Extract_SSHnigripes_fall10s >-10)
av <- mean(x$Extract_SSHnigripes_fall10s) 
av
mx <- max(x$Extract_SSHnigripes_fall10s)
mx
mn <- min(x$Extract_SSHnigripes_fall10s)
mn

nigSSH_spring90s <- raster("Extract_SSHnigripes_spring90s.tif")
nigSSH_spring90s <- as.data.frame(nigSSH_spring90s, xy = TRUE)
x <- nigSSH_spring90s%>%
  filter(Extract_SSHnigripes_spring90s >-10)
av <- mean(x$Extract_SSHnigripes_spring90s) 
av
mx <- max(x$Extract_SSHnigripes_spring90s)
mx
mn <- min(x$Extract_SSHnigripes_spring90s)
mn

nigSSH_summer90s <- raster("Extract_SSHnigripes_summer90s.tif")
nigSSH_summer90s <- as.data.frame(nigSSH_summer90s, xy = TRUE)
x <- nigSSH_summer90s%>%
  filter(Extract_SSHnigripes_summer90s >-10)
av <- mean(x$Extract_SSHnigripes_summer90s) 
av
mx <- max(x$Extract_SSHnigripes_summer90s)
mx
mn <- min(x$Extract_SSHnigripes_summer90s)
mn

nigSSH_summer00s <- raster("Extract_SSHnigripes_summer00s.tif")
nigSSH_summer00s <- as.data.frame(nigSSH_summer00s, xy = TRUE)
x <- nigSSH_summer00s %>%
  filter(Extract_SSHnigripes_summer00s >-10)
av <- mean(x$Extract_SSHnigripes_summer00s) 
av
mx <- max(x$Extract_SSHnigripes_summer00s)
mx
mn <- min(x$Extract_SSHnigripes_summer00s)
mn

nigSSH_summer10s <- raster("Extract_SSHnigripes_summer10s.tif")
nigSSH_summer10s <- as.data.frame(nigSSH_summer10s, xy = TRUE)
x <- nigSSH_summer10s%>%
  filter(Extract_SSHnigripes_summer10s >-10)
av <- mean(x$Extract_SSHnigripes_summer10s) 
av
mx <- max(x$Extract_SSHnigripes_summer10s)
mx
mn <- min(x$Extract_SSHnigripes_summer10s)
mn
# Sea surface height immer ----

imSSH_fall90s <- raster("Extract_SSHimmer_fall90s.tif")
imSSH_fall90s <- as.data.frame(imSSH_fall90s, xy = TRUE)
x <- imSSH_fall90s%>%
  filter(Extract_SSHimmer_fall90s >-10)
av <- mean(x$Extract_SSHimmer_fall90s) #number of occurrence points
av
mx <- max(x$Extract_SSHimmer_fall90s)
mx
mn <- min(x$Extract_SSHimmer_fall90s)
mn

imSSH_fall00s <- raster("Extract_SSHimmer_fall00s.tif")
imSSH_fall00s <- as.data.frame(imSSH_fall00s, xy = TRUE)
x <- imSSH_fall00s%>%
  filter(Extract_SSHimmer_fall00s >-10)
av <- mean(x$Extract_SSHimmer_fall00s) #number of occurrence points
av
mx <- max(x$Extract_SSHimmer_fall00s)
mx
mn <- min(x$Extract_SSHimmer_fall00s)
mn

imSSH_fall10s <- raster("Extract_SSHimmer_fall10s.tif")
imSSH_fall10s <- as.data.frame(imSSH_fall10s, xy = TRUE)
x <- imSSH_fall10s%>%
  filter(Extract_SSHimmer_fall10s >-10)
av <- mean(x$Extract_SSHimmer_fall10s) #number of occurrence points
av
mx <- max(x$Extract_SSHimmer_fall10s)
mx
mn <- min(x$Extract_SSHimmer_fall10s)
mn

imSSH_winter90s <- raster("Extract_SSHimmer_winter90s.tif")
imSSH_winter90s <- as.data.frame(imSSH_winter90s, xy = TRUE)
x <- imSSH_winter90s%>%
  filter(Extract_SSHimmer_winter90s >-10)
av <- mean(x$Extract_SSHimmer_winter90s) #number of occurrence points
av
mx <- max(x$Extract_SSHimmer_winter90s)
mx
mn <- min(x$Extract_SSHimmer_winter90s)
mn

imSSH_winter00s <- raster("Extract_SSHimmer_winter00s.tif")
imSSH_winter00s <- as.data.frame(imSSH_winter00s, xy = TRUE)
x <- imSSH_winter00s%>%
  filter(Extract_SSHimmer_winter00s >-10)
av <- mean(x$Extract_SSHimmer_winter00s) #number of occurrence points
av
mx <- max(x$Extract_SSHimmer_winter00s)
mx
mn <- min(x$Extract_SSHimmer_winter00s)
mn

imSSH_winter10s <- raster("Extract_SSHimmer_winter10.tif")
imSSH_winter10s <- as.data.frame(imSSH_winter10s, xy = TRUE)
x <- imSSH_winter10s%>%
  filter(Extract_SSHimmer_winter10 >-10)
av <- mean(x$Extract_SSHimmer_winter10) #number of occurrence points
av
mx <- max(x$Extract_SSHimmer_winter10)
mx
mn <- min(x$Extract_SSHimmer_winter10)
mn

imSSH_spring90s <- raster("Extract_SSHimmer_spring90s.tif")
imSSH_spring90s <- as.data.frame(imSSH_spring90s, xy = TRUE)
x <- imSSH_spring90s%>%
  filter(Extract_SSHimmer_spring90s >-10)
av <- mean(x$Extract_SSHimmer_spring90s) #number of occurrence points
av
mx <- max(x$Extract_SSHimmer_spring90s)
mx
mn <- min(x$Extract_SSHimmer_spring90s)
mn

imSSH_spring00s <- raster("Extract_SSHimmer_spring00.tif")
imSSH_spring00s <- as.data.frame(imSSH_spring00s, xy = TRUE)
x <- imSSH_spring00s%>%
  filter(Extract_SSHimmer_spring00 >-10)
av <- mean(x$Extract_SSHimmer_spring00) #number of occurrence points
av
mx <- max(x$Extract_SSHimmer_spring00)
mx
mn <- min(x$Extract_SSHimmer_spring00)
mn

imSSH_spring10s <- raster("Extract_SSHimmer_spring10.tif")
imSSH_spring10s <- as.data.frame(imSSH_spring10s, xy = TRUE)
x <- imSSH_spring10s%>%
  filter(Extract_SSHimmer_spring10 >-10)
av <- mean(x$Extract_SSHimmer_spring10) #number of occurrence points
av
mx <- max(x$Extract_SSHimmer_spring10)
mx
mn <- min(x$Extract_SSHimmer_spring10)
mn

imSSH_summer00s <- raster("Extract_SSHimmer_summer00.tif")
imSSH_summer00s <- as.data.frame(imSSH_summer00s, xy = TRUE)
x <- imSSH_summer00s%>%
  filter(Extract_SSHimmer_summer00 >-10)
av <- mean(x$Extract_SSHimmer_summer00) #number of occurrence points
av
mx <- max(x$Extract_SSHimmer_summer00)
mx
mn <- min(x$Extract_SSHimmer_summer00)
mn

imSSH_summer10s <- raster("Extract_SSHimmer_summer10.tif")
imSSH_summer10s <- as.data.frame(imSSH_summer10s, xy = TRUE)
x <- imSSH_summer10s%>%
  filter(Extract_SSHimmer_summer10 >-10)
av <- mean(x$Extract_SSHimmer_summer10) #number of occurrence points
av
mx <- max(x$Extract_SSHimmer_summer10)
mx
mn <- min(x$Extract_SSHimmer_summer10)
mn

# import summary data
immerSummary    <- read.csv("C:/Users/savan/Documents/github/AvesOBIS/immerSummary.csv")
immerSummary    <- immerSummary %>% 
  column_to_rownames(var="X")
nigSummary      <- read.csv("C:/Users/savan/Documents/github/AvesOBIS/nigSummary.csv")
nigSummary      <- nigSummary %>% 
  column_to_rownames(var="X")
grisWestSummary <- read.csv("C:/Users/savan/Documents/github/AvesOBIS/grisWestSummary.csv")
grisWestSummary    <- grisWestSummary %>% 
  column_to_rownames(var="X")
grisEastSummary <- read.csv("C:/Users/savan/Documents/github/AvesOBIS/grisEastSummary.csv")
grisEastSummary    <- grisEastSummary %>% 
  column_to_rownames(var="X")

# Parse out the lat/lon and env characteristics and place them in data frames:
grisWestEnv     <- grisWestSummary[,3:ncol(grisWestSummary)]    # Enviro variables
grisWestLatLon  <- grisWestSummary[,1:2]                        # Lat/long variables
grisEastEnv     <- grisEastSummary[,3:ncol(grisEastSummary)]
grisEastLatLon  <- grisEastSummary[,1:2]
nigEnv          <- nigSummary[,3:ncol(nigSummary)]
nigLatLon       <- nigSummary[,1:2]
immerEnv        <- immerSummary[,3:ncol(immerSummary)]
immerLatLon     <- immerSummary[,1:2]

# Visually investigate the data standardization.
# grisWest ----

# Using the 'par' function in conjunction with the 'mfrow' parameter, you can 
# set the graphical window to default to a two rows with two figures in each:
par(mfrow = c(2,2))

# Plot the RAW data in the first row:
boxplot(grisWestEnv)
title(main = 'Environmental P. griseus West Data',
      x    = 'Predictor Var.',
      y    = 'Measured Value')

boxplot(grisWestLatLon)
title(main = 'Occurrence Data',
      x    = 'Response Var.',
      y    = 'Measured Value')

# Plot the Z-Scored data in the second row (same order as 1st row):

boxplot(scale(grisWestEnv))
title(main = 'Z-Score Environmental P. griseus West Data',
      x    = 'Predictor Var.',
      y    = 'Measured Value')

# no need to do this for lat/lon values

# Reset the plotting window:
par(mfrow=c(1,1))

# Env dataset made up of continuous variables with different units of measure, 
# the best approach is to standardize the data using Z-Scores standardization
# !!!!RDA algorithm automatically performs this action for the PREDICTOR data!!!!

# Use an AIC-based stepwise variable selection procedure with a distance-based
# RDA (db-RDA) approach to build an optimal (most parsimonious) model depicting 
# the relationship between the fish community and the set of associated 
# environmental variables.

# Create a euclidean dissimilarity matrix:
disY      <- vegdist(grisWestLatLon, method = 'euclidean')  # <-- No data transformation applied

# Create the "null" and "full" models described above:
null      <- rda(disY ~ 1, grisWestEnv)
full      <- rda(disY ~ ., grisWestEnv)

# Now perform the AIC stepwise selection process:
optGrisWest       <- step(object = null, scope = formula(full)) #optimal model selected by AIC selection

# Perform RDA:
rdagrisWest        <- rda(X = grisWestLatLon, Y = grisWestEnv)

# Produce and display the summary results:
rdagrisWest_sum    <- summary(rdagrisWest)
rdagrisWest_sum

# Perform the permutation test for the RDA test statistic:

permutest(x = rdagrisWest, permutations = 9999)

# Classical RDA is the most appropriate statistical test for this multivariate 
# analysis because all of the variables under consideration are CONTINUOUS 
# variables that are not subject to issues such as the "double-zero" effect. True?

# The R^2 value indicates that over half (53.8%) of the total variability in
# occurrence within a population can be accounted for (explained) by the 
# environmental data. Therefore, the environmental characteristics can
# be said to partially affect the location of P. griseus on the west coast over the
# past 3 decades.

# Create the ordination diagram.

# Extract the percent of the variability explained by each axis:
pct_explGrisWest  <- 100 * rdagrisWest_sum$cont$importance[2,]

# Plot the data
plot(x    = rdagrisWest, scaling = "sites", const = c(1,3),
     main = "Redundancy Analysis",
     xlab = paste("RDA Axis-I (",  round(pct_explGrisWest[1],2),"%)" ,sep = ""),
     ylab = paste("RDA Axis-II (", round(pct_explGrisWest[2],2),"%)" ,sep = ""))

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
  bind_rows(grisEV_eastsummer90s)%>%
  
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
  
dat <- left_join(grisEastSa,grisEastML)%>%
  left_join(grisEastSSH)%>%
  left_join(grisEastSST)%>%
  left_join(grisEastEV)%>%
  left_join(grisEastNV)
dat[is.na(dat)] <- 0

dat <- dat %>%
  filter(Sa > 0,ML >= 0)

grisEastLatLon <- dat[,1:2]
grisEastEnv    <- dat[,5:ncol(dat)]

# Use an AIC-based stepwise variable selection procedure with a RDA approach
# to build an optimal (most parsimonious) model depicting the relationship
# between the fish community and the set of associated environmental variables.

# Create the "null" and "full" models described above:
null      <- rda(grisEastLatLon ~ 1, grisEastEnv)
full      <- rda(grisEastLatLon ~ ., grisEastEnv)

# Now perform the AIC stepwise selection process:
optGrisEast       <- step(object = null, scope = formula(full)) #optimal model selected by AIC selection

# Perform RDA -------------------------------------------------------------
permutest(x = optGrisEast, permutations = 9999) 
# very significant P value (Pr(>F))
# model is F ratio 148 (148x more variability explained than by random chance)

# Produce and display the summary results:
optGrisEast_sum    <- summary(optGrisEast)
optGrisEast_sum

# Create the ordination diagram.
# permutation significant 
# Extract the percent of the variability explained by each axis:
pct_explGrisEast  <- 100 * optgrisEast_sum$cont$importance[2,]

scores_GrisEast <- scores(optGrisEast)
dat <- cbind(dat, scores_GrisEast$sites)

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
