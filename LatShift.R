# Code written by Savannah Hartman, August 2022

# Latitudinal shift in Atlantic and Pacific Oceans
# Using centroid points see what environmental data is causing shift
#   Use UTM coordinates, Use vegan package, classical rda
# Need environmental data for the specific points in time (Atlantic -> 
#   care about environmental parameters for the Gulf Stream and Northern Atlantic 
#   (SST, SSH, Surf Current, RTG: https://polar.ncep.noaa.gov/global/monitor/),
#   (https://coastwatch.noaa.gov/cw/satellite-data-products.html) SST, SSH, 
#   Ocean color, Sea surface Winds
#   Pacific -> northern shift due to temperature)

# input table for Phoebastria nigripes, Puffinus griseus, Gavia immer
library(vegan)
library(tidyverse)
latshift <- read.csv("C:/Users/savan/Desktop/Mean Center Clusters/LatShift.csv")
