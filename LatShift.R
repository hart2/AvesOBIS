# Latitudinal shift in Atlantic and Pacific Oceans
# Using centroid points see what environmental data is causing shift
#   Possibly use vegan package? constrained ordination model -> canonical correspondence analysis
# Need environmental data for the specific points in time (Atlantic -> 
#   care about environmental parameters for the GUlf Stream and Northern Atlantic 
#   (SST, SSH, Surf Current, RTG: https://polar.ncep.noaa.gov/global/monitor/), 
#   Pacific -> northern shift due to temperature)

# input table for Phoebastria nigripes, Puffinus griseus, Gavia immer
library(vegan)
library(tidyverse)
latshift <- read.csv("C:/Users/savan/Desktop/Mean Center Clusters/LatShift.csv")
