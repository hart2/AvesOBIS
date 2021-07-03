# This script subsamples datasets
# Written by S. Hartman (2021)

library(tidyverse) # packages for data visualization
# library(vegan)
library(dplyr)
library(readr)
library(devtools)
library(ggplot2)

#sampling without replacement, covariance is not 0
# x1 <- Aves_NA[sample(1:nrow(Aves_NA), 50, replace=FALSE),] 
# x2 <- Aves_NA[sample(1:nrow(Aves_NA), 200000, replace=FALSE),]

# bootstrap resampling: sampling the same number of items WITH replacement, covariance is 0
# x1 <- Aves_NA[sample(1:nrow(Aves_NA), 350000, replace=TRUE),] 

# Randomly subsetting dataset in arcGIS using Subset Features tool with a 1 km grid
# Upload "AvesSubset#.csv"

# Some of the rarer species aren't identified, Majority of 1-3 points per grid cell
num <- AvesSubset1_20perc %>% 
  select(scientificName) 
freq1 <- as.data.frame(table(num)) 

num <- AvesSubset2_20perc %>% 
  select(scientificName) 
freq2 <- as.data.frame(table(num)) 

num <- AvesSubset3_20perc %>% 
  select(scientificName) 
freq3 <- as.data.frame(table(num))

# When you use a higher percentage of the dataset for random subsampling, ~10 
# of the rarer species are identified. Majority of 3-6 points per grid cell...
num <- AvesSubset4_50perc %>% 
  select(scientificName) 
freq4 <- as.data.frame(table(num))

num <- AvesSubset5_50perc %>% 
  select(scientificName) 
freq5 <- as.data.frame(table(num))