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
x1 <- Aves_NA[sample(1:nrow(Aves_NA), 350000, replace=TRUE),] 
