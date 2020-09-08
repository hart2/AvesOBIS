# Messing around with iNEXT, generate rarefaction curves

install.packages("iNEXT")
library(iNEXT)
setwd("~/github/AvesOBIS")
source("scripts/covstop.R")
Avesnxt <- Aves %>% 
  select(scientificName,decimalLongitude,decimalLatitude,date_year,individualCount)

# separate aves by lme, then have each regions abundance (each species' abundance?)
out1 <- iNEXT(Avesnxt, q=0, datatype="abundance")
ggiNEXT(out1)



# messing around with covstop
# A function that takes a sample (rows)-by-species (columns) community matrix and
# uses coverage-based subsampling to identify the number of samples of individuals 
# required to achieve a given level of coverage of total biodiversity.

# From: Chao, Anne, and Lou Jost. "Coverage‐based rarefaction and extrapolation:
# standardizing samples by completeness rather than size." Ecology 93.12 (2012): 2533-2547.

# generate fake community matrix
vec <- sample(0:100, 100, replace = T)

vec[sample(1:100, 50)] <- 0

mat <- matrix(vec, nrow = 10)

# determine number of samples required
covstop(mat, type = "samples")

# determine number of samples required for 99% coverage
covstop(mat, Cn = 0.98)

# determine number of individuals required
covstop(mat, type = "individuals")