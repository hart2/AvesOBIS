# Code written by Savannah Hartman for dissertation chapter 1
# September 8th, 2020 

library(tidyverse)
library(iNEXT)
library(devtools)
library(ggplot2)

source("~/github/AvesOBIS/scripts/AvesOBISscript.R")
source("~/github/AvesOBIS/scripts/covstop.R")

usgs <- Aves %>% 
  filter(ownerInstitutionCode == "USGS-FWS") #has multiple sampling methods

eco <- Aves %>% 
  filter(ownerInstitutionCode == "NORMANDEAU") #aerial surveys

# write csv
write.csv(eco, "./ecoSites.csv")

# Read in site lat/longs
sites <- read.csv("./ecoSites.csv", header = T)

# Generate interpolation/extrapolation curves for each locality (or site)

rare <- do.call(rbind, lapply(unique(eco$institutionCode), function(i) {
  
  x <- subset(sites, institutionCode == i)
    
    if(nrow(x) == 0) data.frame() else {
      
      # summarize by locality
      x$presence <- 1
      
      # Cast longways
      mat <- x %>% select(X, scientificName, presence) %>% 
        
        pivot_wider(id_cols = c(X), names_from = scientificName, values_from = presence) 
      
      mat[is.na(mat)] <- 0
      
      dnames <- list(colnames(mat)[-(1:2)], as.character(mat$X))
      
      mat <- t(as.matrix(mat)[, -(1:2), drop = FALSE])
      
      mat <- apply(mat, 2, as.numeric)
      
      dimnames(mat) <- dnames
      
      z <- as.incfreq(mat) 
      
      out <- iNEXT(z, datatype = "incidence_freq") 
      
      ret <- out$iNextEst
      
      idx <- which(ret$method == "observed")
      
      ret <- rbind.data.frame(
        ret[ret$method == "interpolated", ],
        ret[idx, ],
        ret[idx, ],
        ret[idx, ],
        ret[ret$method == "extrapolated", ]
      )
      
      ret[idx, "method"] <- "interpolated"
      
      ret[idx + 2, "method"] <- "extrapolated"
      
      data.frame(
        datasetName = i,
        ret[, c(1:2, 4)]
      )
      
    }
    
  } ) )
  
# Plot results for the subdatasets collected by Normandeau
rareplot_2 <- ggplot() +
    geom_line(data = subset(rare, method == "interpolated"), aes(x = t, y = qD, col = datasetName)) + 
    geom_line(data = subset(rare, method == "extrapolated"), aes(x = t, y = qD, col = datasetName), lty = 3) + 
    geom_point(data = subset(rare, method == "observed"), aes(x = t, y = qD, col = datasetName), size = 2) + 
    labs(x = "Number of samples", y = "Species richness") + 
    theme_bw(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # legend.position = "none"
      )
ggsave(rareplot_2, device = "pdf", width = 10, height = 5, units = "in")

mms <- Aves %>% 
  filter (ownerInstitutionCode == "DOI;BOEM") #aerial surveys

calcofi <- Aves %>% 
  filter(ownerInstitutionCode == "PRBO Conservation Science") #ship surveys

pirop <- Aves %>% 
  filter(ownerInstitutionCode == "Canadian Wildlife Service") #ship surveys

