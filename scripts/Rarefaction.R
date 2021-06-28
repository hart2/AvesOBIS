# Code written by Savannah Hartman for dissertation chapter 1
# September 8th, 2020 

library(tidyverse)
library(tidyr)
library(iNEXT)
library(dplyr)
library(devtools)
library(ggplot2)

source("~/github/AvesOBIS/scripts/AvesOBISscript.R")
source("~/github/AvesOBIS/scripts/covstop.R")


# USGS (USGS-FWS)---------------------------------------------------------------
usgs <- Aves_NA %>%
  filter(ownerInstitutionCode == "USGS-FWS") #has multiple sampling methods

# write csv
write.csv(usgs, "./usgsSites.csv")

# Read in site lat/longs
sites <- read.csv("./usgsSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare <- do.call(rbind, lapply(unique(usgs$datasetName), function(i) {
  
  
  # read in data
  x <- subset(sites, datasetName == i)
  
  if(nrow(x) == 0) data.frame() else {
    
    # summarize by dataset
    x$presence <- 1
    
    # Cast longways
    mat <- x %>% select(X, scientificName) %>% 
      
      pivot_wider(id_cols = c(X), names_from = scientificName) 
    
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
      totsamples = nrow(mat),
      minsamples = covstop(mat[, -1])
    )
  }
} ) )

# Plot results for the subdatasets collected
rareplot_0 <- ggplot() +
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
ggsave("USGS_data.png", rareplot_0, width = 10, height = 5, units = "in")

### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- rare$minsamples
collected_samps <- rare$totsamples

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage


# Eco (Normandeau)--------------------------------------------------------------

eco <- Aves_NA %>% 
  filter(ownerInstitutionCode == "NORMANDEAU") #aerial surveys

# write csv
write.csv(eco, "./ecoSites.csv")

# Read in site lat/longs
sites <- read.csv("./ecoSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare <- do.call(rbind, lapply(unique(eco$datasetName), function(i) {
  
  x <- subset(sites, datasetName == i)
    
    if(nrow(x) == 0) data.frame() else {
      
      # summarize by dataset
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
rareplot_1 <- ggplot() +
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
ggsave("Normandeau_data.png", rareplot_1, width = 10, height = 5, units = "in")

### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare1, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- mean(rare1$qD)
collected_samps <- nrow(eco)

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage

# MMS (BOEM) ---------------------------------------------------------------------
mms <- Aves_NA %>% 
  filter (ownerInstitutionCode == "DOI;BOEM") #aerial surveys

write.csv(mms, "./mmsSites.csv")
sites <- read.csv("./mmsSites.csv", header = T)

rare2 <- do.call(rbind, lapply(unique(mms$datasetName), function(i) {
  
  x <- subset(sites, datasetName == i)
  
  if(nrow(x) == 0) data.frame() else {
    
    # summarize by dataset
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

# Plot results for the subdatasets collected
rareplot_2 <- ggplot() +
  geom_line(data = subset(rare2, method == "interpolated"), aes(x = t, y = qD, col = datasetName)) + 
  geom_line(data = subset(rare2, method == "extrapolated"), aes(x = t, y = qD, col = datasetName), lty = 3) + 
  geom_point(data = subset(rare2, method == "observed"), aes(x = t, y = qD, col = datasetName), size = 2) + 
  labs(x = "Number of samples", y = "Species richness") + 
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.position = "none"
  )

ggsave("BOEM_data.png", rareplot_2, width = 10, height = 5, units = "in")

### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare2, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- mean(rare2$qD)
collected_samps <- nrow(mms)

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage

# Pirop (Canadian Wildlife Service)---------------------------------------------

pirop <- Aves_NA %>% 
  filter(ownerInstitutionCode == "Canadian Wildlife Service") #ship surveys

# write csv
write.csv(pirop, "./piropSites.csv")

# Read in site lat/longs
sites <- read.csv("./piropSites.csv", header = T)

rare4 <- do.call(rbind, lapply(unique(pirop$datasetName), function(i) {
  
  x <- subset(sites, datasetName == i)
  
  if(nrow(x) == 0) data.frame() else {
    
    # summarize by dataset
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

# Plot results for the subdatasets collected
rareplot_4 <- ggplot() +
  geom_line(data = subset(rare4, method == "interpolated"), aes(x = t, y = qD, col = datasetName)) + 
  geom_line(data = subset(rare4, method == "extrapolated"), aes(x = t, y = qD, col = datasetName), lty = 3) + 
  geom_point(data = subset(rare4, method == "observed"), aes(x = t, y = qD, col = datasetName), size = 2) + 
  labs(x = "Number of samples", y = "Species richness") + 
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.position = "none"
  )
ggsave("CWS_data.png", rareplot_4, width = 10, height = 5, units = "in")


### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare4, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- mean(rare4$qD)
collected_samps <- nrow(pirop)

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage

# WADFW (Washington Dept. of Fish and Wildlife)---------------------------------

wadfw <- Aves %>% 
  filter(ownerInstitutionCode == "Washington Dept. of Fish and Wildlife")

# write csv
write.csv(wadfw, "./wadfwSites.csv")

# Read in site lat/longs
sites <- read.csv("./wadfwSites.csv", header = T)

rare5 <- do.call(rbind, lapply(unique(wadfw$datasetName), function(i) {
  
  x <- subset(sites, datasetName == i)
  
  if(nrow(x) == 0) data.frame() else {
    
    # summarize by dataset
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

# Plot results for the subdatasets collected
rareplot_5 <- ggplot() +
  geom_line(data = subset(rare5, method == "interpolated"), aes(x = t, y = qD, col = datasetName)) + 
  geom_line(data = subset(rare5, method == "extrapolated"), aes(x = t, y = qD, col = datasetName), lty = 3) + 
  geom_point(data = subset(rare5, method == "observed"), aes(x = t, y = qD, col = datasetName), size = 2) + 
  labs(x = "Number of samples", y = "Species richness") + 
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.position = "none"
  )
ggsave("WADFW_data.png", rareplot_5, width = 10, height = 5, units = "in")

# Censo abundancia (CALIDRIS)--------------------------------------------------------

calidris <- Aves %>% 
  filter(institutionCode == "CALIDRIS")

# write csv
write.csv(sem, "./calidrisSites.csv")

# Read in site lat/longs
sites <- read.csv("./calidrisSites.csv", header = T)


# Biomass 1980-1985 -------------------------------------------------------

sayed <- Aves %>% 
  filter(institutionCode == "Dr. S.Z. El-Sayed")

# write csv
write.csv(sem, "./sayedSites.csv")

# Read in site lat/longs
sites <- read.csv("./sayedSites.csv", header = T)

# Seabird and cetacean sightings from Cruise SY002 of RV Shoyo Maru, Tropical Atlantic Ocean, Oct. 2000

sc <- Aves_NA %>% 
  filter(ownerInstitutionCode == "Institut de Recherche pour le Developpement, Departement des Ressources Vivantes")

# write csv
write.csv(sc, "./scSites.csv")

# Read in site lat/longs
sites <- read.csv("./scSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare <- do.call(rbind, lapply(unique(sc$datasetName), function(i) {
  
  x <- subset(sites, datasetName == i)
  
  if(nrow(x) == 0) data.frame() else {
    
    # summarize by dataset
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
rareplot_1 <- ggplot() +
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
ggsave("sc_data.png", rareplot_1, width = 10, height = 5, units = "in")

### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- mean(rare$qD)
collected_samps <- nrow(sc)

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage
