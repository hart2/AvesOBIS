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

# Seabird and cetacean sightings from Cruise SY002 of RV Shoyo Maru, Tropical Atlantic Ocean, Oct. 2000------------------------

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

# Plot results for the subdatasets collected by Institut de Recherche
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

# Richard Sears, Mingan Island Cetacean Study -------------------

mic <- Aves_NA %>% 
  filter(ownerInstitutionCode == "Richard Sears, Mingan Island Cetacean Study")

# write csv
write.csv(mic, "./micSites.csv")

# Read in site lat/longs
sites <- read.csv("./micSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare <- do.call(rbind, lapply(unique(mic$datasetName), function(i) {
  
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

# Plot results for the subdatasets collected by Institut de Recherche
rareplot <- ggplot() +
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
ggsave("mic_data.png", rareplot, width = 10, height = 5, units = "in")

### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- mean(rare$qD)
collected_samps <- nrow(mic)

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage

# Dalhousie University ----------------------------------------------------

dal <- Aves_NA %>% 
  filter(ownerInstitutionCode == "Dalhousie University")

# write csv
write.csv(dal, "./dalSites.csv")

# Read in site lat/longs
sites <- read.csv("./dalSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare <- do.call(rbind, lapply(unique(dal$datasetName), function(i) {
  
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

# Plot results for the subdatasets collected by Institut de Recherche
rareplot <- ggplot() +
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
ggsave("dal_data.png", rareplot, width = 10, height = 5, units = "in")

### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- mean(rare$qD)
collected_samps <- nrow(dal)

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage

# SEFSC --------

target1 <- c("Gomex Sperm Whale Survey 2000","SEFSC Atlantic surveys 1992","SEFSC Atlantic surveys 1999","SEFSC Atlantic surveys, 1998 (3)",
             "SEFSC Caribbean Survey 1995","SEFSC Caribbean Survey 2000","SEFSC GoMex Oceanic 1992 (199)")
target2 <- c("SEFSC GoMex Oceanic 1993 (S)","SEFSC GoMex Oceanic 1993 (W)","SEFSC GoMex Oceanic 1994","SEFSC GoMex Oceanic 1996",
             "SEFSC GoMex Oceanic 1997","SEFSC GoMex Oceanic 1999")
target3 <- c("SEFSC GoMex Oceanic 2000","SEFSC GoMex Oceanic 2001","SEFSC Gomex Shelf 1994","SEFSC Gomex Shelf 1998",
             "SEFSC Gomex Shelf 2000","SEFSC Gomex Shelf 2001")
sefsc <- Aves_NA %>% 
  filter(ownerInstitutionCode == "NOAA;NMFS;SEFSC")
# %>% filter(datasetName %in% target3)

# write csv
write.csv(sefsc, "./sefscSites.csv")

# Read in site lat/longs
sites <- read.csv("./sefscSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare <- do.call(rbind, lapply(unique(sefsc$datasetName), function(i) {
  
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

# Plot results for the subdatasets collected by Institut de Recherche
rareplot <- ggplot() +
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
ggsave("sefsc_data.png", rareplot, width = 10, height = 5, units = "in")

### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- mean(rare$qD)
collected_samps <- nrow(sefsc)

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage

# Canadian Wildlife Service - Atlantic Region (CWS - Atlantic) ------------
cws <- Aves_NA %>% 
  filter(institutionCode == "Canadian Wildlife Service - Atlantic Region (CWS - Atlantic)")

# write csv
write.csv(cws, "./cwsSites.csv")

# Read in site lat/longs
sites <- read.csv("./cwsSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare <- do.call(rbind, lapply(unique(cws$datasetName), function(i) {
  
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

# Plot results for the subdatasets collected by Institut de Recherche
rareplot <- ggplot() +
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
ggsave("cws2_data.png", rareplot, width = 10, height = 5, units = "in")

### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- mean(rare$qD)
collected_samps <- nrow(cws)

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage

# APEM --------------------------------------------------------------------
apem <- Aves_NA %>% 
  filter(ownerInstitutionCode == "APEM")

# write csv
write.csv(apem, "./apemSites.csv")

# Read in site lat/longs
sites <- read.csv("./apemSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare <- do.call(rbind, lapply(unique(apem$datasetName), function(i) {
  
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

# Plot results for the subdatasets collected by Institut de Recherche
rareplot <- ggplot() +
  geom_line(data = subset(rare, method == "interpolated"), aes(x = t, y = qD, col = datasetName)) + 
  geom_line(data = subset(rare, method == "extrapolated"), aes(x = t, y = qD, col = datasetName), lty = 3) + 
  geom_point(data = subset(rare, method == "observed"), aes(x = t, y = qD, col = datasetName), size = 2) + 
  labs(x = "Number of samples", y = "Species richness") + 
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
ggsave("apem_data.png", rareplot, width = 10, height = 5, units = "in")

### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- mean(rare$qD)
collected_samps <- nrow(apem)

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage

# IPHC --------------------------------------------------------------------

iphc <- Aves_NA %>% 
  filter(institutionCode == "IPHC")

# write csv
write.csv(iphc, "./iphcSites.csv")

# Read in site lat/longs
sites <- read.csv("./iphcSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare <- do.call(rbind, lapply(unique(iphc$datasetName), function(i) {
  
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

# Plot results for the subdatasets
rareplot <- ggplot() +
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
ggsave("iphc_data.png", rareplot, width = 10, height = 5, units = "in")

### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- mean(rare$qD)
collected_samps <- nrow(iphc)

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage


# PELAGIS -----------------------------------------------------------------

pel <- Aves_NA %>% 
  filter(institutionCode == "PELAGIS")

# write csv
write.csv(pel, "./pelSites.csv")

# Read in site lat/longs
sites <- read.csv("./pelSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare <- do.call(rbind, lapply(unique(pel$datasetName), function(i) {
  
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

# Plot results for the subdatasets
rareplot <- ggplot() +
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
ggsave("pel_data.png", rareplot, width = 10, height = 5, units = "in")

### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- mean(rare$qD)
collected_samps <- nrow(pel)

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage

# DON ---------------------------------------------------------------------

don <- Aves_NA %>% 
  filter(ownerInstitutionCode == "DOD;DON")

# write csv
write.csv(don, "./donSites.csv")

# Read in site lat/longs
sites <- read.csv("./donSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare <- do.call(rbind, lapply(unique(don$datasetName), function(i) {
  
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

# Plot results for the subdatasets
rareplot <- ggplot() +
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
ggsave("don_data.png", rareplot, width = 10, height = 5, units = "in")

### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- mean(rare$qD)
collected_samps <- nrow(don)

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage

# GOMA --------------------------------------------------------------------

goma <- Aves_NA %>% 
  filter(institutionCode == "GoMA")

# write csv
write.csv(goma, "./gomaSites.csv")

# Read in site lat/longs
sites <- read.csv("./gomaSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare <- do.call(rbind, lapply(unique(goma$datasetName), function(i) {
  
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

# Plot results for the subdatasets
rareplot <- ggplot() +
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
ggsave("goma_data.png", rareplot, width = 10, height = 5, units = "in")

### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- mean(rare$qD)
collected_samps <- nrow(goma)

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage


# Duke / UNC Oceanographic Consortium ----------------------------------------------------------------

dunc <- Aves_NA %>% 
  filter(ownerInstitutionCode == "Duke / UNC Oceanographic Consortium")

# write csv
write.csv(dunc, "./duncSites.csv")

# Read in site lat/longs
sites <- read.csv("./duncSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare <- do.call(rbind, lapply(unique(dunc$datasetName), function(i) {
  
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

# Plot results for the subdatasets
rareplot <- ggplot() +
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
ggsave("dunc_data.png", rareplot, width = 10, height = 5, units = "in")

### This extracts maximum values of extrapolated richness from dataset
max_extra <- filter(rare, method == "extrapolated")
max(max_extra$qD)

### This calculates the difference between minimum number of samples needed for 
#   100% coverage and actual number of samples collected

min_coverage <- mean(rare$qD)
collected_samps <- nrow(dunc)

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage


Aves_NA1 <- Aves_NA %>% filter(!(datasetName == "SEFSC GoMex Oceanic 1993 (S)"))
Aves_NA1 <- Aves_NA1 %>% filter(!(datasetName == "SEFSC GoMex Oceanic 1994"))
Aves_NA1 <- Aves_NA1 %>% filter(!(datasetName == "SEFSC GoMex Oceanic 1992 (199)"))
Aves_NA1 <- Aves_NA1 %>% filter(!(datasetName == "SEFSC GoMex Oceanic 1997"))
Aves_NA1 <- Aves_NA1 %>% filter(!(datasetName == "Observatoire Pelagis aerial surveys 2002-2018"))
Aves_NA1 <- Aves_NA1 %>% filter(!(datasetName == "SEFSC Atlantic surveys 1992"))
Aves_NA1 <- Aves_NA1 %>% filter(!(datasetName == "SEFSC GoMex Oceanic 1999"))
Aves_NA1 <- Aves_NA1 %>% filter(!(datasetName == "SEFSC Atlantic surveys 1999"))
Aves_NA1 <- Aves_NA1 %>% filter(!(datasetName == "SEFSC Caribbean Survey 1995"))
Aves_NA1 <- Aves_NA1 %>% filter(!(institutionCode == "GoMA"))
Aves_NA1 <- Aves_NA1 %>% filter(!(datasetName == "SEFSC Atlantic surveys, 1998 (3)"))
Aves_NA1 <- Aves_NA1 %>% filter(!(datasetName == "SEFSC GoMex Oceanic 2001"))
Aves_NA1 <- Aves_NA1 %>% filter(!(ownerInstitutionCode == "Census of Marine Life Tagging of Pacific Predators"))
Aves_NA1 <- Aves_NA1 %>% filter(!(datasetName == "SEFSC Caribbean Survey 2000"))
Aves_NA1 <- Aves_NA1 %>% filter(!(datasetName == "SEFSC Gomex Shelf 2001"))
Aves_NA1 <- Aves_NA1 %>% filter(!(datasetName == "Hatteras Eddy Cruise 2004"))
Aves_NA <- Aves_NA1
rm(Aves_NA1,AvesFamily)

#Forgot some datasets
Aves_NA <- Aves_NA%>% filter(!(datasetName == "SEFSC Gomex Shelf 2000"))
Aves_NA <- Aves_NA%>% filter(!(datasetName == "SEFSC Gomex Shelf 1998"))
Aves_NA <- Aves_NA%>% filter(!(datasetName == "Sargasso 2004 - Seabirds"))

write.csv(Aves_NA, "./Aves_NA.csv")