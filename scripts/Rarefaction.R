# Code written by Savannah Hartman for dissertation chapter 1
# September 8th, 2020 

library(tidyverse)
library(iNEXT)
library(devtools)
library(ggplot2)

source("~/github/AvesOBIS/scripts/AvesOBISscript.R")
source("~/github/AvesOBIS/scripts/covstop.R")


# USGS (USGS-FWS)---------------------------------------------------------------
usgs <- Aves %>%
  filter(ownerInstitutionCode == "USGS-FWS") #has multiple sampling methods

# write csv
write.csv(usgs, "./usgsSites.csv")

# Read in site lat/longs
sites <- read.csv("./usgsSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare <- do.call(rbind, lapply(unique(usgs$datasetName), function(i) {
  
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

min_coverage <- samps.summary$mean.samples
collected_samps <- samps.summary$totsamples

diff_samps <- collected_samps - min_coverage
frac_samps <- diff_samps/min_coverage


# Eco (Normandeau)--------------------------------------------------------------

eco <- Aves %>% 
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

# MMS (BOEM) ---------------------------------------------------------------------
mms <- Aves %>% 
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

# CalCOFi (PRBO Conservation Science) ------------------------------------------

calcofi <- Aves %>% 
  filter(ownerInstitutionCode == "PRBO Conservation Science") #ship surveys

# write csv
write.csv(calcofi, "./calcofiSites.csv")

# Read in site lat/longs
sites <- read.csv("./calcofiSites.csv", header = T)

# Generate interpolation/extrapolation curves for each dataset

rare3 <- do.call(rbind, lapply(unique(calcofi$datasetName), function(i) {
  
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
rareplot_3 <- ggplot() +
  geom_line(data = subset(rare3, method == "interpolated"), aes(x = t, y = qD, col = datasetName)) + 
  geom_line(data = subset(rare3, method == "extrapolated"), aes(x = t, y = qD, col = datasetName), lty = 3) + 
  geom_point(data = subset(rare3, method == "observed"), aes(x = t, y = qD, col = datasetName), size = 2) + 
  labs(x = "Number of samples", y = "Species richness") + 
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.position = "none"
  )
ggsave("PRBO_data.png", rareplot_3, width = 10, height = 5, units = "in")

# Pirop (Canadian Wildlife Service)---------------------------------------------

pirop <- Aves %>% 
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

# Southeast Mangrove (APTA or Argos tracking) ----------------------------------

sem <- Aves %>% 
  filter(institutionCode == "APTA" | institutionCode == "IPESCA")

# write csv
write.csv(sem, "./semSites.csv")

# Read in site lat/longs
sites <- read.csv("./semSites.csv", header = T)

rare6 <- do.call(rbind, lapply(unique(sem$dataset_id), function(i) {
  
  x <- subset(sites, dataset_id == i)
  
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
      dataset_id = i,
      ret[, c(1:2, 4)]
    )
    
  }
  
} ) )

# Plot results for the subdatasets collected
rareplot_6 <- ggplot() +
  geom_line(data = subset(rare6, method == "interpolated"), aes(x = t, y = qD, col = dataset_id)) + 
  geom_line(data = subset(rare6, method == "extrapolated"), aes(x = t, y = qD, col = dataset_id), lty = 3) + 
  geom_point(data = subset(rare6, method == "observed"), aes(x = t, y = qD, col = dataset_id), size = 2) + 
  labs(x = "Number of samples", y = "Species richness") + 
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.position = "none"
  )
ggsave("SoutheastMangrove_data.png", rareplot_6, width = 10, height = 5, units = "in")


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