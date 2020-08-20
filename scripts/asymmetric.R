####-----------------------------------------------------------------------------------------------
####                      OPTIMIZATION OF ROCKY SHORE SAMPLING FROM P2P                           #
####-----------------------------------------------------------------------------------------------
# Written by E. Montes, adapted by S. Hartman


# Load required libraries
library(iNEXT)
library(tidyverse)

x <- ECUADOR_GALAPAGOS_ALL_ipt_occurrence

# Load function to compute coverage-based stopping
source("R/covstop.R")

# Read in data

# split metadata
meta <- do.call(rbind.data.frame, strsplit(as.character(x$eventID), "\\_")) 

names(meta) <- c("country", "locality", "site", "date", "strata", "quadrat")    #rename columns

taxa <- do.call(rbind.data.frame, strsplit(as.character(x$scientificNameID), "\\:"))
names(taxa) <- c("taxa")

# bind back in
data <- cbind.data.frame(eventID = x$eventID, meta, taxa)

x <- data

# Remove duplicate rows
x <- x %>% distinct(country, locality, site, strata, quadrat, taxa, .keep_all = TRUE) %>% ungroup()

# Write csv
write.csv(x, "./sites.csv")

# Read in site lat/longs
sites <- read.csv("./sites.csv", header = T)

# Generate interpolation/extrapolation curves for each locality (or site)

rare <- do.call(rbind, lapply(unique(x$locality), function(i) {
  
  x <- subset(x, locality == i)
  
  # do.call(rbind, lapply(unique(p2p$site), function(j) {
  
  # x <- subset(p2p, site == j)
  
  do.call(rbind, lapply(unique(x$strata), function(k) {
    
    print(paste(i, k))
    
    x <- subset(x, strata == k)
    
    if(nrow(x) == 0) data.frame() else {
      
      # summarize by locality
      x$presence <- 1
      
      # Cast longways
      mat <- x %>% select(site, quadrat, taxa, presence) %>% 
        
        pivot_wider(id_cols = c(site, quadrat), names_from = taxa, values_from = presence) 
      
      mat[is.na(mat)] <- 0
      
      dnames <- list(colnames(mat)[-(1:2)], as.character(mat$site))
      
      mat <- t(as.matrix(mat)[, -(1:2), drop = FALSE])
      
      mat <- apply(mat, 2, as.numeric)
      
      dimnames(mat) <- dnames
      
      # z <- c(sum(mat), apply(mat, 1, sum))
      
      z <- as.incfreq(mat) # ERROR: could not find function "as.incfreq"
      
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
        locality = i,
        strata = k,
        ret[, c(1:2, 4)]
      )
      
    }
    
  } ) )
  
} ) )

# } ) )

rare$strata <- factor(rare$strata, levels = c("HIGHTIDE", "MIDTIDE", "LOWTIDE"))

# Plot results
(rareplot <- ggplot() +
    geom_line(data = subset(rare, method == "interpolated"), aes(x = t, y = qD, group = paste(locality, strata), col = strata)) +
    geom_line(data = subset(rare, method == "extrapolated"), aes(x = t, y = qD, group = paste(locality, strata), col = strata), lty = 3) +
    geom_point(data = subset(rare, method == "observed"), aes(x = t, y = qD, group = paste(locality, strata), col = strata), size = 2) +
    scale_color_manual(values = c("black", "dodgerblue3", "forestgreen")) +
    labs(x = "Number of samples", y = "Species richness") +
    facet_grid( ~ strata, scales = "free_x") +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
)

# Plot results with curves colored according to locality
(rareplot_1 <- ggplot() +
    geom_line(data = subset(rare, method == "interpolated" & strata == strata), aes(x = t, y = qD, group = paste(locality, strata), col = locality)) + 
    geom_line(data = subset(rare, method == "extrapolated" & strata == strata), aes(x = t, y = qD, group = paste(locality, strata), col = locality), lty = 3) + 
    geom_point(data = subset(rare, method == "observed" & strata == strata), aes(x = t, y = qD, group = paste(locality, strata), col = locality), size = 2) + 
    ylim(0, 65) + 
    labs(x = "Number of samples", y = "Species richness") + 
    facet_grid( ~ strata, scales = "free_x") + 
    theme_bw(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # legend.position = "none"
    )
)

ggsave(rareplot, device = "pdf", width = 10, height = 5, units = "in")

### This plots individual sites

# sel_locality = "NORTHERNMA"

(rareplot_1 <- ggplot() +
    geom_line(data = subset(rare, method == "interpolated" & strata == strata), aes(x = t, y = qD, group = paste(strata))) + 
    geom_line(data = subset(rare, method == "extrapolated" & strata == strata), aes(x = t, y = qD, group = paste(strata)), lty = 3) + 
    geom_point(data = subset(rare, method == "observed" & strata == strata), aes(x = t, y = qD, group = paste(strata)), size = 2) + 
    ylim(0, 65) + 
    xlim(0, 80) + 
    labs(x = "Number of samples", y = "Species richness") + 
    facet_grid( ~ strata, scales = "free_x") + 
    theme_bw(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # legend.position = "none"
    )
)