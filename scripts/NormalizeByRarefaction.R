# This script plots LME's and observations inside LME's
# Written by E. Montes (2019), edited by S. Hartman
# August 3, 2020

# install.packages("maptools")
# install.packages("rgeos")
# install.packages("rgdal")
# install.packages("maps")
# install.packages("mapproj")

library(robis)
library(rgdal) # for `ogrInfo()` and `readOGR()`
library(tools) # for `file_path_sans_ext()`
library(dplyr) # for `inner_join()`, `filter()`, `summarise()`, and the pipe operator (%>%)
library(ggplot2) # for `fortify()` and for plotting
library(sp) # for `point.in.polygon()` and `spDists()`
library(tidyr) # for `gather()`
library(readr) # for `write_tsv()`
library(leaflet)
library(lubridate)
library(tidyverse)
library(sp)
library(sf)
library(iNEXT)
library(maptools)
library(maps)
library(mapproj)


# LME Subregions ----------------------------------------------------------

# Read in and view the shapefile, get a feel for it
setwd("~/Chp1/data/LME66")
lme <- st_read("LMEs66.shp") # found http://lme.edc.uri.edu/index.php/digital-data
ggplot() + 
  geom_sf(data = lme, color = "black", fill = "cyan1") + 
  coord_sf()

# Provide the function fortify.shape(), which puts the shapefile data in the object class data.frame, 
# so that it can be used by ggplot2

fortify.shape <- function(x){
  x@data$id <- rownames(x@data)
  x.f <- fortify(x, region = "id")
  x.join <- inner_join(x.f, x@data, by = "id")
}

# extract portions of the data (from the fortified data.frame object) for a smaller domain

subset.shape <- function(x, domain){
  x.subset <- filter(x, long > domain[1] & 
                       long < domain[2] & 
                       lat > domain[3] & 
                       lat < domain[4])
  x.subset
}

# Plotting the coastline and some animal observations
# Specify the local directory and name of the shapefile 
# and read its contents (global coastline data)
setwd("~/Chp1")
path.lme.coast <- ("data/LME66") #localdirectory
fnam.lme.coast <- "LMEs66.shp"   #shapefile
dat.coast <- readOGR(dsn = path.lme.coast, 
                     layer = file_path_sans_ext(fnam.lme.coast))

# fortify the global data and then extract domain
dat.coast <- fortify.shape(dat.coast) 
# Specify the desired LME:
# See numbers here: http://lme.edc.uri.edu/index.php/lme-introduction
dat.sel_1 <- subset(dat.coast, LME_NUMBER == 17) # North Brazil Shelf 
dat.sel_2 <- subset(dat.coast, LME_NUMBER == 3) # California Current
dat.sel_3 <- subset(dat.coast, LME_NUMBER == 4) # Gulf of California
dat.sel_4 <- subset(dat.coast, LME_NUMBER == 5) # Gulf of Mexico
dat.sel_5 <- subset(dat.coast, LME_NUMBER == 6) # Southeast U.S. Continental Shelf
dat.sel_6 <- subset(dat.coast, LME_NUMBER == 7) # Northeast U.S. Continental Shelf
dat.sel_7 <- subset(dat.coast, LME_NUMBER == 8) # Scotian Shelf
dat.sel_8 <- subset(dat.coast, LME_NUMBER == 11) # Pacific Central-American
dat.sel_9 <- subset(dat.coast, LME_NUMBER == 12) # Caribbean Sea
dat.sel_10 <- subset(dat.coast, LME_NUMBER == 13) # Humboldt Current
dat.sel_11 <- subset(dat.coast, LME_NUMBER == 14) # Patagonian Shelf
dat.sel_12 <- subset(dat.coast, LME_NUMBER == 15) # South Brazil Shelf
dat.sel_13 <- subset(dat.coast, LME_NUMBER == 16) # East Brazil Shelf
dat.sel_14 <- subset(dat.coast, LME_NUMBER == 9) # Newfoundland-Labrador Shelf

xlims <- c(-148, -18) #longitude
ylims <- c(-60,55)    #latitude

# World map
mapWorld <- borders(database = "world", colour="gray50", fill="gray50")
# Generate a base map with the coastline:

p0 <- ggplot() + theme(text = element_text(size=15),plot.title = element_text(hjust = 0.5),
                       plot.caption = element_text(hjust = 0.6)) + 
  geom_path(data = dat.coast, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.25) + 
  coord_map(projection = "mercator") + 
  scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
  scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
  labs(title = "Large Marine Ecosystems", x = "Longitude", y = "Latitude", 
       caption = "Figure 1. Large Marine Ecosystems as subregions for rarefaction 
       analysis. Generated from NOAA shapefiles (http://www.lme.noaa.gov/).")

p0 # view map

# Separate Data by Subregion ----------------------------------------------

# want to filter out the necessary data from each region using LMEs
# Use GIS with LME66.shp and Aves.csv to separate into regions
# write.csv(Aves, file = "C:\\Users\\samantha\\Documents\\Aves.csv") # saving Aves.csv to local directory
setwd("~/LME subregions")

california     <- read_csv("CaliforniaCurrentLMEspecies.csv")
caribbean      <- read_csv("CarribbeanLMEspecies.csv")
gulfofMexico   <- read_csv("GoMLMEspecies.csv")
gulfofAlaska   <- read_csv("GulfofAlaskaLMEspecies.csv")
humboldt       <- read_csv("HumboldtLMEspecies.csv")
neUSShelf      <- read_csv("NEUSShelfLMEspecies.csv")
northBrazilian <- read_csv("NorthBrazilianShelfLMEspecies.csv")
pacificCentral <- read_csv("PacificCentralAmericanLMEspecies.csv")
southBrazilian <- read_csv("South Brazilian Shelf LME Species.csv")
pacificCentral <- read_csv("PacificCentralAmericanLMEspecies.csv")
patagonia      <- read_csv("PatagoniaLMEspecies.csv")
seUSShelf      <- read_csv("SEUSShelfLMEspecies.csv")

setwd("~/github/AvesOBIS")

# select variables you need for analysis
california           <- california %>% 
  select(scientific,eventDate,decimalLon,decimalLat,basisOfRec,date_year,  
         individual, identified, datasetID, datasetNam, dataset_id, institutio, 
         ownerInsti, collection, catalogNum, occurrence, samplingEf)
colnames(california) <- c("scientificName","eventDate","decimalLongitude",
                          "decimalLatitude","basisOfRecord","date_year",  
                          "individualCount","identifiedBy","datasetID","datasetName",
                          "dataset_id", "institutionCode","ownerInstitutionCode",
                          "collectionCode","catalogNumber","occurrenceStatus","samplingEffort")
california <- add_column(california, LME = "California Current")

caribbean           <- caribbean %>% 
  select(scientific,eventDate,decimalLon,decimalLat,basisOfRec,date_year,  
         individual, identified, datasetID, datasetNam, dataset_id, institutio, 
         ownerInsti, collection, catalogNum, occurrence, samplingEf)
colnames(caribbean) <- c("scientificName","eventDate","decimalLongitude",
                          "decimalLatitude","basisOfRecord","date_year",  
                          "individualCount","identifiedBy","datasetID","datasetName",
                          "dataset_id", "institutionCode","ownerInstitutionCode",
                          "collectionCode","catalogNumber","occurrenceStatus","samplingEffort")
caribbean <- add_column(caribbean, LME = "Caribbean Sea")

gulfofMexico           <- gulfofMexico %>% 
  select(scientific,eventDate,decimalLon,decimalLat,basisOfRec,date_year,  
         individual, identified, datasetID, datasetNam, dataset_id, institutio, 
         ownerInsti, collection, catalogNum, occurrence, samplingEf)
colnames(gulfofMexico) <- c("scientificName","eventDate","decimalLongitude",
                          "decimalLatitude","basisOfRecord","date_year",  
                          "individualCount","identifiedBy","datasetID","datasetName",
                          "dataset_id", "institutionCode","ownerInstitutionCode",
                          "collectionCode","catalogNumber","occurrenceStatus","samplingEffort")
gulfofMexico <- add_column(gulfofMexico, LME = "Gulf of Mexico")

gulfofAlaska           <- gulfofAlaska %>% 
  select(scientific,eventDate,decimalLon,decimalLat,basisOfRec,date_year,  
         individual, identified, datasetID, datasetNam, dataset_id, institutio, 
         ownerInsti, collection, catalogNum, occurrence, samplingEf)
colnames(gulfofAlaska) <- c("scientificName","eventDate","decimalLongitude",
                            "decimalLatitude","basisOfRecord","date_year",  
                            "individualCount","identifiedBy","datasetID","datasetName",
                            "dataset_id", "institutionCode","ownerInstitutionCode",
                            "collectionCode","catalogNumber","occurrenceStatus","samplingEffort")
gulfofAlaska <- add_column(gulfofAlaska, LME = "Gulf of Alaska")

humboldt           <- humboldt %>% 
  select(scientific,eventDate,decimalLon,decimalLat,basisOfRec,date_year,  
         individual, identified, datasetID, datasetNam, dataset_id, institutio, 
         ownerInsti, collection, catalogNum, occurrence, samplingEf)
colnames(humboldt) <- c("scientificName","eventDate","decimalLongitude",
                            "decimalLatitude","basisOfRecord","date_year",  
                            "individualCount","identifiedBy","datasetID","datasetName",
                            "dataset_id", "institutionCode","ownerInstitutionCode",
                            "collectionCode","catalogNumber","occurrenceStatus","samplingEffort")
humboldt <- add_column(humboldt, LME = "Humboldt Current")

neUSShelf           <- neUSShelf %>% 
  select(scientific,eventDate,decimalLon,decimalLat,basisOfRec,date_year,  
         individual, identified, datasetID, datasetNam, dataset_id, institutio, 
         ownerInsti, collection, catalogNum, occurrence, samplingEf)
colnames(neUSShelf) <- c("scientificName","eventDate","decimalLongitude",
                            "decimalLatitude","basisOfRecord","date_year",  
                            "individualCount","identifiedBy","datasetID","datasetName",
                            "dataset_id", "institutionCode","ownerInstitutionCode",
                            "collectionCode","catalogNumber","occurrenceStatus","samplingEffort")
neUSShelf <- add_column(neUSShelf, LME = "Northeast U.S. Continental Shelf")

northBrazilian           <- northBrazilian %>% 
  select(scientific,eventDate,decimalLon,decimalLat,basisOfRec,date_year,  
         individual, identified, datasetID, datasetNam, dataset_id, institutio, 
         ownerInsti, collection, catalogNum, occurrence, samplingEf)
colnames(northBrazilian) <- c("scientificName","eventDate","decimalLongitude",
                            "decimalLatitude","basisOfRecord","date_year",  
                            "individualCount","identifiedBy","datasetID","datasetName",
                            "dataset_id", "institutionCode","ownerInstitutionCode",
                            "collectionCode","catalogNumber","occurrenceStatus","samplingEffort")
northBrazilian <- add_column(northBrazilian, LME = "North Brazil Shelf")

pacificCentral           <- pacificCentral %>% 
  select(scientific,eventDate,decimalLon,decimalLat,basisOfRec,date_year,  
         individual, identified, datasetID, datasetNam, dataset_id, institutio, 
         ownerInsti, collection, catalogNum, occurrence, samplingEf)
colnames(pacificCentral) <- c("scientificName","eventDate","decimalLongitude",
                              "decimalLatitude","basisOfRecord","date_year",  
                              "individualCount","identifiedBy","datasetID","datasetName",
                              "dataset_id", "institutionCode","ownerInstitutionCode",
                              "collectionCode","catalogNumber","occurrenceStatus","samplingEffort")
pacificCentral <- add_column(pacificCentral, LME = "Pacific Central-American Coastal")

patagonia           <- patagonia %>% 
  select(scientific,eventDate,decimalLon,decimalLat,basisOfRec,date_year,  
         individual, identified, datasetID, datasetNam, dataset_id, institutio, 
         ownerInsti, collection, catalogNum, occurrence, samplingEf)
colnames(patagonia) <- c("scientificName","eventDate","decimalLongitude",
                              "decimalLatitude","basisOfRecord","date_year",  
                              "individualCount","identifiedBy","datasetID","datasetName",
                              "dataset_id", "institutionCode","ownerInstitutionCode",
                              "collectionCode","catalogNumber","occurrenceStatus","samplingEffort")
patagonia <- add_column(patagonia, LME = "Patagonian Shelf")

seUSShelf           <- seUSShelf %>% 
  select(scientific,eventDate,decimalLon,decimalLat,basisOfRec,date_year,  
         individual, identified, datasetID, datasetNam, dataset_id, institutio, 
         ownerInsti, collection, catalogNum, occurrence, samplingEf)
colnames(seUSShelf) <- c("scientificName","eventDate","decimalLongitude",
                              "decimalLatitude","basisOfRecord","date_year",  
                              "individualCount","identifiedBy","datasetID","datasetName",
                              "dataset_id", "institutionCode","ownerInstitutionCode",
                              "collectionCode","catalogNumber","occurrenceStatus","samplingEffort")
seUSShelf <- add_column(seUSShelf, LME = "Southeast U.S. Continental Shelf")

southBrazilian           <- southBrazilian %>% 
  select(scientific,eventDate,decimalLon,decimalLat,basisOfRec,date_year,  
         individual, identified, datasetID, datasetNam, dataset_id, institutio, 
         ownerInsti, collection, catalogNum, occurrence, samplingEf)
colnames(southBrazilian) <- c("scientificName","eventDate","decimalLongitude",
                              "decimalLatitude","basisOfRecord","date_year",  
                              "individualCount","identifiedBy","datasetID","datasetName",
                              "dataset_id", "institutionCode","ownerInstitutionCode",
                              "collectionCode","catalogNumber","occurrenceStatus","samplingEffort")
southBrazilian <- add_column(southBrazilian, LME = "South Brazil Shelf")

x       <- rbind(california,caribbean) %>% 
          rbind(gulfofAlaska) 
AvesLME <- rbind(x,gulfofMexico) %>% 
          rbind(humboldt) %>% 
          rbind(neUSShelf) %>% 
          rbind(northBrazilian) %>% 
          rbind(pacificCentral) %>% 
          rbind(patagonia) %>% 
          rbind(seUSShelf) %>% 
          rbind(southBrazilian)
# change AvesLME eventDate format


y       <- left_join(Aves,AvesLME)

# Optimization of sampling ------------------------------------------------
setwd("~/github/AvesOBIS")
source("scripts/covstop.R")

# generate rarefaction curves by region

