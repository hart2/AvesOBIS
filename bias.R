#bias north vs south america

library(tidyverse) # packages for data visualization
library(dplyr)
library(readr)
library(devtools)
library(robis)
library(obistools)

# data downloaded from OBIS already within lat long limits of the study
Aves_EEZ1 <- read.csv("~/github/Aves_EEZ.csv")
Aves_EEZ2 <- Aves_EEZ1 %>% 
  select(scientificName,family,eventDate,date_mid,date_year,decimalLongitude,decimalLatitude,basisOfRecord,  
         individualCount, identifiedBy, datasetID, datasetName, dataset_id, institutionCode, 
         ownerInstitutionCode, collectionCode, catalogNumber) %>%  
  filter(basisOfRecord == "HumanObservation", date_year >= 1960 & date_year < 2019, decimalLatitude > -55)        #filtering for only human observations and data collection post 1960

Aves_EEZ3 <- Aves_EEZ1 %>% 
  select(scientificName,family,eventDate,date_mid,date_year,decimalLongitude,decimalLatitude,basisOfRecord,  
         individualCount, identifiedBy, datasetID, datasetName, dataset_id, institutionCode, 
         ownerInstitutionCode, collectionCode, catalogNumber) %>%  
  filter(basisOfRecord == "PreservedSpecimen", date_year >= 1960 & date_year < 2019, decimalLatitude > -55)

gensp <- Aves_EEZ2 %>% 
  select(scientificName)            # Create data frame with only scientific names
# freq <- table(gensp)     create a table with species names and how often they appear in dataset "Aves_EEZ"
freq <- as.data.frame(table(gensp)) # Create a data frame with species names and how often they appear in dataset "Aves_EEZ"
num_gs <- count(freq)

v1 <- gensp                      # vector with scientificNames
v2 <- 1:nrow(gensp)              # num of cells in scientificName and creating vector with the number of cells necessary for running splitstring fxn
species <- data.frame(v1,v2)
colnames(species) <- c("scientificName", "v2")

alpha <- function(species){        # Fxn to filter dataframe to include only rows with a space between two character strings (aka genus and species)
  booleans <- vector()
  i <- 1
  while (i <= nrow(species)){
    tmp <- strsplit(as.character(species$scientificName[i]),' ')[[1]]
    booleans[i] <- (length(tmp) == 2)
    i <- i + 1
    # print(booleans) prints what is true or false, remove when in full use
  } 
  return((booleans)) 
}

species <- alpha(species)          # Gives True/False whether scientifcName contains a space
species <- as.data.frame(species)  # Creating into a vector

# Trying to merge "species" with "Aves_EEZ" to remove genus only names
df1 <- c(species,Aves_EEZ2)         # Inputting "species" T/F into "Aves_EEZ" dataframe
df2 <- as.data.frame(df1) %>% 
  filter(species == "TRUE")        # Making it readable as a dataframe and removing genus only

Aves_EEZ2 <- df2 %>% 
  select(-species)                 # Removing T/F column from Aves dataset

num <- Aves_EEZ2 %>% 
  select(scientificName) 
freq1 <- as.data.frame(table(num)) 
numSpeciesFreq <- freq1 %>% 
  filter(Freq != 0)                
alphadiv <- count(numSpeciesFreq)  # Number of species found in Aves dataset with genus and species name         


# Remove likely duplicates 
# If scientificName are equal & decimalLongitude are equal & decimalLatitude are equal & eventDate are equal, remove
# one of the observations
# NOTE: This step will take some time
Aves_EEZnd <- invisible(unique(Aves_EEZ2)) %>%         # "invisible()" suppresses output
  arrange('scientificName')
rm(Aves_EEZ2,Aves_EEZ3,num,v1,species,gensp,freq,freq1,df1,df2)

# rm suspicious records within 1km of coastline
bufferA <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^A")) # looking at all scientificNames starting with "a"
bufferA <- check_onland(bufferA, buffer = 1000)

bufferB <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^B"))
bufferB <- check_onland(bufferB, buffer = 1000)

bufferC <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^C"))
bufferC <- check_onland(bufferC, buffer = 1000)

bufferD <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^D"))
bufferD <- check_onland(bufferD, buffer = 1000)

bufferE <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^E"))
bufferE <- check_onland(bufferE, buffer = 1000)

bufferF <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^F"))
bufferF <- check_onland(bufferF, buffer = 1000)

bufferG <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^G"))
bufferG <- check_onland(bufferG, buffer = 1000)

bufferH <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^H"))
bufferH <- check_onland(bufferH, buffer = 1000)

bufferI <- Aves_EEZnd %>%
  filter(str_detect(scientificName, "^I"))
bufferI <- check_onland(bufferI, buffer = 1000)

bufferJ <- Aves_EEZnd %>%
  filter(str_detect(scientificName, "^J"))
bufferJ <- check_onland(bufferJ, buffer = 1000)

bufferK <- Aves_EEZnd %>%
  filter(str_detect(scientificName, "^K"))
bufferK <- check_onland(bufferK, buffer = 1000)

bufferL <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^L"))
bufferL <- check_onland(bufferL, buffer = 1000)

bufferM <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^M"))
bufferM <- check_onland(bufferM, buffer = 1000)

bufferN <- Aves_EEZnd %>%
  filter(str_detect(scientificName, "^N"))
bufferN <- check_onland(bufferN, buffer = 1000)

bufferO <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^O"))
bufferO <- check_onland(bufferO, buffer = 1000)

bufferP <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^P"))
bufferP <- check_onland(bufferP, buffer = 1000)

bufferQ <- Aves_EEZnd %>%
  filter(str_detect(scientificName, "^Q"))
bufferQ <- check_onland(bufferQ, buffer = 1000)

bufferR <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^R"))
bufferR <- check_onland(bufferR, buffer = 1000)

bufferS <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^S"))
bufferS <- check_onland(bufferS, buffer = 1000)

bufferT <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^T"))
bufferT <- check_onland(bufferT, buffer = 1000)

bufferU <- Aves_EEZnd %>% 
  filter(str_detect(scientificName, "^U"))
bufferU <- check_onland(bufferU, buffer = 1000)

bufferV <- Aves_EEZnd %>%
  filter(str_detect(scientificName, "^V"))
bufferV <- check_onland(bufferV, buffer = 1000)

bufferW <- Aves_EEZnd %>%
  filter(str_detect(scientificName, "^W"))
bufferW <- check_onland(bufferW, buffer = 1000)

bufferX <- Aves_EEZnd %>%
  filter(str_detect(scientificName, "^X"))
bufferX <- check_onland(bufferX, buffer = 1000)

land_buffer <- rbind(bufferA,bufferB) %>% 
  rbind(bufferC) %>% 
  rbind(bufferD) %>% 
  rbind(bufferE) %>% 
  rbind(bufferF) %>% 
  rbind(bufferG) %>% 
  rbind(bufferH) %>% 
  rbind(bufferL) %>% 
  rbind(bufferM) %>% 
  rbind(bufferO) %>% 
  rbind(bufferP) %>% 
  rbind(bufferR) %>% 
  rbind(bufferS) %>% 
  rbind(bufferT) %>% 
  rbind(bufferU)

land_buffer <- land_buffer %>% 
  arrange(scientificName)

rm(bufferA, bufferB, bufferC, bufferD, bufferE, bufferF, bufferG, bufferH, bufferI,
   bufferJ, bufferK, bufferL, bufferM, bufferN, bufferO, bufferP, bufferQ,
   bufferR, bufferS, bufferT, bufferU, bufferV, bufferW, bufferX)

# remove land_buffer from Aves_EEZnd (there are additional 300 records removed 
# bc they had duplicate catalogNumbers - which isn't supposed to happen, human error putting info into OBIS)
Aves <- anti_join(Aves_EEZnd,land_buffer, by = "catalogNumber") 
Aves <- Aves %>% 
  arrange(scientificName)

rm(Aves_EEZnd)

gensp <- Aves %>% 
  select(scientificName)            # Create data frame with only scientific names
freq <- as.data.frame(table(gensp))
