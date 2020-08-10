# Code written by Savannah Hartman for dissertation chapter 1
# Normalizing Aves collection data by effort (time involved in data collection)

# Dig into the metadata of the top 5-10 datasets since the files downloaded from
# OBIS are a mixed bag. Look at the datasets that give me the most records in 
# the geographic extent I'm looking at.

# USGS Patuxent Wildlife Research Center Seabirds Compendium (collection of 50 datasets)

# manipulated dataset "seabird_data_archive_2" from "seabird_data_archive_NODC_30Dec2013)
# by adding back years and scientific names, simplifying long and lat to 4 decimal, adding effort

# upload seabird_data.csv and usgs_eaec.csv into R (done previously)

library(tidyverse)
library(dplyr)

# rename to usgs for simplification, remove black scientificName
usgs <- usgs_eaec7873_a1d8_43cf_baa2_27b2772a8d1f %>%
  filter(!(scientificName == "")) %>% 
  select(scientificName,eventDate,decimalLongitude,decimalLatitude,basisOfRecord,date_year,  
         individualCount, identifiedBy, institutionCode, ownerInstitutionCode, collectionCode, catalogNumber, 
         occurrenceStatus, samplingEffort)%>%  
  filter(basisOfRecord == "HumanObservation", date_year >= 1960 & date_year < 2019, decimalLatitude > -55)

# Finding species present: 1960-2020 (originally wrote in AvesOBISscript.R)
# Removing scientificName that is genus only
genspUSGS <- usgs %>% 
  select(scientificName)                    # Create data frame with only scientific names
freqUSGS <- as.data.frame(table(genspUSGS)) # Create a data frame with species names and how often they appear in dataset "Aves_EEZ"
num_gsUSGS <- count(freqUSGS)               # Counts the number of genus/genus species found in dataset "freq"

v1 <- genspUSGS                      # vector with scientificNames
v2 <- 1:nrow(genspUSGS)              # number of cells in scientificName and creating vector with the number of cells necessary for running splitstring fxn
species <- data.frame(v1,v2)
colnames(species) <- c("scientificName", "v2")

alpha <- function(species){          # Fxn to filter data frame to include only rows with a space between two character strings (aka genus and species)
  booleans <- vector()
  i <- 1
  while (i <= nrow(species)){
    tmp <- strsplit(as.character(species$scientificName[i]),' ')[[1]]
    booleans[i] <- (length(tmp) == 2)
    i <- i + 1
  } 
  return((booleans)) 
}

species <- alpha(species)          # Gives True/False whether scientifcName contains a space
species <- as.data.frame(species)  # Creating into a vector

# Trying to merge "species" with "Aves_EEZ" to remove genus only names
df1 <- c(species,usgs)             # Inputting "species" T/F into "usgs" dataframe
df2 <- as.data.frame(df1) %>% 
  filter(species == "TRUE")        # Making it readable as a dataframe and removing genus only

usgs <- df2 %>% 
  select(-species)

# making sure usgs has columns needed, only 4 decimal places for lat and lon
usgs <- usgs %>% 
  select(date_year,eventDate,scientificName,decimalLongitude,decimalLatitude)
options(digits = 6)
usgs$effort <- NA

seabird <- seabird_data_archive_2 %>% 
  select(year,observation_date,scientific_nm,observation_longitude,observation_latitude,effort) 
# rename columns to match usgs
# effort is distance / hr
colnames(seabird) <- c("date_year","eventDate","scientificName","decimalLongitude","decimalLatitude","effort")
seabird <- seabird %>%  
  # remove scientificName blanks
  filter(!(scientificName == "")) %>% 
  filter(date_year >= 1960 & date_year < 2019, decimalLatitude > -55)


df <- left_join(usgs, seabird)
df <- invisible(unique(df))

rm(df1,df2,freqUSGS,genspUSGS,num_gsUSGS,v1,species)
# Find species per year (from seabird_data_archive_2 put it into usgs data frame)

# usgsNorm <- usgs %>% 
#   filter(year == "1978") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientific_nm`)
# 
# add a column of averaged effort for a species for a year
# usgsNorm <- aggregate(.~scientific_nm, FUN=mean, data=usgsNorm[, -3])