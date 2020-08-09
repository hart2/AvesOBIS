# Code written by Savannah Hartman for dissertation chapter 1
# Normalizing Aves collection data by effort (time involved in data collection)

# Dig into the metadata of the top 5-10 datasets since the files downloaded from
# OBIS are a mixed bag. Look at the datasets that give me the most records in 
# the geographic extent I'm looking at.

# USGS Patuxent Wildlife Research Center Seabirds Compendium (collection of 50 datasets)

# manipulated dataset "seabird_data_archive_2" from "seabird_data_archive_NODC_30Dec2013)
# by adding back years and scientific names, simplifying long and lat to 4 decimal 
# places (trying to match Aves_EEZ), adding effort

library(tidyverse)

usgs <- read.csv("data/usgs_eaec7873-a1d8-43cf-baa2-27b2772a8d1f.csv")
usgs <- usgs %>%  
  filter(!(scientificName == "")) %>% 
  select(scientificName,eventDate,decimalLongitude,decimalLatitude,basisOfRecord,date_year,  
         individualCount, identifiedBy, institutionCode, ownerInstitutionCode, collectionCode, catalogNumber, 
         occurrenceStatus, samplingEffort)%>%  
  filter(basisOfRecord == "HumanObservation", date_year >= 1960 & date_year < 2019, decimalLatitude > -55)

usgsNorm <- usgs %>% 
  filter(year == "1978") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientific_nm`)

# add a column of averaged effort for a species for a year
usgsNorm <- aggregate(.~scientific_nm, FUN=mean, data=usgsNorm[, -3])