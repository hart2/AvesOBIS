# Code written by Savannah Hartman for dissertation chapter 1
# Normalizing Aves collection data by effort (time involved in data collection)

# Dig into the metadata of the top 5-10 datasets since the files downloaded from
# OBIS are a mixed bag. Look at the datasets that give me the most records in 
# the geographic extent I'm looking at.


# USGS Patuxent Wildlife Research Center Seabirds Compendium (coll --------
# SHIP SURVEYS

# manipulated dataset "seabird_data_archive_2" from "seabird_data_archive_NODC_30Dec2013)

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
gensp <- usgs %>% 
  select(scientificName)            # Create data frame with only scientific names
freq <- as.data.frame(table(gensp)) # Create a data frame with species names and how often they appear in dataset "Aves_EEZ"
num_gs <- count(freq)               # Counts the number of genus/genus species found in dataset "freq"

v1 <- gensp                         # vector with scientificNames
v2 <- 1:nrow(gensp)                 # number of cells in scientificName and creating vector with the number of cells necessary for running splitstring fxn
species <- data.frame(v1,v2)
colnames(species) <- c("scientificName", "v2")

alpha <- function(species){         # Fxn to filter data frame to include only rows with a space between two character strings (aka genus and species)
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

# Find Species per Year for USGS -------------------------------------------------
# commented out data frames had 0 records

df <- rbind(usgs, seabird) %>% 
  # remove scientificName not found in usgs
  filter(!(scientificName == "Ardea alba" | scientificName == "Actitis macularius" | 
             scientificName == "Calonectris borealis")) %>% 
  filter(!(effort == "NA"))

rm(df1,df2,freqUSGS,genspUSGS,num_gsUSGS,v1,species)

usgs1 <- df %>% 
   filter(date_year == "1978") %>% 
   select(scientificName,effort) %>% 
   arrange(`scientificName`)

# column of averaged effort for a species for a year
usgs1 <- aggregate(.~scientificName, FUN=mean, data=usgs1[, -3])
usgs1$year <- c(1978)

usgs2 <- df %>% 
  filter(date_year == "1979") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs2 <- aggregate(.~scientificName, FUN=mean, data=usgs2[, -3])
usgs2$year <- c(1979)

usgs3 <- df %>% 
  filter(date_year == "1980") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs3 <- aggregate(.~scientificName, FUN=mean, data=usgs3[, -3])
usgs3$year <- c(1980)

usgs4 <- df %>% 
  filter(date_year == "1981") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs4 <- aggregate(.~scientificName, FUN=mean, data=usgs4[, -3])
usgs4$year <- c(1981)

usgs5 <- df %>% 
  filter(date_year == "1982") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs5 <- aggregate(.~scientificName, FUN=mean, data=usgs5[, -3])
usgs5$year <- c(1982)

usgs6 <- df %>% 
  filter(date_year == "1983") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs6 <- aggregate(.~scientificName, FUN=mean, data=usgs6[, -3])
usgs6$year <- c(1983)

usgs7 <- df %>% 
  filter(date_year == "1984") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs7 <- aggregate(.~scientificName, FUN=mean, data=usgs7[, -3])
usgs7$year <- c(1984)

usgs8 <- df %>% 
  filter(date_year == "1985") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs8 <- aggregate(.~scientificName, FUN=mean, data=usgs8[, -3])
usgs8$year <- c(1985)

usgs9 <- df %>% 
  filter(date_year == "1986") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs9 <- aggregate(.~scientificName, FUN=mean, data=usgs9[, -3])
usgs9$year <- c(1986)

usgs10 <- df %>% 
  filter(date_year == "1987") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs10 <- aggregate(.~scientificName, FUN=mean, data=usgs10[, -3])
usgs10$year <- c(1987)

usgs11 <- df %>% 
  filter(date_year == "1988") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs11 <- aggregate(.~scientificName, FUN=mean, data=usgs11[, -3])
usgs11$year <- c(1988)

# usgs12 <- df %>% 
#   filter(date_year == "1989") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs12 <- aggregate(.~scientificName, FUN=mean, data=usgs12[, -3])
# usgs12$year <- c(1989)
# 
# usgs13 <- df %>% 
#   filter(date_year == "1990") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs13 <- aggregate(.~scientificName, FUN=mean, data=usgs13[, -3])
# usgs13$year <- c(1990)
# 
# usgs14 <- df %>% 
#   filter(date_year == "1991") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs14 <- aggregate(.~scientificName, FUN=mean, data=usgs14[, -3])
# usgs14$year <- c(1991)

usgs15 <- df %>% 
  filter(date_year == "1992") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs15 <- aggregate(.~scientificName, FUN=mean, data=usgs15[, -3])
usgs15$year <- c(1992)

usgs16 <- df %>% 
  filter(date_year == "1993") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs16 <- aggregate(.~scientificName, FUN=mean, data=usgs16[, -3])
usgs16$year <- c(1993)

usgs17 <- df %>% 
  filter(date_year == "1994") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs17 <- aggregate(.~scientificName, FUN=mean, data=usgs17[, -3])
usgs17$year <- c(1994)

usgs18 <- df %>% 
  filter(date_year == "1995") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs18 <- aggregate(.~scientificName, FUN=mean, data=usgs18[, -3])
usgs18$year <- c(1995)

usgs19 <- df %>% 
  filter(date_year == "1996") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs19 <- aggregate(.~scientificName, FUN=mean, data=usgs19[, -3])
usgs19$year <- c(1996)

usgs20 <- df %>% 
  filter(date_year == "1997") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs20 <- aggregate(.~scientificName, FUN=mean, data=usgs20[, -3])
usgs20$year <- c(1997)

usgs21 <- df %>% 
  filter(date_year == "1998") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs21 <- aggregate(.~scientificName, FUN=mean, data=usgs21[, -3])
usgs21$year <- c(1998)

usgs22 <- df %>% 
  filter(date_year == "1999") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs22 <- aggregate(.~scientificName, FUN=mean, data=usgs22[, -3])
usgs22$year <- c(1999)

usgs23 <- df %>% 
  filter(date_year == "2000") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs23 <- aggregate(.~scientificName, FUN=mean, data=usgs23[, -3])
usgs23$year <- c(2000)

usgs24 <- df %>% 
  filter(date_year == "2001") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs24 <- aggregate(.~scientificName, FUN=mean, data=usgs24[, -3])
usgs24$year <- c(2001)

usgs25 <- df %>% 
  filter(date_year == "2002") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs25 <- aggregate(.~scientificName, FUN=mean, data=usgs25[, -3])
usgs25$year <- c(2002)

usgs26 <- df %>% 
  filter(date_year == "2003") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs26 <- aggregate(.~scientificName, FUN=mean, data=usgs26[, -3])
usgs26$year <- c(2003)

usgs27 <- df %>% 
  filter(date_year == "2004") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs27 <- aggregate(.~scientificName, FUN=mean, data=usgs27[, -3])
usgs27$year <- c(2004)

usgs28 <- df %>% 
  filter(date_year == "2005") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs28 <- aggregate(.~scientificName, FUN=mean, data=usgs28[, -3])
usgs28$year <- c(2005)

usgs29 <- df %>% 
  filter(date_year == "2006") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs29 <- aggregate(.~scientificName, FUN=mean, data=usgs29[, -3])
usgs29$year <- c(2006)

usgs30 <- df %>% 
  filter(date_year == "2007") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs30 <- aggregate(.~scientificName, FUN=mean, data=usgs30[, -3])
usgs30$year <- c(2007)

usgs31 <- df %>% 
  filter(date_year == "2008") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs31 <- aggregate(.~scientificName, FUN=mean, data=usgs31[, -3])
usgs31$year <- c(2008)

usgs32 <- df %>% 
  filter(date_year == "2009") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs32 <- aggregate(.~scientificName, FUN=mean, data=usgs32[, -3])
usgs32$year <- c(2009)

usgs33 <- df %>% 
  filter(date_year == "2010") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs33 <- aggregate(.~scientificName, FUN=mean, data=usgs33[, -3])
usgs33$year <- c(2010)

usgs34 <- df %>% 
  filter(date_year == "2011") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs34 <- aggregate(.~scientificName, FUN=mean, data=usgs34[, -3])
usgs34$year <- c(2011)

usgs35 <- df %>% 
  filter(date_year == "2012") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs35 <- aggregate(.~scientificName, FUN=mean, data=usgs35[, -3])
usgs35$year <- c(2012)

usgs36 <- df %>% 
  filter(date_year == "2013") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs36 <- aggregate(.~scientificName, FUN=mean, data=usgs36[, -3])
usgs36$year <- c(2013)

# usgsNorm gives sampling effort for every species every year present in the data set
usgsNorm <- full_join(usgs1,usgs2) %>% 
  full_join(usgs3) %>% 
  full_join(usgs4) %>% 
  full_join(usgs5) %>% 
  full_join(usgs6) %>% 
  full_join(usgs7) %>% 
  full_join(usgs8) %>% 
  full_join(usgs9) %>% 
  full_join(usgs10) %>% 
  full_join(usgs11) %>% 
  # full_join(usgs12) %>% 
  # full_join(usgs13) %>% 
  # full_join(usgs14) %>% 
  full_join(usgs15) %>% 
  full_join(usgs16) %>% 
  full_join(usgs17) %>% 
  full_join(usgs18) %>% 
  full_join(usgs19) %>% 
  full_join(usgs20) %>% 
  full_join(usgs21) %>% 
  full_join(usgs22) %>% 
  full_join(usgs23) %>% 
  full_join(usgs24) %>% 
  full_join(usgs25) %>% 
  full_join(usgs26) %>% 
  full_join(usgs27) %>% 
  full_join(usgs28) %>% 
  full_join(usgs29) %>% 
  full_join(usgs30) %>% 
  full_join(usgs31) %>% 
  full_join(usgs32) %>% 
  full_join(usgs33) %>% 
  full_join(usgs34) %>% 
  full_join(usgs35) %>% 
  full_join(usgs36)

rm(usgs1,usgs2,usgs3,usgs4,usgs5,usgs6,usgs7,usgs8,usgs9,usgs10,usgs11,usgs12,
   usgs13,usgs14,usgs15,usgs16,usgs17,usgs18,usgs19,usgs20,usgs21,usgs22,usgs23,
   usgs24,usgs25,usgs26,usgs27,usgs28,usgs29,usgs30,usgs31,usgs32,usgs33,usgs34,
   usgs35,usgs36)

# Gives total effort for each species from 1978-2013 (want it separated by year)
# splitmean <- function(df) {
#   s <- split(df, df$scientificName)
#   sapply(s, function(x) mean(x$effort) )
# }
# z <- splitmean(df)

rm(usgs_eaec7873_a1d8_43cf_baa2_27b2772a8d1f)
# Ecological Baseline Studies of the U.S. Outer Continental Shelf  --------
# AERIAL SURVEYS

eco <- ecologicalBaseline_b26ccb50_aab3_400d_ac62_b2b421a25b6b %>%
  filter(!(scientificName == "")) %>% 
  select(scientificName,eventDate,decimalLongitude,decimalLatitude,basisOfRecord,date_year,  
         individualCount, identifiedBy, institutionCode, ownerInstitutionCode, collectionCode, catalogNumber, 
         occurrenceStatus, samplingEffort)%>%  
  filter(basisOfRecord == "HumanObservation", date_year >= 1960 & date_year < 2019, decimalLatitude > -55)

# Finding species present: 1960-2020 (originally wrote in AvesOBISscript.R)
# Removing scientificName that is genus only
gensp <- eco %>% 
  select(scientificName)            # Create data frame with only scientific names
freq <- as.data.frame(table(gensp)) # Create a data frame with species names and how often they appear in dataset "Aves_EEZ"
num_gs <- count(freq)               # Counts the number of genus/genus species found in dataset "freq"

v1 <- gensp                         # vector with scientificNames
v2 <- 1:nrow(gensp)                 # number of cells in scientificName and creating vector with the number of cells necessary for running splitstring fxn
species <- data.frame(v1,v2)
colnames(species) <- c("scientificName", "v2")

species <- alpha(species)           # Gives True/False whether scientifcName contains a space
species <- as.data.frame(species)   # Creating into a vector

# Trying to merge "species" with "Aves_EEZ" to remove genus only names
df1 <- c(species,eco)               # Inputting "species" T/F into "eco" dataframe
df2 <- as.data.frame(df1) %>% 
  filter(species == "TRUE")         # Making it readable as a dataframe and removing genus only

eco <- df2 %>% 
  select(-species)

# making sure usgs has columns needed, only 4 decimal places for lat and lon
eco <- eco %>% 
  select(date_year,eventDate,scientificName,decimalLongitude,decimalLatitude)
options(digits = 6)
eco$effort <- NA

rm(df1,df2,freq,gensp,num_gs,v1,v2,species)


# MMS Low Altitude Survey 1980-1983 ---------------------------------------
# AERIAL SURVEYS

# CalCOFI and NMFS Seabird and Marine Mammal Observation Data, 198 --------
# SHIP SURVEYS

# PIROP Northwest Atlantic 1965-1992 --------------------------------------
# sHIP SURVEYS
