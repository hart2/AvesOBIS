# Code written by Savannah Hartman for dissertation chapter 1
# Normalizing Aves collection data by effort (time involved in data collection)

# Dig into the metadata of the top 5-10 datasets since the files downloaded from
# OBIS are a mixed bag. Look at the datasets that give me the most records in 
# the geographic extent I'm looking at.


# USGS Patuxent Wildlife Research Center Seabirds Compendium (coll --------
# SHIP AND AERIAL SURVEYS

# manipulated dataset "seabird_data_archive_2" from "seabird_data_archive_NODC_30Dec2013)
library(readr)
library(tidyverse)
library(dplyr)

setwd("~/Chp1/data")
# upload seabird_data.csv and usgs_eaec.csv 
usgs_eaec7873_a1d8_43cf_baa2_27b2772a8d1f <- read_csv("usgs_eaec7873-a1d8-43cf-baa2-27b2772a8d1f.csv")
seabird_data_archive_2 <- read_csv("seabird_data_archive_2.csv")

setwd("~/github/AvesOBIS")
# rename to usgs for simplification, remove blank scientificName
usgs <- usgs_eaec7873_a1d8_43cf_baa2_27b2772a8d1f %>%
  filter(!(scientificName == "")) %>% 
  select(scientificName,eventDate,decimalLongitude,decimalLatitude,basisOfRecord,date_year,  
         individualCount, identifiedBy, institutionCode, ownerInstitutionCode, collectionCode, catalogNumber, 
         occurrenceStatus, effort)%>%  
  filter(basisOfRecord == "HumanObservation", date_year >= 1960 & date_year < 2018, decimalLatitude > -55)

# Finding species present: 1960-2020 (originally wrote in AvesOBISscript.R)
# Removing scientificName that is genus only
gensp <- usgs %>% 
  select(scientificName)            # Create data frame with only scientific names
freq <- as.data.frame(table(gensp)) # Create a data frame with species names and how often they appear in dataset "usgs"
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

# Trying to merge "species" with "usgs" to remove genus only names
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
usgs$eventDate <- format(as.POSIXct(usgs$eventDate,format='%m/%d/%Y %H:%M'),format='%m/%d/%Y')

seabird <- seabird_data_archive_2 %>% 
  select(year,observation_date,scientific_nm,observation_longitude,observation_latitude,transect_time_minutes) 
# rename columns to match usgs
# effort is time (minutes)
colnames(seabird) <- c("date_year","eventDate","scientificName","decimalLongitude","decimalLatitude","effort")
seabird <- seabird %>%  
  # remove scientificName blanks
  filter(!(scientificName == "")) %>% 
  filter(date_year >= 1960 & date_year < 2018, decimalLatitude > -55)
seabird$eventDate <- format(as.POSIXct(seabird$eventDate,format='%m/%d/%Y'),format='%m/%d/%Y')

# Find Species per Year for USGS -------------------------------------------------
# commented out data frames had 0 records

df <- rbind(usgs, seabird) %>% 
  filter(!(effort == "NA"))

rm(df1,df2,freq,gensp,num_gs,v1,species)

usgs1 <- df %>% 
   filter(eventDate == "01/01/1978") %>% 
   select(scientificName,effort) %>%     # effort is the average effort for total of single species identified
   arrange(`scientificName`)

# column of averaged effort for a species for single day
usgs1 <- aggregate(.~scientificName, FUN=mean, data=usgs1[, -3])   # Here seems to be a good spot to count the abundance!!!!
usgs1$date <- c("1/1/1978")

usgs2 <- df %>% 
  filter(eventDate == "1/1/2013") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs2 <- aggregate(.~scientificName, FUN=mean, data=usgs2[, -3])
usgs2$date <- c("1/1/2013")

usgs3 <- df %>% 
  filter(eventDate == "1/10/1978") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs3 <- aggregate(.~scientificName, FUN=mean, data=usgs3[, -3])
usgs3$eventDate <- c("1/10/1978")

usgs4 <- df %>% 
  filter(eventDate == "1/10/1979") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs4 <- aggregate(.~scientificName, FUN=mean, data=usgs4[, -3])
usgs4$eventDate <- c("1/10/1979")

usgs5 <- df %>% 
  filter(eventDate == "1/10/1984") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
usgs5 <- aggregate(.~scientificName, FUN=mean, data=usgs5[, -3])
usgs5$eventDate <- c("1/10/1984")

usgs6 <- df %>%
  filter(eventDate == "1/10/1987") %>%
  select(scientificName,effort) %>%
  arrange(`scientificName`)
usgs6 <- aggregate(.~scientificName, FUN=mean, data=usgs6[, -3])
usgs6$eventDate <- c("1/10/1987")

usgs7 <- df %>%
  filter(eventDate == "1/10/1988") %>%
  select(scientificName,effort) %>%
  arrange(`scientificName`)
usgs7 <- aggregate(.~scientificName, FUN=mean, data=usgs7[, -3])
usgs7$eventDate <- c("1/10/1988")

usgs8 <- df %>%
  filter(eventDate == "1/10/1992") %>%
  select(scientificName,effort) %>%
  arrange(`scientificName`)
usgs8 <- aggregate(.~scientificName, FUN=mean, data=usgs8[, -3])
usgs8$eventDate <- c("1/10/1992")

usgs9 <- df %>%
  filter(eventDate == "1/11/1978") %>%
  select(scientificName,effort) %>%
  arrange(`scientificName`)
usgs9 <- aggregate(.~scientificName, FUN=mean, data=usgs9[, -3])
usgs9$eventDate <- c("1/11/1978")

usgs10 <- df %>%
  filter(eventDate == "1/11/1979") %>%
  select(scientificName,effort) %>%
  arrange(`scientificName`)
usgs10 <- aggregate(.~scientificName, FUN=mean, data=usgs10[, -3])
usgs10$eventDate <- c("1/11/1979")

usgs11 <- df %>%
  filter(eventDate == "1/11/1984") %>%
  select(scientificName,effort) %>%
  arrange(`scientificName`)
usgs11 <- aggregate(.~scientificName, FUN=mean, data=usgs11[, -3])
usgs11$eventDate <- c("1/11/1984")

usgs12 <- df %>%
  filter(eventDate == "1/11/1985") %>%
  select(scientificName,effort) %>%
  arrange(`scientificName`)
usgs12 <- aggregate(.~scientificName, FUN=mean, data=usgs12[, -3])
usgs12$eventDate <- c("1/11/1985")

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
# 
# usgs15 <- df %>% 
#   filter(date_year == "1992") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs15 <- aggregate(.~scientificName, FUN=mean, data=usgs15[, -3])
# usgs15$year <- c(1992)
# 
# usgs16 <- df %>% 
#   filter(date_year == "1993") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs16 <- aggregate(.~scientificName, FUN=mean, data=usgs16[, -3])
# usgs16$year <- c(1993)
# 
# usgs17 <- df %>% 
#   filter(date_year == "1994") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs17 <- aggregate(.~scientificName, FUN=mean, data=usgs17[, -3])
# usgs17$year <- c(1994)
# 
# usgs18 <- df %>% 
#   filter(date_year == "1995") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs18 <- aggregate(.~scientificName, FUN=mean, data=usgs18[, -3])
# usgs18$year <- c(1995)
# 
# usgs19 <- df %>% 
#   filter(date_year == "1996") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs19 <- aggregate(.~scientificName, FUN=mean, data=usgs19[, -3])
# usgs19$year <- c(1996)
# 
# usgs20 <- df %>% 
#   filter(date_year == "1997") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs20 <- aggregate(.~scientificName, FUN=mean, data=usgs20[, -3])
# usgs20$year <- c(1997)
# 
# usgs21 <- df %>% 
#   filter(date_year == "1998") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs21 <- aggregate(.~scientificName, FUN=mean, data=usgs21[, -3])
# usgs21$year <- c(1998)
# 
# usgs22 <- df %>% 
#   filter(date_year == "1999") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs22 <- aggregate(.~scientificName, FUN=mean, data=usgs22[, -3])
# usgs22$year <- c(1999)
# 
# usgs23 <- df %>% 
#   filter(date_year == "2000") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs23 <- aggregate(.~scientificName, FUN=mean, data=usgs23[, -3])
# usgs23$year <- c(2000)
# 
# usgs24 <- df %>% 
#   filter(date_year == "2001") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs24 <- aggregate(.~scientificName, FUN=mean, data=usgs24[, -3])
# usgs24$year <- c(2001)
# 
# usgs25 <- df %>% 
#   filter(date_year == "2002") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs25 <- aggregate(.~scientificName, FUN=mean, data=usgs25[, -3])
# usgs25$year <- c(2002)
# 
# usgs26 <- df %>% 
#   filter(date_year == "2003") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs26 <- aggregate(.~scientificName, FUN=mean, data=usgs26[, -3])
# usgs26$year <- c(2003)
# 
# usgs27 <- df %>% 
#   filter(date_year == "2004") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs27 <- aggregate(.~scientificName, FUN=mean, data=usgs27[, -3])
# usgs27$year <- c(2004)
# 
# usgs28 <- df %>% 
#   filter(date_year == "2005") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs28 <- aggregate(.~scientificName, FUN=mean, data=usgs28[, -3])
# usgs28$year <- c(2005)
# 
# usgs29 <- df %>% 
#   filter(date_year == "2006") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs29 <- aggregate(.~scientificName, FUN=mean, data=usgs29[, -3])
# usgs29$year <- c(2006)
# 
# usgs30 <- df %>% 
#   filter(date_year == "2007") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs30 <- aggregate(.~scientificName, FUN=mean, data=usgs30[, -3])
# usgs30$year <- c(2007)
# 
# usgs31 <- df %>% 
#   filter(date_year == "2008") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs31 <- aggregate(.~scientificName, FUN=mean, data=usgs31[, -3])
# usgs31$year <- c(2008)
# 
# usgs32 <- df %>% 
#   filter(date_year == "2009") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs32 <- aggregate(.~scientificName, FUN=mean, data=usgs32[, -3])
# usgs32$year <- c(2009)
# 
# usgs33 <- df %>% 
#   filter(date_year == "2010") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs33 <- aggregate(.~scientificName, FUN=mean, data=usgs33[, -3])
# usgs33$year <- c(2010)
# 
# usgs34 <- df %>% 
#   filter(date_year == "2011") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs34 <- aggregate(.~scientificName, FUN=mean, data=usgs34[, -3])
# usgs34$year <- c(2011)
# 
# usgs35 <- df %>% 
#   filter(date_year == "2012") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs35 <- aggregate(.~scientificName, FUN=mean, data=usgs35[, -3])
# usgs35$year <- c(2012)
# 
# usgs36 <- df %>% 
#   filter(date_year == "2013") %>% 
#   select(scientificName,effort) %>% 
#   arrange(`scientificName`)
# usgs36 <- aggregate(.~scientificName, FUN=mean, data=usgs36[, -3])
# usgs36$year <- c(2013)

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

rm(usgs1,usgs2,usgs3,usgs4,usgs5,usgs6,usgs7,usgs8,usgs9,usgs10,usgs11,usgs15,
   usgs16,usgs17,usgs18,usgs19,usgs20,usgs21,usgs22,usgs23,usgs24,usgs25,usgs26,
   usgs27,usgs28,usgs29,usgs30,usgs31,usgs32,usgs33,usgs34,usgs35,usgs36)


rm(usgs_eaec7873_a1d8_43cf_baa2_27b2772a8d1f)
# Ecological Baseline Studies of the U.S. Outer Continental Shelf  --------
# AERIAL SURVEYS
library(readr)
eco <- read_csv("~/Chp1/data/ecologicalBaseline_b26ccb50-aab3-400d-ac62-b2b421a25b6b.csv")
eco <- eco %>%
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
rm(seabird_data_archive_2,seabird)

# MMS Low Altitude Survey 1980-1983 ---------------------------------------
# AERIAL SURVEYS

# CalCOFI and NMFS Seabird and Marine Mammal Observation Data, 198 --------
# SHIP SURVEYS

setwd("~/Chp1/data/3a9eb868-6924-4776-8040-bd635cbd2bfd")
X1488405241_allspecieslist <- read_csv("1488405241_allspecieslist.csv")

speciesList <- X1488405241_allspecieslist %>% 
  select(Species,`Latin Name`)
colnames(speciesList) <- c("Species","scientificName")

setwd("~/Chp1/data")
CalCOFI_NMFS_3a9eb868_6924_4776_8040_bd635cbd2bfd <- read_csv("CalCOFI_NMFS_3a9eb868-6924-4776-8040-bd635cbd2bfd.csv")

setwd("~/github/AvesOBIS/scripts")
source("CalCOFI_NMFS.R")

# put scientificName into data table of dt 2,4,6 (join by Species)
dt2 <- right_join(speciesList,dt2, by ="Species") %>% 
  filter(!(scientificName == ""))
dt4 <- right_join(speciesList,dt4, by ="Species") %>% 
  filter(!(scientificName == ""))
dt6 <- right_join(speciesList,dt6, by ="Species") %>% 
  filter(!(scientificName == ""))

# put scientific Names into NEW data tables (rename dt1,dt2,dt3) (join by GIS.key - aka lon/lat)
dt1 <- left_join(dt1,dt2, by = "GIS.key") %>% 
  filter(!(scientificName == ""))

dt2 <- left_join(dt3,dt4, by = "GIS.key") %>% 
  filter(!(scientificName == ""))

dt3 <- left_join(dt5,dt6, by = "GIS.key") %>% 
  filter(!(scientificName == ""))

# df4 will be the final data frame with the above calcofi data
df4 <- rbind(dt1,dt2)
df4 <- rbind(df4,dt3)
df4 <- add_column(df4, effort = (df4$Time / 60)) # get effort of time (min)
df4 <- df4 %>% 
  select(Date,Longitude.Mid,Latitude.Mid,scientificName,effort) %>% 
  filter(!(effort == "Inf"))
colnames(df4) <- c("eventDate","decimalLongitude","decimalLatitude","scientificName","effort")
options(digits = 6)
df4 <- df4 %>%
  dplyr::mutate(year = lubridate::year(eventDate))

#calcofi data from from Aves - OBIS eml
calco <- CalCOFI_NMFS_3a9eb868_6924_4776_8040_bd635cbd2bfd %>%
  filter(!(scientificName == "")) %>% 
  select(scientificName,eventDate,decimalLongitude,decimalLatitude,basisOfRecord,date_year,  
         individualCount, identifiedBy, institutionCode, ownerInstitutionCode, collectionCode, catalogNumber, 
         occurrenceStatus, samplingEffort)%>%  
  filter(basisOfRecord == "HumanObservation", date_year >= 1960 & date_year < 2019, 
         decimalLatitude > -55, decimalLongitude >= -126.474 & decimalLongitude <= -117.234)

# Finding species present: 1960-2020 (originally wrote in AvesOBISscript.R)
# Removing scientificName that is genus only
gensp <- calco %>% 
  select(scientificName)            # Create data frame with only scientific names
freq <- as.data.frame(table(gensp)) # Create a data frame with species names and how often they appear in dataset "calco"
num_gs <- count(freq)               # Counts the number of genus/genus species found in dataset "freq"

v1 <- gensp                         # vector with scientificNames
v2 <- 1:nrow(gensp)                 # number of cells in scientificName and creating vector with the number of cells necessary for running splitstring fxn
species <- data.frame(v1,v2)
colnames(species) <- c("scientificName", "v2")

species <- alpha(species)           # Gives True/False whether scientifcName contains a space
species <- as.data.frame(species)   # Creating into a vector

# Trying to merge "species" with "calco" to remove genus only names
df1 <- c(species,calco)             # Inputting "species" T/F into "eco" dataframe
df2 <- as.data.frame(df1) %>% 
  filter(species == "TRUE")         # Making it readable as a dataframe and removing genus only

calco <- df2 %>%                    # final step just making sure there aren't any true/false statements
  select(-species)

# making sure calco has columns needed
calco <- calco %>% 
  select(eventDate,scientificName,decimalLongitude,decimalLatitude)
options(digits = 6)                 # only 4 decimal places for lat and lon
calco <- calco %>%
  add_column(NA)
colnames(calco) <- c("eventDate","scientificName","decimalLongitude","decimalLatitude","effort")
calco$eventDate <- as.Date(calco$eventDate)
# reformatting the date to get a column of just years
calco <- calco %>%
  dplyr::mutate(year = lubridate::year(eventDate))


# joining together calco and df4, remove empty effort variables
df4 <- df4 %>% 
  filter(year >= 1960 & year <= 2006) %>% 
  filter(decimalLongitude >= -126.474 & decimalLongitude <= -117.234)
df4 <- rbind(calco,df4) %>% 
  filter(!(effort == "NA"))

# creating a data frame of averaged effort for a species for a year
calco1 <- df4 %>% 
  filter(year == "1987") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco1 <- aggregate(.~scientificName, FUN=mean, data=calco1[, -3])
calco1$year <- c(1987)

calco2 <- df4 %>% 
  filter(year == "1988") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco2 <- aggregate(.~scientificName, FUN=mean, data=calco2[, -3])
calco2$year <- c(1988)

calco3 <- df4 %>% 
  filter(year == "1989") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco3 <- aggregate(.~scientificName, FUN=mean, data=calco3[, -3])
calco3$year <- c(1989)

calco4 <- df4 %>% 
  filter(year == "1990") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco4 <- aggregate(.~scientificName, FUN=mean, data=calco4[, -3])
calco4$year <- c(1990)

calco5 <- df4 %>% 
  filter(year == "1991") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco5 <- aggregate(.~scientificName, FUN=mean, data=calco5[, -3])
calco5$year <- c(1991)

calco6 <- df4 %>% 
  filter(year == "1992") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco6 <- aggregate(.~scientificName, FUN=mean, data=calco6[, -3])
calco6$year <- c(1992)

calco7 <- df4 %>% 
  filter(year == "1993") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco7 <- aggregate(.~scientificName, FUN=mean, data=calco7[, -3])
calco7$year <- c(1993)

calco8 <- df4 %>% 
  filter(year == "1994") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco8 <- aggregate(.~scientificName, FUN=mean, data=calco8[, -3])
calco8$year <- c(1994)

calco9 <- df4 %>% 
  filter(year == "1995") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco9 <- aggregate(.~scientificName, FUN=mean, data=calco9[, -3])
calco9$year <- c(1995)

calco10 <- df4 %>% 
  filter(year == "1996") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco10 <- aggregate(.~scientificName, FUN=mean, data=calco10[, -3])
calco10$year <- c(1996)

calco11 <- df4 %>% 
  filter(year == "1997") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco11 <- aggregate(.~scientificName, FUN=mean, data=calco11[, -3])
calco11$year <- c(1997)

calco12 <- df4 %>% 
  filter(year == "1998") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco12 <- aggregate(.~scientificName, FUN=mean, data=calco12[, -3])
calco12$year <- c(1998)

calco13 <- df4 %>% 
  filter(year == "1999") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco13 <- aggregate(.~scientificName, FUN=mean, data=calco13[, -3])
calco13$year <- c(1999)

calco14 <- df4 %>% 
  filter(year == "2000") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco14 <- aggregate(.~scientificName, FUN=mean, data=calco14[, -3])
calco14$year <- c(2000)

calco15 <- df4 %>% 
  filter(year == "2001") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco15 <- aggregate(.~scientificName, FUN=mean, data=calco15[, -3])
calco15$year <- c(2001)

calco16 <- df4 %>% 
  filter(year == "2002") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco16 <- aggregate(.~scientificName, FUN=mean, data=calco16[, -3])
calco16$year <- c(2002)

calco17 <- df4 %>% 
  filter(year == "2003") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco17 <- aggregate(.~scientificName, FUN=mean, data=calco17[, -3])
calco17$year <- c(2003)

calco18 <- df4 %>% 
  filter(year == "2004") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco18 <- aggregate(.~scientificName, FUN=mean, data=calco18[, -3])
calco18$year <- c(2004)

calco19 <- df4 %>% 
  filter(year == "2005") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco19 <- aggregate(.~scientificName, FUN=mean, data=calco19[, -3])
calco19$year <- c(2005)

calco20 <- df4 %>% 
  filter(year == "2006") %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)
calco20 <- aggregate(.~scientificName, FUN=mean, data=calco20[, -3])
calco20$year <- c(2006)

calcoNorm <- full_join(calco1,calco2) %>% 
  full_join(calco3) %>% 
  full_join(calco4) %>% 
  full_join(calco5) %>% 
  full_join(calco6) %>% 
  full_join(calco7) %>% 
  full_join(calco8) %>% 
  full_join(calco9) %>% 
  full_join(calco10) %>% 
  full_join(calco11) %>% 
  full_join(calco12) %>%
  full_join(calco13) %>%
  full_join(calco14) %>%
  full_join(calco15) %>% 
  full_join(calco16) %>% 
  full_join(calco17) %>% 
  full_join(calco18) %>% 
  full_join(calco19) %>% 
  full_join(calco20)

rm(calco1,calco2,calco3,calco4,calco5,calco6,calco7,calco8,calco9,calco10,calco11,
   calco12,calco13,calco14,calco15,calco16,calco17,calco18,calco19,calco20)
rm(df1,df2,freq,gensp,num_gs,v1,v2,species)
rm(dt1,dt2,dt3,dt4,dt5,dt6,speciesList,X1488405241_allspecieslist,
   CalCOFI_NMFS_3a9eb868_6924_4776_8040_bd635cbd2bfd)
# PIROP Northwest Atlantic 1965-1992 --------------------------------------
# SHIP SURVEYS
