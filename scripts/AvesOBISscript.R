# Code written by Savannah Hartman for dissertation chapter 1
# Creating figures which will compare average species richness (aka alpha diversity), # of records
# and latitude 

# ctrlL will clear the console window, ctrlshiftR will create collapsible tab

# Introduction, Install Packages and Data ------------------------------------------
install.packages("tidyverse")
# library(tidyverse), package that provides a bunch of tools to help tidy up your messy datasets
install.packages("devtools")
install_github("iobis/robis") # install obis packages 
# You only need to install a package once, but you need to reload it every time you start a new session.


# Data Wrangling OBIS Data---------------------------------------------------------------
library(tidyverse) # packages for data visualization
library(dplyr)
library(readr)
library(devtools)
library(robis)
library(obistools)
library(ggplot2)
library(sf)
library(rnaturalearth)

# data downloaded from OBIS already within lat long limits of the study
Aves_EEZ <- read.csv("Aves_EEZ.csv")
Aves_EEZ <- Aves_EEZ %>% 
  select(scientificName,family,eventDate,date_mid,date_year,decimalLongitude,decimalLatitude,basisOfRecord,  
         individualCount, identifiedBy, datasetID, datasetName, dataset_id, institutionCode, 
         ownerInstitutionCode, collectionCode, catalogNumber) %>%  
  filter(basisOfRecord == "HumanObservation", date_year >= 1960 & date_year < 2019, decimalLatitude > 5)        #filtering for only human observations and data collection post 1960

gensp <- Aves_EEZ %>% 
  select(scientificName)            # Create data frame with only scientific names
# freq <- table(gensp)     create a table with species names and how often they appear in dataset "Aves_EEZ"
freq <- as.data.frame(table(gensp)) # Create a data frame with species names and how often they appear in dataset "Aves_EEZ"
num_gs <- count(freq)               # Counts the number of genus/genus species found in dataset "freq"

# Finding species present Americas: 1960-2020-------------------------------------------------
# (n = # of species), using a splitstring function

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
df1 <- c(species,Aves_EEZ)         # Inputting "species" T/F into "Aves_EEZ" dataframe
df2 <- as.data.frame(df1) %>% 
  filter(species == "TRUE")        # Making it readable as a dataframe and removing genus only

Aves_EEZ <- df2 %>% 
  select(-species)                 # Removing T/F column from Aves dataset

num <- Aves_EEZ %>% 
  select(scientificName) 
freq1 <- as.data.frame(table(num)) 
numSpeciesFreq <- freq1 %>% 
  filter(Freq != 0)                
alphadiv <- count(numSpeciesFreq)  # Number of species found in Aves dataset with genus and species name         


# Remove likely duplicates 
# If scientificName are equal & decimalLongitude are equal & decimalLatitude are equal & eventDate are equal, remove
# one of the observations
# NOTE: This step will take some time
Aves_EEZnd <- invisible(unique(Aves_EEZ)) %>%         # "invisible()" suppresses output
  arrange('scientificName')
rm(Aves_EEZ,num,v1,species,num_gs,gensp,freq,freq1,df1,df2)

# Important!!!
# Aves_EEZnd (aves data without duplicates), alphadiv (alpha diversity from 1960 - present), 
# numSpeciesFreq (frequency of appearance in dataset of genus species )

# Finding alpha diversity per year (Americas) -----------------------------
# NOTE: Actual alpha diversity is the average species diversity in a habitat or specific area 
# Determine how to find alpha diversity per region/habitat (geographic maps?)

# see script "alphaDiversity.R"

# Points on Land ----------------------------------------------------------
# Check to make sure none of the points are on land, check top 5 species to start with
# bucephala albeola, map showing all suspicious records, in orange by default but in red 
# when they are suspicious even with the 1000 m buffer zone:
land_ba <- Aves_EEZnd %>% 
  filter(scientificName == "Bucephala albeola")
land_ba <- check_onland(land_ba)
land_babuffer <- check_onland(land_ba, buffer = 1000)
world <- map_data("world") %>% 
  filter(lat >= -60 & lat <= 60, long >= -150 & long <= -25)

my_title <- expression(paste("Fig. 1 Suspicious Records of ", italic("Bucephala albeola")))
ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group), 
               fill = "#dddddd") +
  geom_point(data = land_ba, 
             aes(x = decimalLongitude, y = decimalLatitude, color = "Buffered")) +     #this is unbuffered data
  geom_point(data = land_babuffer, 
             aes(x = decimalLongitude, y = decimalLatitude, color = "Unbuffered")) +   #this is buffered data
  coord_fixed(1)+
  labs(title = my_title, color = 'Records')+           #title for for graph and legend
  scale_color_manual(values=c("#cc3300", "#ff9900"))+  #specific colors for map
  theme(plot.title = element_text(face="italic"))      #creating a italicized, centered title

# Larus argentatus
land_la <- Aves_EEZnd %>% 
  filter(scientificName == "Larus argentatus")
land_la <- check_onland(land_la)
land_labuffer <- check_onland(land_la, buffer = 1000)

my_title <- expression(paste("Fig. 2 Suspicious Records of ", italic("Larus argentatus")))
ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group), 
               fill = "#dddddd") +
  geom_point(data = land_la, 
             aes(x = decimalLongitude, y = decimalLatitude, color = "Buffered")) +     #this is unbuffered data
  geom_point(data = land_labuffer, 
             aes(x = decimalLongitude, y = decimalLatitude, color = "Unbuffered")) +   #this is buffered data
  coord_fixed(1)+
  labs(title = my_title, color = 'Records')+           #title for for graph and legend
  scale_color_manual(values=c("#cc3300", "#ff9900"))+  #specific colors for map
  theme(plot.title = element_text(face="italic"))      #creating a italicized, centered title

# Morus bassanus
land_mb <- Aves_EEZnd %>% 
  filter(scientificName == "Morus bassanus")
land_mb <- check_onland(land_mb)
land_mbbuffer <- check_onland(land_mb, buffer = 1000)

my_title <- expression(paste("Fig. 3 Suspicious Records of ", italic("Morus bassanus")))
ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group), 
               fill = "#dddddd") +
  geom_point(data = land_mb, 
             aes(x = decimalLongitude, y = decimalLatitude, color = "Buffered")) +     #this is unbuffered data
  geom_point(data = land_mbbuffer, 
             aes(x = decimalLongitude, y = decimalLatitude, color = "Unbuffered")) +   #this is buffered data
  coord_fixed(1)+
  labs(title = my_title, color = 'Records')+           #title for for graph and legend
  scale_color_manual(values=c("#cc3300", "#ff9900"))+  #specific colors for map
  theme(plot.title = element_text(face="italic"))      #creating a italicized, centered title

# Uria aalge, no suspicious records
land_ua <- Aves_EEZnd %>% 
  filter(scientificName == "Uria aalgae")
land_ua <- check_onland(land_ua) # shows that there aren't any suspicious records

# Larus glaucescens
land_lg <- Aves_EEZnd %>% 
  filter(scientificName == "Larus glaucescens")
land_lg <- check_onland(land_lg)
land_lgbuffer <- check_onland(land_lg, buffer = 1000)


my_title <- expression(paste("Fig. 4 Suspicious Records of ", italic("Larus glaucescens")))
ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group), 
               fill = "#dddddd") +
  geom_point(data = land_lg, 
             aes(x = decimalLongitude, y = decimalLatitude, color = "Buffered")) +     #this is unbuffered data
  geom_point(data = land_lgbuffer, 
             aes(x = decimalLongitude, y = decimalLatitude, color = "Unbuffered")) +   #this is buffered data
  coord_fixed(1)+
  labs(title = my_title, color = 'Records')+           #title for for graph and legend
  scale_color_manual(values=c("#cc3300", "#ff9900"))+  #specific colors for map
  theme(plot.title = element_text(face="italic"))      #creating a italicized, centered title


#-------Should remove all suspicious buffered records from original data ------------
# buffers commented out had a zero value, buffer of 1000 m

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

# Aves does not have duplicates or iffy land data
# Make sure time is in the same format
Aves$eventDate_2 <- as.POSIXct(Aves$date_mid/1000, origin="1970-01-01", tz="UTC")

# Remove datasets that don't meet rarefaction standard --------------------

# Keep these datasets:
         # "Aerial Oil Spill Response Survey 1994-1997",
         # "Digital Aerial Baseline Survey of Marine Wildlife in Support of Offshore Wind Energy - OPA 2017", 
         # "Ecological Baseline Studies of the U.S. Outer Continental Shelf Option Year 1", 
         # "Empire Wind Digital Aerial Wildlife Surveys for BOEM Lease Area OCS-A 0520, Equinor Wind US LLC, November 2017-October 2018", 
         # "Gomex Sperm Whale Survey 2000", "IPHC seabird survey 2002-2011",
         # "Mingan Island Cetacean Study 84-07", "MMS Aerial Survey, PNW 1989-1990",
         # "MMS Low Altitude Survey 1980-1983","MMS Ship Survey, PNW 1989",
         # "PIROP Northwest Atlantic 1965-1992","SEFSC GoMex Oceanic 1993 (W)","SEFSC GoMex Oceanic 1996",
         # "SEFSC GoMex Oceanic 2000","SEFSC Gomex Shelf 1994","USGS Patuxent Wildlife Research Center Seabirds Compendium",
         # "WADFW PSAMP S1993","WADFW PSAMP S1994","WADFW PSAMP S1996","WADFW PSAMP S1997",
         # "WADFW PSAMP S1998","WADFW PSAMP S1999","WADFW PSAMP W1993","WADFW PSAMP W1994",
         # "WADFW PSAMP W1995","WADFW PSAMP W1996","WADFW PSAMP W1997","WADFW PSAMP W1998",
         # "WADFW PSAMP W1999","WADFW PSAMP W2000","WADFW PSAMP W2001","WADFW PSAMP W2002",
         # "WADFW PSAMP W2003","WADFW PSAMP W2004")
Aves1 <- Aves%>%
  filter(!(datasetName == "CalCOFI and NMFS Seabird and Marine Mammal Observation Data, 1987-2006"))
Aves1 <- Aves1%>%
  filter(!(datasetName == "Digital Aerial Baseline Survey of Marine Wildlife in Support of Offshore Wind Energy - OPA 2016"))
Aves1 <- Aves1 %>%
  filter(!(datasetName == "MMS Surveys, SCB 1995-1997"))
Aves1 <- Aves1 %>% 
  filter(!(datasetName == "MMS Ship survey, SCB 1975-1978"))

# Seasons -----------------------------------------------------------------

# Added month column via excel
AvWinter1 <- Aves1 %>% 
  filter(month =="12") #December
AvWinter2 <- Aves1 %>%
  filter(month == "1") #January
AvWinter3 <- Aves1%>%
  filter(month == "2") #February
AvSpring1 <- Aves1 %>%
  filter(month == "3") #March
AvSpring2 <- Aves1 %>%
  filter(month == "4") #April
AvSpring3<- Aves1 %>%
  filter(month == "5") #May
AvSummer1<- Aves1 %>%
  filter(month == "6") #June
AvSummer2<- Aves1 %>%
  filter(month == "7") #July
AvSummer3<- Aves1 %>%
  filter(month == "8") #August
AvFall1<- Aves1 %>%
  filter(month == "9") #September
AvFall2<- Aves1 %>%
  filter(month == "10") #October
AvFall3<- Aves1 %>%
  filter(month == "11") #November

Fall <- rbind(AvFall1,AvFall2)
Fall <- rbind(Fall,AvFall3)

Summer <- rbind(AvSummer1,AvSummer2)
Summer <- rbind(Summer,AvSummer3)

Spring <- rbind(AvSpring1,AvSpring2)
Spring <- rbind(Spring,AvSpring3)

Winter <- rbind(AvWinter1,AvWinter2)
Winter <- rbind(Winter,AvWinter3)

rm(AvSpring1,AvSpring2,AvSpring3,AvSummer1,AvSummer2,AvSummer3,AvFall1,AvFall2,AvFall3,
   AvWinter1,AvWinter2,AvWinter3)

Fall$season <- "Fall"
Spring$season <- "Spring"
Summer$season <- "Summer"
Winter$season <- "Winter"

Aves2 <- rbind(Fall,Spring)
Aves2 <- rbind(Aves2,Summer)
Aves2 <- rbind(Aves2,Winter)

rm(Fall,Spring,Summer,Winter,Aves1)

# Long Table: Number of data sets, Records from OBIS (Find better output in shinyAppTable.R) -----------------------------
# before removing duplicates, land_buffer, and data without sampling effort
# library("readxl")
# install.packages("formattable")
# library(formattable)
# library("htmltools")
# library("webshot") 

# formattable(tbl, 
#             align =c("l","c","c","c","r"), 
#             list(`Dataset Name` = formatter(
#               "span", style = ~ style(color = "grey",font.weight = "bold")) 
#             ))
# # webshot::install_phantomjs()
# export_formattable <- function(f, file, width = "100%", height = NULL, 
#                                background = "white", delay = 0.2)
# {
#   w <- as.htmlwidget(f, width = width, height = height)
#   path <- html_print(w, background = background, viewer = NULL)
#   url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
#   webshot(url,
#           file = file,
#           selector = ".formattable_widget",
#           delay = delay)
# }
# FT <- formattable(tbl, align =c("l","c","c","c","r"), 
#                   list(`Dataset Name` = formatter(
#                     "span", style = ~ style(color = "grey",font.weight = "bold")) 
#                   )) 
# 
# export_formattable(FT,"FT.png")

#Note: Aves_EEZnd replaced with Aves_NA (north american datasets only)