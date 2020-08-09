# Code written by Savannah Hartman for dissertation chapter 1
# Creating figures which will compare average species richness (aka alpha diversity), # of records
# and latitude 

# ctrlL will clear the console window, ctrlshiftR will create collapsable tab

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
# upload AvesEEZ.csv into R (done previously)
Aves_EEZ <- Aves_EEZ %>% 
  select(scientificName,eventDate,decimalLongitude,decimalLatitude,basisOfRecord,date_year,  
         individualCount, identifiedBy, datasetID, datasetName, dataset_id, institutionCode, 
         ownerInstitutionCode, collectionCode, catalogNumber, occurrenceStatus, samplingEffort) %>%  
  filter(basisOfRecord == "HumanObservation", date_year >= 1960 & date_year < 2019, decimalLatitude > -55)        #filtering for only human observations and data collection post 1960

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
Aves_EEZnd <- invisible(unique(Aves_EEZ))         # "invisible()" suppresses output
rm(Aves_EEZ,num,v1,species,num_gs,gensp,fre1,df1,df2)

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
land_ba <- Aves_EEZnd %>% 
  filter(scientificName == "Bucephala albeola")
land_babuffer <- check_onland(land_ba, buffer = 1000)


# Table: Number of datasets, Records from OBIS -----------------------------
library("readxl")
# install.packages("formattable")
library(formattable)
tbl <- read_excel("data/datasetsForOBIS.xlsx") %>% 
  select(hemisphere, name, numberRecords, makeupPerc)
colnames(tbl) <- c("Hemisphere","Dataset Name","Number of Records","Makeup of Aves OBIS (%)") 
formattable(tbl, 
            align =c("l","c","c","c","r"), 
            list(`Dataset Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")) 
            ))

# Needs a scroll bar, will this work? 

# shinyDashboard(
#   tags$head(
#     tags$style(HTML(".sidebar {
#                       height: 90vh; overflow-y: auto;
#                     }"
#     ) # close HTML       
#     )            # close tags$style
#   ),             # close tags#Head

library("htmltools")
library("webshot") 
# webshot::install_phantomjs()
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}
FT <- formattable(tbl, align =c("l","c","c","c","r"), 
                  list(`Dataset Name` = formatter(
                    "span", style = ~ style(color = "grey",font.weight = "bold")) 
                  )) 

export_formattable(FT,"FT.png")