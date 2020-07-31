# Code written by Savannah Hartman for dissertation chapter 1
# Normalizing Aves collection data by effort (time involved in data collection)

# Dig into the metadata of the top 5-10 datasets since the files downloaded from
# OBIS are a mixed bag. Look at the datasets that give me the most records in 
# the geographic extent I'm looking at.

# USGS Patuxent Wildlife Research Center Seabirds Compendium
usgs <- read_csv("seabird_data_archive_NODC_30Dec2013.csv")
normusgs <- Aves_EEZnd %>% 
  filter(collectionCode == "") 
# cannot filter by institutionCode b/c NOAA didn't always include it in OBIS
#        seen as NOAA, US Dept of Interior, BOEM/RE, FWS, USGS
# cannot filter by eventDate b/c NOAA didn't always include it
