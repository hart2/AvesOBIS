# Code written by Savannah Hartman for dissertation chapter 1
# Normalizing Aves collection data by effort (time involved in data collection)

# Dig into the metadata of the top 5-10 datasets since the files downloaded from
# OBIS are a mixed bag. Look at the datasets that give me the most records in 
# the geographic extent I'm looking at.


# USGS Patuxent Wildlife Research Center Seabirds Compendium (coll --------

# manipulated dataset "seabird_data_archive_2" from "seabird_data_archive_NODC_30Dec2013)
# by adding column of years and scientific names, simplifying long and lat to 4 decimal 
# places (trying to match Aves_EEZ), adding effort (found from metadata)


usgs <- read.csv("data/seabird_data_archive_2.csv")
usgs <- usgs %>%  
  filter(!(scientific_nm == "")) %>% 
  select(dataset,title,year,observation_date,scientific_nm,observation_longitude,
         observation_latitude,transect_time_minutes,effort)

usgsNorm <- usgs %>% 
  filter(year == "1978") %>% 
  select(scientific_nm,effort) %>% 
  arrange(`scientific_nm`)

# add a column of averaged effort for a species for a year
usgsNorm <- aggregate(.~scientific_nm, FUN=mean, data=usgsNorm[, -3])

for(year in 1978:2013){
  x <- usgs %>% 
    select(scientific_nm,effort) %>% 
    filter(year == year) %>% 
    arrange(`scientific_nm`)
  x <- aggregate(.~scientific_nm,FUN=mean,data = x[,-3])
  
  usgsNorm <- full_join(usgsNorm,x,by = 'scientific_nm')
}

colnames(usgsNorm) <- c("Scientific Name","1978","1979","1980","1981","1982","1983","1984","1985","1986",
                        "1987","1988","1989","1990","1991","1992","1993","1994","1995",
                        "1996","1997","1998","1999","2000","2001","2002","2003","2004","2005",
                        "2006","2007","2008","2009","2010","2011","2012","2013")

