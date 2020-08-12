# Package ID: knb-lter-cce.255.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Bird and mammal observations aboard CalCOFI (1987-2015, ongoing), NMFS (1996-2015, ongoing) and CPR (2003-2006, completed) cruises. .
# Data set creator:    - Farallon Institute Advanced Ecosystem Research 
# Data set creator:    - CalCOFI - Scripps Institution of Oceanography 
# Data set creator:    - California Current Ecosystem LTER 
# Data set creator:  Bill Sydeman - FIAER 
# Metadata Provider:    - California Current Ecosystem LTER 
# Contact:    - CCE LTER Information Manager California Current Ecosystem LTER  - ccelter-im@ucsd.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cce/255/2/30f95e31357695b063edf028635f3cef" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "GIS.key",     
                 "Cruise",     
                 "Transect.number",     
                 "Bin.number",     
                 "Date",     
                 "Time",     
                 "Latitude.Start",     
                 "Longitude.Start",     
                 "Latitude.Mid",     
                 "Longitude.Mid",     
                 "Latitude.Stop",     
                 "Longitude.Stop",     
                 "Length",     
                 "Width",     
                 "Area",     
                 "Depth",     
                 "Julian.date",     
                 "Julian.day",     
                 "SVY",     
                 "Season"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$GIS.key)!="factor") dt1$GIS.key<- as.factor(dt1$GIS.key)
if (class(dt1$Cruise)!="factor") dt1$Cruise<- as.factor(dt1$Cruise)
if (class(dt1$Transect.number)!="factor") dt1$Transect.number<- as.factor(dt1$Transect.number)
if (class(dt1$Bin.number)!="factor") dt1$Bin.number<- as.factor(dt1$Bin.number)                                   
# attempting to convert dt1$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1Date<-as.Date(dt1$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1Date) == length(tmp1Date[!is.na(tmp1Date)])){dt1$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1Date) 
if (class(dt1$Time)=="factor") dt1$Time <-as.numeric(levels(dt1$Time))[as.integer(dt1$Time) ]               
if (class(dt1$Time)=="character") dt1$Time <-as.numeric(dt1$Time)
if (class(dt1$Latitude.Start)=="factor") dt1$Latitude.Start <-as.numeric(levels(dt1$Latitude.Start))[as.integer(dt1$Latitude.Start) ]               
if (class(dt1$Latitude.Start)=="character") dt1$Latitude.Start <-as.numeric(dt1$Latitude.Start)
if (class(dt1$Longitude.Start)=="factor") dt1$Longitude.Start <-as.numeric(levels(dt1$Longitude.Start))[as.integer(dt1$Longitude.Start) ]               
if (class(dt1$Longitude.Start)=="character") dt1$Longitude.Start <-as.numeric(dt1$Longitude.Start)
if (class(dt1$Latitude.Mid)=="factor") dt1$Latitude.Mid <-as.numeric(levels(dt1$Latitude.Mid))[as.integer(dt1$Latitude.Mid) ]               
if (class(dt1$Latitude.Mid)=="character") dt1$Latitude.Mid <-as.numeric(dt1$Latitude.Mid)
if (class(dt1$Longitude.Mid)=="factor") dt1$Longitude.Mid <-as.numeric(levels(dt1$Longitude.Mid))[as.integer(dt1$Longitude.Mid) ]               
if (class(dt1$Longitude.Mid)=="character") dt1$Longitude.Mid <-as.numeric(dt1$Longitude.Mid)
if (class(dt1$Latitude.Stop)=="factor") dt1$Latitude.Stop <-as.numeric(levels(dt1$Latitude.Stop))[as.integer(dt1$Latitude.Stop) ]               
if (class(dt1$Latitude.Stop)=="character") dt1$Latitude.Stop <-as.numeric(dt1$Latitude.Stop)
if (class(dt1$Longitude.Stop)=="factor") dt1$Longitude.Stop <-as.numeric(levels(dt1$Longitude.Stop))[as.integer(dt1$Longitude.Stop) ]               
if (class(dt1$Longitude.Stop)=="character") dt1$Longitude.Stop <-as.numeric(dt1$Longitude.Stop)
if (class(dt1$Length)=="factor") dt1$Length <-as.numeric(levels(dt1$Length))[as.integer(dt1$Length) ]               
if (class(dt1$Length)=="character") dt1$Length <-as.numeric(dt1$Length)
if (class(dt1$Width)=="factor") dt1$Width <-as.numeric(levels(dt1$Width))[as.integer(dt1$Width) ]               
if (class(dt1$Width)=="character") dt1$Width <-as.numeric(dt1$Width)
if (class(dt1$Area)=="factor") dt1$Area <-as.numeric(levels(dt1$Area))[as.integer(dt1$Area) ]               
if (class(dt1$Area)=="character") dt1$Area <-as.numeric(dt1$Area)
if (class(dt1$Depth)=="factor") dt1$Depth <-as.numeric(levels(dt1$Depth))[as.integer(dt1$Depth) ]               
if (class(dt1$Depth)=="character") dt1$Depth <-as.numeric(dt1$Depth)
if (class(dt1$Julian.date)!="factor") dt1$Julian.date<- as.factor(dt1$Julian.date)
if (class(dt1$Julian.day)!="factor") dt1$Julian.day<- as.factor(dt1$Julian.day)
if (class(dt1$SVY)!="factor") dt1$SVY<- as.factor(dt1$SVY)
if (class(dt1$Season)!="factor") dt1$Season<- as.factor(dt1$Season)

# Convert Missing Values to NA for non-dates

dt1$GIS.key <- as.factor(ifelse((trimws(as.character(dt1$GIS.key))==trimws("NULL")),NA,as.character(dt1$GIS.key)))
dt1$Cruise <- as.factor(ifelse((trimws(as.character(dt1$Cruise))==trimws("NULL")),NA,as.character(dt1$Cruise)))
dt1$Transect.number <- as.factor(ifelse((trimws(as.character(dt1$Transect.number))==trimws("NULL")),NA,as.character(dt1$Transect.number)))
dt1$Bin.number <- as.factor(ifelse((trimws(as.character(dt1$Bin.number))==trimws("NULL")),NA,as.character(dt1$Bin.number)))
dt1$Time <- ifelse((trimws(as.character(dt1$Time))==trimws("NULL")),NA,dt1$Time)               
suppressWarnings(dt1$Time <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt1$Time))==as.character(as.numeric("NULL"))),NA,dt1$Time))
dt1$Latitude.Start <- ifelse((trimws(as.character(dt1$Latitude.Start))==trimws("NULL")),NA,dt1$Latitude.Start)               
suppressWarnings(dt1$Latitude.Start <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt1$Latitude.Start))==as.character(as.numeric("NULL"))),NA,dt1$Latitude.Start))
dt1$Longitude.Start <- ifelse((trimws(as.character(dt1$Longitude.Start))==trimws("NULL")),NA,dt1$Longitude.Start)               
suppressWarnings(dt1$Longitude.Start <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt1$Longitude.Start))==as.character(as.numeric("NULL"))),NA,dt1$Longitude.Start))
dt1$Latitude.Mid <- ifelse((trimws(as.character(dt1$Latitude.Mid))==trimws("NULL")),NA,dt1$Latitude.Mid)               
suppressWarnings(dt1$Latitude.Mid <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt1$Latitude.Mid))==as.character(as.numeric("NULL"))),NA,dt1$Latitude.Mid))
dt1$Longitude.Mid <- ifelse((trimws(as.character(dt1$Longitude.Mid))==trimws("NULL")),NA,dt1$Longitude.Mid)               
suppressWarnings(dt1$Longitude.Mid <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt1$Longitude.Mid))==as.character(as.numeric("NULL"))),NA,dt1$Longitude.Mid))
dt1$Latitude.Stop <- ifelse((trimws(as.character(dt1$Latitude.Stop))==trimws("NULL")),NA,dt1$Latitude.Stop)               
suppressWarnings(dt1$Latitude.Stop <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt1$Latitude.Stop))==as.character(as.numeric("NULL"))),NA,dt1$Latitude.Stop))
dt1$Longitude.Stop <- ifelse((trimws(as.character(dt1$Longitude.Stop))==trimws("NULL")),NA,dt1$Longitude.Stop)               
suppressWarnings(dt1$Longitude.Stop <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt1$Longitude.Stop))==as.character(as.numeric("NULL"))),NA,dt1$Longitude.Stop))
dt1$Length <- ifelse((trimws(as.character(dt1$Length))==trimws("NULL")),NA,dt1$Length)               
suppressWarnings(dt1$Length <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt1$Length))==as.character(as.numeric("NULL"))),NA,dt1$Length))
dt1$Width <- ifelse((trimws(as.character(dt1$Width))==trimws("NULL")),NA,dt1$Width)               
suppressWarnings(dt1$Width <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt1$Width))==as.character(as.numeric("NULL"))),NA,dt1$Width))
dt1$Area <- ifelse((trimws(as.character(dt1$Area))==trimws("NULL")),NA,dt1$Area)               
suppressWarnings(dt1$Area <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt1$Area))==as.character(as.numeric("NULL"))),NA,dt1$Area))
dt1$Depth <- ifelse((trimws(as.character(dt1$Depth))==trimws("NULL")),NA,dt1$Depth)               
suppressWarnings(dt1$Depth <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt1$Depth))==as.character(as.numeric("NULL"))),NA,dt1$Depth))
dt1$Julian.date <- as.factor(ifelse((trimws(as.character(dt1$Julian.date))==trimws("NULL")),NA,as.character(dt1$Julian.date)))
dt1$Julian.day <- as.factor(ifelse((trimws(as.character(dt1$Julian.day))==trimws("NULL")),NA,as.character(dt1$Julian.day)))
dt1$SVY <- as.factor(ifelse((trimws(as.character(dt1$SVY))==trimws("NULL")),NA,as.character(dt1$SVY)))
dt1$Season <- as.factor(ifelse((trimws(as.character(dt1$Season))==trimws("NULL")),NA,as.character(dt1$Season)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(GIS.key)
summary(Cruise)
summary(Transect.number)
summary(Bin.number)
summary(Date)
summary(Time)
summary(Latitude.Start)
summary(Longitude.Start)
summary(Latitude.Mid)
summary(Longitude.Mid)
summary(Latitude.Stop)
summary(Longitude.Stop)
summary(Length)
summary(Width)
summary(Area)
summary(Depth)
summary(Julian.date)
summary(Julian.day)
summary(SVY)
summary(Season) 
# Get more details on character variables

summary(as.factor(dt1$GIS.key)) 
summary(as.factor(dt1$Cruise)) 
summary(as.factor(dt1$Transect.number)) 
summary(as.factor(dt1$Bin.number)) 
summary(as.factor(dt1$Julian.date)) 
summary(as.factor(dt1$Julian.day)) 
summary(as.factor(dt1$SVY)) 
summary(as.factor(dt1$Season))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cce/255/2/2dece374a80ca17644a796ebe765f7df" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "GIS.key",     
                 "Species",     
                 "Behavior",     
                 "Count"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$GIS.key)!="factor") dt2$GIS.key<- as.factor(dt2$GIS.key)
if (class(dt2$Species)!="factor") dt2$Species<- as.factor(dt2$Species)
if (class(dt2$Behavior)!="factor") dt2$Behavior<- as.factor(dt2$Behavior)
if (class(dt2$Count)=="factor") dt2$Count <-as.numeric(levels(dt2$Count))[as.integer(dt2$Count) ]               
if (class(dt2$Count)=="character") dt2$Count <-as.numeric(dt2$Count)

# Convert Missing Values to NA for non-dates

dt2$GIS.key <- as.factor(ifelse((trimws(as.character(dt2$GIS.key))==trimws("NULL")),NA,as.character(dt2$GIS.key)))
dt2$Species <- as.factor(ifelse((trimws(as.character(dt2$Species))==trimws("NULL")),NA,as.character(dt2$Species)))
dt2$Behavior <- as.factor(ifelse((trimws(as.character(dt2$Behavior))==trimws("NULL")),NA,as.character(dt2$Behavior)))
dt2$Count <- ifelse((trimws(as.character(dt2$Count))==trimws("NULL")),NA,dt2$Count)               
suppressWarnings(dt2$Count <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt2$Count))==as.character(as.numeric("NULL"))),NA,dt2$Count))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(GIS.key)
summary(Species)
summary(Behavior)
summary(Count) 
# Get more details on character variables

summary(as.factor(dt2$GIS.key)) 
summary(as.factor(dt2$Species)) 
summary(as.factor(dt2$Behavior))
detach(dt2)               


inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cce/255/2/4e9e15827ba5dbc2949368182b4f9e49" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "GIS.key",     
                 "Cruise",     
                 "Transect.number",     
                 "Bin.number",     
                 "Date",     
                 "Time",     
                 "Latitude.Start",     
                 "Longitude.Start",     
                 "Latitude.Mid",     
                 "Longitude.Mid",     
                 "Latitude.Stop",     
                 "Longitude.Stop",     
                 "Length",     
                 "Width",     
                 "Area",     
                 "Depth",     
                 "Julian.date",     
                 "Julian.day",     
                 "SVY",     
                 "Season"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$GIS.key)!="factor") dt3$GIS.key<- as.factor(dt3$GIS.key)
if (class(dt3$Cruise)!="factor") dt3$Cruise<- as.factor(dt3$Cruise)
if (class(dt3$Transect.number)!="factor") dt3$Transect.number<- as.factor(dt3$Transect.number)
if (class(dt3$Bin.number)!="factor") dt3$Bin.number<- as.factor(dt3$Bin.number)                                   
# attempting to convert dt3$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp3Date<-as.Date(dt3$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp3Date) == length(tmp3Date[!is.na(tmp3Date)])){dt3$Date <- tmp3Date } else {print("Date conversion failed for dt3$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp3Date) 
if (class(dt3$Time)=="factor") dt3$Time <-as.numeric(levels(dt3$Time))[as.integer(dt3$Time) ]               
if (class(dt3$Time)=="character") dt3$Time <-as.numeric(dt3$Time)
if (class(dt3$Latitude.Start)=="factor") dt3$Latitude.Start <-as.numeric(levels(dt3$Latitude.Start))[as.integer(dt3$Latitude.Start) ]               
if (class(dt3$Latitude.Start)=="character") dt3$Latitude.Start <-as.numeric(dt3$Latitude.Start)
if (class(dt3$Longitude.Start)=="factor") dt3$Longitude.Start <-as.numeric(levels(dt3$Longitude.Start))[as.integer(dt3$Longitude.Start) ]               
if (class(dt3$Longitude.Start)=="character") dt3$Longitude.Start <-as.numeric(dt3$Longitude.Start)
if (class(dt3$Latitude.Mid)=="factor") dt3$Latitude.Mid <-as.numeric(levels(dt3$Latitude.Mid))[as.integer(dt3$Latitude.Mid) ]               
if (class(dt3$Latitude.Mid)=="character") dt3$Latitude.Mid <-as.numeric(dt3$Latitude.Mid)
if (class(dt3$Longitude.Mid)=="factor") dt3$Longitude.Mid <-as.numeric(levels(dt3$Longitude.Mid))[as.integer(dt3$Longitude.Mid) ]               
if (class(dt3$Longitude.Mid)=="character") dt3$Longitude.Mid <-as.numeric(dt3$Longitude.Mid)
if (class(dt3$Latitude.Stop)=="factor") dt3$Latitude.Stop <-as.numeric(levels(dt3$Latitude.Stop))[as.integer(dt3$Latitude.Stop) ]               
if (class(dt3$Latitude.Stop)=="character") dt3$Latitude.Stop <-as.numeric(dt3$Latitude.Stop)
if (class(dt3$Longitude.Stop)=="factor") dt3$Longitude.Stop <-as.numeric(levels(dt3$Longitude.Stop))[as.integer(dt3$Longitude.Stop) ]               
if (class(dt3$Longitude.Stop)=="character") dt3$Longitude.Stop <-as.numeric(dt3$Longitude.Stop)
if (class(dt3$Length)=="factor") dt3$Length <-as.numeric(levels(dt3$Length))[as.integer(dt3$Length) ]               
if (class(dt3$Length)=="character") dt3$Length <-as.numeric(dt3$Length)
if (class(dt3$Width)=="factor") dt3$Width <-as.numeric(levels(dt3$Width))[as.integer(dt3$Width) ]               
if (class(dt3$Width)=="character") dt3$Width <-as.numeric(dt3$Width)
if (class(dt3$Area)=="factor") dt3$Area <-as.numeric(levels(dt3$Area))[as.integer(dt3$Area) ]               
if (class(dt3$Area)=="character") dt3$Area <-as.numeric(dt3$Area)
if (class(dt3$Depth)=="factor") dt3$Depth <-as.numeric(levels(dt3$Depth))[as.integer(dt3$Depth) ]               
if (class(dt3$Depth)=="character") dt3$Depth <-as.numeric(dt3$Depth)
if (class(dt3$Julian.date)!="factor") dt3$Julian.date<- as.factor(dt3$Julian.date)
if (class(dt3$Julian.day)!="factor") dt3$Julian.day<- as.factor(dt3$Julian.day)
if (class(dt3$SVY)!="factor") dt3$SVY<- as.factor(dt3$SVY)
if (class(dt3$Season)!="factor") dt3$Season<- as.factor(dt3$Season)

# Convert Missing Values to NA for non-dates

dt3$GIS.key <- as.factor(ifelse((trimws(as.character(dt3$GIS.key))==trimws("NULL")),NA,as.character(dt3$GIS.key)))
dt3$Cruise <- as.factor(ifelse((trimws(as.character(dt3$Cruise))==trimws("NULL")),NA,as.character(dt3$Cruise)))
dt3$Transect.number <- as.factor(ifelse((trimws(as.character(dt3$Transect.number))==trimws("NULL")),NA,as.character(dt3$Transect.number)))
dt3$Bin.number <- as.factor(ifelse((trimws(as.character(dt3$Bin.number))==trimws("NULL")),NA,as.character(dt3$Bin.number)))
dt3$Time <- ifelse((trimws(as.character(dt3$Time))==trimws("NULL")),NA,dt3$Time)               
suppressWarnings(dt3$Time <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt3$Time))==as.character(as.numeric("NULL"))),NA,dt3$Time))
dt3$Latitude.Start <- ifelse((trimws(as.character(dt3$Latitude.Start))==trimws("NULL")),NA,dt3$Latitude.Start)               
suppressWarnings(dt3$Latitude.Start <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt3$Latitude.Start))==as.character(as.numeric("NULL"))),NA,dt3$Latitude.Start))
dt3$Longitude.Start <- ifelse((trimws(as.character(dt3$Longitude.Start))==trimws("NULL")),NA,dt3$Longitude.Start)               
suppressWarnings(dt3$Longitude.Start <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt3$Longitude.Start))==as.character(as.numeric("NULL"))),NA,dt3$Longitude.Start))
dt3$Latitude.Mid <- ifelse((trimws(as.character(dt3$Latitude.Mid))==trimws("NULL")),NA,dt3$Latitude.Mid)               
suppressWarnings(dt3$Latitude.Mid <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt3$Latitude.Mid))==as.character(as.numeric("NULL"))),NA,dt3$Latitude.Mid))
dt3$Longitude.Mid <- ifelse((trimws(as.character(dt3$Longitude.Mid))==trimws("NULL")),NA,dt3$Longitude.Mid)               
suppressWarnings(dt3$Longitude.Mid <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt3$Longitude.Mid))==as.character(as.numeric("NULL"))),NA,dt3$Longitude.Mid))
dt3$Latitude.Stop <- ifelse((trimws(as.character(dt3$Latitude.Stop))==trimws("NULL")),NA,dt3$Latitude.Stop)               
suppressWarnings(dt3$Latitude.Stop <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt3$Latitude.Stop))==as.character(as.numeric("NULL"))),NA,dt3$Latitude.Stop))
dt3$Longitude.Stop <- ifelse((trimws(as.character(dt3$Longitude.Stop))==trimws("NULL")),NA,dt3$Longitude.Stop)               
suppressWarnings(dt3$Longitude.Stop <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt3$Longitude.Stop))==as.character(as.numeric("NULL"))),NA,dt3$Longitude.Stop))
dt3$Length <- ifelse((trimws(as.character(dt3$Length))==trimws("NULL")),NA,dt3$Length)               
suppressWarnings(dt3$Length <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt3$Length))==as.character(as.numeric("NULL"))),NA,dt3$Length))
dt3$Width <- ifelse((trimws(as.character(dt3$Width))==trimws("NULL")),NA,dt3$Width)               
suppressWarnings(dt3$Width <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt3$Width))==as.character(as.numeric("NULL"))),NA,dt3$Width))
dt3$Area <- ifelse((trimws(as.character(dt3$Area))==trimws("NULL")),NA,dt3$Area)               
suppressWarnings(dt3$Area <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt3$Area))==as.character(as.numeric("NULL"))),NA,dt3$Area))
dt3$Depth <- ifelse((trimws(as.character(dt3$Depth))==trimws("NULL")),NA,dt3$Depth)               
suppressWarnings(dt3$Depth <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt3$Depth))==as.character(as.numeric("NULL"))),NA,dt3$Depth))
dt3$Julian.date <- as.factor(ifelse((trimws(as.character(dt3$Julian.date))==trimws("NULL")),NA,as.character(dt3$Julian.date)))
dt3$Julian.day <- as.factor(ifelse((trimws(as.character(dt3$Julian.day))==trimws("NULL")),NA,as.character(dt3$Julian.day)))
dt3$SVY <- as.factor(ifelse((trimws(as.character(dt3$SVY))==trimws("NULL")),NA,as.character(dt3$SVY)))
dt3$Season <- as.factor(ifelse((trimws(as.character(dt3$Season))==trimws("NULL")),NA,as.character(dt3$Season)))


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(GIS.key)
summary(Cruise)
summary(Transect.number)
summary(Bin.number)
summary(Date)
summary(Time)
summary(Latitude.Start)
summary(Longitude.Start)
summary(Latitude.Mid)
summary(Longitude.Mid)
summary(Latitude.Stop)
summary(Longitude.Stop)
summary(Length)
summary(Width)
summary(Area)
summary(Depth)
summary(Julian.date)
summary(Julian.day)
summary(SVY)
summary(Season) 
# Get more details on character variables

summary(as.factor(dt3$GIS.key)) 
summary(as.factor(dt3$Cruise)) 
summary(as.factor(dt3$Transect.number)) 
summary(as.factor(dt3$Bin.number)) 
summary(as.factor(dt3$Julian.date)) 
summary(as.factor(dt3$Julian.day)) 
summary(as.factor(dt3$SVY)) 
summary(as.factor(dt3$Season))
detach(dt3)               


inUrl4  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cce/255/2/a442ebd09808906e0d514533150aa510" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


dt4 <-read.csv(infile4,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "GIS.key",     
                 "Species",     
                 "Behavior",     
                 "Count"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$GIS.key)!="factor") dt4$GIS.key<- as.factor(dt4$GIS.key)
if (class(dt4$Species)!="factor") dt4$Species<- as.factor(dt4$Species)
if (class(dt4$Behavior)!="factor") dt4$Behavior<- as.factor(dt4$Behavior)
if (class(dt4$Count)=="factor") dt4$Count <-as.numeric(levels(dt4$Count))[as.integer(dt4$Count) ]               
if (class(dt4$Count)=="character") dt4$Count <-as.numeric(dt4$Count)

# Convert Missing Values to NA for non-dates

dt4$GIS.key <- as.factor(ifelse((trimws(as.character(dt4$GIS.key))==trimws("NULL")),NA,as.character(dt4$GIS.key)))
dt4$Species <- as.factor(ifelse((trimws(as.character(dt4$Species))==trimws("NULL")),NA,as.character(dt4$Species)))
dt4$Behavior <- as.factor(ifelse((trimws(as.character(dt4$Behavior))==trimws("NULL")),NA,as.character(dt4$Behavior)))
dt4$Count <- ifelse((trimws(as.character(dt4$Count))==trimws("NULL")),NA,dt4$Count)               
suppressWarnings(dt4$Count <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt4$Count))==as.character(as.numeric("NULL"))),NA,dt4$Count))


# Here is the structure of the input data frame:
str(dt4)                            
attach(dt4)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(GIS.key)
summary(Species)
summary(Behavior)
summary(Count) 
# Get more details on character variables

summary(as.factor(dt4$GIS.key)) 
summary(as.factor(dt4$Species)) 
summary(as.factor(dt4$Behavior))
detach(dt4)               


inUrl5  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cce/255/2/03d8a486a739b787fc9592864578c4c3" 
infile5 <- tempfile()
try(download.file(inUrl5,infile5,method="curl"))
if (is.na(file.size(infile5))) download.file(inUrl5,infile5,method="auto")


dt5 <-read.csv(infile5,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "GIS.key",     
                 "Cruise",     
                 "Transect.number",     
                 "Bin.number",     
                 "Date",     
                 "Time",     
                 "Latitude.Start",     
                 "Longitude.Start",     
                 "Latitude.Mid",     
                 "Longitude.Mid",     
                 "Latitude.Stop",     
                 "Longitude.Stop",     
                 "Length",     
                 "Width",     
                 "Area",     
                 "Depth",     
                 "Julian.date",     
                 "Julian.day",     
                 "SVY",     
                 "Season"    ), check.names=TRUE)

unlink(infile5)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt5$GIS.key)!="factor") dt5$GIS.key<- as.factor(dt5$GIS.key)
if (class(dt5$Cruise)!="factor") dt5$Cruise<- as.factor(dt5$Cruise)
if (class(dt5$Transect.number)!="factor") dt5$Transect.number<- as.factor(dt5$Transect.number)
if (class(dt5$Bin.number)!="factor") dt5$Bin.number<- as.factor(dt5$Bin.number)                                   
# attempting to convert dt5$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp5Date<-as.Date(dt5$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp5Date) == length(tmp5Date[!is.na(tmp5Date)])){dt5$Date <- tmp5Date } else {print("Date conversion failed for dt5$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp5Date) 
if (class(dt5$Time)=="factor") dt5$Time <-as.numeric(levels(dt5$Time))[as.integer(dt5$Time) ]               
if (class(dt5$Time)=="character") dt5$Time <-as.numeric(dt5$Time)
if (class(dt5$Latitude.Start)=="factor") dt5$Latitude.Start <-as.numeric(levels(dt5$Latitude.Start))[as.integer(dt5$Latitude.Start) ]               
if (class(dt5$Latitude.Start)=="character") dt5$Latitude.Start <-as.numeric(dt5$Latitude.Start)
if (class(dt5$Longitude.Start)=="factor") dt5$Longitude.Start <-as.numeric(levels(dt5$Longitude.Start))[as.integer(dt5$Longitude.Start) ]               
if (class(dt5$Longitude.Start)=="character") dt5$Longitude.Start <-as.numeric(dt5$Longitude.Start)
if (class(dt5$Latitude.Mid)=="factor") dt5$Latitude.Mid <-as.numeric(levels(dt5$Latitude.Mid))[as.integer(dt5$Latitude.Mid) ]               
if (class(dt5$Latitude.Mid)=="character") dt5$Latitude.Mid <-as.numeric(dt5$Latitude.Mid)
if (class(dt5$Longitude.Mid)=="factor") dt5$Longitude.Mid <-as.numeric(levels(dt5$Longitude.Mid))[as.integer(dt5$Longitude.Mid) ]               
if (class(dt5$Longitude.Mid)=="character") dt5$Longitude.Mid <-as.numeric(dt5$Longitude.Mid)
if (class(dt5$Latitude.Stop)=="factor") dt5$Latitude.Stop <-as.numeric(levels(dt5$Latitude.Stop))[as.integer(dt5$Latitude.Stop) ]               
if (class(dt5$Latitude.Stop)=="character") dt5$Latitude.Stop <-as.numeric(dt5$Latitude.Stop)
if (class(dt5$Longitude.Stop)=="factor") dt5$Longitude.Stop <-as.numeric(levels(dt5$Longitude.Stop))[as.integer(dt5$Longitude.Stop) ]               
if (class(dt5$Longitude.Stop)=="character") dt5$Longitude.Stop <-as.numeric(dt5$Longitude.Stop)
if (class(dt5$Length)=="factor") dt5$Length <-as.numeric(levels(dt5$Length))[as.integer(dt5$Length) ]               
if (class(dt5$Length)=="character") dt5$Length <-as.numeric(dt5$Length)
if (class(dt5$Width)=="factor") dt5$Width <-as.numeric(levels(dt5$Width))[as.integer(dt5$Width) ]               
if (class(dt5$Width)=="character") dt5$Width <-as.numeric(dt5$Width)
if (class(dt5$Area)=="factor") dt5$Area <-as.numeric(levels(dt5$Area))[as.integer(dt5$Area) ]               
if (class(dt5$Area)=="character") dt5$Area <-as.numeric(dt5$Area)
if (class(dt5$Depth)=="factor") dt5$Depth <-as.numeric(levels(dt5$Depth))[as.integer(dt5$Depth) ]               
if (class(dt5$Depth)=="character") dt5$Depth <-as.numeric(dt5$Depth)
if (class(dt5$Julian.date)!="factor") dt5$Julian.date<- as.factor(dt5$Julian.date)
if (class(dt5$Julian.day)!="factor") dt5$Julian.day<- as.factor(dt5$Julian.day)
if (class(dt5$SVY)!="factor") dt5$SVY<- as.factor(dt5$SVY)
if (class(dt5$Season)!="factor") dt5$Season<- as.factor(dt5$Season)

# Convert Missing Values to NA for non-dates

dt5$GIS.key <- as.factor(ifelse((trimws(as.character(dt5$GIS.key))==trimws("NULL")),NA,as.character(dt5$GIS.key)))
dt5$Cruise <- as.factor(ifelse((trimws(as.character(dt5$Cruise))==trimws("NULL")),NA,as.character(dt5$Cruise)))
dt5$Transect.number <- as.factor(ifelse((trimws(as.character(dt5$Transect.number))==trimws("NULL")),NA,as.character(dt5$Transect.number)))
dt5$Bin.number <- as.factor(ifelse((trimws(as.character(dt5$Bin.number))==trimws("NULL")),NA,as.character(dt5$Bin.number)))
dt5$Time <- ifelse((trimws(as.character(dt5$Time))==trimws("NULL")),NA,dt5$Time)               
suppressWarnings(dt5$Time <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt5$Time))==as.character(as.numeric("NULL"))),NA,dt5$Time))
dt5$Latitude.Start <- ifelse((trimws(as.character(dt5$Latitude.Start))==trimws("NULL")),NA,dt5$Latitude.Start)               
suppressWarnings(dt5$Latitude.Start <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt5$Latitude.Start))==as.character(as.numeric("NULL"))),NA,dt5$Latitude.Start))
dt5$Longitude.Start <- ifelse((trimws(as.character(dt5$Longitude.Start))==trimws("NULL")),NA,dt5$Longitude.Start)               
suppressWarnings(dt5$Longitude.Start <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt5$Longitude.Start))==as.character(as.numeric("NULL"))),NA,dt5$Longitude.Start))
dt5$Latitude.Mid <- ifelse((trimws(as.character(dt5$Latitude.Mid))==trimws("NULL")),NA,dt5$Latitude.Mid)               
suppressWarnings(dt5$Latitude.Mid <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt5$Latitude.Mid))==as.character(as.numeric("NULL"))),NA,dt5$Latitude.Mid))
dt5$Longitude.Mid <- ifelse((trimws(as.character(dt5$Longitude.Mid))==trimws("NULL")),NA,dt5$Longitude.Mid)               
suppressWarnings(dt5$Longitude.Mid <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt5$Longitude.Mid))==as.character(as.numeric("NULL"))),NA,dt5$Longitude.Mid))
dt5$Latitude.Stop <- ifelse((trimws(as.character(dt5$Latitude.Stop))==trimws("NULL")),NA,dt5$Latitude.Stop)               
suppressWarnings(dt5$Latitude.Stop <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt5$Latitude.Stop))==as.character(as.numeric("NULL"))),NA,dt5$Latitude.Stop))
dt5$Longitude.Stop <- ifelse((trimws(as.character(dt5$Longitude.Stop))==trimws("NULL")),NA,dt5$Longitude.Stop)               
suppressWarnings(dt5$Longitude.Stop <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt5$Longitude.Stop))==as.character(as.numeric("NULL"))),NA,dt5$Longitude.Stop))
dt5$Length <- ifelse((trimws(as.character(dt5$Length))==trimws("NULL")),NA,dt5$Length)               
suppressWarnings(dt5$Length <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt5$Length))==as.character(as.numeric("NULL"))),NA,dt5$Length))
dt5$Width <- ifelse((trimws(as.character(dt5$Width))==trimws("NULL")),NA,dt5$Width)               
suppressWarnings(dt5$Width <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt5$Width))==as.character(as.numeric("NULL"))),NA,dt5$Width))
dt5$Area <- ifelse((trimws(as.character(dt5$Area))==trimws("NULL")),NA,dt5$Area)               
suppressWarnings(dt5$Area <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt5$Area))==as.character(as.numeric("NULL"))),NA,dt5$Area))
dt5$Depth <- ifelse((trimws(as.character(dt5$Depth))==trimws("NULL")),NA,dt5$Depth)               
suppressWarnings(dt5$Depth <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt5$Depth))==as.character(as.numeric("NULL"))),NA,dt5$Depth))
dt5$Julian.date <- as.factor(ifelse((trimws(as.character(dt5$Julian.date))==trimws("NULL")),NA,as.character(dt5$Julian.date)))
dt5$Julian.day <- as.factor(ifelse((trimws(as.character(dt5$Julian.day))==trimws("NULL")),NA,as.character(dt5$Julian.day)))
dt5$SVY <- as.factor(ifelse((trimws(as.character(dt5$SVY))==trimws("NULL")),NA,as.character(dt5$SVY)))
dt5$Season <- as.factor(ifelse((trimws(as.character(dt5$Season))==trimws("NULL")),NA,as.character(dt5$Season)))


# Here is the structure of the input data frame:
str(dt5)                            
attach(dt5)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(GIS.key)
summary(Cruise)
summary(Transect.number)
summary(Bin.number)
summary(Date)
summary(Time)
summary(Latitude.Start)
summary(Longitude.Start)
summary(Latitude.Mid)
summary(Longitude.Mid)
summary(Latitude.Stop)
summary(Longitude.Stop)
summary(Length)
summary(Width)
summary(Area)
summary(Depth)
summary(Julian.date)
summary(Julian.day)
summary(SVY)
summary(Season) 
# Get more details on character variables

summary(as.factor(dt5$GIS.key)) 
summary(as.factor(dt5$Cruise)) 
summary(as.factor(dt5$Transect.number)) 
summary(as.factor(dt5$Bin.number)) 
summary(as.factor(dt5$Julian.date)) 
summary(as.factor(dt5$Julian.day)) 
summary(as.factor(dt5$SVY)) 
summary(as.factor(dt5$Season))
detach(dt5)               


inUrl6  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cce/255/2/e594afc5881df5dd3b02cd41e2a89c33" 
infile6 <- tempfile()
try(download.file(inUrl6,infile6,method="curl"))
if (is.na(file.size(infile6))) download.file(inUrl6,infile6,method="auto")


dt6 <-read.csv(infile6,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "GIS.key",     
                 "Species",     
                 "Behavior",     
                 "Count"    ), check.names=TRUE)

unlink(infile6)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt6$GIS.key)!="factor") dt6$GIS.key<- as.factor(dt6$GIS.key)
if (class(dt6$Species)!="factor") dt6$Species<- as.factor(dt6$Species)
if (class(dt6$Behavior)!="factor") dt6$Behavior<- as.factor(dt6$Behavior)
if (class(dt6$Count)=="factor") dt6$Count <-as.numeric(levels(dt6$Count))[as.integer(dt6$Count) ]               
if (class(dt6$Count)=="character") dt6$Count <-as.numeric(dt6$Count)

# Convert Missing Values to NA for non-dates

dt6$GIS.key <- as.factor(ifelse((trimws(as.character(dt6$GIS.key))==trimws("NULL")),NA,as.character(dt6$GIS.key)))
dt6$Species <- as.factor(ifelse((trimws(as.character(dt6$Species))==trimws("NULL")),NA,as.character(dt6$Species)))
dt6$Behavior <- as.factor(ifelse((trimws(as.character(dt6$Behavior))==trimws("NULL")),NA,as.character(dt6$Behavior)))
dt6$Count <- ifelse((trimws(as.character(dt6$Count))==trimws("NULL")),NA,dt6$Count)               
suppressWarnings(dt6$Count <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt6$Count))==as.character(as.numeric("NULL"))),NA,dt6$Count))


# Here is the structure of the input data frame:
str(dt6)                            
attach(dt6)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(GIS.key)
summary(Species)
summary(Behavior)
summary(Count) 
# Get more details on character variables

summary(as.factor(dt6$GIS.key)) 
summary(as.factor(dt6$Species)) 
summary(as.factor(dt6$Behavior))
detach(dt6)       