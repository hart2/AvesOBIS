library(tidyverse)

x1 <- read_csv('seabird_data_archive_2.csv')
x2 <- read_csv('usgs_eaec7873-a1d8-43cf-baa2-27b2772a8d1f.csv')

# need a for loop and an if statement??? OR should I merge the dataframes and see 
# if there are any matches between the multiple columns

# Do I need variables for each data$column?

# Conditions for if statement
if(x1$observation_longitude == x2$decimalLongitude && 
   x1$observation_latitude == x2$decimalLatitude && 
   x1$year == x2$date_year &&
   x1$scientific_nm == x2$scientificName)
# what to execute
    { }


# Notes on if statements --------------------------------------------------

# randomly samples years from dataset x1, gives you 50 samples
y <- factor(sample(x1$year,50,replace = TRUE)) 
for (i in unique(y)) print(i) # selects the uniquely year and prints them in the console

# randomly samples scientific names from x2, gives you 10 samples
y <- factor(sample(x2$scientificName,10,replace=TRUE))
# selects the unique scientific names and prints them in the console
for(i in unique(y)) print(i)

y <- x1$year
  ifelse(y <= 1983,y,NA)      # only prints in console

y <- x2$scientificName 
ifelse(y == 'Alle alle',y,NA) # only prints in console
