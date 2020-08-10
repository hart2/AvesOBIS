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



