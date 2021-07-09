# subsample each region at 20% seeing how many species were observed, 1000 iterations, giving 
# probability/percent of seeing species in that region (percent on average that you would see a species of bird)
# randomly iterating
# giving total of species found over all iterations as a probability of it being observed in that region

# Gulf of California ------------------------------------------------------
GulfofCalifornia <- read_csv("~/github/AvesOBIS/GulfofCalifornia.csv") #read in csv

total_samp <- nrow(GulfofCalifornia) #find total number of samples for subregion

sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
# create matrix with same number of rows as iterations and the same number of columns 
# as total species found in whole dataset
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves_NA$scientificName)))  
colnames(dat1) <- unique(Aves_NA$scientificName) #Give columns names that coincide with the scientific names in whole dataset

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(GulfofCalifornia[idx,1])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probGulfofCalifornia.csv")
# for each species, gives how often it is seen in each iteration 
# (seen more often or less often over this area)
# Done as 1 or 0, not a cumulative sum of the times the species observed
# Can show differences over different time intervals

# graphn <- apply(df, 2, cumsum)
# for (k in 1:ncol(graphn)){
# plot(graphn[,k])
# }

# California Current ------------------------------------------------------
CaliforniaCurrent <- read_csv("~/github/AvesOBIS/CaliforniaCurrent.csv")

total_samp <- nrow(CaliforniaCurrent)

sub20 <- round(0.2*total_samp)
iters <- 1000
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves_NA$scientificName)))
colnames(dat1) <- unique(Aves_NA$scientificName)

for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(CaliforniaCurrent[idx,1])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x2 <- apply(df, 2, sum)/iters*100
write.csv(x2, "./probCaliforniaCurrent.csv")

# for each species, gives how often it is seen in each iteration 
# (seen more often or less often over this area)
# Done as 1 or 0, not a cumulative sum of the times the species observed
# Can show differences over different time intervals
# graphn <- apply(df, 2, cumsum)
# for (k in 1:ncol(graphn)){
# plot(graphn[,k])
# }

# Gulf of Alaska ----------------------------------------------------------
GulfofAlaska <- read_csv("~/github/AvesOBIS/GulfofAlaska.csv")

total_samp <- nrow(GulfofAlaska)

sub20 <- round(0.2*total_samp)
iters <- 1000
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves_NA$scientificName)))
colnames(dat1) <- unique(Aves_NA$scientificName)

for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(GulfofAlaska[idx,1])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x3 <- apply(df, 2, sum)/iters*100
write.csv(x3, "./probGulfofAlaska.csv")

# for each species, gives how often it is seen in each iteration 
# (seen more often or less often over this area)
# Done as 1 or 0, not a cumulative sum of the times the species observed
# Can show differences over different time intervals
# graphn <- apply(df, 2, cumsum)
# for (k in 1:ncol(graphn)){
# plot(graphn[,k])
# }



# NE US Shelf -------------------------------------------------------------
NEShelf <- read_csv("~/github/AvesOBIS/NEShelf.csv")

total_samp <- nrow(NEShelf)

sub20 <- round(0.2*total_samp)
iters <- 1000
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves_NA$scientificName)))
colnames(dat1) <- unique(Aves_NA$scientificName)

for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(NEShelf[idx,1])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x4 <- apply(df, 2, sum)/iters*100
write.csv(x4, "./probNEShelf.csv")

# for each species, gives how often it is seen in each iteration 
# (seen more often or less often over this area)
# Done as 1 or 0, not a cumulative sum of the times the species observed
# Can show differences over different time intervals
# graphn <- apply(df, 2, cumsum)
# for (k in 1:ncol(graphn)){
# plot(graphn[,k])
# }

# Pacific Central America -------------------------------------------------
PacificCentral <- read_csv("~/github/AvesOBIS/PacificCentralAmerica.csv")

total_samp <- nrow(PacificCentral)

sub20 <- round(0.2*total_samp)
iters <- 1000
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves_NA$scientificName)))
colnames(dat1) <- unique(Aves_NA$scientificName)

for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(PacificCentral[idx,1])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x5 <- apply(df, 2, sum)/iters*100
write.csv(x5, "./probPacificCentralAmerica.csv")

# for each species, gives how often it is seen in each iteration 
# (seen more often or less often over this area)
# Done as 1 or 0, not a cumulative sum of the times the species observed
# Can show differences over different time intervals
# graphn <- apply(df, 2, cumsum)
# for (k in 1:ncol(graphn)){
# plot(graphn[,k])
# }

# SE US Shelf -------------------------------------------------------------
SEShelf <- read_csv("~/github/AvesOBIS/SEShelf.csv")
total_samp <- nrow(SEShelf)

sub20 <- round(0.2*total_samp)
iters <- 1000
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves_NA$scientificName)))
colnames(dat1) <- unique(Aves_NA$scientificName)

for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(SEShelf[idx,1])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x6 <- apply(df, 2, sum)/iters*100
write.csv(x6, "./probSEShelf.csv")

# for each species, gives how often it is seen in each iteration 
# (seen more often or less often over this area)
# Done as 1 or 0, not a cumulative sum of the times the species observed
# Can show differences over different time intervals
# graphn <- apply(df, 2, cumsum)
# for (k in 1:ncol(graphn)){
# plot(graphn[,k])
# }

# Carribean ---------------------------------------------------------------
Carribean <- read_csv("~/github/AvesOBIS/Carribean.csv")
total_samp <- nrow(Carribean)

sub20 <- round(0.2*total_samp)
iters <- 1000
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves_NA$scientificName)))
colnames(dat1) <- unique(Aves_NA$scientificName)

for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(Carribean[idx,1])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x7 <- apply(df, 2, sum)/iters*100
write.csv(x7, "./probCarribean.csv")

# for each species, gives how often it is seen in each iteration 
# (seen more often or less often over this area)
# Done as 1 or 0, not a cumulative sum of the times the species observed
# Can show differences over different time intervals
# graphn <- apply(df, 2, cumsum)
# for (k in 1:ncol(graphn)){
# plot(graphn[,k])
# }


# Gulf of Mexico ----------------------------------------------------------
GulfofMexico <- read_csv("~/github/AvesOBIS/GulfofMexico.csv")
total_samp <- nrow(GulfofMexico)

sub20 <- round(0.2*total_samp)
iters <- 1000
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves_NA$scientificName)))
colnames(dat1) <- unique(Aves_NA$scientificName)

for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(GulfofMexico[idx,1])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x8 <- apply(df, 2, sum)/iters*100
write.csv(x8, "./probGulfofMexico.csv")

# for each species, gives how often it is seen in each iteration 
# (seen more often or less often over this area)
# Done as 1 or 0, not a cumulative sum of the times the species observed
# Can show differences over different time intervals
# graphn <- apply(df, 2, cumsum)
# for (k in 1:ncol(graphn)){
# plot(graphn[,k])
# }

