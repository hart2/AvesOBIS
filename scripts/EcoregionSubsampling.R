# subsample each region at 20% seeing how many species were observed, THEN subsample 
# with 1000 iterations, giving probability/percent of seeing species in that 
# region (percent on average that you would see a species of bird)randomly iterating
# giving total of species found over all iterations as a probability of it being
# observed in that region

library(tidyverse) # packages for data visualization
library(dplyr)
library(readr)
library(devtools)

# read in data csv
Bahamas <- read_csv("~/Ecoregions/Bahamas.csv")
Carolinian <- read_csv("~/Ecoregions/Carolinian.csv") 
ChiapasNicaragua <- read_csv("~/Ecoregions/ChiapasNicaragua.csv")
Cortezian <- read_csv("~/Ecoregions/Cortezian.csv")
Floridian <- read_csv("~/Ecoregions/Floridian.csv")
GreaterAntilles <- read_csv("~/Ecoregions/GreaterAntilles.csv")
GulfofMaineBayFundy <- read_csv("~/Ecoregions/GulfofMaineBayFundy.csv")
MagdalenaTransition <- read_csv("~/Ecoregions/MagdalenaTransition.csv")
MexicanTropicPacific <- read_csv("~/Ecoregions/MexicanTropicPacific.csv")
Nicoya <- read_csv("~/Ecoregions/Nicoya.csv")
NorthernCalifornia <- read_csv("~/Ecoregions/NorthernCalifornia.csv")
NorthGoM <- read_csv("~/Ecoregions/NorthGoM.csv")
OregonWashingtonVancouverCoast <- read_csv("~/Ecoregions/OregonWashingtonVancouverCoast.csv")
PanamaBight <- read_csv("~/Ecoregions/PanamaBight.csv")
PugetTroughGeorgiaBasin <- read_csv("~/Ecoregions/PugetTroughGeorgiaBasin.csv")
SouthGoM <- read_csv("~/Ecoregions/SouthGoM.csv")
SouthernCaliforniaBight <- read_csv("~/Ecoregions/SouthernCaliforniaBight.csv")
SouthwestCaribbean <- read_csv("~/Ecoregions/SouthwestCaribbean.csv")
Virginian <- read_csv("~/Ecoregions/Virginian.csv")

# Subsampling each Ecoregion ----------------------------------------------
#Bahamas
total_samp <- nrow(Bahamas)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #number of values needed as a subsample 20% of the total sample
Bahamas20 <- Bahamas[sample(nrow(Bahamas), sub20), ] #subsample
write.csv(Bahamas20,"./Bahamas20.csv")

#Carolinian
total_samp <- nrow(Carolinian)          
sub20 <- round(0.2*total_samp)       
Carolinian20 <- Carolinian[sample(nrow(Carolinian), sub20), ]
write.csv(Carolinian20,"./Carolinian20.csv")

#ChiapasNicaragua
total_samp <- nrow(ChiapasNicaragua)          
sub20 <- round(0.2*total_samp)       
ChiapasNicaragua20 <- ChiapasNicaragua[sample(nrow(ChiapasNicaragua), sub20), ]
write.csv(ChiapasNicaragua20,"./ChiapasNicaragua20.csv")

#Cortezian
total_samp <- nrow(Cortezian)          
sub20 <- round(0.2*total_samp)       
Cortezian20 <- Cortezian[sample(nrow(Cortezian), sub20), ]
write.csv(Cortezian20,"./Cortezian20.csv")

#Floridian
total_samp <- nrow(Floridian)          
sub20 <- round(0.2*total_samp)       
Floridian20 <- Floridian[sample(nrow(Floridian), sub20), ]
write.csv(Floridian20,"./Floridian20.csv")

#Greater Antilles
total_samp <- nrow(GreaterAntilles)          
sub20 <- round(0.2*total_samp)       
GreaterAntilles20 <- GreaterAntilles[sample(nrow(GreaterAntilles), sub20), ]
write.csv(GreaterAntilles20,"./GreaterAntilles20.csv")

#Gulf of Maine/ Bay of Fundy
total_samp <- nrow(GulfofMaineBayFundy)          
sub20 <- round(0.2*total_samp)       
GulfofMaineBayFundy20 <- GulfofMaineBayFundy[sample(nrow(GulfofMaineBayFundy), sub20), ]
write.csv(GulfofMaineBayFundy20,"./GulfofMaineBayFundy20.csv")

#Magdalena Transition
total_samp <- nrow(MagdalenaTransition)          
sub20 <- round(0.2*total_samp)       
MagdalenaTransition20 <- MagdalenaTransition[sample(nrow(MagdalenaTransition), sub20), ]
write.csv(MagdalenaTransition20,"./MagdalenaTransition20.csv")

#Mexican Tropical Pacific
total_samp <- nrow(MexicanTropicPacific)          
sub20 <- round(0.2*total_samp)       
MexicanTropicPacific20 <- MexicanTropicPacific[sample(nrow(MexicanTropicPacific), sub20), ]
write.csv(MexicanTropicPacific20,"./MexicanTropicPacific20.csv")

#Nicoya
total_samp <- nrow(Nicoya)          
sub20 <- round(0.2*total_samp)       
Nicoya20 <- Nicoya[sample(nrow(Nicoya), sub20), ]
write.csv(Nicoya20,"./Nicoya20.csv")

#Northern California
total_samp <- nrow(NorthernCalifornia)          
sub20 <- round(0.2*total_samp)       
NorthernCalifornia20 <- NorthernCalifornia[sample(nrow(NorthernCalifornia), sub20), ]
write.csv(NorthernCalifornia20,"./NorthernCalifornia20.csv")

#Northern Gulf of Mexico
total_samp <- nrow(NorthGoM)          
sub20 <- round(0.2*total_samp)       
NorthGoM20 <- NorthGoM[sample(nrow(NorthGoM), sub20), ]
write.csv(NorthGoM20,"./NorthGoM20.csv")

#Oregon Washington Vancouver Coast
total_samp <- nrow(OregonWashingtonVancouverCoast)          
sub20 <- round(0.2*total_samp)       
OregonWashingtonVancouverCoast20 <- OregonWashingtonVancouverCoast[sample(nrow(OregonWashingtonVancouverCoast), sub20), ]
write.csv(OregonWashingtonVancouverCoast20,"./OregonWashingtonVancouverCoast20.csv")

#Panama Bight
total_samp <- nrow(PanamaBight)          
sub20 <- round(0.2*total_samp)       
PanamaBight20 <- PanamaBight[sample(nrow(PanamaBight), sub20), ]
write.csv(PanamaBight20,"./PanamaBight20.csv")

#Puget Trough/ Georgia Basin
total_samp <- nrow(PugetTroughGeorgiaBasin)          
sub20 <- round(0.2*total_samp)       
PugetTroughGeorgiaBasin20 <- PugetTroughGeorgiaBasin[sample(nrow(PugetTroughGeorgiaBasin), sub20), ]
write.csv(PugetTroughGeorgiaBasin20,"./PugetTroughGeorgiaBasin20.csv")

#Southern California Bight
total_samp <- nrow(SouthernCaliforniaBight)          
sub20 <- round(0.2*total_samp)       
SouthernCaliforniaBight20 <- SouthernCaliforniaBight[sample(nrow(SouthernCaliforniaBight), sub20), ]
write.csv(SouthernCaliforniaBight20,"./SouthernCaliforniaBight20.csv")

#Southern Gulf of Mexico
total_samp <- nrow(SouthGoM)          
sub20 <- round(0.2*total_samp)       
SouthGoM20 <- SouthGoM[sample(nrow(SouthGoM), sub20), ]
write.csv(SouthGoM20,"./SouthGoM20.csv")

#Southwest Caribbean
total_samp <- nrow(SouthwestCaribbean)          
sub20 <- round(0.2*total_samp)       
SouthwestCaribbean20 <- SouthwestCaribbean[sample(nrow(SouthwestCaribbean), sub20), ]
write.csv(SouthwestCaribbean20,"./SouthwestCaribbean20.csv")

#Virginian
total_samp <- nrow(Virginian)          
sub20 <- round(0.2*total_samp)       
Va20 <- Virginian[sample(nrow(Virginian), sub20), ]
write.csv(Va20,"./Virginian20.csv")

#Aves merged with 20% subsampling
Bahamas20$ecoregion                        <- "Bahamas"
Carolinian20$ecoregion                     <- "Carolinian"
ChiapasNicaragua20$ecoregion               <- "Chiapas Nicaragua"
Cortezian20$ecoregion                      <- "Cortezian"
Floridian20$ecoregion                      <- "Floridian"
GreaterAntilles20$ecoregion                <- "Greater Antilles"
GulfofMaineBayFundy20$ecoregion            <- "Gulf of Maine/ Bay of Fundy"
MagdalenaTransition20$ecoregion            <- "Magdalena Transition"
MexicanTropicPacific20$ecoregion           <- "Mexican Tropical Pacific"
Nicoya20$ecoregion                         <- "Nicoya"
NorthernCalifornia20$ecoregion             <- "Northern California"
NorthGoM20$ecoregion                       <- "Northern Gulf of Mexico"
OregonWashingtonVancouverCoast20$ecoregion <- "Oregon Washington Vancouver Coast"
PanamaBight20$ecoregion                    <- "Panama Bight"
PugetTroughGeorgiaBasin20$ecoregion        <- "Puget Trough Georgia Basin"
SouthernCaliforniaBight20$ecoregion        <- "Southern California Bight"
SouthGoM20$ecoregion                       <- "Southern Gulf of Mexico"
SouthwestCaribbean20$ecoregion             <- "Southwest Caribbean"
Virginian$ecoregion                        <- "Virginian"

Aves_20perc <- rbind(Bahamas20,Carolinian20)%>%
  rbind(ChiapasNicaragua20)%>%
  rbind(Cortezian20)%>%
  rbind(Floridian20)%>%
  rbind(GreaterAntilles20)%>%
  rbind(GulfofMaineBayFundy20)%>%
  rbind(MagdalenaTransition20)%>%
  rbind(MexicanTropicPacific20)%>%
  rbind(Nicoya20)%>%
  rbind(NorthernCalifornia20)%>%
  rbind(NorthGoM20)%>%
  rbind(OregonWashingtonVancouverCoast20)%>%
  rbind(PanamaBight20)%>%
  rbind(PugetTroughGeorgiaBasin20)%>%
  rbind(SouthernCaliforniaBight20)%>%
  rbind(SouthGoM20)%>%
  rbind(SouthwestCaribbean20)%>%
  rbind(Virginian)
Aves_20perc <- Aves_20perc%>%
  select(scientificName,family,decimalLongitude,decimalLatitude,date_year,
         datasetName,season,ecoregion)
write.csv(Aves_20perc,"./Aves_20perc.csv")


# Probability with iterations ---------------------------------------------
# Bahamas ------------------------------------------------------
total_samp <- nrow(Bahamas)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
# create matrix with same number of rows as iterations and the same number of columns 
# as total species found in whole dataset
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName) #Give columns names that coincide with the scientific names in whole dataset

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(Bahamas[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probBahamas.csv")

# for each species, gives how often it is seen in each iteration 
# (seen more often or less often over this area)
# Done as 1 or 0, not a cumulative sum of the times the species observed
# Can show differences over different time intervals

# graphn <- apply(df, 2, cumsum)
# for (k in 1:ncol(graphn)){
# plot(graphn[,k])
# }


# Carolinian --------------------------------------------------------------

total_samp <- nrow(Carolinian)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)
# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(Carolinian[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probCarolinian.csv")


# Chiapas Nicaragua --------------------------------------------------------

total_samp <- nrow(ChiapasNicaragua)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(ChiapasNicaragua[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probChiapasNicaragua.csv")

# Cortezian ---------------------------------------------------------------

total_samp <- nrow(Cortezian)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(Cortezian[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probCortezian.csv")
# Floridian ---------------------------------------------------------------
total_samp <- nrow(Floridian)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(Floridian[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probFloridian.csv")

# Greater Antilles ---------------------------------------------------------
total_samp <- nrow(GreaterAntilles)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(GreaterAntilles[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probGreaterAntilles.csv")

# Gulf of Maine/ Bay of Fundy -----------------------------------------------------
total_samp <- nrow(GulfofMaineBayFundy)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(GulfofMaineBayFundy[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probGulfofMainBayFundy.csv")


# Magdalena Transition ----------------------------------------------------
total_samp <- nrow(MagdalenaTransition)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(MagdalenaTransition[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probMagdalenaTransition.csv")


# Mexican Tropical Pacific ------------------------------------------------
total_samp <- nrow(MexicanTropicPacific)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(MexicanTropicPacific[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./MexicanTropicPacific.csv")


# Nicoya ------------------------------------------------------------------
total_samp <- nrow(Nicoya)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(Nicoya[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probNicoya.csv")


# Northern California -----------------------------------------------------
total_samp <- nrow(NorthernCalifornia)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(NorthernCalifornia[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probNorthernCalifornia.csv")



# Northern Gulf of Mexico -------------------------------------------------
total_samp <- nrow(NorthGoM)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(NorthGoM[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probNorthGoM.csv")

# Oregon Washington Vancouver Coast --------------------------------------------
total_samp <- nrow(OregonWashingtonVancouverCoast)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(OregonWashingtonVancouverCoast[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probOregonWashingtonVancouverCoast.csv")


# Panama Bight ------------------------------------------------------------
total_samp <- nrow(PanamaBight)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(PanamaBight[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probPanamaBight.csv")


# Puget Trough/ Georgia Basin ------------------------------------------------------------
total_samp <- nrow(PugetTroughGeorgiaBasin)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(PugetTroughGeorgiaBasin[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probPugetTroughGeorgiaBasin.csv")


# Southern California Bight-----------------------------------------------------
total_samp <- nrow(SouthernCaliforniaBight)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(SouthernCaliforniaBight[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probSouthernCaliforniaBight.csv")


# Southern Gulf of Mexico -------------------------------------------------
total_samp <- nrow(SouthGoM)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(SouthGoM[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probSouthGoM.csv")


# Southwest Caribbean -----------------------------------------------------

total_samp <- nrow(SouthwestCaribbean)          #find total number of samples for subregion
sub20 <- round(0.2*total_samp)       #subsample 20% of the total sample
iters <- 1000                        #iterations
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves2$scientificName)))  
colnames(dat1) <- unique(Aves2$scientificName)

# For the number of iterations, randomly sample 20% of the data and put it into the matrix formed above
for (i in 1:iters){
  idx <- sample(total_samp, sub20)
  
  dat1[i,]
  tests2 <- t(SouthwestCaribbean[idx,6])
  
  for (j in 1:length(tests2)){
    idx2 <- colnames(dat1) == tests2[j]
    dat1[i,idx2] <- 1
  }
}
df <- as.data.frame(dat1)
df[is.na(df)] <- 0
x1 <- apply(df, 2, sum)/iters*100 #Shows probability of species showing up in that subregion
write.csv(x1, "./probSouthwestCaribbean.csv")

