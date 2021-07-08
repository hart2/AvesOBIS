# subsample each region at 20% seeing how many species were observed, 1000 iterations, giving 
# probability/percent of seeing species in that region (percent on average that you would see a species of bird)
# randomly iterating
# giving total of species found over all iterations as a probability of it being observed in that region
GulfofCalifornia <- read_csv("~/github/AvesOBIS/GulfofCalifornia.csv")

total_samp <- nrow(GulfofCalifornia)

sub20 <- round(0.2*total_samp)
iters <- 1000
dat1 <- matrix(NA, nrow = iters, ncol = length(unique(Aves_NA$scientificName)))
colnames(dat1) <- unique(Aves_NA$scientificName)

dat1 <- matrix(NA, nrow = iters, ncol = length(unique(GulfofCalifornia$scientific)))
colnames(dat1) <- unique(GulfofCalifornia$scientific)

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
apply(df, 2, sum)/iters*100

# for each species, gives how often it is seen in each iteration 
# (seen more often or less ofter over this area)
# Done as 1 or 0, not a cumulative sum of the times the species observed
# Can show differences over different time intervals
graphn <- apply(df, 2, cumsum)
for (k in 1:ncol(graphn)){
plot(graphn[,k])
}
