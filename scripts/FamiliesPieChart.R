# Create charts of bird families found within each ecoregion

library(tidyverse) # packages for data visualization
library(dplyr)
library(readr)
library(devtools)
library(ggplot2)
library(sf)
library(rcartocolor) # colorblind friendly color palette (Can have 2-11 different colors)
library(viridis)   # create custom color scale           (Can have many different colors, still color blind friendly)

fam <- read.csv("~/github/AvesOBIS/Subsamp20/families.csv")

# "Accipitrida","Alcedinidae","Alcidae","Anatidae","Ardeidae","Charadriidae","Diomedeidae",
# "Fregatidae","Garildae","Hydrobatidae","Laridae","Pelecanidae","Phaethontidae",
# "Phalacrocoracidae","Phalaropidae","Podicipedidae","Procellariidae",
# "Recurvirostridae","Scolopacidae","Stercorariidae","Sternidae","Sulidae")

#Create a custom color scale
myColors <- plasma(22) #need 22 different colors instead of 9
names(myColors) <- levels(fam$Family)
colScale <- scale_colour_manual(name = "Family",values = myColors)

#One plot with all the data as points on a graph
p <- ggplot(fam,aes(x,y,colour = Family)) + geom_point()
p1 <- p + colScale

#A second plot with only four of the levels
p2 <- p %+% droplevels(subset(fam[4:10,])) + colScale