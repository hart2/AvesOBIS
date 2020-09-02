# Plot observations of sampling effort and species richness

# this will be done for every dataset and method

library(ggplot2)
my_title <- expression(paste("CalCOFI_NMFS Species Richness vs Sampling Effort"))

x <- df4 %>% 
  select(scientificName,effort) %>% 
  arrange(`scientificName`)

write.csv(x,"C:\\Users\\samantha\\Documents\\Chp1\\calco1.csv", row.names = FALSE)
# I manipulated the file in excel