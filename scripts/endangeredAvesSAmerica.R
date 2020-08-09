tbl2 <- Aves_EEZnd %>% 
  select(scientificName, date_year,decimalLongitude,decimalLatitude,individualCount) %>% 
  filter(scientificName == "Pterodroma incerta" | scientificName == "Thalassarche chlororhynchos")
colnames(tbl2) <- c("Scientific Name","Year","Longitude","Latitude","Individual Count")
tbl3 <- arrange(tbl2,`Year`)

FT <- formattable(tbl3, align =c("l","c","c","c","r"), 
                  list(`Latitude` = formatter(
                    "span", style = ~ style(color = "red",font.weight = "bold")) 
                  )) 

export_formattable(FT,"FT.png")