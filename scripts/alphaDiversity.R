# Finding alpha diversity/species richness per year (Americas)-----------------------------
# NOTE: Actual alpha diversity is the average species diversity in a habitat or specific area 
# Determine how to find alpha diversity per region/habitat (geographic maps?)

# set up the first year
Avesperyr <- Aves %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1961) 
Avesperyr <- unique(Avesperyr)
Avesperyr <- count(Avesperyr)

# join all following years
for(year in 1962:2018){
  count <- Aves %>%
    select(date_year,scientificName) %>%
    filter(date_year == year) 
  count <- unique(count)
  count <- count(count)
  
  Avesperyr <- full_join(Avesperyr, count, by = 'n')
}

# Years are missing, add them by hand (Years: 1963, 1964, 1966:1968, 1997, 2004, 2010:2012, 2014, 2018)

cnt_1962 <- Aves %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1962)
cnt_1962 <- unique(cnt_1962)
cnt_1962 <- count(cnt_1962)

cnt_1963 <- Aves %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1963)
cnt_1963 <- unique(cnt_1963)
cnt_1963 <- count(cnt_1963)

cnt_1964 <- Aves %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1964)
cnt_1964 <- unique(cnt_1964)
cnt_1964 <- count(cnt_1964)

cnt_1966 <- Aves %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1966)
cnt_1966 <- unique(cnt_1966)
cnt_1966 <- count(cnt_1966)

cnt_1967 <- Aves %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1967)
cnt_1967 <- unique(cnt_1967)
cnt_1967 <- count(cnt_1967)

cnt_1968 <- Aves %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1968)
cnt_1968 <- unique(cnt_1968)
cnt_1968 <- count(cnt_1968)

cnt_1997 <- Aves %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1997)
cnt_1997 <- unique(cnt_1997)
cnt_1997 <- count(cnt_1997)

cnt_2004 <- Aves %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2004)
cnt_2004 <- unique(cnt_2004)
cnt_2004 <- count(cnt_2004)

cnt_2010 <- Aves %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2010)
cnt_2010 <- unique(cnt_2010)
cnt_2010 <- count(cnt_2010)

cnt_2011 <- Aves %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2011)
cnt_2011 <- unique(cnt_2011)
cnt_2011 <- count(cnt_2011)

cnt_2012 <- Aves %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2012)
cnt_2012 <- unique(cnt_2012)
cnt_2012 <- count(cnt_2012)

cnt_2014 <- Aves %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2014)
cnt_2014 <- unique(cnt_2014)
cnt_2014 <- count(cnt_2014)

cnt_2018 <- Aves %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2018)
cnt_2018 <- unique(cnt_2018)
cnt_2018 <- count(cnt_2018)

# Join matrices ("full_join" does not work)
a1 <- rbind(cnt_1963,Avesperyr) %>% 
  rbind(cnt_1964) %>% 
  rbind(cnt_1966) %>% 
  rbind(cnt_1967) %>% 
  rbind(cnt_1968) %>% 
  rbind(cnt_1997) %>% 
  rbind(cnt_2004) %>% 
  rbind(cnt_2010) %>% 
  rbind(cnt_2011) %>% 
  rbind(cnt_2012) %>% 
  rbind(cnt_2014) %>% 
  rbind(cnt_2018)

a1$year=c(1963,1961,1962,1965,1969:1996,1998:2003,2005:2009,2013,2015,2016,2017,1964,
          1966,1967,1968,1997,2004,2010,2011,2012,2014,2018)
colnames(a1) <- c("species","year")
Avesperyr = a1

rm(list=ls(pattern="cnt_")) # remove everything from Environment that starts with "cnt_"
rm(a1)
rm(count)

# Creating a barplot with the available years
Avesperyr <- Avesperyr %>% 
  arrange(year)


# Graphics uploaded to box and github
my_title <- expression(paste("Alpha Diversity of the Americas per Year"))
ggplot(data=Avesperyr, aes(x=year, y=species)) +
  geom_bar(stat="identity", fill="steelblue")+
  labs(x = "Year", y = "Number of Species", title = my_title)+
  theme(plot.title = element_text(h=0.5))