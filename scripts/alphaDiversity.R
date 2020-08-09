# Finding alpha diversity per year (Americas) works on my old Dell Inspiron 5421-----------------------------
# NOTE: Actual alpha diversity is the average species diversity in a habitat or specific area 
# Determine how to find alpha diversity per region/habitat (geographic maps?)

# set up the first year
Avesperyr <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1961) 
Avesperyr <- unique(Avesperyr)
Avesperyr <- count(Avesperyr)

# join all following years
for(year in 1962:2018){
  count <- Aves_EEZnd %>%
    select(date_year,scientificName) %>%
    filter(date_year == year) 
  count <- unique(count)
  count <- count(count)
  
  Avesperyr <- full_join(Avesperyr, count, by = 'n')
}


# Years are missing, add them by hand (Years: 1985,1990,1991,1999,2000,2013,2016,2018)

cnt_1985 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1985)
cnt_1985 <- unique(cnt_1985)
cnt_1985 <- count(cnt_1985)

cnt_1990 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1990)
cnt_1990 <- unique(cnt_1990)
cnt_1990 <- count(cnt_1990)

cnt_1991 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1991)
cnt_1991 <- unique(cnt_1991)
cnt_1991 <- count(cnt_1991)

cnt_1999 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1999)
cnt_1999 <- unique(cnt_1999)
cnt_1999 <- count(cnt_1999)

cnt_2000 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2000)
cnt_2000 <- unique(cnt_2000)
cnt_2000 <- count(cnt_2000)

cnt_2013 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2013)
cnt_2013 <- unique(cnt_2013)
cnt_2013 <- count(cnt_2013)

cnt_2016 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2016)
cnt_2016 <- unique(cnt_2016)
cnt_2016 <- count(cnt_2016)

cnt_2018 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2018)
cnt_2018 <- unique(cnt_2018)
cnt_2018 <- count(cnt_2018)

# Join matrices ("full_join" does not work)
a1 <- rbind(cnt_1985,Avesperyr) %>% 
  rbind(cnt_1990) %>% 
  rbind(cnt_1991) %>% 
  rbind(cnt_1999) %>% 
  rbind(cnt_2000) %>% 
  rbind(cnt_2013) %>% 
  rbind(cnt_2016) %>% 
  rbind(cnt_2018)

colnames(a1) <- c("species","year")
a1$year=c(1985,1961:1984,1986:1989,1992:1998,2001:2012,2014,2015,2017,1990,1991,
          1999,2000,2013,2016,2018)
Avesperyr = a1
colnames(Avesperyr) <- c("species","year")

# Creating a barplot with the available years
Avesperyr$year=c(1961:2018)


# Graphics uploaded to box and github
my_title <- expression(paste("Alpha Diversity of the Americas per Year"))
ggplot(data=Avesperyr, aes(x=year, y=species)) +
  geom_bar(stat="identity", fill="steelblue")+
  labs(x = "Year", y = "Number of Species", title = my_title)+
  theme(plot.title = element_text(h=0.5))