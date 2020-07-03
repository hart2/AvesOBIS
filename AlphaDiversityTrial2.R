# Finding alpha diversity per year (Americas) -----------------------------
# NOTE: Actual alpha diversity is the average species diversity in a habitat or specific area 
# Determine how to find alpha diversity per region/habitat (geographic maps?)

# set up the first year
Avesperyr <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1961) 
cnt_1961 <- unique(cnt_1961)
cnt_1961 <- count(cnt_1961)

# join all following years
for(year in 1962:2018){
  count <- Aves_EEZnd %>%
    select(date_year,scientificName) %>%
    filter(date_year == year) 
  count <- unique(count)
  count <- count(count)
  
  Avesperyr <- full_join(Avesperyr, count, by = 'n')
}


# data frame with "missing" values in the joins above, "missing" won't join with "Avesperyr"
# Apy1 <- full_join(cnt_1986, cnt_1989, by='n') 
# Apy1 <- Apy1 %>% 
#   full_join(cnt_1990, by='n') %>%             
#   full_join(cnt_1997, by='n') %>%             
#   full_join(cnt_2002, by='n') %>%             
#   full_join(cnt_2012, by='n') %>%             
#   full_join(cnt_2013, by='n') %>%            
#   full_join(cnt_2017, by='n')  

# colnames(Apy1) <-("species")
colnames(Avesperyr) <- c("species","year")
# avespy         <- full_join(Apy1,Avesperyr,by='species')

# When I try to merge "Avesperyr" and "Apy1", data from "Apy1" does not appear...

# Creating a barplot with the available years
Avesperyr$year=c(1961:1985,1987,1988,1991:1996,1998:2001,2003:2011,2014:2016,2018)

my_title <- expression(paste("Alpha Diversity of the Americas per Year"))
ggplot(data=Avesperyr, aes(x=year, y=species)) +
        geom_bar(stat="identity", fill="steelblue")+
        labs(x = "Year", y = "Number of Species", title = my_title,
             caption = "* Missing Years 1986, 1989, 1990, 1997, 2002, 2012, 2013, 2017")+
        theme(plot.title = element_text(h=0.5))
