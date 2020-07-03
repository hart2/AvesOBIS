# Finding alpha diversity per year (Americas) -----------------------------
# NOTE: Actual alpha diversity is the average species diversity in a habitat or specific area 
# Determine how to find alpha diversity per region/habitat (geographic maps?)

cnt_1961 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1961) 
cnt_1961 <- unique(cnt_1961)
cnt_1961 <- count(cnt_1961)

cnt_1962 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1962) 
cnt_1962 <- unique(cnt_1962)
cnt_1962 <- count(cnt_1962)

cnt_1963 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1963) 
cnt_1963 <- unique(cnt_1963)
cnt_1963 <- count(cnt_1963)

cnt_1964 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1964) 
cnt_1964 <- unique(cnt_1964)
cnt_1964 <- count(cnt_1964)

cnt_1965 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1965) 
cnt_1965 <- unique(cnt_1965)
cnt_1965 <- count(cnt_1965)

cnt_1966 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1966) 
cnt_1966 <- unique(cnt_1966)
cnt_1966 <- count(cnt_1966)

cnt_1967 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1967) 
cnt_1967 <- unique(cnt_1967)
cnt_1967 <- count(cnt_1967)

cnt_1968 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1968) 
cnt_1968 <- unique(cnt_1968)
cnt_1968 <- count(cnt_1968)

cnt_1969 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1969) 
cnt_1969 <- unique(cnt_1969)
cnt_1969 <- count(cnt_1969)

cnt_1970 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1970) 
cnt_1970 <- unique(cnt_1970)
cnt_1970 <- count(cnt_1970)

cnt_1971 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1971) 
cnt_1971 <- unique(cnt_1971)
cnt_1971 <- count(cnt_1971)

cnt_1972 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1972) 
cnt_1972 <- unique(cnt_1972)
cnt_1972 <- count(cnt_1972)

cnt_1973 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1973) 
cnt_1973 <- unique(cnt_1973)
cnt_1973 <- count(cnt_1973)

cnt_1974 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1974) 
cnt_1974 <- unique(cnt_1974)
cnt_1974 <- count(cnt_1974)

cnt_1975 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1975) 
cnt_1975 <- unique(cnt_1975)
cnt_1975 <- count(cnt_1975)

cnt_1976 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1976) 
cnt_1976 <- unique(cnt_1976)
cnt_1976 <- count(cnt_1976)

cnt_1977 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1977) 
cnt_1977 <- unique(cnt_1977)
cnt_1977 <- count(cnt_1977)

cnt_1978 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1978) 
cnt_1978 <- unique(cnt_1978)
cnt_1978 <- count(cnt_1978)

cnt_1979 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1979) 
cnt_1979 <- unique(cnt_1979)
cnt_1979 <- count(cnt_1979)

cnt_1980 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1980) 
cnt_1980 <- unique(cnt_1980)
cnt_1980 <- count(cnt_1980)

cnt_1981 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1981) 
cnt_1981 <- unique(cnt_1981)
cnt_1981 <- count(cnt_1981)

cnt_1982 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1982) 
cnt_1982 <- unique(cnt_1982)
cnt_1982 <- count(cnt_1982)

cnt_1983 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1983) 
cnt_1983 <- unique(cnt_1983)
cnt_1983 <- count(cnt_1983)

cnt_1984 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1984) 
cnt_1984 <- unique(cnt_1984)
cnt_1984 <- count(cnt_1984)

cnt_1985 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1985) 
cnt_1985 <- unique(cnt_1985)
cnt_1985 <- count(cnt_1985)

cnt_1986 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1986) 
cnt_1986 <- unique(cnt_1986)
cnt_1986 <- count(cnt_1986)

cnt_1987 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1987) 
cnt_1987 <- unique(cnt_1987)
cnt_1987 <- count(cnt_1987)

cnt_1988 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1988) 
cnt_1988 <- unique(cnt_1988)
cnt_1988 <- count(cnt_1988)

cnt_1989 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1989) 
cnt_1989 <- unique(cnt_1989)
cnt_1989 <- count(cnt_1989)

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

cnt_1992 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1992) 
cnt_1992 <- unique(cnt_1992)
cnt_1992 <- count(cnt_1992)

cnt_1993 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1993) 
cnt_1993 <- unique(cnt_1993)
cnt_1993 <- count(cnt_1993)

cnt_1994 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1994) 
cnt_1994 <- unique(cnt_1994)
cnt_1994 <- count(cnt_1994)

cnt_1995 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1995) 
cnt_1995 <- unique(cnt_1995)
cnt_1995 <- count(cnt_1995)

cnt_1996 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1996) 
cnt_1996 <- unique(cnt_1996)
cnt_1996 <- count(cnt_1996)

cnt_1997 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1997) 
cnt_1997 <- unique(cnt_1997)
cnt_1997 <- count(cnt_1997)

cnt_1998 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 1998) 
cnt_1998 <- unique(cnt_1998)
cnt_1998 <- count(cnt_1998)

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

cnt_2001 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2001) 
cnt_2001 <- unique(cnt_2001)
cnt_2001 <- count(cnt_2001)

cnt_2002 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2002) 
cnt_2002 <- unique(cnt_2002)
cnt_2002 <- count(cnt_2002)

cnt_2003 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2003) 
cnt_2003 <- unique(cnt_2003)
cnt_2003 <- count(cnt_2003)

cnt_2004 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2004) 
cnt_2004 <- unique(cnt_2004)
cnt_2004 <- count(cnt_2004)

cnt_2005 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2005) 
cnt_2005 <- unique(cnt_2005)
cnt_2005 <- count(cnt_2005)

cnt_2006 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2006) 
cnt_2006 <- unique(cnt_2006)
cnt_2006 <- count(cnt_2006)

cnt_2007 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2007) 
cnt_2007 <- unique(cnt_2007)
cnt_2007 <- count(cnt_2007)

cnt_2008 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2008) 
cnt_2008 <- unique(cnt_2008)
cnt_2008 <- count(cnt_2008)

cnt_2009 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2009) 
cnt_2009 <- unique(cnt_2009)
cnt_2009 <- count(cnt_2009)

cnt_2010 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2010) 
cnt_2010 <- unique(cnt_2010)
cnt_2010 <- count(cnt_2010)

cnt_2011 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2011) 
cnt_2011 <- unique(cnt_2011)
cnt_2011 <- count(cnt_2011)

cnt_2012 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2012) 
cnt_2012 <- unique(cnt_2012)
cnt_2012 <- count(cnt_2012)

cnt_2013 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2013) 
cnt_2013 <- unique(cnt_2013)
cnt_2013 <- count(cnt_2013)

cnt_2014 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2014) 
cnt_2014 <- unique(cnt_2014)
cnt_2014 <- count(cnt_2014)

cnt_2015 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2015) 
cnt_2015 <- unique(cnt_2015)
cnt_2015 <- count(cnt_2015)

cnt_2016 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2016) 
cnt_2016 <- unique(cnt_2016)
cnt_2016 <- count(cnt_2016)

cnt_2017 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2017) 
cnt_2017 <- unique(cnt_2017)
cnt_2017 <- count(cnt_2017)

cnt_2018 <- Aves_EEZnd %>%
  select(date_year,scientificName) %>%
  filter(date_year == 2018) 
cnt_2018 <- unique(cnt_2018)
cnt_2018 <- count(cnt_2018)

Avesperyr <- cnt_1961 %>% 
  full_join(cnt_1962, by = 'n') %>% 
  full_join(cnt_1963, by = 'n') %>% 
  full_join(cnt_1964, by = 'n') %>%
  full_join(cnt_1965, by = 'n') %>% 
  full_join(cnt_1966, by = 'n') %>% 
  full_join(cnt_1967, by = 'n') %>% 
  full_join(cnt_1968, by = 'n') %>% 
  full_join(cnt_1969, by = 'n') %>% 
  full_join(cnt_1970, by = 'n') %>% 
  full_join(cnt_1971, by = 'n') %>% 
  full_join(cnt_1972, by = 'n') %>% 
  full_join(cnt_1973, by = 'n') %>% 
  full_join(cnt_1974, by = 'n') %>% 
  full_join(cnt_1975, by = 'n') %>% 
  full_join(cnt_1976, by = 'n') %>% 
  full_join(cnt_1977, by = 'n') %>% 
  full_join(cnt_1978, by = 'n') %>% 
  full_join(cnt_1979, by = 'n') %>% 
  full_join(cnt_1980, by = 'n') %>% 
  full_join(cnt_1981, by = 'n') %>%
  full_join(cnt_1982, by = 'n') %>%
  full_join(cnt_1983, by = 'n') %>% 
  full_join(cnt_1984, by = 'n') %>% 
  full_join(cnt_1985, by = 'n') %>% 
  full_join(cnt_1986, by = 'n') %>% # missing
  full_join(cnt_1987, by = 'n') %>% 
  full_join(cnt_1988, by = 'n') %>% 
  full_join(cnt_1989, by = 'n') %>% # missing
  full_join(cnt_1990, by = 'n') %>% # missing
  full_join(cnt_1991, by = 'n') %>%
  full_join(cnt_1992, by = 'n') %>%
  full_join(cnt_1993, by = 'n') %>%
  full_join(cnt_1994, by = 'n') %>%
  full_join(cnt_1995, by = 'n') %>%
  full_join(cnt_1996, by = 'n') %>%
  full_join(cnt_1997, by = 'n') %>% # missing
  full_join(cnt_1998, by = 'n') %>%
  full_join(cnt_1999, by = 'n') %>%
  full_join(cnt_2000, by = 'n') %>%
  full_join(cnt_2001, by = 'n') %>%
  full_join(cnt_2002, by = 'n') %>% # missing
  full_join(cnt_2003, by = 'n') %>% 
  full_join(cnt_2004, by = 'n') %>%
  full_join(cnt_2005, by = 'n') %>%
  full_join(cnt_2006, by = 'n') %>%
  full_join(cnt_2007, by = 'n') %>%
  full_join(cnt_2008, by = 'n') %>%
  full_join(cnt_2009, by = 'n') %>%
  full_join(cnt_2010, by = 'n') %>%
  full_join(cnt_2011, by = 'n') %>%
  full_join(cnt_2012, by = 'n') %>% # missing
  full_join(cnt_2013, by = 'n') %>% # missing 
  full_join(cnt_2014, by = 'n') %>%
  full_join(cnt_2015, by = 'n') %>%
  full_join(cnt_2016, by = 'n') %>%
  full_join(cnt_2017, by = 'n') %>% # missing
  full_join(cnt_2018, by = 'n')

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
