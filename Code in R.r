install.packages('tidyverse')
library(tidyverse)

#loading the csv file
arrivals <- read.csv('/Users/jayasmitachakraborty/Downloads/world_bank_international_arrivals_islands copy.csv')

#preparing for imputation
library(mice)
md.pattern(arrivals)

#imputing using mice and method = 'cart' for all the columns
#imputation by classification and regression trees
mice_imputes = mice(arrivals, m=7, method='cart')
mice_imputes$method
imputed_data=complete(mice_imputes,7)
head(imputed_data)
arrivals <- imputed_data
write.csv(arrivals, 'processed_data.csv')
arrivals$country <- as.factor(arrivals$country)

#Part 1 - Population, GDP & Tourism Revenue

#population of each country
ggplot(arrivals, aes(x=country, y=pop)) + geom_bar(stat="identity", colour="light blue") + ylab("Population") + 
  labs(title="Figure 1: Population per country") + theme_light()

#Average GDP for 13 years in descending order
library(plyr)
arrivals_mean <- ddply( arrivals, .(country),  
                        summarise, pop.mean = mean(pop), gdp.mean = mean(gdpnom), 
                        receipt.mean=mean(receipt), hotels.mean=mean(hotels), 
                        hotrooms.mean=mean(hotrooms), flights.mean=mean(flights...WB),
                        dayvisit.mean = mean(dayvisit), ovnarriv.mean = mean(ovnarriv),
                        arr = mean(arram + arreur + arraus))

p <- ggplot(arrivals_mean, aes(reorder(country, gdp.mean),gdp.mean)) + 
  geom_bar(stat="identity", fill="red", alpha=0.6) + coord_flip()

p + labs(title="Figure 2: GDP per Nation", x="Mean GDP", y="Country") 

#some countries seem to have a higher mean GDP than others.
#Let's try to understand how the GDP was behaving in the 13 year time frame.
countries_gdp = arrivals %>% filter(country %in% c(25,26,5,1,13,7,6))
ggplot(countries_gdp, aes(gdpnom, year)) + geom_point(aes(colour=country)) + 
  facet_grid(country~.) + scale_y_continuous(breaks=seq(0,12,1)) + 
  labs(title = "Figure 3: 13 Year GDP Trend")
#the only noticeable trend is for country 25 where GDP constantly increases over the 13 years
#we will look at country 25 in more details later.

#relationship between gdp and population
ggplot(arrivals, aes(x=pop, y=gdpnom)) + geom_point()
#does high population imply higher GDP?
#we don't have enough data to say that, 
#but we can try to verify the relationship between population and GDP using another metric
#such as population density

arrivals$popden <- arrivals$pop/arrivals$areakm2
ggplot(arrivals, aes(x=popden, y=gdpnom)) + geom_point() + scale_y_log10() +
  annotate("rect", xmin=5200, xmax = 8000, ymin=1e+10, ymax=1e+12, alpha=0.1, fill="blue") +
  annotate("rect", xmin=0, xmax = 750, ymin=0, ymax=1e+12, alpha=0.3, fill="red") +
  labs(title="Figure 4: GDP vs Population")
#there are 2 parts of this plot which are significant
# 1. the part highlighted in blue where increase in population density increase GDP rapidly
# 2. part highlighted in red where GDP increases a lot for a certain constant population density
#let's investigate these 2 parts further

sort(unique(arrivals$popden))
country_by_inc_popden <- subset(arrivals, (popden>=5200 & popden<=8000))
View(country_by_inc_popden)
#it seems only country 25 is falls under the part annotated by blue in the previous groph

country25 <- subset(arrivals, country==25)
head(country25)
ggplot(country25, aes(x=popden, y=gdpnom)) + geom_point(colour='red') + 
  labs(title="Figure 5:Population Density vs. GDP (Year-on-Year)", x="Population Density", y="GDP")
#we can see that gdp is continuous increasing for country 25 - Singapore as population density increases.
#let's try to understand the tourism pattern for Singapore
#how much has tourism increased over the 13 year time frame?
#is it dependent on GDP growth?

ggplot(country25, aes(x=gdpnom, y=receipt)) + geom_line() +
  labs(title="Figure 6: Tourism Revenue vs. GDP", x="Annual GDP", y="Tourism Revenue")
ggplot(country25, aes(x=year, y=receipt)) + geom_line() + scale_x_continuous(breaks=seq(0,13,1))
#tourism has been on an increasing trend till year 11, but there seems to be no increase in year 12. WHY?
#the most important factors for tourism revenue = ovnarriv, dayvist, arram, arreur, arraus
ggplot(country25, aes(year)) + geom_line(aes(y=ovnarriv,colour="ovnarriv")) +
  geom_line(aes(y=dayvisit, colour="dayvisit")) +
  geom_line(aes(y=arram, colour="arram")) +
  geom_line(aes(y=arreur, colour="arreur")) +
  geom_line(aes(y=arraus, colour="arraus"))  +
  labs(title="Figure 7: Factors Affecting Tourism Revenue", y="Overnight Arrivals")
 #it seems that the sharpest drop of incoming tourists is because of lower overnight arrivals.
  #followed by drop in arrivals from US and EU.

#let's try to find how much the receipt amount varies with the number of day visits
ggplot(country25, aes(receipt,sort(dayvisit))) + geom_line()
#We can see that in general, receipt amount increases with increase in number of visit days
#people spend more is they stay longer in the country, obviously!

country_by_const_popden <- subset(arrivals, popden>0 & popden<=700)
View(country_by_const_popden)
ggplot(country_by_const_popden, aes(x=popden,y=gdpnom, colour=country)) + 
  geom_point(size=3) + scale_y_log10() +
  labs(title= "Figure 8: GDP Increase for Near Constant Population Density",
       x="Population Density", y="GDP")
#most of these countries show a very rapid increase in GDP for a constant population density
#the most noticeable increases are for countries 23(Tuvalu), 24(Palau) & 26(Trinidad & Tobago)
countries <- subset(arrivals, country==c(23,24,26))
View(countries)
ggplot(countries, aes(popden,gdpnom,colour=country)) + geom_point(size=4) + scale_y_log10() +
  labs(title = "Figure 8.1", x="Population Density", y="GDP")
#it seems that country 24(Palau) has shown the largest increase in GDP
#NOTE: country 24 had no "gdpnoom", "dayvisit" and "arrus" information.
#most of the visualizations below is from imputed data, hence may be inconclusive
country24 <- subset(arrivals, country==24)
ggplot(country24, aes(popden,gdpnom)) + geom_point() + scale_y_log10()
#let's check the trend for tourism for country24 Palau
ggplot(country24, aes(x=popden, y=gdpnom)) + geom_point()

#mathematically, what can be a good measure to check how of tourism revenue contributed to GDP?
country24$percent_tourism <- country24$receipt/country24$gdpnom * 100
ggplot(country24, aes(x=gdpnom, y=percent_tourism)) + geom_line()
#by taking the revenue generate by tourism as a percentage of the total GDP of the nation
#we can see that tourism revenue as a part of GDP has rapidly declined over the 13 year time frame,
#did tourism decline?
ggplot(country24, aes(x=year, y=receipt)) + geom_line() + scale_x_continuous(breaks=seq(0,13,1))
#revenue from tourism has been continually on the increase.
#this means that the growing GDP was due to factors other than tourism

#Part 2 - No. of flights and Tourism Revenue
flights <- subset(arrivals, country!=c(4,6,7,9,10,11,15,18,19,20,21,23,24))
ggplot(flights, aes(country, flights...WB)) + geom_bar(stat="identity") + theme_light()
#the plot is as expected because the country 25 - Singapore has the maximum tourism as well
ggplot(country25, aes(year,flights...WB)) + geom_line()
#over the 13 year time frame, it also shows an increase in flight volume.

#Part 3 - Cheapest country to visit for best tourism
#we want to select something that is cheap, but also has good infrastructure for tourists
#such as regular flights, hotels
#we can consider GDP to check if the country has the capacity to spend on tourism infrastructure
#ASSUMPTION: countries with high GDP will tend to spend more on tourism infrastructure
#can we verify this assumption graphically?
ggplot(arrivals_mean, aes(x=gdp.mean, y=hotels.mean+hotrooms.mean)) + geom_point(size=2.5) + 
  scale_x_log10() + geom_smooth(method="lm") + labs(title="Figure 9: No. of Hotels + Hotrooms vs. National GDP",
                                                    x="GDP", y="No.of Hotels+Hotrooms")

#first, we find the per day cost
ggplot(arrivals_mean, aes(country, receipt.mean/dayvisit.mean)) + geom_point(size=3,colour="dark green") + 
  coord_flip() + theme_light() + scale_x_continuous(breaks=seq(1,27,1)) +
  labs(title="Figure 10: Per Day Cost in Each Country", y="Per Day Cost", x="Country")
#we can see the most expensive countries to visit are:
# 1. Mauritius (1)
# 2. Cape Verde (8)
# 3. Samoa (16)
#for all other nations, expense per day is lower

#second, let's analyze the tourism infrastructure for the nations
ggplot(arrivals_mean, aes(x=country, y=hotels.mean+hotrooms.mean)) + geom_point(size=3, colour="dark blue") + 
  coord_flip() + labs(title="Figure 11: Tourism Infrastructure In Each Country", y="Hotels+Hotrooms",
                      x="Country") + theme_minimal()
#it seems that tourism infrastructure is most developed in country 25 -Singapore. 
#Followed by countries 13(Malta), 12(Maldives), 1(Mauritius), 26(Trinidad & Tobago) and 5(Bahrain)
#the remaining countries don't seem to have a lot of options for hotels and hotrooms

#third, let's look at flight options - 
#we need to ensure that the country has a good number of flights so that tourists have choice
ggplot(arrivals_mean, aes(country, flights.mean)) + geom_point(size=3, colour="dark red") +
  coord_flip() + theme_light() +
  labs(title="Figure 12: Flights in the Countries", y="Number of Flights")
#again, it seems country 25(Singapore) has the most flights
#Followed by countries 5(Bahrain), 9(Comoros), 3(Antigua & Barbuda)

#last, we check the number of overnight arrivals to check how popular the countries are for tourists
ggplot(arrivals_mean, aes(country, ovnarriv.mean)) + geom_point(size=3,colour="purple") +
  coord_flip() +theme_minimal() + labs(title="Figure 13: International Arrivals per Country", 
                                       y="International Arrivals")

#It seems that countries 25(Singapore) & 5(Bahrain) have the most international passengers from flights
#can we come to direct conclusion that these countries are most popular for tourists?
#no, because not all flight passengers come for tourism, some may come for work.
#a better way would be to compare the tourism revenue(receipt) of both these nations 
#to check if indeed these countries are popular tourist destinations
#we have already done the analysis for Singapore (country 25), now let's compare with the others

#for other countries

#normalising data to reduce the skewed effect by "receipt" attribute
unit_length <- function(x) { x / sqrt(sum(x^2)) }
arrivals_mean[,2:10] <- as.data.frame(lapply(arrivals_mean[,2:10], unit_length))

popular_countries <- arrivals_mean %>% filter(country %in% c(1,3,5,9,12,13,25,26))
popular_countries <- popular_countries %>%
  mutate(country_name = case_when(
    country == 1 ~ 'Mauritius',
    country == 3 ~ 'Antigua & Barbuda',    
    country == 5 ~ 'Bahrain',
    country == 9 ~ 'Comoros',
    country == 12 ~ 'Maldives',
    country == 13 ~ 'Malta',
    country == 25 ~ 'Singapore',
    country == 26 ~ 'Trinidad & Tobago'
  ))

#processing data to create the table with factors for tourism
popular_countries.mean <- ddply( popular_countries, .(country, country_name),  
                        summarise,
                        Hotels=hotels.mean+hotrooms.mean, 
                        Flights=flights.mean,
                        PerDayCost = receipt.mean/dayvisit.mean, 
                        Popularity = receipt.mean/ovnarriv.mean)

#converting to wide format based on the tourism factors
popular_countries_tall <- gather(popular_countries.mean, factor, value, 
                                 Hotels:Popularity, factor_key=TRUE)

#plotting factors for each nation
ggplot(popular_countries_tall, aes(fill=factor, y=value, x=country_name, colour=factor)) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Figure 14: Best (and Cheap) Tourism Destination") + labs(x="Country")

#rescaling the values to rating scale of 1 to 5
install.packages("scales")
library(scales)
popular_countries.mean$PerDayCost <- rescale(popular_countries.mean$PerDayCost, to=c(5, 1)) 
popular_countries.mean$Hotels <- rescale(popular_countries.mean$Hotels, to=c(1,5)) 
popular_countries.mean$Flights <- rescale(popular_countries.mean$Flights, to=c(1,5)) 
popular_countries.mean$Popularity <- rescale(popular_countries.mean$Popularity, to=c(1,5)) 

popular_countries_tall2 <- gather(popular_countries.mean, factor, value, 
                                 Hotels:Popularity, factor_key=TRUE)

ggplot(popular_countries_tall2, aes(fill=factor, y=value, x=country_name, colour=factor)) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Figure 15: Best (and Cheap) Tourism Destination") + labs(x="Country", y="Rating")

#other rudimentary plots for the popular countries
#per day expenses
ggplot(popular_countries, aes(year, receipt/dayvisit)) + geom_line((aes(colour=country_name))) + 
  scale_x_continuous(breaks=seq(0,12,1))

#tourism infrastructure = hotels + hotrooms
ggplot(popular_countries, aes(year, hotels+hotrooms)) + geom_line((aes(colour=country_name))) + 
  scale_x_continuous(breaks=seq(0,12,1))

#accessibility & ease of travel = flights
ggplot(popular_countries, aes(year, flights...WB)) + geom_line((aes(colour=country_name))) + 
  scale_x_continuous(breaks=seq(0,12,1))
#from flight options, Country 3(Antigua & Barbuda) shows a significant decline

#from all the visualizations above, we can conclude the best & cheap tourism destinations are -
# Singapore and Bahrain
#and for luxury tourist destination, it's Maldives and Mauritius

#persuasion
average_rating <- ddply( popular_countries.mean, .(country_name),  
                         summarise,
                         rating = mean(Hotels+PerDayCost+Flights+Popularity))

average_rating$rating <- rescale(average_rating$rating, to=c(1, 5)) 

ggplot(average_rating, aes(x=reorder(country_name,rating),y=rating)) +
  geom_bar(position="dodge", stat="identity", fill="maroon") + 
  ggtitle("Popular Travel Destinations Rated") + labs(x="Country", y="Average Rating") + 
  theme_light() + coord_flip()
