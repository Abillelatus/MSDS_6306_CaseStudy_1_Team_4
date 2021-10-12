##############################
#Beers and Brewery Case Study#
#Celia Banks                 #
#October 18, 2021            #
##############################


#Machine Learning for Classification | k-NN
#k-NN Class package in R
#Responses require k-NN be used
library(ggplot2)
library(lattice)
library(tidyverse)
library(class)
library(caret)
library(e1071)
library(xlsx)
library(stats)

#Code from week2 since EDA will be presented
#visualization techniques are needed

library(MASS)
library(dplyr)
library(readr)
library(GGally)
library(plotly)
library(ggthemes)
library(geofacet) #for geographic-based facets
library(naniar) #help with missing values
library(leaflet) #for map visualization



#read in datasets
beer <- read.xlsx(file="Beers.xlsx", sheetIndex = 1,   header=TRUE, stringsAsFactors = FALSE)

#Added state Latitude, Longitude, and Region columns in Excel
brewery <- read.xlsx(file="Breweries.xlsx", sheetIndex = 1,   header=TRUE, stringsAsFactors = FALSE)

#identify column names
names(brewery)

#VISUALIZATIONS
############################################################

#Begin prepare an interactive geo map of breweries by state
#set the map
leaflet()%>%addTiles()
#add popup for brewery information
brewery_map <- brewery%>%mutate(popup_info=paste(Name,"<br/>",City,"<br>",State,"<br>"))
#check popup_info column of data added to dataset
head(brewery_map)
#tweak the map with info layout, set circles smaller to enhance visual
leaflet()%>%addTiles()%>%addCircleMarkers(data=brewery_map, lat=~Latitude, lng=~Longitude, radius=~1, popup=~popup_info)
#End prepare an interactive geo map of breweries by state#

#corresponding distribution plot of breweries by state
ggplot(brewery_map, aes(x=as.factor(State), color="blue")) +
  geom_bar(color="blue") +
  ggtitle("Distribution of Breweries by State") +
  xlab("State") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-.25) +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(hjust = 0.5))

#corresponding distribution plot of breweries by region
ggplot(brewery_map, aes(x=as.factor(Region), color="green")) +
  geom_bar(color="green") +
  ggtitle("Distribution of Breweries by Region") +
  xlab("Region") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-.25) +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(hjust = 0.5))

##########################################
#Begin-Exploratory Data Analysis #
##########################################

#examine the datasets
View(beer)
View(brewery)

names(beer)

#merge the datasets
merge_bb <- merge(beer, brewery, by.x="Brewery_id", by.y="Brew_ID")

View(merge_bb)

#Drop latitude and longitude columns as no longer needed
drop <- c("Latitude","Longitude")
df = merge_bb[,!(names(merge_bb) %in% drop)]

View(df)
names(df)
#rename columns to avoid confusion
df <- df %>% 
  rename(
    Beer_Name = Name.x,
    Brewery_Name = Name.y
  )
View(df)

#get count of breweries in each state
brewery_count <- aggregate(Brewery_id ~ State, df, sum)
aggregate(df$Brewery_id, by=list(Category=df$State), FUN=sum)

#check for missing values 
library(naniar)
vis_miss(df)

#ABV, IBU and Style contain missing values - count
table(is.na(df$ABV))
table(is.na(df$IBU))
table(is.na(df$Style))

#view which beers do not have styles
df[is.na(df$Style),]

#impute the rows with zeroes
df$ABV[is.na(df$ABV)] <- 0
df$IBU[is.na(df$IBU)] <- 0

#recheck for missing values
vis_miss(df)

#compute median ABV and IBU for each state
medianABV <- aggregate(ABV ~ State + Region, df, median)
medianIBU <- aggregate(IBU ~ State + Region, df, median)

#quick visualize check
#scatter plot of medians ABV v IBU
library(ggvis)
mergeABVIBU %>% ggvis(x=~ABV, y=~IBU) %>% layer_points()

#merge the medians dataframes
mergeABVIBU <- merge(medianABV, medianIBU, by="State")
head(mergeABVIBU)

drop <- c("Region.y")
mergeABVIBU = merge_bb[,!(names(merge_bb) %in% drop)]

#Visualize ABV v IBU medians by state and by region
options(scipen = 999)
mergeABVIBU <- as.data.frame(mergeABVIBU)
#by state
ggplot(mergeABVIBU, aes(ABV, IBU)) +
  geom_point() +
  facet_wrap(vars(State)) 
#by region
ggplot(mergeABVIBU, aes(ABV, IBU, color=Region)) +
  geom_point() +
  facet_wrap(vars(Region)) 


###################################
#k-NN Analysis
###################################

#Problem Stated per Case Study:   
#Budweiser would also like to investigate the difference 
#with respect to IBU and ABV between IPAs (India Pale Ales) 
#and other types of Ale (any beer with "Ale" in its name other 
#than IPA).  You decide to use KNN classification to investigate
#this relationship.  Provide statistical evidence one way or the
#other. You can of course assume your audience is comfortable 
#with percentages.
