rm(list=ls())


## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")

#############################
## Middle East
#############################

# Load events
acled.raw <- read.csv("Input/Events/ACLED/ACLED-MiddleEast_2017-2018_upd-Mar6.csv")
head(acled.raw)
tail(acled.raw)
colnames(acled.raw)
unique(acled.raw$COUNTRY)
unique(acled.raw$ACTOR1)
unique(acled.raw$ACTOR2)

# Clean up
names(acled.raw)[names(acled.raw)=="COUNTRY"] <- "country"
countrylist <- sort(unique(as.character(acled.raw$country)))
countrylist <- data.frame(country=countrylist,iso3=countrycode(countrylist,origin="country.name",destination="iso3c"))
for(j in 1:2){countrylist[,j]<-as.character(countrylist[,j])}
countrylist
acled.raw <- merge(acled.raw,countrylist,by="country",all.x=T,all.y=T)

# Create actor dictionary

# Loop by country (first line finds k where you left off)
k0 <- max(which(countrylist$iso3%in%gsub("ACLED_|_Actors.RData","",dir("Dictionaries/ACLED/YZ"))))+1
k0
k <- 2

for(k in k0:nrow(countrylist)){
  subdata <- acled.raw[acled.raw$iso3%in%c(countrylist[k,"iso3"]),]
  print(countrylist[k,"country"])
  actorlist <- actorList(subdata,sidea="ACTOR1",sideb="ACTOR2",timevar="styr",countryvar="country")
  save(actorlist,file=paste0("Dictionaries/ACLED/YZ/ACLED_",countrylist[k,"iso3"],"_Actors.RData"))
}



#############################
## Asia
#############################

# Load events
acled.raw <- read.csv("Input/Events/ACLED/ACLED_Asia_2015-2018-_updMar6.csv")
head(acled.raw)
tail(acled.raw)
colnames(acled.raw)
unique(acled.raw$COUNTRY)
unique(acled.raw$ACTOR1)
unique(acled.raw$ACTOR2)

# Clean up
names(acled.raw)[names(acled.raw)=="COUNTRY"] <- "country"
countrylist <- sort(unique(as.character(acled.raw$country)))
countrylist <- data.frame(country=countrylist,iso3=countrycode(countrylist,origin="country.name",destination="iso3c"))
for(j in 1:2){countrylist[,j]<-as.character(countrylist[,j])}
countrylist
acled.raw <- merge(acled.raw,countrylist,by="country",all.x=T,all.y=T)

# Create actor dictionary

# Loop by country (first line finds k where you left off)

# k0 <- max(which(countrylist$iso3%in%gsub("ACLED_|_Actors.RData","",dir("Dictionaries/ACLED/Combined"))))+1
countrylist <- countrylist[countrylist$iso3%in%countrylist$iso3[!countrylist$iso3%in%gsub("ACLED_|_Actors.RData","",dir("Dictionaries/ACLED/Combined"))],]
countrylist
k <- 4

for(k in 1:nrow(countrylist)){
  subdata <- acled.raw[acled.raw$iso3%in%c(countrylist[k,"iso3"]),]
  print(countrylist[k,"country"])
  actorlist <- actorList(subdata,sidea="ACTOR1",sideb="ACTOR2",timevar="styr",countryvar="country")
  save(actorlist,file=paste0("Dictionaries/ACLED/YZ/ACLED_",countrylist[k,"iso3"],"_Actors.RData"))
}

