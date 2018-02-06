rm(list=ls())

## Set directory
#setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")
setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")

#############################
## Clean up data
#############################

# Load events
#For African countries
acled.raw <- read.csv("Input/Events/ACLED/ACLED7_Africa_1997-2016_dyadic.csv")
head(acled.raw)
tail(acled.raw)
colnames(acled.raw)
unique(acled.raw$COUNTRY)
unique(acled.raw$ACTOR1)
unique(acled.raw$ACTOR2)



#For Asia
acled.raw <- read.csv("Input/Events/ACLED/ACLED_Asia/India-ACLED-Asia-2015-2016.csv")
#acled.raw <- read.csv("Input/Events/ACLED/ACLED_Asia/acled_Asia_2015-2016.csv")
head(acled.raw)
tail(acled.raw)
colnames(acled.raw)
unique(acled.raw$COUNTRY)
#acled.raw[acled.raw$COUNTRY=="India", ] <- acled.raw[acled.raw$COUNTRY=="india",]
#write.csv(acled.raw, file="Input/Events/ACLED/ACLED_Asia/acled_Asia_2015-2016.csv")

# Clean up
names(acled.raw)[names(acled.raw)=="COUNTRY"] <- "country"
countrylist <- sort(unique(as.character(acled.raw$country)))
countrylist <- data.frame(country=countrylist,iso3=countrycode(countrylist,origin="country.name",destination="iso3c"))
for(j in 1:2){countrylist[,j]<-as.character(countrylist[,j])}
#countrylist[countrylist$country=="India ","iso3"] <- countrycode("India","country.name","iso3c")
#countrylist[countrylist$country=="india","iso3"] <- countrycode("India","country.name","iso3c")
countrylist
acled.raw <- merge(acled.raw,countrylist,by="country",all.x=T,all.y=T)
#save(acled.raw,file="Input/Events/ACLED/ACLED_Asia/acled_Asia_2015-2016.RData")
#save(acled.raw,file="Input/Events/ACLED/ACLED_Asia/acled_India_2015-2016.RData")

#############################
## Create actor dictionary
#############################

# Loop by country (first line finds k where you left off)
k0 <- max(which(countrylist$iso3%in%gsub("ACLED_|_Actors.RData","",dir("Dictionaries/ACLED/NK"))))+1
k0

for(k in 1:nrow(countrylist)){
  subdata <- acled.raw[acled.raw$country%in%c(countrylist[k,"country"]),]
  print(countrylist[k,"country"])
  actorlist <- actorList(subdata,sidea="ACTOR1",sideb="ACTOR2",timevar="styr",countryvar="country")
  save(actorlist,file=paste0("Dictionaries/ACLED/NK/ACLED_",countrylist[k,"iso3"],"_Actors.RData"))
}

