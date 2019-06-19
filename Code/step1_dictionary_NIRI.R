rm(list=ls())


## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")



#############################
## Lod data
#############################

load("Input/Events/Davenport/Northern Ireland/NOrthernIreland_20140211.RData")

# Country code
davenport.raw$ISO3 <- countrycode("Great Britain","country.name","iso3c")
# davenport.raw$COUNTRY <- countrycode(davenport.raw$ISO3,origin="iso3c",destination="country.name")
davenport.raw$COUNTRY <- "Northern Ireland"

# # Dates
davenport.raw$YEAR <- as.numeric(as.character(davenport.raw$Year.of.Event))
davenport.raw$Year.of.Event[is.na(davenport.raw$YEAR)]

countrylist <- sort(unique(as.character(davenport.raw$ISO3)))
countrylist <- data.frame(country=countrycode(countrylist,origin="iso3c",destination="country.name"),iso3=countrylist)

#############################
## Create actor dictionary
#############################

# Loop by country (first line finds k where you left off)
# countrylist <- countrylist[countrylist$iso3%in%countrylist$iso3[!countrylist$iso3%in%gsub("NIRI_|_Actors.RData","",dir("Dictionaries/NIRI"))],]
# k0 <- max(which(countrylist$iso3%in%gsub("NIRI_|_Actors.RData","",dir("Dictionaries/NIRI/"))))+1
k0 <- 1
countrylist[k0,]

for(k in k0:nrow(countrylist)){
  subdata <- davenport.raw[davenport.raw$ISO3%in%as.character(countrylist$iso3[k]),]
  print(countrylist[k,"country"])
  actorlist <- actorList(subdata,sidea="Perpetrator.s.Organizational.Affiliation",sideb="Victim.s.Organizational.Affiliation",timevar="YEAR",countryvar="COUNTRY")
  save(actorlist,file=paste0("Dictionaries/NIRI/NIRI_",countrylist[k,"iso3"],"_Actors.RData"))
}

names(davenport.raw)
# # Fix
# k <- 21
# countrylist[k,]
# load(paste0("Dictionaries/GED/YZ/GED_",countrylist[k,"iso3"],"_Actors.RData"))
# actorlist$actors_OTH <- c(actorlist$actors_OTH,"Rock Machine (1995 - 1995) Mexico")
# actorlist$actors <- c(actorlist$actors,"Rock Machine (1995 - 1995) Mexico")
# save(actorlist,file=paste0("Dictionaries/GED/YZ/GED_",countrylist[k,"iso3"],"_Actors.RData"))
