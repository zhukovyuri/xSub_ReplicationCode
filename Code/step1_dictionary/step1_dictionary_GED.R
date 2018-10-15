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

load("Input/Events/UCDP_GED/ged171.RData")

countrylist <- sort(unique(as.character(ged.raw$country)))
countrylist <- data.frame(country=countrylist,iso3=countrycode(countrylist,origin="country.name",destination="iso3c"))
for(j in 1:2){countrylist[,j]<-as.character(countrylist[,j])}
countrylist[countrylist$country=="Yemen (North Yemen)","iso3"] <- countrycode("Yemen","country.name","iso3c")
countrylist[countrylist$country=="Serbia (Yugoslavia)","iso3"] <- countrycode("Serbia","country.name","iso3c")

#############################
## Create actor dictionary
#############################

# Loop by country (first line finds k where you left off)
countrylist <- countrylist[countrylist$iso3%in%countrylist$iso3[!countrylist$iso3%in%gsub("GED_|_Actors.RData","",dir("Dictionaries/GED/Combined"))],]
k0 <- max(which(countrylist$iso3%in%gsub("GED_|_Actors.RData","",dir("Dictionaries/GED/YZ/"))))+1
k0
countrylist[k0,]

for(k in k0:nrow(countrylist)){
  subdata <- ged.raw[ged.raw$country%in%c(countrylist[k,"country"]),]
  print(countrylist[k,"country"])
  actorlist <- actorList(subdata,sidea="side_a",sideb="side_b",timevar="year",countryvar="country")
  save(actorlist,file=paste0("Dictionaries/GED/YZ/GED_",countrylist[k,"iso3"],"_Actors.RData"))
}

# # Fix
# k <- 21
# countrylist[k,]
# load(paste0("Dictionaries/GED/YZ/GED_",countrylist[k,"iso3"],"_Actors.RData"))
# actorlist$actors_OTH <- c(actorlist$actors_OTH,"Rock Machine (1995 - 1995) Mexico")
# actorlist$actors <- c(actorlist$actors,"Rock Machine (1995 - 1995) Mexico")
# save(actorlist,file=paste0("Dictionaries/GED/YZ/GED_",countrylist[k,"iso3"],"_Actors.RData"))
