rm(list=ls())

## Set directory
#setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
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
ged.raw <- read.csv("Input/Events/UCDP_GED/ged30.csv")

# Clean up
countrylist <- sort(unique(as.character(ged.raw$country)))
countrylist <- data.frame(country=countrylist,iso3=countrycode(countrylist,origin="country.name",destination="iso3c"))
for(j in 1:2){countrylist[,j]<-as.character(countrylist[,j])}
countrylist[countrylist$country=="Yemen (North Yemen)","iso3"] <- countrycode("Yemen","country.name","iso3c")
ged.raw <- merge(ged.raw,countrylist,by="country",all.x=T,all.y=T)
save(ged.raw,file="Input/Events/UCDP_GED/ged30.RData")

#############################
## Create actor dictionary
#############################

# Loop by country (first line finds k where you left off)
k0 <- max(which(countrylist$iso3%in%gsub("GED_|_Actors.RData","",dir("Dictionaries/GED/"))))+1
k0

for(k in 1:nrow(countrylist)){
  subdata <- ged.raw[ged.raw$country%in%c(countrylist[k,"country"]),]
  print(countrylist[k,"country"])
  actorlist <- actorList(subdata,sidea="side_a",sideb="side_b",timevar="year",countryvar="country")
  save(actorlist,file=paste0("Dictionaries/GED/GED_",countrylist[k,"iso3"],"_Actors.RData"))
}

