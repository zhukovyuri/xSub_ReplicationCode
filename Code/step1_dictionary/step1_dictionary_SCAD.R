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
scad.raw <- read.csv("Input/Events/SCAD/SCAD_Africa_32_Update.csv")
tail(scad.raw)
unique(scad.raw$countryname)


#For Latin America
scad.raw <- read.csv("Input/Events/SCAD/SCAD_Latin America_FINAL.csv")
tail(scad.raw)
unique(scad.raw$countryname)


# Clean up
names(scad.raw)[names(scad.raw)=="countryname"] <- "country"
countrylist <- sort(unique(as.character(scad.raw$country)))
countrylist <- data.frame(country=countrylist,iso3=countrycode(countrylist,origin="country.name",destination="iso3c"))
for(j in 1:2){countrylist[,j]<-as.character(countrylist[,j])}
#countrylist[countrylist$country=="Yemen (North Yemen)","iso3"] <- countrycode("Yemen","country.name","iso3c")
scad.raw <- merge(scad.raw,countrylist,by="country",all.x=T,all.y=T)
#save(scad.raw,file="Input/Events/SCAD/scad_LatinAmerica_32.RData")
#load("Input/Events/SCAD/scad_Africa_32.RData")

#############################
## Create actor dictionary
#############################

# Loop by country (first line finds k where you left off)
k0 <- max(which(countrylist$iso3%in%gsub("SCAD_|_Actors.RData","",dir("Dictionaries/SCAD/"))))+1
k0

#k <- 6

for(k in 1:nrow(countrylist)){  
  subdata <- scad.raw[scad.raw$country%in%c(countrylist[k,"country"]),]
  print(countrylist[k,"country"])
  actorlist <- actorList(subdata,sidea="actor1",sideb="target1",timevar="styr",countryvar="country")
  save(actorlist,file=paste0("Dictionaries/SCAD/SCAD_Africa/SCAD_",countrylist[k,"iso3"],"_Actors.RData"))
}


