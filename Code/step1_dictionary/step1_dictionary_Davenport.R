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
#Rwanda
davenport.raw <- read.csv("Input/Events/Davenport/Rwanda/genodynamics_commune_data.csv")
tail(davenport.raw)
head(davenport.raw)
colnames(davenport.raw)

#Northern Ireland
davenport.raw <- read.csv("Input/Events/Davenport/Northern Ireland/NIRI-20140211-with-georeference.csv")
tail(davenport.raw)
head(davenport.raw)
colnames(davenport.raw)

#saving
save(davenport.raw,file="Input/Events/Davenport/Northern Ireland/NOrthernIreland_20140211.RData")

#############################
## Create actor dictionary
#############################

# Loop by country (first line finds k where you left off)
subdata <- davenport.raw
actorlist <- actorList(subdata,sidea="Perpetrator.s.Organizational.Affiliation",sideb="Victim.s.Organizational.Affiliation")
save(actorlist,file=paste0("Dictionaries/Davenport/Davenport_NorthernIreland_Actors.RData"))

