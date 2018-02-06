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
## Clean up data
#############################

# Load events
#France
FerwerdaMiller.raw <- read.dta("Input/Events/FerwerdaMiller/Replication/FM_France.dta")
tail(FerwerdaMiller.raw)
head(FerwerdaMiller.raw)
colnames(FerwerdaMiller.raw)

#saving
save(FerwerdaMiller.raw,file="Input/Events/FerwerdaMiller/Replication/FM_France.RData")
#load("Input/Events/FerwerdaMiller/Replication/FM_France.RData")

#############################
## Create actor dictionary
#############################

# Loop by country (first line finds k where you left off)
subdata <- FerwerdaMiller.raw
actorlist <- actorList(subdata,sidea="Perpetrator.s.Organizational.Affiliation",sideb="Victim.s.Organizational.Affiliation")
save(actorlist,file=paste0("Dictionaries/FerwerdaMiller/FM_France_Actors.RData"))




