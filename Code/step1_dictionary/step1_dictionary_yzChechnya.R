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
yz.raw <- load("Input/Events/Zhukov/Chechnya/CHECHNYA_Events.RData")
yz.raw <- get(yz.raw); rm(data)
unique(yz.raw$ACTOR1)
unique(yz.raw$ACTOR2)


#############################
## Create actor dictionary
#############################

# Loop by country (first line finds k where you left off)
subdata <- yz.raw
actorlist <- actorList(subdata,sidea="ACTOR1",sideb="ACTOR2")
save(actorlist,file=paste0("Dictionaries/YZ_Chechnya/YZ_Chechnya_Actors.RData"))

