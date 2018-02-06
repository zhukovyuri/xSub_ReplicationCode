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
#Guatemala
#Sullivan.raw <- read.csv("Input/Events/Sullivan/ceh-massacres.csv")
#Sullivan.raw <- read.csv("Input/Events/Sullivan/From his website/atv201.csv")
Sullivan.raw <- read.dta("Input/Events/Sullivan/Sullivan 2014 replication & appendix/guatemala data 121212.dta")
tail(Sullivan.raw)
head(Sullivan.raw)
colnames(Sullivan.raw)

#saving
save(Sullivan.raw,file="Input/Events/Sullivan/Sullivan_Guatemala121212.RData")


#############################
## Create actor dictionary
#############################

# Loop by country (first line finds k where you left off)
subdata <- Sullivan.raw
actorlist <- actorList(subdata,sidea="military",sideb="military")
save(actorlist,file=paste0("Dictionaries/Sullivan/Sullivan_Guatemala_Actors.RData"))


