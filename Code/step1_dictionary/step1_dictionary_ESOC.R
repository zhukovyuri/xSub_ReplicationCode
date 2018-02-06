rm(list=ls())

## Set directory
#setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")
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
#Pakistan
esoc.raw <- read.csv("Input/Events/ESOC/BFRS/esoc_PK_v10.csv")
tail(esoc.raw)
colnames(esoc.raw)
unique(esoc.raw$Party.Responsible)
esoc.raw$country <- NA
esoc.raw$country <- "Pakistan"
head(esoc.raw)

#Mexico Homicide
library(readstata13)
esoc.raw <- read.dta13("Input/Events/ESOC/Mexico/Mexico.Data.Homicides.dta")
tail(esoc.raw)
colnames(esoc.raw)
esoc.raw$Party.Responsible <- NA
esoc.raw$Party.Responsible <- "Other"
esoc.raw$country <- NA
esoc.raw$country <- "Mexico"
head(esoc.raw)

#Mexico drug-related murders
esoc.raw <- read.dta13("Input/Events/ESOC/Mexico/Mexico.drug.related.murders.dta") 
tail(esoc.raw)
colnames(esoc.raw)
esoc.raw$Party.Responsible <- NA
esoc.raw$Party.Responsible <- "Other"
esoc.raw$country <- NA
esoc.raw$country <- "Mexico"
head(esoc.raw)

##Iraq
esoc.raw <- read.dta13("Input/Events/ESOC/SIGACT/esoc-iraq-v3_sigact_district-week.dta") 
tail(esoc.raw)
colnames(esoc.raw)
esoc.raw$Party.Responsible <- NA
esoc.raw$Party.Responsible <- "Other"
esoc.raw$country <- NA
esoc.raw$country <- "Iraq"
head(esoc.raw)

##WITS
#Afghanistan
esoc.raw <- read.dta13("Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105.dta")
tail(esoc.raw)
colnames(esoc.raw)
head(esoc.raw)
#Iraq
esoc.raw <- read.dta("Input/Events/ESOC/WITS/Iraq WITS Data/iraq_wits_attacks_v3.dta")
tail(esoc.raw)
colnames(esoc.raw)
head(esoc.raw)
textvar <- esoc.raw$detailsubject
#Pakistan
esoc.raw <- read.dta("Input/Events/ESOC/WITS/Paksitan Geo-referenced WITS Data (2004-2009)/PK_geo_wits_20100608.dta")
tail(esoc.raw) ##subject column - text with mixed actors

#saving
#save(esoc.raw,file="Input/Events/ESOC/BFRS/esoc_PK_v10.RData")
#save(esoc.raw,file="Input/Events/ESOC/Mexico/esoc_MEX_homicide.RData")
#save(esoc.raw,file="Input/Events/ESOC/SIGACT/esoc_iraq_week_district.RData")
save(esoc.raw,file="Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105.RData")
#load("Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105.RData")

#############################
## Create actor dictionary
#############################

# Loop by country (first line finds k where you left off)
subdata <- esoc.raw


actorlist <- actorList(subdata,sidea="group_name",sideb="victim_type")
save(actorlist,file=paste0("Dictionaries/ESOC/ESOC_AF_geo_wits_20101105_Actors.RData"))

load("Dictionaries/ESOC/ESOC_MEX_homicide_Actors.RData")
# ls()
# actorlist$actors_OTH




