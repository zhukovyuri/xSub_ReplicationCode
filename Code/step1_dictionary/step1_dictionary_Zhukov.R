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
#Caucasus1
zhukov.raw <- read.csv("Input/Events/Zhukov/Caucasus1/events.csv")
tail(zhukov.raw)
colnames(zhukov.raw)
zhukov.raw$INITIATOR
zhukov.raw$TARGET
##All BAND in Target and Initiator columns should be changed to REB? 

##Chechnya
load("Input/Events/Zhukov/Chechnya/CHECHNYA_events.Rdata")
ls()
unique(data$ACTOR1)
unique(data$ACTOR2)
unique(data$INITIATOR) #REB, GOV, CIV
unique(data$TARGET) # OTH, REB, GOV, CIV
##need to be coded into SIDE_A, SIDE_B, SIDE_C, SIDE_D

##Iraq
zhukov.raw <- read.csv("Input/Events/Zhukov/Iraq/IQ_GEO_v1.csv")
colnames(zhukov.raw)
head(zhukov.raw)
tail(zhukov.raw)
unique(zhukov.raw$GADM_TYPE_2)
zhukov.raw$GADM_ID_1

##Libya
zhukov.raw <- read.csv("Input/Events/Zhukov/Libya/LibyaEvents1.csv")
colnames(zhukov.raw)
head(zhukov.raw)
tail(zhukov.raw)
##In Libya, already coded SIDE_A, SIDE_B, SIDE_C, SIDE_D

##Russia
#Caucasus
load("Input/Events/Zhukov/Russia/CaucasusData.Rdata")
ls()
##looks like, already coded SIDE_A, SIDE_B, SIDE_C, SIDE_D
#EVENTS
load("Input/Events/Zhukov/Russia/EVENTS.Rdata")
ls()
head(events) ##coded already to SIDE_A, SIDE_B, SIDE_C, SIDE_D
##EventsMat
load("Input/Events/Zhukov/Russia/EventsMat.Rdata")
ls()
head(events)##coded already to SIDE_A, SIDE_B, SIDE_C, SIDE_D
##RawEvents1
load("Input/Events/Zhukov/Russia/RawEvents1.Rdata")
ls()
head(events1)##coded already to SIDE_A, SIDE_B, SIDE_C, SIDE_D

##Syria
zhukov.raw <- read.csv("Input/Events/Zhukov/Syria/SY_GEO_v1.csv")
colnames(zhukov.raw)
head(zhukov.raw)
tail(zhukov.raw)
#no information on target/initiator. 

#Turkey folder is empty

##Ukraine1
zhukov.raw <- read.csv("Input/Events/Zhukov/Ukraine1/AllEvents2.csv")
colnames(zhukov.raw)
head(zhukov.raw)
tail(zhukov.raw)
##coded already to SIDE_A, SIDE_B, SIDE_C, SIDE_D
#JCR_RepData
load("Input/Events/Zhukov/Ukraine1/JCR_RepData.RData")
ls()
head(sub.data)
##coded already to SIDE_A, SIDE_B, SIDE_C, SIDE_D

##Ukraine2
load("Input/Events/Zhukov/Ukraine2/Combined_1PD_event.RData")
ls()
head(data.event)
tail(data.event)
##coded already to SIDE_A, SIDE_B, SIDE_C, SIDE_D

##USSR
load("Input/Events/Zhukov/USSR/Memorial_Data_GeoSubset_v3.RData")
ls()
head(events)
tail(events)
##coded already to SIDE_A, SIDE_B, SIDE_C, SIDE_D

##Vietnam
load("Input/Events/Zhukov/Vietnam/VNM_merged3.RData")
ls()
colnames(data)
head(data)
tail(data)
##no information about target/initiator

##Yemen
zhukov.raw <- read.csv("Input/Events/Zhukov/Yemen/YE_GEO_v1.csv")
colnames(zhukov.raw)
head(zhukov.raw)
tail(zhukov.raw)
##no information on target/initiator










