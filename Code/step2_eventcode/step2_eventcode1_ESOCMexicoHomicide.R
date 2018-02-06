rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


#############################
## Creat event-level data
#############################
## Mexico homicide: 1998 - 2011 (by year only)
#############################

## Load custom functions
source("Code/functions.R")

# Load events
load("Input/Events/ESOC/Mexico/esoc_MEX_homicide_GEO.RData")
source("Code/step2_eventcode/step2x_event_types_list.R")
source("Code/step2_eventcode/step_2x_eventType_function.R")
head(data)
dim(data) #34398    13
##only Year is available, no month nor day.

# Subset
subdata <- data

# Dates & locations
sub.datez <- as.numeric(as.character(subdata$YEAR)) #*10000
sub.lat <- subdata$LAT
sub.long <- subdata$LONG
cnt <- "MEX"
sub0 <- data.frame(SOURCE=paste0("ESOCMexicoHomicide"),CONFLICT=countrycode(cnt,"iso3c","country.name"),COWN=countrycode(cnt,origin = "iso3c",destination = "cown"),COWC=countrycode(cnt,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cnt,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long)
sub0
head(sub0)

# Actors (use pre-existing dictionaries)
dir("Dictionaries/ESOC")
load("Dictionaries/ESOC/ESOC_MEX_homicide_Actors.RData")

head(subdata)
sub0$INITIATOR_SIDEA <- 0
sub0$INITIATOR_SIDEB <- 0
sub0$INITIATOR_SIDEC <- 0
sub0$INITIATOR_SIDED <- 1
sub0$TARGET_SIDEA <- NA
sub0$TARGET_SIDEB <- NA
sub0$TARGET_SIDEC <- NA
sub0$TARGET_SIDED <- NA

# Actions (indiscriminate = violence vs. civilians)
sub0$ACTION_ANY <- 1
sub0$ACTION_IND <- 0
sub0$ACTION_SEL <- 1
sub0$ACTION_PRT <- 0

# Actor-action
sub0$SIDEA_ANY <- sub0$INITIATOR_SIDEA*sub0$ACTION_ANY
sub0$SIDEA_IND <- sub0$INITIATOR_SIDEA*sub0$ACTION_IND
sub0$SIDEA_SEL <- sub0$INITIATOR_SIDEA*sub0$ACTION_SEL
sub0$SIDEA_PRT <- sub0$INITIATOR_SIDEA*sub0$ACTION_PRT
sub0$SIDEB_ANY <- sub0$INITIATOR_SIDEB*sub0$ACTION_ANY
sub0$SIDEB_IND <- sub0$INITIATOR_SIDEB*sub0$ACTION_IND
sub0$SIDEB_SEL <- sub0$INITIATOR_SIDEB*sub0$ACTION_SEL
sub0$SIDEB_PRT <- sub0$INITIATOR_SIDEB*sub0$ACTION_PRT
sub0$SIDEC_ANY <- sub0$INITIATOR_SIDEC*sub0$ACTION_ANY
sub0$SIDEC_IND <- sub0$INITIATOR_SIDEC*sub0$ACTION_IND
sub0$SIDEC_SEL <- sub0$INITIATOR_SIDEC*sub0$ACTION_SEL
sub0$SIDEC_PRT <- sub0$INITIATOR_SIDEC*sub0$ACTION_PRT
sub0$SIDED_ANY <- sub0$INITIATOR_SIDED*sub0$ACTION_ANY
sub0$SIDED_IND <- sub0$INITIATOR_SIDED*sub0$ACTION_IND
sub0$SIDED_SEL <- sub0$INITIATOR_SIDED*sub0$ACTION_SEL
sub0$SIDED_PRT <- sub0$INITIATOR_SIDED*sub0$ACTION_PRT
events <- sub0
head(events)
tail(events)


# Save
save(events,file=paste0("Output/Output_ESOCMexicoHomicide/Events/ESOCMexicoHomicide_Events_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData"))
head(events)


