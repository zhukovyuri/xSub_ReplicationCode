rm(list=ls())


## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("lubricate", "gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


#############################
## Creat event-level data
#############################
## FM: France: December 1942 to September 1944
#############################

## Load custom functions
source("Code/functions.R")

# # Load events

# load("Input/Events/FerwerdaMiller/Replication/FM_GEO.RData")
# source("Code/step2_eventcode/step2x_event_types_list.R")
# head(FerwerdaMiller.raw)
# tail(FerwerdaMiller.raw)
# colnames(FerwerdaMiller.raw)
# dim(FerwerdaMiller.raw) ##1429 by 28
# 
# #Create the DATE column in ESOC
# data <- FerwerdaMiller.raw
# data$DATE <- NA
# data$DATE[1] <- 19421201
# data$DATE[1429] <- 19440930
# head(data)
# tail(data)
# #save(data, file="Input/Events/FerwerdaMiller/Replication/FM_GEO.RData")

load("Input/Events/FerwerdaMiller/Replication/FM_France_GEO.RData")

# Subset
subdata <- data

# Dates missing: set random dates for events (as placeholder)
date.vec <- gsub("-","",seq(as.Date("1942-12-01"),as.Date("1944-09-30"),by="day"))
subdata$DATE <- sample(date.vec,nrow(subdata),replace=TRUE)
head(subdata)
dim(subdata) #1429 by 29
head(subdata)

# Dates & locations
sub.datez <- as.numeric(as.character(subdata$DATE)) #*10000+as.numeric(as.character(subdata$Month))*100+as.numeric(as.character(subdata$Calendar.Day...End))
sub.lat <- subdata$LAT
sub.long <- subdata$LONG
cnt <- "FRA"
sub0 <- data.frame(SOURCE=paste0("FM"),CONFLICT=countrycode(cnt,"iso3c","country.name"),COWN=countrycode(cnt,origin = "iso3c",destination = "cown"),COWC=countrycode(cnt,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cnt,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long)
sub0
head(sub0)

# Actors (based on the article)
sub0$INITIATOR_SIDEA <- 0
sub0$INITIATOR_SIDEB <- 1
sub0$INITIATOR_SIDEC <- 0
sub0$INITIATOR_SIDED <- 0

sub0$TARGET_SIDEA <- 0
sub0$TARGET_SIDEB <- 1
sub0$TARGET_SIDEC <- 0
sub0$TARGET_SIDED <- 0

# Actions (indiscriminate = violence vs. civilians)
sub0$ACTION_ANY <- 1
sub0$ACTION_IND <- NA
sub0$ACTION_SEL <- NA
sub0$ACTION_PRT <- NA

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
save(events,file=paste0("Output/Output_FM/Events/FM_Events_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData"))
head(events)


