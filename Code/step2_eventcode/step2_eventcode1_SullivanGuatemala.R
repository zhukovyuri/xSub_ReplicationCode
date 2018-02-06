rm(list=ls())

## Set directory
#setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")
setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)



#############################
## Creat event-level data
#############################

## Load custom functions
source("Code/functions.R")

# Load events
load("Input/Events/Sullivan/ceh-massacres_GEO.RData")
data <- Sullivan.raw
summary(data)
head(data)

#Create the DATE column in the SUllivan data
data <- data[complete.cases(data[,1:2]),]
head(data)
tail(data)
dim(data)
#dropped 206 observations because of NAs in day and month columns

data$DATE <- as.Date(with(data, paste(year, month, day)), "%Y%m%d")
data$DATE <- gsub("-", "", data$DATE)
head(data)
#save(Sullivan.raw, file="Input/Events/Sullivan/ceh-massacres_GEO.RData")

# Subset
subdata <- data
head(subdata)
dim(data)

# Dates & locations
sub.datez <- as.numeric(as.character(subdata$year))*10000+as.numeric(as.character(subdata$month))*100+as.numeric(as.character(subdata$day))
sub.lat <- subdata$LONG
sub.long <- subdata$LAT
cnt <- "GTM"
sub0 <- data.frame(SOURCE=paste0("SullivanGuatemala"),CONFLICT=countrycode(cnt,"iso3c","country.name"),COWN=countrycode(cnt,origin = "iso3c",destination = "cown"),COWC=countrycode(cnt,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cnt,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long)
sub0
head(sub0)

# Actors (recode)
names(subdata)
#This portion just for this data (NK)
subdata$INITIATOR_G <- 0
subdata$INITIATOR_R <- 0
subdata$INITIATOR_C <- 0
subdata$INITIATOR_D <- 0
subdata$INITIATOR_G[which(subdata$military=="Y")] <- 1
subdata$INITIATOR_R[which(subdata$military=="Mil and Pac")] <- 1


subdata$ZERO <- 0
sub0$INITIATOR_SIDEA <- 1*apply(subdata[,c("INITIATOR_G","ZERO")],1,function(x){sum(x,na.rm=T)>0})
sub0$INITIATOR_SIDEB <- 1*apply(subdata[,c("INITIATOR_R","ZERO")],1,function(x){sum(x,na.rm=T)>0})
sub0$INITIATOR_SIDEC <- 0
sub0$INITIATOR_SIDED <- 0

sub0$TARGET_SIDEA <- NA
sub0$TARGET_SIDEB <- NA
sub0$TARGET_SIDEC <- NA
sub0$TARGET_SIDED <- NA
sub0$TARGET_SIDEX <- NA
head(sub0)
tail(sub0)

# Actions (indiscriminate = violence vs. civilians)
#names(subdata)
sub0$ACTION_ANY <- 1
sub0$ACTION_IND <- NA
sub0$ACTION_SEL <- NA
sub0$ACTION_PRT <- NA
head(sub0)
tail(sub0)

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


# Save
save(events,file=paste0("Output/Output_SullivanGuatemala/Events/SullivanGuatemala_Events_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData"))
summary(events)



