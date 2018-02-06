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
## Mexico drug-related murders: 2006-2011
#############################

## Load custom functions
source("Code/functions.R")

# Load events
load("Input/Events/ESOC/Mexico/esoc_MEX_drug_related_murders_GEO.RData")
source("Code/step2_eventcode/step2x_event_types_list.R")
colnames(data)
unique(data$YEAR)
head(data)
tail(data)
dim(data) ##14736 by 13


# data$DATE <- as.Date(with(data, paste(Year, Month, Calendar.Day...End)), "%Y%m%d")
# data$DATE <- gsub("-", "", data$DATE)
# head(data)
# #save(data, file="Input/Events/ESOC/BFRS/esoc_PK_v10_GEO.RData")

# Subset
subdata <- data
head(subdata)
dim(data)

# Dates & locations
sub.datez <- as.numeric(as.character(subdata$YEAR)) #*10000+as.numeric(as.character(subdata$Month))*100+as.numeric(as.character(subdata$Calendar.Day...End))
sub.lat <- subdata$LAT
sub.long <- subdata$LONG
cnt <- "MEX"
sub0 <- data.frame(SOURCE=paste0("ESOCMexicoDrugRelatedMurders"),CONFLICT=countrycode(cnt,"iso3c","country.name"),COWN=countrycode(cnt,origin = "iso3c",destination = "cown"),COWC=countrycode(cnt,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cnt,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long)
sub0
head(sub0)

# Actors ##Responsible party -- "Other"
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
sub0$ACTION_SEL <- 1*apply(data.frame(DEATHS_TOTAL=subdata[,c("DEATHS_TOTAL")]),1,function(x){sum(x,na.rm=T)>0})
sub0$ACTION_PRT <- 0
summary(sub0)
head(subdata)

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

# Save
save(events,file=paste0("Output/Output_ESOCMexicoDrugRelatedMurders/Events/ESOCMexicoDrugRelatedMurders_Events_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData"))
head(events)
tail(events)

