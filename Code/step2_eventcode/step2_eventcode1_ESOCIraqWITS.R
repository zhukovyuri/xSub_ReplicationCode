rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("lubridate", "gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


#############################
## Creat event-level data
#############################
## Iraq
#############################

## Load custom functions
source("Code/functions.R")

# Load events
load("Input/Events/ESOC/WITS/Iraq WITS Data/Iraq_WITS_GEO.RData")
source("Code/step2_eventcode/step2x_event_types_list.R")
head(data)
tail(data)
colnames(data)

#Create the DATE column in ESOC
# data <- esoc.raw
# data$DATE <- dmy(data$detailincidentdate)
# data$DATE <- gsub("-", "", data$DATE)
# head(data)
#save(data, file="Input/Events/ESOC/WITS/Iraq WITS Data/Iraq_WITS_GEO.RData")
#load("Input/Events/ESOC/WITS/Iraq WITS Data/Iraq_WITS_GEO.RData")

# Subset
subdata <- data
head(subdata)
dim(subdata) #25490 by 38

# Dates & locations
sub.datez <- as.numeric(as.character(subdata$DATE)) #*10000+as.numeric(as.character(subdata$Month))*100+as.numeric(as.character(subdata$Calendar.Day...End))
sub.lat <- subdata$LAT
sub.long <- subdata$LONG
cnt <- "IRQ"
sub0 <- data.frame(SOURCE=paste0("ESOCIraqWITS"),CONFLICT=countrycode(cnt,"iso3c","country.name"),COWN=countrycode(cnt,origin = "iso3c",destination = "cown"),COWC=countrycode(cnt,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cnt,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long)
sub0
head(sub0)

# Actors (use pre-existing dictionaries)
# Load data
data.raw <- read.dta("Input/Events/ESOC/WITS/Iraq WITS Data/iraq_wits_attacks_v3.dta")
head(data.raw)
textvar <- "subject"


# Load actor dictionary
load("Dictionaries/ActorTypes/Combined_ActorDictionary.RData")

# Run function
source("Code/step1_dictionary/step1x_actorType_function.R")
actors <- actorType(data.raw,textvar,actor.type=actor.type)
head(actors)

sub0$INITIATOR_SIDEA <- 0
sub0$INITIATOR_SIDEB <- 1
sub0$INITIATOR_SIDEC <- 0
sub0$INITIATOR_SIDED <- 0

sub0$TARGET_SIDEA <- actors$SIDEA
sub0$TARGET_SIDEB <- actors$SIDEB
sub0$TARGET_SIDEC <- actors$SIDEC
sub0$TARGET_SIDED <- actors$SIDED

# Event Types
dir("Dictionaries/EventTypes/ESOC")
load("Dictionaries/EventTypes/Combined_EventDictionary.RData")
head(subdata)
source("Code/step2_eventcode/step2x_event_types_list.R")
source("Code/step2_eventcode/step_2x_eventType_function.R")
subdata$ID_TEMP <- 1:nrow(subdata)
textvar <- "detailsummary"
idvar <- "ID_TEMP"
events0 <- eventType(subdata=subdata,idvar=idvar,textvar=textvar,term.type=term.type,types.specific=types.specific,types.general=types.general)
summary(events0)
head(events0)
subdata$weapon <- gdata:::trim(subdata$weapon)

sort(unique(subdata$weapon))

# Actions (indiscriminate = violence vs. civilians)
sub0$ACTION_ANY <- events0$ACTION_ANY
sub0$ACTION_IND <- events0$ACTION_IND
sub0$ACTION_SEL <- events0$ACTION_SEL
sub0$ACTION_PRT <- events0$ACTION_PRT


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
save(events,file=paste0("Output/Output_ESOCIraqWITS/Events/ESOCIraqWITS_Events_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData"))
summary(events)


