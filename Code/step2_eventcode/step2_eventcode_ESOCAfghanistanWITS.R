rm(list=ls())


## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
# setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")

## Install & load packages (all at once)
list.of.packages <- c( "gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


#############################
## Creat event-level data
#############################
## Afghanistan
#############################

## Load custom functions
source("Code/functions.R")

# Load events
load("Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105_GEO.Rdata")
source("Code/step2_eventcode/step2x_event_types_list.R")
head(esoc.raw)
tail(esoc.raw)
colnames(esoc.raw)

# #Create the DATE column in ESOC 
# data <- esoc.raw 
# data$DATE <- as.Date(data$incident_date, format="%Y%m%d")
# data$DATE <- dmy(data$incident_date)
# data$DATE <- gsub("-", "", data$DATE)
# head(data)
#save(data, file="Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105.RData")

# Subset
data <- esoc.raw

# Precision codes
head(data)
sort(unique(data$WHERE_PREC))
data$GEOPRECISION0 <- "settlement"
data$GEOPRECISION0[grep("district|District",data$category)] <- "adm2"
data$GEOPRECISION0[grep("provinc|Provinc",data$category)] <- "adm1"
data$TIMEPRECISION0 <- "day"
tail(data)

# Subset
subdata <- data[data$iso3%in%"AFG",]
head(subdata)
dim(subdata) #7846 by 102

# Dates & locations
sub.datez <- as.numeric(as.character(subdata$DATE)) #*10000+as.numeric(as.character(subdata$Month))*100+as.numeric(as.character(subdata$Calendar.Day...End))
sub.lat <- subdata$LAT
sub.long <- subdata$LONG
sub.precis <- subdata$GEOPRECISION0
sub.tprecis <- subdata$TIMEPRECISION0
cnt <- "AFG"
sub0 <- data.frame(SOURCE=paste0("ESOCAfghanistanWITS"),CONFLICT=countrycode(cnt,"iso3c","country.name"),COWN=countrycode(cnt,origin = "iso3c",destination = "cown"),COWC=countrycode(cnt,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cnt,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long,GEOPRECISION=sub.precis,TIMEPRECISION=sub.tprecis)
head(sub0)

# Actors (use pre-existing dictionaries)
# Load data
data.raw <- read.dta("Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105.dta")
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

# Dyads 
sub0$DYAD_A_A <- sub0$INITIATOR_SIDEA*sub0$TARGET_SIDEA
sub0$DYAD_A_B <- sub0$INITIATOR_SIDEA*sub0$TARGET_SIDEB
sub0$DYAD_A_C <- sub0$INITIATOR_SIDEA*sub0$TARGET_SIDEC
sub0$DYAD_A_D <- sub0$INITIATOR_SIDEA*sub0$TARGET_SIDED
sub0$DYAD_B_A <- sub0$INITIATOR_SIDEB*sub0$TARGET_SIDEA
sub0$DYAD_B_B <- sub0$INITIATOR_SIDEB*sub0$TARGET_SIDEB
sub0$DYAD_B_C <- sub0$INITIATOR_SIDEB*sub0$TARGET_SIDEC
sub0$DYAD_B_D <- sub0$INITIATOR_SIDEB*sub0$TARGET_SIDED
sub0$DYAD_C_A <- sub0$INITIATOR_SIDEC*sub0$TARGET_SIDEA
sub0$DYAD_C_B <- sub0$INITIATOR_SIDEC*sub0$TARGET_SIDEB
sub0$DYAD_C_C <- sub0$INITIATOR_SIDEC*sub0$TARGET_SIDEC
sub0$DYAD_C_D <- sub0$INITIATOR_SIDEC*sub0$TARGET_SIDED
sub0$DYAD_D_A <- sub0$INITIATOR_SIDED*sub0$TARGET_SIDEA
sub0$DYAD_D_B <- sub0$INITIATOR_SIDED*sub0$TARGET_SIDEB
sub0$DYAD_D_C <- sub0$INITIATOR_SIDED*sub0$TARGET_SIDEC
sub0$DYAD_D_D <- sub0$INITIATOR_SIDED*sub0$TARGET_SIDED

# Event Types (use dictionary)
dir("Dictionaries/EventTypes/ESOC")
load("Dictionaries/EventTypes/Combined_EventDictionary.RData")
head(term.type)
source("Code/step2_eventcode/step2x_event_types_list.R")
source("Code/step2_eventcode/step2x_eventType_function.R")
subdata$ID_TEMP <- 1:nrow(subdata)
textvar <- "summary"
idvar <- "ID_TEMP"
events0 <- eventType(subdata=subdata,idvar=idvar,textvar=textvar,term.type=term.type,types.specific=types.specific,types.general=types.general)
summary(events0)
head(events0)
head(subdata)

sort(unique(subdata$weapon))

# Actions
sub0$ACTION_ANY <- events0$ACTION_ANY
sub0$ACTION_IND <- events0$ACTION_IND
sub0$ACTION_DIR <- events0$ACTION_DIR
sub0$ACTION_PRT <- events0$ACTION_PRT

# Actor-action
sub0$SIDEA_ANY <- sub0$INITIATOR_SIDEA*sub0$ACTION_ANY
sub0$SIDEA_IND <- sub0$INITIATOR_SIDEA*sub0$ACTION_IND
sub0$SIDEA_DIR <- sub0$INITIATOR_SIDEA*sub0$ACTION_DIR
sub0$SIDEA_PRT <- sub0$INITIATOR_SIDEA*sub0$ACTION_PRT
sub0$SIDEB_ANY <- sub0$INITIATOR_SIDEB*sub0$ACTION_ANY
sub0$SIDEB_IND <- sub0$INITIATOR_SIDEB*sub0$ACTION_IND
sub0$SIDEB_DIR <- sub0$INITIATOR_SIDEB*sub0$ACTION_DIR
sub0$SIDEB_PRT <- sub0$INITIATOR_SIDEB*sub0$ACTION_PRT
sub0$SIDEC_ANY <- sub0$INITIATOR_SIDEC*sub0$ACTION_ANY
sub0$SIDEC_IND <- sub0$INITIATOR_SIDEC*sub0$ACTION_IND
sub0$SIDEC_DIR <- sub0$INITIATOR_SIDEC*sub0$ACTION_DIR
sub0$SIDEC_PRT <- sub0$INITIATOR_SIDEC*sub0$ACTION_PRT
sub0$SIDED_ANY <- sub0$INITIATOR_SIDED*sub0$ACTION_ANY
sub0$SIDED_IND <- sub0$INITIATOR_SIDED*sub0$ACTION_IND
sub0$SIDED_DIR <- sub0$INITIATOR_SIDED*sub0$ACTION_DIR
sub0$SIDED_PRT <- sub0$INITIATOR_SIDED*sub0$ACTION_PRT
events <- sub0
head(events)

# Save
save(events,file=paste0("Output/Output_ESOCAfghanistanWITS/Events/ESOCAfghanistanWITS_Events_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData"))
events0 <- events0[,names(events0)[!names(events0)%in%names(events)]]
save(events0,file=paste0("Output/Output_ESOCAfghanistanWITS/Events/EventType/ESOCAfghanistanWITS_EventType_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData")) 
head(events)


