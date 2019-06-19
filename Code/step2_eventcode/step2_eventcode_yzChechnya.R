rm(list=ls())


## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
# setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")

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
load("Input/Events/Zhukov/Chechnya/CHECHNYA_Events.RData")
sort(unique(data$TYPE))
head(data)

## Load event type dictionary
load("Dictionaries/EventTypes/Combined_EventDictionary.RData")
source("Code/step2_eventcode/step2x_event_types_list.R")
source("Code/step2_eventcode/step2x_eventType_function.R")

## Load locations
cities <- read.csv("Input/Events/Zhukov/Chechnya/Cities.csv")
cities <- cities[,c("CID","LONG","LAT")]
head(cities)
data <- merge(data,cities,by="CID",all.x=T,all.y=F)
plot(data$LONG,data$LAT)
rm(cities)

# Precision codes
head(data)
data$GEOPRECISION0 <- "settlement"
data$GEOPRECISION0[which((data$NAME==""|is.na(data$NAME)))] <- "adm2"
data$TIMEPRECISION0 <- "day"
summary(data)

# Subset
subdata <- data
head(subdata)

# Dates & locations
sub.datez <- as.numeric(as.character(subdata$YEAR))*10000+as.numeric(as.character(subdata$MONTH))*100+as.numeric(as.character(subdata$DAY))
sub.lat <- subdata$LAT
sub.long <- subdata$LONG
sub.precis <- subdata$GEOPRECISION0
sub.tprecis <- subdata$TIMEPRECISION0
cnt <- "RUS"
sub0 <- data.frame(SOURCE=paste0("yzChechnya"),CONFLICT=countrycode(cnt,"iso3c","country.name"),COWN=countrycode(cnt,origin = "iso3c",destination = "cown"),COWC=countrycode(cnt,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cnt,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long,GEOPRECISION=sub.precis,TIMEPRECISION=sub.tprecis)

# Actors (use pre-existing dictionaries)
dir("Dictionaries/yzChechnya")
if(paste0("yzChechnya_Actors.RData")%in%dir("Dictionaries/yzChechnya")){load(paste0("Dictionaries/yzChechnya/yzChechnya_Actors.RData"))}

if(length(actorlist$actors_GOV)>0){actorlist$actors_GOV <- trim(sapply(strsplit(actorlist$actors_GOV,split="\\(\\d{4}|\\(Inf"), '[', 1))}
if(length(actorlist$actors_REB)>0){actorlist$actors_REB <- trim(sapply(strsplit(actorlist$actors_REB,split="\\(\\d{4}|\\(Inf"), '[', 1))}
if(length(actorlist$actors_CIV)>0){actorlist$actors_CIV <- trim(sapply(strsplit(actorlist$actors_CIV,split="\\(\\d{4}|\\(Inf"), '[', 1))}
if(length(actorlist$actors_OTH)>0){actorlist$actors_OTH <- trim(sapply(strsplit(actorlist$actors_OTH,split="\\(\\d{4}|\\(Inf"), '[', 1))}

actor.var <- "ACTOR1"
sub0$INITIATOR_SIDEA <- 1*(subdata[,actor.var]%in%actorlist$actors_GOV)
sub0$INITIATOR_SIDEB <- 1*(subdata[,actor.var]%in%actorlist$actors_REB)
sub0$INITIATOR_SIDEC <- 1*(subdata[,actor.var]%in%actorlist$actors_CIV)
sub0$INITIATOR_SIDED <- 1*((subdata[,actor.var]%in%actorlist$actors_OTH)|(subdata[,actor.var]%in%actorlist$actors&(!subdata[,actor.var]%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV))))
actor.var <- "ACTOR2"
sub0$TARGET_SIDEA <- 1*(subdata[,actor.var]%in%actorlist$actors_GOV)
sub0$TARGET_SIDEB <- 1*(subdata[,actor.var]%in%actorlist$actors_REB)
sub0$TARGET_SIDEC <- 1*(subdata[,actor.var]%in%actorlist$actors_CIV)
sub0$TARGET_SIDED <- 1*((subdata[,actor.var]%in%actorlist$actors_OTH)|(subdata[,actor.var]%in%actorlist$actors&(!subdata[,actor.var]%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV))))
tail(sub0)

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
head(subdata)
subdata$ID_TEMP <- 1:nrow(subdata)
subdata$TEXT <- subdata$TYPE
subdata$TEXT <- iconv(subdata$TEXT,"WINDOWS-1252","UTF-8")
subdata$TEXT <- tolower(subdata$TEXT)
textvar <- "TEXT"
idvar <- "ID_TEMP"
length(unique(subdata[,idvar]))==nrow(subdata)
events0 <- eventType(subdata=subdata,idvar=idvar,textvar=textvar,term.type=term.type,types.specific=types.specific,types.general=types.general)
summary(events0)

# Actions (indiscriminate = violence vs. civilians)
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

# Save
save(events,file=paste0("Output/Output_yzChechnya/Events/yzChechnya_Events_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData"))
events0 <- events0[,names(events0)[!names(events0)%in%names(events)]]
save(events0,file=paste0("Output/Output_yzChechnya/Events/EventType/yzChechnya_EventType_",cnt,".RData")) 
head(events)


