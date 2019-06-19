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

# Precision codes
head(data)
data$GEOPRECISION0 <- "settlement"
data$TIMEPRECISION0 <- "year"
tail(data)

# Subset
subdata <- data
head(subdata)
dim(data)

# Dates & locations
sub.datez <- as.numeric(as.character(subdata$YEAR)) #*10000+as.numeric(as.character(subdata$Month))*100+as.numeric(as.character(subdata$Calendar.Day...End))
sub.lat <- subdata$LAT
sub.long <- subdata$LONG
sub.precis <- subdata$GEOPRECISION0
sub.tprecis <- subdata$TIMEPRECISION0
cnt <- "MEX"
sub0 <- data.frame(SOURCE=paste0("ESOCMexicoDrugRelatedMurders"),CONFLICT=countrycode(cnt,"iso3c","country.name"),COWN=countrycode(cnt,origin = "iso3c",destination = "cown"),COWC=countrycode(cnt,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cnt,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long,GEOPRECISION=sub.precis,TIMEPRECISION=sub.tprecis)
sub0
head(sub0)

# Actors ##Responsible party -- "Other"
sub0$INITIATOR_SIDEA <- 0
sub0$INITIATOR_SIDEB <- 0
sub0$INITIATOR_SIDEC <- 0
sub0$INITIATOR_SIDED <- 1
sub0$TARGET_SIDEA <- 0
sub0$TARGET_SIDEB <- 0
sub0$TARGET_SIDEC <- 0
sub0$TARGET_SIDED <- 1

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

# Actions (indiscriminate = violence vs. civilians)
sub0$ACTION_ANY <- 1
sub0$ACTION_IND <- 0
sub0$ACTION_DIR <- 1*apply(data.frame(DEATHS_TOTAL=subdata[,c("DEATHS_TOTAL")]),1,function(x){sum(x,na.rm=T)>0})
sub0$ACTION_PRT <- 0
summary(sub0)
head(subdata)

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

# EventType
source("Code/step2_eventcode/step2x_event_types_list.R")
types.specific
events0 <- as.data.frame(matrix(0,nrow=nrow(events),ncol=length(types.specific)))
names(events0) <- paste0("ACTION_",types.specific)
events0 <- cbind(data.frame(ID_TEMP=1:nrow(events)),events0)
head(events0)
head(subdata)
events0$ACTION_KILLING <- 1*(sub0$ACTION_DIR>0) 


# Save
save(events,file=paste0("Output/Output_ESOCMexicoDrugRelatedMurders/Events/ESOCMexicoDrugRelatedMurders_Events_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData"))
events0 <- events0[,names(events0)[!names(events0)%in%names(events)]]
save(events0,file=paste0("Output/Output_ESOCMexicoDrugRelatedMurders/Events/EventType/ESOCMexicoDrugRelatedMurders_EventType_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData")) 
head(events)
tail(events)

