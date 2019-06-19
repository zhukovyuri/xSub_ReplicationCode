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
## Iraq: Feb 4, 2004 - Feb 24, 2009
#############################

## Load custom functions
source("Code/functions.R")

# # Load events
# load("Input/Events/ESOC/SIGACT/esoc_iraq_week_district_GEO.RData")
# #df <- read.dta("Input/Events/ESOC/SIGACT/esoc-iraq-v3_sigact_district-year.dta")
# 
# # Subset
# subdata <- esoc.raw
# head(subdata)
# 
# #creating YEAR, MONTH, DATA  since they are missing from the orignal data
# subdata$YEAR <- NA
# subdata$MONTH <- NA
# subdata$DAY <- NA
# subdata$DATE <- NA
# subdata$DATE[1] <- 20040204
# dim(subdata) #27456 by 20
# subdata$DATE[27456] <- 20090224
# head(subdata)
# tail(subdata)
# subdata$WID <- subdata$week + 774
# #save(subdata, file="Input/Events/ESOC/SIGACT/ESOCIraqSIGACT_week_district_GEO.RData")

# Load data
load("Input/Events/ESOC/SIGACT/esoc_iraq_week_district_GEO.RData")

# Dates
datez <- seq(as.Date("1900-01-01"), as.Date("2016-10-24"), by="days")
ticker <-data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
ticker <- ticker[as.character(ticker$DATE)>=range(as.character(subdata$DATE),na.rm=TRUE)[1]&as.character(ticker$DATE)<=range(as.character(subdata$DATE),na.rm=TRUE)[2],]
head(ticker)
tail(ticker)
head(subdata)
tail(subdata)
d.wid <- ticker[ticker$DATE%in%min(subdata$DATE,na.rm=T),"WID"]-subdata[subdata$DATE%in%min(subdata$DATE,na.rm=T),"WID"]
subdata$WID <- subdata$WID+d.wid

# Precision codes
subdata$GEOPRECISION0 <- "adm2"
subdata$TIMEPRECISION0 <- "week"

# Dates & locations
sub.datez <- as.numeric(as.character(subdata$WID))
sub.lat <- subdata$LAT
sub.long <- subdata$LONG
sub.precis <- subdata$GEOPRECISION0
sub.tprecis <- subdata$TIMEPRECISION0
cnt <- "IRQ"
sub0 <- data.frame(SOURCE=paste0("ESOCIraqSIGACT"),CONFLICT=countrycode(cnt,"iso3c","country.name"),COWN=countrycode(cnt,origin = "iso3c",destination = "cown"),COWC=countrycode(cnt,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cnt,origin = "iso3c",destination = "iso3c"),WID=sub.datez,LAT=sub.lat,LONG=sub.long,GEOPRECISION=sub.precis,TIMEPRECISION=sub.tprecis)
head(sub0)

# Actors )
sub0$INITIATOR_SIDEA <- 0
sub0$INITIATOR_SIDEB <- 1*apply(subdata[,c("ied_attack","suicide")],1,function(x){sum(x,na.rm=T)>0})
sub0$INITIATOR_SIDEC <- 0
sub0$INITIATOR_SIDED <- 0

sub0$TARGET_SIDEA <- 1*apply(subdata[,c("ied_attack","ied_attack")],1,function(x){sum(x,na.rm=T)>0})
sub0$TARGET_SIDEB <- 0
sub0$TARGET_SIDEC <- 0
sub0$TARGET_SIDED <- 0
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

# Event Types (no need to use the dictionary, since the data contains such information already)
sub0$ACTION_ANY <- 1
sub0$ACTION_IND <- 1*apply(subdata[,c("ied_attack","idf")],1,function(x){sum(x,na.rm=T)>0})
sub0$ACTION_DIR <- 1*apply(subdata[,c("df","ied_clear")],1,function(x){sum(x,na.rm=T)>0}) #"df" column include double as was giving me an error
sub0$ACTION_PRT <- 0
head(sub0)
tail(sub0)

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
tail(events)

# EventType
source("Code/step2_eventcode/step2x_event_types_list.R")
types.specific
events0 <- as.data.frame(matrix(0,nrow=nrow(events),ncol=length(types.specific)))
names(events0) <- paste0("ACTION_",types.specific)
events0 <- cbind(data.frame(ID_TEMP=1:nrow(events)),events0)
head(events0)
head(subdata)
events0$ACTION_SUICIDE <- 1*(subdata$suicide>0) 
events0$ACTION_BOMB <- 1*(subdata$ied_attack>0)

# Save
save(events,file=paste0("Output/Output_ESOCIraqSIGACT/Events/ESOCIraqSIGACT_Events_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData"))
events0 <- events0[,names(events0)[!names(events0)%in%names(events)]]
save(events0,file=paste0("Output/Output_ESOCIraqSIGACT/Events/EventType/ESOCIraqSIGACT_EventType_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData")) 
head(events)


