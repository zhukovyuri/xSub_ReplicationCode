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
data <- read.csv("Input/Events/Zhukov/Libya/LibyaEvents1.csv")
sort(unique(data$TYPE))
head(data)

# Precision codes
head(data)
data$GEOPRECISION0 <- "settlement"
data$GEOPRECISION0[which((data$CITY==""|is.na(data$CITY)))] <- "adm2"
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
cnt <- "LBY"
sub0 <- data.frame(SOURCE=paste0("yzLibya"),CONFLICT=countrycode(cnt,"iso3c","country.name"),COWN=countrycode(cnt,origin = "iso3c",destination = "cown"),COWC=countrycode(cnt,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cnt,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long,GEOPRECISION=sub.precis,TIMEPRECISION=sub.tprecis)
head(sub0)

# Actors (recode)
names(subdata)

subdata$ZERO <- 0
sub0$INITIATOR_SIDEA <- 1*apply(subdata[,c("IRRG_GOV","CONV_GOV")],1,function(x){sum(x,na.rm=T)>0})
sub0$INITIATOR_SIDEB <- 1*apply(subdata[,c("IRRG_INS","CONV_INS")],1,function(x){sum(x,na.rm=T)>0})
sub0$INITIATOR_SIDEC <- 0
sub0$INITIATOR_SIDED <- 1*apply(subdata[,c("NATO_STRIKE","ZERO")],1,function(x){sum(x,na.rm=T)>0})

sub0$TARGET_SIDEA <- 1*apply(subdata[,c("KIA_GOV","WIA_GOV")],1,function(x){sum(x,na.rm=T)>0})
sub0$TARGET_SIDEB <- 1*apply(subdata[,c("KIA_INS","WIA_INS")],1,function(x){sum(x,na.rm=T)>0})
sub0$TARGET_SIDEC <- 1*apply(subdata[,c("KIA_CIVbyI","WIA_CIVbyI","KIA_CIVbyG","WIA_CIVbyG")],1,function(x){sum(x,na.rm=T)>0})
sub0$TARGET_SIDED <- 0
head(sub0)

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
names(subdata)
sub0$ACTION_ANY <- 1
sub0$ACTION_IND <- 1*apply(subdata[,c("CONV_INS","CONV_GOV","NATO_STRIKE")],1,function(x){sum(x,na.rm=T)>0})
sub0$ACTION_DIR <- 1*apply(subdata[,c("IRRG_INS","IRRG_GOV")],1,function(x){sum(x,na.rm=T)>0})
sub0$ACTION_PRT <- 1*apply(subdata[,c("PROTEST","ZERO")],1,function(x){sum(x,na.rm=T)>0})

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

# EventType
source("Code/step2_eventcode/step2x_event_types_list.R")
types.specific
events0 <- as.data.frame(matrix(0,nrow=nrow(events),ncol=length(types.specific)))
names(events0) <- paste0("ACTION_",types.specific)
events0 <- cbind(data.frame(ID_TEMP=1:nrow(events)),events0)
names(events0)
names(subdata)
events0$ACTION_ARREST <- 1*(subdata$ARREST>0) 
events0$ACTION_PROTEST <- 1*(subdata$PROTEST>0)
events0$ACTION_FIREFIGHT <- 1*(subdata$IRRG_INS>0|subdata$IRRG_GOV>0)
events0$ACTION_ARTILLERY <- 1*(subdata$CONV_GOV>0|subdata$CONV_INS>0)
events0$ACTION_CIV_ABUSE <- 1*(subdata$KIA_CIVbyG>0|subdata$KIA_CIVbyI>0|subdata$WIA_CIVbyG>0|subdata$WIA_CIVbyI>0)
head(events0)

# Save
save(events,file=paste0("Output/Output_yzLibya/Events/yzLibya_Events_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData"))
events0 <- events0[,names(events0)[!names(events0)%in%names(events)]]
save(events0,file=paste0("Output/Output_yzLibya/Events/EventType/yzLibya_EventType_",cnt,".RData")) 

head(events)


