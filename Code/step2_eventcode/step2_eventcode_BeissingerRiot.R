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
## Create event-level data
#############################

## Load custom functions
rm(list=ls())
source("Code/functions.R")

load("Input/Events/Beissinger/Soviet Union/Riot_GEO.RData")

# data <- beissinger.raw
data[data$REPUBLIC == "Moldova", ][, "REPUBLIC_RENAME"] <- "Moldova"
tail(data)

# Precision codes
names(data)
which(is.na(data$PLACENAME)|data$PLACENAME=="")
sort(unique(data$PLACENAME))
data$GEOPRECISION0 <- "settlement"
data$TIMEPRECISION0 <- "day"
tail(data)

# Country codes
data$ISO3 <- countrycode(data$REPUBLIC_RENAME,origin = "country.name",destination = "iso3c")
summary(as.factor(data$ISO3))
head(data)
tail(data)

## By country
disag <- sort(unique(data$ISO3))
j <- 15; disag[j]
beissinger.list <- lapply(1:length(disag),function(j){print(j)
  subdata <- data[data$ISO3==disag[j],]
  head(subdata)
  cntz <- disag[j]
  
  # Dates & locations
  sub.datez <- as.numeric(as.character(subdata$DATE)) #*10000+as.numeric(as.character(subdata$Month))*100+as.numeric(as.character(subdata$Calendar.Day...End))
  sub.lat <- subdata$LAT
  sub.long <- subdata$LONG
  sub.precis <- subdata$GEOPRECISION0
  sub.tprecis <- subdata$TIMEPRECISION0
  sub0 <- data.frame(SOURCE=paste0("BeissingerRiot"),CONFLICT=countrycode(cntz,"iso3c","country.name"),COWN=countrycode(cntz,origin = "iso3c",destination = "cown"),COWC=countrycode(cntz,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cntz,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long,GEOPRECISION=sub.precis,TIMEPRECISION=sub.tprecis)
  sub0
  head(sub0)
  
  # Extract government violence events
  # subdata_GOV <- subdata[subdata$ARRESTS%in%c("Y"),]
  # sub0_GOV <- sub0[subdata$ARRESTS%in%c("Y"),]
  subdata_GOV <- subdata[subdata$ARRESTNUM>0,]
  sub0_GOV <- sub0[subdata$ARRESTNUM>0,]
  
  # Protest events
  sub0$INITIATOR_SIDEA <- 0
  sub0$INITIATOR_SIDEB <- 1
  sub0$INITIATOR_SIDEC <- 0
  sub0$INITIATOR_SIDED <- 0
  sub0$TARGET_SIDEA <- 1*(subdata$NATTARGET=="Y"|subdata$LOC1TARGET=="L"|subdata$LOC1TARGET=="Y"|subdata$LOC1TARGET=="R"|subdata$LOC2TARGET=="Y")
  sub0$TARGET_SIDEB <- 0
  sub0$TARGET_SIDEC <- 1*(subdata$ETHTARGET=="Y"|subdata$ENTTARGET=="Y"|subdata$OTHTARGET=="Y")
  sub0$TARGET_SIDED <- 0
  
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
  
  # Action
  sub0$ACTION_ANY <- 1
  sub0$ACTION_IND <- 1*(subdata$WEAPONLEV>=4)
  sub0$ACTION_IND[is.na(sub0$ACTION_IND)] <- 0
  sub0$ACTION_DIR <- 1*(subdata$WEAPONLEV>0&subdata$WEAPONLEV<4)
  sub0$ACTION_DIR[is.na(sub0$ACTION_DIR)] <- 0
  sub0$ACTION_PRT <- 1
  
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
  if(nrow(sub0_GOV)>0){
    # Government events
    sub0_GOV$INITIATOR_SIDEA <- 1
    sub0_GOV$INITIATOR_SIDEB <- 0
    sub0_GOV$INITIATOR_SIDEC <- 0
    sub0_GOV$INITIATOR_SIDED <- 0
    
    sub0_GOV$TARGET_SIDEA <- 0
    sub0_GOV$TARGET_SIDEB <- 1
    sub0_GOV$TARGET_SIDEC <- 0
    sub0_GOV$TARGET_SIDED <- 0
    
    # Dyads 
    sub0_GOV$DYAD_A_A <- sub0_GOV$INITIATOR_SIDEA*sub0_GOV$TARGET_SIDEA
    sub0_GOV$DYAD_A_B <- sub0_GOV$INITIATOR_SIDEA*sub0_GOV$TARGET_SIDEB
    sub0_GOV$DYAD_A_C <- sub0_GOV$INITIATOR_SIDEA*sub0_GOV$TARGET_SIDEC
    sub0_GOV$DYAD_A_D <- sub0_GOV$INITIATOR_SIDEA*sub0_GOV$TARGET_SIDED
    sub0_GOV$DYAD_B_A <- sub0_GOV$INITIATOR_SIDEB*sub0_GOV$TARGET_SIDEA
    sub0_GOV$DYAD_B_B <- sub0_GOV$INITIATOR_SIDEB*sub0_GOV$TARGET_SIDEB
    sub0_GOV$DYAD_B_C <- sub0_GOV$INITIATOR_SIDEB*sub0_GOV$TARGET_SIDEC
    sub0_GOV$DYAD_B_D <- sub0_GOV$INITIATOR_SIDEB*sub0_GOV$TARGET_SIDED
    sub0_GOV$DYAD_C_A <- sub0_GOV$INITIATOR_SIDEC*sub0_GOV$TARGET_SIDEA
    sub0_GOV$DYAD_C_B <- sub0_GOV$INITIATOR_SIDEC*sub0_GOV$TARGET_SIDEB
    sub0_GOV$DYAD_C_C <- sub0_GOV$INITIATOR_SIDEC*sub0_GOV$TARGET_SIDEC
    sub0_GOV$DYAD_C_D <- sub0_GOV$INITIATOR_SIDEC*sub0_GOV$TARGET_SIDED
    sub0_GOV$DYAD_D_A <- sub0_GOV$INITIATOR_SIDED*sub0_GOV$TARGET_SIDEA
    sub0_GOV$DYAD_D_B <- sub0_GOV$INITIATOR_SIDED*sub0_GOV$TARGET_SIDEB
    sub0_GOV$DYAD_D_C <- sub0_GOV$INITIATOR_SIDED*sub0_GOV$TARGET_SIDEC
    sub0_GOV$DYAD_D_D <- sub0_GOV$INITIATOR_SIDED*sub0_GOV$TARGET_SIDED
    
    # Actions (indiscriminate = violence vs. civilians)
    sub0_GOV$ACTION_ANY <- 1
    sub0_GOV$ACTION_IND <- 0
    sub0_GOV$ACTION_DIR <- 1
    sub0_GOV$ACTION_PRT <- 0
    
    # Actor-action
    sub0_GOV$SIDEA_ANY <- sub0_GOV$INITIATOR_SIDEA*sub0_GOV$ACTION_ANY
    sub0_GOV$SIDEA_IND <- sub0_GOV$INITIATOR_SIDEA*sub0_GOV$ACTION_IND
    sub0_GOV$SIDEA_DIR <- sub0_GOV$INITIATOR_SIDEA*sub0_GOV$ACTION_DIR
    sub0_GOV$SIDEA_PRT <- sub0_GOV$INITIATOR_SIDEA*sub0_GOV$ACTION_PRT
    sub0_GOV$SIDEB_ANY <- sub0_GOV$INITIATOR_SIDEB*sub0_GOV$ACTION_ANY
    sub0_GOV$SIDEB_IND <- sub0_GOV$INITIATOR_SIDEB*sub0_GOV$ACTION_IND
    sub0_GOV$SIDEB_DIR <- sub0_GOV$INITIATOR_SIDEB*sub0_GOV$ACTION_DIR
    sub0_GOV$SIDEB_PRT <- sub0_GOV$INITIATOR_SIDEB*sub0_GOV$ACTION_PRT
    sub0_GOV$SIDEC_ANY <- sub0_GOV$INITIATOR_SIDEC*sub0_GOV$ACTION_ANY
    sub0_GOV$SIDEC_IND <- sub0_GOV$INITIATOR_SIDEC*sub0_GOV$ACTION_IND
    sub0_GOV$SIDEC_DIR <- sub0_GOV$INITIATOR_SIDEC*sub0_GOV$ACTION_DIR
    sub0_GOV$SIDEC_PRT <- sub0_GOV$INITIATOR_SIDEC*sub0_GOV$ACTION_PRT
    sub0_GOV$SIDED_ANY <- sub0_GOV$INITIATOR_SIDED*sub0_GOV$ACTION_ANY
    sub0_GOV$SIDED_IND <- sub0_GOV$INITIATOR_SIDED*sub0_GOV$ACTION_IND
    sub0_GOV$SIDED_DIR <- sub0_GOV$INITIATOR_SIDED*sub0_GOV$ACTION_DIR
    sub0_GOV$SIDED_PRT <- sub0_GOV$INITIATOR_SIDED*sub0_GOV$ACTION_PRT
    
    events <- rbind(sub0,sub0_GOV)
    subdata <- rbind(subdata,subdata_GOV)
  }
  summary(events)
  
  # EventType
  source("Code/step2_eventcode/step2x_event_types_list.R")
  types.specific
  
  events0 <- as.data.frame(matrix(0,nrow=nrow(events),ncol=length(types.specific)))
  names(events0) <- paste0("ACTION_",types.specific)
  events0 <- cbind(data.frame(ID_TEMP=1:nrow(events)),events0)
  head(events0)
  events0$ACTION_RIOTCONTROL <- 1*(events$SIDEA_DIR>0|events$SIDEA_IND>0) 
  events0$ACTION_ARREST <- 1*(subdata$ARRESTNUM>0)*(events$SIDEA_ANY==1)
  events0$ACTION_KILLING <- 1*(subdata[,"DEATHSNUM"]>0)
  events0$ACTION_PROTEST_V <- 1*(events$SIDEB_DIR>0|events$SIDEB_IND>0)
  events0$ACTION_PROTEST <- 1*(events$SIDEB_PRT>0)
  
  # Save
  save(events,file=paste0("Output/Output_BeissingerRiot/Events/BeissingerRiot_Events_",countrycode(cntz,origin = "iso3c",destination = "iso3c"),".RData"))
  save(events0,file=paste0("Output/Output_BeissingerRiot/Events/EventType/BeissingerRiot_EventType_",countrycode(disag[j],origin = "iso3c",destination = "iso3c"),".RData"))  
  events
  
})
# beissinger.mat <- do.call(rbind,beissinger.list)
# summary(beissinger.mat)
# 
# # Save to file
# events <- beissinger.mat
# save(events,file=paste0("Output/Output_BeissingerRiot/Events/BeissingerRiot_Events_000.RData"))




