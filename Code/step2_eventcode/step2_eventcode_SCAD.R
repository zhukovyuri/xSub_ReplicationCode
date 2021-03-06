rm(list=ls())


## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
# setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","spatstat","parallel","foreach")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

#############################
## Creat event-level data
#############################

source("Code/functions.R")

## Load event type dictionary
load("Dictionaries/EventTypes/Combined_EventDictionary.RData")
source("Code/step2_eventcode/step2x_event_types_list.R")
source("Code/step2_eventcode/step2x_eventType_function.R")

## Load raw data (combine 2 files)
load("Input/Events/SCAD/scad_Africa_32.RData")
data1 <- scad.raw; rm(scad.raw)
load("Input/Events/SCAD/scad_LatinAmerica_32.RData")
data2 <- scad.raw; rm(scad.raw)
names(data2)[names(data2)%in%c("endday")] <- "eday"
names(data2)[names(data2)%in%c("endmo")] <- "emo"
names(data2)[names(data2)%in%c("endyr")] <- "eyr"
commonvars <- intersect(names(data1),names(data2))
data <- rbind(data1[,commonvars],data2[,commonvars])
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data); rm(data1,data2,commonvars)

# Precision codes
sort(unique(data$LOCATION_PRECISION))
sort(unique(data$LOCNUM))
data$GEOPRECISION0 <- "settlement"
data$GEOPRECISION0[which(data$LOCNUM%in%c(-99,7)|nchar(data$LOCATION_PRECISION)>0)] <- "adm0"
data$TIMEPRECISION0 <- "day"

## By country
disag <- sort(unique(data$ISO3))

## Check missing dictionaries
disag[!disag%in%sapply(strsplit(dir("Dictionaries/SCAD/Combined/"),"_"),"[",2)]

# disag <- disag[disag%in%gsub("SCAD_|_Actors.RData","",dir("Dictionaries/SCAD/Combined/"))]
j <- 1; disag[j]
scad.list <- mclapply(1:length(disag),function(j){print(j)
  subdata <- data[data$ISO3==disag[j],]
  head(subdata)
  
  # Dates & locations
  sub.datez <- subdata$STYR*10000+subdata$STMO*100+subdata$STDAY
  sub.lat <- subdata$LATITUDE
  sub.long <- subdata$LONGITUDE
  sub.precis <- subdata$GEOPRECISION0
  sub.tprecis <- subdata$TIMEPRECISION0
  sub0 <- data.frame(SOURCE=paste0("SCAD_v32"),CONFLICT=countrycode(disag[j],"iso3c","country.name"),COWN=countrycode(disag[j],origin = "iso3c",destination = "cown"),COWC=countrycode(disag[j],origin = "iso3c",destination = "cowc"),ISO3=countrycode(disag[j],origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long,GEOPRECISION=sub.precis,TIMEPRECISION=sub.tprecis)
  
  # Actors (use pre-existing dictionaries)
  dir("Dictionaries/SCAD/Combined/")
  if(paste0("SCAD_",toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/SCAD/Combined/")){load(paste0("Dictionaries/SCAD/Combined/SCAD_",toupper(disag[j]),"_Actors.RData"))}
  
  if(length(actorlist$actors_GOV)>0){actorlist$actors_GOV <- trim(sapply(strsplit(actorlist$actors_GOV,split="\\(\\d{4}|\\(Inf"), '[', 1))}
  if(length(actorlist$actors_REB)>0){actorlist$actors_REB <- trim(sapply(strsplit(actorlist$actors_REB,split="\\(\\d{4}|\\(Inf"), '[', 1))}
  if(length(actorlist$actors_CIV)>0){actorlist$actors_CIV <- trim(sapply(strsplit(actorlist$actors_CIV,split="\\(\\d{4}|\\(Inf"), '[', 1))}
  if(length(actorlist$actors_OTH)>0){actorlist$actors_OTH <- trim(sapply(strsplit(actorlist$actors_OTH,split="\\(\\d{4}|\\(Inf"), '[', 1))}
  
  # Actor code (combine dictionary and GOVTARGET variables)
  sub0$INITIATOR_SIDEA <- 1*(subdata$ACTOR1%in%actorlist$actors_GOV|subdata$ACTOR2%in%actorlist$actors_GOV|subdata$ACTOR3%in%actorlist$actors_GOV)
  sub0$INITIATOR_SIDEB <- 1*(!(subdata$ACTOR1%in%c(actorlist$actors_GOV,actorlist$actors_CIV)|subdata$ACTOR2%in%c(actorlist$actors_GOV,actorlist$actors_CIV)|subdata$ACTOR3%in%c(actorlist$actors_GOV,actorlist$actors_CIV)))*(1*(subdata$CGOVTARGET+subdata$RGOVTARGET>0))
  sub0$INITIATOR_SIDEC <- 1*(subdata$ACTOR1%in%actorlist$actors_CIV|subdata$ACTOR2%in%actorlist$actors_CIV|subdata$ACTOR3%in%actorlist$actors_CIV)
  sub0$INITIATOR_SIDED <- 1*(!(subdata$ACTOR1%in%c(actorlist$actors_GOV,actorlist$actors_CIV)|subdata$ACTOR2%in%c(actorlist$actors_GOV,actorlist$actors_CIV)|subdata$ACTOR3%in%c(actorlist$actors_GOV,actorlist$actors_CIV)))*(1*(subdata$CGOVTARGET+subdata$RGOVTARGET==0))
  sub0$TARGET_SIDEA <-  1*((subdata$TARGET1%in%actorlist$actors_GOV|subdata$TARGET2%in%actorlist$actors_GOV)|(subdata$CGOVTARGET+subdata$RGOVTARGET>0))
  sub0$TARGET_SIDEB <- 1*((subdata$TARGET1%in%actorlist$actors_REB|subdata$TARGET2%in%actorlist$actors_REB)|(!(subdata$TARGET1%in%actorlist$actors_GOV|subdata$TARGET2%in%actorlist$actors_GOV|subdata$TARGET1%in%actorlist$actors_CIV|subdata$TARGET2%in%actorlist$actors_CIV))&(subdata$CGOVTARGET+subdata$RGOVTARGET==0))
  sub0$TARGET_SIDEC <- 1*(subdata$TARGET1%in%actorlist$actors_CIV|subdata$TARGET2%in%actorlist$actors_CIV)
  sub0$TARGET_SIDED <- 1*((subdata$TARGET1%in%actorlist$actors_OTH|subdata$TARGET2%in%actorlist$actors_OTH)|(!(subdata$TARGET1%in%actorlist$actors_GOV|subdata$TARGET2%in%actorlist$actors_GOV|subdata$TARGET1%in%actorlist$actors_CIV|subdata$TARGET2%in%actorlist$actors_CIV)))
  
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
  subdata$TEXT <- subdata$ISSUENOTE
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
  
  # Multi-day events
  sub0$ID_TEMP <- events0$ID_TEMP
  head(subdata)
  end.dates <- subdata$EYR*10000+subdata$EMO*100+subdata$EDAY
  md.list <- lapply(1:nrow(sub0),function(t){#print(t)
    sub.t <- sub0[t,]
    if(sub0[t,"DATE"]<end.dates[t]){
      start.t <- paste(substr(sub0[t,"DATE"],1,4),substr(sub0[t,"DATE"],5,6),substr(sub0[t,"DATE"],7,8),sep="-")
      end.t <- paste(substr(end.dates[t],1,4),substr(end.dates[t],5,6),substr(end.dates[t],7,8),sep="-")
      spells <- seq(as.Date(start.t), as.Date(end.t), by="1 day")
      spells <- gsub("-","",spells)
      sub.t <- sub.t[rep(1,length(spells)),]
      sub.t$DATE <- spells
      row.names(sub.t) <- 1:nrow(sub.t)}
    sub.t
  })
  events <- do.call(rbind,md.list)
  head(events)
  
  # Remove pre-1990 events
  events <- events[which(events$DATE>19891231),]
  
  # Conform events0
  events0.temp <- data.frame(ID_TEMP=events$ID_TEMP)
  events0 <- merge(events0.temp,events0,by="ID_TEMP",all.x=T,all.y=F)
  mean(events$ID_TEMP==events0$ID_TEMP)
  events$ID_TEMP <- NULL
  
  # Save
  save(events,file=paste0("Output/Output_SCAD/Events/SCAD_Events_",countrycode(disag[j],origin = "iso3c",destination = "iso3c"),".RData"))
  events0 <- events0[,names(events0)[!names(events0)%in%names(events)]]
  save(events0,file=paste0("Output/Output_SCAD/Events/EventType/SCAD_EventType_",countrycode(disag[j],origin = "iso3c",destination = "iso3c"),".RData"))  
  events
  
},mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = detectCores())
# scad.mat <- do.call(rbind,scad.list)
# summary(scad.mat)

# # Save to file
# events <- scad.mat
# save(events,file=paste0("Output/Output_SCAD/Events/SCAD_Events_000.RData"))



