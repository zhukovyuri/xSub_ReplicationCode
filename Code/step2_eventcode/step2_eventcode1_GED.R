rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")



#############################
## Create event-level data
#############################

## Load event type dictionary
load("Dictionaries/EventTypes/Combined_EventDictionary.RData")
source("Code/step2_eventcode/step2x_event_types_list.R")
source("Code/step2_eventcode/step_2x_eventType_function.R")

## Load raw data
load("Input/Events/UCDP_GED/ged30.RData")
data <- ged.raw
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)

## By country
disag <- sort(unique(data$ISO3))
disag <- disag[disag%in%gsub("GED_|_Actors.RData","",dir("Dictionaries/GED/"))]
j <- 1; disag[j]

# # Open loop, Single-Core
# ged.list <- lapply(1:length(disag),function(j){print(j)

# Open loop, Multi-Core
ncores <- detectCores()
ged.list <- mclapply(1:length(disag),function(j){print(j)
  
  # Subset
  subdata <- data[data$ISO3==disag[j],]
  head(subdata)
  
  # Dates & locations
  sub.datez <- gsub("-","",subdata$DATE_START)
  sub.lat <- subdata$LATITUDE
  sub.long <- subdata$LONGITUDE
  sub0 <- data.frame(SOURCE=paste0("GED_v3"),CONFLICT=countrycode(disag[j],"iso3c","country.name"),COWN=countrycode(disag[j],origin = "iso3c",destination = "cown"),COWC=countrycode(disag[j],origin = "iso3c",destination = "cowc"),ISO3=countrycode(disag[j],origin = "iso3c",destination = "iso3c"),DATE=as.character(sub.datez),LAT=sub.lat,LONG=sub.long)
  
  # Actors (use pre-existing dictionaries)
  dir("Dictionaries/GED")
  if(paste0("GED_",toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/GED/YZ/")){load(paste0("Dictionaries/GED/YZ/GED_",toupper(disag[j]),"_Actors.RData"))}
  
  if(length(actorlist$actors_GOV)>0){actorlist$actors_GOV <- trim(sapply(strsplit(actorlist$actors_GOV,split="\\(\\d{4}|\\(Inf"), '[', 1))}
  if(length(actorlist$actors_REB)>0){actorlist$actors_REB <- trim(sapply(strsplit(actorlist$actors_REB,split="\\(\\d{4}|\\(Inf"), '[', 1))}
  if(length(actorlist$actors_CIV)>0){actorlist$actors_CIV <- trim(sapply(strsplit(actorlist$actors_CIV,split="\\(\\d{4}|\\(Inf"), '[', 1))}
  if(length(actorlist$actors_OTH)>0){actorlist$actors_OTH <- trim(sapply(strsplit(actorlist$actors_OTH,split="\\(\\d{4}|\\(Inf"), '[', 1))}
  
  sub0$INITIATOR_SIDEA <- 1*(subdata$SIDE_A%in%actorlist$actors_GOV)
  sub0$INITIATOR_SIDEB <- 1*(subdata$SIDE_A%in%actorlist$actors_REB)
  sub0$INITIATOR_SIDEC <- 1*(subdata$SIDE_A%in%actorlist$actors_CIV)
  sub0$INITIATOR_SIDED <- 1*((subdata$SIDE_A%in%actorlist$actors_OTH)|(subdata$SIDE_A%in%actorlist$actors&(!subdata$SIDE_A%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV))))
  sub0$TARGET_SIDEA <- 1*(subdata$SIDE_B%in%actorlist$actors_GOV)
  sub0$TARGET_SIDEB <- 1*(subdata$SIDE_B%in%actorlist$actors_REB)
  sub0$TARGET_SIDEC <- 1*(subdata$SIDE_B%in%actorlist$actors_CIV)
  sub0$TARGET_SIDED <- 1*((subdata$SIDE_B%in%actorlist$actors_OTH)|(subdata$SIDE_B%in%actorlist$actors&(!subdata$SIDE_B%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV))))
  
  # Event Types (use dictionary)
  head(subdata)
  subdata$ID_TEMP <- 1:nrow(subdata)
  subdata$TEXT <- subdata$SOURCE_ARTICLE 
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
  
  # Multi-day events
  head(subdata)
  end.dates <- gsub("-","",subdata$DATE_END)
  t <- 3
  max(end.dates)
  summary(as.numeric(end.dates)-as.numeric(sub.datez))
  md.list <- lapply(1:nrow(sub0),function(t){#print(t)
    sub.t <- sub0[t,]
    if(as.character(sub0[t,"DATE"])<end.dates[t]){
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
  summary(events)
  
  # Save 
  save(events,file=paste0("Output/Output_GED/Events/GED_Events_",countrycode(disag[j],origin = "iso3c",destination = "iso3c"),".RData"))  
  events
  
  
  
  # # Close loop, Single-core
  # })
  
  # Close loop, Multi-core
},mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)

# ged.mat <- do.call(rbind,ged.list)
# summary(ged.mat)
# 
# # Save to file
# events <- ged.mat
# save(events,file=paste0("Output/Output_GED/Events/GED_Events_000.RData"))  

dim(events)


