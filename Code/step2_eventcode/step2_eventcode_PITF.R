rm(list=ls())


## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
# setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")

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
source("Code/step2_eventcode/step2x_eventType_function.R")

## Load raw data (combined file)
load("Input/Events/PITF/pitf_1995_2017.RData")
data <- pitf.raw; rm(pitf.raw)
names(data) <- toupper(names(data))

# Convert geo coordinates
data$LATITUDE <- as.numeric(as.character(data$LAT_DEGREES))+as.numeric(as.character(data$LAT_MINUTES))/60+as.numeric(as.character(data$LAT_SECONDS))/3600
data$LONGITUDE <- as.numeric(as.character(data$LONG_DEGREES))+as.numeric(as.character(data$LONG_MINUTES))/60+as.numeric(as.character(data$LONG_SECONDS))/3600
data$LONGITUDE[which(is.na(data$LONG_SECONDS)&(!is.na(data$LAT_SECONDS)))] <- (as.numeric(as.character(data$LONG_DEGREES))+as.numeric(as.character(data$LONG_MINUTES))/60)[which(is.na(data$LONG_SECONDS)&(!is.na(data$LAT_SECONDS)))]
data$LONGITUDE[gdata:::trim(data$LONG_DIRECTION)%in%c("W","West")] <- 0-data$LONGITUDE[gdata:::trim(data$LONG_DIRECTION)%in%c("W","West")]
data$LATITUDE[gdata:::trim(data$LAT_DIRECTION)%in%c("S","South")] <- 0-data$LATITUDE[gdata:::trim(data$LAT_DIRECTION)%in%c("S","South")]

# Sanity check 
plot(data$LONGITUDE,data$LATITUDE)

# Precision codes
names(data)
data$GEOPRECISION0 <- "settlement"
data$GEOPRECISION0[which(((data$LONG_SECONDS==0|is.na(data$LONG_SECONDS))&(data$LAT_SECONDS==0|is.na(data$LAT_SECONDS)))&((data$LONG_MINUTES!=0&(!is.na(data$LONG_MINUTES)))&(data$LAT_MINUTES!=0&(!is.na(data$LAT_MINUTES)))))] <- "adm2"
data$GEOPRECISION0[which((data$LONG_MINUTES==0|is.na(data$LONG_MINUTES))&(data$LAT_MINUTES==0|is.na(data$LAT_MINUTES)))] <- "adm1"
data$TIMEPRECISION0 <- "day"
data$TIMEPRECISION0[is.na(as.numeric(data$START.DAY))&(!is.na(as.numeric(data$START.MONTH)))&(!is.na(data$START.YEAR))] <- "month"
data$TIMEPRECISION0[is.na(as.numeric(data$START.DAY))&(is.na(as.numeric(data$START.MONTH)))&(!is.na(data$START.YEAR))] <- "year"
data[is.na(data$TIMEPRECISION0),]

# Dates
datez <- as.numeric(as.character(data$START.YEAR))*10000+as.numeric(as.character(data$START.MONTH))*100+as.numeric(as.character(data$START.DAY))
datez[data$TIMEPRECISION0%in%c("month")] <- (as.numeric(as.character(data$START.YEAR))*10000+as.numeric(as.character(data$START.MONTH))*100+01)[data$TIMEPRECISION0%in%c("month")]
datez[data$TIMEPRECISION0%in%c("year")] <- (as.numeric(as.character(data$START.YEAR))*10000+101)[data$TIMEPRECISION0%in%c("year")]
data[is.na(datez),]
sum(is.na(datez)|datez=="")
data$DATE <- as.numeric(datez)
range(datez)

## By country
disag <- sort(unique(data$ISO3))

## Check missing dictionaries
disag[!disag%in%sapply(strsplit(dir("Dictionaries/PITF/"),"_"),"[",2)]
disag
j <- 109; disag[j]

# # Open loop, Single-Core
# pitf.list <- lapply(1:length(disag),function(j){print(j)

# Open loop, Multi-Core
pitf.list <- mclapply(1:length(disag),function(j){print(j)
  
  # Subset
  subdata <- data[data$ISO3==disag[j],]
  head(subdata)
  
  # Dates & locations
  sub.datez <- subdata$DATE
  sub.lat <- subdata$LATITUDE
  sub.long <- subdata$LONGITUDE
  sub.precis <- subdata$GEOPRECISION0
  sub.tprecis <- subdata$TIMEPRECISION0
  sub0 <- data.frame(SOURCE=paste0("PITF"),CONFLICT=countrycode(disag[j],"iso3c","country.name"),COWN=countrycode(disag[j],origin = "iso3c",destination = "cown"),COWC=countrycode(disag[j],origin = "iso3c",destination = "cowc"),ISO3=countrycode(disag[j],origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long,GEOPRECISION=sub.precis,TIMEPRECISION=sub.tprecis)
  head(sub0)
  
  # Actors (use pre-existing dictionaries)
  dir("Dictionaries/PITF")
  if(paste0("PITF_",toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/PITF")){load(paste0("Dictionaries/PITF/PITF_",toupper(disag[j]),"_Actors.RData"))}
  
  if(length(actorlist$actors_GOV)>0){actorlist$actors_GOV <- trim(sapply(strsplit(actorlist$actors_GOV,split="\\(\\d{4}|\\(Inf"), '[', 1))}
  if(length(actorlist$actors_REB)>0){actorlist$actors_REB <- trim(sapply(strsplit(actorlist$actors_REB,split="\\(\\d{4}|\\(Inf"), '[', 1))}
  if(length(actorlist$actors_CIV)>0){actorlist$actors_CIV <- trim(sapply(strsplit(actorlist$actors_CIV,split="\\(\\d{4}|\\(Inf"), '[', 1))}
  if(length(actorlist$actors_OTH)>0){actorlist$actors_OTH <- trim(sapply(strsplit(actorlist$actors_OTH,split="\\(\\d{4}|\\(Inf"), '[', 1))}
  
  sub0$INITIATOR_SIDEA <- 1*(subdata$PERP.STATE.ROLE%in%actorlist$actors_GOV)
  sub0$INITIATOR_SIDEB <- 1*(subdata$PERP.STATE.ROLE%in%actorlist$actors_REB)
  sub0$INITIATOR_SIDEC <- 1*(subdata$PERP.STATE.ROLE%in%actorlist$actors_CIV)
  sub0$INITIATOR_SIDED <- 1*((subdata$PERP.STATE.ROLE%in%actorlist$actors_OTH)|(subdata$PERP.STATE.ROLE%in%actorlist$actors&(!subdata$PERP.STATE.ROLE%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV))))
  sub0$TARGET_SIDEA <- 0#1*(subdata$SIDE_B%in%actorlist$actors_GOV)
  sub0$TARGET_SIDEB <- 0#1*(subdata$SIDE_B%in%actorlist$actors_REB)
  sub0$TARGET_SIDEC <- 1#1*(subdata$SIDE_B%in%actorlist$actors_CIV)
  sub0$TARGET_SIDED <- 0#1*((subdata$SIDE_B%in%actorlist$actors_OTH)|(subdata$SIDE_B%in%actorlist$actors&(!subdata$SIDE_B%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV))))
  
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
  subdata$TEXT <- paste(gsub(" |\\/","",gdata:::trim(subdata$WEAPONS)),gsub(" |\\/","",gdata:::trim(subdata$COLLATERAL.DAMAGE)),subdata$DESCRIPTION)
  subdata$TEXT <- gsub("NotCollateralDamage|NoCollateralDamage","NotCollDamage",subdata$TEXT)
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
  head(subdata)
  end.dates <- as.numeric(as.character(subdata$END.YEAR))*10000+as.numeric(as.character(subdata$END.MONTH))*100+as.numeric(as.character(subdata$END.DAY))
  end.dates[which(end.dates>29999999)] <- sub.datez[which(end.dates>29999999)]
  end.dates[nchar(end.dates)<8] <- NA
  end.dates[as.character(sub0[,"DATE"])<end.dates] <- NA
  end.dates[is.na(end.dates)] <- sub.datez[is.na(end.dates)]
  summary(as.numeric(end.dates)-as.numeric(sub.datez))
  t0 <- 10
  md.list <- lapply(1:nrow(sub0),function(t0){#print(t0)
    sub.t <- sub0[t0,]
    if(!is.na(sub0[t0,"DATE"])){
      if(as.character(sub0[t0,"DATE"])<end.dates[t0]){
        start.t <- paste(substr(sub0[t0,"DATE"],1,4),substr(sub0[t0,"DATE"],5,6),substr(sub0[t0,"DATE"],7,8),sep="-")
        end.t <- paste(substr(end.dates[t0],1,4),substr(end.dates[t0],5,6),substr(end.dates[t0],7,8),sep="-")
        spells <- seq(as.Date(start.t), as.Date(end.t), by="1 day")
        spells <- gsub("-","",spells)
        sub.t <- sub.t[rep(1,length(spells)),]
        sub.t$DATE <- spells
        row.names(sub.t) <- 1:nrow(sub.t)
      }}
    sub.t
  })
  events <- do.call(rbind,md.list)
  summary(events)
  
  # Save 
  save(events,file=paste0("Output/Output_PITF/Events/PITF_Events_",countrycode(disag[j],origin = "iso3c",destination = "iso3c"),".RData"))  
  events0 <- events0[,names(events0)[!names(events0)%in%names(events)]]
  save(events0,file=paste0("Output/Output_PITF/Events/EventType/PITF_EventType_",countrycode(disag[j],origin = "iso3c",destination = "iso3c"),".RData"))  
  events
  
  # # Close loop, Single-core
  # })
  
  # Close loop, Multi-core
},mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = detectCores())

# pitf.mat <- do.call(rbind,pitf.list)
# summary(pitf.mat)
# 
# # Save to file
# events <- pitf.mat
# save(events,file=paste0("Output/Output_PITF/Events/PITF_Events_000.RData"))  



