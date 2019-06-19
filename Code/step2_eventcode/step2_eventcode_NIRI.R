rm(list=ls())


## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
if(grepl("w64",sessionInfo()[[1]][[1]])){setwd("H:/Dropbox (Zhukov research team)/XSub/Data/")}

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
load("Input/Events/Davenport/Northern Ireland/NOrthernIreland_20140211.RData")
data <- davenport.raw; rm(davenport.raw)
head(data)

# Overlay countries
data("wrld_simpl")
data$TEMP_ID <- 1:nrow(data)
data.sp <- data[which(!is.na(as.numeric(as.character(data$Longitude)))&!is.na(as.numeric(as.character(data$Latitude)))),]
data.sp <- SpatialPointsDataFrame(coords = data.frame(LONG=as.numeric(as.character(data.sp$Longitude)),LAT=as.numeric(as.character(data.sp$Latitude))),data=data.sp,proj4string = CRS(proj4string(wrld_simpl)))
o <- over(data.sp,wrld_simpl,returnList = FALSE)
data.sp$ISO3 <- o$ISO3
data.sp <- data.sp[,c("TEMP_ID","ISO3")]
data <- merge(data.sp,data,by="TEMP_ID",all.x=F,all.y=T)
data <- data@data
data$TEMP_ID <- NULL
rm(data.sp,o,wrld_simpl)
head(data)

# Dates
data$YEAR <- as.numeric(as.character(data$Year.of.Event))
data$Year.of.Event[is.na(data$YEAR)]
data$MONTH <- as.numeric(as.character(data$Month.of.Event))
mo.name <- match(sapply(strsplit(as.character(data$Month.of.Event),","),"[",1),month.name)
data$MONTH[!is.na(mo.name)] <- mo.name[!is.na(mo.name)]
data$DAY <- as.numeric(as.character(data$Day.of.Event))
data$Day.of.Event[is.na(data$DAY)]
data$DAY[is.na(data$DAY)] <- as.numeric(sapply(strsplit(as.character(data$Day.of.Event),"-|\\/"),"[",1)[is.na(data$DAY)])
data$DATE <- data$YEAR*10000+data$MONTH*100+data$DAY
data$DATE[which((!is.na(data$YEAR))&(!is.na(data$MONTH))&(is.na(data$DAY)))] <- (data$YEAR*10000+data$MONTH*100+1)[which((!is.na(data$YEAR))&(!is.na(data$MONTH))&(is.na(data$DAY)))]
data$DATE[which((!is.na(data$YEAR))&(is.na(data$MONTH))&(is.na(data$DAY)))] <- (data$YEAR*10000+101)[which((!is.na(data$YEAR))&(is.na(data$MONTH))&(is.na(data$DAY)))]
summary(data)

# Drop missing dates & locations
data <- data[!(is.na(data$DATE)|is.na(as.numeric(as.character(data$Longitude)))),]

# Precision codes
head(data)
data$GEOPRECISION0 <- "settlement"
data$GEOPRECISION0[which(data$Precision%in%c(4))] <- "adm2"
data$GEOPRECISION0[which(data$Precision%in%c(5))] <- "adm1"
data$TIMEPRECISION0 <- "day"
data$TIMEPRECISION0[which((!is.na(data$YEAR))&(!is.na(data$MONTH))&(is.na(data$DAY)))] <- "month"
data$TIMEPRECISION0[which((!is.na(data$YEAR))&(is.na(data$MONTH))&(is.na(data$DAY)))] <- "year"
head(data)

# Subset
disag <- sort(unique(as.character(data$ISO3)))
j <- 1; disag[j]
niri.list <- lapply(1:length(disag),function(j){print(j)
  subdata <- data[data$ISO3%in%disag[j],]
  head(subdata)
  cntz <- disag[j]
  
  # Dates & locations
  sub.datez <- as.numeric(as.character(subdata$DATE))
  sub.lat <- subdata$Latitude
  sub.long <- subdata$Longitude
  sub.precis <- subdata$GEOPRECISION0
  sub.tprecis <- subdata$TIMEPRECISION0
  cnt <- cntz
  sub0 <- data.frame(SOURCE=paste0("NIRI"),CONFLICT=countrycode(cnt,"iso3c","country.name"),COWN=countrycode(cnt,origin = "iso3c",destination = "cown"),COWC=countrycode(cnt,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cnt,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long,GEOPRECISION=sub.precis,TIMEPRECISION=sub.tprecis)
  head(sub0)
  
  # Actors (use pre-existing dictionaries)
  subdata$ACTOR1 <- as.character(subdata$Perpetrator.s.Organizational.Affiliation)
  subdata$ACTOR2 <- as.character(subdata$Victim.s.Organizational.Affiliation)
  dir("Dictionaries/NIRI/")
  if(paste0("NIRI_",cnt,"_Actors.RData")%in%dir("Dictionaries/NIRI")){load(paste0("Dictionaries/NIRI/NIRI_",toupper(cnt),"_Actors.RData"))}
  if(!paste0("NIRI_",cnt,"_Actors.RData")%in%dir("Dictionaries/NIRI")){load(paste0("Dictionaries/NIRI/NIRI_GBR_Actors.RData"))}
  
  # Clean dictionary
  if(length(actorlist$actors_GOV)>0){actorlist$actors_GOV <- gdata:::trim(sapply(strsplit(actorlist$actors_GOV,"\\(Inf|\\(\\d{4} - \\d{4}\\)|\\(\\d{4}-\\d{4}\\)|\\(NA - NA\\)"),"[",1))}
  if(length(actorlist$actors_REB)>0){actorlist$actors_REB <- gdata:::trim(sapply(strsplit(actorlist$actors_REB,"\\(Inf|\\(\\d{4} - \\d{4}\\)|\\(\\d{4}-\\d{4}\\)|\\(NA - NA\\)"),"[",1))}
  if(length(actorlist$actors_CIV)>0){actorlist$actors_CIV <- gdata:::trim(sapply(strsplit(actorlist$actors_CIV,"\\(Inf|\\(\\d{4} - \\d{4}\\)|\\(\\d{4}-\\d{4}\\)|\\(NA - NA\\)"),"[",1))}
  if(length(actorlist$actors_OTH)>0){actorlist$actors_OTH <- gdata:::trim(sapply(strsplit(actorlist$actors_OTH,"\\(Inf|\\(\\d{4} - \\d{4}\\)|\\(\\d{4}-\\d{4}\\)|\\(NA - NA\\)"),"[",1))}
  
  head(subdata)
  
  sub0$INITIATOR_SIDEA <- 1*(subdata$ACTOR1%in%actorlist$actors_GOV)
  sub0$INITIATOR_SIDEB <- 1*(subdata$ACTOR1%in%actorlist$actors_REB)
  sub0$INITIATOR_SIDEC <- 1*(subdata$ACTOR1%in%actorlist$actors_CIV)
  sub0$INITIATOR_SIDED <- 1*((subdata$ACTOR1%in%actorlist$actors_OTH)|(subdata$ACTOR1%in%actorlist$actors&(!subdata$ACTOR1%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV))))
  sub0$TARGET_SIDEA <- 1*(subdata$ACTOR2%in%actorlist$actors_GOV)
  sub0$TARGET_SIDEB <- 1*(subdata$ACTOR2%in%actorlist$actors_REB)
  sub0$TARGET_SIDEC <- 1*(subdata$ACTOR2%in%actorlist$actors_CIV)
  sub0$TARGET_SIDED <- 1*((subdata$ACTOR2%in%actorlist$actors_OTH)|(subdata$ACTOR2%in%actorlist$actors&(!subdata$ACTOR2%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV))))
  summary(sub0)

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
  load("Dictionaries/EventTypes/Combined_EventDictionary.RData")
  head(term.type)
  source("Code/step2_eventcode/step2x_event_types_list.R")
  source("Code/step2_eventcode/step2x_eventType_function.R")
  subdata$ID_TEMP <- 1:nrow(subdata)
  subdata$summary <- as.character(subdata$Type.of.Event)
  textvar <- "summary"
  idvar <- "ID_TEMP"
  events0 <- eventType(subdata=subdata,idvar=idvar,textvar=textvar,term.type=term.type,types.specific=types.specific,types.general=types.general)
  summary(events0)
  head(events0)
  head(subdata)
  
  # Actions
  sub0$ACTION_ANY <- events0$ACTION_ANY
  sub0$ACTION_IND <- 1*(events0$ACTION_IND|grepl("indiscrim|Indiscrim",subdata$summary))
  sub0$ACTION_DIR <- 1*(events0$ACTION_DIR|grepl("selective|Selective",subdata$summary))
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
  head(events0)
  
  # Save
  save(events,file=paste0("Output/Output_NIRI/Events/NIRI_Events_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData"))
  events0 <- events0[,names(events0)[!names(events0)%in%names(events)]]
  save(events0,file=paste0("Output/Output_NIRI/Events/EventType/NIRI_EventType_",cnt,".RData")) 

})
