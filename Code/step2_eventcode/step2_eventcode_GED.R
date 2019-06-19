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

# Load events

# # Clean up
# ged.raw <- read.csv("Input/Events/UCDP_GED/ged171.csv")
# countrylist <- sort(unique(as.character(ged.raw$country)))
# countrylist <- data.frame(country=countrylist,iso3=countrycode(countrylist,origin="country.name",destination="iso3c"))
# for(j in 1:2){countrylist[,j]<-as.character(countrylist[,j])}
# countrylist[countrylist$country=="Yemen (North Yemen)","iso3"] <- countrycode("Yemen","country.name","iso3c")
# countrylist[countrylist$country=="Serbia (Yugoslavia)","iso3"] <- countrycode("Serbia","country.name","iso3c")
# ged.raw <- merge(ged.raw,countrylist,by="country",all.x=T,all.y=T)
# save(ged.raw,file="Input/Events/UCDP_GED/ged171.RData")

# Load Syria data (for some reason missing from v 17.1)
load("Input/Events/UCDP_GED/ged30.RData")
data <- ged.raw[ged.raw$iso3%in%"SYR",]
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
syr.raw <- data
head(syr.raw); rm(data)

## Load raw data
load("Input/Events/UCDP_GED/ged171.RData")
data <- ged.raw
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)

# Merge
commonvars <- intersect(names(data),names(syr.raw))
data <- rbind(data[,commonvars],syr.raw[,commonvars])
head(data); rm(syr.raw,commonvars)

# Precision codes
names(data)
sort(unique(data$WHERE_PREC))
data$GEOPRECISION0 <- "settlement"
data$GEOPRECISION0[which(data$WHERE_PREC%in%c(2,3))] <- "adm2"
data$GEOPRECISION0[which(data$WHERE_PREC%in%c(4,5))] <- "adm1"
data$GEOPRECISION0[which(data$WHERE_PREC%in%c(6,7))] <- "adm0"
data$TIMEPRECISION0 <- "day"
data$TIMEPRECISION0[which(data$DATE_PREC%in%c(2,3))] <- "week"
data$TIMEPRECISION0[which(data$DATE_PREC%in%c(4))] <- "month"
data$TIMEPRECISION0[which(data$DATE_PREC%in%c(5))] <- "year"
tail(data)

# Initiator classification
init.type <- ""

## By country
disag <- sort(unique(data$ISO3))

## Check missing dictionaries
disag[!disag%in%sapply(strsplit(dir("Dictionaries/GED/Combined/"),"_"),"[",2)]

j <- 1; disag[j]

# # Open loop, Single-Core
# ged.list <- lapply(1:length(disag),function(j){print(j)

# Open loop, Multi-Core
ged.list <- mclapply(1:length(disag),function(j){print(j)
  
  # Subset
  subdata <- data[data$ISO3==disag[j],]
  head(subdata)
  
  # Dates & locations
  sub.datez <- gsub("-","",subdata$DATE_START)
  sub.lat <- subdata$LATITUDE
  sub.long <- subdata$LONGITUDE
  sub.precis <- subdata$GEOPRECISION0
  sub.tprecis <- subdata$TIMEPRECISION0
  sub0 <- data.frame(SOURCE=paste0("GED_v171"),CONFLICT=countrycode(disag[j],"iso3c","country.name"),COWN=countrycode(disag[j],origin = "iso3c",destination = "cown"),COWC=countrycode(disag[j],origin = "iso3c",destination = "cowc"),ISO3=countrycode(disag[j],origin = "iso3c",destination = "iso3c"),DATE=as.character(sub.datez),LAT=sub.lat,LONG=sub.long,GEOPRECISION=sub.precis,TIMEPRECISION=sub.tprecis)
  
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
  
  # Tactic-based actor coding
  if(init.type%in%c("_init")){
    load("DataCensus/TacticsActors_v1.RData")
    # tactic.ix <- data.frame(ACTION=types.specific,INITIATOR="",stringsAsFactors = FALSE)
    # tactic.ix[tactic.ix$ACTION%in%c("AIRSTRIKE","ARMOR","ARREST","RIOTCONTROL"),"INITIATOR"] <- "SIDEA"
    # tactic.ix[tactic.ix$ACTION%in%c("PROTEST","PROTEST_V","RIOT","SUICIDE","TERROR"),"INITIATOR"] <- "SIDEB"
  }
  
  # Actors (use pre-existing dictionaries)
  dir("Dictionaries/GED")
  if(paste0("GED_",toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/GED/Combined/")){load(paste0("Dictionaries/GED/Combined/GED_",toupper(disag[j]),"_Actors.RData"))}
  
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
  
  # Tactic-based event intiator fix
  if(init.type%in%c("_init")){
    sub2 <- sub0
    flipz.a <- apply(events0[,paste0("ACTION_",tactic.ix$ACTION[tactic.ix$INITIATOR%in%"SIDEA"])],1,function(x){sum(x,na.rm=T)})>0
    flipz.ab <- flipz.a&sub0$INITIATOR_SIDEB==1
    flipz.aba <- flipz.ab&flipz.a&sub0$TARGET_SIDEA==1
    flipz.b <- apply(events0[,paste0("ACTION_",tactic.ix$ACTION[tactic.ix$INITIATOR%in%"SIDEB"])],1,function(x){sum(x,na.rm=T)})>0
    flipz.ba <- flipz.b&sub0$INITIATOR_SIDEA==1
    flipz.bab <- flipz.ba&flipz.b&sub0$TARGET_SIDEB==1
    sub2$INITIATOR_SIDEA[flipz.ab] <- 1
    sub2$INITIATOR_SIDEB[flipz.ab] <- 0
    sub2$TARGET_SIDEA[flipz.aba] <- 0
    sub2$TARGET_SIDEB[flipz.aba] <- 1
    sub2$INITIATOR_SIDEA[flipz.ba] <- 0
    sub2$INITIATOR_SIDEB[flipz.ba] <- 1
    sub2$TARGET_SIDEA[flipz.bab] <- 1
    sub2$TARGET_SIDEB[flipz.bab] <- 0
    sub0 <- sub2
  }
  
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
  
  # Undirected
  if(!init.type%in%c("_init")){
    sub0$DYAD_B_A <- sub0$DYAD_A_B
    sub0$DYAD_C_A <- sub0$DYAD_A_C
    sub0$DYAD_C_B <- sub0$DYAD_B_C
    sub0$DYAD_D_A <- sub0$DYAD_A_D
    sub0$DYAD_D_B <- sub0$DYAD_B_D
    sub0$DYAD_D_C <- sub0$DYAD_C_D
  }
  
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
  
  # Conform events0
  events0.temp <- data.frame(ID_TEMP=events$ID_TEMP)
  events0 <- merge(events0.temp,events0,by="ID_TEMP",all.x=T,all.y=T)
  mean(events$ID_TEMP==events0$ID_TEMP)
  events$ID_TEMP <- NULL
  
  # Save 
  save(events,file=paste0("Output/Output_GED/Events/GED_Events_",countrycode(disag[j],origin = "iso3c",destination = "iso3c"),".RData"))  
  events0 <- events0[,names(events0)[!names(events0)%in%names(events)]]
  save(events0,file=paste0("Output/Output_GED/Events/EventType/GED_EventType_",countrycode(disag[j],origin = "iso3c",destination = "iso3c"),".RData"))  
  events
  
  
  
  # # Close loop, Single-core
  # })
  
  # Close loop, Multi-core
},mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = detectCores())



