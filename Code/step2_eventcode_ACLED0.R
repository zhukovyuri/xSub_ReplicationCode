rm(list=ls())


## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
# setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","parallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")

# Event types
source("Code/step2_eventcode/step2x_eventType_function.R")
source("Code/step2_eventcode/step2x_event_types_list.R")


#############################
## Creats event-level data
#############################


## Load event type dictionary
load("Dictionaries/EventTypes/Combined_EventDictionary.RData")

## Load raw data
#data <- read.csv("Input/Events/ACLED/ACLED_Asia/acled_Asia_2015-2016.csv",stringsAsFactors=FALSE, fileEncoding="latin1")
# data <- read.csv("Input/Events/ACLED/ACLED7_Africa_1997-2016_dyadic.csv",stringsAsFactors=FALSE, fileEncoding="latin1")

# Africa
data1 <- read.csv("Input/Events/ACLED/ACLED-Africa_1997-2018_upd-mar6.csv",stringsAsFactors=FALSE, fileEncoding="latin1")
# acled.africa <- data1$country
# ME
data2 <- read.csv("Input/Events/ACLED/ACLED-MiddleEast_2017-2018_upd-Mar6.csv",stringsAsFactors=FALSE, fileEncoding="latin1")

# Africa
data3 <- read.csv("Input/Events/ACLED/ACLED_Asia_2015-2018-_updMar6.csv",stringsAsFactors=FALSE, fileEncoding="latin1")

## India
data4 <- read.csv("Input/Events/ACLED/ACLED_Asia/India-ACLED-Asia-2015-2016.csv") #,stringsAsFactors=FALSE, fileEncoding="latin1")
data4 <- data4[data4$COUNTRY%in%"India",]
# classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
# names(data) <- toupper(names(data))
# head(data)

# Make columns consistent
names(data1)[!names(data1)%in%names(data2)]
names(data2)[!names(data2)%in%names(data1)]
names(data2)[!names(data2)%in%names(data1)] <- names(data1)[!names(data1)%in%names(data2)]
names(data1)[!names(data1)%in%names(data3)]
names(data3)[!names(data3)%in%names(data1)]
names(data3)[!names(data3)%in%names(data1)] <- names(data1)[!names(data1)%in%names(data3)]
names(data1)[!names(data1)%in%names(data4)]
names(data4)[!names(data4)%in%names(data1)]
names(data4)[names(data4)%in%c("ALLY_ACTOR_1","ALLY_ACTOR_2")] <- c("ASSOC_ACTOR_1","ASSOC_ACTOR_2")
# names(data4)[!names(data3)%in%names(data1)] <- names(data1)[!names(data1)%in%names(data4)]

# Merge
commonvars <- intersect(names(data1),names(data2))
commonvars <- intersect(commonvars,names(data3))
commonvars <- intersect(commonvars,names(data4))
data <- rbind(data1[,commonvars],data2[,commonvars],data3[,commonvars],data4[,commonvars])
head(data); rm(data1,data2,data3,data4,commonvars)

# COnvert factors to strings
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)
tail(data)

# Country codes
data$ISO3 <- countrycode(data$COUNTRY,origin = "country.name",destination = "iso3c")
sort(unique(data$ISO3))

# Fix dates
datez <- paste0(sapply(strsplit(data$EVENT_DATE,"/"), '[', 3),sapply(strsplit(data$EVENT_DATE,"/"), '[', 2),sapply(strsplit(data$EVENT_DATE,"/"), '[', 1))
data[data$ISO3%in%c("IND"),"EVENT_DATE"]
x <- data$EVENT_DATE
x <- strsplit(x,"-|/")
days <- sapply(x,"[",1)
months <- sapply(x,"[",2)
months[data$ISO3%in%c("IND")] <- month.name[match(months[data$ISO3%in%c("IND")],month.abb)]
months[!is.na(match(months,month.name))] <- match(months,month.name)[!is.na(match(months,month.name))]
years <- sapply(x,"[",3)
years[nchar(years)==2&years<89] <- paste0("20",years[nchar(years)==2&years<89])
years[nchar(years)==2&years>=89] <- paste0("19",years[nchar(years)==2&years>=89])
years <- as.numeric(years)
days <- as.numeric(days)
months <- as.numeric(months)
data$DATE <- years*10000+months*100+days
range(data$DATE)

# Missing dates
length(which(nchar(data$DATE)==0|is.na(data$DATE)))

# Precision codes
names(data)
sort(unique(data$GEO_PRECISION))
data$GEOPRECISION0 <- "settlement"
data$GEOPRECISION0[which(data$GEO_PRECISION%in%c(2))] <- "adm2"
data$GEOPRECISION0[which(data$GEO_PRECISION%in%c(3))] <- "adm1"
data$TIMEPRECISION0 <- "day"
data$TIMEPRECISION0[which(data$TIME_PRECISION%in%c(2))] <- "week"
data$TIMEPRECISION0[which(data$TIME_PRECISION%in%c(3))] <- "month"
tail(data)

## By country
disag <- sort(unique(data$ISO3))

## Check missing dictionaries
disag[!disag%in%sapply(strsplit(dir("Dictionaries/ACLED/Combined/"),"_"),"[",2)]

# Actor interactions
head(data)
actor.ix <- data.frame(INTERACTION=as.numeric(as.character(sort(unique(data$INTERACTION)))),INITIATOR="",TARGET="",stringsAsFactors = FALSE)
actor.ix$INITIATOR[actor.ix$INTERACTION%in%c(10:18,30:38)] <- "SIDEA"
actor.ix$INITIATOR[actor.ix$INTERACTION%in%c(20:28,50:58,60:68)] <- "SIDEB"
actor.ix$INITIATOR[actor.ix$INTERACTION%in%c(70:78)] <- "SIDEC"
actor.ix$INITIATOR[actor.ix$INTERACTION%in%c(40:48,80:88)] <- "SIDED"
actor.ix$TARGET[actor.ix$INTERACTION%in%c(11,13,23,33)] <- "SIDEA"
actor.ix$TARGET[actor.ix$INTERACTION%in%c(12,15,16,22,25,26,35,36,45,46,55,56,65,66)] <- "SIDEB"
actor.ix$TARGET[actor.ix$INTERACTION%in%c(17,27,37,47,57,67,77)] <- "SIDEC"
actor.ix$TARGET[actor.ix$INTERACTION%in%c(14,18,24,28,34,38,44,48,58,68,78)] <- "SIDED"

# Initiator classification
init.type <- "_init"

## Loop over countries
j <- 6; disag[j]
acled.list <- mclapply(1:length(disag),function(j){print(j)
  subdata <- data[data$ISO3==disag[j],]
  tail(subdata)
  
  # Dates
  sub.datez <- subdata$DATE
  sub.lat <- subdata$LATITUDE
  sub.long <- subdata$LONGITUDE
  sub.precis <- subdata$GEOPRECISION0
  sub.tprecis <- subdata$TIMEPRECISION0
  sub0 <- data.frame(SOURCE=paste0("ACLED_v8"),CONFLICT=countrycode(disag[j],"iso3c","country.name"),COWN=countrycode(disag[j],origin = "iso3c",destination = "cown"),COWC=countrycode(disag[j],origin = "iso3c",destination = "cowc"),ISO3=countrycode(disag[j],origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long,GEOPRECISION=sub.precis,TIMEPRECISION=sub.tprecis)
  head(sub0)  
  
  # Event Types (use dictionary)
  head(subdata)
  subdata$ID_TEMP <- 1:nrow(subdata)
  subdata$TEXT <- paste(subdata$EVENT_TYPE, subdata$NOTES)
  subdata$TEXT <- gsub("No change of territory","noterritorychange",subdata$TEXT)
  subdata$TEXT <- gsub("Violence against civilians","anticivilian",subdata$TEXT)
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
  dir("Dictionaries/ACLED/Combined/")
  if(paste0("ACLED_",toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/ACLED/Combined/")){load(paste0("Dictionaries/ACLED/Combined/ACLED_",toupper(disag[j]),"_Actors.RData"))}
  
  # Clean dictionary
  if(length(actorlist$actors_GOV)>0){actorlist$actors_GOV <- gdata:::trim(sapply(strsplit(actorlist$actors_GOV,"\\(Inf|\\(\\d[4] - \\d[4]\\)|\\(\\d[4]-\\d[4]\\)"),"[",1))}
  if(length(actorlist$actors_REB)>0){actorlist$actors_REB <- gdata:::trim(sapply(strsplit(actorlist$actors_REB,"\\(Inf|\\(\\d[4] - \\d[4]\\)|\\(\\d[4]-\\d[4]\\)"),"[",1))}
  if(length(actorlist$actors_CIV)>0){actorlist$actors_CIV <- gdata:::trim(sapply(strsplit(actorlist$actors_CIV,"\\(Inf|\\(\\d[4] - \\d[4]\\)|\\(\\d[4]-\\d[4]\\)"),"[",1))}
  if(length(actorlist$actors_OTH)>0){actorlist$actors_OTH <- gdata:::trim(sapply(strsplit(actorlist$actors_OTH,"\\(Inf|\\(\\d[4] - \\d[4]\\)|\\(\\d[4]-\\d[4]\\)"),"[",1))}
  
  sub0$INITIATOR_SIDEA <- 1*(subdata$ACTOR1%in%actorlist$actors_GOV)
  sub0$INITIATOR_SIDEB <- 1*(subdata$ACTOR1%in%actorlist$actors_REB)
  sub0$INITIATOR_SIDEC <- 1*(subdata$ACTOR1%in%actorlist$actors_CIV)
  sub0$INITIATOR_SIDED <- 1*((subdata$ACTOR1%in%actorlist$actors_OTH)|(subdata$ACTOR1%in%actorlist$actors&(!subdata$ACTOR1%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV))))
  sub0$TARGET_SIDEA <- 1*(subdata$ACTOR2%in%actorlist$actors_GOV)
  sub0$TARGET_SIDEB <- 1*(subdata$ACTOR2%in%actorlist$actors_REB)
  sub0$TARGET_SIDEC <- 1*(subdata$ACTOR2%in%actorlist$actors_CIV)
  sub0$TARGET_SIDED <- 1*((subdata$ACTOR2%in%actorlist$actors_OTH)|(subdata$ACTOR2%in%actorlist$actors&(!subdata$ACTOR2%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV))))
  
  # From interaction codes
  sub1 <- sub0
  sub1$INITIATOR_SIDEA <- 1*(actor.ix[match(subdata$INTERACTION,actor.ix$INTERACTION),"INITIATOR"]%in%"SIDEA")
  sub1$INITIATOR_SIDEB <- 1*(actor.ix[match(subdata$INTERACTION,actor.ix$INTERACTION),"INITIATOR"]%in%"SIDEB")
  sub1$INITIATOR_SIDED <- 1*(actor.ix[match(subdata$INTERACTION,actor.ix$INTERACTION),"INITIATOR"]%in%"SIDED")
  sub1$TARGET_SIDEA <- 1*(actor.ix[match(subdata$INTERACTION,actor.ix$INTERACTION),"TARGET"]%in%"SIDEA")
  sub1$TARGET_SIDEB <- 1*(actor.ix[match(subdata$INTERACTION,actor.ix$INTERACTION),"TARGET"]%in%"SIDEB")
  sub1$TARGET_SIDEC <- 1*(actor.ix[match(subdata$INTERACTION,actor.ix$INTERACTION),"TARGET"]%in%"SIDEC")
  sub1$TARGET_SIDED <- 1*(actor.ix[match(subdata$INTERACTION,actor.ix$INTERACTION),"TARGET"]%in%"SIDED")
  
  # Use native ACLED codes in cases of disagreement
  sub0$INITIATOR_SIDEA[!sub0$INITIATOR_SIDEA==sub1$INITIATOR_SIDEA] <- sub1$INITIATOR_SIDEA[!sub0$INITIATOR_SIDEA==sub1$INITIATOR_SIDEA]
  sub0$INITIATOR_SIDEB[!sub0$INITIATOR_SIDEB==sub1$INITIATOR_SIDEB] <- sub1$INITIATOR_SIDEB[!sub0$INITIATOR_SIDEB==sub1$INITIATOR_SIDEB]
  sub0$INITIATOR_SIDEB[!sub0$INITIATOR_SIDEC==sub1$INITIATOR_SIDEC] <- sub1$INITIATOR_SIDEC[!sub0$INITIATOR_SIDEC==sub1$INITIATOR_SIDEC]
  sub0$INITIATOR_SIDED[!sub0$INITIATOR_SIDED==sub1$INITIATOR_SIDED] <- sub1$INITIATOR_SIDED[!sub0$INITIATOR_SIDED==sub1$INITIATOR_SIDED]
  sub0$TARGET_SIDEA[!sub0$TARGET_SIDEA==sub1$TARGET_SIDEA] <- sub1$TARGET_SIDEA[!sub0$TARGET_SIDEA==sub1$TARGET_SIDEA]
  sub0$TARGET_SIDEB[!sub0$TARGET_SIDEB==sub1$TARGET_SIDEB] <- sub1$TARGET_SIDEB[!sub0$TARGET_SIDEB==sub1$TARGET_SIDEB]
  sub0$TARGET_SIDEB[!sub0$TARGET_SIDEC==sub1$TARGET_SIDEC] <- sub1$TARGET_SIDEC[!sub0$TARGET_SIDEC==sub1$TARGET_SIDEC]
  sub0$TARGET_SIDED[!sub0$TARGET_SIDED==sub1$TARGET_SIDED] <- sub1$TARGET_SIDED[!sub0$TARGET_SIDED==sub1$TARGET_SIDED]
  head(sub0)
  
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
  
  # Actions
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
  head(events)
  # Multi-day events
  # not needed, because all ACLED events are single-day
  
  # Save 
  save(events,file=paste0("Output/Output_ACLED0/Events/ACLED0_Events_",countrycode(disag[j],origin = "iso3c",destination = "iso3c"),".RData"))  
  events0 <- events0[,names(events0)[!names(events0)%in%names(events)]]
  save(events0,file=paste0("Output/Output_ACLED0/Events/EventType/ACLED0_EventType_",countrycode(disag[j],origin = "iso3c",destination = "iso3c"),".RData"))  
  events
  
},mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = detectCores())


