rm(list=ls())

## Set directory
setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")
#setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")

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

## Load raw data (combine 3 files)
load("Input/Events/PITF/pitf_1995_2012.RData")
data1 <- pitf.raw; rm(pitf.raw)
load("Input/Events/PITF/pitf_2013_2015.RData")
data2 <- pitf.raw; rm(pitf.raw)
data2$iso3.x <- NULL
data2$iso3 <- data2$iso3.y
load("Input/Events/PITF/pitf_20160101_20161231.RData")
data3 <- pitf.raw; rm(pitf.raw)
names(data3)[names(data3)%in%gsub(".1","",names(data3)[grep(".1",names(data3))])] <- paste0("LAT_",gsub(".1","",names(data3)[grep(".1",names(data3))]))
names(data3)[grep(".1",names(data3))] <- gsub(".1","",paste0("LONG_",names(data3)[grep(".1",names(data3))]))
commonvars <- intersect(names(data1),names(data2))
commonvars <- intersect(commonvars,names(data3))
data <- rbind(data1[,commonvars],data2[,commonvars],data3[,commonvars])
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data); rm(data1,data2,data3,commonvars)

# Convert geo coordinates
data$LATITUDE <- as.numeric(as.character(data$LAT_DEGREES))+as.numeric(as.character(data$LAT_MINUTES))/60+as.numeric(as.character(data$LAT_SECONDS))/3600
data$LONGITUDE <- as.numeric(as.character(data$LONG_DEGREES))+as.numeric(as.character(data$LONG_MINUTES))/60+as.numeric(as.character(data$LONG_SECONDS))/3600
data$LONGITUDE[gdata:::trim(data$LONG_DIRECTION)%in%c("W","West")] <- 0-data$LONGITUDE[gdata:::trim(data$LONG_DIRECTION)%in%c("W","West")]
data$LATITUDE[gdata:::trim(data$LAT_DIRECTION)%in%c("S","South")] <- 0-data$LATITUDE[gdata:::trim(data$LAT_DIRECTION)%in%c("S","South")]

# # Combine actor dictionaries
# dictz <- union(dir("Dictionaries/PITF/PITF_1995_2012"),dir("Dictionaries/PITF/PITF_2013_2015"))
# dictz <- union(dictz,dir("Dictionaries/PITF/PITF_20160101_20161231"))
# dictz <- sort(unique(dictz))
# dictz <- dictz[-grep("__Actors| _Actors",dictz)]
# for(d in 1:length(dictz)){
#   actors_OTH <- actors_CIV <- actors_REB <- actors_GOV <- actors <- c()
#   if(dictz[d]%in%dir("Dictionaries/PITF/PITF_1995_2012")){
#     load(paste0("Dictionaries/PITF/PITF_1995_2012/",dictz[d]))
#     actors <- c(actors,actorlist$actors)
#     actors_GOV <- c(actors_GOV,actorlist$actors_GOV)
#     actors_REB <- c(actors_REB,actorlist$actors_REB)
#     actors_CIV <- c(actors_CIV,actorlist$actors_CIV)
#     actors_OTH <- c(actors_OTH,actorlist$actors_OTH)
#     rm(actorlist)
#   }
#   if(dictz[d]%in%dir("Dictionaries/PITF/PITF_2013_2015")){
#     load(paste0("Dictionaries/PITF/PITF_2013_2015/",dictz[d]))
#     actors <- c(actors,actorlist$actors)
#     actors_GOV <- c(actors_GOV,actorlist$actors_GOV)
#     actors_REB <- c(actors_REB,actorlist$actors_REB)
#     actors_CIV <- c(actors_CIV,actorlist$actors_CIV)
#     actors_OTH <- c(actors_OTH,actorlist$actors_OTH)
#     rm(actorlist)
#   }
#   if(dictz[d]%in%dir("Dictionaries/PITF/PITF_20160101_20161231")){
#     load(paste0("Dictionaries/PITF/PITF_20160101_20161231/",dictz[d]))
#     actors <- c(actors,actorlist$actors)
#     actors_GOV <- c(actors_GOV,actorlist$actors_GOV)
#     actors_REB <- c(actors_REB,actorlist$actors_REB)
#     actors_CIV <- c(actors_CIV,actorlist$actors_CIV)
#     actors_OTH <- c(actors_OTH,actorlist$actors_OTH)
#     rm(actorlist)
#   }
# actorlist <- list(actors=actors,actors_GOV=actors_GOV,actors_REB=actors_REB,actors_CIV=actors_CIV,actors_OTH=actors_OTH)
# save(actorlist,file=paste0("Dictionaries/PITF/",dictz[d]))
# }
# rm(dictz)

## By country
disag <- sort(unique(data$ISO3))
disag <- disag[disag%in%gsub("PITF_|_Actors.RData","",dir("Dictionaries/PITF/"))]
disag
j <- 109; disag[j]

# # Open loop, Single-Core
# pitf.list <- lapply(1:length(disag),function(j){print(j)

# Open loop, Multi-Core
ncores <- detectCores()
pitf.list <- mclapply(1:length(disag),function(j){print(j)
  
  # Subset
  subdata <- data[data$ISO3==disag[j],]
  head(subdata)
  
  # Dates & locations
  sub.datez <- as.numeric(as.character(subdata$START.YEAR))*10000+as.numeric(as.character(subdata$START.MONTH))*100+as.numeric(as.character(subdata$START.DAY))
  sub.lat <- subdata$LATITUDE
  sub.long <- subdata$LONGITUDE
  
  sub0 <- data.frame(SOURCE=paste0("PITF"),CONFLICT=countrycode(disag[j],"iso3c","country.name"),COWN=countrycode(disag[j],origin = "iso3c",destination = "cown"),COWC=countrycode(disag[j],origin = "iso3c",destination = "cowc"),ISO3=countrycode(disag[j],origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long)
  
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
  sub0$TARGET_SIDEA <- NA#1*(subdata$SIDE_B%in%actorlist$actors_GOV)
  sub0$TARGET_SIDEB <- NA#1*(subdata$SIDE_B%in%actorlist$actors_REB)
  sub0$TARGET_SIDEC <- NA#1*(subdata$SIDE_B%in%actorlist$actors_CIV)
  sub0$TARGET_SIDED <- NA#1*((subdata$SIDE_B%in%actorlist$actors_OTH)|(subdata$SIDE_B%in%actorlist$actors&(!subdata$SIDE_B%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV))))
  
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
  events
  
  # # Close loop, Single-core
  # })
  
  # Close loop, Multi-core
},mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)

pitf.mat <- do.call(rbind,pitf.list)
summary(pitf.mat)

# Save to file
events <- pitf.mat
save(events,file=paste0("Output/Output_PITF/Events/PITF_Events_000.RData"))  



