rm(list=ls())


## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
if(grepl("w64",sessionInfo()[[1]][[1]])){setwd("D:/Dropbox (Zhukov research team)/XSub/Data/")}

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


#############################
## Creat event-level data
#############################
## Pakistan: 19880101 - 20111108
#############################

## Load custom functions
source("Code/functions.R")

# # Load events
# load("Input/Events/ESOC/BFRS/esoc_PK_v10_GEO.RData")
# source("Code/step2_eventcode/step2x_event_types_list.R")
# source("Code/step2_eventcode/step2x_eventType_function.R")
# 
# #Create the DATE column in ESOC Pakistan
# data <- esoc.raw #30404    46
# data <- data[complete.cases(data[,9:10]),]
# data <- data[complete.cases(data[,40]),]
# head(data)
# tail(data)
# dim(data) #29969 by  46
# #dropped almost 500 observation due to missing data in the "year", "month" and "calendar.end...day" columns.
# 
# data$DATE <- as.Date(with(data, paste(Year, Month, Calendar.Day...End)), "%Y%m%d")
# data$DATE <- gsub("-", "", data$DATE)
# head(data)
# tail(data)
# # save(data, file="Input/Events/ESOC/BFRS/esoc_PK_v10_GEO.RData")

load("Input/Events/ESOC/BFRS/esoc_PK_v10_GEO.RData")
head(data)

# Precision codes
head(data)
data$GEOPRECISION0 <- "settlement"
data$GEOPRECISION0[which((data$Town.City==""|is.na(data$Town.City))&(data$Village==""|is.na(data$Village)))] <- "adm2"
data$TIMEPRECISION0 <- "day"
tail(data)

# Subset
subdata <- data
head(subdata)
dim(data)

# Dates & locations
sub.datez <- as.numeric(as.character(subdata$Year))*10000+as.numeric(as.character(subdata$Month))*100+as.numeric(as.character(subdata$Calendar.Day...End))
sub.lat <- subdata$LAT
sub.long <- subdata$LONG
sub.precis <- subdata$GEOPRECISION0
sub.tprecis <- subdata$TIMEPRECISION0
cnt <- "PAK"
sub0 <- data.frame(SOURCE=paste0("ESOCPakistanBFRS"),CONFLICT=countrycode(cnt,"iso3c","country.name"),COWN=countrycode(cnt,origin = "iso3c",destination = "cown"),COWC=countrycode(cnt,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cnt,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long,GEOPRECISION=sub.precis,TIMEPRECISION=sub.tprecis)
sub0
head(sub0)

# Actors (use pre-existing dictionaries)
dir("Dictionaries/ESOC")
load("Dictionaries/ESOC/ESOC_Pakistan_Actors.RData")
actorlist

##deleting extra portion from the actorlist
tt = gdata:::trim(lapply(actorlist, function(x) gsub(" \\(Inf - -Inf\\) Pakistan", "", x)))
# cut_v = function(v){
#   n_v = NULL
#   for (i in v){
#     n_v = c(n_v, substr(i, 1, nchar(i)-2))
#   }
#   return(n_v)
# }
# tt = lapply(tt, cut_v)

actorlist <- tt
actorlist

if(length(actorlist$actors_GOV)>0){actorlist$actors_GOV <- trim(sapply(strsplit(actorlist$actors_GOV,split="\\(\\d{4}|\\(Inf"), '[', 1))}
if(length(actorlist$actors_REB)>0){actorlist$actors_REB <- trim(sapply(strsplit(actorlist$actors_REB,split="\\(\\d{4}|\\(Inf"), '[', 1))}
if(length(actorlist$actors_CIV)>0){actorlist$actors_CIV <- trim(sapply(strsplit(actorlist$actors_CIV,split="\\(\\d{4}|\\(Inf"), '[', 1))}
if(length(actorlist$actors_OTH)>0){actorlist$actors_OTH <- trim(sapply(strsplit(actorlist$actors_OTH,split="\\(\\d{4}|\\(Inf"), '[', 1))}


sub0$INITIATOR_SIDEA <- 1*(subdata$Party.Responsible%in%actorlist$actors_GOV)
sub0$INITIATOR_SIDEB <- 1*(subdata$Party.Responsible%in%actorlist$actors_REB)
sub0$INITIATOR_SIDEC <- 1*(subdata$Party.Responsible%in%actorlist$actors_CIV)
sub0$INITIATOR_SIDED <- 1*((subdata$Party.Responsible%in%actorlist$actors_OTH)|(subdata$Party.Responsible%in%actorlist$actors&(!subdata$Party.Responsible%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV))))
sub0$TARGET_SIDEA <- NA#1*(subdata$SIDE_B%in%actorlist$actors_GOV)
sub0$TARGET_SIDEB <- NA#1*(subdata$SIDE_B%in%actorlist$actors_REB)
sub0$TARGET_SIDEC <- NA#1*(subdata$SIDE_B%in%actorlist$actors_CIV)
sub0$TARGET_SIDED <- NA#1*((subdata$SIDE_B%in%actorlist$actors_OTH)|(subdata$SIDE_B%in%actorlist$actors&(!subdata$SIDE_B%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV))))
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

# Event Types (use dictionary)
dir("Dictionaries/EventTypes/ESOC")
load("Dictionaries/EventTypes/Combined_EventDictionary.RData")
source("Code/step2_eventcode/step2x_event_types_list.R")
source("Code/step2_eventcode/step2x_eventType_function.R")
summary(term.type)
subdata$ID_TEMP <- 1:nrow(subdata)
textvar <- "Description"
idvar <- "ID_TEMP"
events0 <- eventType(subdata=subdata,idvar=idvar,textvar=textvar,term.type=term.type,types.specific=types.specific,types.general=types.general)
summary(events0)
head(events0)
head(subdata)

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
end.dates <- as.numeric(as.character(subdata$Year))*10000+as.numeric(as.character(subdata$Month))*100+as.numeric(as.character(subdata$Calendar.Day...End))
end.dates[which(end.dates>20111108)] <- sub.datez[which(end.dates>20111108)]
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
save(events,file=paste0("Output/Output_ESOCPakistanBFRS/Events/ESOCPakistanBFRS_Events_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData"))
events0 <- events0[,names(events0)[!names(events0)%in%names(events)]]
save(events0,file=paste0("Output/Output_ESOCPakistanBFRS/Events/EventType/ESOCPakistanBFRS_EventType_",cnt,".RData")) 

head(events)


