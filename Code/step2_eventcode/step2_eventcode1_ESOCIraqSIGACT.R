rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


#############################
## Creat event-level data
#############################
## Iraq: Feb 4, 2004 - Feb 24, 2009
#############################

## Load custom functions
source("Code/functions.R")

# # Load events
# load("Input/Events/ESOC/SIGACT/esoc_iraq_week_district_GEO.RData")
# #df <- read.dta("Input/Events/ESOC/SIGACT/esoc-iraq-v3_sigact_district-year.dta")
# 
# # Subset
# subdata <- esoc.raw
# head(subdata)
# 
# #creating YEAR, MONTH, DATA  since they are missing from the orignal data
# subdata$YEAR <- NA
# subdata$MONTH <- NA
# subdata$DAY <- NA
# subdata$DATE <- NA
# subdata$DATE[1] <- 20040204
# dim(subdata) #27456 by 20
# subdata$DATE[27456] <- 20090224
# head(subdata)
# tail(subdata)
# subdata$WID <- subdata$week + 774
# #save(subdata, file="Input/Events/ESOC/SIGACT/ESOCIraqSIGACT_week_district_GEO.RData")

# Load data
load("Input/Events/ESOC/SIGACT/esoc_iraq_week_district_GEO.RData")

# Dates
datez <- seq(as.Date("1900-01-01"), as.Date("2016-10-24"), by="days")
ticker <-data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
ticker <- ticker[as.character(ticker$DATE)>=range(as.character(subdata$DATE),na.rm=TRUE)[1]&as.character(ticker$DATE)<=range(as.character(subdata$DATE),na.rm=TRUE)[2],]
head(ticker)
tail(ticker)
head(subdata)
tail(subdata)
d.wid <- ticker[ticker$DATE%in%min(subdata$DATE,na.rm=T),"WID"]-subdata[subdata$DATE%in%min(subdata$DATE,na.rm=T),"WID"]
subdata$WID <- subdata$WID+d.wid

# Dates & locations
sub.datez <- as.numeric(as.character(subdata$WID))
sub.lat <- subdata$LAT
sub.long <- subdata$LONG
cnt <- "IRQ"
sub0 <- data.frame(SOURCE=paste0("ESOCIraqSIGACT"),CONFLICT=countrycode(cnt,"iso3c","country.name"),COWN=countrycode(cnt,origin = "iso3c",destination = "cown"),COWC=countrycode(cnt,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cnt,origin = "iso3c",destination = "iso3c"),WID=sub.datez,LAT=sub.lat,LONG=sub.long)
head(sub0)

# Actors )
sub0$INITIATOR_SIDEA <- NA
sub0$INITIATOR_SIDEB <- 1*apply(subdata[,c("ied_attack","suicide")],1,function(x){sum(x,na.rm=T)>0})
sub0$INITIATOR_SIDEC <- NA
sub0$INITIATOR_SIDED <- NA

sub0$TARGET_SIDEA <- 1*apply(subdata[,c("ied_attack","ied_attack")],1,function(x){sum(x,na.rm=T)>0})
sub0$TARGET_SIDEB <- NA
sub0$TARGET_SIDEC <- NA
sub0$TARGET_SIDED <- NA
tail(sub0)

# Event Types (no need to use the dictionary, since the data contains such information already)
sub0$ACTION_ANY <- 1
sub0$ACTION_IND <- 1*apply(subdata[,c("ied_attack","idf")],1,function(x){sum(x,na.rm=T)>0})
sub0$ACTION_SEL <- 1*apply(subdata[,c("df","ied_clear")],1,function(x){sum(x,na.rm=T)>0}) #"df" column include double as was giving me an error
sub0$ACTION_PRT <- 0
head(sub0)
tail(sub0)

# This portion does not apply to this dataset, since there is no Summary/TEXT column
# head(subdata)
# subdata$ID_TEMP <- 1:nrow(subdata)
# subdata$TEXT <- subdata$TYPE
# subdata$TEXT <- iconv(subdata$TEXT,"WINDOWS-1252","UTF-8")
# subdata$TEXT <- tolower(subdata$TEXT)
# textvar <- "TEXT"
# idvar <- "ID_TEMP"
# length(unique(subdata[,idvar]))==nrow(subdata)
# events0 <- eventType(subdata=subdata,idvar=idvar,textvar=textvar,term.type=term.type,types.specific=types.specific,types.general=types.general)
# summary(events0)

# # Actions (indiscriminate = violence vs. civilians)
# sub0$ACTION_ANY <- events0$ACTION_ANY
# sub0$ACTION_IND <- events0$ACTION_IND
# sub0$ACTION_SEL <- events0$ACTION_SEL
# sub0$ACTION_PRT <- events0$ACTION_PRT

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
head(events)
tail(events)

# # Multi-day events
# head(subdata)
# end.dates <- as.numeric(as.character(subdata$END.YEAR))*10000+as.numeric(as.character(subdata$END.MONTH))*100+as.numeric(as.character(subdata$END.DAY))
# end.dates[which(end.dates>29999999)] <- sub.datez[which(end.dates>29999999)]
# end.dates[nchar(end.dates)<8] <- NA
# end.dates[as.character(sub0[,"DATE"])<end.dates] <- NA
# end.dates[is.na(end.dates)] <- sub.datez[is.na(end.dates)]
# summary(as.numeric(end.dates)-as.numeric(sub.datez))
# t0 <- 10
# md.list <- lapply(1:nrow(sub0),function(t0){print(t0)
#   sub.t <- sub0[t0,]
#   if(!is.na(sub0[t0,"WID"])){
#     if(as.character(sub0[t0,"WID"])<end.dates[t0]){
#       start.t <- paste(substr(sub0[t0,"WID"],1,4))
#       end.t <- paste(substr(end.dates[t0],1,4))
#       spells <- seq(as.Date(start.t), as.Date(end.t), by="1 week")
#       spells <- gsub("-","",spells)
#       sub.t <- sub.t[rep(1,length(spells)),]
#       sub.t$WID <- spells
#       row.names(sub.t) <- 1:nrow(sub.t)
#     }}
#   sub.t
# })
# events <- do.call(rbind,md.list)
# summary(events)

# Save
save(events,file=paste0("Output/Output_ESOCIraqSIGACT/Events/ESOCIraqSIGACT_Events_",countrycode(cnt,origin = "iso3c",destination = "iso3c"),".RData"))
head(events)


