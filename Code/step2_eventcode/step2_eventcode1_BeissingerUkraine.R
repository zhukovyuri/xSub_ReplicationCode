rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")
#setwd("~/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("lubridate", "gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


#############################
## Creat event-level data
#############################


## Load custom functions
rm(list=ls())
source("Code/functions.R")

load("Input/Events/Beissinger/Ukraine/Ukraine_GEO.RData")

# data <- beissinger.raw
data[data$REPUBLIC == "Moldova", ][, "REPUBLIC_RENAME"] <- "Moldova"
tail(data)

data$ISO3 <- countrycode(data$REPUBLIC,origin = "country.name",destination = "iso3c")
summary(as.factor(data$ISO3))
head(data)
tail(data)

## By country
disag <- sort(unique(data$ISO3))
j <- 1; disag[j]
beissinger.list <- lapply(1:length(disag),function(j){print(j)
  subdata <- data[data$ISO3==disag[j],]
  head(subdata)
  cntz <- disag[j]
  
  # Dates & locations
  sub.datez <- as.numeric(as.character(subdata$DATE)) #*10000+as.numeric(as.character(subdata$Month))*100+as.numeric(as.character(subdata$Calendar.Day...End))
  sub.lat <- subdata$LAT
  sub.long <- subdata$LONG
  sub0 <- data.frame(SOURCE=paste0("BeissingerUkraine"),CONFLICT=countrycode(cntz,"iso3c","country.name"),COWN=countrycode(cntz,origin = "iso3c",destination = "cown"),COWC=countrycode(cntz,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cntz,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long)
  sub0
  head(sub0)
  
  # Actors (based on the article)
  sub0$INITIATOR_SIDEA <- NA
  sub0$INITIATOR_SIDEB <- 1
  sub0$INITIATOR_SIDEC <- NA
  sub0$INITIATOR_SIDED <- NA
  
  sub0$TARGET_SIDEA <- 1*(subdata$NATTARGET=="Y"|subdata$LOC1TARGET=="L"|subdata$LOC1TARGET=="Y"|subdata$LOC1TARGET=="R"|subdata$LOC2TARGET=="Y")
  sub0$TARGET_SIDEB <- NA
  sub0$TARGET_SIDEC <- 1*(subdata$ETHTARGET=="Y"|subdata$ENTTARGET=="Y"|subdata$OTHTARGET=="Y")
  sub0$TARGET_SIDED <- NA
  
  # Actions (indiscriminate = violence vs. civilians)
  sub0$ACTION_ANY <- 1
  sub0$ACTION_IND <- NA
  sub0$ACTION_SEL <- NA
  #for protest and riot data only
  sub0$ACTION_PRT <- 1
  
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
  summary(events)
  
  # Save
  save(events,file=paste0("Output/Output_BeissingerUkraine/Events/BeissingerUkraine_Events_",countrycode(cntz,origin = "iso3c",destination = "iso3c"),".RData"))
  events
  
})




