rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")
# setwd("~/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("lubricate", "gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


#############################
## Creat event-level data
#############################
## NVMS: Indonesia
#############################

## Load custom functions
source("Code/functions.R")

# Load events
load("Input/Events/NVMS/NVMS_GEO.RData")
source("Code/step2_eventcode/step2x_event_types_list.R")
head(nvms.raw)
tail(nvms.raw)
colnames(nvms.raw)
dim(nvms.raw) ##241849 by 104

#Create the DATE column
data <- nvms.raw
data$DATE <- as.Date(data$tanggal_Kejadian,"%m/%d/%Y")
data$DATE <- gsub("-", "", data$DATE)
head(data)
tail(data)

cntz <- "IDN"
subdata <- data
head(subdata)
tail(subdata)

## By country
disag <- sort(unique(subdata$ISO3))
j <- 1; disag[j]

# Dates & locations
sub.datez <- as.numeric(as.character(subdata$DATE)) #*10000+as.numeric(as.character(subdata$Month))*100+as.numeric(as.character(subdata$Calendar.Day...End))
sub.lat <- subdata$LAT
sub.long <- subdata$LONG
sub0 <- data.frame(SOURCE=paste0("NMVS_Indonesia"),CONFLICT=countrycode(cntz,"iso3c","country.name"),COWN=countrycode(cntz,origin = "iso3c",destination = "cown"),COWC=countrycode(cntz,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cntz,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long)
sub0
head(sub0)

# Actors (based on the article)
sub0$INITIATOR_SIDEA <- 1*(subdata$actor_s1_tp=="5"|subdata$actor_s1_tp=="6"|subdata$actor_s1_tp=="10"|subdata$actor_s1_tp=="14"|subdata$actor_s1_tp=="15"|subdata$actor_s1_tp=="16"|subdata$actor_s1_tp=="19")
sub0$INITIATOR_SIDEB <- 1*(subdata$actor_s1_tp=="3"|subdata$actor_s1_tp=="17")
sub0$INITIATOR_SIDEC <- NA
sub0$INITIATOR_SIDED <- 1*(subdata$actor_s1_tp=="2"|subdata$actor_s1_tp=="4"|subdata$actor_s1_tp=="7"|subdata$actor_s1_tp=="8"|subdata$actor_s1_tp=="9"|subdata$actor_s1_tp=="11"|subdata$actor_s1_tp=="12"|subdata$actor_s1_tp=="13"|subdata$actor_s1_tp=="18")
sub0$TARGET_SIDEA <- 1*(subdata$actor_s2_tp=="5"|subdata$actor_s2_tp=="6"|subdata$actor_s2_tp=="10"|subdata$actor_s2_tp=="14"|subdata$actor_s2_tp=="15"|subdata$actor_s2_tp=="16"|subdata$actor_s2_tp=="19")
sub0$TARGET_SIDEB <- 1*(subdata$actor_s2_tp=="3"|subdata$actor_s2_tp=="17")
sub0$TARGET_SIDEC <- NA
sub0$TARGET_SIDED <- 1*(subdata$actor_s2_tp=="2"|subdata$actor_s2_tp=="4"|subdata$actor_s2_tp=="7"|subdata$actor_s2_tp=="8"|subdata$actor_s2_tp=="9"|subdata$actor_s2_tp=="11"|subdata$actor_s2_tp=="12"|subdata$actor_s2_tp=="13"|subdata$actor_s2_tp=="18")

# Actions (indiscriminate = violence vs. civilians)
sub0$ACTION_ANY <- 1 
sub0$ACTION_IND <- 1*apply(subdata[,c("kil_total","kil_f","inj_total","inj_f","build_dmg_total", "bdg_des")],1,function(x){sum(x, na.rm=T)>0})
sub0$ACTION_SEL <- 1*apply(subdata[,c("kidnap_tot","kid_f","sex_as_tot","sex_f")],1,function(x){sum(x,na.rm=T)>0})
sub0$ACTION_PRT <- 0

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


# Save
save(events,file=paste0("Output/Output_NVMS/Events/NVMS_Events_",countrycode(cntz,origin = "iso3c",destination = "iso3c"),".RData"))
summary(events)


