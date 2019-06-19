rm(list=ls())


## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
# setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")


## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
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
source("Code/step2_eventcode/step2x_eventType_function.R")
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

# Precision codes
head(data)[,1:20]
data$GEOPRECISION0 <- "settlement"
data$GEOPRECISION0[which((data$Kecamatan1==""|is.na(data$Kecamatan1)))] <- "adm2"
data$TIMEPRECISION0 <- "day"
tail(data)

# By country
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
sub.precis <- subdata$GEOPRECISION0
sub.tprecis <- subdata$TIMEPRECISION0
sub0 <- data.frame(SOURCE=paste0("NMVS_Indonesia"),CONFLICT=countrycode(cntz,"iso3c","country.name"),COWN=countrycode(cntz,origin = "iso3c",destination = "cown"),COWC=countrycode(cntz,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cntz,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long,GEOPRECISION=sub.precis,TIMEPRECISION=sub.tprecis)
head(sub0)

# Actors (based on the article)
sub0$INITIATOR_SIDEA <- 1*(subdata$actor_s1_tp=="5"|subdata$actor_s1_tp=="6"|subdata$actor_s1_tp=="10"|subdata$actor_s1_tp=="14"|subdata$actor_s1_tp=="15"|subdata$actor_s1_tp=="16"|subdata$actor_s1_tp=="19")
sub0$INITIATOR_SIDEB <- 1*(subdata$actor_s1_tp=="3"|subdata$actor_s1_tp=="17")
sub0$INITIATOR_SIDEC <- 1*(subdata$actor_s2_tp=="4")
sub0$INITIATOR_SIDED <- 1*(subdata$actor_s1_tp=="2"|subdata$actor_s1_tp=="7"|subdata$actor_s1_tp=="8"|subdata$actor_s1_tp=="9"|subdata$actor_s1_tp=="11"|subdata$actor_s1_tp=="12"|subdata$actor_s1_tp=="13"|subdata$actor_s1_tp=="18")
sub0$TARGET_SIDEA <- 1*(subdata$actor_s2_tp=="5"|subdata$actor_s2_tp=="6"|subdata$actor_s2_tp=="10"|subdata$actor_s2_tp=="14"|subdata$actor_s2_tp=="15"|subdata$actor_s2_tp=="16"|subdata$actor_s2_tp=="19")
sub0$TARGET_SIDEB <- 1*(subdata$actor_s2_tp=="3"|subdata$actor_s2_tp=="17")
sub0$TARGET_SIDEC <- 1*(subdata$actor_s2_tp=="4")
sub0$TARGET_SIDED <- 1*(subdata$actor_s2_tp=="2"|subdata$actor_s2_tp=="7"|subdata$actor_s2_tp=="8"|subdata$actor_s2_tp=="9"|subdata$actor_s2_tp=="11"|subdata$actor_s2_tp=="12"|subdata$actor_s2_tp=="13"|subdata$actor_s2_tp=="18")

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

# Actions (indiscriminate = violence vs. civilians)
sub0$ACTION_ANY <- 1 
sub0$ACTION_IND <- 1*(subdata$ben_kek1%in%c(2,6,9))
sub0$ACTION_DIR <- 1*(subdata$ben_kek1%in%c(7,8,10,11,13,14))
sub0$ACTION_PRT <- 1*(subdata$ben_kek1%in%c(3,5))

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


# EventType
source("Code/step2_eventcode/step2x_event_types_list.R")
types.specific
events0 <- as.data.frame(matrix(0,nrow=nrow(events),ncol=length(types.specific)))
names(events0) <- paste0("ACTION_",types.specific)
events0 <- cbind(data.frame(ID_TEMP=1:nrow(events)),events0)
names(events0)
head(subdata)
events0$ACTION_FIREFIGHT <- 1*(subdata$wpn.fire>0) 
events0$ACTION_KIDNAP <- 1*(subdata$kidnap_tot>0) 
events0$ACTION_PROPERTY <- 1*(apply(subdata[,c("build_dmg_total", "bdg_des")],1,function(x){sum(x, na.rm=T)>0})|subdata$ben_kek1%in%c(10,14))
events0$ACTION_KILLING <- 1*apply(subdata[,c("kil_total","kil_f")],1,function(x){sum(x, na.rm=T)>0})
events0$ACTION_TERROR <- 1*(subdata$ben_kek1%in%c(9))
events0$ACTION_SIEGE <- 1*(subdata$ben_kek1%in%c(4))
events0$ACTION_ROBBERY <- 1*(subdata$ben_kek1%in%c(14))
events0$ACTION_RIOT <- 1*(subdata$ben_kek1%in%c(5))
events0$ACTION_PROTEST <- 1*(subdata$ben_kek1%in%c(3))
events0$ACTION_RAID <- 1*(subdata$ben_kek1%in%c(6:8,11))
events0$ACTION_STORM <- 1*(subdata$ben_kek1%in%c(2))
head(events0)

# Save
save(events,file=paste0("Output/Output_NVMS/Events/NVMS_Events_",countrycode(cntz,origin = "iso3c",destination = "iso3c"),".RData"))
events0 <- events0[,names(events0)[!names(events0)%in%names(events)]]
save(events0,file=paste0("Output/Output_NVMS/Events/EventType/NVMS_EventType_",disag[j],".RData")) 
summary(events)


