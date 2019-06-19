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
## ABA: Darfur
#############################

## Load custom functions
source("Code/functions.R")

# # Load events
# load("Input/Events/ABA/ABA_GEO.RData")
# aba.raw <- data; rm(data)
# head(aba.raw)
# 
# #aba.raw <- read.csv("Input/Events/ABA/ABA_Database.csv", header=T)
# source("Code/step2_eventcode/step2x_event_types_list.R")
# head(aba.raw)
# tail(aba.raw)
# colnames(aba.raw)
# dim(aba.raw) 
# 
#Create the DATE column; three different date formats; 10 columns with dates
# multidate <- function(data, formats){
#   a<-list()
#   for(i in 1:length(formats)){
#     a[[i]]<- as.Date(data$Date1,format=formats[i])
#     a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
#   }
#   a[[1]]
# }
# 
# data$Date1 <- multidate(data$Date1, 
#                                   c("%m/%d/%Y","%m%d%Y","%Y%m%d"))

# library(lubridate)
# data$DATE <- as.character(data$DATE)
# data$DATE[1:600]
# # mdy <- mdy(data$DATE)
# ymd <- ymd(data$DATE)
# mdy[is.na(mdy)] <- ymd[!is.na(ymd)]
# data$DATE <- mdy
# data$DATE <- gsub("-","", data$DATE)
# data$DATE[19800:21000]

#save(data, file="Input/Events/ABA/ABA_GEO.RData")
load("Input/Events/ABA/ABA_GEO_v2.RData")
data <- aba.raw
head(data)
plot(data$LONG,data$LAT)

# Event codes
ecodes <- read.csv("Input/Events/ABA/ABA_eventcodes.csv")
ecodes$EVENT <- gdata::trim(sapply(strsplit(as.character(ecodes$EVENT),"-"),"[",1))
head(ecodes)
data$ECODE <- ecodes$EVENT[match(data$Event1,ecodes$CODE)]
head(data)

# Fix dates
data$TPRECIS <- "day"
data$DATE[as.numeric(data$DATE)<20000000] <- NA
data$DATE <- gdata::trim(as.character(data$DATE))
data$Date <- gdata::trim(as.character(data$Date))
data$DATE2 <- as.numeric(as.character(data$DATE))
monthz <- as.numeric(sapply(strsplit(data$DATE[is.na(data$DATE2)],"/"),"[",1))
dayz <- as.numeric(sapply(strsplit(data$DATE[is.na(data$DATE2)],"/"),"[",2))
yearz <- as.numeric(sapply(strsplit(data$DATE[is.na(data$DATE2)],"/"),"[",3))
data$DATE2[is.na(data$DATE2)] <- yearz*10000+monthz*100+dayz
monthz <- as.numeric(sapply(strsplit(data$Date[is.na(data$DATE2)],"/"),"[",1))
monthz[nchar(monthz)>2] <- NA
dayz <- as.numeric(sapply(strsplit(data$Date[is.na(data$DATE2)],"/"),"[",2))
yearz <- as.numeric(sapply(strsplit(data$Date[is.na(data$DATE2)],"/"),"[",3))
data$DATE2[is.na(data$DATE2)] <- yearz*10000+monthz*100+dayz
monthz <- as.numeric(substr(data$Date[is.na(data$DATE2)],1,1))
dayz <- as.numeric(substr(data$Date[is.na(data$DATE2)],2,3))
yearz <- substr(data$Date[is.na(data$DATE2)],4,5)
yearz <- as.numeric(paste0("20",yearz))
data$DATE2[is.na(data$DATE2)] <- yearz*10000+monthz*100+dayz
data$DATE <- data$DATE2; data$DATE2 <- NULL
range(data$DATE)


# Precision codes
head(data)
data$GEOPRECISION0 <- data$GEOPRECIS
data$TIMEPRECISION0 <- "day"
head(data)

# Subset
subdata <- data
head(subdata)
tail(subdata)

## By country
subdata$ISO3 <- "SDN"
cntz <- "SDN"
#disag <- subdata$ISO3
#j <- 1; disag[j]

# subdata <- subdata[subdata$ISO3==disag[j],]
# head(subdata)

# Dates & locations
unique(subdata$DATE)
sort(as.numeric(subdata$DATE))

sub.datez <- as.numeric(as.character(subdata$DATE)) #*10000+as.numeric(as.character(subdata$Month))*100+as.numeric(as.character(subdata$Calendar.Day...End))
sub.lat <- subdata$LAT
sub.long <- subdata$LONG
sub.precis <- subdata$GEOPRECISION0
sub.tprecis <- subdata$TIMEPRECISION0
sub0 <- data.frame(SOURCE=paste0("ABADarfur"),CONFLICT=countrycode(cntz,"iso3c","country.name"),COWN=countrycode(cntz,origin = "iso3c",destination = "cown"),COWC=countrycode(cntz,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cntz,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long,GEOPRECISION=sub.precis,TIMEPRECISION=sub.tprecis)
tail(sub0)

# Actors (based on the article)
sub0$INITIATOR_SIDEA <- 1*(subdata$Perp1%in%"1"|subdata$Perp1%in%"2"|subdata$Perp1%in%"4"|subdata$Perp1%in%"5"|subdata$Perp1%in%"11")
sub0$INITIATOR_SIDEB <- 1*(subdata$Perp1%in%"3"|subdata$Perp1%in%"6"|subdata$Perp1%in%"7")
sub0$INITIATOR_SIDEC <- 0
sub0$INITIATOR_SIDED <- 1*(subdata$Perp1%in%"8"|subdata$Perp1%in%"9"|subdata$Perp1%in%"10")

#Not clear from the interviews, the data includes only interviewees' ethnicities (mostly Black tribes) but nothing concrete about the victims
sub0$TARGET_SIDEA <- 0
sub0$TARGET_SIDEB <- 1*(subdata$VillDef%in%c(2,3)|subdata$RebelAct%in%c(2,3))
sub0$TARGET_SIDEC <- 1*(subdata$VillDef%in%c(1)|subdata$RebelAct%in%c(1))
sub0$TARGET_SIDED <- 0

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
subdata$summary <- tolower(as.character(subdata$ECODE))
textvar <- "summary"
idvar <- "ID_TEMP"
events0 <- eventType(subdata=subdata,idvar=idvar,textvar=textvar,term.type=term.type,types.specific=types.specific,types.general=types.general)
summary(events0)
head(events0)
head(subdata)

# Actions
sub0$ACTION_ANY <- events0$ACTION_ANY
sub0$ACTION_IND <- 1*(events0$ACTION_IND|grepl("^Aerial|^Burning|^Collat|^Setting|^Sepa|^Well|^Targ",subdata$ECODE))
sub0$ACTION_DIR <- 1*(events0$ACTION_DIR|grepl("^Abduct|^Ampu|^Beat|^Desec|^Disem|^Force|Kill|^Knif|^Prop|Rape|^Sexua|^Shoo|^Whi",subdata$summary))
sub0$ACTION_PRT <- events0$ACTION_PRT

# Specific actions
sort(unique(subdata$ECODE))
events0$ACTION_AIRSTRIKE <- 1*(events0$ACTION_AIRSTRIKE==1|grepl("^Aerial",subdata$ECODE))
events0$ACTION_CIV_ABUSE <- 1*(events0$ACTION_CIV_ABUSE==1|grepl("^Amput|Abort|^Burning|^Collat|^Setting|^Sepa|^Well|^Targ|Rape|rape|Sexual|sexual|Whip",subdata$ECODE))
events0$ACTION_DISPLACE <- 1*(events0$ACTION_DISPLACE==1|grepl("Dispa|displa",subdata$ECODE))
events0$ACTION_FIREFIGHT <- 1*(events0$ACTION_FIREFIGHT==1|grepl("Shooting",subdata$ECODE))
events0$ACTION_KIDNAP <- 1*(events0$ACTION_KIDNAP==1|grepl("^Abduct",subdata$ECODE))
events0$ACTION_KILLING <- 1*(events0$ACTION_KILLING==1|grepl("Kill|kill|Knif",subdata$ECODE))
events0$ACTION_PROPERTY <- 1*(events0$ACTION_PROPERTY==1|grepl("Proper|proper",subdata$ECODE))

# # Actions
# sub0$ACTION_ANY <- 1
# sub0$ACTION_IND <- 1*(subdata$Event1%in%"1"|subdata$Event1%in%"2"|subdata$Event1%in%"3"|subdata$Event1%in%"4"|subdata$Event1%in%"11"|subdata$Event1%in%"12"|subdata$Event1%in%"13"|subdata$Event1%in%"14"|subdata$Event1%in%"15"|subdata$Event1%in%"16"|subdata$Event1%in%"26"|subdata$Event1%in%"27"|subdata$Event1%in%"32"|subdata$Event1%in%"33"|subdata$Event1%in%"34"|subdata$Event1%in%"44"|subdata$Event1%in%"48"|subdata$Event1%in%"49"|subdata$Event1%in%"50"|subdata$Event1%in%"51"|subdata$Event1%in%"52"|subdata$Event1%in%"56")
# sub0$ACTION_DIR <- 1*(subdata$Event1%in%"5"|subdata$Event1%in%"6"|subdata$Event1%in%"7"|subdata$Event1%in%"8"|subdata$Event1%in%"9"|subdata$Event1%in%"10"|subdata$Event1%in%"17"|subdata$Event1%in%"18"|subdata$Event1%in%"19"|subdata$Event1%in%"20"|subdata$Event1%in%"21"|subdata$Event1%in%"22"|subdata$Event1%in%"23"|subdata$Event1%in%"25"|subdata$Event1%in%"36"|subdata$Event1%in%"40"|subdata$Event1%in%"41"|subdata$Event1%in%"42"|
#                         subdata$Event1%in%"43"|subdata$Event1%in%"46"|subdata$Event1%in%"47"|subdata$Event1%in%"53"|subdata$Event1%in%"54"|subdata$Event1%in%"55")
# sub0$ACTION_PRT <- 0
summary(sub0)

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
head(sub0)
summary(events)
summary(events0)


# Save
save(events,file=paste0("Output/Output_ABADarfur/Events/ABADarfur_Events_",countrycode(cntz,origin = "iso3c",destination = "iso3c"),".RData"))
events0 <- events0[,names(events0)[!names(events0)%in%names(events)]]
save(events0,file=paste0("Output/Output_ABADarfur/Events/EventType/ABADarfur_EventType_",countrycode(cntz,origin = "iso3c",destination = "iso3c"),".RData")) 

