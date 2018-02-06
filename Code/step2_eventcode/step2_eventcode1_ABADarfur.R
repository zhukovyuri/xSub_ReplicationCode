rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")
#setwd("~/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("lubricate", "gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
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
load("Input/Events/ABA/ABA_GEO.RData")

# Subset
subdata <- data
head(subdata)
tail(subdata)

# Fix dates
bad.dates <- which(subdata$DATE<"20000000")
b.m <- sapply(strsplit(as.character(as.character(subdata$Date[bad.dates])),"/"),"[",1)
b.d <- sapply(strsplit(as.character(as.character(subdata$Date[bad.dates])),"/"),"[",2)
b.y <- sapply(strsplit(as.character(as.character(subdata$Date[bad.dates])),"/"),"[",3)
subdata$DATE[bad.dates] <- paste0(b.y,sprintf("%02d", as.numeric(b.m)),sprintf("%02d", as.numeric(b.d)))


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
sub0 <- data.frame(SOURCE=paste0("ABADarfur"),CONFLICT=countrycode(cntz,"iso3c","country.name"),COWN=countrycode(cntz,origin = "iso3c",destination = "cown"),COWC=countrycode(cntz,origin = "iso3c",destination = "cowc"),ISO3=countrycode(cntz,origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long)
sub0
head(sub0)

# Actors (based on the article)
sub0$INITIATOR_SIDEA <- 1*(subdata$Perp1=="1"|subdata$Perp1=="2"|subdata$Perp1=="4"|subdata$Perp1=="5"|subdata$Perp1=="11")
sub0$INITIATOR_SIDEB <- 1*(subdata$Perp1=="3"|subdata$Perp1=="6"|subdata$Perp1=="7")
sub0$INITIATOR_SIDEC <- NA
sub0$INITIATOR_SIDED <- 1*(subdata$Perp1=="8"|subdata$Perp1=="9"|subdata$Perp1=="10")

#Not clear from the interviews, the data includes only interviewees' ethnicities (mostly Black tribes) but nothing concrete about the victims
sub0$TARGET_SIDEA <- NA
sub0$TARGET_SIDEB <- NA
sub0$TARGET_SIDEC <- NA
sub0$TARGET_SIDED <- NA

# Actions (indiscriminate = violence vs. civilians)
sub0$ACTION_ANY <- 1
sub0$ACTION_IND <- 1*(subdata$Event1=="1"|subdata$Event1=="2"|subdata$Event1=="3"|subdata$Event1=="4"|subdata$Event1=="11"|subdata$Event1=="12"|subdata$Event1=="13"|subdata$Event1=="14"|subdata$Event1=="15"|subdata$Event1=="16"|subdata$Event1=="26"|subdata$Event1=="27"|subdata$Event1=="32"|subdata$Event1=="33"|subdata$Event1=="34"|subdata$Event1=="44"|subdata$Event1=="48"|subdata$Event1=="49"|subdata$Event1=="50"|subdata$Event1=="51"|subdata$Event1=="52"|subdata$Event1=="56")
sub0$ACTION_SEL <- 1*(subdata$Event1=="5"|subdata$Event1=="6"|subdata$Event1=="7"|subdata$Event1=="8"|subdata$Event1=="9"|subdata$Event1=="10"|subdata$Event1=="17"|subdata$Event1=="18"|subdata$Event1=="19"|subdata$Event1=="20"|subdata$Event1=="21"|subdata$Event1=="22"|subdata$Event1=="23"|subdata$Event1=="25"|subdata$Event1=="36"|subdata$Event1=="40"|subdata$Event1=="41"|subdata$Event1=="42"|
                        subdata$Event1=="43"|subdata$Event1=="46"|subdata$Event1=="47"|subdata$Event1=="53"|subdata$Event1=="54"|subdata$Event1=="55")
sub0$ACTION_PRT <- 0
summary(sub0)

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
head(sub0)
summary(events)



# Save
save(events,file=paste0("Output/Output_ABADarfur/Events/ABADarfur_Events_",countrycode(cntz,origin = "iso3c",destination = "iso3c"),".RData"))
head(events)
tail(events)


