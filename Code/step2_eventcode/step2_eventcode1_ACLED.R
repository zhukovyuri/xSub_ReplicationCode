rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")
# setwd("~/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")

# Event types
source("Code/step2_eventcode/step_2x_eventType_function.R")
source("Code/step2_eventcode/step2x_event_types_list.R")


#############################
## Creats event-level data (Africa)
#############################


## Load event type dictionary
load("Dictionaries/EventTypes/Combined_EventDictionary.RData")

## Load raw data
#data <- read.csv("Input/Events/ACLED/ACLED_Asia/acled_Asia_2015-2016.csv",stringsAsFactors=FALSE, fileEncoding="latin1")
data <- read.csv("Input/Events/ACLED/ACLED7_Africa_1997-2016_dyadic.csv",stringsAsFactors=FALSE, fileEncoding="latin1")

classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)
tail(data)

# Asia subset
#acled.asia <- c("BGD","IND","KHM","LAO","LKA","MMR","NPL","PAK","THA","VNM")
cntz <- unique(data$COUNTRY)
i <- 49; cntz[i]


data$ISO3 <- countrycode(data$COUNTRY,origin = "country.name",destination = "iso3c")
sort(unique(data$ISO3))
head(data)
tail(data)
acled.africa <- data$ISO3

# Fix dates
x <- data$EVENT_DATE
x <- strsplit(x,"-|/")
days <- sapply(x,"[",1)
months <- sapply(x,"[",2)
months[!is.na(match(months,month.abb))] <- match(months,month.abb)[!is.na(match(months,month.abb))]
years <- sapply(x,"[",3)
years[nchar(years)==2&years<89] <- paste0("20",years[nchar(years)==2&years<89])
years[nchar(years)==2&years>=89] <- paste0("19",years[nchar(years)==2&years>=89])
years <- as.numeric(years)
days <- as.numeric(days)
months <- as.numeric(months)
data$DATE <- years*10000+months*100+days
range(data$DATE)


## By country
disag <- sort(unique(data$ISO3))
j <- 6; disag[j]

acled.list <- lapply(1:length(disag),function(j){print(j)
  subdata <- data[data$ISO3==disag[j],]
  tail(subdata)
  
  # # Dates & locations
  # #sub.datez <- paste0(sapply(strsplit(subdata$EVENT_DATE,"/"), '[', 3),sapply(strsplit(subdata$EVENT_DATE,"/"), '[', 2),sapply(strsplit(subdata$EVENT_DATE,"/"), '[', 1))
  # #if(countrycode(disag[j],"country.name","iso3c")%in%acled.africa){
  # sub.datez <- as.character(subdata$EVENT_DATE)
  # sub.datez <- gsub("-","",as.Date(sub.datez,"%d-%b-%y"))
  # #}
  
  #for Africa
  sub.datez <- subdata$DATE
  sub.lat <- subdata$LATITUDE
  sub.long <- subdata$LONGITUDE
  sub0 <- data.frame(SOURCE=paste0("ACLED_v7"),CONFLICT=countrycode(disag[j],"iso3c","country.name"),COWN=countrycode(disag[j],origin = "iso3c",destination = "cown"),COWC=countrycode(disag[j],origin = "iso3c",destination = "cowc"),ISO3=countrycode(disag[j],origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long)
  head(sub0)  
  
  # Actors (use pre-existing dictionaries)#Africa
  dir("Dictionaries/ACLED/NK/ACLED_Africa/")
  if(paste0("ACLED_",toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/ACLED/NK/ACLED_Africa/")){load(paste0("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_",toupper(disag[j]),"_Actors.RData"))}
  #if(!paste0(toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/ACLED/NK/ACLED_Africa/")){load(paste0("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_",nam.match[nam.match$n1%in%disag[j],"n2"],"_Actors.RData"))}
  
  # Clean dictionary
  if(length(actorlist$actors_GOV)>0){actorlist$actors_GOV <- gdata:::trim(sapply(strsplit(actorlist$actors_GOV,"\\(Inf|\\(\\d[4] - \\d[4]\\)|\\(\\d[4]-\\d[4]\\)"),"[",1))}
  if(length(actorlist$actors_REB)>0){actorlist$actors_REB <- gdata:::trim(sapply(strsplit(actorlist$actors_REB,"\\(Inf|\\(\\d[4] - \\d[4]\\)|\\(\\d[4]-\\d[4]\\)"),"[",1))}
  if(length(actorlist$actors_CIV)>0){actorlist$actors_CIV <- gdata:::trim(sapply(strsplit(actorlist$actors_CIV,"\\(Inf|\\(\\d[4] - \\d[4]\\)|\\(\\d[4]-\\d[4]\\)"),"[",1))}
  if(length(actorlist$actors_OTH)>0){actorlist$actors_OTH <- gdata:::trim(sapply(strsplit(actorlist$actors_OTH,"\\(Inf|\\(\\d[4] - \\d[4]\\)|\\(\\d[4]-\\d[4]\\)"),"[",1))}
  
  sub0$INITIATOR_SIDEA <- 1*(subdata$ACTOR1%in%actorlist$actors_GOV)
  sub0$INITIATOR_SIDEB <- 1*(subdata$ACTOR1%in%actorlist$actors_REB)
  sub0$INITIATOR_SIDEC <- 1*(subdata$ACTOR1%in%actorlist$actors_CIV)
  sub0$INITIATOR_SIDED <- 1*(subdata$ACTOR1%in%actorlist$actors&(!subdata$ACTOR1%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV)))
  sub0$TARGET_SIDEA <- 1*(subdata$ACTOR2%in%actorlist$actors_GOV)
  sub0$TARGET_SIDEB <- 1*(subdata$ACTOR2%in%actorlist$actors_REB)
  sub0$TARGET_SIDEC <- 1*(subdata$ACTOR2%in%actorlist$actors_CIV)
  sub0$TARGET_SIDED <- 1*(subdata$ACTOR2%in%actorlist$actors&(!subdata$ACTOR1%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV)))
  
  # Actors (use pre-existing dictionaries)#Asia
  #nam.match <- data.frame(n1=c("Bangladesh", "Cambodia", "india", "Laos", "Myanmar", "Nepal", "Pakistan", "Sri Lanka", "Thailand", "Vietnam"),n2=c("BGD","KHM","IND","LAO","MMR","NPL","PAK","LKA","THA","VNM"))
  #dir("Dictionaries/ACLED/NK/ACLED_Asia/")
  #if(paste0(toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/ACLED/NK/ACLED_Asia/")){load(paste0("Dictionaries/ACLED/NK/ACLED_Asia/",toupper(disag[j]),"_Actors.RData"))}
  #if(!paste0(toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/ACLED/NK/ACLED_Asia/")){load(paste0("Dictionaries/ACLED/NK/ACLED_Asia/",nam.match[nam.match$n1%in%disag[j],"n2"],"_Actors.RData"))}
  #sub0$INITIATOR_SIDEA <- 1*(subdata$ACTOR1%in%actorlist$actors_GOV)
  #sub0$INITIATOR_SIDEB <- 1*(subdata$ACTOR1%in%actorlist$actors_REB)
  #sub0$INITIATOR_SIDEC <- 1*(subdata$ACTOR1%in%actorlist$actors_CIV)
  #sub0$INITIATOR_SIDED <- 1*(subdata$ACTOR1%in%actorlist$actors&(!subdata$ACTOR1%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV)))
  #sub0$TARGET_SIDEA <- 1*(subdata$ACTOR2%in%actorlist$actors_GOV)
  #sub0$TARGET_SIDEB <- 1*(subdata$ACTOR2%in%actorlist$actors_REB)
  #sub0$TARGET_SIDEC <- 1*(subdata$ACTOR2%in%actorlist$actors_CIV)
  #sub0$TARGET_SIDED <- 1*(subdata$ACTOR2%in%actorlist$actors&(!subdata$ACTOR1%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV)))
  
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
  
  # Actions
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
  head(events)
  # Multi-day events
  # not needed, because all ACLED events are single-day
  
  # Save 
  save(events,file=paste0("Output/Output_ACLED/Events/ACLED_Events_",countrycode(disag[j],origin = "iso3c",destination = "iso3c"),".RData"))  
  sub0
  
})
acled.mat <- do.call(rbind,acled.list)
summary(acled.mat)
 

# # Save to file
# events <- acled.mat
# save(events,file=paste0("Output/Output_ACLED/Events/ACLED_Events_000.RData"))  






#############################
## Creats event-level data (Asia)
#############################

rm(list=ls())
## Load custom functions
source("Code/functions.R")
# Event types
source("Code/step2_eventcode/step_2x_eventType_function.R")
source("Code/step2_eventcode/step2x_event_types_list.R")


## Load event type dictionary
load("Dictionaries/EventTypes/Combined_EventDictionary.RData")

## Load raw data
data <- read.csv("Input/Events/ACLED/ACLED_Asia/acled_Asia_2015-2016.csv",stringsAsFactors=FALSE, fileEncoding="latin1")
# data <- read.csv("Input/Events/ACLED/ACLED7_Africa_1997-2016_dyadic.csv",stringsAsFactors=FALSE, fileEncoding="latin1")

classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)
tail(data)

# Asia subset
#acled.asia <- c("BGD","IND","KHM","LAO","LKA","MMR","NPL","PAK","THA","VNM")
cntz <- unique(data$COUNTRY)
i <- 1; cntz[i]


data$ISO3 <- countrycode(data$COUNTRY,origin = "country.name",destination = "iso3c")
sort(unique(data$ISO3))
head(data)
tail(data)

# Fix dates
x <- data$EVENT_DATE
x <- strsplit(x,"-|/")
days <- sapply(x,"[",1)
months <- sapply(x,"[",2)
months[!is.na(match(months,month.abb))] <- match(months,month.abb)[!is.na(match(months,month.abb))]
years <- sapply(x,"[",3)
years[nchar(years)==2&years<89] <- paste0("20",years[nchar(years)==2&years<89])
years[nchar(years)==2&years>=89] <- paste0("19",years[nchar(years)==2&years>=89])
years <- as.numeric(years)
days <- as.numeric(days)
months <- as.numeric(months)
data$DATE <- years*10000+months*100+days
range(data$DATE)

## By country
disag <- sort(unique(data$ISO3))
j <- 1; disag[j]

acled.list <- lapply(1:length(disag),function(j){print(j)
  subdata <- data[data$ISO3==disag[j],]
  head(subdata)
  
  #for Asia
  sub.datez <- subdata$DATE
  sub.lat <- subdata$LATITUDE
  sub.long <- subdata$LONGITUDE
  sub0 <- data.frame(SOURCE=paste0("ACLED_v7"),CONFLICT=countrycode(disag[j],"iso3c","country.name"),COWN=countrycode(disag[j],origin = "iso3c",destination = "cown"),COWC=countrycode(disag[j],origin = "iso3c",destination = "cowc"),ISO3=countrycode(disag[j],origin = "iso3c",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long)
  head(sub0)  
  
  # Actors (use pre-existing dictionaries) #Asia
  dir("Dictionaries/ACLED/NK/ACLED_Asia/")
  if(paste0("ACLED_",toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/ACLED/NK/ACLED_Asia/")){load(paste0("Dictionaries/ACLED/NK/ACLED_Asia/ACLED_",toupper(disag[j]),"_Actors.RData"))}
  #if(!paste0(toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/ACLED/NK/ACLED_Africa/")){load(paste0("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_",nam.match[nam.match$n1%in%disag[j],"n2"],"_Actors.RData"))}
  
  # Clean dictionary
  if(length(actorlist$actors_GOV)>0){actorlist$actors_GOV <- gdata:::trim(sapply(strsplit(actorlist$actors_GOV,"\\(Inf|\\(\\d[4] - \\d[4]\\)|\\(\\d[4]-\\d[4]\\)"),"[",1))}
  if(length(actorlist$actors_REB)>0){actorlist$actors_REB <- gdata:::trim(sapply(strsplit(actorlist$actors_REB,"\\(Inf|\\(\\d[4] - \\d[4]\\)|\\(\\d[4]-\\d[4]\\)"),"[",1))}
  if(length(actorlist$actors_CIV)>0){actorlist$actors_CIV <- gdata:::trim(sapply(strsplit(actorlist$actors_CIV,"\\(Inf|\\(\\d[4] - \\d[4]\\)|\\(\\d[4]-\\d[4]\\)"),"[",1))}
  if(length(actorlist$actors_OTH)>0){actorlist$actors_OTH <- gdata:::trim(sapply(strsplit(actorlist$actors_OTH,"\\(Inf|\\(\\d[4] - \\d[4]\\)|\\(\\d[4]-\\d[4]\\)"),"[",1))}
  
  sub0$INITIATOR_SIDEA <- 1*(subdata$ACTOR1%in%actorlist$actors_GOV)
  sub0$INITIATOR_SIDEB <- 1*(subdata$ACTOR1%in%actorlist$actors_REB)
  sub0$INITIATOR_SIDEC <- 1*(subdata$ACTOR1%in%actorlist$actors_CIV)
  sub0$INITIATOR_SIDED <- 1*(subdata$ACTOR1%in%actorlist$actors&(!subdata$ACTOR1%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV)))
  sub0$TARGET_SIDEA <- 1*(subdata$ACTOR2%in%actorlist$actors_GOV)
  sub0$TARGET_SIDEB <- 1*(subdata$ACTOR2%in%actorlist$actors_REB)
  sub0$TARGET_SIDEC <- 1*(subdata$ACTOR2%in%actorlist$actors_CIV)
  sub0$TARGET_SIDED <- 1*(subdata$ACTOR2%in%actorlist$actors&(!subdata$ACTOR1%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV)))
  
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
  
  # Actions
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
  summary(events)
  # Multi-day events
  # not needed, because all ACLED events are single-day
  
  # Save 
  save(events,file=paste0("Output/Output_ACLED/Events/ACLED_Events_",countrycode(disag[j],origin = "iso3c",destination = "iso3c"),".RData"))  
  sub0
  
})



#############################
## Creat event-level data (India)
#############################


rm(list=ls())
## Load custom functions
source("Code/functions.R")
# Event types
source("Code/step2_eventcode/step_2x_eventType_function.R")
source("Code/step2_eventcode/step2x_event_types_list.R")

## Load event type dictionary
load("Dictionaries/EventTypes/Combined_EventDictionary.RData")

## Load raw data
data <- read.csv("Input/Events/ACLED/ACLED_Asia/India-ACLED-Asia-2015-2016.csv") #,stringsAsFactors=FALSE, fileEncoding="latin1")
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)

# Asia subset
acled.asia <- "IND"

## By country
disag <- sort(unique(data$COUNTRY))
j <- 1; disag[j]

#acled.list <- lapply(1:length(disag),function(j){print(j)
subdata <- data[data$COUNTRY==disag[j],]
tail(subdata)

# Dates & locations
sub.datez <- paste0(sapply(strsplit(subdata$EVENT_DATE,"/"), '[', 3),sapply(strsplit(subdata$EVENT_DATE,"/"), '[', 2),sapply(strsplit(subdata$EVENT_DATE,"/"), '[', 1))
if(countrycode(disag[j],"country.name","iso3c")%in%acled.asia){
  sub.datez <- as.character(subdata$EVENT_DATE)
  sub.datez <- gsub("-","",as.Date(sub.datez,"%d-%b-%y"))
}
sub.lat <- subdata$LATITUDE
sub.long <- subdata$LONGITUDE
sub0 <- data.frame(SOURCE=paste0("ACLED_v7_"),CONFLICT=disag[j],COWN=countrycode(disag[j],origin = "country.name",destination = "cown"),COWC=countrycode(disag[j],origin = "country.name",destination = "cowc"),ISO3=countrycode(disag[j],origin = "country.name",destination = "iso3c"),DATE=sub.datez,LAT=sub.lat,LONG=sub.long)
head(sub0)  
tail(sub0)


# Actors (use pre-existing dictionaries)#Africa
#nam.match <- data.frame(n1=c("Burkina Faso","Central African Republic","Democratic Republic of Congo","Equatorial Guinea","Guinea-Bissau","Ivory Coast","Republic of Congo","Sierra Leone","South Africa","South Sudan"),n2=c("BURKINA","CAR","DRC","EQGUINEA","GUINEABISSAU","IVORYCOAST","CONGO","SIERRALEONE","SOUTHAFRICA","SUDAN"))
#dir("Dictionaries/ACLED/Preexisting")
#if(paste0(toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/ACLED/Preexisting/")){load(paste0("Dictionaries/ACLED/Preexisting/",toupper(disag[j]),"_Actors.RData"))}
#if(!paste0(toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/ACLED/Preexisting/")){load(paste0("Dictionaries/ACLED/Preexisting/",nam.match[nam.match$n1%in%disag[j],"n2"],"_Actors.RData"))}
#sub0$INITIATOR_SIDEA <- 1*(subdata$ACTOR1%in%actors_GOV)
#sub0$INITIATOR_SIDEB <- 1*(subdata$ACTOR1%in%actors_REB)
#sub0$INITIATOR_SIDEC <- 1*(subdata$ACTOR1%in%actors_CIV)
#sub0$INITIATOR_SIDED <- 1*(subdata$ACTOR1%in%actors&(!subdata$ACTOR1%in%c(actors_GOV,actors_REB,actors_CIV)))
#sub0$TARGET_SIDEA <- 1*(subdata$ACTOR2%in%actors_GOV)
#sub0$TARGET_SIDEB <- 1*(subdata$ACTOR2%in%actors_REB)
#sub0$TARGET_SIDEC <- 1*(subdata$ACTOR2%in%actors_CIV)
#sub0$TARGET_SIDED <- 1*(subdata$ACTOR2%in%actors&(!subdata$ACTOR1%in%c(actors_GOV,actors_REB,actors_CIV)))

# Actors (use pre-existing dictionaries)#Asia
#nam.match <- data.frame(n1=c("India"))
dir("Dictionaries/ACLED/NK/ACLED_Asia/")
#if(paste0(toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/ACLED/NK/ACLED_Asia/")){load(paste0("Dictionaries/ACLED/NK/ACLED_Asia/",toupper(disag[j]),"_Actors.RData"))}
#if(!paste0(toupper(disag[j]),"_Actors.RData")%in%dir("Dictionaries/ACLED/NK/ACLED_Asia/")){load(paste0("Dictionaries/ACLED/NK/ACLED_Asia/",nam.match[nam.match$n1%in%disag[j],"n2"],"_Actors.RData"))}
load("Dictionaries/ACLED/NK/ACLED_Asia/ACLED_IND_Actors.RData")
actorlist

##deleting extra portion from the actorlist
tt = lapply(actorlist, function(x) gsub(" \\(Inf - -Inf\\) India", "", x))
actorlist <- tt
actorlist

sub0$INITIATOR_SIDEA <- 1*(subdata$ACTOR1%in%actorlist$actors_GOV)
sub0$INITIATOR_SIDEB <- 1*(subdata$ACTOR1%in%actorlist$actors_REB)
sub0$INITIATOR_SIDEC <- 1*(subdata$ACTOR1%in%actorlist$actors_CIV)
sub0$INITIATOR_SIDED <- 1*(subdata$ACTOR1%in%actorlist$actors&(!subdata$ACTOR1%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV)))
sub0$TARGET_SIDEA <- 1*(subdata$ACTOR2%in%actorlist$actors_GOV)
sub0$TARGET_SIDEB <- 1*(subdata$ACTOR2%in%actorlist$actors_REB)
sub0$TARGET_SIDEC <- 1*(subdata$ACTOR2%in%actorlist$actors_CIV)
sub0$TARGET_SIDED <- 1*(subdata$ACTOR2%in%actorlist$actors&(!subdata$ACTOR1%in%c(actorlist$actors_GOV,actorlist$actors_REB,actorlist$actors_CIV)))
head(sub0)
tail(sub0)

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
head(events)
# Multi-day events
# not needed, because all ACLED events are single-day

# Save 
save(events,file=paste0("Output/Output_ACLED/Events/ACLED_Events_",countrycode(disag[j],origin = "country.name",destination = "iso3c"),".RData"))  
sub0[1:20,]

#})
# acled.mat <- do.call(rbind,acled.list)
# summary(acled.mat)
# 
# # Save to file
# events <- acled.mat
# save(events,file=paste0("Output/Output_ACLED/Events/ACLED_Events_000.RData"))  

