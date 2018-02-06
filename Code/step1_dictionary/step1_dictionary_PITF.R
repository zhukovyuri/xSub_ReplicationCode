rm(list=ls())

## Set directory
#setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")



#############################
## Clean up data
#############################

# Load events
#1995-2012
pitf.raw <- read.csv("Input/Events/PITF/pitf_1995_2012.csv")
pitf.raw[pitf.raw$country=="CAR",] <- pitf.raw[pitf.raw$country=="CAF",]
pitf.raw[pitf.raw$country=="GZS",] <- pitf.raw[pitf.raw$country=="PSE",]
pitf.raw[pitf.raw$country=="IRQ ",] <- pitf.raw[pitf.raw$country=="IRQ",]
pitf.raw[pitf.raw$country=="NGR",] <- pitf.raw[pitf.raw$country=="NGA",]
pitf.raw[pitf.raw$country=="SOM ",] <- pitf.raw[pitf.raw$country=="SOM",]
pitf.raw[pitf.raw$country=="SYR ",] <- pitf.raw[pitf.raw$country=="SYR",]
pitf.raw[pitf.raw$country=="SUD",] <- pitf.raw[pitf.raw$country=="SDN",]
pitf.raw[pitf.raw$country=="THL",] <- pitf.raw[pitf.raw$country=="THA",]
pitf.raw[pitf.raw$country=="TMP",] <- pitf.raw[pitf.raw$country=="TLS",]
tail(pitf.raw)
colnames(pitf.raw)

load("Dictionaries/PITF/PITF_1995_2012/PITF_PSE_Actors.RData")

#2013-2015
pitf.raw <- read.csv("Input/Events/PITF/pitf_2013_2015.csv")
pitf.raw[pitf.raw$country=="ALG",] <- pitf.raw[pitf.raw$country=="DZA",]
pitf.raw[pitf.raw$country=="BRZ",] <- pitf.raw[pitf.raw$country=="BRA",]
pitf.raw[pitf.raw$country=="ELS",] <- pitf.raw[pitf.raw$country=="SLV",]  
pitf.raw[pitf.raw$country=="SYR  ",] <- pitf.raw[pitf.raw$country=="SYR",] 
pitf.raw[pitf.raw$country=="THL",] <- pitf.raw[pitf.raw$country=="THA",] 

#2016
pitf.raw <- read.csv("Input/Events/PITF/pitf.world.20160101-20161231.csv") 

# Clean up
countrylist <- sort(unique(as.character(pitf.raw$country)))
countrylist <- data.frame(country=countrylist,iso3=countrylist)
for(j in 1:2){countrylist[,j]<-as.character(countrylist[,j])}

#for the second dataset only
# countrylist[countrylist$country=="\nIRQ","iso3"] <- countrycode("Iraq","country.name","iso3c")
# countrylist[countrylist$country=="\nYEM","iso3"] <- countrycode("Yemen","country.name","iso3c")
# countrylist[countrylist$country=="\nIRQ","country"] <- countrycode("Iraq","country.name","iso3c")
# countrylist[countrylist$country=="\nYEM","country"] <- countrycode("Yemen","country.name","iso3c")
# pitf.raw[pitf.raw$country=="\nIRQ","iso3"] <- countrycode("Iraq","country.name","iso3c")
# pitf.raw[pitf.raw$country=="\nYEM","iso3"] <- countrycode("Yemen","country.name","iso3c")
# pitf.raw[pitf.raw$country=="\nIRQ","country"] <- countrycode("Iraq","country.name","iso3c")
# pitf.raw[pitf.raw$country=="\nYEM","country"] <- countrycode("Yemen","country.name","iso3c")


pitf.raw <- merge(pitf.raw,countrylist,by="country",all.x=T,all.y=T)

#1995-2012
save(pitf.raw,file="Input/Events/PITF/pitf_1995_2012.RData")

#2013-2015
save(pitf.raw,file="Input/Events/PITF/pitf_2013_2015.RData")

#2016
save(pitf.raw,file="Input/Events/PITF/pitf_20160101_20161231.RData")

#############################
## Create actor dictionary
#############################

# Loop by country (first line finds k where you left off)
k0 <- max(which(countrylist$iso3%in%gsub("PITF_|_Actors.RData","",dir("Dictionaries/PITF/"))))+1
k0
for(k in 1:nrow(countrylist)){
  subdata <- pitf.raw[pitf.raw$country%in%c(countrylist[k,"country"]),]
  print(countrylist[k,"country"])
  actorlist <- actorList(subdata,sidea="Perp.State.Role",sideb="Perp.State.Role",timevar="year",countryvar="country")
  save(actorlist,file=paste0("Dictionaries/PITF/PITF_",countrylist[k,"iso3"],"_Actors.RData"))
}

