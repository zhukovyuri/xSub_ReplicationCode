rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")



#############################
## Clean up data
#############################

data1 <- read.csv("Input/Events/PITF/pitf_1995_2012.csv")
data1$X <- data1$X.1 <- NULL
data2 <- read.csv("Input/Events/PITF/pitf_2013_2015.csv")
data2$X <- data2$X.1 <- NULL
data3 <- read.csv("Input/Events/PITF/pitf.world.20160101-20170930.csv")
data3$X <- data3$X.1 <- NULL

# Make columns consistent
names(data1)[!names(data1)%in%names(data2)]
names(data2)[!names(data2)%in%names(data1)]
# names(data2)[!names(data2)%in%names(data1)] <- names(data1)[!names(data1)%in%names(data2)]
names(data1)[!names(data1)%in%names(data3)]
names(data3)[!names(data3)%in%names(data1)]
names(data3)[!names(data3)%in%names(data1)] <- names(data1)[!names(data1)%in%names(data3)]
names(data2)[!names(data2)%in%names(data3)]
names(data3)[!names(data3)%in%names(data2)]

# Fix column formats
lvarz <- names(data1)[grepl("LAT_|LONG_",names(data1))&(!grepl("Direction$",names(data1)))]
for(j in seq_along(lvarz)){
  data1[,lvarz[j]] <- as.numeric(as.character(data1[,lvarz[j]]))
  data2[,lvarz[j]] <- as.numeric(as.character(data2[,lvarz[j]]))
  data3[,lvarz[j]] <- as.numeric(as.character(data3[,lvarz[j]]))
}
classez<-c();for(j in 1:ncol(data1)){classez[j]<-class(data1[,j]);if(classez[j]=="factor"){data1[,j]<-as.character(data1[,j])}}
classez<-c();for(j in 1:ncol(data2)){classez[j]<-class(data2[,j]);if(classez[j]=="factor"){data2[,j]<-as.character(data2[,j])}}
classez<-c();for(j in 1:ncol(data3)){classez[j]<-class(data3[,j]);if(classez[j]=="factor"){data3[,j]<-as.character(data3[,j])}}

# Merge
commonvars <- intersect(names(data1),names(data2))
commonvars <- intersect(commonvars,names(data3))
data <- rbind(data1[,commonvars],data2[,commonvars],data3[,commonvars])
head(data); rm(data1,data2,data3,commonvars)

# Country codes
countrylist <- sort(unique(as.character(data$country)))
countrylist <- data.frame(country=countrylist,iso3=countrycode(countrylist,origin="iso3c",destination="iso3c"))
for(j in 1:2){countrylist[,j]<-as.character(countrylist[,j])}
countrylist[is.na(countrylist$iso3),]
countrylist[countrylist$country=="ALG","iso3"] <- countrycode("Algeria","country.name","iso3c")
countrylist[countrylist$country=="BRZ","iso3"] <- countrycode("Brazil","country.name","iso3c")
countrylist[countrylist$country=="CAR","iso3"] <- countrycode("Central African Republic","country.name","iso3c")
countrylist[countrylist$country=="ELS","iso3"] <- countrycode("El Salvador","country.name","iso3c")
countrylist[countrylist$country=="GZS","iso3"] <- countrycode("Gaza","country.name","iso3c")
countrylist[countrylist$country=="\nIRQ","iso3"] <- countrycode("Iraq","country.name","iso3c")
countrylist[countrylist$country=="IRQ ","iso3"] <- countrycode("Iraq","country.name","iso3c")
countrylist[countrylist$country=="NGR","iso3"] <- countrycode("Nigeria","country.name","iso3c")
countrylist[countrylist$country=="SOM ","iso3"] <- countrycode("Somalia","country.name","iso3c")
countrylist[countrylist$country=="SUD","iso3"] <- countrycode("Sudan","country.name","iso3c")
countrylist[countrylist$country=="SYR ","iso3"] <- countrycode("Syria","country.name","iso3c")
countrylist[countrylist$country=="THL","iso3"] <- countrycode("Thailand","country.name","iso3c")
countrylist[countrylist$country=="\nYEM","iso3"] <- countrycode("Yemen","country.name","iso3c")
pitf.raw <- merge(data,countrylist,by="country",all.x=T,all.y=T)
pitf.raw <- pitf.raw[!is.na(pitf.raw$iso3),]
# save(pitf.raw,file="Input/Events/PITF/pitf_1995_2017.RData")


#############################
## Create actor dictionary
#############################

countrylist <- countrylist[!is.na(countrylist$iso3),]
dir("Dictionaries/PITF/PITF_1995_2012/")
dir("Dictionaries/PITF/")
countrylist <- countrylist[countrylist$iso3%in%countrylist$iso3[!countrylist$iso3%in%gsub("PITF_|_Actors.RData","",dir("Dictionaries/PITF"))],]

# Loop by country (first line finds k where you left off)
k0 <- max(which(countrylist$iso3%in%gsub("PITF_|_Actors.RData","",dir("Dictionaries/PITF/"))))+1
k0

for(k in 1:nrow(countrylist)){
  subdata <- pitf.raw[pitf.raw$country%in%c(countrylist[k,"country"]),]
  print(countrylist[k,"country"])
  actorlist <- actorList(subdata,sidea="Perp.State.Role",sideb="Perp.State.Role",timevar="year",countryvar="country")
  save(actorlist,file=paste0("Dictionaries/PITF/PITF_",countrylist[k,"iso3"],"_Actors.RData"))
}

