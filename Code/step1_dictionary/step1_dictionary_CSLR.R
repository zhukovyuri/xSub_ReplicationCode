rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")

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
cslr.raw <- read.csv("Input/Events/CSLR/cslr_protests_v2.csv")
head(cslr.raw)
names(cslr.raw)

# Fix dates
cslr.raw$DATE_START
datez1 <- format(as.Date(cslr.raw$DATE_START), "%Y%m%d")
datez2 <- cslr.raw$DATE_START[is.na(datez1)]
datez1[is.na(datez1)] <- paste0(sapply(str_split(datez2,"/|-"),"[",3),sapply(str_split(datez2,"/|-"),"[",1),sapply(str_split(datez2,"/|-"),"[",2))
datez1[nchar(datez1)<8] <- NA
datez2 <- cslr.raw$DATE_START[is.na(datez1)]
datez1[is.na(datez1)] <- paste0(sapply(str_split(datez2,"/|-"),"[",3),sapply(str_split(datez2,"/|-"),"[",1),sapply(str_split(datez2,"/|-"),"[",2))
cslr.raw$DATE <- datez1
head(cslr.raw)

# Consolidate actors
actorz1 <- as.character(cslr.raw$ACTOR_FORM)
actorz2 <- as.character(cslr.raw$ACTOR_STRUCTURE)[is.na(actorz1)|nchar(actorz1)==0]
actorz1[is.na(actorz1)|nchar(actorz1)==0] <- actorz2
actorz2 <- as.character(cslr.raw$ACTOR_IDEOLOGY)[is.na(actorz1)|nchar(actorz1)==0]
actorz1[is.na(actorz1)|nchar(actorz1)==0] <- actorz2
actorz2 <- as.character(cslr.raw$ACTOR_SPECIFIED)[is.na(actorz1)|nchar(actorz1)==0]
actorz1[is.na(actorz1)|nchar(actorz1)==0] <- actorz2

# Consolidate targets
targetz1 <- as.character(cslr.raw$TARGET_ADDRESSEE)
targetz2 <- as.character(cslr.raw$TARGET_STRUCTURE)[is.na(targetz1)|nchar(targetz1)==0]
targetz1[is.na(targetz1)|nchar(targetz1)==0] <- targetz2
targetz2 <- as.character(cslr.raw$TARGET_FORM)[is.na(targetz1)|nchar(targetz1)==0]
targetz1[is.na(targetz1)|nchar(targetz1)==0] <- targetz2
targetz2 <- as.character(cslr.raw$TARGET_SPECIFIED)[is.na(targetz1)|nchar(targetz1)==0]
targetz1[is.na(targetz1)|nchar(targetz1)==0] <- targetz2


# Convert dummies to factors
cslr.raw$ACTORS <- gdata:::trim(tolower(actorz1))
cslr.raw$TARGETS <- gdata:::trim(tolower(targetz1))
row.names(cslr.raw) <- 1:nrow(cslr.raw)

# Write to file
tail(cslr.raw)
save(cslr.raw,file="Input/Events/CSLR/EVENTS_clean.RData")

#############################
## Create actor dictionary
#############################

load("Input/Events/CSLR/EVENTS_clean.RData")

# Loop by country (first line finds k where you left off)
subdata <- cslr.raw
actorz <- sort(unique(c(subdata$ACTORS,subdata$TARGETS)))
X <- data.frame(ACTORS=sort(unique(gdata:::trim(unlist(strsplit(actorz,","))))))
length(sort(unique(X$ACTORS)))
actorlist <- actorList(X,sidea="ACTORS",sideb="ACTORS")
addz <- c("central and local government","dai","evpatoria city rada executive committee","kmda","local electoral committee member","mia","mia department in ternopil","mia director v. sak","mia of kiev","mia of lviv oblast'","mia website","mp from pr vadym kolesnychenko","ovk","proposed law","special services","svyatoshyn police department","vekhovna rada")
remz <- c("russian bloc")
actorlist$actors_GOV <- actorlist$actors_GOV[!actorlist$actors_GOV%in%remz]
actorlist$actors_GOV <- c(actorlist$actors_GOV,addz)

save(actorlist,file=paste0("Dictionaries/CSLR/CSLR_Actors.RData"))



## Code actors (basic)
load("Input/Events/CSLR/EVENTS_clean.RData")

mat.temp <- matrix(0,nrow=nrow(cslr.raw),ncol=length(actorlist$actors_GOV))
j <- 11
for(j in seq_along(actorlist$actors_GOV)){
  mat.temp[,j] <- grepl(actorlist$actors_GOV[j],as.character(cslr.raw$ACTORS),fixed=TRUE)
}
cslr.raw$ACTOR_GOV <- 0
cslr.raw$ACTOR_GOV[which(apply(mat.temp,1,sum)>0)] <- 1

mat.temp <- matrix(0,nrow=nrow(cslr.raw),ncol=length(actorlist$actors_GOV))
for(j in seq_along(actorlist$actors_GOV)){
  mat.temp[,j] <- grepl(actorlist$actors_GOV[j],as.character(cslr.raw$TARGETS),fixed=TRUE)
}
cslr.raw$TARGET_GOV <- 0
cslr.raw$TARGET_GOV[which(apply(mat.temp,1,sum)>0)] <- 1
head(cslr.raw)

save(cslr.raw,file="Input/Events/CSLR/EVENTS_clean.RData")
