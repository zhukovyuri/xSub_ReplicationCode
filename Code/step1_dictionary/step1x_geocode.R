rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")


#############################
## Geocode: Ishchenko protest data
#############################

load("Input/Events/CSLR/EVENTS_clean.RData")
head(cslr.raw)

i <- 1

##
# Yandex geocode
##

# Create empty matrix
geo.mat <- data.frame(LOCATION=paste0("Ukraine, ",cslr.raw$OBLAST," oblast, ",cslr.raw$LOCATION),ADDRESS="",LONG=NA,LAT=NA)
geo.mat$ADDRESS <- as.character(geo.mat$ADDRESS)
geo.mat$LOCATION <- as.character(geo.mat$LOCATION)
geo.mat$LOCATION_ID <- as.numeric(as.factor(geo.mat$LOCATION))
geo.mat.uq <- geo.mat[!duplicated(geo.mat$LOCATION_ID),]
geo.mat.uq <- geo.mat.uq[order(geo.mat.uq$LOCATION_ID),]
head(geo.mat.uq)

# Initial pass (Yandex)
i <- 1
for(i in 1:nrow(geo.mat.uq)){
  geo.out <- geoCode2(query = geo.mat.uq$LOCATION[i])
  geo.mat.uq$ADDRESS[i] <- geo.out[3]
  geo.mat.uq$LONG[i] <- geo.out[2]
  geo.mat.uq$LAT[i] <- geo.out[1]
  print(paste(i,"of",nrow(geo.mat.uq),"/",geo.out[3]))
}
geo.mat

# Initial pass (Google)
i <- 1
for(i in 841:nrow(geo.mat.uq)){
  geo.out <- geoCode(geo.mat.uq$LOCATION[i])
  geo.mat.uq$ADDRESS[i] <- geo.out[4]
  geo.mat.uq$LONG[i] <- geo.out[2]
  geo.mat.uq$LAT[i] <- geo.out[1]
  print(paste(i,"of",nrow(geo.mat.uq),"/",geo.out[4]))
}
save(geo.mat,geo.mat.uq,file="Input/Events/CSLR/cslr_protests_GEO_TEMP.RData")

head(geo.mat.uq)

# Fill out missing (Yandex)
is.miss <- which(is.na(geo.mat.uq$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat.uq)
for(i in is.miss){
  geo.out <- geoCode2(geo.mat.uq$LOCATION[i])
  geo.mat.uq$ADDRESS[i] <- geo.out[3]
  geo.mat.uq$LONG[i] <- geo.out[2]
  geo.mat.uq$LAT[i] <- geo.out[1]
  print(paste(i,"of",nrow(geo.mat.uq),"/",geo.out[3]))
} 
save(geo.mat,geo.mat.uq,file="Input/Events/CSLR/cslr_protests_GEO_TEMP.RData")

# Re-merge
load("Input/Events/CSLR/cslr_protests_GEO_TEMP.RData")
geo.mat.uq$LOCATION <- NULL
geo.mat$ADDRESS <- geo.mat$LAT <- geo.mat$LONG <- NULL
geo.mat <- merge(geo.mat,geo.mat.uq,by="LOCATION_ID",all.x=T,all.y=F)
head(geo.mat)
is.miss <- which(is.na(geo.mat$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat)
cslr.raw$LOCATION_FULL <- paste0("Ukraine, ",cslr.raw$OBLAST," oblast, ",cslr.raw$LOCATION)
cslr.raw$LOCATION_ID <- as.numeric(as.factor(cslr.raw$LOCATION_FULL))
head(cslr.raw)
cslr.raw <- cslr.raw[order(cslr.raw$LOCATION_ID),]
mean(cslr.raw$LOCATION_FULL==geo.mat$LOCATION)

# Add coordinates
if(mean(cslr.raw$LOCATION_FULL==geo.mat$LOCATION)){
cslr.raw$LONG <- geo.mat$LONG
cslr.raw$LAT <- geo.mat$LAT
}
head(cslr.raw)

# Saving
save(cslr.raw,file="Input/Events/CSLR/cslr_protests_GEO.RData")

plot(cslr.raw$LONG,cslr.raw$LAT)


#############################
## Geocode: Sullivan
#############################

load("Input/Events/Sullivan/ceh-massacres.RData")
head(Sullivan.raw)

i <- 1

##
# Google geocode (max 2500 per day)
##

# Create empty matrix
geo.mat <- data.frame(LOCATION=paste(Sullivan.raw$canton,Sullivan.raw$municipality,Sullivan.raw$department,"Guatemala",sep=", "),ADDRESS="",LONG=NA,LAT=NA)
geo.mat$ADDRESS <- as.character(geo.mat$ADDRESS)
geo.mat$LOCATION <- as.character(geo.mat$LOCATION)

# Initial pass
for(i in 1:nrow(geo.mat)){print(paste(i,"of",nrow(geo.mat)))
  geo.out <- geoCode(address = geo.mat$LOCATION[i])
  geo.mat$ADDRESS[i] <- geo.out[4]
  geo.mat$LONG[i] <- geo.out[2]
  geo.mat$LAT[i] <- geo.out[1]
}
geo.mat
# Fill out missing
is.miss <- which(is.na(geo.mat$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat)
geo.mat[is.miss,"LOCATION"] <- paste(Sullivan.raw$municipality,Sullivan.raw$department,"Guatemala",sep=", ")[is.miss]
while(prop.miss>.01){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in is.miss){
    geo.out <- geoCode(address = geo.mat$LOCATION[i])
    geo.mat$ADDRESS[i] <- geo.out[4]
    geo.mat$LONG[i] <- geo.out[2]
    geo.mat$LAT[i] <- geo.out[1]
  } 
  is.miss <- which(is.na(geo.mat$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat) 
}

# Add coordinates
Sullivan.raw$LONG <- geo.mat$LONG
Sullivan.raw$LAT <- geo.mat$LAT

# Saving
save(Sullivan.raw,file="Input/Events/Sullivan/ceh-massacres_GEO.RData")




#############################
## Geocode: FerwerdaMiller
#############################

load("Input/Events/FerwerdaMiller/Replication/FM_France.RData")
head(FerwerdaMiller.raw)

i <- 1

##
# Yandex API geocode 
##

# Create empty matrix
geo.mat <- data.frame(LOCATION=paste(FerwerdaMiller.raw$nom_comm,FerwerdaMiller.raw$nom_dept,"France",sep=", "),ADDRESS="",LONG=NA,LAT=NA)
geo.mat$ADDRESS <- as.character(geo.mat$ADDRESS)
geo.mat$LOCATION <- as.character(geo.mat$LOCATION)

# Initial pass
for(i in 1:nrow(geo.mat)){print(paste(i,"of",nrow(geo.mat)))
  geo.out <- geoCode2(query = geo.mat$LOCATION[i])
  geo.mat$ADDRESS[i] <- geo.out[3]
  geo.mat$LONG[i] <- geo.out[2]
  geo.mat$LAT[i] <- geo.out[1]
}
head(geo.mat)

# # Fill out missing (w/ Google API)
# is.miss <- which(is.na(geo.mat$LONG))
# prop.miss <- length(is.miss)/nrow(geo.mat)
# while(prop.miss>.01){
#   print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
#   for(i in is.miss){
#     geo.out <- geoCode(address = geo.mat$LOCATION[i])
#     geo.mat$ADDRESS[i] <- geo.out[4]
#     geo.mat$LONG[i] <- geo.out[2]
#     geo.mat$LAT[i] <- geo.out[1]
#   } 
#   is.miss <- which(is.na(geo.mat$LONG))
#   prop.miss <- length(is.miss)/nrow(geo.mat) 
# }

# Add coordinates
FerwerdaMiller.raw$LONG <- geo.mat$LONG
FerwerdaMiller.raw$LAT <- geo.mat$LAT

# Saving
save(FerwerdaMiller.raw,file="Input/Events/FerwerdaMiller/Replication/FM_France_GEO.RData")
head(FerwerdaMiller.raw)





#############################
## Geocode: ESOC Mexico: homicide
#############################

# Homicides
load("Input/Events/ESOC/Mexico/esoc_MEX_homicide.RData")
tail(data)

i <- 1

##
# Geocode 
##

# Create empty matrix
geo.mat <- data.frame(LOCATION=paste(data$NOM_MUN,data$NOM_ENT,"Mexico",sep=", "),ADDRESS="",LONG=NA,LAT=NA)
geo.mat$ADDRESS <- as.character(geo.mat$ADDRESS)
geo.mat$LOCATION <- as.character(geo.mat$LOCATION)

# Unique locations

geo.mat2 <- data.frame(LOCATION=sort(unique(geo.mat$LOCATION)),ADDRESS="",LONG=NA,LAT=NA)
geo.mat2$ADDRESS <- as.character(geo.mat2$ADDRESS)
geo.mat2$LOCATION <- as.character(geo.mat2$LOCATION)

# Initial pass (w/ Google)
i <- 1
for(i in 1:nrow(geo.mat2)){
  geo.out <- geoCode(geo.mat2$LOCATION[i])
  print(paste(i,"of",nrow(geo.mat2)," ",geo.out[4]))
  geo.mat2$ADDRESS[i] <- geo.out[4]
  geo.mat2$LONG[i] <- geo.out[2]
  geo.mat2$LAT[i] <- geo.out[1]
}
head(geo.mat2)
i <- 59

# Fill out missing (w/ paid Google API key)

is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
#data("wrld_simpl")
#bbx <- bbox(wrld_simpl[wrld_simpl$NAME=="Mexico",])
#geoCode3(geo.mat2$LOCATION[6],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))

while(prop.miss>.05){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in is.miss){
    geo.out <- geoCode(address = geo.mat2$LOCATION[i],key=TRUE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}

head(geo.mat)
# Merge back
geo.mat$TEMP <- 1:nrow(geo.mat)
geo.mat$ADDRESS <- geo.mat$LAT <- geo.mat$LONG <- NULL
geo.mat <- merge(geo.mat,geo.mat2,by="LOCATION",all.x=TRUE)
geo.mat <- geo.mat[order(geo.mat$TEMP),]

# Add coordinates
data$LONG <- geo.mat$LONG
data$LAT <- geo.mat$LAT

# Saving
head(data)
#save(data,file="Input/Events/ESOC/Mexico/esoc_MEX_homicide_GEO.RData")



#############################
## Geocode: ESOC Mexico: drug-related murders
#############################

# Homicides
load("Input/Events/ESOC/Mexico/esoc_MEX_drug_related_murders.RData")
tail(data)

i <- 1

##
# Geocode 
##

# Create empty matrix
geo.mat <- data.frame(LOCATION=paste(data$NOM_MUN,data$NOM_ENT,"Mexico",sep=", "),ADDRESS="",LONG=NA,LAT=NA)
geo.mat$ADDRESS <- as.character(geo.mat$ADDRESS)
geo.mat$LOCATION <- as.character(geo.mat$LOCATION)

# Unique locations

geo.mat2 <- data.frame(LOCATION=sort(unique(geo.mat$LOCATION)),ADDRESS="",LONG=NA,LAT=NA)
geo.mat2$ADDRESS <- as.character(geo.mat2$ADDRESS)
geo.mat2$LOCATION <- as.character(geo.mat2$LOCATION)

# Initial pass (w/ Google)
i <- 1
for(i in 1:nrow(geo.mat2)){
  geo.out <- geoCode(geo.mat2$LOCATION[i])
  print(paste(i,"of",nrow(geo.mat2)," ",geo.out[4]))
  geo.mat2$ADDRESS[i] <- geo.out[4]
  geo.mat2$LONG[i] <- geo.out[2]
  geo.mat2$LAT[i] <- geo.out[1]
}
head(geo.mat2)
i <- 59

# Fill out missing (w/ paid Google API key)

is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
#data("wrld_simpl")
#bbx <- bbox(wrld_simpl[wrld_simpl$NAME=="Mexico",])
#geoCode3(geo.mat2$LOCATION[6],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))

while(prop.miss>.05){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in is.miss){
    geo.out <- geoCode(address = geo.mat2$LOCATION[i],key=TRUE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}

head(geo.mat)
# Merge back
geo.mat$TEMP <- 1:nrow(geo.mat)
geo.mat$ADDRESS <- geo.mat$LAT <- geo.mat$LONG <- NULL
geo.mat <- merge(geo.mat,geo.mat2,by="LOCATION",all.x=TRUE)
geo.mat <- geo.mat[order(geo.mat$TEMP),]

# Add coordinates
data$LONG <- geo.mat$LONG
data$LAT <- geo.mat$LAT

# Saving
head(data)
save(data,file="Input/Events/ESOC/Mexico/esoc_MEX_drug_related_murders_GEO.RData")


#############################
## Geocode: ESOC SIGACT Iraq
#############################

# Homicides
load("Input/Events/ESOC/SIGACT/esoc_iraq_week_district.RData")
data <- esoc.raw
head(data)
i <- 1

##
# Geocode 
##

# Create empty matrix
geo.mat <- data.frame(LOCATION=paste(data$district,"Iraq",sep=", "),ADDRESS="",LONG=NA,LAT=NA)
geo.mat$ADDRESS <- as.character(geo.mat$ADDRESS)
geo.mat$LOCATION <- as.character(geo.mat$LOCATION)

# Unique locations

geo.mat2 <- data.frame(LOCATION=sort(unique(geo.mat$LOCATION)),ADDRESS="",LONG=NA,LAT=NA)
geo.mat2$ADDRESS <- as.character(geo.mat2$ADDRESS)
geo.mat2$LOCATION <- as.character(geo.mat2$LOCATION)

# Initial pass (w/ Google)
i <- 1
for(i in 1:nrow(geo.mat2)){
  geo.out <- geoCode(geo.mat2$LOCATION[i],key=TRUE)
  print(paste(i,"of",nrow(geo.mat2)," ",geo.out[4]))
  geo.mat2$ADDRESS[i] <- geo.out[4]
  geo.mat2$LONG[i] <- geo.out[2]
  geo.mat2$LAT[i] <- geo.out[1]
}
tail(geo.mat2)
i <- 59

# Fill out missing (w/ paid Google API key)

is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
# data("wrld_simpl")
# bbx <- bbox(wrld_simpl[wrld_simpl$NAME=="Mexico",])
# geoCode3(geo.mat2$LOCATION[6],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))

while(prop.miss>.05){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in is.miss){
    geo.out <- geoCode(address = geo.mat2$LOCATION[i],key=TRUE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}

tail(geo.mat)
# Merge back
geo.mat$TEMP <- 1:nrow(geo.mat)
geo.mat$ADDRESS <- geo.mat$LAT <- geo.mat$LONG <- NULL
geo.mat <- merge(geo.mat,geo.mat2,by="LOCATION",all.x=TRUE)
geo.mat <- geo.mat[order(geo.mat$TEMP),]

# Add coordinates
data$LONG <- geo.mat$LONG
data$LAT <- geo.mat$LAT

# Saving
head(data)
esoc.raw <- data
save(esoc.raw,file="Input/Events/ESOC/SIGACT/esoc_iraq_week_district_GEO.RData")







#############################
## Geocode: ESOC BFRS Pak
#############################

# Homicides
load("Input/Events/ESOC/BFRS/esoc_PK_v10.RData")
data <- esoc.raw
head(data)
i <- 1

##
# Geocode 
##

# Create empty matrix
geo.mat[3245,]
geo.mat <- data.frame(LOCATION=paste(tolower(iconv(as.character(data$Location),"WINDOWS-1252","UTF-8")),data$District,data$Province,"Pakistan",sep=", "),ADDRESS="",LONG=NA,LAT=NA)
geo.mat$LOCATION <- gsub("^, |Unknown, ","",geo.mat$LOCATION)
geo.mat$ADDRESS <- as.character(geo.mat$ADDRESS)
geo.mat$LOCATION <- as.character(geo.mat$LOCATION)

# Unique locations
geo.mat2 <- data.frame(LOCATION=sort(unique(geo.mat$LOCATION)),ADDRESS="",LONG=NA,LAT=NA)
geo.mat2$ADDRESS <- as.character(geo.mat2$ADDRESS)
geo.mat2$LOCATION <- as.character(geo.mat2$LOCATION)

# Initial pass (w/ Google)
i <- 2
for(i in 1:nrow(geo.mat2)){
  geo.out <- geoCode(geo.mat2$LOCATION[i],key=FALSE)
  print(paste(i,"of",nrow(geo.mat2)," ",geo.out[4]))
  geo.mat2$ADDRESS[i] <- geo.out[4]
  geo.mat2$LONG[i] <- geo.out[2]
  geo.mat2$LAT[i] <- geo.out[1]
}
tail(geo.mat2)
i <- 59
# save(geo.mat2,file="Input/Events/ESOC/BFRS/esoc_PK_v10_GEO_TEMP.RData")

# Fill out missing (w/ paid Google API key: set key=TRUE)
is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
while(prop.miss>.05){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in rev(is.miss)){
    geo.out <- geoCode(address = geo.mat2$LOCATION[i],key=FALSE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}
# save(geo.mat2,file="Input/Events/ESOC/BFRS/esoc_PK_v10_GEO_TEMP.RData")
load("Input/Events/ESOC/BFRS/esoc_PK_v10_GEO_TEMP.RData")

# Simplified addresses
is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)

# remove street address (for still-missing locations)
ss <- strsplit(geo.mat2[is.miss,"LOCATION"],", ")
deltz <- sapply(ss,"[",1)
deltz[grepl("^[[:upper:]]", deltz)] <- ""
j <- 1
while(sum(nchar(deltz)>0)>12){
for(j in 1:length(ss)){
  if(nchar(deltz[j])>0){geo.mat2[is.miss,"LOCATION"][j] <- gsub(paste0(deltz[j],", "),"",geo.mat2[is.miss,"LOCATION"][j])}}
  ss <- strsplit(geo.mat2[is.miss,"LOCATION"],", ")
  deltz <- sapply(ss,"[",1)
  deltz[grepl("^[[:upper:]]", deltz)] <- ""
}
geo.mat2[is.miss,"LOCATION"][1:20]
# geo.mat2[is.miss,"LOCATION"] <- gsub("^(.*?), ","",geo.mat2[is.miss,"LOCATION"]) # remove street address (for still-missing locations)

# Unique locations
geo.mat3 <- data.frame(LOCATION=sort(unique(geo.mat2$LOCATION[is.miss])),ADDRESS="",LONG=NA,LAT=NA)
geo.mat3$ADDRESS <- as.character(geo.mat3$ADDRESS)
geo.mat3$LOCATION <- as.character(geo.mat3$LOCATION)
head(geo.mat3)

is.miss2 <- which(is.na(geo.mat3$LONG))
prop.miss2 <- length(is.miss2)/nrow(geo.mat3)

while(prop.miss2>.05){
  print(paste("Proportion missing:",round(prop.miss2,2),"; N missing=",length(is.miss2)))
  for(i in rev(is.miss2)){
    geo.out <- geoCode(address = geo.mat3$LOCATION[i],key=TRUE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(prop.miss2,i,geo.out[4]))
    geo.mat3$ADDRESS[i] <- geo.out[4]
    geo.mat3$LONG[i] <- geo.out[2]
    geo.mat3$LAT[i] <- geo.out[1]
  }
  save(geo.mat3,file="Input/Events/ESOC/BFRS/esoc_PK_v10_GEO_TEMP2.RData")
  is.miss2 <- which(is.na(geo.mat3$LONG))
  prop.miss2 <- length(is.miss2)/nrow(geo.mat3)
}

# save(geo.mat3,file="Input/Events/ESOC/BFRS/esoc_PK_v10_GEO_TEMP2.RData")
load("Input/Events/ESOC/BFRS/esoc_PK_v10_GEO_TEMP2.RData")

# Merge back
geo.mat2$TEMP <- 1:nrow(geo.mat2)
geo.mat2.a <- geo.mat2[-is.miss,]
geo.mat2.b <- geo.mat2[is.miss,]
tail(geo.mat2.b)
geo.mat2.b$ADDRESS <- geo.mat2.b$LAT <- geo.mat2.b$LONG <- NULL
geo.mat2.b <- merge(geo.mat2.b,geo.mat3,by="LOCATION",all.x=TRUE)
geo.mat2.x <- rbind(geo.mat2.a,geo.mat2.b)
geo.mat2.x <- geo.mat2.x[order(geo.mat2.x$TEMP),]
geo.mat2.x$TEMP <- NULL
geo.mat2 <- geo.mat2.x
head(geo.mat2.x)
tail(geo.mat2)
# save(geo.mat2,file="Input/Events/ESOC/BFRS/esoc_PK_v10_GEO_TEMP.RData")
load("Input/Events/ESOC/BFRS/esoc_PK_v10_GEO_TEMP.RData")

# Merge back
# Recreate old location variable
old.loc <- sort(unique(geo.mat$LOCATION))
geo.mat2$LOCATION <- old.loc
head(geo.mat2)
geo.mat$TEMP <- 1:nrow(geo.mat)
geo.mat$ADDRESS <- geo.mat$LAT <- geo.mat$LONG <- NULL
geo.mat <- merge(geo.mat,geo.mat2,by="LOCATION",all.x=TRUE)
geo.mat <- geo.mat[order(geo.mat$TEMP),]
head(geo.mat)
head(data)

# Add coordinates
data$LONG <- as.numeric(as.character(geo.mat$LONG))
data$LAT <- as.numeric(as.character(geo.mat$LAT))
data$iso3 <- as.character("PAK")

# Saving
head(data)
summary(data)
esoc.raw <- data
save(esoc.raw,file="Input/Events/ESOC/BFRS/esoc_PK_v10_GEO.RData")


#############################
## Geocode: ESOC WITS AFG
#############################

load("Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105.RData")
head(data)
i <- 1
dim(data) #7846 by 99

##
# Geocode 
##

# Create empty matrix
# Create empty matrix
geo.mat <- data.frame(LOCATION=paste(data$distprov,data$district,"Afghanistan",sep=", "),ADDRESS="",LONG=NA,LAT=NA)
geo.mat$ADDRESS <- as.character(geo.mat$ADDRESS)
geo.mat$LOCATION <- as.character(geo.mat$LOCATION)

# Unique locations

geo.mat2 <- data.frame(LOCATION=sort(unique(geo.mat$LOCATION)),ADDRESS="",LONG=NA,LAT=NA)
geo.mat2$ADDRESS <- as.character(geo.mat2$ADDRESS)
geo.mat2$LOCATION <- as.character(geo.mat2$LOCATION)

# Initial pass (w/ Google)
i <- 2
for(i in 1:nrow(geo.mat2)){
  geo.out <- geoCode(geo.mat2$LOCATION[i],key=FALSE)
  print(paste(i,"of",nrow(geo.mat2)," ",geo.out[4]))
  geo.mat2$ADDRESS[i] <- geo.out[4]
  geo.mat2$LONG[i] <- geo.out[2]
  geo.mat2$LAT[i] <- geo.out[1]
}
tail(geo.mat2)
i <- 134
save(geo.mat2,file="Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105_GEO_TEMP.RData")

# Fill out missing (w/ paid Google API key: set key=TRUE)
is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
while(prop.miss>.05){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in rev(is.miss)){
    geo.out <- geoCode(address = geo.mat2$LOCATION[i],key=FALSE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}
#save(geo.mat2,file="Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105_GEO_TEMP2.Rdata")
load("Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105_GEO_TEMP2.RData")

# Simplified addresses
is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)

# remove street address (for still-missing locations)
ss <- strsplit(geo.mat2[is.miss,"LOCATION"],", ")
deltz <- sapply(ss,"[",1)
deltz[grepl("^[[:upper:]]", deltz)] <- ""
j <- 1
while(sum(nchar(deltz)>0)>12){
  for(j in 1:length(ss)){
    if(nchar(deltz[j])>0){geo.mat2[is.miss,"LOCATION"][j] <- gsub(paste0(deltz[j],", "),"",geo.mat2[is.miss,"LOCATION"][j])}}
  ss <- strsplit(geo.mat2[is.miss,"LOCATION"],", ")
  deltz <- sapply(ss,"[",1)
  deltz[grepl("^[[:upper:]]", deltz)] <- ""
}
geo.mat2[is.miss,"LOCATION"][1:20]
# geo.mat2[is.miss,"LOCATION"] <- gsub("^(.*?), ","",geo.mat2[is.miss,"LOCATION"]) # remove street address (for still-missing locations)

# Unique locations
geo.mat3 <- data.frame(LOCATION=sort(unique(geo.mat2$LOCATION[is.miss])),ADDRESS="",LONG=NA,LAT=NA)
geo.mat3$ADDRESS <- as.character(geo.mat3$ADDRESS)
geo.mat3$LOCATION <- as.character(geo.mat3$LOCATION)
head(geo.mat3)

is.miss2 <- which(is.na(geo.mat3$LONG))
prop.miss2 <- length(is.miss2)/nrow(geo.mat3)

while(prop.miss2>.05){
  print(paste("Proportion missing:",round(prop.miss2,2),"; N missing=",length(is.miss2)))
  for(i in rev(is.miss2)){
    geo.out <- geoCode(address = geo.mat3$LOCATION[i],key=TRUE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(prop.miss2,i,geo.out[4]))
    geo.mat3$ADDRESS[i] <- geo.out[4]
    geo.mat3$LONG[i] <- geo.out[2]
    geo.mat3$LAT[i] <- geo.out[1]
  }
  save(geo.mat3,file="Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105_GEO_TEMP2.RData")
  is.miss2 <- which(is.na(geo.mat3$LONG))
  prop.miss2 <- length(is.miss2)/nrow(geo.mat3)
}

# save(geo.mat3,file="Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105_GEO_TEMP2.RData")
load("Input/Events/ESOC/BFRS/esoc_PK_v10_GEO_TEMP2.RData")

# Merge back
geo.mat2$TEMP <- 1:nrow(geo.mat2)
geo.mat2.a <- geo.mat2[-is.miss,]
geo.mat2.b <- geo.mat2[is.miss,]
tail(geo.mat2.b)
geo.mat2.b$ADDRESS <- geo.mat2.b$LAT <- geo.mat2.b$LONG <- NULL
geo.mat2.b <- merge(geo.mat2.b,geo.mat3,by="LOCATION",all.x=TRUE)
geo.mat2.x <- rbind(geo.mat2.a,geo.mat2.b)
geo.mat2.x <- geo.mat2.x[order(geo.mat2.x$TEMP),]
geo.mat2.x$TEMP <- NULL
geo.mat2 <- geo.mat2.x
head(geo.mat2.x)
tail(geo.mat2)
#save(geo.mat2,file="Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105_GEO_TEMP.RData")
load("Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105_GEO_TEMP.RData")

# Merge back
# Recreate old location variable
old.loc <- sort(unique(geo.mat$LOCATION))
geo.mat2$LOCATION <- old.loc
head(geo.mat2)
geo.mat$TEMP <- 1:nrow(geo.mat)
geo.mat$ADDRESS <- geo.mat$LAT <- geo.mat$LONG <- NULL
geo.mat <- merge(geo.mat,geo.mat2,by="LOCATION",all.x=TRUE)
geo.mat <- geo.mat[order(geo.mat$TEMP),]
head(geo.mat)
head(data)

# Add coordinates
data$LONG <- as.numeric(as.character(geo.mat$LONG))
data$LAT <- as.numeric(as.character(geo.mat$LAT))
data$iso3 <- as.character("AFG")

# Saving
head(data)
summary(data)
esoc.raw <- data
save(esoc.raw,file="Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105_GEO.RData")



#############################
## Geocode: ESOC WITS Pakistan
#############################

data <- read.dta("Input/Events/ESOC/WITS/Pakistan Geo-referenced WITS Data (2004-2009)/PK_geo_wits_20100608.dta")
head(data)
i <- 1
dim(data) #5601 by 15

##
# Geocode 
##

# Create empty matrix
geo.mat <- data.frame(LOCATION=paste(data$distprov,data$district,"Pakistan",sep=", "),ADDRESS="",LONG=NA,LAT=NA)
geo.mat$ADDRESS <- as.character(geo.mat$ADDRESS)
geo.mat$LOCATION <- as.character(geo.mat$LOCATION)

# Unique locations

geo.mat2 <- data.frame(LOCATION=sort(unique(geo.mat$LOCATION)),ADDRESS="",LONG=NA,LAT=NA)
geo.mat2$ADDRESS <- as.character(geo.mat2$ADDRESS)
geo.mat2$LOCATION <- as.character(geo.mat2$LOCATION)

# Initial pass (w/ Google)
i <- 2
for(i in 1:nrow(geo.mat2)){
  geo.out <- geoCode(geo.mat2$LOCATION[i],key=FALSE)
  print(paste(i,"of",nrow(geo.mat2)," ",geo.out[4]))
  geo.mat2$ADDRESS[i] <- geo.out[4]
  geo.mat2$LONG[i] <- geo.out[2]
  geo.mat2$LAT[i] <- geo.out[1]
}
tail(geo.mat2)
#i <- 134
#save(geo.mat2,file="Input/Events/ESOC/WITS/Paksitan Geo-referenced WITS Data (2004-2009)/Pakistan_WITS_GEO_TEMP.RData")

# Fill out missing (w/ paid Google API key: set key=TRUE)
is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
while(prop.miss>.05){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in rev(is.miss)){
    geo.out <- geoCode(address = geo.mat2$LOCATION[i],key=FALSE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}
#save(geo.mat2,file="Input/Events/ESOC/WITS/Pakistan Geo-referenced WITS Data (2004-2009)/Pakistan_WITS_GEO_TEMP2.Rdata")
load("Input/Events/ESOC/WITS/Pakistan Geo-referenced WITS Data (2004-2009)/Pakistan_WITS_GEO_TEMP2.RData")

# # Simplified addresses
is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)

# remove street address (for still-missing locations)
ss <- strsplit(geo.mat2[is.miss,"LOCATION"],", ")
deltz <- sapply(ss,"[",1)
deltz[grepl("^[[:upper:]]", deltz)] <- ""
j <- 1
while(sum(nchar(deltz)>0)>12){
  for(j in 1:length(ss)){
    if(nchar(deltz[j])>0){geo.mat2[is.miss,"LOCATION"][j] <- gsub(paste0(deltz[j],", "),"",geo.mat2[is.miss,"LOCATION"][j])}}
  ss <- strsplit(geo.mat2[is.miss,"LOCATION"],", ")
  deltz <- sapply(ss,"[",1)
  deltz[grepl("^[[:upper:]]", deltz)] <- ""
}
geo.mat2[is.miss,"LOCATION"][1:20]
# geo.mat2[is.miss,"LOCATION"] <- gsub("^(.*?), ","",geo.mat2[is.miss,"LOCATION"]) # remove street address (for still-missing locations)

# Unique locations
geo.mat3 <- data.frame(LOCATION=sort(unique(geo.mat2$LOCATION[is.miss])),ADDRESS="",LONG=NA,LAT=NA)
geo.mat3$ADDRESS <- as.character(geo.mat3$ADDRESS)
geo.mat3$LOCATION <- as.character(geo.mat3$LOCATION)
head(geo.mat3)

is.miss2 <- which(is.na(geo.mat3$LONG))
prop.miss2 <- length(is.miss2)/nrow(geo.mat3)

while(prop.miss2>.05){
  print(paste("Proportion missing:",round(prop.miss2,2),"; N missing=",length(is.miss2)))
  for(i in rev(is.miss2)){
    geo.out <- geoCode(address = geo.mat3$LOCATION[i],key=TRUE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(prop.miss2,i,geo.out[4]))
    geo.mat3$ADDRESS[i] <- geo.out[4]
    geo.mat3$LONG[i] <- geo.out[2]
    geo.mat3$LAT[i] <- geo.out[1]
  }
  save(geo.mat3,file="Input/Events/ESOC/WITS/Pakistan Geo-referenced WITS Data (2004-2009)/Pakistan_WITS_GEO_TEMP2.RData")
  is.miss2 <- which(is.na(geo.mat3$LONG))
  prop.miss2 <- length(is.miss2)/nrow(geo.mat3)
}

#save(geo.mat3,file="Input/Events/ESOC/WITS/Pakistan Geo-referenced WITS Data (2004-2009)/Pakistan_WITS_GEO_TEMP2.RData")
load("Input/Events/ESOC/WITS/Pakistan Geo-referenced WITS Data (2004-2009)/Pakistan_WITS_GEO_TEMP2.RData")

# # Merge back
geo.mat2$TEMP <- 1:nrow(geo.mat2)
geo.mat2.a <- geo.mat2[-is.miss,]
geo.mat2.b <- geo.mat2[is.miss,]
tail(geo.mat2.b)
geo.mat2.b$ADDRESS <- geo.mat2.b$LAT <- geo.mat2.b$LONG <- NULL
geo.mat2.b <- merge(geo.mat2.b,geo.mat3,by="LOCATION",all.x=TRUE)
geo.mat2.x <- rbind(geo.mat2.a,geo.mat2.b)
geo.mat2.x <- geo.mat2.x[order(geo.mat2.x$TEMP),]
geo.mat2.x$TEMP <- NULL
geo.mat2 <- geo.mat2.x
head(geo.mat2.x)
tail(geo.mat2)
#save(geo.mat2,file="Input/Events/ESOC/WITS/Pakistan Geo-referenced WITS Data (2004-2009)/Pakistan_WITS_GEO_TEMP.RData")
load("Input/Events/ESOC/WITS/Pakistan Geo-referenced WITS Data (2004-2009)/Pakistan_WITS_GEO_TEMP.RData")

# Merge back
# Recreate old location variable
old.loc <- sort(unique(geo.mat$LOCATION))
geo.mat2$LOCATION <- old.loc
head(geo.mat2)
geo.mat$TEMP <- 1:nrow(geo.mat)
geo.mat$ADDRESS <- geo.mat$LAT <- geo.mat$LONG <- NULL
geo.mat <- merge(geo.mat,geo.mat2,by="LOCATION",all.x=TRUE)
geo.mat <- geo.mat[order(geo.mat$TEMP),]
head(geo.mat)
head(data)

# Add coordinates
data$LONG <- as.numeric(as.character(geo.mat$LONG))
data$LAT <- as.numeric(as.character(geo.mat$LAT))
data$iso3 <- as.character("AFG")

# Saving
head(data)
summary(data)
esoc.raw <- data
save(esoc.raw,file="Input/Events/ESOC/WITS/Pakistan Geo-referenced WITS Data (2004-2009)/Pakistan_WITS_GEO.RData")



#############################
## Geocode: ESOC WITS IRQ
#############################

data <- read.dta("Input/Events/ESOC/WITS/Iraq WITS Data/iraq_wits_attacks_v3.dta")
head(data)
i <- 1
dim(data) #25490 by 34

##
# Geocode 
##

# Create empty matrix
geo.mat <- data.frame(LOCATION=paste(data$distprov,data$district,"Iraq",sep=", "),ADDRESS="",LONG=NA,LAT=NA)
geo.mat$ADDRESS <- as.character(geo.mat$ADDRESS)
geo.mat$LOCATION <- as.character(geo.mat$LOCATION)

# Unique locations

geo.mat2 <- data.frame(LOCATION=sort(unique(geo.mat$LOCATION)),ADDRESS="",LONG=NA,LAT=NA)
geo.mat2$ADDRESS <- as.character(geo.mat2$ADDRESS)
geo.mat2$LOCATION <- as.character(geo.mat2$LOCATION)

# Initial pass (w/ Google)
i <- 2
for(i in 1:nrow(geo.mat2)){
  geo.out <- geoCode(geo.mat2$LOCATION[i],key=FALSE)
  print(paste(i,"of",nrow(geo.mat2)," ",geo.out[4]))
  geo.mat2$ADDRESS[i] <- geo.out[4]
  geo.mat2$LONG[i] <- geo.out[2]
  geo.mat2$LAT[i] <- geo.out[1]
}
tail(geo.mat2)
#i <- 134
#save(geo.mat2,file="Input/Events/ESOC/WITS/Iraq WITS Data/Iraq_WITS_GEO_TEMP.RData")

# Fill out missing (w/ paid Google API key: set key=TRUE)
is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
while(prop.miss>.05){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in rev(is.miss)){
    geo.out <- geoCode(address = geo.mat2$LOCATION[i],key=FALSE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}
#save(geo.mat2,file="Input/Events/ESOC/WITS/Iraq WITS Data/Iraq_WITS_GEO_TEMP2.Rdata")
load("Input/Events/ESOC/WITS/Iraq WITS Data/Iraq_WITS_GEO_TEMP2.RData")

# # Simplified addresses
# is.miss <- which(is.na(geo.mat2$LONG))
# prop.miss <- length(is.miss)/nrow(geo.mat2)
# 
# # remove street address (for still-missing locations)
# ss <- strsplit(geo.mat2[is.miss,"LOCATION"],", ")
# deltz <- sapply(ss,"[",1)
# deltz[grepl("^[[:upper:]]", deltz)] <- ""
# j <- 1
# while(sum(nchar(deltz)>0)>12){
#   for(j in 1:length(ss)){
#     if(nchar(deltz[j])>0){geo.mat2[is.miss,"LOCATION"][j] <- gsub(paste0(deltz[j],", "),"",geo.mat2[is.miss,"LOCATION"][j])}}
#   ss <- strsplit(geo.mat2[is.miss,"LOCATION"],", ")
#   deltz <- sapply(ss,"[",1)
#   deltz[grepl("^[[:upper:]]", deltz)] <- ""
# }
# geo.mat2[is.miss,"LOCATION"][1:20]
# # geo.mat2[is.miss,"LOCATION"] <- gsub("^(.*?), ","",geo.mat2[is.miss,"LOCATION"]) # remove street address (for still-missing locations)
# 
# # Unique locations
# geo.mat3 <- data.frame(LOCATION=sort(unique(geo.mat2$LOCATION[is.miss])),ADDRESS="",LONG=NA,LAT=NA)
# geo.mat3$ADDRESS <- as.character(geo.mat3$ADDRESS)
# geo.mat3$LOCATION <- as.character(geo.mat3$LOCATION)
# head(geo.mat3)
# 
# is.miss2 <- which(is.na(geo.mat3$LONG))
# prop.miss2 <- length(is.miss2)/nrow(geo.mat3)
# 
# while(prop.miss2>.05){
#   print(paste("Proportion missing:",round(prop.miss2,2),"; N missing=",length(is.miss2)))
#   for(i in rev(is.miss2)){
#     geo.out <- geoCode(address = geo.mat3$LOCATION[i],key=TRUE)
#     # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
#     print(paste(prop.miss2,i,geo.out[4]))
#     geo.mat3$ADDRESS[i] <- geo.out[4]
#     geo.mat3$LONG[i] <- geo.out[2]
#     geo.mat3$LAT[i] <- geo.out[1]
#   }
#   save(geo.mat3,file="Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105_GEO_TEMP2.RData")
#   is.miss2 <- which(is.na(geo.mat3$LONG))
#   prop.miss2 <- length(is.miss2)/nrow(geo.mat3)
# }
# 
# # save(geo.mat3,file="Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105_GEO_TEMP2.RData")
# load("Input/Events/ESOC/BFRS/esoc_PK_v10_GEO_TEMP2.RData")

# # Merge back
# geo.mat2$TEMP <- 1:nrow(geo.mat2)
# geo.mat2.a <- geo.mat2[-is.miss,]
# geo.mat2.b <- geo.mat2[is.miss,]
# tail(geo.mat2.b)
# geo.mat2.b$ADDRESS <- geo.mat2.b$LAT <- geo.mat2.b$LONG <- NULL
# geo.mat2.b <- merge(geo.mat2.b,geo.mat3,by="LOCATION",all.x=TRUE)
# geo.mat2.x <- rbind(geo.mat2.a,geo.mat2.b)
# geo.mat2.x <- geo.mat2.x[order(geo.mat2.x$TEMP),]
# geo.mat2.x$TEMP <- NULL
# geo.mat2 <- geo.mat2.x
# head(geo.mat2.x)
# tail(geo.mat2)
# #save(geo.mat2,file="Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105_GEO_TEMP.RData")
# load("Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105_GEO_TEMP.RData")

# Merge back
# Recreate old location variable
old.loc <- sort(unique(geo.mat$LOCATION))
geo.mat2$LOCATION <- old.loc
head(geo.mat2)
geo.mat$TEMP <- 1:nrow(geo.mat)
geo.mat$ADDRESS <- geo.mat$LAT <- geo.mat$LONG <- NULL
geo.mat <- merge(geo.mat,geo.mat2,by="LOCATION",all.x=TRUE)
geo.mat <- geo.mat[order(geo.mat$TEMP),]
head(geo.mat)
head(data)

# Add coordinates
data$LONG <- as.numeric(as.character(geo.mat$LONG))
data$LAT <- as.numeric(as.character(geo.mat$LAT))
data$iso3 <- as.character("AFG")

# Saving
head(data)
summary(data)
esoc.raw <- data
save(esoc.raw,file="Input/Events/ESOC/WITS/Iraq WITS Data/Iraq_WITS_GEO.RData")



#############################
## Geocode: Beissinger Soviet Union
#############################

rm(list=ls())

## Set directory
#setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")

##Protest data
#data <- read.csv("Input/Events/Beissinger/Soviet Union/Protest.csv")
##Riot data
data <- read.csv("Input/Events/Beissinger/Soviet Union/Riots.csv")
##Ukraine
#data <- read.csv("Input/Events/Beissinger/Ukraine/Ukraine.csv")

head(data)

#country.list <- c("KAZ", "LVA", "RUS", "EST", "UZB", "LTU", "ARM", "BLR", "UKR", "TJK", "GEO", "AZE", "MDA", "KGZ", "TKM")

## Renaming the republics
unique(data$REPUBLIC)
data$REPUBLIC_RENAME <- NA
data[data$REPUBLIC == "Kazakhstan", ][, "REPUBLIC_RENAME"] <- "Kazakhstan"
data[data$REPUBLIC == "Latvia", ][, "REPUBLIC_RENAME"] <- "Latvia"
data[data$REPUBLIC == "RSFSR", ][, "REPUBLIC_RENAME"] <- "Russia"
data[data$REPUBLIC == "Estonia", ][, "REPUBLIC_RENAME"] <- "Estonia"
data[data$REPUBLIC == "Uzbekistan", ][, "REPUBLIC_RENAME"] <- "Uzbekistan"
data[data$REPUBLIC == "Lithuania", ][, "REPUBLIC_RENAME"] <- "Lithuania"
data[data$REPUBLIC == "Armenia", ][, "REPUBLIC_RENAME"] <- "Armenia"
data[data$REPUBLIC == "Belorussia", ][, "REPUBLIC_RENAME"] <- "Belarus"
data[data$REPUBLIC == "Ukraine", ][, "REPUBLIC_RENAME"] <- "Ukraine"
data[data$REPUBLIC == "Tadzhikistan", ][, "REPUBLIC_RENAME"] <- "Tajikistan"
data[data$REPUBLIC == "Georgia", ][, "REPUBLIC_RENAME"] <- "Georgia"
data[data$REPUBLIC == "Azerbaidzhan", ][, "REPUBLIC_RENAME"] <- "Azerbaijan"
data[data$REPUBLIC == "Moldavia", ][, "REPUBLIC_RENAME"] <- "Moldova"
data[data$REPUBLIC == "Kirgizia", ][, "REPUBLIC_RENAME"] <- "Kyrgyzstan"
data[data$REPUBLIC == "Turkmenia", ][, "REPUBLIC_RENAME"] <- "Turkmenistan"
data$PLACENAME <- gsub("Alma-Ata","Almaty",data$PLACENAME)
data$PROVINCE <- gsub("Alma-Ata","Almaty",data$PROVINCE)

#Replacing the Soviet names with the current names; also using the Provicen names (since the google maps do not recognize small villages)
data$PLACENAME <- gsub("Araratskii raion","Ararat",data$PLACENAME)
data$PLACENAME <- gsub("Surkhtepa","Fergana",data$PLACENAME)
data$PLACENAME <- gsub("Frunzenskii raion","Fergana",data$PLACENAME)
data$PLACENAME <- gsub("Bisharyk","Fergana",data$PLACENAME)
data$PLACENAME <- gsub("Kirovskii raion","Fergana",data$PLACENAME)
data$PLACENAME <- gsub("Kimyagar","Fergana",data$PLACENAME)
data$PLACENAME <- gsub("Gorskii","Fergana",data$PLACENAME)
data$PLACENAME <- gsub("Margilanskii raion","Fergana",data$PLACENAME)
data$PLACENAME <- gsub("Margilanskii raion","Fergana",data$PLACENAME)
data$PLACENAME <- gsub("Naugarzan","Khujand",data$PLACENAME)
data$PLACENAME <- gsub("Yeralievo","Atyrau",data$PLACENAME)
data$PLACENAME <- gsub("Mangyshlak","Atyrau",data$PLACENAME)
data$PLACENAME <- gsub("Kzyl-Tiube","Atyrau",data$PLACENAME)
data$PLACENAME <- gsub("Munaishi","Atyrau",data$PLACENAME)
data$PLACENAME <- gsub("Tokh","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Gushchular","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Agbulag","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Yekhtsaog","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Dashdova","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Gostnaia","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Khrabod","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Nidzhevan","Nakhchivan",data$PLACENAME)
data$PLACENAME <- gsub("Gal'skii raion","Abkhazia",data$PLACENAME)
data$PLACENAME <- gsub("Gul'ripshskii raion","Abkhazia",data$PLACENAME)
data$PLACENAME <- gsub("Ochamchirskii raion","Abkhazia",data$PLACENAME)
data$PLACENAME <- gsub("Sukhumskii raion","Abkhazia",data$PLACENAME)
data$PLACENAME <- gsub("Gasanabad","Nagorno-Karabakh",data$PLACENAME)

data$PLACENAME <- gsub("Agdaban","Kalbajar",data$PLACENAME)
data$PLACENAME <- gsub("Agdeli","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Ak-Dovurake","Ak-Dovurak",data$PLACENAME)
data$PLACENAME <- gsub("Akhullu","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Akseran","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Alibeili","Alibeyli",data$PLACENAME)
data$PLACENAME <- gsub("Amirallar","Kalbajar",data$PLACENAME)
data$PLACENAME <- gsub("Amiranly","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Amralylar","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Aradu","Abkhazia",data$PLACENAME)
data$PLACENAME <- gsub("Aragyul","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Armianskie Borisy"," Shahumyan",data$PLACENAME)

data$PLACENAME <- gsub("Arpagiaduk","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Arus","Nakhchivan",data$PLACENAME)
data$PLACENAME <- gsub("Arutyunagomer","Kalbacar",data$PLACENAME)
data$PLACENAME <- gsub("Ashagy Askipara"," Qazakh",data$PLACENAME)
data$PLACENAME <- gsub("Ashagy Buzgov","Babek",data$PLACENAME)
data$PLACENAME <- gsub("Askipar","Gazakh",data$PLACENAME)
data$PLACENAME <- gsub("Askipira","Askipara",data$PLACENAME)
data$PLACENAME <- gsub("Atara-Armianskaia","Abkhazia",data$PLACENAME)
data$PLACENAME <- gsub("Avneri","Avneni",data$PLACENAME)
data$PLACENAME <- gsub("Avush","Nakhchivan",data$PLACENAME)
data$PLACENAME <- gsub("Babekskii raion","Babek",data$PLACENAME)

data$PLACENAME <- gsub("Badar","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Bagania-Airun","Baganis-Ayrun",data$PLACENAME)
data$PLACENAME <- gsub("Baganiz-Airun","Baganis-Ayrun",data$PLACENAME)
data$PLACENAME <- gsub("Bakati Kau","South Ossetia",data$PLACENAME)
data$PLACENAME <- gsub("Bala-Dzhafarly","Qazakh",data$PLACENAME)
data$PLACENAME <- gsub("Baladzhary","Qazakh",data$PLACENAME)
data$PLACENAME <- gsub("Bash Giuneipeia","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Bashkend","Gegharkunik",data$PLACENAME)
data$PLACENAME <- gsub("Berdadzor","Berkadzor",data$PLACENAME)
data$PLACENAME <- gsub("Berkavan","Nagorno-Karabakh",data$PLACENAME)

data$PLACENAME <- gsub("Bokhtarskii raion","Bokhtar",data$PLACENAME)
data$PLACENAME <- gsub("Border Outpost","Nakhchivan",data$PLACENAME)
data$PLACENAME <- gsub("Buniatly","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Chaikend","Dashkasan",data$PLACENAME)
data$PLACENAME <- gsub("Chailu","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Chardokhlu","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Charyshskii raion","Nakhchivan",data$PLACENAME)
data$PLACENAME <- gsub("Dachnoe","North Ossetia-Alania",data$PLACENAME)
data$PLACENAME <- gsub("Chimishliia","Moldova",data$PLACENAME)
data$PLACENAME <- gsub("Davanlar","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Dribon","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Dzhagazur","Nakhchivan",data$PLACENAME)

data[data$PROVINCE == "Nakhichevan ASSR", ][, "PLACENAME"] <- "Nakhchivan"
data[data$PROVINCE == "Nagorno-Karabakh AO", ][, "PLACENAME"] <- "Nagorno-Karabakh"
data[data$PROVINCE == "Chechen-Ingush ASSR", ][, "PLACENAME"] <- "Chechnya"
data[data$PROVINCE == "Azerbaidzhan (non ASSR/AO)", ][, "PLACENAME"] <- "Nagorno-Karabakh"
data[data$PROVINCE == "Andizhan", ][, "PLACENAME"] <- "Andijan"
data[data$PROVINCE == "Khmel'nitskii", ][, "PLACENAME"] <- "Khmelnytskyi"
data[data$PROVINCE == "Dnepropetrovsk", ][, "PLACENAME"] <- "Dnipropetrovsk"
data[data$PROVINCE == "Komi ASSR", ][, "PLACENAME"] <- "Komi Republic"
data[data$PROVINCE == "Northern Ossetian ASSR", ][, "PLACENAME"] <- "North Ossetia-Alania"



data$PLACENAME <- gsub("Derdavan","Berdavan",data$PLACENAME)
data$PLACENAME <- gsub("Dubesarii Vek","Dubasari",data$PLACENAME)
data$PLACENAME <- gsub("Dubossarskii raion","Dubasari",data$PLACENAME)
data$PLACENAME <- gsub("Dubossary-2","Dubasari",data$PLACENAME)
data$PLACENAME <- gsub("Dubossary","Dubasari",data$PLACENAME)
data$PLACENAME <- gsub("Dzhava","Java",data$PLACENAME)
data$PLACENAME <- gsub("Fakhrabad","Khatlon",data$PLACENAME)
data$PLACENAME <- gsub("Gerzel-aul","Chechnya",data$PLACENAME)
data$PLACENAME <- gsub("Goga","Fizuli",data$PLACENAME)
data$PLACENAME <- gsub("Khinzirak","Armenia",data$PLACENAME)
data$PLACENAME <- gsub("Dzhibikli","Ashagy Dzhibikli",data$PLACENAME)
data$PLACENAME <- gsub("Gadrut","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Geyali","Zangilan",data$PLACENAME)
data$PLACENAME <- gsub("Gorisskii raion","Armenia",data$PLACENAME)
data$PLACENAME <- gsub("Gursaly","Armenia",data$PLACENAME)
data$PLACENAME <- gsub("Kekhna Gyshlag","Azerbaijan",data$PLACENAME)
data$PLACENAME <- gsub("Khanashad","Azerbaijan",data$PLACENAME)


data$PLACENAME <- gsub("Vovieri-Bender","Bender",data$PLACENAME)
data$PLACENAME <- gsub("Zolochevskii raion","Kharkiv",data$PLACENAME)
data$PLACENAME <- gsub("Zolotoreve","Zakarpattia Oblast",data$PLACENAME)
data[data$PROVINCE == "Transcarpathian", ][, "PLACENAME"] <- "Zakarpattia Oblast"
data[data$PROVINCE == "Southern Ossetian AO", ][, "PLACENAME"] <- "Tskhinvali Region"
#data[data$PLACENAME == "South Ossetia", ][, "PLACENAME"] <- "Tskhinvali Region"
#data[data$PROVINCE == "Alma-Ata", ][, "PLACENAME"] <- "Almaty"
data[data$PROVINCE == "Yakut ASSR", ][, "PLACENAME"] <- "Yakutia"
data[data$PROVINCE == "Tadzhikistan--Other", ][, "PLACENAME"] <- "Tajikistan"

data$PLACENAME <- gsub("Yuzhnyi","Osh",data$PLACENAME)
data$PLACENAME <- gsub("Osh-2","Osh",data$PLACENAME)
data$PLACENAME <- gsub("Yavanskii raion","Qurghonteppa",data$PLACENAME)
data$PLACENAME <- gsub("Uzunagach-2","Almaty",data$PLACENAME)
data$PLACENAME <- gsub("Tazikend","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Vakhshkii raion","Qurghonteppa",data$PLACENAME)
data[data$PROVINCE == "Kurgan-Tiube", ][, "PLACENAME"] <- "Qurghonteppa"
data$PLACENAME <- gsub("Yukhara","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Zeiva","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Uzgenskii raion","Osh",data$PLACENAME)
data$PLACENAME <- gsub("Kyzylgadzhely","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Mirzankhanly","Nagorno-Karabakh",data$PLACENAME)
data$PLACENAME <- gsub("Niuvady","Armenia",data$PLACENAME)
data$PLACENAME <- gsub("Noemberianskii raion","Armenia",data$PLACENAME)
data$PLACENAME <- gsub("Noemberianskii raion","Armenia",data$PLACENAME)
data$PLACENAME <- gsub("Saral","Armenia",data$PLACENAME)
data$PLACENAME <- gsub("Teg","Armenia",data$PLACENAME)
data$PLACENAME <- gsub("Sisianskii raion","Armenia",data$PLACENAME)
data$PLACENAME <- gsub("Sarnakul","Armenia",data$PLACENAME)
data$PLACENAME <- gsub("Prudovo","Crimea",data$PLACENAME)
data$PLACENAME <- gsub("Nyuvedi","Armenia",data$PLACENAME)
data$PLACENAME <- gsub("Noemberianskii raion-2","Armenia",data$PLACENAME)
data$PLACENAME <- gsub("Tauzskii raion","Armenia",data$PLACENAME)
data$PLACENAME <- gsub("Vardenisskii raion","Armenia",data$PLACENAME)

#save(data, file="Input/Events/Beissinger/Soviet Union/Riots.RData")
load("Input/Events/Beissinger/Soviet Union/Riots.RData")
data[data$PROVINCE == "Kuliab", ][, "PLACENAME"] <- "Kulob"
data$PLACENAME <- gsub("Leontovo","Moldova",data$PLACENAME)
data[data$PROVINCE == "Abkhaz ASSR", ][, "PLACENAME"] <- "Abkhazia"
data$PLACENAME <- gsub("Samorda","Nagorno-Karabakh",data$PLACENAME)
data[data$PROVINCE == "Azerbaidzhan (non-ASSR/AO)", ][, "PLACENAME"] <- "Nagorno-Karabakh"

dim(data)
head(data)
tail(data)

#for Soviet Union
geo.mat <- data.frame(LOCATION=paste(data$PLACENAME, data$REPUBLIC_RENAME,sep=", "),ADDRESS="",LONG=NA,LAT=NA)
#for Ukraine
#geo.mat <- data.frame(LOCATION=paste(data$PLACENAME, data$REPUBLIC,sep=", "),ADDRESS="",LONG=NA,LAT=NA)
geo.mat$ADDRESS <- as.character(geo.mat$ADDRESS)
geo.mat$LOCATION <- as.character(geo.mat$LOCATION)

# Unique locations

geo.mat2 <- data.frame(LOCATION=sort(unique(geo.mat$LOCATION)),ADDRESS="",LONG=NA,LAT=NA)
geo.mat2$ADDRESS <- as.character(geo.mat2$ADDRESS)
geo.mat2$LOCATION <- as.character(geo.mat2$LOCATION)

# Initial pass (w/ Google)
i <- 2
for(i in 1:nrow(geo.mat2)){
  geo.out <- geoCode(geo.mat2$LOCATION[i],key=TRUE)
  print(paste(i,"of",nrow(geo.mat2)," ",geo.out[4]))
  geo.mat2$ADDRESS[i] <- geo.out[4]
  geo.mat2$LONG[i] <- geo.out[2]
  geo.mat2$LAT[i] <- geo.out[1]
}
tail(geo.mat2)
#i <- 134
#save(geo.mat2,file="Input/Events/Beissinger/Soviet Union/Protest_GEO_TEMP.RData")
save(geo.mat2,file="Input/Events/Beissinger/Soviet Union/Riot_GEO_TEMP.RData")
#save(geo.mat2,file="Input/Events/Beissinger/Ukraine/Ukraine_GEO_TEMP.RData")
#load("Input/Events/Beissinger/Soviet Union/Riot_GEO_TEMP.RData")
#load("Input/Events/Beissinger/Soviet Union/Protest_GEO_TEMP.RData")
#load("Input/Events/Beissinger/Ukraine/Ukraine_GEO_TEMP.RData")

# Fill out missing (w/ paid Google API key: set key=TRUE)
is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
while(prop.miss>.05){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in rev(is.miss)){
    geo.out <- geoCode(address = geo.mat2$LOCATION[i],key=TRUE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}
#save(geo.mat2,file="Input/Events/Beissinger/Soviet Union/Protest_GEO_TEMP2.Rdata")
#load("Input/Events/Beissinger/Soviet Union/Protest_GEO_TEMP2.RData")

# # Simplified addresses
is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)

# remove street address (for still-missing locations)
ss <- strsplit(geo.mat2[is.miss,"LOCATION"],", ")
deltz <- sapply(ss,"[",1)
deltz[grepl("^[[:upper:]]", deltz)] <- ""
j <- 1
while(sum(nchar(deltz)>0)>12){
  for(j in 1:length(ss)){
    if(nchar(deltz[j])>0){geo.mat2[is.miss,"LOCATION"][j] <- gsub(paste0(deltz[j],", "),"",geo.mat2[is.miss,"LOCATION"][j])}}
  ss <- strsplit(geo.mat2[is.miss,"LOCATION"],", ")
  deltz <- sapply(ss,"[",1)
  deltz[grepl("^[[:upper:]]", deltz)] <- ""
}
geo.mat2[is.miss,"LOCATION"][50:80]
# geo.mat2[is.miss,"LOCATION"] <- gsub("^(.*?), ","",geo.mat2[is.miss,"LOCATION"]) # remove street address (for still-missing locations)

# Unique locations
# Those that are still missing
geo.mat3 <- data.frame(LOCATION=sort(unique(geo.mat2$LOCATION[is.miss])),ADDRESS="",LONG=NA,LAT=NA)
geo.mat3$ADDRESS <- as.character(geo.mat3$ADDRESS)
geo.mat3$LOCATION <- as.character(geo.mat3$LOCATION)
head(geo.mat3)

is.miss2 <- which(is.na(geo.mat3$LONG))
prop.miss2 <- length(is.miss2)/nrow(geo.mat3)

while(prop.miss2>.05){
  print(paste("Proportion missing:",round(prop.miss2,2),"; N missing=",length(is.miss2)))
  for(i in rev(is.miss2)){
    geo.out <- geoCode(address = geo.mat3$LOCATION[i],key=TRUE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(prop.miss2,i,geo.out[4]))
    geo.mat3$ADDRESS[i] <- geo.out[4]
    geo.mat3$LONG[i] <- geo.out[2]
    geo.mat3$LAT[i] <- geo.out[1]
  }
  save(geo.mat3,file="Input/Events/Beissinger/Soviet Union/Riot_GEO_TEMP2.RData")
  is.miss2 <- which(is.na(geo.mat3$LONG))
  prop.miss2 <- length(is.miss2)/nrow(geo.mat3)
}

#save(geo.mat3,file="Input/Events/Beissinger/Soviet Union/Riot_GEO_TEMP2.RData")
#load("Input/Events/Beissinger/Soviet Union/Riot_GEO_TEMP2.RData")

# # Merge back
geo.mat2$TEMP <- 1:nrow(geo.mat2)
geo.mat2.a <- geo.mat2[-is.miss,]
geo.mat2.b <- geo.mat2[is.miss,]
tail(geo.mat2.b)
geo.mat2.b$ADDRESS <- geo.mat2.b$LAT <- geo.mat2.b$LONG <- NULL
geo.mat2.b <- merge(geo.mat2.b,geo.mat3,by="LOCATION",all.x=TRUE)
geo.mat2.x <- rbind(geo.mat2.a,geo.mat2.b)
geo.mat2.x <- geo.mat2.x[order(geo.mat2.x$TEMP),]
geo.mat2.x$TEMP <- NULL
geo.mat2 <- geo.mat2.x
head(geo.mat2.x)
tail(geo.mat2)
#save(geo.mat2,file="Input/Events/Beissinger/Soviet Union/Riot_GEO_TEMP.RData")
#load("Input/Events/Beissinger/Ukraine/Ukraine_GEO_TEMP.RData")

# Merge back
# Recreate old location variable
old.loc <- sort(unique(geo.mat$LOCATION))
geo.mat2$LOCATION <- old.loc
head(geo.mat2)
geo.mat$TEMP <- 1:nrow(geo.mat)
geo.mat$ADDRESS <- geo.mat$LAT <- geo.mat$LONG <- NULL
geo.mat <- merge(geo.mat,geo.mat2,by="LOCATION",all.x=TRUE)
geo.mat <- geo.mat[order(geo.mat$TEMP),]
head(geo.mat)
head(data)

# Add coordinates
data$LONG <- as.numeric(as.character(geo.mat$LONG))
data$LAT <- as.numeric(as.character(geo.mat$LAT))
#data$iso3 <- as.character("UKR")

# Saving
head(data)
summary(data)
#unique(data$LONG)
#unique(data$LAT)
beissinger.raw <- data
#save(beissinger.raw,file="Input/Events/Beissinger/Soviet Union/Riot_GEO.RData")
#load("Input/Events/Beissinger/Ukraine/Ukraine_GEO.RData")
head(beissinger.raw)
tail(beissinger.raw)





#############################
## Geocode: ABA
#############################

rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")

data <- read.csv("Input/Events/ABA/ABA_Database2.csv")
head(data)

# Village locations
data$PPL1 <- gsub("(\\b[a-z]{1})", "\\U\\1" ,gdata::trim(tolower(as.character(data$Village))), perl=TRUE)
data$PPL2 <- gsub("(\\b[a-z]{1})", "\\U\\1" ,gdata::trim(tolower(as.character(data$LOC))), perl=TRUE)
data$PPL2[data$LOC==1] <- data$PPL1[data$LOC==1]

## Recoding Districts from codes to regions
unique(data$District)
data$DISTRICT_RENAME <- NA
data[data$Distric == "1", ][, "DISTRICT_RENAME"] <- "North Darfur"
data[data$District == "2", ][, "DISTRICT_RENAME"] <- "West Darfur"
data[data$District == "3", ][, "DISTRICT_RENAME"] <- "South Darfur"
data[data$District == "99", ][, "DISTRICT_RENAME"] <- "Darfur"

##adding a column for the country
data$COUNTRY <- "Sudan"
data$ISO3 <- "SDN"

dim(data)#22720 BY 25
head(data)
tail(data)

#Geocoding
locz <- gdata::trim(paste0(data$PPL2,", ",data$COUNTRY))
locz2 <- gdata::trim(paste0(data$PPL1,", ",data$COUNTRY))
locz3 <- gdata::trim(paste0(data$DISTRICT_RENAME,", ",data$COUNTRY))
locz4 <- gdata::trim(paste0(data$PPL2,", ",data$DISTRICT_RENAME))
geo.mat <- data.frame(LOCATION=locz,ADDRESS="",LONG=NA,LAT=NA,LOCATION2=locz2,LOCATION3=locz3,LOCATION4=locz4)
geo.mat$ADDRESS <- as.character(geo.mat$ADDRESS)
geo.mat$LOCATION <- as.character(geo.mat$LOCATION)
geo.mat$LOCATION2 <- as.character(geo.mat$LOCATION2)
geo.mat$LOCATION3 <- as.character(geo.mat$LOCATION3)
geo.mat$LOCATION4 <- as.character(geo.mat$LOCATION4)

# Unique locations
geo.mat2 <- data.frame(LOCATION=sort(unique(geo.mat$LOCATION)),ADDRESS="",LONG=NA,LAT=NA)
geo.mat2$ADDRESS <- as.character(geo.mat2$ADDRESS)
geo.mat2$LOCATION <- as.character(geo.mat2$LOCATION)

# Initial pass (w/ Google)
i <- 2
for(i in 1:nrow(geo.mat2)){
  geo.out <- geoCode(geo.mat2$LOCATION[i],key=FALSE)
  print(paste(i,"of",nrow(geo.mat2)," ",geo.out[4]))
  geo.mat2$ADDRESS[i] <- geo.out[4]
  geo.mat2$LONG[i] <- geo.out[2]
  geo.mat2$LAT[i] <- geo.out[1]
}
geo.mat2.backup <- geo.mat2
geo.mat2$LONG[!grepl("Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$LAT[!grepl("Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$ADDRESS[!grepl("Sudan$",geo.mat2$ADDRESS)] <- ""
geo.mat2$LONG[grepl("^Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$LAT[grepl("^Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$ADDRESS[grepl("^Sudan$",geo.mat2$ADDRESS)] <- ""
tail(geo.mat2)
#i <- 134
save(geo.mat2,file="Input/Events/ABA/ABA_GEO_TEMP_v2.RData")
# load("Input/Events/ABA/ABA_GEO_TEMP_v2.RData")
summary(geo.mat2)
tail(geo.mat2)

# Fill out missing (w/ paid Google API key: set key=TRUE)
is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
while(prop.miss>.5){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in rev(is.miss)){
    geo.out <- geoCode(address = geo.mat2$LOCATION[i],key=TRUE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}
geo.mat2.backup <- geo.mat2
geo.mat2$LONG[!grepl("Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$LAT[!grepl("Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$ADDRESS[!grepl("Sudan$",geo.mat2$ADDRESS)] <- ""
geo.mat2$LONG[grepl("^Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$LAT[grepl("^Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$ADDRESS[grepl("^Sudan$",geo.mat2$ADDRESS)] <- ""
tail(geo.mat2)
#i <- 134
save(geo.mat2,file="Input/Events/ABA/ABA_GEO_TEMP2_v2.RData")
# load("Input/Events/ABA/ABA_GEO_TEMP2_v2.RData")

# Alternative addresses
geo.mat3 <- merge(geo.mat2,geo.mat[,c("LOCATION","LOCATION2","LOCATION3","LOCATION4")],by="LOCATION",all.x=T,all.y=F)
geo.mat3 <- geo.mat3[!duplicated(geo.mat3$LOCATION),]
mean(geo.mat3$LOCATION==geo.mat2$LOCATION)
geo.mat2 <- geo.mat3; rm(geo.mat3)
head(geo.mat2)

is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
while(prop.miss>.5){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in rev(is.miss)){
    geo.out <- geoCode(address = geo.mat2$LOCATION2[i],key=TRUE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}
geo.mat2.backup <- geo.mat2
geo.mat2$LONG[!grepl("Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$LAT[!grepl("Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$ADDRESS[!grepl("Sudan$",geo.mat2$ADDRESS)] <- ""
geo.mat2$LONG[grepl("^Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$LAT[grepl("^Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$ADDRESS[grepl("^Sudan$",geo.mat2$ADDRESS)] <- ""
tail(geo.mat2)
#i <- 134
save(geo.mat2,file="Input/Events/ABA/ABA_GEO_TEMP3_v2.RData")
# load("Input/Events/ABA/ABA_GEO_TEMP3_v2.RData")

# Alternative addresses 2
is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
while(prop.miss>.5){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in rev(is.miss)){
    geo.out <- geoCode(address = as.character(geo.mat2$LOCATION4[i]),key=TRUE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}
geo.mat2.backup <- geo.mat2
geo.mat2$LONG[!grepl("Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$LAT[!grepl("Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$ADDRESS[!grepl("Sudan$",geo.mat2$ADDRESS)] <- ""
geo.mat2$LONG[grepl("^Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$LAT[grepl("^Sudan$",geo.mat2$ADDRESS)] <- NA
geo.mat2$ADDRESS[grepl("^Sudan$",geo.mat2$ADDRESS)] <- ""
tail(geo.mat2)
#i <- 134
save(geo.mat2,file="Input/Events/ABA/ABA_GEO_TEMP4_v2.RData")
# load("Input/Events/ABA/ABA_GEO_TEMP4_v2.RData")

# Simplified addresses
geo.mat2$GEOPRECISION <- "adm2"
is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
geo.mat2$GEOPRECISION[is.miss] <- "adm1"
geo.mat2$LOCATION3 <- gsub("^Darfur,","Zalingei,",geo.mat2$LOCATION3)
while(prop.miss>.01){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in rev(is.miss)){
    geo.out <- geoCode(address = as.character(geo.mat2$LOCATION3[i]),key=TRUE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}
geo.mat2.backup <- geo.mat2
geo.mat2[is.miss,]
head(geo.mat2)
tail(geo.mat2)
#i <- 134
save(geo.mat2,file="Input/Events/ABA/ABA_GEO_TEMP5_v2.RData")


# Fix addresses outside Darfur
load("Input/Events/ABA/ABA_GEO_TEMP5_v2.RData")
head(geo.mat2)
map <- readRDS("Input/GIS/Borders/GADM/SDN_adm1.rds")
map <- map[grep("Darfur",map$NAME_1),]
coords.temp <- data.frame(LONG=as.numeric(geo.mat2$LONG),LAT=as.numeric(geo.mat2$LAT))
plot(map)
o <- over(SpatialPoints(coords = coords.temp,proj4string = CRS(proj4string(map))),map)
geo.mat2$LONG[which(is.na(o$OBJECTID))] <- NA
geo.mat2$LAT[which(is.na(o$OBJECTID))] <- NA
geo.mat2$ADDRESS[which(is.na(o$OBJECTID))] <- NA
geo.mat2$GEOPRECISION[which(is.na(o$OBJECTID))] <- "adm1"

is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
geo.mat2$GEOPRECISION[is.miss] <- "adm1"
while(prop.miss>.01){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in rev(is.miss)){
    geo.out <- geoCode(address = as.character(geo.mat2$LOCATION3[i]),key=TRUE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}
geo.mat2.backup <- geo.mat2
geo.mat2[is.miss,]

save(geo.mat2,file="Input/Events/ABA/ABA_GEO_TEMP6_v2.RData")


###
# Merge back
###
load("Input/Events/ABA/ABA_GEO_TEMP6_v2.RData")
geo.mat2.backup <- geo.mat2
unique(geo.mat2.backup$LAT)
head(geo.mat2)
tail(geo.mat2)

# # Merge back
geo.mat$TEMP2 <- paste0(geo.mat$LOCATION,"_",geo.mat$LOCATION2,"_",geo.mat$LOCATION3,"_",geo.mat$LOCATION4)
geo.mat2$TEMP2 <- paste0(geo.mat2$LOCATION,"_",geo.mat2$LOCATION2,"_",geo.mat2$LOCATION3,"_",geo.mat2$LOCATION4)

geo.mat.backup <- geo.mat
# geo.mat <- geo.mat.backup
geo.mat$TEMP <- 1:nrow(geo.mat)
geo.mat$ADDRESS <- geo.mat$LAT <- geo.mat$LONG <- NULL
head(geo.mat)

geo.mat.a <- merge(geo.mat,geo.mat2[,c("TEMP2","ADDRESS","LONG","LAT","GEOPRECISION")],by="TEMP2",all.x=TRUE)
geo.mat.a <- geo.mat.a[order(geo.mat.a$TEMP),]
geo.mat.b <- merge(geo.mat,geo.mat2[,c("LOCATION2","ADDRESS","LONG","LAT","GEOPRECISION")],by="LOCATION2",all.x=TRUE)
geo.mat.b <- geo.mat.b[order(geo.mat.b$TEMP),]
geo.mat.c <- merge(geo.mat,geo.mat2[,c("LOCATION4","ADDRESS","LONG","LAT","GEOPRECISION")],by="LOCATION4",all.x=TRUE)
geo.mat.c <- geo.mat.c[order(geo.mat.c$TEMP),]
geo.mat.d <- merge(geo.mat,geo.mat2[,c("LOCATION","ADDRESS","LONG","LAT","GEOPRECISION")],by="LOCATION",all.x=TRUE)
geo.mat.d <- geo.mat.d[order(geo.mat.d$TEMP),]

# Combine
mean(geo.mat.a$GEOPRECISION%in%"adm2")
mean(geo.mat.b$GEOPRECISION%in%"adm2")
mean(geo.mat.c$GEOPRECISION%in%"adm2")
mean(geo.mat.d$GEOPRECISION%in%"adm2")
mean(is.na(geo.mat.a$LONG))
mean(is.na(geo.mat.b$LONG))
mean(is.na(geo.mat.c$LONG))
mean(is.na(geo.mat.d$LONG))

# # FInd optimal set
# geo.mat.e <- geo.mat.a
# geo.mat.e$LONG[is.na(geo.mat.e$LONG)|geo.mat.e$LONG==""] <- geo.mat.d$LONG[is.na(geo.mat.e$LONG)|geo.mat.e$LONG==""]
# geo.mat.e$LAT[is.na(geo.mat.e$LAT)|geo.mat.e$LAT==""] <- geo.mat.d$LAT[is.na(geo.mat.e$LAT)|geo.mat.e$LAT==""]
# geo.mat.e$ADDRESS[is.na(geo.mat.e$ADDRESS)|geo.mat.e$ADDRESS==""] <- geo.mat.d$ADDRESS[is.na(geo.mat.e$ADDRESS)|geo.mat.e$ADDRESS==""]
# geo.mat.e$GEOPRECISION[is.na(geo.mat.e$GEOPRECISION)|geo.mat.e$GEOPRECISION==""] <- geo.mat.d$GEOPRECISION[is.na(geo.mat.e$GEOPRECISION)|geo.mat.e$GEOPRECISION==""]
# tail(geo.mat.e)

mean(geo.mat.d$LOCATION==geo.mat$LOCATION)
geo.mat <- geo.mat.d
head(geo.mat)
tail(geo.mat)
head(data)

# Add coordinates
data$LONG <- as.numeric(as.character(geo.mat$LONG))
data$LAT <- as.numeric(as.character(geo.mat$LAT))
data$GEOPRECIS <- as.character(geo.mat$GEOPRECISION)
tail(geo.mat)

# Fix precision codes
data$GEOPRECIS[grep("Darfur|Zalingei",geo.mat$ADDRESS)] <- "adm1"

# Saving
tail(data)
summary(data)
#unique(data$LONG)
#unique(data$LAT)
aba.raw <- data
save(aba.raw,file="Input/Events/ABA/ABA_GEO_v2.RData")
load("Input/Events/ABA/ABA_GEO_v2.RData")
head(aba.raw)
tail(aba.raw)
unique(aba.raw$LAT)


#############################
## Geocode: NVMS
#############################

rm(list=ls())

## Set directory
#setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")

data <- read.csv("Input/Events/NVMS/NVMS-Final/nvms.csv")
head(data)
tail(data)
colnames(data)

## Recoding Districts from codes to regions
unique(data$District)
data$DISTRICT_RENAME <- NA
data[data$Distric == "1", ][, "DISTRICT_RENAME"] <- "North Darfur"
data[data$District == "2", ][, "DISTRICT_RENAME"] <- "West Darfur"
data[data$District == "3", ][, "DISTRICT_RENAME"] <- "South Darfur"
data[data$District == "99", ][, "DISTRICT_RENAME"] <- "NA"

##adding a column for the country
data$COUNTRY <- "Indonesia"
data$ISO3 <- "IDN"

#Replacing names
data$Village <- gsub("AA-DAAR","North Darfur",data$Village)
#data$Village <- tolower(data$Village)
data$Village <- gsub("TURKENYA","Darfur",data$Village)
data$Village <- gsub("MESKEE","North Darfur",data$Village)                                         
data$Village <- gsub("ADARE","North Darfur",data$Village)
data$Village <- gsub("ADARE","North Darfur",data$Village)
data$Village <- gsub("GASUMBA","North Darfur",data$Village)
data$DISTRICT_RENAME <- gsub("NA","Darfur",data$DISTRICT_RENAME)
dim(data)#22720 BY 25
head(data)
tail(data)

#Geocoding
geo.mat <- data.frame(LOCATION=paste(data$Kecamatan1,data$Kecamatan2, data$Provinsi, data$kabupaten, data$COUNTRY, sep=", "),ADDRESS="",LONG=NA,LAT=NA)
geo.mat$ADDRESS <- as.character(geo.mat$ADDRESS)
geo.mat$LOCATION <- as.character(geo.mat$LOCATION)

# Unique locations
geo.mat2 <- data.frame(LOCATION=sort(unique(geo.mat$LOCATION)),ADDRESS="",LONG=NA,LAT=NA)
geo.mat2$ADDRESS <- as.character(geo.mat2$ADDRESS)
geo.mat2$LOCATION <- as.character(geo.mat2$LOCATION)

# Initial pass (w/ Google)
i <- 2
for(i in 1:nrow(geo.mat2)){
  geo.out <- geoCode(geo.mat2$LOCATION[i],key=TRUE)
  print(paste(i,"of",nrow(geo.mat2)," ",geo.out[4]))
  geo.mat2$ADDRESS[i] <- geo.out[4]
  geo.mat2$LONG[i] <- geo.out[2]
  geo.mat2$LAT[i] <- geo.out[1]
}
tail(geo.mat2)
save(geo.mat2,file="Input/Events/NVMS/NVMS_GEO_TEMP.RData")
#load("Input/Events/NVMS/NVMS_GEO_TEMP.RData")
head(geo.mat2)
tail(geo.mat2)

# Fill out missing (w/ paid Google API key: set key=TRUE)
is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
while(prop.miss>.05){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in rev(is.miss)){
    geo.out <- geoCode(address = geo.mat2$LOCATION[i],key=TRUE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}
save(geo.mat2,file="Input/Events/NVMS/NVMS_GEO_TEMP2.Rdata")
#load("Input/Events/NVMS/NVMS_GEO_TEMP2.RData")



# Merge back
# Recreate old location variable
old.loc <- sort(unique(geo.mat$LOCATION))
geo.mat2$LOCATION <- old.loc
head(geo.mat2)
geo.mat$TEMP <- 1:nrow(geo.mat)
geo.mat$ADDRESS <- geo.mat$LAT <- geo.mat$LONG <- NULL
geo.mat <- merge(geo.mat,geo.mat2,by="LOCATION",all.x=TRUE)
geo.mat <- geo.mat[order(geo.mat$TEMP),]
head(geo.mat)
head(data)

# Add coordinates
data$LONG <- as.numeric(as.character(geo.mat$LONG))
data$LAT <- as.numeric(as.character(geo.mat$LAT))

# Saving
head(data)
summary(data)
nvms.raw <- data
save(nvms.raw,file="Input/Events/NVMS/NVMS_GEO.RData")
head(nvms.raw)
tail(nvms.raw)


#############################
## Geocode: Davenport: Rwanda
#############################

rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")

data <- read.csv("Input/Events/Davenport/Rwanda/genodynamics_commune_data.csv")
head(data)
tail(data)
colnames(data)

## Recoding Districts from codes to regions
unique(data$commune)
unique(data$prefecture)

##adding a column for the country
data$COUNTRY <- "Rwanda"
data$ISO3 <- "RWA"
dim(data) #885 by 118

#Geocoding
# geo.mat <- data.frame(LOCATION=paste0(gsub("(\\b[a-z]{1})", "\\U\\1" ,data$commune, perl=TRUE),", ",data$prefecture,", ",data$COUNTRY),ADDRESS="",LONG=NA,LAT=NA)
geo.mat <- data.frame(LOCATION=paste0(gsub("(\\b[a-z]{1})", "\\U\\1" ,data$commune, perl=TRUE),", ",data$COUNTRY),ADDRESS="",LONG=NA,LAT=NA)
geo.mat$ADDRESS <- as.character(geo.mat$ADDRESS)
geo.mat$LOCATION <- as.character(geo.mat$LOCATION)

# Unique locations
geo.mat2 <- data.frame(LOCATION=sort(unique(geo.mat$LOCATION)),ADDRESS="",LONG=NA,LAT=NA)
geo.mat2$ADDRESS <- as.character(geo.mat2$ADDRESS)
geo.mat2$LOCATION <- as.character(geo.mat2$LOCATION)

# Initial pass (w/ Google)
i <- 1
for(i in 1:nrow(geo.mat2)){
  geo.out <- geoCode(geo.mat2$LOCATION[i],key=TRUE)
  print(paste(i,"of",nrow(geo.mat2)," ",geo.out[4]))
  geo.mat2$ADDRESS[i] <- geo.out[4]
  geo.mat2$LONG[i] <- geo.out[2]
  geo.mat2$LAT[i] <- geo.out[1]
}
tail(geo.mat2)
save(geo.mat2,file="Input/Events/Davenport/Rwanda/DavenportRwanda_GEO_TEMP.RData")
#load("Input/Events/Davenport/Rwanda/DavenportRwanda_GEO_TEMP.RData")
head(geo.mat2)
tail(geo.mat2)

# Fill out missing (w/ paid Google API key: set key=TRUE)
is.miss <- which(is.na(geo.mat2$LONG))
prop.miss <- length(is.miss)/nrow(geo.mat2)
while(prop.miss>.05){
  print(paste("Proportion missing:",round(prop.miss,2),"; N missing=",length(is.miss)))
  for(i in rev(is.miss)){
    geo.out <- geoCode(address = geo.mat2$LOCATION[i],key=FALSE)
    # geo.out <- geoCode3(query = geo.mat2$LOCATION[i],boundingBox = paste0(c(bbx[2,2],bbx[1,1],bbx[2,1],bbx[1,2]),collapse=","))
    print(paste(i,geo.out[4]))
    geo.mat2$ADDRESS[i] <- geo.out[4]
    geo.mat2$LONG[i] <- geo.out[2]
    geo.mat2$LAT[i] <- geo.out[1]
  }
  is.miss <- which(is.na(geo.mat2$LONG))
  prop.miss <- length(is.miss)/nrow(geo.mat2)
}

save(geo.mat2,file="Input/Events/Davenport/Rwanda/DavenportRwanda_GEO_TEMP2.RData")
#load("Input/Events/Davenport/Rwanda/DavenportRwanda_GEO_TEMP2.RData")



# Merge back
# Recreate old location variable
old.loc <- sort(unique(geo.mat$LOCATION))
geo.mat2$LOCATION <- old.loc
head(geo.mat2)
geo.mat$TEMP <- 1:nrow(geo.mat)
geo.mat$ADDRESS <- geo.mat$LAT <- geo.mat$LONG <- NULL
geo.mat <- merge(geo.mat,geo.mat2,by="LOCATION",all.x=TRUE)
geo.mat <- geo.mat[order(geo.mat$TEMP),]
head(geo.mat)
head(data)

# Add coordinates
data$LONG <- as.numeric(as.character(geo.mat$LONG))
data$LAT <- as.numeric(as.character(geo.mat$LAT))
data$ADDRESS <- as.character(geo.mat$ADDRESS)

# Saving
head(data)
summary(data)
davenport.raw <- data
save(davenport.raw,file="Input/Events/Davenport/Rwanda/DavenportRwanda_GEO.RData")
head(davenport.raw)









