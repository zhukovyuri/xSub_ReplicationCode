

########################################
## Google geocode
########################################

goo.key <- "AIzaSyAaVMlQFb7gDjFyaByW37d0X8VTIZ9FoiQ"

## Install & load packages (all at once)
list.of.packages <- c("RCurl","RJSONIO","plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Google geocode

url <- function(address, return.call = "json", sensor = "false",key=FALSE) {
  root <- "http://maps.google.com/maps/api/geocode/"
  roots <- "https://maps.google.com/maps/api/geocode/"
  u <- paste(roots, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  if(key){u <- paste(roots, return.call, "?address=", address,"&key=",goo.key, sep = "")}
  return(URLencode(u))
}


geoCode <- function(address,verbose=FALSE,key=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address,key=key)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type  <- x$results[[1]]$geometry$location_type
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA, NA))
  }
}


########################################
## Yandex geocode
########################################

## Install & load packages (all at once)
list.of.packages <- c("RCurl","RJSONIO","plyr","stringr","geonames")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


# query <- tweets$LOCATION[i]
# query <- "32.752625,-97.138904"
# query <- "Hong Kong"

url2 <- function(address, return.call = "json", sensor = "false") {
  #root <- "http://geocode-maps.yandex.ru/1.x/?format=json&lang=en-BR&geocode="
  root <- "https://geocode-maps.yandex.ru/1.x/?format=json&lang=en-BR&geocode="
  u <- paste(root, address, sep = "")
  return(URLencode(u))
}


geoCode2 <- function(query,match.num=1){
  # target=paste0("http://geocode-maps.yandex.ru/1.x/?format=json&lang=en-BR&geocode=",query)
  # rd <- readLines(target, warn="F",encoding="UTF-8") 
  # dat <- fromJSON(rd)
  
  u <- url2(query)
  doc <- getURL(u)
  dat <- fromJSON(doc,simplify = FALSE)
  
  #Exctract address and location data
  if(length(dat$response$GeoObjectCollection$featureMember)>0){
    address <- ifelse(length(dat$response$GeoObjectCollection$featureMember[[match.num]]$GeoObject$metaDataProperty$GeocoderMetaData$AddressDetails$Country)>2,dat$response$GeoObjectCollection$featureMember[[match.num]]$GeoObject$metaDataProperty$GeocoderMetaData$AddressDetails$Country$AddressLine,dat$response$GeoObjectCollection$featureMember[[match.num]]$GeoObject$metaDataProperty$GeocoderMetaData$AddressDetails$Country[2])
    if(address=="NULL"){address<-NA}
    pos <- dat$response$GeoObjectCollection$featureMember[[match.num]]$GeoObject$Point
    # temp <- unlist(str_split(pos," "))
    temp <- unlist(strsplit(unlist(pos)," "))
    latitude=as.numeric(temp)[2]
    longitude=as.numeric(temp)[1]}
  
  if(length(dat$response$GeoObjectCollection$featureMember)==0){address<- NA;longitude=NA;latitude=NA}
  
  return(c(latitude,longitude,address))
  
}




########################################
## MapQuest
########################################

my.key <- "Uc6ZJAsmiBdobU5o43ATNs1GiXfm9OEt"

url3 <- function(address, return.call = "json", sensor = "false") {
  root <- paste0("http://www.mapquestapi.com/geocoding/v1/address?key=",my.key,"&location=")
  u <- paste(root, address, sep = "")
  return(URLencode(u))
}


geoCode3 <- function(query,match.num=1,boundingBox=NULL){
  # target=paste0("http://geocode-maps.yandex.ru/1.x/?format=json&lang=en-BR&geocode=",query)
  # rd <- readLines(target, warn="F",encoding="UTF-8") 
  # dat <- fromJSON(rd)
  if(length(boundingBox>0)){query<-paste0(query,"&boundingBox=",boundingBox)}
  u <- url3(query)
  doc <- getURL(u)
  dat <- fromJSON(doc,simplify = FALSE)
  
  #Exctract address and location data
  if(length(dat$results)>0){
    address <- ifelse(length(dat$results[[1]]$locations[[match.num]])>2,dat$results[[1]]$locations[[match.num]]$adminArea5,dat$results[[1]]$providedLocation$location)
    if(address=="NULL"){address<-NA}
    pos <- dat$results[[1]]$locations[[match.num]]$latLng
    # temp <- unlist(str_split(pos," "))
    temp <- unlist(pos)
    latitude=as.numeric(temp)[1]
    longitude=as.numeric(temp)[2]}
  
  if(length(dat$results[[1]]$locations)==0){address<- NA;longitude=NA;latitude=NA}
  
  return(c(latitude,longitude,address))
  
}



#############################
## DDG search
#############################

ddg.search <- function(search){browseURL(paste("http://www.duckduckgo.com/?q=", search, sep = ""))}

#############################
## Google search
#############################

#google.search <- function(search){browseURL(paste("http://www.google.com/search?q=", search, sep = ""))}

#############################
## Actor list
#############################

actorList <- function(sub,sidea="ACTOR1",sideb="ACTOR2",timevar=NULL,countryvar=NULL){
  
  for(j in 1:ncol(sub)){if(class(sub[,j])=="factor"){sub[,j] <- as.character(sub[,j])}}
  nn <- sort(unique(c(unique(sub[,sidea]),unique(sub[,sideb]))))
  
  if(length(timevar)>0){
    for(l in 1:length(nn)){nn[l] <- paste0(nn[l]," (",paste(range(as.numeric(as.character(sub[sub[,sidea]%in%nn[l]|sub[,sideb]%in%nn[l],timevar]))),collapse=" - "),")")}
  }
  
  if(length(countryvar)>0){
    for(l in 1:length(nn)){nn[l] <- paste0(nn[l]," ",sub[1,countryvar])}
  }
  
  ## General actor dictionary
  actors <- c()
  actors_GOV <- c()
  actors_REB <- c()
  actors_CIV <- c()
  actors_OTH <- c()
  
  # Who to include?  
  N <- length(nn)
  i <- 1
  while(i <= N){input <- readline(paste("Include: ",nn[i],"? (G/R/C/O/Y/?):   "));
  if(input=="?"){ddg.search(nn[i]); nn <- c(nn[1:i],nn[i:length(nn)])}
  if(input=="1"|input=="y"|input=="Y"|input=="G"|input=="g"|input=="R"|input=="r"|input=="C"|input=="c"|input=="O"|input=="o"){actors <- c(actors,nn[i])}
  if(input=="G"|input=="g"){actors_GOV <- c(actors_GOV,nn[i])}
  if(input=="R"|input=="r"){actors_REB <- c(actors_REB,nn[i])}
  if(input=="C"|input=="c"){actors_CIV <- c(actors_CIV,nn[i])}
  if(input=="O"|input=="o"){actors_OTH <- c(actors_OTH,nn[i])}
  N <- length(nn); i <- i+1
  }
  
  
  actors <- sort(unique(actors))
  actors_GOV <- sort(unique(actors_GOV))
  actors_REB <- sort(unique(actors_REB))
  actors_CIV <- sort(unique(actors_CIV))
  actors_OTH <- sort(unique(actors_OTH))
  nn[!nn%in%actors]
  
  return(list(actors=actors,actors_GOV=actors_GOV,actors_REB=actors_REB,actors_CIV=actors_CIV,actors_OTH=actors_OTH))
}





#############################
## Nils' functions
#############################

streg.sort <- function(data, unitvar, timevar) {
  data[order(data[,unitvar], data[,timevar]),]
}

streg.tlag <- function(data, unitvar,timevar, lagvar, order=1) {
  result <- numeric(nrow(data))
  timevalues <- data[,timevar]
  varvalues <- data[,lagvar]
  times <- unique(timevalues)
  unitvalues <- data[,unitvar]
  
  # set initial lag values to NA
  for (time in 1:order) {
    result[timevalues==times[time]] <- NA
  }
  
  # compute lag
  for (time in (order+1):length(times)) {#print(time)
    if(sum(timevalues%in%times[time])==sum(timevalues%in%times[time-order])){
      result[timevalues%in%times[time]] <- varvalues[timevalues%in%times[time-order]]
    }
    if(sum(timevalues%in%times[time])!=sum(timevalues%in%times[time-order])){
      units.in <- unitvalues[timevalues%in%times[time]]
      units.now <- unitvalues[timevalues%in%times[time-order]]
      lag.vals <- varvalues[timevalues%in%times[time-order]]
      temp <- merge(data.frame(units=units.in),data.frame(lag.vals=lag.vals[units.now%in%units.in],units=units.now[units.now%in%units.in]),by="units",all.x=T,all.y=F)$lag.vals
      temp[is.na(temp)] <- 0
      result[timevalues%in%times[time]] <- temp
    }
  }
  result
}



streg.slag <- function(data, timevar, lagvar, Wmat) {
  result <- numeric(nrow(data))
  timevalues <- data[,timevar]
  varvalues <- data[,lagvar]
  times <- unique(timevalues)
  for (time in 1:length(times)) {
    result[timevalues==times[time]] <- Wmat %*% varvalues[timevalues==times[time]]
  }
  result
}

