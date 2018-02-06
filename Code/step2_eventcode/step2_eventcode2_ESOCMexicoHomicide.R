rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)



#############################
## Aggregate: GADM
#############################

rm(list=ls())

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# List of files
filez <- dir("Output/Output_ESOCMexicoHomicide/Events")
filez <- filez[grep("Events",filez)]
cntz <- gsub("ESOCMexicoHomicide_Events_|.RData","",filez)
#cntz <- cntz[cntz!="000"]
cntz

# Exceptions
source("Code/step2_eventcode/step2x_eventcode_admex.R")

k <- 1

# Load events
load(paste0("Output/Output_ESOCMexicoHomicide/Events/ESOCMexicoHomicide_Events_",cntz[k],".RData",sep=""))
names(events) <- toupper(names(events))
events <- events[is.finite(as.numeric(as.character(events$LONG)))&is.finite(as.numeric(as.character(events$LAT))),]
coords <- events[,c("LONG","LAT")]
for(m in 1:2){coords[,m] <- as.numeric(as.character(coords[,m]))}
head(events)
tail(events)


# Dates
datez <- seq(as.Date("1900-01-01"), as.Date("2016-10-24"), by="days")
ticker <-data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
ticker <- ticker[as.character(ticker$YEAR) > 1997 & as.character(ticker$YEAR) < 2012,]
head(ticker)
tail(ticker)


## Open loop (adm)
a <- 1
for(a in 0:2){print(a)
  admz <- a
  #load(url(paste("http://biogeo.ucdavis.edu/data/gadm2/R/",cntz[k],"_adm",admz,".rds",sep="")))
  
  # Start adm2 exception
  if(!((a==1)&(cntz[k]%in%noadm1))){
    if(!((a==2)&(cntz[k]%in%noadm2))){
      
      if(!paste0(cntz[k],"_adm",admz,".rds",sep="")%in%dir("Input/GIS/Borders/GADM/")){
        download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",admz,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",admz,".rds",sep=""))}
      gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",admz,".rds",sep=""))
      
      if(a==0){
        data("wrld_simpl")
        gadm0 <- wrld_simpl[wrld_simpl$ISO3==cntz[k],]
        gadm <- SpatialPolygonsDataFrame(Sr = as(gadm0,"SpatialPolygons"),data = gadm@data,match.ID = FALSE)
        rm(gadm0)
      }
      
      # Overlay
      sp.data <- SpatialPoints(coords,proj4string=CRS(proj4string(gadm)))
      o <- over(sp.data, as(gadm, "SpatialPolygons"))
      o <- data.frame(KEY=o)
      subdata <- cbind(events,o)
      gadm.data <- as.data.frame(gadm)
      gadm.data$KEY <- 1:nrow(gadm.data)
      head(gadm.data)
      # plot(gadm);points(sp.data)
      
      ## Yearly event counts
      disag <- sort(unique(gadm.data$KEY))
      i <- 1
      agg.sub <- lapply(1:length(disag),function(i){
        sub <- subdata[subdata$KEY%in%disag[i],]
        sub <- merge(ticker,sub[,],by.x="YEAR",by.y="DATE")
        sub$YSID <- sub$KEY*(10^nchar(max(as.character(ticker$YEAR))))+as.numeric(as.character(sub$YEAR))
        sub$YSID2 <- as.numeric(as.character(sub$YEAR))*(10^nchar(max(disag)))+sub$KEY
        vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
        sub
        if(nrow(sub)>0){
          sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(YSID=sub$YSID,YSID2=sub$YSID2,KEY=sub$KEY,YEAR=sub$YEAR),FUN=function(x){sum(x,na.rm=TRUE)})
        }
        if(nrow(sub)==0){
          sub <- sub[,c("YSID","YSID2","KEY","YEAR",vars)]
        }
        # Remaining years (0's)
        wids <- sort(unique(as.numeric(as.character(ticker$YEAR))))
        wids <- wids[!wids%in%sub$YEAR]
        if(length(wids)>0){
          sub. <- data.frame(YSID=disag[i]*(10^nchar(max(wids)))+wids,YSID2=wids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],YEAR=wids)
          sub.. <- as.data.frame(matrix(0,nrow=length(wids),ncol=length(vars)))
          names(sub..) <- vars
          sub <- rbind(sub,cbind(sub.,sub..))
        }
        # # Merge with months, years
        # tix <- aggregate(ticker[,c("YEAR")],by=list(YRMO=ticker$YRMO),FUN=function(x){x[1]})
        # sub <- merge(tix,sub,by="YRMO",all.x=F,all.y=T)
        # Merge with covariates
        sub <- merge(gadm.data,sub,by="KEY",all.x=F,all.y=T)
        sub <- sub[order(sub$YSID),]
        sub
      })
      agg.data <- do.call(rbind,agg.sub)
      agg.data <- agg.data[order(agg.data$YSID2),]
      adm.year <- agg.data
      head(adm.year)
      save(adm.year,file=paste0("Output/Output_ESOCMexicoHomicide/ESOCMexicoHomicide_",cntz[k],"_adm",admz,"_year.RData"))
      
      # End adm2 exception
    }
  }
  # Close loop (adm)
}



#############################
## Grid cells
#############################

rm(list=ls())

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# List of files
filez <- dir("Output/Output_ESOCMexicoHomicide/Events")
filez <- filez[grep("Events",filez)]
cntz <- gsub("ESOCMexicoHomicide_Events_|.RData","",filez)
#cntz <- cntz[cntz!="000"]
cntz

# Exceptions
source("Code/step2_eventcode/step2x_eventcode_admex.R")

k <- 1

# Load events
load("Output/Output_ESOCMexicoHomicide/Events/ESOCMexicoHomicide_Events_MEX.RData")
names(events) <- toupper(names(events))
events <- events[is.finite(as.numeric(as.character(events$LONG)))&is.finite(as.numeric(as.character(events$LAT))),]
coords <- events[,c("LONG","LAT")]
for(m in 1:2){coords[,m] <- as.numeric(as.character(coords[,m]))}

# Dates
datez <- seq(as.Date("1900-01-01"), as.Date("2016-10-24"), by="days")
ticker <- data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
ticker <- ticker[as.character(ticker$YEAR) > 1997 & as.character(ticker$YEAR) < 2012,]
ticker
tail(ticker)

# Load Prio Grid
#map0 <- readShapePoly("Input/GIS/Borders/PRIOGRID/priogrid_cell")
#save(map0,file="Input/GIS/Borders/PRIOGRID/PRIOGRID.RData")
load("Input/GIS/Borders/PRIOGRID/PRIOGRID.RData")

# Load GADM crop layer
if(!paste0(cntz[k],"_adm",1,".rds",sep="")%in%dir("Input/GIS/Borders/GADM/")){
  download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",1,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",1,".rds",sep=""))}
gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",1,".rds",sep=""))
proj4string(map0) <- proj4string(gadm)

# Crop by extent of map
coords0 <- as.data.frame(map0)[,c("xcoord","ycoord")]
sp.data0 <- SpatialPoints(coords0,proj4string=CRS(proj4string(gadm)))
sp.box <- as(extent(bbox(gadm)),"SpatialPolygons")
proj4string(sp.box) <- proj4string(gadm)
o <- over(map0, as(sp.box, "SpatialPolygons"))
map.crop <- map0[which(!is.na(o)),]
o <- over(map.crop, as(gadm, "SpatialPolygons"))
map.crop <- map.crop[which(!is.na(o)),]
names(map.crop) <- paste0("PRIO_",toupper(names(map.crop)))

# Add GADM names
gadm2 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",ifelse(cntz[k]%in%noadm2,ifelse(cntz[k]%in%noadm1,0,1),2),".rds",sep=""))
o <- over(map.crop,gadm2)
row.names(o) <- row.names(map.crop)
map.crop <- spCbind(map.crop,o)

# Overlay events
sp.data <- SpatialPoints(coords,proj4string=CRS(proj4string(gadm)))
# par(mar=c(0,0,0,0));plot(map.crop);points(sp.data,col="red")
o <- over(sp.data, as(map.crop, "SpatialPolygons"))
o <- data.frame(KEY=o)
subdata <- cbind(events,o)
grid.data <- as.data.frame(map.crop)
grid.data$KEY <- 1:nrow(grid.data)
tail(grid.data)

## Yearly event counts
disag <- sort(unique(grid.data$KEY))
head(subdata)
agg.sub <- lapply(1:length(disag),function(i){
  sub <- subdata[subdata$KEY%in%disag[i],]
  sub <- merge(ticker,sub[,],by.x="YEAR",by.y="DATE")
  sub$YSID <- sub$KEY*(10^nchar(max(as.character(ticker$YEAR))))+as.numeric(as.character(sub$YEAR))
  sub$YSID2 <- as.numeric(as.character(sub$YEAR))*(10^nchar(max(disag)))+sub$KEY
  vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
  if(nrow(sub)>0){
    sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(YSID=sub$YSID,YSID2=sub$YSID2,KEY=sub$KEY,YEAR=sub$YEAR),FUN=function(x){sum(x,na.rm=TRUE)})
  }
  if(nrow(sub)==0){
    sub <- sub[,c("YSID","YSID2","KEY","YEAR",vars)]
  }
  # Remaining years (0's)
  wids <- sort(unique(as.numeric(as.character(ticker$YEAR))))
  wids <- wids[!wids%in%sub$YEAR]
  if(length(wids)>0){
    sub. <- data.frame(YSID=disag[i]*(10^nchar(max(wids)))+wids,YSID2=wids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],YEAR=wids)
    sub.. <- as.data.frame(matrix(0,nrow=length(wids),ncol=length(vars)))
    names(sub..) <- vars
    sub <- rbind(sub,cbind(sub.,sub..))
  }
  # # Merge with months, years
  # tix <- aggregate(ticker[,c("YEAR")],by=list(YRMO=ticker$YRMO),FUN=function(x){x[1]})
  # sub <- merge(tix,sub,by="YRMO",all.x=F,all.y=T)
  # Merge with covariates
  sub <- merge(grid.data,sub,by="KEY",all.x=F,all.y=T)
  sub <- sub[order(sub$YSID),]
  sub
})
agg.data <- do.call(rbind,agg.sub)
agg.data <- agg.data[order(agg.data$YSID2),]
adm.year <- agg.data
head(adm.year)
save(adm.year,file=paste0("Output/Output_ESOCMexicoHomicide/ESOCMexicoHomicide_",cntz[k],"_priogrid_year.RData"))



#############################
## CLEA constituencies
#############################

rm(list=ls())

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","spatstat","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# List files
filez <- dir("Output/Output_ESOCMexicoHomicide/Events")
filez <- filez[grep("Events",filez)]
cntz <- gsub("ESOCMexicoHomicide_Events_|.RData","",filez)


# List CLEA
cleaz <- dir("Input/GIS/Borders/CLEA/")
cleaz <- countrycode(sourcevar = gsub("[0-9]","",cleaz),origin = "country.name",destination = "iso3c")
cleaz <- cleaz[!is.na(cleaz)]

# Overlap
cntz <- cleaz[cleaz%in%cntz]
k0 <- 1; cntz[k0]; k <- k0

# Exceptions
source("Code/step2_eventcode/step2x_eventcode_admex.R")

# Load events
load("Output/Output_ESOCMexicoHomicide/Events/ESOCMexicoHomicide_Events_MEX.RData")
names(events) <- toupper(names(events))
events <- events[is.finite(as.numeric(as.character(events$LONG)))&is.finite(as.numeric(as.character(events$LAT))),]
coords <- events[,c("LONG","LAT")]
for(m in 1:2){coords[,m] <- as.numeric(as.character(coords[,m]))}

# Dates
datez <- seq(as.Date("1900-01-01"), as.Date("2016-10-24"), by="days")
ticker <- data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
head(ticker)
ticker <- ticker[as.character(ticker$DATE)>=range(as.character(events$DATE),na.rm=T)[1]&as.character(ticker$DATE)<=range(as.character(events$DATE),na.rm=T)[2],]

# Load CLEA polygons
cleaz.dir <- dir("Input/GIS/Borders/CLEA/")
cleaz.dir <- cleaz.dir[countrycode(gsub("[0-9]","",cleaz.dir),"country.name","iso3c")%in%cntz[k]]
cleaz.file <- dir(paste0("Input/GIS/Borders/CLEA/",cleaz.dir))
map.crop <- readShapePoly(paste0("Input/GIS/Borders/CLEA/",cleaz.dir,"/",gsub(".shp","",cleaz.file[grep(".shp",cleaz.file)])))
names(map.crop) <- paste0("CLEA_",toupper(names(map.crop)))
names(map.crop)[grep("[0-9]",names(map.crop))] <- "CLEA_CST"

# Load GADM crop layer
#download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",0,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))
gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))
proj4string(map.crop) <- proj4string(gadm)

# Add GADM names
gadm2 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",ifelse(cntz[k]%in%noadm2,ifelse(cntz[k]%in%noadm1,0,1),2),".rds",sep=""))
o <- over(map.crop,gadm2)
row.names(o) <- row.names(map.crop)
map.crop <- spCbind(map.crop,o)
head(map.crop)

# Overlay events
sp.data <- SpatialPoints(coords,proj4string=CRS(proj4string(gadm)))
# par(mar=c(0,0,0,0));plot(map.crop);points(sp.data,col="red")
o <- over(sp.data, as(map.crop, "SpatialPolygons"))
o <- data.frame(KEY=o)
subdata <- cbind(events,o)
grid.data <- as.data.frame(map.crop)
grid.data$KEY <- 1:nrow(grid.data)
head(grid.data)

## Yearly event counts
disag <- sort(unique(grid.data$KEY))
i <- 1
agg.sub <- lapply(1:length(disag),function(i){
  sub <- subdata[subdata$KEY%in%disag[i],]
  sub <- merge(ticker,sub[,],by.x="YEAR",by.y="DATE")
  sub$YSID <- sub$KEY*(10^nchar(max(as.character(ticker$YEAR))))+as.numeric(as.character(sub$YEAR))
  sub$YSID2 <- as.numeric(as.character(sub$YEAR))*(10^nchar(max(disag)))+sub$KEY
  vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
  sub
  if(nrow(sub)>0){
    sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(YSID=sub$YSID,YSID2=sub$YSID2,KEY=sub$KEY,YEAR=sub$YEAR),FUN=sum)
  }
  if(nrow(sub)==0){
    sub <- sub[,c("YSID","YSID2","KEY","YEAR",vars)]
  }
  # Remaining years (0's)
  wids <- sort(unique(as.numeric(as.character(ticker$YEAR))))
  wids <- wids[!wids%in%sub$YEAR]
  if(length(wids)>0){
    sub. <- data.frame(YSID=disag[i]*(10^nchar(max(wids)))+wids,YSID2=wids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],YEAR=wids)
    sub.. <- as.data.frame(matrix(0,nrow=length(wids),ncol=length(vars)))
    names(sub..) <- vars
    sub <- rbind(sub,cbind(sub.,sub..))
  }
  # # Merge with months, years
  # tix <- aggregate(ticker[,c("YEAR")],by=list(YRMO=ticker$YRMO),FUN=function(x){x[1]})
  # sub <- merge(tix,sub,by="YRMO",all.x=F,all.y=T)
  # Merge with covariates
  sub <- merge(grid.data,sub,by="KEY",all.x=F,all.y=T)
  sub <- sub[order(sub$YSID),]
  sub
})
agg.data <- do.call(rbind,agg.sub)
agg.data <- agg.data[order(agg.data$YSID2),]
adm.year <- agg.data
summary(adm.year)
save(adm.year,file=paste0("Output/Output_ESOCMexicoHomicide/ESOCMexicoHomicide_",cntz[k],"_clea_year.RData"))


