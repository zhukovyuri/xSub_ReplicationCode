rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)



#############################
## Aggregate: GADM
#############################

rm(list=ls())

# List of files
filez <- dir("Output/Output_yzChechnya/Events")
filez <- filez[grep("Events",filez)]
cntz <- gsub("yzChechnya_Events_|.RData","",filez)
cntz <- cntz[cntz!="000"]
cntz

# Exceptions
source("Code/step2_eventcode/step2x_eventcode_admex.R")

k <- 1

# Load events
load(paste0("Output/Output_yzChechnya/Events/yzChechnya_Events_",cntz[k],".RData",sep=""))
names(events) <- toupper(names(events))
events <- events[is.finite(as.numeric(as.character(events$LONG)))&is.finite(as.numeric(as.character(events$LAT))),]
coords <- events[,c("LONG","LAT")]
for(m in 1:2){coords[,m] <- as.numeric(as.character(coords[,m]))}

# Dates
datez <- seq(as.Date("1900-01-01"), as.Date("2016-10-24"), by="days")
ticker <-data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
ticker <- ticker[as.character(ticker$DATE)>=range(as.character(events$DATE),na.rm=TRUE)[1]&as.character(ticker$DATE)<=range(as.character(events$DATE),na.rm=TRUE)[2],]
head(ticker)

## Open loop (adm)
a <- 0
for(a in 0:2){print(a)
  admz <- a
  #load(url(paste("http://biogeo.ucdavis.edu/data/gadm2/R/",cntz[k],"_adm",admz,".rds",sep="")))
  
  # Start adm2 exception
  if(!((a==1)&(cntz[k]%in%noadm1))){
    if(!((a==2)&(cntz[k]%in%noadm2))){
      
      if(!paste0(cntz[k],"_adm",admz,".rds",sep="")%in%dir("Input/GIS/Borders/GADM/")){
        download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",admz,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",admz,".rds",sep=""))}
      gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",admz,".rds",sep=""))
      
      # Subset
      if(a!=0){
        reg.cauc <- c("Adygey","Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","Krasnodar","North Ossetia","Stavropol'")
        reg.skfo <- c("Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","North Ossetia","Stavropol'")
        reg.ufo <- c("Adygey","Astrakhan'","Kalmyk","Krasnodar","Rostov","Volgograd")
        gadm <- gadm[gadm$NAME_1%in%c(reg.cauc),]
        # par(mar=rep(0,4)); plot(gadm); points(events[,c("LONG","LAT")],cex=.8,col="red")
      }
      if(a==0){
        gadm1 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",1,".rds",sep=""))
        reg.cauc <- c("Adygey","Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","Krasnodar","North Ossetia","Stavropol'")
        reg.skfo <- c("Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","North Ossetia","Stavropol'")
        reg.ufo <- c("Adygey","Astrakhan'","Kalmyk","Krasnodar","Rostov","Volgograd")
        gadm1 <- gadm1[gadm1$NAME_1%in%c(reg.cauc),]
        gadm <- crop(gadm,gadm1)
      }     
      
      # Overlay
      sp.data <- SpatialPoints(coords,proj4string=CRS(proj4string(gadm)))
      o <- over(sp.data, as(gadm, "SpatialPolygons"))
      o <- data.frame(KEY=o)
      subdata <- cbind(events,o)
      gadm.data <- as.data.frame(gadm)
      gadm.data$KEY <- 1:nrow(gadm.data)
      head(gadm.data)
      
      ## Yearly event counts
      disag <- sort(unique(gadm.data$KEY))
      i <- 1
      agg.sub <- lapply(1:length(disag),function(i){
        sub <- subdata[subdata$KEY%in%disag[i],]
        sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
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
        sub <- merge(gadm.data,sub,by="KEY",all.x=F,all.y=T)
        sub <- sub[order(sub$YSID),]
        sub
      })
      agg.data <- do.call(rbind,agg.sub)
      agg.data <- agg.data[order(agg.data$YSID2),]
      adm.year <- agg.data
      tail(adm.year)
      save(adm.year,file=paste0("Output/Output_yzChechnya/yzChechnya_",cntz[k],"_adm",admz,"_year.RData"))
      
      ## Monthly event counts
      disag <- sort(unique(gadm.data$KEY))
      i <- 2
      agg.sub <- lapply(1:length(disag),function(i){
        sub <- subdata[subdata$KEY%in%disag[i],]
        sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
        sub$MSID <- sub$KEY*(10^nchar(max(as.character(ticker$YRMO))))+as.numeric(as.character(sub$YRMO))
        sub$MSID2 <- as.numeric(as.character(sub$YRMO))*(10^nchar(max(disag)))+sub$KEY
        vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
        sub
        if(nrow(sub)>0){
          sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(MSID=sub$MSID,MSID2=sub$MSID2,KEY=sub$KEY,YRMO=sub$YRMO),FUN=sum)
        }
        if(nrow(sub)==0){
          vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
          sub <- sub[,c("MSID","MSID2","KEY","YRMO",vars)]
        }
        # Remaining weeks (0's)
        wids <- sort(unique(as.numeric(as.character(ticker$YRMO))))
        wids <- wids[!wids%in%sub$YRMO]
        if(length(wids)>0){
          sub. <- data.frame(MSID=disag[i]*(10^nchar(max(wids)))+wids,MSID2=wids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],YRMO=wids)
          sub.. <- as.data.frame(matrix(0,nrow=length(wids),ncol=length(vars)))
          names(sub..) <- vars
          sub <- rbind(sub,cbind(sub.,sub..))
        }
        # Merge with months, years
        tix <- aggregate(ticker[,c("YEAR")],by=list(YRMO=ticker$YRMO),FUN=function(x){x[1]})
        sub <- merge(tix,sub,by="YRMO",all.x=F,all.y=T)
        # Merge with covariates
        sub <- merge(gadm.data,sub,by="KEY",all.x=F,all.y=T)
        sub <- sub[order(sub$MSID),]
        sub
      })
      agg.data <- do.call(rbind,agg.sub)
      agg.data <- agg.data[order(agg.data$MSID2),]
      adm.month <- agg.data
      save(adm.month,file=paste0("Output/Output_yzChechnya/yzChechnya_",cntz[k],"_adm",admz,"_month.RData"))
      
      
      ## Weekly event counts
      disag <- sort(unique(gadm.data$KEY))
      i <- 2
      agg.sub <- lapply(1:length(disag),function(i){
        sub <- subdata[subdata$KEY%in%disag[i],]
        sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
        sub$WSID <- sub$KEY*(10^nchar(max(ticker$WID)))+sub$WID
        sub$WSID2 <- sub$WID*(10^nchar(max(disag)))+sub$KEY
        vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
        sub
        if(nrow(sub)>0){
          sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(WSID=sub$WSID,WSID2=sub$WSID2,KEY=sub$KEY,WID=sub$WID),FUN=sum)
        }
        if(nrow(sub)==0){
          vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
          sub <- sub[,c("WSID","WSID2","KEY","WID",vars)]
        }
        # Remaining weeks (0's)
        wids <- sort(unique(ticker$WID))
        wids <- wids[!wids%in%sub$WID]
        if(length(wids)>0){
          sub. <- data.frame(WSID=disag[i]*(10^nchar(max(ticker$WID)))+wids,WSID2=wids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],WID=wids)
          sub.. <- as.data.frame(matrix(0,nrow=length(wids),ncol=length(vars)))
          names(sub..) <- vars
          sub <- rbind(sub,cbind(sub.,sub..))
        }
        # Merge with months, years
        tix <- aggregate(ticker[,c("YEAR","YRMO")],by=list(WID=ticker$WID),FUN=function(x){x[1]})
        sub <- merge(tix,sub,by="WID",all.x=F,all.y=T)
        # Merge with covariates
        sub <- merge(gadm.data,sub,by="KEY",all.x=F,all.y=T)
        sub <- sub[order(sub$WSID),]
        sub
      })
      agg.data <- do.call(rbind,agg.sub)
      agg.data <- agg.data[order(agg.data$WSID2),]
      adm.week <- agg.data
      save(adm.week,file=paste0("Output/Output_yzChechnya/yzChechnya_",cntz[k],"_adm",admz,"_week.RData"))
      
      ## Daily event counts
      if(admz!=2){
        disag <- sort(unique(gadm.data$KEY))
        i <- 2
        agg.sub <- lapply(1:length(disag),function(i){
          sub <- subdata[subdata$KEY%in%disag[i],]
          sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
          sub$TSID <- sub$KEY*(10^nchar(max(ticker$TID)))+sub$TID
          sub$TSID2 <- sub$TID*(10^nchar(max(disag)))+sub$KEY
          vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
          sub
          if(nrow(sub)>0){
            sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(TSID=sub$TSID,TSID2=sub$TSID2,KEY=sub$KEY,TID=sub$TID),FUN=sum)
          }
          if(nrow(sub)==0){
            vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
            sub <- sub[,c("TSID","TSID2","KEY","TID",vars)]
          }
          # Remaining days (0's)
          tids <- sort(unique(ticker$TID))
          tids <- tids[!tids%in%sub$TID]
          if(length(tids)>0){
            sub. <- data.frame(TSID=disag[i]*(10^nchar(max(ticker$TID)))+tids,TSID2=tids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],TID=tids)
            sub.. <- as.data.frame(matrix(0,nrow=length(tids),ncol=length(vars)))
            names(sub..) <- vars
            sub <- rbind(sub,cbind(sub.,sub..))
          }
          # Merge with months, years
          tix <- aggregate(ticker[,c("YEAR","YRMO","WID","DATE")],by=list(TID=ticker$TID),FUN=function(x){x[1]})
          sub <- merge(tix,sub,by="TID",all.x=F,all.y=T)
          head(sub)
          # Merge with covariates
          sub <- merge(gadm.data,sub,by="KEY",all.x=F,all.y=T)
          sub <- sub[order(sub$TSID),]
          sub
        })
        agg.data <- do.call(rbind,agg.sub)
        agg.data <- agg.data[order(agg.data$TSID2),]
        adm.day <- agg.data
        save(adm.day,file=paste0("Output/Output_yzChechnya/yzChechnya_",cntz[k],"_adm",admz,"_day.RData"))
      }
      # End adm2 exception
    }
  }
  # Close loop (adm)
}



#############################
## Grid cells
#############################

rm(list=ls())

# List of files
filez <- dir("Output/Output_yzChechnya/Events")
filez <- filez[grep("Events",filez)]
cntz <- gsub("yzChechnya_Events_|.RData","",filez)
cntz <- cntz[cntz!="000"]
cntz

# Exceptions
source("Code/step2_eventcode/step2x_eventcode_admex.R")

k <- 1

# Load events
load(paste0("Output/Output_yzChechnya/Events/yzChechnya_Events_",cntz[k],".RData",sep=""))
names(events) <- toupper(names(events))
events <- events[is.finite(as.numeric(as.character(events$LONG)))&is.finite(as.numeric(as.character(events$LAT))),]
coords <- events[,c("LONG","LAT")]
for(m in 1:2){coords[,m] <- as.numeric(as.character(coords[,m]))}

# Dates
datez <- seq(as.Date("1900-01-01"), as.Date("2016-10-24"), by="days")
ticker <- data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
ticker <- ticker[as.character(ticker$DATE)>=range(as.character(events$DATE),na.rm=TRUE)[1]&as.character(ticker$DATE)<=range(as.character(events$DATE),na.rm=TRUE)[2],]
ticker

# Load Prio Grid
#map0 <- readShapePoly("Input/GIS/Borders/PRIOGRID/priogrid_cell")
#save(map0,file="Input/GIS/Borders/PRIOGRID/PRIOGRID.RData")
load("Input/GIS/Borders/PRIOGRID/PRIOGRID.RData")

# Load GADM crop layer
if(!paste0(cntz[k],"_adm",1,".rds",sep="")%in%dir("Input/GIS/Borders/GADM/")){
  download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",1,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",1,".rds",sep=""))}
gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",1,".rds",sep=""))
proj4string(map0) <- proj4string(gadm)

# Subset
reg.cauc <- c("Adygey","Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","Krasnodar","North Ossetia","Stavropol'")
reg.skfo <- c("Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","North Ossetia","Stavropol'")
reg.ufo <- c("Adygey","Astrakhan'","Kalmyk","Krasnodar","Rostov","Volgograd")
gadm <- gadm[gadm$NAME_1%in%c(reg.cauc),]
# par(mar=rep(0,4)); plot(gadm); points(events[,c("LONG","LAT")],cex=.8,col="red")

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
  sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
  sub$YSID <- sub$KEY*(10^nchar(max(as.character(ticker$YEAR))))+as.numeric(as.character(sub$YEAR))
  sub$YSID2 <- as.numeric(as.character(sub$YEAR))*(10^nchar(max(disag)))+sub$KEY
  vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
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
save(adm.year,file=paste0("Output/Output_yzChechnya/yzChechnya_",cntz[k],"_priogrid_year.RData"))

## Monthly event counts
disag <- sort(unique(grid.data$KEY))
i <- 2
agg.sub <- lapply(1:length(disag),function(i){
  sub <- subdata[subdata$KEY%in%disag[i],]
  sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
  sub$MSID <- sub$KEY*(10^nchar(max(as.character(ticker$YRMO))))+as.numeric(as.character(sub$YRMO))
  sub$MSID2 <- as.numeric(as.character(sub$YRMO))*(10^nchar(max(disag)))+sub$KEY
  vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
  sub
  if(nrow(sub)>0){
    sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(MSID=sub$MSID,MSID2=sub$MSID2,KEY=sub$KEY,YRMO=sub$YRMO),FUN=sum)
  }
  if(nrow(sub)==0){
    vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
    sub <- sub[,c("MSID","MSID2","KEY","YRMO",vars)]
  }
  # Remaining weeks (0's)
  wids <- sort(unique(as.numeric(as.character(ticker$YRMO))))
  wids <- wids[!wids%in%sub$YRMO]
  if(length(wids)>0){
    sub. <- data.frame(MSID=disag[i]*(10^nchar(max(wids)))+wids,MSID2=wids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],YRMO=wids)
    sub.. <- as.data.frame(matrix(0,nrow=length(wids),ncol=length(vars)))
    names(sub..) <- vars
    sub <- rbind(sub,cbind(sub.,sub..))
  }
  # Merge with months, years
  tix <- aggregate(ticker[,c("YEAR")],by=list(YRMO=ticker$YRMO),FUN=function(x){x[1]})
  sub <- merge(tix,sub,by="YRMO",all.x=F,all.y=T)
  # Merge with covariates
  sub <- merge(grid.data,sub,by="KEY",all.x=F,all.y=T)
  sub <- sub[order(sub$MSID),]
  sub
})
agg.data <- do.call(rbind,agg.sub)
agg.data <- agg.data[order(agg.data$MSID2),]
adm.month <- agg.data
save(adm.month,file=paste0("Output/Output_yzChechnya/yzChechnya_",cntz[k],"_priogrid_month.RData"))


## Weekly event counts
disag <- sort(unique(grid.data$KEY))
i <- 2
agg.sub <- lapply(1:length(disag),function(i){
  sub <- subdata[subdata$KEY%in%disag[i],]
  sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
  sub$WSID <- sub$KEY*(10^nchar(max(ticker$WID)))+sub$WID
  sub$WSID2 <- sub$WID*(10^nchar(max(disag)))+sub$KEY
  vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
  sub
  if(nrow(sub)>0){
    sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(WSID=sub$WSID,WSID2=sub$WSID2,KEY=sub$KEY,WID=sub$WID),FUN=sum)
  }
  if(nrow(sub)==0){
    vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
    sub <- sub[,c("WSID","WSID2","KEY","WID",vars)]
  }
  # Remaining weeks (0's)
  wids <- sort(unique(ticker$WID))
  wids <- wids[!wids%in%sub$WID]
  if(length(wids)>0){
    sub. <- data.frame(WSID=disag[i]*(10^nchar(max(ticker$WID)))+wids,WSID2=wids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],WID=wids)
    sub.. <- as.data.frame(matrix(0,nrow=length(wids),ncol=length(vars)))
    names(sub..) <- vars
    sub <- rbind(sub,cbind(sub.,sub..))
  }
  # Merge with months, years
  tix <- aggregate(ticker[,c("YEAR","YRMO")],by=list(WID=ticker$WID),FUN=function(x){x[1]})
  sub <- merge(tix,sub,by="WID",all.x=F,all.y=T)
  # Merge with covariates
  sub <- merge(grid.data,sub,by="KEY",all.x=F,all.y=T)
  sub <- sub[order(sub$WSID),]
  sub
})
agg.data <- do.call(rbind,agg.sub)
agg.data <- agg.data[order(agg.data$WSID2),]
adm.week <- agg.data
save(adm.week,file=paste0("Output/Output_yzChechnya/yzChechnya_",cntz[k],"_priogrid_week.RData"))

## Daily event counts
disag <- sort(unique(grid.data$KEY))
i <- 2
agg.sub <- lapply(1:length(disag),function(i){
  sub <- subdata[subdata$KEY%in%disag[i],]
  sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
  sub$TSID <- sub$KEY*(10^nchar(max(ticker$TID)))+sub$TID
  sub$TSID2 <- sub$TID*(10^nchar(max(disag)))+sub$KEY
  vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
  sub
  if(nrow(sub)>0){
    sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(TSID=sub$TSID,TSID2=sub$TSID2,KEY=sub$KEY,TID=sub$TID),FUN=sum)
  }
  if(nrow(sub)==0){
    vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
    sub <- sub[,c("TSID","TSID2","KEY","TID",vars)]
  }
  # Remaining days (0's)
  tids <- sort(unique(ticker$TID))
  tids <- tids[!tids%in%sub$TID]
  if(length(tids)>0){
    sub. <- data.frame(TSID=disag[i]*(10^nchar(max(ticker$TID)))+tids,TSID2=tids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],TID=tids)
    sub.. <- as.data.frame(matrix(0,nrow=length(tids),ncol=length(vars)))
    names(sub..) <- vars
    sub <- rbind(sub,cbind(sub.,sub..))
  }
  # Merge with months, years
  tix <- aggregate(ticker[,c("YEAR","YRMO","WID","DATE")],by=list(TID=ticker$TID),FUN=function(x){x[1]})
  sub <- merge(tix,sub,by="TID",all.x=F,all.y=T)
  head(sub)
  # Merge with covariates
  sub <- merge(grid.data,sub,by="KEY",all.x=F,all.y=T)
  sub <- sub[order(sub$TSID),]
  sub
})
agg.data <- do.call(rbind,agg.sub)
agg.data <- agg.data[order(agg.data$TSID2),]
adm.day <- agg.data
save(adm.day,file=paste0("Output/Output_yzChechnya/yzChechnya_",cntz[k],"_priogrid_day.RData"))

