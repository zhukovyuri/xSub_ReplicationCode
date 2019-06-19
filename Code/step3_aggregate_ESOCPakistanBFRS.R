rm(list=ls())


## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
if(grepl("w64",sessionInfo()[[1]][[1]])){setwd("D:/Dropbox (Zhukov research team)/XSub/Data/")}

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Date
# mdatez <- c("
#   mdate <- Sys.Date()
#   ")
# mdatez <- c("
#   mdate <- as.Date(\"2018-03-18\")
#   ")
# write.table(mdatez,file="Code/step3_aggregate/step3x_mdatez.R",quote = FALSE,col.names = FALSE,row.names = FALSE)
# rm(mdatez)



#############################
## Aggregate: GADM
#############################

rm(list=ls())

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# NEW: Area conversion functions
sqdeg2sqkm <- function(x){12379.77*x}
sqkm2sqdeg <- function(x){x/12379.77}

# List of files
filez <- dir("Output/Output_ESOCPakistanBFRS/Events")
filez <- filez[grep("Events",filez)]
cntz <- gsub("ESOCPakistanBFRS_Events_|.RData","",filez)
#cntz <- cntz[cntz!="000"]
cntz

# Exceptions
source("Code/step3_aggregate/step3x_aggregate_admex.R")

# NEW: Missing and older files
source("Code/step3_aggregate/step3x_mdatez.R")
filez.all <- paste0("ESOCPakistanBFRS_",rep(cntz,each=4*3),"_adm",rep(0:2,each=4),"_",rep(c("year","month","week","day"),3),".RData")
filez.all <- filez.all[!grepl(paste0(c(paste0(noadm1,"_adm1"),paste0(noadm2,"_adm2")),collapse="|"),filez.all)]
filez.old <- filez.all[!filez.all%in%dir("Output/Output_ESOCPakistanBFRS/")]
filez.older <- dir("Output/Output_ESOCPakistanBFRS")[substr(file.mtime(paste0("Output/Output_ESOCPakistanBFRS/",dir("Output/Output_ESOCPakistanBFRS"))),1,10)<mdate]
filez.older <- intersect(filez.all,filez.older)
filez.old <- union(filez.old,filez.older)
filez.old <- filez.old[!grepl("adm2_day",filez.old)]
cntz <- cntz[cntz%in%sapply(strsplit(filez.old,"_"),"[",2)]
cntz

# Loop over countries in list
if(length(cntz)>0){

k <- 1

# Load events
load(paste0("Output/Output_ESOCPakistanBFRS/Events/ESOCPakistanBFRS_Events_",cntz[k],".RData",sep=""))
names(events) <- toupper(names(events))
events <- events[is.finite(as.numeric(as.character(events$LONG)))&is.finite(as.numeric(as.character(events$LAT))),]
coords <- events[,c("LONG","LAT")]
for(m in 1:2){coords[,m] <- as.numeric(as.character(coords[,m]))}

# NEW: Make copies
events00 <- events
coords00 <- coords

# Dates
datez <- seq(as.Date("1900-01-01"), as.Date(Sys.Date()), by="days")
ticker <-data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
ticker <- ticker[as.character(ticker$DATE)>=range(as.character(events$DATE),na.rm=TRUE)[1]&as.character(ticker$DATE)<=range(as.character(events$DATE),na.rm=TRUE)[2],]
head(ticker)


## Open loop (adm)
a <- 1
for(a in 0:2){print(a)
  admz <- a
  #load(url(paste("http://biogeo.ucdavis.edu/data/gadm2/R/",cntz[k],"_adm",admz,".rds",sep="")))
  
  # NEW: Filter events by geographic precision code
  if(admz==0){
    events <- events00[events00$GEOPRECISION%in%c("settlement","adm2","adm1","adm0"),]
    coords <- coords00[events00$GEOPRECISION%in%c("settlement","adm2","adm1","adm0"),]
  }
  if(admz==1){
    events <- events00[events00$GEOPRECISION%in%c("settlement","adm2","adm1"),]
    coords <- coords00[events00$GEOPRECISION%in%c("settlement","adm2","adm1"),]
  }
  if(admz==2){
    events <- events00[events00$GEOPRECISION%in%c("settlement","adm2"),]
    coords <- coords00[events00$GEOPRECISION%in%c("settlement","adm2"),]
  }


  # Start adm2 exception
  if(!((a==1)&(cntz[k]%in%noadm1))){
    if(!((a==2)&(cntz[k]%in%noadm2))){
      
      if(!paste0(cntz[k],"_adm",admz,".rds",sep="")%in%dir("Input/GIS/Borders/GADM/")){
        download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",admz,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",admz,".rds",sep=""))}
      gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",admz,".rds",sep=""))
      
      # Overlay
      sp.data <- SpatialPoints(coords,proj4string=CRS(proj4string(gadm)))
      # plot(gadm); points(sp.data)
      o <- over(sp.data, as(gadm, "SpatialPolygons"))
      o <- data.frame(KEY=o)
      subdata <- cbind(events,o)
      gadm.data <- as.data.frame(gadm)
      gadm.data$KEY <- 1:nrow(gadm.data)
      head(gadm.data)
      
      ## Yearly event counts
      disag <- sort(unique(gadm.data$KEY))
      i <- 1
      if(paste0("ESOCPakistanBFRS_",cntz[k],"_adm",admz,"_year.RData")%in%filez.old){
      agg.sub <- lapply(1:length(disag),function(i){
        sub <- subdata[subdata$KEY%in%disag[i],]
        sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
        sub$YSID <- sub$KEY*(10^nchar(max(as.character(ticker$YEAR))))+as.numeric(as.character(sub$YEAR))
        sub$YSID2 <- as.numeric(as.character(sub$YEAR))*(10^nchar(max(disag)))+sub$KEY
        vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))]
        # NEW: Subset by precision
        sub <- sub[sub$TIMEPRECISION%in%c("day","week","month","year"),]
        if(nrow(sub)>0){
          sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))],by=list(YSID=sub$YSID,YSID2=sub$YSID2,KEY=sub$KEY,YEAR=sub$YEAR),FUN=function(x){sum(x,na.rm=TRUE)})
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
      save(adm.year,file=paste0("Output/Output_ESOCPakistanBFRS/ESOCPakistanBFRS_",cntz[k],"_adm",admz,"_year.RData"))
      }
      
      ## Monthly event counts
      disag <- sort(unique(gadm.data$KEY))
      i <- 2
      if(paste0("ESOCPakistanBFRS_",cntz[k],"_adm",admz,"_month.RData")%in%filez.old){
      agg.sub <- lapply(1:length(disag),function(i){
        sub <- subdata[subdata$KEY%in%disag[i],]
        sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
        sub$MSID <- sub$KEY*(10^nchar(max(as.character(ticker$YRMO))))+as.numeric(as.character(sub$YRMO))
        sub$MSID2 <- as.numeric(as.character(sub$YRMO))*(10^nchar(max(disag)))+sub$KEY
        vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))]
        # NEW: Subset by precision
        sub <- sub[sub$TIMEPRECISION%in%c("day","week","month"),]
        if(nrow(sub)>0){
          sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))],by=list(MSID=sub$MSID,MSID2=sub$MSID2,KEY=sub$KEY,YRMO=sub$YRMO),FUN=function(x){sum(x,na.rm=TRUE)})
        }
        if(nrow(sub)==0){
          vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))]
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
      save(adm.month,file=paste0("Output/Output_ESOCPakistanBFRS/ESOCPakistanBFRS_",cntz[k],"_adm",admz,"_month.RData"))
      }
      
      ## Weekly event counts
      disag <- sort(unique(gadm.data$KEY))
      i <- 2
      if(paste0("ESOCPakistanBFRS_",cntz[k],"_adm",admz,"_week.RData")%in%filez.old){
      agg.sub <- lapply(1:length(disag),function(i){
        sub <- subdata[subdata$KEY%in%disag[i],]
        sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
        sub$WSID <- sub$KEY*(10^nchar(max(ticker$WID)))+sub$WID
        sub$WSID2 <- sub$WID*(10^nchar(max(disag)))+sub$KEY
        vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))]
        # NEW: Subset by precision
        sub <- sub[sub$TIMEPRECISION%in%c("day","week"),]
        if(nrow(sub)>0){
          sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))],by=list(WSID=sub$WSID,WSID2=sub$WSID2,KEY=sub$KEY,WID=sub$WID),FUN=function(x){sum(x,na.rm=TRUE)})
        }
        if(nrow(sub)==0){
          vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))]
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
      save(adm.week,file=paste0("Output/Output_ESOCPakistanBFRS/ESOCPakistanBFRS_",cntz[k],"_adm",admz,"_week.RData"))
      }
      
      ## Daily event counts
      if(!admz%in%c(2)){
        disag <- sort(unique(gadm.data$KEY))
        i <- 2
        if(paste0("ESOCPakistanBFRS_",cntz[k],"_adm",admz,"_day.RData")%in%filez.old){
        agg.sub <- lapply(1:length(disag),function(i){
          sub <- subdata[subdata$KEY%in%disag[i],]
          sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
          sub$TSID <- sub$KEY*(10^nchar(max(ticker$TID)))+sub$TID
          sub$TSID2 <- sub$TID*(10^nchar(max(disag)))+sub$KEY
          vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))]
          # NEW: Subset by precision
          sub <- sub[sub$TIMEPRECISION%in%c("day"),]
          if(nrow(sub)>0){
            sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))],by=list(TSID=sub$TSID,TSID2=sub$TSID2,KEY=sub$KEY,TID=sub$TID),FUN=function(x){sum(x,na.rm=TRUE)})
          }
          if(nrow(sub)==0){
            vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))]
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
        save(adm.day,file=paste0("Output/Output_ESOCPakistanBFRS/ESOCPakistanBFRS_",cntz[k],"_adm",admz,"_day.RData"))
        }
        }
      # End adm2 exception
    }
  }
  # Close loop (adm)
}

# Close if statement
}


#############################
## Grid cells
#############################

rm(list=ls())

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# NEW: Area conversion functions
sqdeg2sqkm <- function(x){12379.77*x}
sqkm2sqdeg <- function(x){x/12379.77}

# List of files
filez <- dir("Output/Output_ESOCPakistanBFRS/Events")
filez <- filez[grep("Events",filez)]
cntz <- gsub("ESOCPakistanBFRS_Events_|.RData","",filez)
#cntz <- cntz[cntz!="000"]
cntz

# Exceptions
source("Code/step3_aggregate/step3x_aggregate_admex.R")

# NEW: Missing and older files
source("Code/step3_aggregate/step3x_mdatez.R")
filez.all <- paste0("ESOCPakistanBFRS_",rep(cntz,each=4),"_priogrid_",c("year","month","week","day"),".RData")
filez.old <- filez.all[!filez.all%in%dir("Output/Output_ESOCPakistanBFRS/")]
filez.older <- dir("Output/Output_ESOCPakistanBFRS")[substr(file.mtime(paste0("Output/Output_ESOCPakistanBFRS/",dir("Output/Output_ESOCPakistanBFRS"))),1,10)<mdate]
filez.older <- intersect(filez.all,filez.older)
filez.old <- union(filez.old,filez.older)
filez.old <- filez.old[!grepl("priogrid_day",filez.old)]
cntz <- cntz[cntz%in%sapply(strsplit(filez.old,"_"),"[",2)]
cntz

# Loop over countries in list
if(length(cntz)>0){

k <- 1

# Load events
load("Output/Output_ESOCPakistanBFRS/Events/ESOCPakistanBFRS_Events_PAK.RData")
names(events) <- toupper(names(events))
events <- events[is.finite(as.numeric(as.character(events$LONG)))&is.finite(as.numeric(as.character(events$LAT))),]
coords <- events[,c("LONG","LAT")]
for(m in 1:2){coords[,m] <- as.numeric(as.character(coords[,m]))}

# NEW: Make copies
events00 <- events
coords00 <- coords

# Dates
datez <- seq(as.Date("1900-01-01"), as.Date(Sys.Date()), by="days")
ticker <- data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
ticker <- ticker[as.character(ticker$DATE)>=range(as.character(events$DATE),na.rm=TRUE)[1]&as.character(ticker$DATE)<=range(as.character(events$DATE),na.rm=TRUE)[2],]
ticker

# NEW: Filter events by geographic precision code
events <- events00[events00$GEOPRECISION%in%c("settlement","adm2"),]
coords <- coords00[events00$GEOPRECISION%in%c("settlement","adm2"),]

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

# NEW: Add GADM names
gadm2 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",ifelse(cntz[k]%in%noadm2,ifelse(cntz[k]%in%noadm1,0,1),2),".rds",sep=""))
gvarz <- names(gadm2)
o0 <- gIntersection(map.crop,gadm2,byid = TRUE,drop_lower_td = TRUE)
o0_data <- data.frame(JOINT_ID=row.names(o0),ID1=sapply(strsplit(row.names(o0)," "),"[",1),ID2=sapply(strsplit(row.names(o0)," "),"[",2))
row.names(o0_data) <- row.names(o0)
o0 <- SpatialPolygonsDataFrame(o0,data=o0_data)
o0$AREA <- sqdeg2sqkm(gArea(o0,byid = TRUE))
o0 <- o0[order(o0$ID1,o0$AREA),]
o0 <- o0[!duplicated(o0$ID1,fromLast = TRUE),]
map.crop$ID1 <- row.names(map.crop)
gadm2$ID2 <- row.names(gadm2)
o0 <- merge(o0,gadm2,by="ID2",all.x=T,all.y=F)
map.crop <- merge(map.crop,o0[,c("ID1",gvarz)],by="ID1",all.x=T,all.y=F)
map.crop$ID1 <- NULL
head(map.crop)

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
if(paste0("ESOCPakistanBFRS_",cntz[k],"_priogrid_year.RData")%in%filez.old){
agg.sub <- lapply(1:length(disag),function(i){
  sub <- subdata[subdata$KEY%in%disag[i],]
  sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
  sub$YSID <- sub$KEY*(10^nchar(max(as.character(ticker$YEAR))))+as.numeric(as.character(sub$YEAR))
  sub$YSID2 <- as.numeric(as.character(sub$YEAR))*(10^nchar(max(disag)))+sub$KEY
  vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))]
  # NEW: Subset by precision
  sub <- sub[sub$TIMEPRECISION%in%c("day","week","month","year"),]
  if(nrow(sub)>0){
    sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))],by=list(YSID=sub$YSID,YSID2=sub$YSID2,KEY=sub$KEY,YEAR=sub$YEAR),FUN=function(x){sum(x,na.rm=TRUE)})
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
  sub <- merge(grid.data,sub,by="KEY",all.x=F,all.y=T)
  sub <- sub[order(sub$YSID),]
  sub
})
agg.data <- do.call(rbind,agg.sub)
agg.data <- agg.data[order(agg.data$YSID2),]
adm.year <- agg.data
# summary(adm.year)
save(adm.year,file=paste0("Output/Output_ESOCPakistanBFRS/ESOCPakistanBFRS_",cntz[k],"_priogrid_year.RData"))
}

## Monthly event counts
disag <- sort(unique(grid.data$KEY))
i <- 2
if(paste0("ESOCPakistanBFRS_",cntz[k],"_priogrid_month.RData")%in%filez.old){
agg.sub <- lapply(1:length(disag),function(i){
  sub <- subdata[subdata$KEY%in%disag[i],]
  sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
  sub$MSID <- sub$KEY*(10^nchar(max(as.character(ticker$YRMO))))+as.numeric(as.character(sub$YRMO))
  sub$MSID2 <- as.numeric(as.character(sub$YRMO))*(10^nchar(max(disag)))+sub$KEY
  vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))]
  # NEW: Subset by precision
  sub <- sub[sub$TIMEPRECISION%in%c("day","week","month"),]
  if(nrow(sub)>0){
    sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))],by=list(MSID=sub$MSID,MSID2=sub$MSID2,KEY=sub$KEY,YRMO=sub$YRMO),FUN=function(x){sum(x,na.rm=TRUE)})
  }
  if(nrow(sub)==0){
    vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))]
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
save(adm.month,file=paste0("Output/Output_ESOCPakistanBFRS/ESOCPakistanBFRS_",cntz[k],"_priogrid_month.RData"))
}

## Weekly event counts
disag <- sort(unique(grid.data$KEY))
i <- 2
if(paste0("ESOCPakistanBFRS_",cntz[k],"_priogrid_month.RData")%in%filez.old){
agg.sub <- lapply(1:length(disag),function(i){
  sub <- subdata[subdata$KEY%in%disag[i],]
  sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
  sub$WSID <- sub$KEY*(10^nchar(max(ticker$WID)))+sub$WID
  sub$WSID2 <- sub$WID*(10^nchar(max(disag)))+sub$KEY
  vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))]
  # NEW: Subset by precision
  sub <- sub[sub$TIMEPRECISION%in%c("day","week"),]
  if(nrow(sub)>0){
    sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))],by=list(WSID=sub$WSID,WSID2=sub$WSID2,KEY=sub$KEY,WID=sub$WID),FUN=function(x){sum(x,na.rm=TRUE)})
  }
  if(nrow(sub)==0){
    vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))]
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
save(adm.week,file=paste0("Output/Output_ESOCPakistanBFRS/ESOCPakistanBFRS_",cntz[k],"_priogrid_week.RData"))
}

## Daily event counts
disag <- sort(unique(grid.data$KEY))
i <- 2
if(paste0("ESOCPakistanBFRS_",cntz[k],"_priogrid_day.RData")%in%filez.old){
agg.sub <- lapply(1:length(disag),function(i){
  sub <- subdata[subdata$KEY%in%disag[i],]
  sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
  sub$TSID <- sub$KEY*(10^nchar(max(ticker$TID)))+sub$TID
  sub$TSID2 <- sub$TID*(10^nchar(max(disag)))+sub$KEY
  vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))]
  # NEW: Subset by precision
  sub <- sub[sub$TIMEPRECISION%in%c("day"),]
  if(nrow(sub)>0){
    sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))],by=list(TSID=sub$TSID,TSID2=sub$TSID2,KEY=sub$KEY,TID=sub$TID),FUN=function(x){sum(x,na.rm=TRUE)})
  }
  if(nrow(sub)==0){
    vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE|DYAD",names(sub))]
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
save(adm.day,file=paste0("Output/Output_ESOCPakistanBFRS/ESOCPakistanBFRS_",cntz[k],"_priogrid_day.RData"))
}


# Close if statement
}
