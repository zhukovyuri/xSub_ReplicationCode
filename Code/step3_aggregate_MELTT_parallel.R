rm(list=ls())


## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
if(grepl("w64",sessionInfo()[[1]][[1]])){setwd("H:/Dropbox (Zhukov research team)/XSub/Data/")}


## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","spatstat","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# Set parameters
mpar <- c("
mdate <- \"2018-02-02\"
firstrun <- TRUE
remaining_only <- TRUE
tw <- c(1,2)[1]
sw <- c(1,5)[1]
")
writeChar(mpar,con = "Code/step2_eventcode/step2x_melttpar.R")



# #############################
# ## CLEA constituencies
# #############################
# 
# rm(list=ls())
# 
# # Set parameters
# source("Code/step2_eventcode/step2x_melttpar.R")
# 
# # Parallel computing
# mcoptions <- list(preschedule=FALSE, set.seed=TRUE)
# ncores <- detectCores()
# # cl <- makeCluster(ncores)
# # registerDoParallel(cl)
# 
# ## Install & load packages (all at once)
# list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","spatstat","parallel","foreach","doParallel")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
# lapply(list.of.packages, require, character.only = TRUE)
# 
# # List files
# srzm <- paste0("MELTT",tw,sw)
# filez <- dir(paste0("Output/Output_",srzm,"/Events"))
# filez <- filez[grep("Events",filez)]
# cntz <- gsub(".RData","",sapply(strsplit(filez,"_"),"[",3))
# cntz <- cntz[cntz!="000"]
# 
# # List CLEA
# cleaz <- dir("Input/GIS/Borders/CLEA/Beta")
# cleaz <- cleaz[grep("GRED",cleaz)]
# cleaz <- sapply(strsplit(gsub("[0-9]|GRED_","",cleaz),"_"),"[",1)
# cleaz <- gsub("DominicanRepublic","Dominican Republic",cleaz)
# cleaz <- gsub("SaintLucia","Saint Lucia",cleaz)
# cleaz <- gsub("MexicoPR","Mexico",cleaz)
# cleaz <- gsub("SouthAfrica","South Africa",cleaz)
# cleaz <- countrycode(cleaz,"country.name","iso3c")
# cleaz <- cleaz[!is.na(cleaz)]
# cleaz <- sort(unique(cleaz))
# 
# # Overlap
# cntz <- cleaz[cleaz%in%cntz]
# k0 <- 7; cntz[k0]; k <- k0
# 
# # Fix errors (replace older files)
# filez.old <- dir(paste0("Output/Output_",srzm))[substr(file.mtime(paste0("Output/Output_",srzm,"/",dir(paste0("Output/Output_",srzm)))),1,10)<mdate]
# filez.old <- filez.old[!grepl("Events|_day",filez.old)]
# 
# # Missing files
# mis.y <- paste0(srzm,"_",cntz,"_clea_year.RData")[!paste0(srzm,"_",cntz,"_clea_year.RData")%in%dir(paste0("Output/Output_",srzm))]
# mis.m <- paste0(srzm,"_",cntz,"_clea_month.RData")[!paste0(srzm,"_",cntz,"_clea_month.RData")%in%dir(paste0("Output/Output_",srzm))]
# mis.w <- paste0(srzm,"_",cntz,"_clea_week.RData")[!paste0(srzm,"_",cntz,"_clea_week.RData")%in%dir(paste0("Output/Output_",srzm))]
# mis.d <- paste0(srzm,"_",cntz,"_clea_day.RData")[!paste0(srzm,"_",cntz,"_clea_day.RData")%in%dir(paste0("Output/Output_",srzm))]
# 
# # Exceptions
# source("Code/step2_eventcode/step2x_eventcode_admex.R")
# 
# # # Single core
# # for(k in k0:length(cntz)){
# 
# # Linux/Mac, multi-core
# # stopCluster(cl)
# mclapply(k0:length(cntz),function(k){
# if(sum(grepl(paste0(srzm,"_",cntz[k]), c(mis.y,mis.m,mis.w)))>0|(!remaining_only)){print(cntz[k])
#   # # Windows, multi-core
#   # foreach(k=k0:length(cntz),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(cntz[k])
#   # lapply(list.of.packages, require, character.only = TRUE)
# 
#   # Load events
#   load(paste0("Output/Output_",srzm,"/Events/",srzm,"_Events_",cntz[k],".RData",sep=""))
#   names(events) <- toupper(names(events))
#   events <- events[is.finite(as.numeric(as.character(events$LONG)))&is.finite(as.numeric(as.character(events$LAT))),]
#   coords <- events[,c("LONG","LAT")]
#   for(m in 1:2){coords[,m] <- as.numeric(as.character(coords[,m]))}
# 
#   # Dates
#   datez <- seq(as.Date("1900-01-01"), as.Date("2016-10-24"), by="days")
#   ticker <- data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
#   head(ticker)
#   ticker <- ticker[as.character(ticker$DATE)>=range(as.character(events$DATE),na.rm=T)[1]&as.character(ticker$DATE)<=range(as.character(events$DATE),na.rm=T)[2],]
# 
#   # Load CLEA polygons
#   cleaz.dir <- dir("Input/GIS/Borders/CLEA/Beta")
#   cleaz.dir <- cleaz.dir[grep("GRED",cleaz.dir)]
#   cleaz.dir0 <- sapply(strsplit(gsub("[0-9]|GRED_","",cleaz.dir),"_"),"[",1)
#   cleaz.dir0 <- gsub("DominicanRepublic","Dominican Republic",cleaz.dir0)
#   cleaz.dir0 <- gsub("SaintLucia","Saint Lucia",cleaz.dir0)
#   cleaz.dir0 <- gsub("MexicoPR","Mexico",cleaz.dir0)
#   cleaz.dir0 <- gsub("SouthAfrica","South Africa",cleaz.dir0)
#   cleaz.dir <- cleaz.dir[countrycode(cleaz.dir0,"country.name","iso3c")%in%cntz[k]]
#   cleaz.file <- gsub(".shp","",cleaz.dir[grep(".shp",cleaz.dir)])
#   clea.yr <- sapply(strsplit(cleaz.file,"_"),"[",3)
#   map.crop <- readShapePoly(paste0("Input/GIS/Borders/CLEA/Beta/",cleaz.dir))
#   names(map.crop) <- paste0("CLEA_",toupper(names(map.crop)))
#   clea.cst <- gsub(paste0("^",map.crop$CLEA_CTR[1],"0+"), "\\1", map.crop$CLEA_LINK, perl = TRUE)
#   map.crop$CLEA_CST <- paste0(map.crop$CLEA_CTR[1],".",clea.yr,".",clea.cst)
#   dim(map.crop)
# 
#   # Load GADM crop layer
#   #download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",0,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))
#   gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))
#   proj4string(map.crop) <- proj4string(gadm)
# 
#   # Add GADM names
#   gadm2 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",ifelse(cntz[k]%in%noadm2,ifelse(cntz[k]%in%noadm1,0,1),2),".rds",sep=""))
#   o <- over(map.crop,gadm2)
#   row.names(o) <- row.names(map.crop)
#   map.crop <- spCbind(map.crop,o)
#   head(map.crop)
# 
#   # Overlay events
#   sp.data <- SpatialPoints(coords,proj4string=CRS(proj4string(gadm)))
#   # par(mar=c(0,0,0,0));plot(map.crop);points(sp.data,col="red")
#   o <- over(sp.data, as(map.crop, "SpatialPolygons"))
#   o <- data.frame(KEY=o)
#   subdata <- cbind(events,o)
#   grid.data <- as.data.frame(map.crop)
#   grid.data$KEY <- 1:nrow(grid.data)
#   head(grid.data)
# 
#   ## Yearly event counts
#   disag <- sort(unique(grid.data$KEY))
#   i <- 1
# 
#   if((paste0(srzm,"_",cntz[k],"_clea_year.RData")%in%mis.y)|(!remaining_only)){
#   if((paste0(srzm,"_",cntz[k],"_clea_year.RData")%in%filez.old)|firstrun){
#   agg.sub <- lapply(1:length(disag),function(i){
#     sub <- subdata[subdata$KEY%in%disag[i],]
#     sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
#     sub$YSID <- sub$KEY*(10^nchar(max(as.character(ticker$YEAR))))+as.numeric(as.character(sub$YEAR))
#     sub$YSID2 <- as.numeric(as.character(sub$YEAR))*(10^nchar(max(disag)))+sub$KEY
#     vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
#     sub
#     if(nrow(sub)>0){
#       sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(YSID=sub$YSID,YSID2=sub$YSID2,KEY=sub$KEY,YEAR=sub$YEAR),FUN=sum)
#     }
#     if(nrow(sub)==0){
#       sub <- sub[,c("YSID","YSID2","KEY","YEAR",vars)]
#     }
#     # Remaining years (0's)
#     wids <- sort(unique(as.numeric(as.character(ticker$YEAR))))
#     wids <- wids[!wids%in%sub$YEAR]
#     if(length(wids)>0){
#       sub. <- data.frame(YSID=disag[i]*(10^nchar(max(wids)))+wids,YSID2=wids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],YEAR=wids)
#       sub.. <- as.data.frame(matrix(0,nrow=length(wids),ncol=length(vars)))
#       names(sub..) <- vars
#       sub <- rbind(sub,cbind(sub.,sub..))
#     }
#     # # Merge with months, years
#     # tix <- aggregate(ticker[,c("YEAR")],by=list(YRMO=ticker$YRMO),FUN=function(x){x[1]})
#     # sub <- merge(tix,sub,by="YRMO",all.x=F,all.y=T)
#     # Merge with covariates
#     sub <- merge(grid.data,sub,by="KEY",all.x=F,all.y=T)
#     sub <- sub[order(sub$YSID),]
#     sub
#   })
#   agg.data <- do.call(rbind,agg.sub)
#   agg.data <- agg.data[order(agg.data$YSID2),]
#   adm.year <- agg.data
#   summary(adm.year)
#   save(adm.year,file=paste0("Output/Output_",srzm,"/",srzm,"_",cntz[k],"_clea_year.RData"))
#   }
#   }
# 
#   ## Monthly event counts
#   disag <- sort(unique(grid.data$KEY))
#   i <- 2
#   if((paste0(srzm,"_",cntz[k],"_clea_month.RData")%in%mis.m)|(!remaining_only)){
#   if((paste0(srzm,"_",cntz[k],"_clea_month.RData")%in%filez.old)|firstrun){
#   agg.sub <- lapply(1:length(disag),function(i){
#     sub <- subdata[subdata$KEY%in%disag[i],]
#     sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
#     sub$MSID <- sub$KEY*(10^nchar(max(as.character(ticker$YRMO))))+as.numeric(as.character(sub$YRMO))
#     sub$MSID2 <- as.numeric(as.character(sub$YRMO))*(10^nchar(max(disag)))+sub$KEY
#     vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
#     sub
#     if(nrow(sub)>0){
#       sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(MSID=sub$MSID,MSID2=sub$MSID2,KEY=sub$KEY,YRMO=sub$YRMO),FUN=sum)
#     }
#     if(nrow(sub)==0){
#       vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
#       sub <- sub[,c("MSID","MSID2","KEY","YRMO",vars)]
#     }
#     # Remaining weeks (0's)
#     wids <- sort(unique(as.numeric(as.character(ticker$YRMO))))
#     wids <- wids[!wids%in%sub$YRMO]
#     if(length(wids)>0){
#       sub. <- data.frame(MSID=disag[i]*(10^nchar(max(wids)))+wids,MSID2=wids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],YRMO=wids)
#       sub.. <- as.data.frame(matrix(0,nrow=length(wids),ncol=length(vars)))
#       names(sub..) <- vars
#       sub <- rbind(sub,cbind(sub.,sub..))
#     }
#     # Merge with months, years
#     tix <- aggregate(ticker[,c("YEAR")],by=list(YRMO=ticker$YRMO),FUN=function(x){x[1]})
#     sub <- merge(tix,sub,by="YRMO",all.x=F,all.y=T)
#     # Merge with covariates
#     sub <- merge(grid.data,sub,by="KEY",all.x=F,all.y=T)
#     sub <- sub[order(sub$MSID),]
#     sub
#   })
#   agg.data <- do.call(rbind,agg.sub)
#   agg.data <- agg.data[order(agg.data$MSID2),]
#   adm.month <- agg.data
#   save(adm.month,file=paste0("Output/Output_",srzm,"/",srzm,"_",cntz[k],"_clea_month.RData"))
#   }
#   }
# 
#   ## Weekly event counts
#   disag <- sort(unique(grid.data$KEY))
#   i <- 2
#   if((paste0(srzm,"_",cntz[k],"_clea_week.RData")%in%mis.w)|(!remaining_only)){
#   if((paste0(srzm,"_",cntz[k],"_clea_week.RData")%in%filez.old)|firstrun){
#   agg.sub <- lapply(1:length(disag),function(i){
#     sub <- subdata[subdata$KEY%in%disag[i],]
#     sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
#     sub$WSID <- sub$KEY*(10^nchar(max(ticker$WID)))+sub$WID
#     sub$WSID2 <- sub$WID*(10^nchar(max(disag)))+sub$KEY
#     vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
#     sub
#     if(nrow(sub)>0){
#       sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(WSID=sub$WSID,WSID2=sub$WSID2,KEY=sub$KEY,WID=sub$WID),FUN=sum)
#     }
#     if(nrow(sub)==0){
#       vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
#       sub <- sub[,c("WSID","WSID2","KEY","WID",vars)]
#     }
#     # Remaining weeks (0's)
#     wids <- sort(unique(ticker$WID))
#     wids <- wids[!wids%in%sub$WID]
#     if(length(wids)>0){
#       sub. <- data.frame(WSID=disag[i]*(10^nchar(max(ticker$WID)))+wids,WSID2=wids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],WID=wids)
#       sub.. <- as.data.frame(matrix(0,nrow=length(wids),ncol=length(vars)))
#       names(sub..) <- vars
#       sub <- rbind(sub,cbind(sub.,sub..))
#     }
#     # Merge with months, years
#     tix <- aggregate(ticker[,c("YEAR","YRMO")],by=list(WID=ticker$WID),FUN=function(x){x[1]})
#     sub <- merge(tix,sub,by="WID",all.x=F,all.y=T)
#     # Merge with covariates
#     sub <- merge(grid.data,sub,by="KEY",all.x=F,all.y=T)
#     sub <- sub[order(sub$WSID),]
#     sub
#   })
#   agg.data <- do.call(rbind,agg.sub)
#   agg.data <- agg.data[order(agg.data$WSID2),]
#   adm.week <- agg.data
#   save(adm.week,file=paste0("Output/Output_",srzm,"/",srzm,"_",cntz[k],"_clea_week.RData"))
#   }
#   }
# 
#   # ## Daily event counts
#   # disag <- sort(unique(grid.data$KEY))
#   # i <- 2
#   # if((paste0(srzm,"_",cntz[k],"_clea_day.RData")%in%mis.d)|(!remaining_only)){
#   # if((paste0(srzm,"_",cntz[k],"_clea_day.RData")%in%filez.old)|firstrun){
#   # agg.sub <- lapply(1:length(disag),function(i){
#   #   sub <- subdata[subdata$KEY%in%disag[i],]
#   #   sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
#   #   sub$TSID <- sub$KEY*(10^nchar(max(ticker$TID)))+sub$TID
#   #   sub$TSID2 <- sub$TID*(10^nchar(max(disag)))+sub$KEY
#   #   vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
#   #   sub
#   #   if(nrow(sub)>0){
#   #     sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(TSID=sub$TSID,TSID2=sub$TSID2,KEY=sub$KEY,TID=sub$TID),FUN=sum)
#   #   }
#   #   if(nrow(sub)==0){
#   #     vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
#   #     sub <- sub[,c("TSID","TSID2","KEY","TID",vars)]
#   #   }
#   #   # Remaining days (0's)
#   #   tids <- sort(unique(ticker$TID))
#   #   tids <- tids[!tids%in%sub$TID]
#   #   if(length(tids)>0){
#   #     sub. <- data.frame(TSID=disag[i]*(10^nchar(max(ticker$TID)))+tids,TSID2=tids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],TID=tids)
#   #     sub.. <- as.data.frame(matrix(0,nrow=length(tids),ncol=length(vars)))
#   #     names(sub..) <- vars
#   #     sub <- rbind(sub,cbind(sub.,sub..))
#   #   }
#   #   # Merge with months, years
#   #   tix <- aggregate(ticker[,c("YEAR","YRMO","WID","DATE")],by=list(TID=ticker$TID),FUN=function(x){x[1]})
#   #   sub <- merge(tix,sub,by="TID",all.x=F,all.y=T)
#   #   head(sub)
#   #   # Merge with covariates
#   #   sub <- merge(grid.data,sub,by="KEY",all.x=F,all.y=T)
#   #   sub <- sub[order(sub$TSID),]
#   #   sub
#   # })
#   # agg.data <- do.call(rbind,agg.sub)
#   # agg.data <- agg.data[order(agg.data$TSID2),]
#   # adm.day <- agg.data
#   # save(adm.day,file=paste0("Output/Output_",srzm,"/",srzm,"_",cntz[k],"_clea_day.RData"))
#   # }
#   # }
#   #
#   # # Close loop, Single core
#   # }
#   #
#   # Close loop: countries (Linux/Mac)
# 
# }
#   },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
# #
# # # Close loop (Windows)
# # }
# # stopCluster(cl)



############################
## Aggregate: GADM
#############################

rm(list=ls())

# Set parameters
source("Code/step2_eventcode/step2x_melttpar.R")

# Parallel computing
mcoptions <- list(preschedule=FALSE, set.seed=TRUE)
ncores <- detectCores()
# cl <- makeCluster(ncores)
# registerDoParallel(cl)

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","spatstat","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# Exceptions
source("Code/step2_eventcode/step2x_eventcode_admex.R")

# List files
srzm <- paste0("MELTT",tw,sw)
filez <- dir(paste0("Output/Output_",srzm,"/Events"))
filez <- filez[grep("Events",filez)]
cntz <- gsub(".RData","",sapply(strsplit(filez,"_"),"[",3))
cntz <- cntz[cntz!="000"]
cntz
k0 <- 1; cntz[k0]; k <-k0

# Missing files
mis.y <- paste0(srzm,"_",rep(cntz,3),"_adm",rep(0:2,each=length(cntz)),"_year.RData")[!paste0(srzm,"_",rep(cntz,3),"_adm",rep(0:2,each=length(cntz)),"_year.RData")%in%dir(paste0("Output/Output_",srzm))]
mis.m <- paste0(srzm,"_",rep(cntz,3),"_adm",rep(0:2,each=length(cntz)),"_month.RData")[!paste0(srzm,"_",rep(cntz,3),"_adm",rep(0:2,each=length(cntz)),"_month.RData")%in%dir(paste0("Output/Output_",srzm))]
mis.w <- paste0(srzm,"_",rep(cntz,3),"_adm",rep(0:2,each=length(cntz)),"_week.RData")[!paste0(srzm,"_",rep(cntz,3),"_adm",rep(0:2,each=length(cntz)),"_week.RData")%in%dir(paste0("Output/Output_",srzm))]
mis.d <- paste0(srzm,"_",rep(cntz,3),"_adm",rep(0:2,each=length(cntz)),"_day.RData")[!paste0(srzm,"_",rep(cntz,3),"_adm",rep(0:2,each=length(cntz)),"_day.RData")%in%dir(paste0("Output/Output_",srzm))]
mis.y <- mis.y[!grepl(paste0(paste0(noadm1,"_adm1",collapse="|"),"|",paste0(noadm2,"_adm2",collapse="|")),mis.y)]
mis.m <- mis.m[!grepl(paste0(paste0(noadm1,"_adm1",collapse="|"),"|",paste0(noadm2,"_adm2",collapse="|")),mis.m)]
mis.w <- mis.w[!grepl(paste0(paste0(noadm1,"_adm1",collapse="|"),"|",paste0(noadm2,"_adm2",collapse="|")),mis.w)]
mis.d <- mis.d[!grepl(paste0(paste0(noadm1,"_adm1",collapse="|"),"|",paste0(noadm2,"_adm2",collapse="|")),mis.d)]

# Fix errors (replace older files)
filez.old <- dir(paste0("Output/Output_",srzm))[substr(file.mtime(paste0("Output/Output_",srzm,"/",dir(paste0("Output/Output_",srzm)))),1,10)<mdate]
filez.old <- filez.old[!grepl("Events|_day|_v6",filez.old)]

# # Single core loop
# for(k in k0:length(cntz)){print(cntz[k])

# Linux/Mac
# stopCluster(cl)
mclapply(k0:length(cntz),function(k){
if(sum(grepl(paste0(srzm,"_",cntz[k]), c(mis.y,mis.m,mis.w)))>0|(!remaining_only)){print(cntz[k])

# # Windows
# foreach(k=k0:length(cntz),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(cntz[k])
# lapply(list.of.packages, require, character.only = TRUE)

  # Load events
  load(paste0("Output/Output_",srzm,"/Events/",srzm,"_Events_",cntz[k],".RData",sep=""))
  names(events) <- toupper(names(events))
  events <- events[is.finite(as.numeric(as.character(events$LONG)))&is.finite(as.numeric(as.character(events$LAT))),]
  coords <- events[,c("LONG","LAT")]
  for(m in 1:2){coords[,m] <- as.numeric(as.character(coords[,m]))}

  # Dates
  datez <- seq(as.Date("1900-01-01"), as.Date("2016-10-24"), by="days")
  ticker <- data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
  ticker <- ticker[as.character(ticker$DATE)>=range(as.character(events$DATE),na.rm=T)[1]&as.character(ticker$DATE)<=range(as.character(events$DATE),na.rm=T)[2],]
  head(ticker)

  ## Open loop (adm)
  a <- 0
  for(a in 0:2){print(paste0(cntz[k],"/",a))
    admz <- a
    #load(url(paste("http://biogeo.ucdavis.edu/data/gadm2/R/",cntz[k],"_adm",admz,".RData",sep="")))

    # Start adm2 exception
    if(!((a==1)&(cntz[k]%in%noadm1))){
    if(!((a==2)&(cntz[k]%in%noadm2))){

      if(!paste0(cntz[k],"_adm",admz,".rds",sep="")%in%dir("Input/GIS/Borders/GADM/")){
        download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",admz,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",admz,".rds",sep=""))}
      gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",admz,".rds"))
      # par(mar=rep(0,4)); plot(gadm); points(events[,c("LONG","LAT")],cex=.3,col="red")

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
      if((paste0(srzm,"_",cntz[k],"_adm",admz,"_year.RData")%in%mis.y)|(!remaining_only)){
      if((paste0(srzm,"_",cntz[k],"_adm",admz,"_year.RData")%in%filez.old)|firstrun){
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
      save(adm.year,file=paste0("Output/Output_",srzm,"/",srzm,"_",cntz[k],"_adm",admz,"_year.RData"))
      }
      }

      ## Monthly event counts
      disag <- sort(unique(gadm.data$KEY))
      i <- 2
      if((paste0(srzm,"_",cntz[k],"_adm",admz,"_month.RData")%in%mis.m)|(!remaining_only)){
      if((paste0(srzm,"_",cntz[k],"_adm",admz,"_month.RData")%in%filez.old)|firstrun){
      agg.sub <- lapply(1:length(disag),function(i){
        sub <- subdata[subdata$KEY%in%disag[i],]
        sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
        sub$MSID <- sub$KEY*(10^nchar(max(as.character(ticker$YRMO))))+as.numeric(as.character(sub$YRMO))
        sub$MSID2 <- as.numeric(as.character(sub$YRMO))*(10^nchar(max(disag)))+sub$KEY
        vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
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
      save(adm.month,file=paste0("Output/Output_",srzm,"/",srzm,"_",cntz[k],"_adm",admz,"_month.RData"))
      }
      }

      ## Weekly event counts
      disag <- sort(unique(gadm.data$KEY))
      i <- 2
      if((paste0(srzm,"_",cntz[k],"_adm",admz,"_week.RData")%in%mis.w)|(!remaining_only)){
      if((paste0(srzm,"_",cntz[k],"_adm",admz,"_week.RData")%in%filez.old)|firstrun){
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
      save(adm.week,file=paste0("Output/Output_",srzm,"/",srzm,"_",cntz[k],"_adm",admz,"_week.RData"))
      }
      }

      # ## Daily event counts
      # if(admz!=2){
      # disag <- sort(unique(gadm.data$KEY))
      # i <- 2
      # if((paste0(srzm,"_",cntz[k],"_adm",admz,"_day.RData")%in%mis.d)|(!remaining_only)){
      # if((paste0(srzm,"_",cntz[k],"_adm",admz,"_day.RData")%in%filez.old)|firstrun){
      # agg.sub <- lapply(1:length(disag),function(i){
      #   sub <- subdata[subdata$KEY%in%disag[i],]
      #   sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
      #   sub$TSID <- sub$KEY*(10^nchar(max(ticker$TID)))+sub$TID
      #   sub$TSID2 <- sub$TID*(10^nchar(max(disag)))+sub$KEY
      #   vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
      #   sub
      #   if(nrow(sub)>0){
      #     sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(TSID=sub$TSID,TSID2=sub$TSID2,KEY=sub$KEY,TID=sub$TID),FUN=sum)
      #   }
      #   if(nrow(sub)==0){
      #     vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
      #     sub <- sub[,c("TSID","TSID2","KEY","TID",vars)]
      #   }
      #   # Remaining days (0's)
      #   tids <- sort(unique(ticker$TID))
      #   tids <- tids[!tids%in%sub$TID]
      #   if(length(tids)>0){
      #     sub. <- data.frame(TSID=disag[i]*(10^nchar(max(ticker$TID)))+tids,TSID2=tids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],TID=tids)
      #     sub.. <- as.data.frame(matrix(0,nrow=length(tids),ncol=length(vars)))
      #     names(sub..) <- vars
      #     sub <- rbind(sub,cbind(sub.,sub..))
      #   }
      #   # Merge with months, years
      #   tix <- aggregate(ticker[,c("YEAR","YRMO","WID","DATE")],by=list(TID=ticker$TID),FUN=function(x){x[1]})
      #   sub <- merge(tix,sub,by="TID",all.x=F,all.y=T)
      #   head(sub)
      #   # Merge with covariates
      #   sub <- merge(gadm.data,sub,by="KEY",all.x=F,all.y=T)
      #   sub <- sub[order(sub$TSID),]
      #   sub
      # })
      # agg.data <- do.call(rbind,agg.sub)
      # agg.data <- agg.data[order(agg.data$TSID2),]
      # adm.day <- agg.data
      # save(adm.day,file=paste0("Output/Output_",srzm,"/",srzm,"_",cntz[k],"_adm",admz,"_day.RData"))
      # }
      # }
      # }

      # End adm2 exceptions
    }}

    # Close loop (adm)
  }

# Close loop: countries (Linux/Mac)
}
},mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)

# # Close loop (Windows)
# }
# stopCluster(cl)

# # Close loop (single core)
# }






# #############################
# ## Grid cells
# #############################
# 
# rm(list=ls())
# 
# # Set parameters
# source("Code/step2_eventcode/step2x_melttpar.R")
# 
# # Parallel computing
# mcoptions <- list(preschedule=FALSE, set.seed=TRUE)
# ncores <- detectCores()
# # cl <- makeCluster(ncores)
# # registerDoParallel(cl)
# 
# ## Install & load packages (all at once)
# list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","spatstat","parallel","foreach")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
# lapply(list.of.packages, require, character.only = TRUE)
# 
# # List files
# srzm <- paste0("MELTT",tw,sw)
# filez <- dir(paste0("Output/Output_",srzm,"/Events"))
# filez <- filez[grep("Events",filez)]
# cntz <- gsub(".RData","",sapply(strsplit(filez,"_"),"[",3))
# cntz <- cntz[cntz!="000"]
# k0 <- 1; cntz[k0]; k <- k0
# 
# # Missing files
# mis.y <- paste0(srzm,"_",cntz,"_priogrid_year.RData")[!paste0(srzm,"_",cntz,"_priogrid_year.RData")%in%dir(paste0("Output/Output_",srzm))]
# mis.m <- paste0(srzm,"_",cntz,"_priogrid_month.RData")[!paste0(srzm,"_",cntz,"_priogrid_month.RData")%in%dir(paste0("Output/Output_",srzm))]
# mis.w <- paste0(srzm,"_",cntz,"_priogrid_week.RData")[!paste0(srzm,"_",cntz,"_priogrid_week.RData")%in%dir(paste0("Output/Output_",srzm))]
# mis.d <- paste0(srzm,"_",cntz,"_priogrid_day.RData")[!paste0(srzm,"_",cntz,"_priogrid_day.RData")%in%dir(paste0("Output/Output_",srzm))]
# 
# # # Fix errors
# # ixp2 <- strsplit(dir(paste0("Output/Output_",srzm,""))[file.size(paste0("Output/Output_",srzm,"/",dir("Output/Output_",srzm,"")))<3000],"_")
# # ixp2 <- ixp2[sapply(ixp2, '[', 4)=="week.RData"]
# # cntz <- cntz[cntz%in%sapply(ixp2, '[', 2)]
# 
# # Fix errors (replace older files)
# filez.old <- dir(paste0("Output/Output_",srzm))[substr(file.mtime(paste0("Output/Output_",srzm,"/",dir(paste0("Output/Output_",srzm)))),1,10)<mdate]
# filez.old <- filez.old[!grepl("Events|_day|_v6",filez.old)]
# 
# # Exceptions
# source("Code/step2_eventcode/step2x_eventcode_admex.R")
# 
# # # Single core loop
# # for(k in k0:length(cntz)){print(cntz[k])
# 
# # Linux/Mac
# # stopCluster(cl)
# mclapply(k0:length(cntz),function(k){
# if(sum(grepl(paste0(srzm,"_",cntz[k]), c(mis.y,mis.m,mis.w)))>0|(!remaining_only)){print(cntz[k])
# 
#     # # Windows
#   # foreach(k=k0:length(cntz),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(cntz[k])
#   # lapply(list.of.packages, require, character.only = TRUE)
# 
#   # Load events
#   load(paste0("Output/Output_",srzm,"/Events/",srzm,"_Events_",cntz[k],".RData",sep=""))
#   names(events) <- toupper(names(events))
#   events <- events[is.finite(as.numeric(as.character(events$LONG)))&is.finite(as.numeric(as.character(events$LAT))),]
#   coords <- events[,c("LONG","LAT")]
#   for(m in 1:2){coords[,m] <- as.numeric(as.character(coords[,m]))}
# 
#   # Dates
#   datez <- seq(as.Date("1900-01-01"), as.Date("2016-10-24"), by="days")
#   ticker <- data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
#   head(ticker)
#   ticker <- ticker[as.character(ticker$DATE)>=range(as.character(events$DATE),na.rm=T)[1]&as.character(ticker$DATE)<=range(as.character(events$DATE),na.rm=T)[2],]
# 
#   # Load Prio Grid
#   #map0 <- readShapePoly("Input/GIS/Borders/PRIOGRID/priogrid_cell")
#   #save(map0,file="Input/GIS/Borders/PRIOGRID/PRIOGRID.RData")
#   load("Input/GIS/Borders/PRIOGRID/PRIOGRID.RData")
# 
#   # Load GADM crop layer
#   #download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",0,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))
#   gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))
#   proj4string(map0) <- proj4string(gadm)
# 
#   # Crop by extent of map
#   coords0 <- as.data.frame(map0)[,c("xcoord","ycoord")]
#   sp.data0 <- SpatialPoints(coords0,proj4string=CRS(proj4string(gadm)))
#   sp.box <- as(extent(bbox(gadm)),"SpatialPolygons")
#   proj4string(sp.box) <- proj4string(gadm)
#   o <- over(map0, as(sp.box, "SpatialPolygons"))
#   map.crop <- map0[which(!is.na(o)),]
#   o <- over(map.crop, as(gadm, "SpatialPolygons"))
#   map.crop <- map.crop[which(!is.na(o)),]
#   names(map.crop) <- paste0("PRIO_",toupper(names(map.crop)))
# 
#   # Add GADM names
#   gadm2 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",ifelse(cntz[k]%in%noadm2,ifelse(cntz[k]%in%noadm1,0,1),2),".rds",sep=""))
#   o <- over(map.crop,gadm2)
#   row.names(o) <- row.names(map.crop)
#   map.crop <- spCbind(map.crop,o)
# 
#   # Overlay events
#   sp.data <- SpatialPoints(coords,proj4string=CRS(proj4string(gadm)))
#   # par(mar=c(0,0,0,0));plot(map.crop);points(sp.data,col="red")
#   o <- over(sp.data, as(map.crop, "SpatialPolygons"))
#   o <- data.frame(KEY=o)
#   subdata <- cbind(events,o)
#   grid.data <- as.data.frame(map.crop)
#   grid.data$KEY <- 1:nrow(grid.data)
#   head(grid.data)
# 
#   ## Yearly event counts
#   disag <- sort(unique(grid.data$KEY))
#   i <- 1
#   if((paste0(srzm,"_",cntz[k],"_priogrid_year.RData")%in%mis.y)|(!remaining_only)){
#   if((paste0(srzm,"_",cntz[k],"_priogrid_year.RData")%in%filez.old)|firstrun){
#   agg.sub <- lapply(1:length(disag),function(i){
#     sub <- subdata[subdata$KEY%in%disag[i],]
#     sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
#     sub$YSID <- sub$KEY*(10^nchar(max(as.character(ticker$YEAR))))+as.numeric(as.character(sub$YEAR))
#     sub$YSID2 <- as.numeric(as.character(sub$YEAR))*(10^nchar(max(disag)))+sub$KEY
#     vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
#     sub
#     if(nrow(sub)>0){
#       sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(YSID=sub$YSID,YSID2=sub$YSID2,KEY=sub$KEY,YEAR=sub$YEAR),FUN=sum)
#     }
#     if(nrow(sub)==0){
#       sub <- sub[,c("YSID","YSID2","KEY","YEAR",vars)]
#     }
#     # Remaining years (0's)
#     wids <- sort(unique(as.numeric(as.character(ticker$YEAR))))
#     wids <- wids[!wids%in%sub$YEAR]
#     if(length(wids)>0){
#       sub. <- data.frame(YSID=disag[i]*(10^nchar(max(wids)))+wids,YSID2=wids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],YEAR=wids)
#       sub.. <- as.data.frame(matrix(0,nrow=length(wids),ncol=length(vars)))
#       names(sub..) <- vars
#       sub <- rbind(sub,cbind(sub.,sub..))
#     }
#     # # Merge with months, years
#     # tix <- aggregate(ticker[,c("YEAR")],by=list(YRMO=ticker$YRMO),FUN=function(x){x[1]})
#     # sub <- merge(tix,sub,by="YRMO",all.x=F,all.y=T)
#     # Merge with covariates
#     sub <- merge(grid.data,sub,by="KEY",all.x=F,all.y=T)
#     sub <- sub[order(sub$YSID),]
#     sub
#   })
#   agg.data <- do.call(rbind,agg.sub)
#   agg.data <- agg.data[order(agg.data$YSID2),]
#   adm.year <- agg.data
#   summary(adm.year)
#   save(adm.year,file=paste0("Output/Output_",srzm,"/",srzm,"_",cntz[k],"_priogrid_year.RData"))
#   }
#   }
# 
#   ## Monthly event counts
#   disag <- sort(unique(grid.data$KEY))
#   i <- 2
#   if((paste0(srzm,"_",cntz[k],"_priogrid_month.RData")%in%mis.m)|(!remaining_only)){
#   if((paste0(srzm,"_",cntz[k],"_priogrid_month.RData")%in%filez.old)|firstrun){
#   agg.sub <- lapply(1:length(disag),function(i){
#     sub <- subdata[subdata$KEY%in%disag[i],]
#     sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
#     sub$MSID <- sub$KEY*(10^nchar(max(as.character(ticker$YRMO))))+as.numeric(as.character(sub$YRMO))
#     sub$MSID2 <- as.numeric(as.character(sub$YRMO))*(10^nchar(max(disag)))+sub$KEY
#     vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
#     sub
#     if(nrow(sub)>0){
#       sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(MSID=sub$MSID,MSID2=sub$MSID2,KEY=sub$KEY,YRMO=sub$YRMO),FUN=sum)
#     }
#     if(nrow(sub)==0){
#       vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
#       sub <- sub[,c("MSID","MSID2","KEY","YRMO",vars)]
#     }
#     # Remaining weeks (0's)
#     wids <- sort(unique(as.numeric(as.character(ticker$YRMO))))
#     wids <- wids[!wids%in%sub$YRMO]
#     if(length(wids)>0){
#       sub. <- data.frame(MSID=disag[i]*(10^nchar(max(wids)))+wids,MSID2=wids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],YRMO=wids)
#       sub.. <- as.data.frame(matrix(0,nrow=length(wids),ncol=length(vars)))
#       names(sub..) <- vars
#       sub <- rbind(sub,cbind(sub.,sub..))
#     }
#     # Merge with months, years
#     tix <- aggregate(ticker[,c("YEAR")],by=list(YRMO=ticker$YRMO),FUN=function(x){x[1]})
#     sub <- merge(tix,sub,by="YRMO",all.x=F,all.y=T)
#     # Merge with covariates
#     sub <- merge(grid.data,sub,by="KEY",all.x=F,all.y=T)
#     sub <- sub[order(sub$MSID),]
#     sub
#   })
#   agg.data <- do.call(rbind,agg.sub)
#   agg.data <- agg.data[order(agg.data$MSID2),]
#   adm.month <- agg.data
#   save(adm.month,file=paste0("Output/Output_",srzm,"/",srzm,"_",cntz[k],"_priogrid_month.RData"))
#   }
#   }
# 
#   ## Weekly event counts
#   disag <- sort(unique(grid.data$KEY))
#   i <- 2
#   if((paste0(srzm,"_",cntz[k],"_priogrid_week.RData")%in%mis.w)|(!remaining_only)){
#   if((paste0(srzm,"_",cntz[k],"_priogrid_week.RData")%in%filez.old)|firstrun){
#   agg.sub <- lapply(1:length(disag),function(i){
#     sub <- subdata[subdata$KEY%in%disag[i],]
#     sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
#     sub$WSID <- sub$KEY*(10^nchar(max(ticker$WID)))+sub$WID
#     sub$WSID2 <- sub$WID*(10^nchar(max(disag)))+sub$KEY
#     vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
#     sub
#     if(nrow(sub)>0){
#       sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(WSID=sub$WSID,WSID2=sub$WSID2,KEY=sub$KEY,WID=sub$WID),FUN=sum)
#     }
#     if(nrow(sub)==0){
#       vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
#       sub <- sub[,c("WSID","WSID2","KEY","WID",vars)]
#     }
#     # Remaining weeks (0's)
#     wids <- sort(unique(ticker$WID))
#     wids <- wids[!wids%in%sub$WID]
#     if(length(wids)>0){
#       sub. <- data.frame(WSID=disag[i]*(10^nchar(max(ticker$WID)))+wids,WSID2=wids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],WID=wids)
#       sub.. <- as.data.frame(matrix(0,nrow=length(wids),ncol=length(vars)))
#       names(sub..) <- vars
#       sub <- rbind(sub,cbind(sub.,sub..))
#     }
#     # Merge with months, years
#     tix <- aggregate(ticker[,c("YEAR","YRMO")],by=list(WID=ticker$WID),FUN=function(x){x[1]})
#     sub <- merge(tix,sub,by="WID",all.x=F,all.y=T)
#     # Merge with covariates
#     sub <- merge(grid.data,sub,by="KEY",all.x=F,all.y=T)
#     sub <- sub[order(sub$WSID),]
#     sub
#   })
#   agg.data <- do.call(rbind,agg.sub)
#   agg.data <- agg.data[order(agg.data$WSID2),]
#   adm.week <- agg.data
#   save(adm.week,file=paste0("Output/Output_",srzm,"/",srzm,"_",cntz[k],"_priogrid_week.RData"))
#   }
#   }
# 
#   # ## Daily event counts
#   # disag <- sort(unique(grid.data$KEY))
#   # i <- 2
#   # if((paste0(srzm,"_",cntz[k],"_priogrid_day.RData")%in%mis.d)|(!remaining_only)){
#   # if((paste0(srzm,"_",cntz[k],"_priogrid_day.RData")%in%filez.old)|firstrun){
#   # agg.sub <- lapply(1:length(disag),function(i){
#   #   sub <- subdata[subdata$KEY%in%disag[i],]
#   #   sub <- merge(ticker,sub[,],by.x="DATE",by.y="DATE")
#   #   sub$TSID <- sub$KEY*(10^nchar(max(ticker$TID)))+sub$TID
#   #   sub$TSID2 <- sub$TID*(10^nchar(max(disag)))+sub$KEY
#   #   vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
#   #   sub
#   #   if(nrow(sub)>0){
#   #     sub <- aggregate(sub[,grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))],by=list(TSID=sub$TSID,TSID2=sub$TSID2,KEY=sub$KEY,TID=sub$TID),FUN=sum)
#   #   }
#   #   if(nrow(sub)==0){
#   #     vars <- names(sub)[grep("INITIATOR|TARGET|ACTION|SIDE",names(sub))]
#   #     sub <- sub[,c("TSID","TSID2","KEY","TID",vars)]
#   #   }
#   #   # Remaining days (0's)
#   #   tids <- sort(unique(ticker$TID))
#   #   tids <- tids[!tids%in%sub$TID]
#   #   if(length(tids)>0){
#   #     sub. <- data.frame(TSID=disag[i]*(10^nchar(max(ticker$TID)))+tids,TSID2=tids*(10^nchar(max(disag)))+disag[i],KEY=disag[i],TID=tids)
#   #     sub.. <- as.data.frame(matrix(0,nrow=length(tids),ncol=length(vars)))
#   #     names(sub..) <- vars
#   #     sub <- rbind(sub,cbind(sub.,sub..))
#   #   }
#   #   # Merge with months, years
#   #   tix <- aggregate(ticker[,c("YEAR","YRMO","WID","DATE")],by=list(TID=ticker$TID),FUN=function(x){x[1]})
#   #   sub <- merge(tix,sub,by="TID",all.x=F,all.y=T)
#   #   head(sub)
#   #   # Merge with covariates
#   #   sub <- merge(grid.data,sub,by="KEY",all.x=F,all.y=T)
#   #   sub <- sub[order(sub$TSID),]
#   #   sub
#   # })
#   # agg.data <- do.call(rbind,agg.sub)
#   # agg.data <- agg.data[order(agg.data$TSID2),]
#   # adm.day <- agg.data
#   # save(adm.day,file=paste0("Output/Output_",srzm,"/",srzm,"_",cntz[k],"_priogrid_day.RData"))
#   # }
#   # }
# 
#   # Close loop: countries (Linux/Mac)
# }
# },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
# 
# # # Close loop (Windows)
# # }
# # stopCluster(cl)
# 
# # # Close loop (single core)
# # }