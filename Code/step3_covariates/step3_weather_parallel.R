rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("~/Dropbox (Zhukov research team)/XSub/Data")
# setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","spatstat","parallel","foreach", "doParallel","fields","ncdf4")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# Parallel computing
mcoptions <- list(preschedule=FALSE, set.seed=TRUE)
ncores <- detectCores()
# cl <- makeCluster(ncores)
# registerDoParallel(cl)

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","spatstat","parallel","foreach", "doParallel","fields","ncdf4")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


## Units of analysis
space.agg <- c("adm0","adm1","adm2","priogrid","clea")
space.ix <- c("ID_0","ID_1","ID_2","PRIO_GID","CLEA_CST")
time.agg <- c("year","month","week","day")
time.ix <- c("YEAR","YRMO","WID","TID")


#############################
## Open nc
#############################

## 
# Temp
##

# Load weather data
mycdf <- nc_open("Input/GIS/Covariates/NOAA/air_mon_mean_v401.nc", verbose = TRUE, write = FALSE)
mycdf # Inspect
timedata <- ncvar_get(mycdf,'time')
lat <- ncvar_get(mycdf,'lat')
long <- ncvar_get(mycdf,'lon')
xdata <- ncvar_get(mycdf,'air')
dim(xdata)

# Alternate longitude
long.alt <- long
long.alt[long>180] <- long.alt[long>180]-360

# Re-order columns
xdata <- xdata[order(long.alt),,]
long.alt <- long.alt[order(long.alt)]

# Plot
# image.plot(long.alt,rev(lat),xdata[,rev(1:dim(xdata)[2]),1000])


## 
# Precipitation
##

# Load weather data
mycdf2 <- nc_open("Input/GIS/Covariates/NOAA/precip.mon.total.v401.nc", verbose = TRUE, write = FALSE)
mycdf2 # Inspect
timedata2 <- ncvar_get(mycdf2,'time')
lat2 <- ncvar_get(mycdf2,'lat')
long2 <- ncvar_get(mycdf2,'lon')
xdata2 <- ncvar_get(mycdf2,'precip')
dim(xdata2)

# Alternate longitude
long.alt2 <- long2
long.alt2[long2>180] <- long.alt2[long2>180]-360

# Re-order columns
xdata2 <- xdata2[order(long.alt2),,]
long.alt2 <- long.alt2[order(long.alt2)]

# Plot
# image.plot(long.alt2,rev(lat2),xdata2[,rev(1:dim(xdata2)[2]),100])



#############################
# Time matrix
#############################

##
# Temp
##

# Hours
seq.hourz <- seq(as.Date("1900-1-1 0:0:0"),as.Date("2017-1-1 0:0:0"),by=1/24)
seq.hourz <- data.frame(DATE=gsub("-","",seq.hourz),HOUR=1:length(seq.hourz))
seq.hourz[1:48,]

# Dates (old)
datez <- seq(as.Date("1945-05-09"), as.Date(sapply(strsplit(as.character(Sys.time())," "),"[",1)), by="days")
ticker1 <- data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
# Dates (new)
datez <- seq(as.Date("1900-1-1"), as.Date(sapply(strsplit(as.character(Sys.time())," "),"[",1)), by="days")
ticker2 <- data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
tid.delta <- ticker2[ticker2$DATE==19450509,"TID"]-ticker1[1,"TID"] # 16564
wid.delta <- ticker2[ticker2$DATE==19450509,"WID"]-ticker1[1,"WID"] # 2366

# Merge dates with hours
ticker <- merge(ticker2,seq.hourz,by="DATE",all.x=F,all.y=T)
ticker <- ticker[ticker$HOUR<range(timedata)[2],]
tail(ticker)
weather.times <- ticker[ticker$HOUR%in%timedata,]
tail(weather.times)
rm(ticker,ticker1,ticker2,datez,seq.hourz)

##
# Rain
##

# Hours
seq.hourz <- seq(as.Date("1900-1-1 0:0:0"),as.Date("2017-1-1 0:0:0"),by=1/24)
seq.hourz <- data.frame(DATE=gsub("-","",seq.hourz),HOUR=1:length(seq.hourz))
seq.hourz[1:48,]

# Dates (old)
datez <- seq(as.Date("1945-05-09"), as.Date(sapply(strsplit(as.character(Sys.time())," "),"[",1)), by="days")
ticker1 <- data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
# Dates (new)
datez <- seq(as.Date("1900-1-1"), as.Date(sapply(strsplit(as.character(Sys.time())," "),"[",1)), by="days")
ticker2 <- data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
tid.delta <- ticker2[ticker2$DATE==19450509,"TID"]-ticker1[1,"TID"] # 16564
wid.delta <- ticker2[ticker2$DATE==19450509,"WID"]-ticker1[1,"WID"] # 2366

# Merge dates with hours
ticker <- merge(ticker2,seq.hourz,by="DATE",all.x=F,all.y=T)
ticker <- ticker[ticker$HOUR<range(timedata2)[2],]
rain.times <- ticker[ticker$HOUR%in%timedata2,]
tail(rain.times)
rm(ticker,ticker1,ticker2,datez,seq.hourz)


#############################
## Country list
#############################

#rm(list=ls())

# Exceptions
source("Code/step2_eventcode/step2x_eventcode_admex.R")

# List of files
filez <- c(dir("Output/Output_ACLED/Events"),dir("Output/Output_GED/Events"),dir("Output/Output_PITF/Events"),dir("Output/Output_SCAD/Events"),dir("Output/Output_ACD/Events/"),dir("Output/Output_ESOC/ESOC_Iraq/Events/"),dir("Output/Output_ESOC/ESOC_Mexico_homicide/Events/"),dir("Output/Output_ESOC/ESOC_Pakistan/Events/"),dir("Output/Output_Sullivan_Guatemala/Events/"),dir("Output/Output_yzCaucasus2000/Events/"),dir("Output/Output_yzChechnya/Events/"),dir("Output/Output_yzLibya/"),dir("Output/Output_yzUkraine2014/"),dir("Output/Output_Covariates/"))
filez <- filez[grep("Events|Covariates",filez)]
cntz <- gsub("ACLED_Events_|GED_Events_|PITF_Events_|SCAD_Events_|Covariates_|Sullivan_Guatemala_Events_|ESOC_Iraq_Events_|ESOC_Mexico_homicide_Events_|yzChechnya_Events_|yzCaucasus2000_Events_|ESOC_Pakistan_Events|ACD_Events_|.RData","",filez)
cntz <- sapply(strsplit(cntz,"_"),"[",1)
cntz <- cntz[cntz!="000"&!is.na(cntz)&cntz!="NA"&cntz!="Events"&cntz!=""]
cntz <- sort(unique(cntz))
ixp <- strsplit(dir("Output/Output_Weather"),"_")
filez.in <- dir("Output/Output_Weather")
cntz

# # Limit to remaining/incomplete
# cntz <- cntz[!paste0("Weather_",cntz,"_adm",ifelse(cntz%in%noadm2,ifelse(cntz%in%noadm1,0,1),2),".RData")%in%filez.in]
# cntz <- cntz[!cntz%in%sapply(ixp, '[', 2)]
# cntz <- cntz[!cntz%in%c("USA")]

# List CLEA
cleaz <- dir("Input/GIS/Borders/CLEA/Beta")
cleaz <- cleaz[grep("GRED",cleaz)]
cleaz <- sapply(strsplit(gsub("[0-9]|GRED_","",cleaz),"_"),"[",1)
cleaz <- gsub("DominicanRepublic","Dominican Republic",cleaz)
cleaz <- gsub("SaintLucia","Saint Lucia",cleaz)
cleaz <- gsub("MexicoPR","Mexico",cleaz)
cleaz <- gsub("SouthAfrica","South Africa",cleaz)
cleaz <- countrycode(cleaz,"country.name","iso3c")
cleaz <- cleaz[!is.na(cleaz)]
cleaz <- sort(unique(cleaz))
# Overlap only
cntz <- cleaz[cleaz%in%cntz]
cntz <- cntz[!cntz%in%c("USA")]

# Sort by file size
cnt.size <- file.info(paste0("Input/GIS/Borders/GADM/",paste0(cntz,"_adm2.rds")))$size
cntz <- cntz[order(cnt.size)]
cntz


##############################################
##############################################
# Open loop (countries)
##############################################
##############################################

k0 <- 1; cntz[k0]; k <- k0
# k <- k0

# # Single core
# stopCluster(cl)
# lapply(k0:length(cntz),function(k){print(cntz[k])

# Linux/Mac
# stopCluster(cl)
mclapply(k0:length(cntz),function(k){print(cntz[k])
  
# # Windows
# foreach(k=k0:length(cntz),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(cntz[k])
#   lapply(list.of.packages, require, character.only = TRUE)
  
  
  filez.in <- dir("Output/Output_Weather")
  
  #############################
  #############################
  ## CLEA 
  #############################
  #############################
  
  # Only run if we have CLEA polygons for country
  if((cntz[k]%in%cleaz)&(!cntz[k]%in%c("RUS","USA"))){
    
    # Run only if file not already created
    if(!paste0("Weather_",cntz[k],"_clea.RData")%in%filez.in){
      
    print(paste(cntz[k],"clea"))
    
      # Load CLEA polygons
      cleaz.dir <- dir("Input/GIS/Borders/CLEA/Beta")
      cleaz.dir <- cleaz.dir[grep("GRED",cleaz.dir)]
      cleaz.dir0 <- sapply(strsplit(gsub("[0-9]|GRED_","",cleaz.dir),"_"),"[",1)
      cleaz.dir0 <- gsub("DominicanRepublic","Dominican Republic",cleaz.dir0)
      cleaz.dir0 <- gsub("SaintLucia","Saint Lucia",cleaz.dir0)
      cleaz.dir0 <- gsub("MexicoPR","Mexico",cleaz.dir0)
      cleaz.dir0 <- gsub("SouthAfrica","South Africa",cleaz.dir0)
      cleaz.dir <- cleaz.dir[countrycode(cleaz.dir0,"country.name","iso3c")%in%cntz[k]]
      cleaz.file <- gsub(".shp","",cleaz.dir[grep(".shp",cleaz.dir)])
      clea.yr <- sapply(strsplit(cleaz.file,"_"),"[",3)
      map.crop <- readShapePoly(paste0("Input/GIS/Borders/CLEA/Beta/",cleaz.dir))
      names(map.crop) <- paste0("CLEA_",toupper(names(map.crop)))
      clea.cst <- gsub(paste0("^",map.crop$CLEA_CTR[1],"0+"), "\\1", map.crop$CLEA_LINK, perl = TRUE)
      map.crop$CLEA_CST <- paste0(map.crop$CLEA_CTR[1],".",clea.yr,".",clea.cst)
      dim(map.crop)
      map0 <- map.crop
    
    # Load GADM crop layer
    if(!paste0(cntz[k],"_adm",0,".rds",sep="")%in%dir("Input/GIS/Borders/GADM/")){download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",0,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))}
    gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))
    proj4string(map0) <- proj4string(gadm)
    map0$ISO <- cntz[k]
    
    # Find space/time id's
    space.ag0 <- "clea"
    space.ix0 <- space.ix[match(space.ag0,space.agg)]
    
    # Extract data.frame
    map00 <- map0[,c("ISO",space.ix0)]
    map00.df <- map00@data
    
    
    ##########################
    ## Merge with weather data
    ##########################
    
    # Bounding box
    bbx <- paste("POLYGON((",bbox(map00)[1,1],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,1],"))")
    bbx <- readWKT(bbx)
    # plot(bbx,add=T)
    xrange <- which(round(long.alt)>=round(bbox(bbx)[1,1])&round(long.alt)<=round(bbox(bbx)[1,2]))
    yrange <- which(round(lat)>=round(bbox(bbx)[2,1])&round(lat)<=round(bbox(bbx)[2,2]))
    # xrange <- which(long.alt>bbox(bbx)[1,1]&long.alt<bbox(bbx)[1,2])
    # yrange <- which(lat>bbox(bbx)[2,1]&lat<bbox(bbx)[2,2])
    
    mean(long.alt==long.alt2)
    
    # Loop over times
    t <- 1000
    
    # Single core
    weather.list <- lapply(1:nrow(weather.times),function(t){# print(as.character(weather.times[t,"YRMO"]))
      
      # # Linux/Mac
      # stopCluster(cl)
      # weather.list <- mclapply(1:nrow(weather.times),function(t){# print(as.character(weather.times[t,"YRMO"]))
      
      # # Windows
      # weather.list <- foreach(t=1:nrow(weather.times),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {# print(as.character(weather.times[t,"YRMO"]))
      #   lapply(list.of.packages, require, character.only = TRUE)
      
      # Create empy matrix
      map00.0 <- map00.df
      map00.0$YRMO <- as.character(rain.times[t,"YRMO"])
      
      # Convert to data.frame
      sub.xdata <- xdata[xrange,yrange,t]
      sub.xdata2 <- xdata2[xrange,yrange,t]
      lonlat <- expand.grid(long.alt[xrange], lat[yrange])
      tmp.vec <- as.vector(sub.xdata)
      tmp.vec2 <- as.vector(sub.xdata2)
      xdata.df <- data.frame(cbind(lonlat, tmp.vec))
      xdata.df2 <- data.frame(cbind(lonlat, tmp.vec2))
      names(xdata.df) <- c("LONG", "LAT", paste0("TEMP_",as.character(weather.times[t,"YRMO"])))
      names(xdata.df2) <- c("LONG", "LAT", paste0("RAIN_",as.character(weather.times[t,"YRMO"])))
      # xdata.df <- na.omit(xdata.df)
      
      # Convert to raster
      coordinates(xdata.df) <- ~ LONG + LAT
      gridded(xdata.df) <- TRUE
      xdata.r <- raster(xdata.df)
      coordinates(xdata.df2) <- ~ LONG + LAT
      gridded(xdata.df2) <- TRUE
      xdata.r2 <- raster(xdata.df2)
      
      # Extract means
      head(xdata.df@data)
      head(xdata.df2@data)
      z <- extract(xdata.r,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
      map00.0$TEMP <- z
      z <- extract(xdata.r2,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
      map00.0$RAIN <- z
      
      # Store object
      map00.0
      
    # Close loop: countries (single core)
    })
    
    # # Close loop: countries (Linux/Mac)
    # },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # # Close loop (Windows)
    # }
    
    # Combine
    weather.mat <- do.call(rbind,weather.list)
    row.names(weather.mat) <- 1:nrow(weather.mat)
    head(weather.mat)
    tail(weather.mat)
    
    ## Save
    save(weather.mat,file=paste0("Output/Output_Weather/Weather_",cntz[k],"_clea.RData"))
    rm(weather.mat,weather.list)
    ## End if statement
    }
    
    # End CLEA
  }
  
  
  
  # #############################
  # #############################
  # ## PRIO GRID 
  # #############################
  # #############################
  # 
  # 
  # # Run only if file not already created
  # if(!paste0("Weather_",cntz[k],"_priogrid.RData")%in%filez.in){
  # 
  # # Load Prio Grid
  # #map0 <- readShapePoly("Data/PRIOGRID/priogrid_cell")
  # #save(map0,file="Data/PRIOGRID/PRIOGRID.RData")
  # #k=1
  # load("Input/GIS/Borders/PRIOGRID/PRIOGRID.RData")
  # #print("prio")
  # print(paste(cntz[k],"prio"))
  # 
  # # Load GADM crop layer
  # if(!paste0(cntz[k],"_adm",0,".rds",sep="")%in%dir("Input/GIS/Borders/GADM/")){download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",0,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))}
  # gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))
  # proj4string(map0) <- proj4string(gadm)
  # 
  # # Russia subset
  # if(cntz[k]%in%c("RUS")){
  #   gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",1,".rds",sep=""))
  #   reg.cauc <- c("Adygey","Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","Krasnodar","North Ossetia","Stavropol'")
  #   reg.skfo <- c("Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","North Ossetia","Stavropol'")
  #   reg.ufo <- c("Adygey","Astrakhan'","Kalmyk","Krasnodar","Rostov","Volgograd")
  #   gadm <- gadm[gadm$NAME_1%in%c(reg.cauc),]
  # }
  # 
  # # Crop by extent of map
  # coords0 <- as.data.frame(map0)[,c("xcoord","ycoord")]
  # sp.data0 <- SpatialPoints(coords0,proj4string=CRS(proj4string(gadm)))
  # sp.box <- as(extent(bbox(gadm)),"SpatialPolygons")
  # proj4string(sp.box) <- proj4string(gadm)
  # o <- over(map0, as(sp.box, "SpatialPolygons"))
  # map.crop <- map0[which(!is.na(o)),]
  # o <- over(map.crop, as(gadm, "SpatialPolygons"))
  # map.crop <- map.crop[which(!is.na(o)),]
  # names(map.crop) <- paste0("PRIO_",toupper(names(map.crop)))
  # map.crop$ISO <- cntz[k]
  # 
  # # Find space/time id's
  # space.ag0 <- "priogrid"
  # space.ix0 <- space.ix[match(space.ag0,space.agg)]
  # 
  # # Extract data.frame
  # map00 <- map.crop[,c("ISO",space.ix0)]
  # map00.df <- map00@data
  # 
  # 
  # ##########################
  # ## Merge with weather data
  # ##########################
  # 
  # # Bounding box
  # bbx <- paste("POLYGON((",bbox(map00)[1,1],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,1],"))")
  # bbx <- readWKT(bbx)
  # # plot(bbx,add=T)
  # xrange <- which(round(long.alt)>=round(bbox(bbx)[1,1])&round(long.alt)<=round(bbox(bbx)[1,2]))
  # yrange <- which(round(lat)>=round(bbox(bbx)[2,1])&round(lat)<=round(bbox(bbx)[2,2]))
  # # xrange <- which(long.alt>bbox(bbx)[1,1]&long.alt<bbox(bbx)[1,2])
  # # yrange <- which(lat>bbox(bbx)[2,1]&lat<bbox(bbx)[2,2])
  # 
  # mean(long.alt==long.alt2)
  # 
  # # Loop over times
  # t <- 1000
  # 
  #   # Single core
  #   weather.list <- lapply(1:nrow(weather.times),function(t){# print(as.character(weather.times[t,"YRMO"]))
  #   
  #   # # Linux/Mac
  #   # stopCluster(cl)
  #   # weather.list <- mclapply(1:nrow(weather.times),function(t){# print(as.character(weather.times[t,"YRMO"]))
  #   
  #   # # Windows
  #   # weather.list <- foreach(t=1:nrow(weather.times),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {# print(as.character(weather.times[t,"YRMO"]))
  #   #   lapply(list.of.packages, require, character.only = TRUE)
  #   
  #     # Create empy matrix
  #     map00.0 <- map00.df
  #     map00.0$YRMO <- as.character(rain.times[t,"YRMO"])
  #     
  #     # Convert to data.frame
  #     sub.xdata <- xdata[xrange,yrange,t]
  #     sub.xdata2 <- xdata2[xrange,yrange,t]
  #     lonlat <- expand.grid(long.alt[xrange], lat[yrange])
  #     tmp.vec <- as.vector(sub.xdata)
  #     tmp.vec2 <- as.vector(sub.xdata2)
  #     xdata.df <- data.frame(cbind(lonlat, tmp.vec))
  #     xdata.df2 <- data.frame(cbind(lonlat, tmp.vec2))
  #     names(xdata.df) <- c("LONG", "LAT", paste0("TEMP_",as.character(weather.times[t,"YRMO"])))
  #     names(xdata.df2) <- c("LONG", "LAT", paste0("RAIN_",as.character(weather.times[t,"YRMO"])))
  #     # xdata.df <- na.omit(xdata.df)
  #     
  #     # Convert to raster
  #     coordinates(xdata.df) <- ~ LONG + LAT
  #     gridded(xdata.df) <- TRUE
  #     xdata.r <- raster(xdata.df)
  #     coordinates(xdata.df2) <- ~ LONG + LAT
  #     gridded(xdata.df2) <- TRUE
  #     xdata.r2 <- raster(xdata.df2)
  #     
  #     # Extract means
  #     head(xdata.df@data)
  #     head(xdata.df2@data)
  #     z <- extract(xdata.r,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
  #     map00.0$TEMP <- z
  #     z <- extract(xdata.r2,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
  #     map00.0$RAIN <- z
  #     
  #     # Store object
  #     map00.0
  #     
  #   # Close loop: countries (single core)
  #   })
  #   
  #   # # Close loop: countries (Linux/Mac)
  #   # },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
  #   
  #   # # Close loop (Windows)
  #   # }
  # 
  #   # Combine
  #   weather.mat <- do.call(rbind,weather.list)
  #   row.names(weather.mat) <- 1:nrow(weather.mat)
  #   head(weather.mat)
  #   tail(weather.mat)
  # 
  #   ## Save
  #   save(weather.mat,file=paste0("Output/Output_Weather/Weather_",cntz[k],"_priogrid.RData"))
  #   rm(weather.mat,weather.list)
  #   ## End if statement
  # }
  # 
  # #############################
  # #############################
  # ## GADM
  # #############################
  # #############################
  # 
  # ## Open loop (adm)
  # a <- 1
  # for(a in 0:2){
  #   admz <- a
  #   #load(url(paste("http://biogeo.ucdavis.edu/data/gadm2/R/",cntz[k],"_adm",admz,".RData",sep="")))
  #   
  #   # Start adm2 exception
  #   if(!((a==1)&(cntz[k]%in%noadm1))){
  #     if(!((a==2)&(cntz[k]%in%noadm2))){
  #       
  #       # Run only if file not already created
  #       if(!paste0("Weather_",cntz[k],"_adm",admz,".RData")%in%filez.in){
  #       
  #         print(paste(cntz[k],a))  
  #         
  #         # Load borders
  #       if(!paste0(cntz[k],"_adm",admz,".rds",sep="")%in%dir("Input/GIS/Borders/GADM/")){      
  #         download.file(paste0("http://biogeo.ucdavis.edu/data/map2.8/rds/",cntz[k],"_adm",admz,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",admz,".rds",sep=""))
  #       }
  #       map00 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",admz,".rds",sep=""))
  #       # par(mar=rep(0,4)); plot(map00); #points(events[,c("LONG","LAT")],cex=.3,col="red")
  #       
  #       # Russia subset
  #       if(cntz[k]%in%c("RUS")&a!=0){
  #         gadm1 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",1,".rds",sep=""))
  #         reg.cauc <- c("Adygey","Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","Krasnodar","North Ossetia","Stavropol'")
  #         reg.skfo <- c("Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","North Ossetia","Stavropol'")
  #         reg.ufo <- c("Adygey","Astrakhan'","Kalmyk","Krasnodar","Rostov","Volgograd")
  #         map00 <- gadm1[gadm1$NAME_1%in%c(reg.cauc),]
  #       }
  #       if(cntz[k]%in%c("RUS")&a==0){
  #         gadm1 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",1,".rds",sep=""))
  #         reg.cauc <- c("Adygey","Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","Krasnodar","North Ossetia","Stavropol'")
  #         reg.skfo <- c("Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","North Ossetia","Stavropol'")
  #         reg.ufo <- c("Adygey","Astrakhan'","Kalmyk","Krasnodar","Rostov","Volgograd")
  #         gadm1 <- gadm1[gadm1$NAME_1%in%c(reg.cauc),]
  #         plot(gadm1)
  #         map00 <- crop(map00,gadm1)
  #       }        
  #       
  #       # Find space/time id's
  #       map00$ISO <- cntz[k]
  #       space.ag0 <- paste0("adm",admz)
  #       space.ix0 <- space.ix[match(space.ag0,space.agg)]
  #       
  #       # Extract data.frame
  #       map00 <- map00[,c("ISO",space.ix0)]
  #       map00.df <- map00@data
  #       
  #       head(map00)
  #       
  #       
  #       ##########################
  #       ## Merge with weather data
  #       ##########################
  #       
  #       # Bounding box
  #       bbx <- paste("POLYGON((",bbox(map00)[1,1],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,1],"))")
  #       bbx <- readWKT(bbx)
  #       # plot(bbx,add=T)
  #       xrange <- which(round(long.alt)>=round(bbox(bbx)[1,1])&round(long.alt)<=round(bbox(bbx)[1,2]))
  #       yrange <- which(round(lat)>=round(bbox(bbx)[2,1])&round(lat)<=round(bbox(bbx)[2,2]))
  #       # xrange <- which(long.alt>bbox(bbx)[1,1]&long.alt<bbox(bbx)[1,2])
  #       # yrange <- which(lat>bbox(bbx)[2,1]&lat<bbox(bbx)[2,2])
  #       
  #       mean(long.alt==long.alt2)
  #       
  #       # Loop over times
  #       t <- 1000
  #       
  #       # Single core
  #       weather.list <- lapply(1:nrow(weather.times),function(t){# print(as.character(weather.times[t,"YRMO"]))
  #       
  #       # # Linux/Mac
  #       # stopCluster(cl)
  #       # weather.list <- mclapply(1:nrow(weather.times),function(t){# print(as.character(weather.times[t,"YRMO"]))
  #       
  #       # # Windows
  #       # weather.list <- foreach(t=1:nrow(weather.times),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {# print(as.character(weather.times[t,"YRMO"]))
  #       #   lapply(list.of.packages, require, character.only = TRUE)
  #         
  #         # Create empy matrix
  #         map00.0 <- map00.df
  #         map00.0$YRMO <- as.character(rain.times[t,"YRMO"])
  #         
  #         # Convert to data.frame
  #         sub.xdata <- xdata[xrange,yrange,t]
  #         sub.xdata2 <- xdata2[xrange,yrange,t]
  #         lonlat <- expand.grid(long.alt[xrange], lat[yrange])
  #         tmp.vec <- as.vector(sub.xdata)
  #         tmp.vec2 <- as.vector(sub.xdata2)
  #         xdata.df <- data.frame(cbind(lonlat, tmp.vec))
  #         xdata.df2 <- data.frame(cbind(lonlat, tmp.vec2))
  #         names(xdata.df) <- c("LONG", "LAT", paste0("TEMP_",as.character(weather.times[t,"YRMO"])))
  #         names(xdata.df2) <- c("LONG", "LAT", paste0("RAIN_",as.character(weather.times[t,"YRMO"])))
  #         # xdata.df <- na.omit(xdata.df)
  #         
  #         # Convert to raster
  #         coordinates(xdata.df) <- ~ LONG + LAT
  #         gridded(xdata.df) <- TRUE
  #         xdata.r <- raster(xdata.df)
  #         coordinates(xdata.df2) <- ~ LONG + LAT
  #         gridded(xdata.df2) <- TRUE
  #         xdata.r2 <- raster(xdata.df2)
  #         
  #         # Extract means
  #         head(xdata.df@data)
  #         head(xdata.df2@data)
  #         z <- extract(xdata.r,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
  #         map00.0$TEMP <- z
  #         z <- extract(xdata.r2,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
  #         map00.0$RAIN <- z
  #         
  #         # Store object
  #         map00.0
  #         
  #       # Close loop: countries (single core)
  #       })
  #       
  #       # # Close loop: countries (Linux/Mac)
  #       # },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
  #         
  #       # # Close loop (Windows)
  #       # }
  #       # # stopCluster(cl)
  #       
  # 
  #       # Combine
  #       weather.mat <- do.call(rbind,weather.list)
  #       row.names(weather.mat) <- 1:nrow(weather.mat)
  #       head(weather.mat)
  #       tail(weather.mat)
  #       
  #       ## Save
  #       save(weather.mat,file=paste0("Output/Output_Weather/Weather_",cntz[k],"_adm",admz,".RData"))
  #       rm(weather.mat,weather.list)
  #       # Close if statement
  #       }
  #       
  #       # Close loop: adm exceptions
  #     }}
  #   
  #   # Close loop: adm
  # }
  
  
##############################################
##############################################
# Close loop 
##############################################
##############################################

  # filez.in <- dir("Output/Output_Weather")
  
# # Close loop: countries (single core)
# })
  
# Close loop: countries (Linux/Mac)
},mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)

# # Close loop (Windows)
# }
# stopCluster(cl)
