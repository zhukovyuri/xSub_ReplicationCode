rm(list=ls())

## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
if(grepl("w64",sessionInfo()[[1]][[1]])){setwd("D:/Dropbox (Zhukov research team)/XSub/Data/")}
# if(grepl("Ubuntu 18.04 LTS"
#          ,sessionInfo()[[4]])){setwd("/mnt/oldhdd/home/zhukov/Dropbox (Zhukov research team)/XSub/Data/")}

## Install & load packages (all at once)
list.of.packages <- c("maptools","classInt","raster","sp","rgeos","parallel","foreach","doParallel","countrycode","PBSmapping","mgcv","maps","rgdal")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)




######################################################################################
######################################################################################
######################################################################################
## Individual country maps (panel)
######################################################################################
######################################################################################
######################################################################################

rm(list=ls())

long2km <- function(lat){
  x.km <- c(0,85,111.321,85,0)
  x.lat <- c(-90,-40,0,40,90)
  x.lat2 <- x.lat^2
  deg2km.long <- glm(x.km~x.lat+x.lat2)
  predict(deg2km.long,newdata=data.frame(x.lat=lat,x.lat2=lat^2))
}

# Parallel computing
mcoptions <- list(preschedule=FALSE, set.seed=TRUE)
ncores <- detectCores()
# cl <- makeCluster(ncores)
# registerDoParallel(cl)

## Install & load packages (all at once)
list.of.packages <- c("maptools","classInt","raster","sp","rgeos","parallel","foreach","doParallel","countrycode","rgdal")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load data
srzz <- sort(unique(sapply(strsplit(dir("Upload/data_rdata_panel/"),"_"),"[",1)))
space.agg <- c("adm0","adm1","adm2","priogrid","clea")
time.agg <- c("year","month","week","day")
space.ix <- c("ID_0","ID_1","ID_2","PRIO_GID","CLEA_CST")
time.ix <- c("YEAR","YRMO","WID","TID")
source("Code/step3_aggregate/step3x_aggregate_admex.R")
s <- 4
k <- 1
a <- 3
filez <- dir("Upload/data_rdata_panel/")
filez <- filez[grep("_year.RData",filez)]

# Special subsets 
# filez <- filez[grep("RUS|NZL",filez)]

# Remaining only
if(grepl("Dropbox2",getwd())){
  filez <- filez[(!gsub("_year.RData","",filez)%in%gsub("map_|.png","",dir("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/maps_panel_small/")))|(!gsub("_year.RData","",filez)%in%gsub("map_|.png","",dir("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/maps_panel/")))]
}
if(grepl("w64",sessionInfo()[[1]][[1]])){
  filez <- filez[(!gsub("_year.RData","",filez)%in%gsub("map_|.png","",dir("D:/Dropbox (Zhukov research team)/XSub_upload/graphics/maps_panel_small/")))|(!gsub("_year.RData","",filez)%in%gsub("map_|.png","",dir("D:/Dropbox (Zhukov research team)/XSub_upload/graphics/maps_panel/")))]
}

if(length(filez)){

# filez <- filez[!grepl("priogrid",filez)]
cntz <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez),"_"),"[[",2)
admz <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez),"_"),"[[",3)

# Load world map
wrld_gadm <- rgdal::readOGR(dsn = "Input/GIS/Covariates/GADM_World/",layer = "gadm28_adm0",stringsAsFactors = FALSE)
# wrld_gadm <- readShapePoly("Input/GIS/Covariates/GADM_World/gadm28_adm0")
proj4string(wrld_gadm) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

}

# Loop over sources
# ## Single core
# for(s in 5:8){

# # Linux/Mac
# stopCluster(cl)
# mclapply(1:4,function(s){print(srzz[s])

# # Windows
# foreach(s=5:8,.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(cntz[k])
# lapply(list.of.packages, require, character.only = TRUE)

# ## Single core
# for(k in 1:length(cntz)){

# Linux/Mac
# stopCluster(cl)
filez
k <- grep("yzC",filez)[1]
k <- 94
filez[k]
# write.file <- 'R_progress.txt'
mclapply(1:length(filez),function(k){print(filez[k])
  # # monitor progress
  # file.create(write.file)
  # fileConn<-file(write.file)
  # writeLines(paste0(k,'/',length(filez),' ',round(k/length(filez),4)), fileConn)
  # close(fileConn)
  # # monitor from a console with
  # # tail -c +0 -f ~/Dropbox2/Dropbox\ \(Zhukov\ research\ team\)/XSub/Data/R_progress.txt

  # # Windows
  # foreach(k=1:length(cntz),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(cntz[k])
  # lapply(list.of.packages, require, character.only = TRUE)

  # print(paste0(filez[k]))

  data <- load(paste0("Upload/data_rdata_panel/",filez[k]))
  data <- get(data); rm(indata)
  a <- admz[k]
  a
  timz <- gsub("_|\\.RData","",sapply(strsplit(filez[k],a),"[[",2))
  timz
  srz <- gsub(paste0("_",cntz[k],"_",a,"_",timz,".RData"),"",filez[k])
  srz


  #load(url(paste("http://biogeo.ucdavis.edu/data/gadm2/R/",cntz[k],"_adm",admz,".RData",sep="")))

  # Start adm2 exception
  if(!((a=="adm1")&(cntz[k]%in%noadm1))){
    if(!((a=="adm2")&(cntz[k]%in%noadm2))){

      gadm0 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))

      # Load map
      if(grepl("adm",a)){
        gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_",a,".rds",sep=""))
      }

      # Russia subset
      if(grepl("yz",filez[k])&cntz[k]=="RUS"){
        gadm1 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",1,".rds",sep=""))
        reg.cauc <- c("Adygey","Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","Krasnodar","North Ossetia","Stavropol'")
        reg.skfo <- c("Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","North Ossetia","Stavropol'")
        reg.ufo <- c("Adygey","Astrakhan'","Kalmyk","Krasnodar","Rostov","Volgograd")
        gadm1 <- gadm1[gadm1$NAME_1%in%c(reg.cauc),]
        gadm0 <- crop(gadm0,gadm1)
        if(grepl("adm",a)){gadm <- crop(gadm,gadm1)}
      }

      # PRIO
      if(a=="priogrid"){
        load("Input/GIS/Borders/PRIOGRID/PRIOGRID.RData")
        proj4string(map0) <- proj4string(gadm0)
        coords0 <- as.data.frame(map0)[,c("xcoord","ycoord")]
        sp.data0 <- SpatialPoints(coords0,proj4string=CRS(proj4string(gadm0)))
        sp.box <- as(extent(bbox(gadm0)),"SpatialPolygons")
        proj4string(sp.box) <- proj4string(gadm0)
        o <- over(map0, as(sp.box, "SpatialPolygons"))
        map.crop <- map0[which(!is.na(o)),]
        o <- over(map.crop, as(gadm0, "SpatialPolygons"))
        map.crop <- map.crop[which(!is.na(o)),]
        names(map.crop) <- paste0("PRIO_",toupper(names(map.crop)))
        gadm <- map.crop;rm(map0,coords0,sp.data0,sp.box,o,map.crop)
      }

      # CLEA
      if(a=="clea"){
        # cnm <- countrycode(cntz[k], origin = "iso3c",destination = "country.name")
        # if(cnm%in%c("Russian Federation")){cnm <- "Russia"}
        # if(cnm%in%c("United Kingdom of Great Britain and Northern Ireland")){cnm <- "United Kingdom"}
        # cnm <- gsub(" ","",cnm)
        # # cnm <- gsub("DominicanRepublic","Dominican Republic",cnm)
        # # cnm <- gsub("SaintLucia","Saint Lucia",cnm)
        # # cnm <- gsub("MexicoPR","Mexico",cnm)
        # # cnm <- gsub("SouthAfrica","South Africa",cnm)
        # map0 <- readShapePoly(paste0("Input/GIS/Borders/CLEA/Beta/",gsub(".shp$","",clea.cnt)))
        cleaz.dir <- dir("Input/GIS/Borders/CLEA/Beta")
        cleaz.dir <- cleaz.dir[grep("GRED",cleaz.dir)]
        cleaz.dir0 <- sapply(strsplit(gsub("[0-9]|GRED_","",cleaz.dir),"_"),"[",1)
        cleaz.dir0 <- gsub("DominicanRepublic","Dominican Republic",cleaz.dir0)
        cleaz.dir0 <- gsub("SaintLucia","Saint Lucia",cleaz.dir0)
        cleaz.dir0 <- gsub("MexicoPR","Mexico",cleaz.dir0)
        cleaz.dir0 <- gsub("SouthAfrica","South Africa",cleaz.dir0)
        cleaz.dir <- cleaz.dir[countrycode(cleaz.dir0,"country.name","iso3c")%in%cntz[k]]
        cleaz.file <- gsub(".shp","",cleaz.dir[grep(".shp",cleaz.dir)])
        # clea.cnt <- dir("Input/GIS/Borders/CLEA/Beta/")[grepl(cnm,dir("Input/GIS/Borders/CLEA/Beta"))&grepl("^GRED_",dir("Input/GIS/Borders/CLEA/Beta"))&grepl(".shp$",dir("Input/GIS/Borders/CLEA/Beta"))]
        map0 <- readShapePoly(paste0("Input/GIS/Borders/CLEA/Beta/",cleaz.dir))
        if(diff(bbox(map0)[1,])<=361){
          proj4string(map0) <- proj4string(gadm0)
        }
        if(diff(bbox(map0)[1,])>361){
          proj4string(map0) <- CRS("+init=epsg:24877")
          map0 <- spTransform(map0,CRS(proj4string(gadm0)))
        }
        names(map0) <- paste0("CLEA_",toupper(names(map0)))
        clea.cst <- gsub(paste0("^",map0$CLEA_CTR[1],"0+"), "\\1", map0$CLEA_LINK, perl = TRUE)
        clea.yr <- sapply(strsplit(cleaz.file,"_"),"[",3)
        map0$CLEA_CST <- paste0(map0$CLEA_CTR[1],".",clea.yr,".",clea.cst)
        # Simplify polygons for select countries
        if(cntz[k]%in%c("USA","IRL","CAN")){
          map.sub <- map0
          size1 <- object.size(map.sub)
          map.sub0 <- gSimplify(map.sub,tol=.1,topologyPreserve = TRUE)
          map.sub0 <- gBuffer(map.sub0, byid=TRUE, width=0)
          map.val <- gIsValid(map.sub0,byid = TRUE,reason = TRUE)
          sum(gIsValid(map.sub0, byid=TRUE)==FALSE)
          map.sub0 <- SpatialPolygonsDataFrame(map.sub0,data=map.sub@data)
          size0 <- object.size(map.sub0)
          print(round(size0/size1,2))
          map0 <- map.sub0
          gadm0_simp <- gSimplify(gadm0,tol=.1,topologyPreserve = TRUE)
          gadm0_simp <- gBuffer(gadm0_simp, byid=TRUE, width=0)
          # map.val <- gIsValid(gadm0_simp,byid = TRUE,reason = TRUE)
          # sum(gIsValid(map.sub0, byid=TRUE)==FALSE)
          object.size(gadm0_simp)/object.size(gadm0)
          gadm0 <- gadm0_simp
        }
        coords0 <- coordinates(map0)
        sp.data0 <- SpatialPoints(coords0,proj4string=CRS(proj4string(gadm0)))
        sp.box <- as(extent(bbox(gadm0)),"SpatialPolygons")
        proj4string(sp.box) <- proj4string(gadm0)
        o <- over(map0, as(sp.box, "SpatialPolygons"))
        map.crop <- map0[which(!is.na(o)),]
        o <- over(map.crop, as(gadm0, "SpatialPolygons"))
        map.crop <- map.crop[which(!is.na(o)),]
        gadm <- map.crop;rm(map0,coords0,sp.data0,sp.box,o,map.crop)
      }

      drange <- paste0("(",range(as.character(data$YEAR),na.rm=T)[1],"-",range(as.character(data$YEAR),na.rm=T)[2],")")
      if(length(unique(data$YEAR))==1){drange <- paste0("(",unique(data$YEAR),")")}

      # Find level of analysis
      space.ag0 <- a
      time.ag0 <- "year"
      space.ix0 <- space.ix[match(space.ag0,space.agg)]
      time.ix0 <- time.ix[match(time.ag0,time.agg)]
      # print(filez[k])

      # Change projection for Russia
      if((!grepl("yz",filez[k]))&cntz[k]=="RUS"){
        newProj <- CRS("+proj=lcc +lat_1=30 +lat_2=62 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
        oldProj <- proj4string(gadm)
        if(!is.na(oldProj)){gadm <- spTransform(gadm, newProj)}
        if(is.na(oldProj)){proj4string(gadm) <- newProj}
        oldProj <- proj4string(gadm0)
        if(!is.na(oldProj)){gadm0 <- spTransform(gadm0, newProj)}
        if(is.na(oldProj)){proj4string(gadm0) <- newProj}
        oldProj <- proj4string(wrld_gadm)
        wrld_gadm <- data("wrld_simpl")
        wrld_gadm <- get(wrld_gadm)
        wrld_gadm <- crop(wrld_gadm,extent(-180,180,0,80))
        if(!is.na(oldProj)){wrld_gadm <- spTransform(wrld_gadm, newProj)}
        if(is.na(oldProj)){proj4string(wrld_gadm) <- newProj}
      }

      # Change projection for New Zealand
      if(cntz[k]=="NZL"){
        newProj <- CRS("+proj=utm +zone=60 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
        oldProj <- proj4string(gadm)
        if(!is.na(oldProj)){gadm <- spTransform(gadm, newProj)}
        if(is.na(oldProj)){proj4string(gadm) <- newProj}
        oldProj <- proj4string(gadm0)
        if(!is.na(oldProj)){gadm0 <- spTransform(gadm0, newProj)}
        if(is.na(oldProj)){proj4string(gadm0) <- newProj}
        oldProj <- proj4string(wrld_gadm)
        wrld_gadm <- data("wrld_simpl")
        wrld_gadm <- get(wrld_gadm)
        # wrld_gadm <- crop(wrld_gadm,extent(-180,180,0,80))
        # if(!is.na(oldProj)){wrld_gadm <- spTransform(wrld_gadm, newProj)}
        # if(is.na(oldProj)){proj4string(wrld_gadm) <- newProj}
        oldProj <- proj4string(gadm)
        if(!is.na(oldProj)){gadm <- spTransform(gadm, newProj)}
        if(is.na(oldProj)){proj4string(gadm) <- newProj}
      }
      
      # Create map layers
      agg.data <- aggregate(data[,c("SIDEA_ANY","SIDEB_ANY","ACTION_ANY")],by=list(UNIT=data[,space.ix0]),FUN=function(x){sum(x,na.rm=T)})
      head(agg.data)
      gadm <- merge(gadm,agg.data,by.x=space.ix0,by.y="UNIT",all.x=T,all.y=F)
      head(gadm)
      bb <- bbox(gadm0)

      if(cntz[k]%in%"USA"){
        gadm.backup <- gadm
        gadm1 <- readRDS("Input/GIS/Bo~/Dropbox2/rders/GADM/USA_adm1.rds")
        bb <- bbox(gadm1[which(!gadm1$NAME_1%in%c("Alaska","Hawaii")),])
        rm(gadm); gadm <- gadm.backup; rm(gadm.backup)
      }

      # Golden ratio
      bb[1,1] <- bb[1,1]-(bb[1,2]-bb[1,1])/16
      bb[1,2] <- bb[1,2]+(bb[1,2]-bb[1,1])/16
      bb[2,1] <- bb[2,1]-(bb[2,2]-bb[2,1])/16
      bb[2,2] <- bb[2,2]+(bb[2,2]-bb[2,1])/16
      # convert to km
      if((grepl("yz",filez[k]))|(!cntz[k]%in%c("RUS","NZL"))){
        y.dist.km <- (bb[2,2]-bb[2,1])*111
        x.dist.km <- (bb[1,2]-bb[1,1])*long2km(mean(bb[2,]))
        diff.x <- (y.dist.km*1.618)/long2km(mean(bb[2,]))
        diff.x0 <- diff.x - (x.dist.km)/long2km(mean(bb[2,]))
        if(diff.x0>0){
          bb[1,1] <- bb[1,1]-diff.x0/2
          bb[1,2] <- bb[1,2]+diff.x0/2
        }
        if(diff.x0<0){
          diff.y <- (x.dist.km/1.618)/111
          diff.y0 <- diff.y - (y.dist.km)/111
          bb[2,1] <- bb[2,1]-diff.y0/2
          bb[2,2] <- bb[2,2]+diff.y0/2
        }
      }
      if((!grepl("yz",filez[k]))&cntz[k]%in%c("RUS","NZL")){
        y.dist.km <- (bb[2,2]-bb[2,1])
        x.dist.km <- (bb[1,2]-bb[1,1])
        diff.x <- (y.dist.km*1.618)
        diff.x0 <- diff.x - (x.dist.km)
        if(diff.x0>0){
          bb[1,1] <- bb[1,1]-diff.x0/2
          bb[1,2] <- bb[1,2]+diff.x0/2
        }
        if(diff.x0<0){
          diff.y <- (x.dist.km/1.618)
          diff.y0 <- diff.y - (y.dist.km)
          bb[2,1] <- bb[2,1]-diff.y0/2
          bb[2,2] <- bb[2,2]+diff.y0/2
        }
      }
      WKTs <- paste("POLYGON((",bb[1,1],bb[2,1],",",bb[1,2],bb[2,1],",",bb[1,2],bb[2,2],",",bb[1,1],bb[2,2],",",bb[1,1],bb[2,1],"))")
      test <- readWKT(WKTs)
      set_ll_warn(TRUE)
      proj4string(test) <- proj4string(gadm)
      # data("wrld_simpl")
      # wrld_simpl <- spTransform(wrld_simpl,proj4string(gadm))
      # basemap <- crop(x = wrld_simpl,y=test)
      coords.wrld <- coordinates(wrld_gadm)
      r <- 1
      in.map <- c()
      for(r in 1:nrow(coords.wrld)){
        bb.1 <- bbox(wrld_gadm[r,])
        WKTs.1 <- paste("POLYGON((",bb.1[1,1],bb.1[2,1],",",bb.1[1,2],bb.1[2,1],",",bb.1[1,2],bb.1[2,2],",",bb.1[1,1],bb.1[2,2],",",bb.1[1,1],bb.1[2,1],"))")
        test.1 <- readWKT(WKTs.1)
        proj4string(test.1) <- proj4string(gadm)
        o.1 <- over(test,test.1)
        in.map <- c(in.map,ifelse(is.na(o.1),FALSE,TRUE))
      }
      basemap <- wrld_gadm[which(in.map),]
      if(length(basemap)==0){basemap <- gadm0}
      if(length(basemap)>0){
        if(!((!grepl("yz",filez[k]))&cntz[k]=="RUS")){
          basemap <- crop(x = basemap,y=test)
        }
        if((!grepl("yz",filez[k]))&cntz[k]=="RUS"){
          basemap <- SpatialPolygons2PolySet(basemap)
          basemap.clipped <- clipPolys(basemap, xlim = bbox(test)[1,], ylim = bbox(test)[2,])
          basemap <- PolySet2SpatialPolygons(basemap.clipped, close_polys=TRUE)
        }
      }
      if(cntz[k]%in%c("USA")){
        gadm <- crop(x = gadm,y=test)
      }

      # # Color
      # palz <- c("gray90","dodgerblue1","dodgerblue2")
      palz <- c("gray90","indianred1","indianred3")
      # palz <- c("gray90","coral1","coral3","coral4")
      # palz <- c("gray90","darkorchid1","darkorchid3","darkorchid4")
      res.palette <- colorRampPalette(palz, alpha=.25)
      cols.pal <- res.palette(5)
      # # B&W
      # cols.pal <- c("#CCCCCC", "#BCBCBC", "#ACACAC", "#9C9C9C", "#8C8C8C")

      v <- c(0, 1, 10, 50, 100, max(500,max(gadm$ACTION_ANY,na.rm=T)+1))
      if(grepl("yzChechnya|yzLibya",filez[k])){v <- c(0, 1, 10, 100, 1000, max(10000,max(gadm$ACTION_ANY,na.rm=T)+1))}
      if(grepl("yzCaucasus2000|yzUkraine2014|ESOCPakistanBFRS|ESOCIraq",filez[k])){v <- c(0, 10, 100, 1000, 10000, max(100000,max(gadm$ACTION_ANY,na.rm=T)+1))}
      if(grepl("ESOCMexico",filez[k])){v <- c(0, 1000, 5000, 100000, 500000, max(1000000,max(gadm$ACTION_ANY,na.rm=T)+1))}
      if((srz%in%c("ACLED","GED")&cntz[k]%in%c("SLE"))|(srz%in%c("GED")&cntz[k]%in%c("NPL"))){v <- c(0, 10, 100, 1000, 10000, max(100000,max(gadm$ACTION_ANY,na.rm=T)+1))}
      cols <- cols.pal[findInterval(gadm$ACTION_ANY, v)]
      # if(var(gadm$SIDEA_ANY)==0){
      #   cols <- rep("gray75",nrow(gadm))
      # }
      cname <- countrycode::countrycode(cntz[k],origin = "iso3c",destination = "country.name")
      if(cntz[k]%in%"COD"){cname <- "D.R.C."}
      if(cntz[k]%in%"IRN"){cname <- "Iran"}
      if(cntz[k]%in%"LAO"){cname <- "Laos"}
      if(cntz[k]%in%"TZA"){cname <- "Tanzania"}
      if(cntz[k]%in%"BOL"){cname <- "Bolivia"}
      if(cntz[k]%in%"RUS"){cname <- "Russia"}
      if(cntz[k]%in%"RUS"&grepl("yz",filez[k])){cname <- "Russia - N. Caucasus"}
      if(cntz[k]%in%"MKD"){cname <- "Macedonia (FYROM)"}
      if(cntz[k]%in%"PSE"){cname <- "Palestinian Ter."}
      if(cntz[k]%in%"SYR"){cname <- "Syria"}
      if(cntz[k]%in%"VEN"){cname <- "Venezuela"}
      if(cntz[k]%in%"VNM"){cname <- "Vietnam"}
      if(cntz[k]%in%"MDA"){cname <- "Moldova"}
      if(cntz[k]%in%"CAF"){cname <- "C.A.R."}
      if(cntz[k]%in%"GNQ"){cname <- "Eq. Guinea"}
      if(cntz[k]%in%"GMB"){cname <- "The Gambia"}
      if(cntz[k]%in%"GBR"){cname <- "United Kingdom"}
      if(cntz[k]%in%"USA"){cname <- "United States"}
      cname.xy <- coordinates(test)
      # cname.xy <- c(bbox(test)[1,2],bbox(test)[2,1])


      # Plot (thumbnail)
      # png(paste0("Upload/graphics/maps/map_",gsub("_year.RData","",filez[k]),".png"),height=3,width=3,units = "in",res=300)
      png(paste0("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/maps_panel_small/map_",gsub("_year.RData","",filez[k]),".png"),height=2,width=2*1.618,units = "in",res=100)
      # layout(matrix(c(rep(1,11),2), 12, 1, byrow = TRUE))
      par(mar=rep(0,4),xpd=NA)
      plot(test,col="lightgrey",border=NA,density=30,xlim=bbox(test)[1,],ylim=bbox(test)[2,])
      plot(test,col="lightgrey",border=NA,density=30,angle=-45,add=T)
      plot(basemap,add=T,col="gray95",border="gray60",lwd=.5)
      plot(gadm,col=cols,add=T,border="white",lwd=ifelse(a=="priogrid",ifelse(cntz[k]%in%"RUS"&(!grepl("yz",filez[k])),.01,.05),ifelse(a=="adm2",ifelse(cntz[k]%in%"RUS",.01,.02),.05)))
      plot(gadm0,col=NA,border=rgb(0.1,0.1,0.1,.25),lwd=2,add=T)
      #text(x=cname.xy[1],y=cname.xy[2],labels=paste(cname,drange,sep="\n"),cex=1.5,font=2)
      text(x=cname.xy[1],y=cname.xy[2],labels=cname,cex=1.5,font=2)
      # par(mar=rep(0,4))
      # plot.new()
      # legend(x="top",fill=cols.pal,legend=paste0(c("","<","<","<",">"),v[c(1:4,4)]),bty="n",ncol=5,cex=.9,border=NA)
      dev.off()

      # Plot
      # png(paste0("Upload/graphics/maps/map_",gsub("_year.RData","",filez[k]),".png"),height=3,width=3,units = "in",res=300)
      png(paste0("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/maps_panel/map_",gsub("_year.RData","",filez[k]),".png"),height=3,width=3*1.618,units = "in",res=300)
      # layout(matrix(c(rep(1,11),2), 12, 1, byrow = TRUE))
      par(mar=c(1,0,0,0),xpd=NA)
      plot(test,col="lightgrey",border=NA,density=30,xlim=bbox(test)[1,],ylim=bbox(test)[2,])
      plot(test,col="lightgrey",border="black",density=30,angle=-45,add=T)
      plot(basemap,add=T,col="gray95",border="gray60",lwd=.5)
      plot(gadm,col=cols,add=T,border="white",lwd=ifelse(a=="priogrid",ifelse(cntz[k]%in%"RUS"&(!grepl("yz",filez[k])),.05,.35),ifelse(a=="adm2",ifelse(cntz[k]%in%"RUS",.1,.2),.5)))
      plot(gadm0,col=NA,border=rgb(0.1,0.1,0.1,.25),lwd=2,add=T)
      #text(x=cname.xy[1],y=cname.xy[2],labels=paste(cname,drange,sep="\n"),cex=1.5,font=2)
      text(x=cname.xy[1],y=cname.xy[2],labels=cname,cex=2,font=2)
      # par(mar=rep(0,4))
      par(xpd=TRUE)
      # plot(0,0,col=NA,xlim=bbox(test)[1,],ylim=bbox(test)[2,],xaxt="n",yaxt="n",bty="n")
      x.lab <- seq(bbox(test)[1,1],bbox(test)[1,2],length.out=6)
      y.lab <- bbox(test)[2,1]
      rect(xleft = x.lab[-6],xright = x.lab[-1],ybottom = rep(bbox(test)[2,1]-(bbox(test)[2,2]-bbox(test)[2,1])/16,5),ytop = rep(bbox(test)[2,1],5),col=cols.pal,border="black",lwd=1)
      text(x=x.lab[-6]+diff(x.lab)[1]/2,y=rep(bbox(test)[2,1]-(bbox(test)[2,2]-bbox(test)[2,1])/32),labels=paste0(c("","<","<","<",">"),v[c(1:4,4)]),cex=.8)
      dev.off()

    }}
  # Close a loop
  # plot(gadm0)

  # # Close loop (single core)
  # }

  # Close loop: countries (Linux/Mac)
},mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)

# # Close loop (Windows)
# }
# stopCluster(cl)




######################################################################################
######################################################################################
######################################################################################
## Individual country maps (events)
######################################################################################
######################################################################################
######################################################################################

rm(list=ls())

long2km <- function(lat){
  x.km <- c(0,85,111.321,85,0)
  x.lat <- c(-90,-40,0,40,90)
  x.lat2 <- x.lat^2
  deg2km.long <- glm(x.km~x.lat+x.lat2)
  predict(deg2km.long,newdata=data.frame(x.lat=lat,x.lat2=lat^2))
}


# Parallel computing
mcoptions <- list(preschedule=FALSE, set.seed=TRUE)
ncores <- detectCores()
# cl <- makeCluster(ncores)
# registerDoParallel(cl)

## Install & load packages (all at once)
list.of.packages <- c("maptools","classInt","raster","sp","rgeos","parallel","foreach","doParallel","countrycode")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load data
srzz <- sort(unique(sapply(strsplit(dir("Upload/data_rdata_event/"),"_"),"[",1)))
source("Code/step3_aggregate/step3x_aggregate_admex.R")
s <- 4
k <- 1
a <- 3
filez <- dir("Upload/data_rdata_event/")

# Special subsets 
# filez <- filez[grep("RUS|NZL",filez)]

# Remaining only
if(grepl("Dropbox2",getwd())){
  filez <- filez[(!gsub(".RData","",filez)%in%gsub("map_|.png","",dir("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/maps_event_small/")))|(!gsub(".RData","",filez)%in%gsub("map_|.png","",dir("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/maps_event/")))]
}
if(grepl("w64",sessionInfo()[[1]][[1]])){
  filez <- filez[(!gsub(".RData","",filez)%in%gsub("map_|.png","",dir("D:/Dropbox (Zhukov research team)/XSub_upload/graphics/maps_event_small/")))|(!gsub(".RData","",filez)%in%gsub("map_|.png","",dir("D:/Dropbox (Zhukov research team)/XSub_upload/graphics/maps_event/")))]
}



# filez <- filez[!grepl("priogrid",filez)]
cntz <- gsub(".RData","",sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez),"_"),"[[",2))

if(length(filez)){
# Load world map
wrld_gadm <- rgdal::readOGR("Input/GIS/Covariates/GADM_World/","gadm28_adm0",stringsAsFactors = FALSE)
proj4string(wrld_gadm) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
}

# Loop over sources
# ## Single core
# for(s in 5:8){

# # Linux/Mac
# stopCluster(cl)
# mclapply(1:4,function(s){print(srzz[s])

# # Windows
# foreach(s=5:8,.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(cntz[k])
# lapply(list.of.packages, require, character.only = TRUE)

# ## Single core
# for(k in 1:length(cntz)){

# Linux/Mac
# stopCluster(cl)
k <- 1
k <- which(cntz=="RUS")[2]
filez[k]
# write.file <- 'R_progress.txt'
mclapply(1:length(filez),function(k){print(filez[k])
  # # monitor progress
  # file.create(write.file)
  # fileConn<-file(write.file)
  # writeLines(paste0(k,'/',length(filez),' ',round(k/length(filez),4)), fileConn)
  # close(fileConn)
  # # monitor from a console with
  # # tail -c +0 -f ~/Dropbox2/Dropbox\ \(Zhukov\ research\ team\)/XSub/Data/R_progress.txt

  # # Windows
  # foreach(k=1:length(cntz),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(cntz[k])
  # lapply(list.of.packages, require, character.only = TRUE)

  # print(paste0(filez[k]))

  data <- load(paste0("Upload/data_rdata_event/",filez[k]))
  data <- get(data); rm(indata)
  cntz[k]

  gadm0 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))

  # # Load map
  a <- ifelse(cntz[k]%in%noadm1,0,1)
  gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",a,".rds",sep=""))

  # Spatial points
  data <- data[which((!is.na(data$LONG))&(!is.na(data$LAT))),]
  if(nrow(data)>0){
    sp.events <- SpatialPoints(coords = data.frame(LONG=as.numeric(as.character(data$LONG)),LAT=as.numeric(as.character(data$LAT))),proj4string = CRS(proj4string(gadm0)))
  }
  if(nrow(data)==0){
    sp.events <- SpatialPoints(coords = data.frame(LONG=0,LAT=0),proj4string = CRS(proj4string(gadm0)))
  }

  # Russia subset
  if(grepl("yz",filez[k])&cntz[k]=="RUS"){
    gadm1 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",1,".rds",sep=""))
    reg.cauc <- c("Adygey","Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","Krasnodar","North Ossetia","Stavropol'")
    reg.skfo <- c("Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","North Ossetia","Stavropol'")
    reg.ufo <- c("Adygey","Astrakhan'","Kalmyk","Krasnodar","Rostov","Volgograd")
    gadm1 <- gadm1[gadm1$NAME_1%in%c(reg.cauc),]
    gadm0 <- crop(gadm0,gadm1)
    gadm <- crop(gadm,gadm1)
  }

  # print(filez[k])

  # Change projection for Russia
  if((!grepl("yz",filez[k]))&cntz[k]=="RUS"){
    newProj <- CRS("+proj=lcc +lat_1=30 +lat_2=62 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
    oldProj <- proj4string(gadm)
    if(!is.na(oldProj)){gadm <- spTransform(gadm, newProj)}
    if(is.na(oldProj)){proj4string(gadm) <- newProj}
    oldProj <- proj4string(gadm0)
    if(!is.na(oldProj)){gadm0 <- spTransform(gadm0, newProj)}
    if(is.na(oldProj)){proj4string(gadm0) <- newProj}
    oldProj <- proj4string(wrld_gadm)
    wrld_gadm <- data("wrld_simpl")
    wrld_gadm <- get(wrld_gadm)
    wrld_gadm <- crop(wrld_gadm,extent(-180,180,0,80))
    if(!is.na(oldProj)){wrld_gadm <- spTransform(wrld_gadm, newProj)}
    if(is.na(oldProj)){proj4string(wrld_gadm) <- newProj}
    oldProj <- proj4string(sp.events)
    if(!is.na(oldProj)){sp.events <- spTransform(sp.events, newProj)}
    if(is.na(oldProj)){proj4string(sp.events) <- newProj}
  }
  
  # Change projection for New Zealand
  if(cntz[k]=="NZL"){
    newProj <- CRS("+proj=utm +zone=60 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
    oldProj <- proj4string(gadm)
    if(!is.na(oldProj)){gadm <- spTransform(gadm, newProj)}
    if(is.na(oldProj)){proj4string(gadm) <- newProj}
    oldProj <- proj4string(gadm0)
    if(!is.na(oldProj)){gadm0 <- spTransform(gadm0, newProj)}
    if(is.na(oldProj)){proj4string(gadm0) <- newProj}
    oldProj <- proj4string(wrld_gadm)
    wrld_gadm <- data("wrld_simpl")
    wrld_gadm <- get(wrld_gadm)
    # wrld_gadm <- crop(wrld_gadm,extent(-180,180,0,80))
    # if(!is.na(oldProj)){wrld_gadm <- spTransform(wrld_gadm, newProj)}
    # if(is.na(oldProj)){proj4string(wrld_gadm) <- newProj}
    oldProj <- proj4string(sp.events)
    if(!is.na(oldProj)){sp.events <- spTransform(sp.events, newProj)}
    if(is.na(oldProj)){proj4string(sp.events) <- newProj}
  }

  # Create map layers
  head(gadm)
  bb <- bbox(gadm0)
  if(cntz[k]%in%"USA"){
    gadm.backup <- gadm
    gadm1 <- readRDS("Input/GIS/Borders/GADM/USA_adm1.rds")
    bb <- bbox(gadm1[which(!gadm1$NAME_1%in%c("Alaska","Hawaii")),])
    rm(gadm); gadm <- gadm.backup; rm(gadm.backup)
  }

  # Golden ratio
  bb[1,1] <- bb[1,1]-(bb[1,2]-bb[1,1])/16
  bb[1,2] <- bb[1,2]+(bb[1,2]-bb[1,1])/16
  bb[2,1] <- bb[2,1]-(bb[2,2]-bb[2,1])/16
  bb[2,2] <- bb[2,2]+(bb[2,2]-bb[2,1])/16
  # convert to km
  if((grepl("yz",filez[k]))|(!cntz[k]%in%c("RUS","NZL"))){
    y.dist.km <- (bb[2,2]-bb[2,1])*111
    x.dist.km <- (bb[1,2]-bb[1,1])*long2km(mean(bb[2,]))
    diff.x <- (y.dist.km*1.618)/long2km(mean(bb[2,]))
    diff.x0 <- diff.x - (x.dist.km)/long2km(mean(bb[2,]))
    if(diff.x0>0){
      bb[1,1] <- bb[1,1]-diff.x0/2
      bb[1,2] <- bb[1,2]+diff.x0/2
    }
    if(diff.x0<0){
      diff.y <- (x.dist.km/1.618)/111
      diff.y0 <- diff.y - (y.dist.km)/111
      bb[2,1] <- bb[2,1]-diff.y0/2
      bb[2,2] <- bb[2,2]+diff.y0/2
    }
  }
  if((!grepl("yz",filez[k]))&cntz[k]%in%c("RUS","NZL")){
    y.dist.km <- (bb[2,2]-bb[2,1])
    x.dist.km <- (bb[1,2]-bb[1,1])
    diff.x <- (y.dist.km*1.618)
    diff.x0 <- diff.x - (x.dist.km)
    if(diff.x0>0){
      bb[1,1] <- bb[1,1]-diff.x0/2
      bb[1,2] <- bb[1,2]+diff.x0/2
    }
    if(diff.x0<0){
      diff.y <- (x.dist.km/1.618)
      diff.y0 <- diff.y - (y.dist.km)
      bb[2,1] <- bb[2,1]-diff.y0/2
      bb[2,2] <- bb[2,2]+diff.y0/2
    }
  }
  WKTs <- paste("POLYGON((",bb[1,1],bb[2,1],",",bb[1,2],bb[2,1],",",bb[1,2],bb[2,2],",",bb[1,1],bb[2,2],",",bb[1,1],bb[2,1],"))")
  test <- readWKT(WKTs)
  set_ll_warn(TRUE)
  proj4string(test) <- proj4string(gadm)
  # data("wrld_simpl")
  # wrld_simpl <- spTransform(wrld_simpl,proj4string(gadm))
  # basemap <- crop(x = wrld_simpl,y=test)
  coords.wrld <- coordinates(wrld_gadm)
  r <- 1
  in.map <- c()
  for(r in 1:nrow(coords.wrld)){
    bb.1 <- bbox(wrld_gadm[r,])
    WKTs.1 <- paste("POLYGON((",bb.1[1,1],bb.1[2,1],",",bb.1[1,2],bb.1[2,1],",",bb.1[1,2],bb.1[2,2],",",bb.1[1,1],bb.1[2,2],",",bb.1[1,1],bb.1[2,1],"))")
    test.1 <- readWKT(WKTs.1)
    proj4string(test.1) <- proj4string(gadm)
    o.1 <- over(test,test.1)
    in.map <- c(in.map,ifelse(is.na(o.1),FALSE,TRUE))
  }
  basemap <- wrld_gadm[which(in.map),]
  if(length(basemap)==0){basemap <- gadm0}
  if(!((!grepl("yz",filez[k]))&cntz[k]=="RUS")){
    basemap <- crop(x = basemap,y=test)
  }
  if((!grepl("yz",filez[k]))&cntz[k]=="RUS"){
    basemap <- SpatialPolygons2PolySet(basemap)
    basemap.clipped <- clipPolys(basemap, xlim = bbox(test)[1,], ylim = bbox(test)[2,])
    basemap <- PolySet2SpatialPolygons(basemap.clipped, close_polys=TRUE)
  }

  # Crop points by base layer
  sp.events <- sp.events[test]


  # # Color
  # palz <- c("gray90","dodgerblue1","dodgerblue2")
  palz <- c("gray90","indianred1","indianred2")
  res.palette <- colorRampPalette(palz, alpha=.25)
  cols.pal <- res.palette(5)
  cols <- rgb(col2rgb("indianred1")[1]/255,col2rgb("indianred1")[2]/255,col2rgb("indianred1")[3]/255,alpha=.5)

  # if(var(gadm$SIDEA_ANY)==0){
  #   cols <- rep("gray75",nrow(gadm))
  # }
  cname <- countrycode::countrycode(cntz[k],origin = "iso3c",destination = "country.name")
  if(cntz[k]%in%"COD"){cname <- "D.R.C."}
  if(cntz[k]%in%"IRN"){cname <- "Iran"}
  if(cntz[k]%in%"LAO"){cname <- "Laos"}
  if(cntz[k]%in%"TZA"){cname <- "Tanzania"}
  if(cntz[k]%in%"BOL"){cname <- "Bolivia"}
  if(cntz[k]%in%"RUS"){cname <- "Russia"}
  if(cntz[k]%in%"RUS"&grepl("yz",filez[k])){cname <- "Russia - N. Caucasus"}
  if(cntz[k]%in%"MKD"){cname <- "Macedonia (FYROM)"}
  if(cntz[k]%in%"PSE"){cname <- "Palestinian Ter."}
  if(cntz[k]%in%"SYR"){cname <- "Syria"}
  if(cntz[k]%in%"VEN"){cname <- "Venezuela"}
  if(cntz[k]%in%"VNM"){cname <- "Vietnam"}
  if(cntz[k]%in%"MDA"){cname <- "Moldova"}
  if(cntz[k]%in%"CAF"){cname <- "C.A.R."}
  if(cntz[k]%in%"GNQ"){cname <- "Eq. Guinea"}
  if(cntz[k]%in%"GMB"){cname <- "The Gambia"}
  if(cntz[k]%in%"GBR"){cname <- "United Kingdom"}
  if(cntz[k]%in%"USA"){cname <- "United States"}
  cname.xy <- coordinates(test)
  # cname.xy <- c(bbox(test)[1,2],bbox(test)[2,1])

  # Plot (thumbnail)
  # png(paste0("Upload/graphics/maps/map_",gsub("_year.RData","",filez[k]),".png"),height=3,width=3,units = "in",res=300)
  png(paste0("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/maps_event_small/map_",gsub(".RData","",filez[k]),".png"),height=2,width=2*1.618,units = "in",res=100)
  # layout(matrix(c(rep(1,11),2), 12, 1, byrow = TRUE))
  par(mar=c(0,0,0,0),xpd=NA)
  plot(test,col="lightgrey",border=NA,density=30,xlim=bbox(test)[1,],ylim=bbox(test)[2,])
  plot(test,col="lightgrey",border=NA,density=30,angle=-45,add=T)
  plot(basemap,add=T,col="gray95",border="gray60",lwd=.5)
  plot(gadm0,col=cols.pal[1],border=rgb(0.1,0.1,0.1,.25),lwd=2,add=T)
  points(sp.events,col=cols,pch=16,cex=.7)
  points(sp.events,col=rgb(.75,.3,.3,.2),pch=1,cex=.8)
  text(x=cname.xy[1],y=cname.xy[2],labels=cname,cex=1.5,font=2)
  dev.off()

  # Plot
  # png(paste0("Upload/graphics/maps/map_",gsub("_year.RData","",filez[k]),".png"),height=3,width=3,units = "in",res=300)
  png(paste0("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/maps_event/map_",gsub(".RData","",filez[k]),".png"),height=3,width=3*1.618,units = "in",res=300)
  # layout(matrix(c(rep(1,11),2), 12, 1, byrow = TRUE))
  par(mar=c(1,0,0,0),xpd=NA)
  plot(test,col="lightgrey",border=NA,density=30,xlim=bbox(test)[1,],ylim=bbox(test)[2,])
  plot(test,col="lightgrey",border="black",density=30,angle=-45,add=T)
  plot(basemap,add=T,col="gray95",border="gray60",lwd=.5)
  plot(gadm0,col=cols.pal[1],border=rgb(0.1,0.1,0.1,.25),lwd=2,add=T)
  points(sp.events,col=cols,pch=16,cex=.7)
  points(sp.events,col=rgb(.75,.3,.3,.2),pch=1,cex=.8)
  #text(x=cname.xy[1],y=cname.xy[2],labels=paste(cname,drange,sep="\n"),cex=1.5,font=2)
  text(x=cname.xy[1],y=cname.xy[2],labels=cname,cex=2,font=2)
  # par(mar=rep(0,4))
  par(xpd=TRUE)
  # plot(0,0,col=NA,xlim=bbox(test)[1,],ylim=bbox(test)[2,],xaxt="n",yaxt="n",bty="n")
  x.lab <- seq(bbox(test)[1,1],bbox(test)[1,2],length.out=2)
  y.lab <- bbox(test)[2,1]
  rect(xleft = x.lab[1],xright = x.lab[2],ybottom = bbox(test)[2,1]-(bbox(test)[2,2]-bbox(test)[2,1])/16,ytop = bbox(test)[2,1],col=cols.pal[4],border="black",lwd=1)

  text(x=x.lab[1]+diff(x.lab)[1]/2,y=rep(bbox(test)[2,1]-(bbox(test)[2,2]-bbox(test)[2,1])/32),labels=paste0(formatC(nrow(data), decimal.mark=".", big.mark=",", digits = 0, format = "f"), " events"),cex=.8)
  dev.off()

# # Close adm exceptions
# }}


  # Close a loop
  # plot(gadm0)

  # # Close loop (single core)
  # }

  # Close loop: countries (Linux/Mac)
},mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)

# # Close loop (Windows)
# }
# stopCluster(cl)





######################################################################################
######################################################################################
######################################################################################
## Individual country timelines
######################################################################################
######################################################################################
######################################################################################


rm(list=ls())

# Parallel computing
mcoptions <- list(preschedule=FALSE, set.seed=TRUE)
ncores <- detectCores()
# cl <- makeCluster(ncores)
# registerDoParallel(cl)

## Install & load packages (all at once)
list.of.packages <- c("maptools","classInt","raster","sp","rgeos","parallel","foreach","doParallel","countrycode")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load data
srzz <- sort(unique(sapply(strsplit(dir("Upload/data_rdata_event/"),"_"),"[",1)))
source("Code/step3_aggregate/step3x_aggregate_admex.R")
space.agg <- c("adm0","adm1","adm2","priogrid","clea")
time.agg <- c("year","month","week","day")
space.ix <- c("ID_0","ID_1","ID_2","PRIO_GID","CLEA_CST")
time.ix <- c("YEAR","YRMO","WID","TID")
s <- 4
k <- 1
a <- 3
filez <- dir("Upload/data_rdata_panel/")
filez <- filez[grep("adm0",filez)]

# # Only remaining
filez <- filez[(!gsub("adm0_|.RData","",filez)%in%gsub("time_|.png","",dir("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/time_panel/")))|(!gsub("adm0_|.RData","",filez)%in%gsub("time_|.png","",dir("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/time_panel_small/")))]

cntz <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez),"_"),"[[",2)
admz <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez),"_"),"[[",3)


# Linux/Mac
# stopCluster(cl)
k <- 13
k <- grep("ISR",filez)[1]
filez[k]
write.file <- 'R_progress2.txt'
mclapply(1:length(filez),function(k){print(filez[k])
  # monitor progress
  file.create(write.file)
  fileConn<-file(write.file)
  writeLines(paste0(k,'/',length(filez),' ',round(k/length(filez),4)), fileConn)
  close(fileConn)
  # monitor from a console with
  # tail -c +0 -f ~/Dropbox2/Dropbox\ \(Zhukov\ research\ team\)/XSub/Data/R_progress2.txt

  # # Windows
  # foreach(k=1:length(cntz),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(cntz[k])
  # lapply(list.of.packages, require, character.only = TRUE)

  # print(paste0(filez[k]))

  data <- load(paste0("Upload/data_rdata_panel/",filez[k]))
  data <- get(data); rm(indata)
  cntz[k]
  a <- admz[k]
  a
  timz <- gsub("_|\\.RData","",sapply(strsplit(filez[k],a),"[[",2))
  timz
  srz <- gsub(paste0("_",cntz[k],"_",a,"_",timz,".RData"),"",filez[k])
  srz

  # # Color
  # palz <- c("gray90","dodgerblue1","dodgerblue2")
  palz <- c("gray90","indianred1","indianred3")

  summary(data)
  
  # Ticker
  max.date <- max(gsub("-","",Sys.Date()),gsub("-","","2019-01-01"))
  datez <- seq(as.Date("1900-01-01"), as.Date(paste0(substr(max.date,1,4),"-",substr(max.date,5,6),"-",substr(max.date,7,8))), by="days")
  ticker <- data.frame(DATE=gsub("-","",datez),TID=1:length(datez),WID=rep(1:length(datez),each=7)[1:length(datez)],YRMO=substr(gsub("-","",datez),1,6),YEAR=substr(gsub("-","",datez),1,4))
  head(ticker)
  ticker.labs <- rep("",nrow(ticker))
  ticker.labs[grep("01$",ticker$DATE)] <- gsub("01$","",ticker[grep("01$",ticker$DATE),"YRMO"])
  ticker.labs
  ticker.ticz <- which(ticker.labs!="")
  ticker$LABS <- ticker.labs
  time.ix0 <- time.ix[match(timz,time.agg)]
  if(time.ix0=="TID"){time.ix0 <- "DATE"}
  ticker$TIME_ID <- as.numeric(as.character(ticker[,time.ix0]))
  ticker$YRMO <- as.numeric(as.character(ticker[,"YRMO"]))

  # Merge
  data0 <- data
  if(timz%in%"week"){data0$WID <- data0$WID+ticker[ticker[,"YRMO"]%in%data0[1,"YRMO"],"WID"][1] - data0$WID[1]}
  data0[,time.ix0] <- as.numeric(as.character(data0[,time.ix0]))
  indata.t <- aggregate(data.frame(ACTION_ANY=data0[,"ACTION_ANY"]),by=list(TIME_ID=data0[,time.ix0]),function(x){sum(x,na.rm=T)})
  indata.t <- merge(ticker,indata.t,by="TIME_ID",all.x=T,all.y=F)
  indata.t[is.na(indata.t$ACTION_ANY),"ACTION_ANY"] <- 0
  indata.t$TIME_ID <- as.numeric(as.character(indata.t$TIME_ID))
  indata.t$LABS <- as.character(indata.t$LABS)
  indata.t <- indata.t[order(indata.t$DATE),]
  indata.t <- indata.t[!duplicated(indata.t$TIME_ID,fromLast = FALSE),]
  head(indata.t)
  summary(indata.t)

  # Date range
  minyr <- as.numeric(as.character(indata.t[which(indata.t$ACTION_ANY>0)[1],"YEAR"]))
  if(!is.na(minyr)){
    if(minyr>=1989){yrm <- 198812}
    if(minyr>=1979&minyr<1989){yrm <- 197812}
    if(minyr>=1969&minyr<1979){yrm <- 196812}
    if(minyr>=1959&minyr<1969){yrm <- 195812}
    if(minyr>=1949&minyr<1959){yrm <- 194812}
    if(minyr>=1939&minyr<1949){yrm <- 193812}
  }
  if(is.na(minyr)){
    minyr <- 1989; yrm <- 198812
  }
  # ## PLOT

  # start-finish
  dat.events <- which(indata.t$ACTION_ANY>0)
  padz <- 8*c(1,12,52,365)[match(timz,c("year","month","week","day"))]
  X <- indata.t[indata.t$YRMO>=indata.t$YRMO[max(1,-padz+dat.events[1])]&indata.t$YRMO<=indata.t$YRMO[min(nrow(indata.t), padz+max(dat.events))],]
  if(nrow(na.omit(X))>0){ylimz <- range(X$ACTION_ANY,na.rm=T); l.j <- pretty(range(X$ACTION_ANY,na.rm=T))}
  if(nrow(na.omit(X))==0){X <- indata.t[indata.t$YRMO>198812,]; ylimz <- l.j <- 0:1}
  
  X$TID0 <- 1:nrow(X)
  
  # FIle names
  plot.name <- c(paste0("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/time_panel/time_",srz,"_",cntz[k],"_",timz,".png"),paste0("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/time_event/time_",srz,"_",cntz[k],"_event.png"))
  pix <- 1
  if(timz%in%"day"){pix <- 1:2}
  for(i in pix){
  png(plot.name[i],height=3,width=13,units="in",res=300)
  par(mar=c(2,3,0.5,0.5))
  plot(x=X$TID0,y=X$ACTION_ANY,ylim=ylimz,xaxt="n",yaxt="n",bty="n",col=NA)
  axis(1,at =  which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),labels=NA)
  axis(1,at=which(grepl("001$|501$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),labels=substr(X$YRMO,1,4)[which(grepl("001$|501$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE)))],lwd=0,lwd.ticks=2)
  # axis(1,at=grep("00101$|50101$",X$DATE),labels=substr(X$YRMO,1,4)[grep("00101$|50101$",X$DATE)],lwd=0,lwd.ticks=2)
  l.i <- pretty(which(X$LABS!=""))
  l.i <- l.i[l.i<=nrow(X)]
  l.j <- l.j[l.j%%1==0]
  # axis(1,at = l.i,labels=substr(X$YRMO,1,4)[l.i+1],tick = FALSE,cex=.8)
  axis(2,at = l.j,las=1,cex.axis=.8)
  rect(xleft = X$TID0[1]-1,xright = X$TID0[length(X$TID0)]+1,ybottom = l.j[1],ytop = l.j[length(l.j)],col = "gray90",border = NA)
  abline(h=l.j,col="white",lwd=1)
  abline(h=l.j+diff(l.j)[1]/2,col="white",lwd=.5)
  if(timz=="year"){
    rect(xleft = X$TID0,xright = X$TID0+1,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
    abline(v=X$TID0,col="white",lwd=.5)
  }
  if(timz%in%c("month")){
    abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
    rect(xleft = X$TID0+.2,xright = X$TID0+.8,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  }
  if(timz%in%c("week")){
    abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
    rect(xleft = X$TID0,xright = X$TID0+1,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  }
  if(timz%in%c("day")){
    abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
    segments(x0 = X$TID0,x1 = X$TID0,y0 = rep(0,nrow(X)),y1 = X$ACTION_ANY,col=palz[length(palz)],lwd=.5,lend=2)
  }
  dev.off()
}


  # # start-finish
  # dat.events <- which(indata.t$ACTION_ANY>0)
  # padz <- 5*c(1,12,52,365)[match(timz,c("year","month","week","day"))]
  # X <- indata.t[indata.t$YRMO>=indata.t$YRMO[max(1,-padz+dat.events[1])]&indata.t$YRMO<=indata.t$YRMO[min(nrow(indata.t), padz+max(dat.events))],]
  # head(X)
  # if(nrow(na.omit(X))==0){X <- indata.t[indata.t$YRMO>198812,]; ylimz <- l.j <- 0:1}
  # if(nrow(na.omit(X))>0){ylimz <- range(X$ACTION_ANY,na.rm=T); l.j <- pretty(range(X$ACTION_ANY,na.rm=T))}
  # X$TID0 <- 1:nrow(X)
  # FIle names
  plot.name <- c(paste0("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/time_panel_small/time_",srz,"_",cntz[k],"_",timz,".png"),paste0("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/time_event_small/time_",srz,"_",cntz[k],"_event.png"))
  pix <- 1
  if(timz%in%"day"){pix <- 1:2}
  for(i in pix){
  png(plot.name[i],height=3,width=13,units="in",res=300)
  par(mar=c(2,3,0.5,0.5))
  plot(x=X$TID0,y=X$ACTION_ANY,ylim=ylimz,xaxt="n",yaxt="n",bty="n",col=NA)
  axis(1,at =  which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),labels=NA)
  axis(1,at=which(grepl("001$|501$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),labels=substr(X$YRMO,1,4)[which(grepl("001$|501$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE)))],lwd=0,lwd.ticks=2)
  # axis(1,at=grep("00101$|50101$",X$DATE),labels=substr(X$YRMO,1,4)[grep("00101$|50101$",X$DATE)],lwd=0,lwd.ticks=2)
  l.i <- pretty(which(X$LABS!=""))
  l.i <- l.i[l.i<=nrow(X)]
  l.j <- l.j[l.j%%1==0]
  # axis(1,at = l.i,labels=substr(X$YRMO,1,4)[l.i+1],tick = FALSE,cex=.8)
  axis(2,at = l.j,las=1,cex.axis=.8)
  rect(xleft = X$TID0[1]-1,xright = X$TID0[length(X$TID0)]+1,ybottom = l.j[1],ytop = l.j[length(l.j)],col = "gray90",border = NA)
  abline(h=l.j,col="white",lwd=1)
  abline(h=l.j+diff(l.j)[1]/2,col="white",lwd=.5)
  if(timz=="year"){
    rect(xleft = X$TID0,xright = X$TID0+1,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
    abline(v=X$TID0,col="white",lwd=.5)
  }
  if(timz%in%c("month")){
    abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
    rect(xleft = X$TID0+.2,xright = X$TID0+.8,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  }
  if(timz%in%c("week")){
    abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
    rect(xleft = X$TID0,xright = X$TID0+1,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  }
  if(timz%in%c("day")){
    abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
    segments(x0 = X$TID0,x1 = X$TID0,y0 = rep(0,nrow(X)),y1 = X$ACTION_ANY,col=palz[length(palz)],lwd=.5,lend=2)
  }
  dev.off()
  }



  # Close a loop


  # # Close loop (single core)
  # }

  # Close loop: countries (Linux/Mac)
},mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)

# # Close loop (Windows)
# }
# stopCluster(cl)





# 
# ######################################################################################
# ######################################################################################
# ######################################################################################
# ## Global maps (from individual country files)
# ######################################################################################
# ######################################################################################
# ######################################################################################
# 
# 
# 
# ## Load event data
# srzz <- c("MELTT1km1dB" )
# space.agg <- c("adm0","adm1","adm2","priogrid","clea")
# time.agg <- c("year","month","week","day")
# space.ix <- c("ID_0","ID_1","ID_2","PRIO_GID","CLEA_CST")
# space.lab <- c("country","province","district","grid cell","electoral constituency")
# time.ix <- c("YEAR","YRMO","WID","TID")
# noadm1 <- c("MLT")
# noadm2 <- c("LBY","LSO","BHR","COM","ARE","ISR","KWT","MLT","SAU")
# s <- 4
# k <- 3
# a <- 4
# s0 <- 1
# filez <- dir("Upload/data_rdata_panel/")
# filez <- filez[grep("_year.RData",filez)]
# filez <- filez[grep(srzz[s0],filez)]
# cntz <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez),"_"),"[[",2)
# admz <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez),"_"),"[[",3)
# 
# # Duplicates
# cntz.dup <- sort(unique(cntz[duplicated(cntz)]))
# cntz.dup <- sort(unique(cntz))
# i <- 1
# j <- 1
# cntz.dup
# i <- 105
# filez.list <- lapply(1:length(cntz.dup),function(i){print(i)
#   filez.i <- filez[grep(cntz.dup[i],filez)][grep("adm0",filez[grep(cntz.dup[i],filez)])]
#   filez.size <- c()
#   filez.nevents <- c()
#   for(j in 1:length(filez.i)){
#     filez.size <- c(filez.size,file.info(paste0("Upload/data_rdata_panel/",filez.i[j]))$size)
#     load(paste0("Upload/data_rdata_panel/",filez.i[j]))
#     filez.nevents <- c(filez.nevents,sum(indata$ACTION_ANY,na.rm=T))
#   }
#   filez.pick <- filez.i[which(filez.nevents==max(filez.nevents))][1]
#   srz.pick <- gsub(paste0("_",cntz.dup[i],"_adm0_year.RData"),"",filez.pick)
#   filez.i2 <- filez[grep(cntz.dup[i],filez)][grep(paste0(srz.pick,"_",cntz.dup[i]),filez[grep(cntz.dup[i],filez)])]
#   filez.i2
# })
# filez <- unlist(filez.list)
# cntz <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez),"_"),"[[",2)
# admz <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez),"_"),"[[",3)
# 
# 
# # Create global basemap
# bb <- matrix(c(-145,140,-50,70),2,2,byrow=T)
# bb[1,1] <- bb[1,1]-(bb[1,2]-bb[1,1])/8
# bb[1,2] <- bb[1,2]+(bb[1,2]-bb[1,1])/8
# bb[2,1] <- bb[2,1]-(bb[2,2]-bb[2,1])/8
# bb[2,2] <- bb[2,2]+(bb[2,2]-bb[2,1])/8
# WKTs <- paste("POLYGON((",bb[1,1],bb[2,1],",",bb[1,2],bb[2,1],",",bb[1,2],bb[2,2],",",bb[1,1],bb[2,2],",",bb[1,1],bb[2,1],"))")
# test <- readWKT(WKTs)
# data("wrld_simpl")
# # test <- spTransform(test,proj4string(wrld_simpl))
# # proj4string(test) <- proj4string(wrld_simpl)
# basemap <- crop(x = wrld_simpl,y=test)
# 
# # Color
# palz <- c("gray90","indianred1","indianred3")
# # palz <- c("gray90","red","darkred")
# # palz <- c("gray90","coral1","coral3","coral4")
# # palz <- c("gray90","darkorchid1","darkorchid3","darkorchid4")
# res.palette <- colorRampPalette(palz, alpha=.25)
# cols.pal <- res.palette(5)
# # # B&W
# # cols.pal <- c("#CCCCCC", "#BCBCBC", "#ACACAC", "#9C9C9C", "#8C8C8C")
# 
# 
# # Loop over levels
# s <- 4
# # for(s in 1:length(space.agg)){print(space.agg[s])
# 
# # list of files at this level
# a <- space.agg[s]
# filez.agg <- filez[grep(a,filez)]
# 
# if(a=="adm1"){
#   cntz.agg <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez[grep(space.agg[s],filez)]),"_"),"[[",2)
#   cntz.agg0 <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez[grep(space.agg[s-1],filez)]),"_"),"[[",2)
#   noadm <- cntz.agg0[!cntz.agg0%in%cntz.agg]
#   filez.agg <- c(filez.agg,filez[grepl("adm0",filez)&cntz%in%noadm])
# }
# 
# if(a=="adm2"){
#   cntz.agg <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez[grep(space.agg[s],filez)]),"_"),"[[",2)
#   cntz.agg1 <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez[grep(space.agg[s-1],filez)]),"_"),"[[",2)
#   cntz.agg0 <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez[grep(space.agg[s-2],filez)]),"_"),"[[",2)
#   noadm0 <- cntz.agg0[!cntz.agg0%in%cntz.agg1]
#   noadm <- cntz.agg1[!cntz.agg1%in%cntz.agg]
#   filez.agg <- c(filez.agg,filez[grepl("adm1",filez)&cntz%in%noadm],filez[grepl("adm0",filez)&cntz%in%noadm0])
# }
# 
# cntz.agg <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez.agg),"_"),"[[",2)
# admz.agg <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez.agg),"_"),"[[",3)
# 
# 
# # Draw world plot
# png(paste0("Upload/graphics/map_",srzz[s0],"_",a,".png"),height=4.5,width=10,units = "in",res=300)
# par(mar=rep(0,4))
# plot(test,col="gray90",border=NA,density=30)
# plot(test,col="gray90",border=NA,density=30,angle=-45,add=T)
# plot(basemap,add=T,col="gray95",border="gray60",lwd=.5)
# 
# # Loop over countries
# k <- 1
# filez.agg
# for(k in 1:length(filez.agg)){
#   
#   print(paste0(filez.agg[k]))
#   
#   # Load country file
#   data <- load(paste0("Upload/data_rdata_panel/",filez.agg[k]))
#   data <- get(data); rm(indata)
#   cntz.agg[k]
#   a2 <- admz.agg[k]
#   timz <- gsub("_|\\.RData","",sapply(strsplit(filez.agg[k],a2),"[[",2))
#   timz
#   srz <- gsub(paste0("_",cntz.agg[k],"_",a2,"_",timz,".RData"),"",filez.agg[k])
#   srz
#   
#   # Country borders
#   gadm0 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz.agg[k],"_adm",0,".rds",sep=""))
#   
#   # Load map
#   if(grepl("adm",a2)){
#     gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz.agg[k],"_",a2,".rds",sep=""))
#   }
#   
#   # # Russia subset
#   # if(cntz.agg[k]=="RUS"&(!grepl("adm0",a2))){
#   #   gadm1 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz.agg[k],"_adm",1,".rds",sep=""))
#   #   reg.cauc <- c("Adygey","Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","Krasnodar","North Ossetia","Stavropol'")
#   #   reg.skfo <- c("Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","North Ossetia","Stavropol'")
#   #   reg.ufo <- c("Adygey","Astrakhan'","Kalmyk","Krasnodar","Rostov","Volgograd")
#   #   gadm1 <- gadm1[gadm1$NAME_1%in%c(reg.cauc),]
#   #   gadm00 <- gadm0
#   #   gadm0 <- crop(gadm0,gadm1)
#   #   gadm <- crop(gadm,gadm1)
#   # }
#   
#   # PRIO
#   if(a=="priogrid"){
#     load("Input/GIS/Borders/PRIOGRID/PRIOGRID.RData")
#     proj4string(map0) <- proj4string(gadm0)
#     coords0 <- as.data.frame(map0)[,c("xcoord","ycoord")]
#     sp.data0 <- SpatialPoints(coords0,proj4string=CRS(proj4string(gadm0)))
#     sp.box <- as(extent(bbox(gadm0)),"SpatialPolygons")
#     proj4string(sp.box) <- proj4string(gadm0)
#     o <- over(map0, as(sp.box, "SpatialPolygons"))
#     map.crop <- map0[which(!is.na(o)),]
#     o <- over(map.crop, as(gadm0, "SpatialPolygons"))
#     map.crop <- map.crop[which(!is.na(o)),]
#     names(map.crop) <- paste0("PRIO_",toupper(names(map.crop)))
#     gadm <- map.crop;rm(map0,coords0,sp.data0,sp.box,o,map.crop)
#   }
#   
#   # CLEA
#   if(a=="clea"){
#     clea.cnt <- dir("Input/GIS/Borders/CLEA/")[grep(countrycode(cntz.agg[k], origin = "iso3c",destination = "country.name"),dir("Input/GIS/Borders/CLEA/"))]
#     map0 <- readShapePoly(paste0(paste0("Input/GIS/Borders/CLEA/",clea.cnt),"/",sapply(strsplit(dir(paste0("Input/GIS/Borders/CLEA/",clea.cnt))[1],"\\."),"[[",1)))
#     proj4string(map0) <- proj4string(gadm0)
#     head(map0)
#     coords0 <- coordinates(map0)
#     sp.data0 <- SpatialPoints(coords0,proj4string=CRS(proj4string(gadm0)))
#     sp.box <- as(extent(bbox(gadm0)),"SpatialPolygons")
#     proj4string(sp.box) <- proj4string(gadm0)
#     o <- over(map0, as(sp.box, "SpatialPolygons"))
#     map.crop <- map0[which(!is.na(o)),]
#     o <- over(map.crop, as(gadm0, "SpatialPolygons"))
#     map.crop <- map.crop[which(!is.na(o)),]
#     names(map.crop) <- paste0("CLEA_",toupper(names(map.crop)))
#     names(map.crop) <- gsub("\\_\\d{4}","",names(map.crop))
#     gadm <- map.crop;rm(map0,coords0,sp.data0,sp.box,o,map.crop)
#   }
#   
#   # Find level of analysis
#   space.ag0 <- a2
#   time.ag0 <- "year"
#   space.ix0 <- space.ix[match(space.ag0,space.agg)]
#   time.ix0 <- time.ix[match(time.ag0,time.agg)]
#   
#   # Create map layers
#   agg.data <- aggregate(data[,c("SIDEA_ANY","SIDEB_ANY","ACTION_ANY")],by=list(UNIT=data[,space.ix0]),FUN=function(x){sum(x,na.rm=T)})
#   head(agg.data)
#   gadm <- merge(gadm,agg.data,by.x=space.ix0,by.y="UNIT",all.x=T,all.y=F)
#   head(gadm)
#   
#   # Color
#   v <- c(0, 1, 10, 50, 100, max(500,max(gadm$ACTION_ANY,na.rm=T)+1))
#   if(grepl("yzChechnya|yzLibya",filez.agg[k])){v <- c(0, 1, 10, 100, 1000, max(10000,max(gadm$ACTION_ANY,na.rm=T)+1))}
#   if(grepl("yzCaucasus2000|yzUkraine2014",filez.agg[k])){v <- c(0, 10, 100, 1000, 10000, max(100000,max(gadm$ACTION_ANY,na.rm=T)+1))}
#   if((srz%in%c("ACLED","GED")&cntz.agg[k]%in%c("SLE"))|(srz%in%c("GED")&cntz.agg[k]%in%c("NPL"))){v <- c(0, 10, 100, 1000, 10000, max(100000,max(gadm$ACTION_ANY,na.rm=T)+1))}
#   if(a=="adm0"){v <- c(0, 10, 1000, 5000, 10000, max(100000,max(gadm$ACTION_ANY,na.rm=T)+1))}
#   cols <- cols.pal[findInterval(gadm$ACTION_ANY, v)]
#   
#   # Add layer to plot
#   if(cntz.agg[k]=="RUS"&(!grepl("adm0",a2))){plot(gadm00,col=palz[1],add=T,border="white",lwd=ifelse(a%in%c("adm2","priogrid"),.05,.1))}
#   plot(gadm,col=cols,add=T,border="white",lwd=ifelse(a%in%c("adm2","priogrid"),ifelse(a%in%c("priogrid"),.00001,.05),.1))
#   if(!(cntz.agg[k]=="RUS"&(!grepl("adm0",a2)))){plot(gadm0,col=NA,border=rgb(0.1,0.1,0.1,.25),lwd=.5,add=T)}
#   if(cntz.agg[k]=="RUS"&(!grepl("adm0",a2))){plot(gadm00,col=NA,border=rgb(0.1,0.1,0.1,.25),lwd=.5,add=T)}
#   
# }
# 
# # Finish plot
# # plot(basemap,add=T,col=NA,border="gray60",lwd=.5)
# legend(x="bottomleft",fill=c("white"),legend=c(paste0("Level of violence (per ",space.lab[s],")")),bg=rgb(1,1,1,.75),ncol=1,cex=.75,border=NA,box.lwd=0)
# if(a!="adm0"){legend(x="bottomright",fill=c(cols.pal),legend=c("0","<10","<50","<100",">100"),bg=rgb(1,1,1,.75),ncol=6,cex=.75,border=NA,box.lwd=0)}
# if(a=="adm0"){legend(x="bottomright",fill=c(cols.pal),legend=c("<10","<1000","<5000","<10,000",">10,000"),bg=rgb(1,1,1,.75),ncol=6,cex=.75,border=NA,box.lwd=0)}
# 
# dev.off()
# 
# 
# # }
