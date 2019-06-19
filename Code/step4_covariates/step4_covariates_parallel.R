rm(list=ls())

## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
if(grepl("w64",sessionInfo()[[1]][[1]])){setwd("D:/Dropbox (Zhukov research team)/XSub/Data/")}
if(grepl("Ubuntu 18.04 LTS"
         ,sessionInfo()[[4]])){setwd("/mnt/oldhdd/home/zhukov/Dropbox (Zhukov research team)/XSub/Data/")}

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","spatstat","parallel","foreach", "doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# Parallel computing
mcoptions <- list(preschedule=FALSE, set.seed=TRUE)
ncores <- detectCores()
# cl <- makeCluster(ncores)
# registerDoParallel(cl)

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","spatstat","parallel","foreach")
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

# NEW: Area conversion functions
sqdeg2sqkm <- function(x){12379.77*x}
sqkm2sqdeg <- function(x){x/12379.77}

#############################
## Country list
#############################

#rm(list=ls())

# Exceptions
source("Code/step2_eventcode/step2x_eventcode_admex.R")

# List of files
srz <- sapply(strsplit(dir("Output"),"_"),"[",2)
srz <- srz[!is.na(srz)&!grepl("^ACD|^cdRw|^Covariates|^FM|^Sulliv|^Weather",srz)]
filez <- dir(paste0("Output/Output_",srz,"/Events"))
filez <- filez[grep("Events",filez)]
cntz <- sapply(strsplit(gsub(".RData","",filez),"_"),"[",3)
cntz <- cntz[cntz!="000"&!is.na(cntz)&cntz!="NA"]
cntz <- sort(unique(cntz))
cntz

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

# # Overlap only
# cntz <- cleaz[cleaz%in%cntz]

# # Limit to remaining
# ixp <- strsplit(dir("Output/Output_Covariates"),"_")
# cntz <- cntz[!cntz%in%sapply(ixp, '[', 2)]
# cntz <- cntz[!cntz%in%intersect(na.omit(sapply(ixp, '[', 2)[grep("adm\\d{1}",sapply(ixp, '[', 3))]),na.omit(sapply(ixp, '[', 2)[grep("priogrid",sapply(ixp, '[', 3))]))]

# Exclude some countries
cntz <- cntz[!cntz%in%c("MDV")]

# NEW: Missing and older files
filez.all <- c(paste0("Covariates_",rep(cntz,each=4),"_clea.RData"),paste0("Covariates_",rep(cntz,each=4),"_priogrid.RData"),paste0("Covariates_",rep(cntz,each=4*3),"_adm",rep(0:2,each=4),".RData"))
filez.all <- filez.all[!grepl(paste0(c(paste0(noadm1,"_adm1"),paste0(noadm2,"_adm2")),collapse="|"),filez.all)]
filez.old <- filez.all[!filez.all%in%dir("Output/Output_Covariates/")]
filez.old <- filez.old[!grepl(paste0(paste0(cntz[!cntz%in%cleaz],"_clea"),collapse="|"),filez.old)]
filez.old <- sort(unique(filez.old))
cntz <- cntz[cntz%in%sapply(strsplit(filez.old,"_"),"[",2)]
cntz


# source("Code/step3_aggregate/step3x_mdatez.R")

# filez.older <- dir("Output/Output_Covariates")[substr(file.mtime(paste0("Output/Output_Covariates/",dir("Output/Output_Covariates"))),1,10)<mdate]
# filez.older <- intersect(filez.all,filez.older)
# filez.old <- union(filez.old,filez.older)
cntz <- cntz[cntz%in%sapply(strsplit(filez.old,"_"),"[",2)]
cntz

# Sort by file size
cnt.size <- file.info(paste0("Input/GIS/Borders/GADM/",paste0(cntz,"_adm0.rds")))$size
cntz <- cntz[order(cnt.size)]
cntz

k0 <- 1; cntz[k0]; k <- k0


##############################################
##############################################
# Open loop (countries)
##############################################
##############################################

# k <- k0

# # Single core
# # stopCluster(cl)
# lapply(k0:length(cntz),function(k){print(cntz[k])

# Linux/Mac
# stopCluster(cl)
mclapply(k0:length(cntz),function(k){print(cntz[k])
  
  # # Windows
  # foreach(k=k0:length(cntz),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(cntz[k])
  #   lapply(list.of.packages, require, character.only = TRUE)
  
  
  #######################################################################################
  #######################################################################################
  ## CLEA
  #######################################################################################
  #######################################################################################

  # Only run if we have CLEA polygons for country
  if(cntz[k]%in%cleaz){
    print(paste(cntz[k],"clea"))

    # Only execute if file is missing
    if(paste0("Covariates_",cntz[k],"_clea.RData")%in%filez.old){

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

    
    # Simplify polygons for select countries
    if(cntz[k]%in%c("IRL","CAN")){
      map.sub <- map.crop
      size1 <- object.size(map.sub)
      map.sub0 <- gSimplify(map.sub,tol=.1,topologyPreserve = TRUE)
      map.sub0 <- gBuffer(map.sub0, byid=TRUE, width=0)
      map.val <- gIsValid(map.sub0,byid = TRUE,reason = TRUE)
      sum(gIsValid(map.sub0, byid=TRUE)==FALSE)
      map.sub0 <- SpatialPolygonsDataFrame(map.sub0,data=map.sub@data)
      size0 <- object.size(map.sub0)
      print(round(size0/size1,2))
      map.crop <- map.sub0
    }

    
    # Load GADM crop layer
    if(!paste0(cntz[k],"_adm",0,".rds",sep="")%in%dir("Input/GIS/Borders/GADM/")){download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",0,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))}
    gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))
    if(diff(bbox(map.crop)[1,])<=361){
      proj4string(map.crop) <- proj4string(gadm)
    }
    if(diff(bbox(map.crop)[1,])>361){
      proj4string(map.crop) <- CRS("+init=epsg:24877")
      map.crop <- spTransform(map.crop,CRS(proj4string(gadm)))
    }

    # # Russia subset
    # if(cntz[k]%in%c("RUS")){
    #   gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",1,".rds",sep=""))
    #   reg.cauc <- c("Adygey","Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","Krasnodar","North Ossetia","Stavropol'")
    #   reg.skfo <- c("Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","North Ossetia","Stavropol'")
    #   reg.ufo <- c("Adygey","Astrakhan'","Kalmyk","Krasnodar","Rostov","Volgograd")
    #   gadm <- gadm[gadm$NAME_1%in%c(reg.cauc),]
    # }

    # Add GADM names
    if(!paste0(cntz[k],"_adm",ifelse(cntz[k]%in%noadm2,ifelse(cntz[k]%in%noadm1,0,1),2),".rds")%in%dir("Input/GIS/Borders/GADM/")){download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",ifelse(cntz[k]%in%noadm2,ifelse(cntz[k]%in%noadm1,0,1),2),".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",ifelse(cntz[k]%in%noadm2,ifelse(cntz[k]%in%noadm1,0,1),2),".rds",sep=""))}
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
    # par(mar=rep(0,4)); plot(map.crop); #points(events[,c("LONG","LAT")],cex=.3,col="red")

    # ##########################
    # ## Merge with covariates (1)
    # ##########################
    #
    # source("Code/step4_covariates/step4x_covariates_agg.R")
    # map00 <- covariates_agg(map00=map.crop)
    # head(map00)

    ##########################
    ## Merge with covariates (2)
    ##########################

    map00 <- map.crop
    {
      #######################
      ## Population density
      #######################

      ## Load raster
      yrz <- c(1990,1995,2000)
      for(yy in 1:length(yrz)){
        year <- yrz[yy] # 1990, 1995 or 2000
        r <- raster(paste("Input/GIS/Covariates/POP_",year,"_25/glds",substr(year,3,4),"ag.asc",sep=""))
        proj4string(r) <- CRS(proj4string(map00))
        WKTs <- paste("POLYGON((",bbox(map00)[1,1],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,1],"))")
        test <- readWKT(WKTs)
        r <- crop(r,test)
        z <- extract(r,map00,fun=mean,factors=T,buffer=1000000, small=T,na.rm=T)
        map00$TEMP_NAME <- z
        names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("POP_",year)
      }

      head(as.data.frame(map00))



      #######################
      ## Elevation
      #######################
      ## Load raster
      r <- raster("Input/GIS/Covariates/DEM_geotiff/alwdgg.tif")
      #load("RawInput/GIS/Covariates2/ETOPO5_DEM.RData")
      WKTs <- paste("POLYGON((",bbox(map00)[1,1],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,1],"))")
      test <- readWKT(WKTs)
      proj4string(r) <- CRS(proj4string(map00))
      r <- crop(r, test)
      ##plot(r)

      # Mean
      z <- extract(r,map00,fun=mean,factors=T,buffer=1000000, small=T,na.rm=T)
      map00$TEMP_NAME <- z
      names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("ELEV_MEAN")
      # SD
      z <- extract(r,map00,fun=sd,factors=T,buffer=1000000, small=T,na.rm=T)
      map00$TEMP_NAME <- z
      names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("ELEV_SD")
      # Max
      z <- extract(r,map00,fun=max,factors=T,buffer=1000000, small=T,na.rm=T)
      map00$TEMP_NAME <- z
      names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("ELEV_MAX")
      head(as.data.frame(map00))



      #######################
      ## Land cover (NOTE: takes longer than other to run, therefore commented out)
      #######################

      ## Load raster
      r <- raster("Input/GIS/Covariates/GLCC/gblulcgeo20.tif")
      #load("RawInput/GIS/Covariates2/GLCC_LANDCOVER.RData")
      proj4string(r) <- CRS(proj4string(map00))
      WKTs <- paste("POLYGON((",bbox(map00)[1,1],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,1],"))")
      test <- readWKT(WKTs)
      r <- crop(r,test)
      ##plot(r)
      unique(r)

      # IGBP Land Cover Legend
      # Value   Description
      # 1 	Evergreen Needleleaf Forest
      # 2 	Evergreen Broadleaf Forest
      # 3 	Deciduous Needleleaf Forest
      # 4 	Deciduous Broadleaf Forest
      # 5 	Mixed Forest
      # 6 	Closed Shrublands
      # 7 	Open Shrublands
      # 8 	Woody Savannas
      # 9 	Savannas
      # 10 	Grasslands
      # 11 	Permanent Wetlands
      # 12 	Croplands
      # 13 	Urban and Built-Up
      # 14 	Cropland/Nanical Vegetation Mosaic
      # 15 	Snow and Ice
      # 16 	Barren or Sparsely Vegetated
      # 17 	Water Bodies
      # 99 	Interrupted Areas (Goodes Homolosine Projection)
      # 100 	Missing Data

      forest <- c(1:5)
      wetlands <- c(11,17)
      desert <- c(6:7,8,9:10,16)
      farm <- c(12,14)
      urban <- c(13)

      # Desert/steppe
      rf <- 1*(r%in%desert)
      z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
      map00$TEMP_NAME <- z
      names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("OPEN_TERRAIN")

      # Forest
      rf <- 1*(r%in%forest)
      z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
      map00$TEMP_NAME <- z
      names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("FOREST")

      # Wetland
      rf <- 1*(r%in%wetlands)
      z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
      map00$TEMP_NAME <- z
      names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("WETLAND")

      # Farmland
      rf <- 1*(r%in%farm)
      z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
      map00$TEMP_NAME <- z
      names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("FARMLAND")

      # Urban
      rf <- 1*(r%in%urban)
      z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
      map00$TEMP_NAME <- z
      names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("URBAN")

      head(as.data.frame(map00))


      #######################
      ## Ethnic: GREG
      #######################

      #gg <- readShapePoly("Input/GIS/Covariates/GREG/GREG",proj4string=CRS(proj4string(map00)))
      load("Input/GIS/Covariates2/GREG_ETHNIC.RData")
      proj4string(greg) <- CRS(proj4string(map00))
      proj4string(map00) <- CRS(proj4string(map00))
      g <- greg
      g <- crop(g,map00)
      g$G1SHORTNAM <- as.character(g$G1SHORTNAM)
      ##plot(g)
      gregmat <- data.frame(GREG_NGROUPS=rep(0,nrow(map00)),GREG_GROUPS=rep("",nrow(map00)))
      gregmat$GREG_GROUPS <- as.character(gregmat$GREG_GROUPS)
      if(length(g)>0){
        o <- over(as(map00, "SpatialPolygons"),g,returnList=T)
        for(i in 1:length(o)){
          gregmat$GREG_NGROUPS[i] <- length(unique(o[[i]]$G1SHORTNAM))
          gregmat$GREG_GROUPS[i] <- paste0(sort(unique(o[[i]]$G1SHORTNAM)),collapse=", ")
        }}
      head(gregmat)
      row.names(gregmat) <- row.names(map00)
      for(j in seq_along(names(gregmat))){
        z <- gregmat[,names(gregmat)[j]]
        map00$TEMP_NAME <- z
        names(map00)[names(map00)%in%"TEMP_NAME"] <- names(gregmat)[j]
      }
      head(map00)

      #######################
      ## Language
      #######################

      #ll <- readShapePoly("~/Documents/GDELT/Covariates/gmi_geodata/lang/langa",proj4string=CRS(proj4string(map00)))
      load("Input/GIS/Covariates2/WLMS_LANGUAGE.RData")
      proj4string(lang) <- CRS(proj4string(map00))
      ll <- lang
      ll$NAME2 <- as.character(ll$NAME2)
      proj4string(map00) <- CRS(proj4string(map00))
      #ll <- crop(ll,map00)

      langmat <- data.frame(WLMS_NLANG=rep(0,nrow(map00)),WLMS_LANGS=rep("",nrow(map00)))
      langmat$WLMS_LANGS <- as.character(langmat$WLMS_LANGS)
      if(length(ll)>0){
        o <- over(as(map00, "SpatialPolygons"),ll,returnList=T)
        for(i in 1:length(o)){
          langmat$WLMS_NLANG[i] <- length(unique(o[[i]]$NAME2))
          langmat$WLMS_LANGS[i] <- paste0(sort(unique(o[[i]]$NAME2)),collapse=", ")
        }}
      head(langmat)
      row.names(langmat) <- row.names(map00)
      for(j in seq_along(names(langmat))){
        z <- langmat[,names(langmat)[j]]
        map00$TEMP_NAME <- z
        names(map00)[names(map00)%in%"TEMP_NAME"] <- names(langmat)[j]
      }
      head(map00)



      ##########################
      ## Built-up areas
      ##########################

      #bu <- readShapePoly("Input/GIS/Covariates/GlobalGIS/mapbase/e_social/vpf/builtupa",proj4string=CRS(proj4string(map00)))
      load("Input/GIS/Covariates2/GGIS_BUILTUP.RData")
      proj4string(bu) <- CRS(proj4string(map00))
      proj4string(map00) <- CRS(proj4string(map00))
      bu$PPPTNAME <- as.character(bu$PPPTNAME)
      bu <- crop(bu,map00)

      #plot(bu,add=T)
      bumat <- data.frame(NBUILTUP=rep(0,nrow(map00)),BUILTUP=rep("",nrow(map00)))
      bumat$BUILTUP <- as.character(bumat$BUILTUP)
      if(length(bu)>0){
        o <- over(as(map00, "SpatialPolygons"),bu,returnList=T)
        for(i in 1:length(o)){
          bumat$NBUILTUP[i] <- length(unique(o[[i]]$PPPTNAME))
          bumat$BUILTUP[i] <- paste0(sort(unique(o[[i]]$PPPTNAME)),collapse=", ")
        }}
      bumat$BUILTUP <- gsub("NA, |NA|, NA","",bumat$BUILTUP)
      head(bumat)
      row.names(bumat) <- row.names(map00)
      for(j in seq_along(names(bumat))){
        z <- bumat[,names(bumat)[j]]
        map00$TEMP_NAME <- z
        names(map00)[names(map00)%in%"TEMP_NAME"] <- names(bumat)[j]
      }
      head(map00)


      ##########################
      ## Petroleum
      ##########################

      #pt <- readShapePoly("Input/GIS/Covariates/Petroleum/PETRO_Onshore_080907",proj4string=CRS(proj4string(map00)))
      load("Input/GIS/Covariates2/PRIO_PETROLEUM.RData")
      proj4string(petro) <- CRS(proj4string(map00))
      proj4string(map00) <- CRS(proj4string(map00))
      petro$NAME <- as.character(petro$NAME)
      pt <- petro
      pt <- crop(pt,map00)
      ##plot(pt)
      length(pt)

      ptmat <- data.frame(NPETRO=rep(0,nrow(map00)),PETRO=rep("",nrow(map00)))
      ptmat$PETRO <- as.character(ptmat$PETRO)
      if(length(pt)>0){
        o <- over(as(map00, "SpatialPolygons"),pt,returnList=T)
        i <- 1
        for(i in 1:length(o)){
          ptmat$NPETRO[i] <- length(unique(o[[i]]$NAME))
          ptmat$PETRO[i] <- paste0(sort(unique(o[[i]]$NAME)),collapse=", ")
        }}
      head(ptmat)
      row.names(ptmat) <- row.names(map00)
      for(j in seq_along(names(ptmat))){
        z <- ptmat[,names(ptmat)[j]]
        map00$TEMP_NAME <- z
        names(map00)[names(map00)%in%"TEMP_NAME"] <- names(ptmat)[j]
      }
      head(map00)



      ##########################
      ## Capital cities
      ##########################

      #cities <- readShapePoints("Input/GIS/Covariates/GlobalGIS/mapbase/f_pol/ESRI_cities",proj4string=CRS(proj4string(map00)))
      load("Input/GIS/Covariates2/GGIS_CITIES.RData")
      proj4string(cities) <- CRS(proj4string(map00))
      juba <- data.frame(CITY_NAME="Juba",GMI_ADMIN="",ADMIN_NAME="",FIPS_CNTRY="",CNTRY_NAME="",STATUS="National capital",POP_RANK="",POP_CLASS="",PORT_ID="")
      juba <- SpatialPointsDataFrame(coords = data.frame(x1=31.601111,x2=4.871944),data=juba, proj4string = CRS(proj4string(cities)))
      cities <- spRbind(cities,juba)

      o <- !is.na(over(cities, as(map00, "SpatialPolygons")))
      cities <- cities[o,]
      cities <- cities[grep("capital",cities$STATUS),]

      if(length(cities)==0){
        map00$DIST2PROVCAP <- NA
        map00$DIST2CAP <- NA
      }

      if(length(cities)>0){
        dmat <- crossdist(coordinates(map00)[,1],coordinates(map00)[,2],coordinates(cities)[,1],coordinates(cities)[,2])
        capz <- grepl("National",cities$STATUS)&grepl("capital",cities$STATUS)
        capz
        if(length(capz)>1){capz <- grepl("National",cities$STATUS)&grepl("capital",cities$STATUS)&cities@data[capz,"FIPS_CNTRY"]%in%map00@data[1,"ISO2"]}
        if(length(map00)==1){capmat <- data.frame(DIST2PROVCAP=apply(dmat,1,function(x){min(x,na.rm=T)})*111.32,DIST2CAP=min(dmat[,capz],na.rm=T)*111.32)}
        if(length(map00)>1){capmat <- data.frame(DIST2PROVCAP=apply(dmat,1,function(x){min(x,na.rm=T)})*111.32,DIST2CAP=dmat[,capz]*111.32)}
        head(capmat)
        row.names(capmat) <- row.names(map00)
        names(capmat)
        for(j in seq_along(names(capmat))){
          z <- capmat[,names(capmat)[j]]
          map00$TEMP_NAME <- z
          names(map00)[names(map00)%in%"TEMP_NAME"] <- names(capmat)[j]
        }
      }
      head(map00)

      ##########################
      ## Road
      ##########################

      cnt.r <- cntz[k]
      if(cntz[k]=="SSD"){cnt.r <- "SDN"}

      if(!paste0("gRoads_",cnt.r,".RData")%in%dir("Input/GIS/Covariates/gRoads/CountryFiles/RData/")){
        rlmat <- data.frame(ROAD_XING=rep(NA,nrow(map00)),ROAD_LENGTH=rep(NA,nrow(map00)))
      }
      if(paste0("gRoads_",cnt.r,".RData")%in%dir("Input/GIS/Covariates/gRoads/CountryFiles/RData/")){
        if(cnt.r!="IRQ"){load(paste0("Input/GIS/Covariates/gRoads/CountryFiles/RData/gRoads_",cnt.r,".RData"));}
        if(cnt.r=="IRQ"){rd <- readShapeLines(paste("Input/GIS/Covariates2/Diva/",cnt.r,"_roads",sep=""))}

        # Intersect
        rd.l <- gIntersection(rd,map00,byid=c(F,T),drop_lower_td=TRUE)

        # Compute # intersections on border & road lengths
        rlmat <- data.frame(ROAD_XING=rep(0,nrow(map00)),ROAD_LENGTH=rep(0,nrow(map00)))
        i <- 1
        l <- 1

        for(i in 1:length(attributes(rd.l)[[1]])){
          rlmat[attributes(rd.l)[[1]][[i]]@ID,"ROAD_XING"] <- length(attributes(rd.l)[[1]][[i]]@Lines)
          lsums <- c()
          for(l in 1:length(attributes(rd.l)[[1]][[i]]@Lines)){
            dmat <- as.matrix(dist(attributes(rd.l)[[1]][[i]]@Lines[[l]]@coords))
            dsum <- c()
            for(k0 in 1:(nrow(dmat)-1)){dsum[k0] <- dmat[k0+1,k0]}
            lsums[l] <- sum(dsum)*111.32}
          rlmat[attributes(rd.l)[[1]][[i]]@ID,"ROAD_LENGTH"] <- sum(lsums)
        }
        rlmat[is.na(rlmat[,"ROAD_LENGTH"]),"ROAD_LENGTH"] <- 0
        rlmat <- rlmat[row.names(map00),]
      }
      row.names(rlmat) <- row.names(map00)
      for(j in seq_along(names(rlmat))){
        z <- rlmat[,names(rlmat)[j]]
        map00$TEMP_NAME <- z
        names(map00)[names(map00)%in%"TEMP_NAME"] <- names(rlmat)[j]
      }
      # summary(map00)
      head(map00)

      # # Sanity check
      # i <- 3
      # par(mar=c(0,0,0,0))
      # plot(map00,xlim=bbox(map00[i,])[1,],ylim=bbox(map00[i,])[2,])
      # plot(rd,col="red",add=T)
      # plot(map00[i,],border="blue",add=T)


      ##########################
      ## Area
      ##########################

      map00$AREA_KM2 <- sapply(slot(map00, "polygons"), slot, "area")*12365.1613
      map00$ROAD_DENSITY <- map00$ROAD_LENGTH/map00$AREA_KM2
      head(map00)
      #print(summary(map00))

    }

    ##########################
    ## Save
    ##########################

    save(map00,file=paste0("Output/Output_Covariates/Covariates_",cntz[k],"_clea.RData"))


    # End if statement
    }

    # End CLEA
  }
  

  

  

  # #######################################################################################
  # #######################################################################################
  # ## GADM
  # #######################################################################################
  # #######################################################################################
  # 
  # ## Open loop (adm)
  # a <- 2
  # for(a in 0:2){print(paste(cntz[k],a))
  #   admz <- a
  #   #load(url(paste("http://biogeo.ucdavis.edu/data/gadm2/R/",cntz[k],"_adm",admz,".RData",sep="")))
  # 
  #   # Only execute if file is missing
  #   if(paste0("Covariates_",cntz[k],"_adm",admz,".RData")%in%filez.old){
  # 
  #   # Start adm2 exception
  #   if(!((a==1)&(cntz[k]%in%noadm1))){
  #     if(!((a==2)&(cntz[k]%in%noadm2))){
  # 
  #       if(!paste0(cntz[k],"_adm",admz,".rds",sep="")%in%dir("Input/GIS/Borders/GADM/")){
  #         download.file(paste0("http://biogeo.ucdavis.edu/data/map2.8/rds/",cntz[k],"_adm",admz,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",admz,".rds",sep=""))
  #       }
  #       map.crop <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",admz,".rds",sep=""))
  #       # par(mar=rep(0,4)); plot(map00); #points(events[,c("LONG","LAT")],cex=.3,col="red")
  # 
  #       # # Russia subset
  #       # if(cntz[k]%in%c("RUS")&a!=0){
  #       #   gadm1 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",admz,".rds",sep=""))
  #       #   reg.cauc <- c("Adygey","Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","Krasnodar","North Ossetia","Stavropol'")
  #       #   reg.skfo <- c("Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","North Ossetia","Stavropol'")
  #       #   reg.ufo <- c("Adygey","Astrakhan'","Kalmyk","Krasnodar","Rostov","Volgograd")
  #       #   map.crop <- gadm1[gadm1$NAME_1%in%c(reg.cauc),]
  #       # }
  #       # if(cntz[k]%in%c("RUS")&a==0){
  #       #   gadm1 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",1,".rds",sep=""))
  #       #   reg.cauc <- c("Adygey","Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","Krasnodar","North Ossetia","Stavropol'")
  #       #   reg.skfo <- c("Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","North Ossetia","Stavropol'")
  #       #   reg.ufo <- c("Adygey","Astrakhan'","Kalmyk","Krasnodar","Rostov","Volgograd")
  #       #   gadm1 <- gadm1[gadm1$NAME_1%in%c(reg.cauc),]
  #       #   plot(gadm1)
  #       #   map.crop <- crop(map00,gadm1)
  #       # }
  # 
  #       # Simplify polygons for select countries
  #       if(cntz[k]%in%c("USA","CAN")){
  #         map.sub <- map.crop
  #         size1 <- object.size(map.sub)
  #         map.sub0 <- gSimplify(map.sub,tol=ifelse(a%in%2,.1,.1),topologyPreserve = TRUE)
  #         map.sub0 <- gBuffer(map.sub0, byid=TRUE, width=0)
  #         map.val <- gIsValid(map.sub0,byid = TRUE,reason = TRUE)
  #         sum(gIsValid(map.sub0, byid=TRUE)==FALSE)
  #         map.sub0 <- SpatialPolygonsDataFrame(map.sub0,data=map.sub@data)
  #         size0 <- object.size(map.sub0)
  #         print(round(size0/size1,2))
  #         map.crop <- map.sub0
  #       }
  #       
  #       # map00 <- map.crop
  # 
  #       # ##########################
  #       # ## Merge with covariates (1)
  #       # ##########################
  #       #
  #       # source("Code/step4_covariates/step4x_covariates_agg.R")
  #       # map00 <- covariates_agg(map00=map.crop)
  #       # head(map00)
  #       # #print(summary(map00))
  # 
  # 
  #       ##########################
  #       ## Merge with covariates (2)
  #       ##########################
  # 
  #       map00 <- map.crop
  #       {
  #         #######################
  #         ## Population density
  #         #######################
  # 
  #         ## Load raster
  #         yrz <- c(1990,1995,2000)
  #         for(yy in 1:length(yrz)){
  #           year <- yrz[yy] # 1990, 1995 or 2000
  #           r <- raster(paste("Input/GIS/Covariates/POP_",year,"_25/glds",substr(year,3,4),"ag.asc",sep=""))
  #           proj4string(r) <- CRS(proj4string(map00))
  #           WKTs <- paste("POLYGON((",bbox(map00)[1,1],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,1],"))")
  #           test <- readWKT(WKTs)
  #           r <- crop(r,test)
  #           z <- extract(r,map00,fun=mean,factors=T,buffer=1000000, small=T,na.rm=T)
  #           map00$TEMP_NAME <- z
  #           names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("POP_",year)
  #         }
  # 
  #         head(as.data.frame(map00))
  # 
  # 
  # 
  #         #######################
  #         ## Elevation
  #         #######################
  #         ## Load raster
  #         r <- raster("Input/GIS/Covariates/DEM_geotiff/alwdgg.tif")
  #         #load("RawInput/GIS/Covariates2/ETOPO5_DEM.RData")
  #         WKTs <- paste("POLYGON((",bbox(map00)[1,1],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,1],"))")
  #         test <- readWKT(WKTs)
  #         proj4string(r) <- CRS(proj4string(map00))
  #         r <- crop(r, test)
  #         ##plot(r)
  # 
  #         # Mean
  #         z <- extract(r,map00,fun=mean,factors=T,buffer=1000000, small=T,na.rm=T)
  #         map00$TEMP_NAME <- z
  #         names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("ELEV_MEAN")
  #         # SD
  #         z <- extract(r,map00,fun=sd,factors=T,buffer=1000000, small=T,na.rm=T)
  #         map00$TEMP_NAME <- z
  #         names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("ELEV_SD")
  #         # Max
  #         z <- extract(r,map00,fun=max,factors=T,buffer=1000000, small=T,na.rm=T)
  #         map00$TEMP_NAME <- z
  #         names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("ELEV_MAX")
  #         head(as.data.frame(map00))
  # 
  # 
  # 
  #         #######################
  #         ## Land cover (NOTE: takes longer than other to run, therefore commented out)
  #         #######################
  # 
  #         ## Load raster
  #         r <- raster("Input/GIS/Covariates/GLCC/gblulcgeo20.tif")
  #         #load("RawInput/GIS/Covariates2/GLCC_LANDCOVER.RData")
  #         proj4string(r) <- CRS(proj4string(map00))
  #         WKTs <- paste("POLYGON((",bbox(map00)[1,1],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,1],"))")
  #         test <- readWKT(WKTs)
  #         r <- crop(r,test)
  #         ##plot(r)
  #         unique(r)
  # 
  #         # IGBP Land Cover Legend
  #         # Value   Description
  #         # 1 	Evergreen Needleleaf Forest
  #         # 2 	Evergreen Broadleaf Forest
  #         # 3 	Deciduous Needleleaf Forest
  #         # 4 	Deciduous Broadleaf Forest
  #         # 5 	Mixed Forest
  #         # 6 	Closed Shrublands
  #         # 7 	Open Shrublands
  #         # 8 	Woody Savannas
  #         # 9 	Savannas
  #         # 10 	Grasslands
  #         # 11 	Permanent Wetlands
  #         # 12 	Croplands
  #         # 13 	Urban and Built-Up
  #         # 14 	Cropland/Nanical Vegetation Mosaic
  #         # 15 	Snow and Ice
  #         # 16 	Barren or Sparsely Vegetated
  #         # 17 	Water Bodies
  #         # 99 	Interrupted Areas (Goodes Homolosine Projection)
  #         # 100 	Missing Data
  # 
  #         forest <- c(1:5)
  #         wetlands <- c(11,17)
  #         desert <- c(6:7,8,9:10,16)
  #         farm <- c(12,14)
  #         urban <- c(13)
  # 
  #         # Desert/steppe
  #         rf <- 1*(r%in%desert)
  #         z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
  #         map00$TEMP_NAME <- z
  #         names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("OPEN_TERRAIN")
  # 
  #         # Forest
  #         rf <- 1*(r%in%forest)
  #         z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
  #         map00$TEMP_NAME <- z
  #         names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("FOREST")
  # 
  #         # Wetland
  #         rf <- 1*(r%in%wetlands)
  #         z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
  #         map00$TEMP_NAME <- z
  #         names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("WETLAND")
  # 
  #         # Farmland
  #         rf <- 1*(r%in%farm)
  #         z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
  #         map00$TEMP_NAME <- z
  #         names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("FARMLAND")
  # 
  #         # Urban
  #         rf <- 1*(r%in%urban)
  #         z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
  #         map00$TEMP_NAME <- z
  #         names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("URBAN")
  # 
  #         head(as.data.frame(map00))
  # 
  # 
  # 
  # 
  #         #######################
  #         ## Ethnic: GREG
  #         #######################
  # 
  #         #gg <- readShapePoly("Input/GIS/Covariates/GREG/GREG",proj4string=CRS(proj4string(map00)))
  #         load("Input/GIS/Covariates2/GREG_ETHNIC.RData")
  #         proj4string(greg) <- CRS(proj4string(map00))
  #         proj4string(map00) <- CRS(proj4string(map00))
  #         g <- greg
  #         g <- crop(g,map00)
  #         g$G1SHORTNAM <- as.character(g$G1SHORTNAM)
  #         ##plot(g)
  #         gregmat <- data.frame(GREG_NGROUPS=rep(0,nrow(map00)),GREG_GROUPS=rep("",nrow(map00)))
  #         gregmat$GREG_GROUPS <- as.character(gregmat$GREG_GROUPS)
  #         if(length(g)>0){
  #           o <- over(as(map00, "SpatialPolygons"),g,returnList=T)
  #           for(i in 1:length(o)){
  #             gregmat$GREG_NGROUPS[i] <- length(unique(o[[i]]$G1SHORTNAM))
  #             gregmat$GREG_GROUPS[i] <- paste0(sort(unique(o[[i]]$G1SHORTNAM)),collapse=", ")
  #           }}
  #         head(gregmat)
  #         row.names(gregmat) <- row.names(map00)
  #         for(j in seq_along(names(gregmat))){
  #           z <- gregmat[,names(gregmat)[j]]
  #           map00$TEMP_NAME <- z
  #           names(map00)[names(map00)%in%"TEMP_NAME"] <- names(gregmat)[j]
  #         }
  #         head(map00)
  # 
  #         #######################
  #         ## Language
  #         #######################
  # 
  #         #ll <- readShapePoly("~/Documents/GDELT/Covariates/gmi_geodata/lang/langa",proj4string=CRS(proj4string(map00)))
  #         load("Input/GIS/Covariates2/WLMS_LANGUAGE.RData")
  #         proj4string(lang) <- CRS(proj4string(map00))
  #         ll <- lang
  #         ll$NAME2 <- as.character(ll$NAME2)
  #         proj4string(map00) <- CRS(proj4string(map00))
  #         #ll <- crop(ll,map00)
  # 
  #         langmat <- data.frame(WLMS_NLANG=rep(0,nrow(map00)),WLMS_LANGS=rep("",nrow(map00)))
  #         langmat$WLMS_LANGS <- as.character(langmat$WLMS_LANGS)
  #         if(length(ll)>0){
  #           o <- over(as(map00, "SpatialPolygons"),ll,returnList=T)
  #           for(i in 1:length(o)){
  #             langmat$WLMS_NLANG[i] <- length(unique(o[[i]]$NAME2))
  #             langmat$WLMS_LANGS[i] <- paste0(sort(unique(o[[i]]$NAME2)),collapse=", ")
  #           }}
  #         head(langmat)
  #         row.names(langmat) <- row.names(map00)
  #         for(j in seq_along(names(langmat))){
  #           z <- langmat[,names(langmat)[j]]
  #           map00$TEMP_NAME <- z
  #           names(map00)[names(map00)%in%"TEMP_NAME"] <- names(langmat)[j]
  #         }
  #         head(map00)
  # 
  # 
  # 
  #         ##########################
  #         ## Built-up areas
  #         ##########################
  # 
  #         #bu <- readShapePoly("Input/GIS/Covariates/GlobalGIS/mapbase/e_social/vpf/builtupa",proj4string=CRS(proj4string(map00)))
  #         load("Input/GIS/Covariates2/GGIS_BUILTUP.RData")
  #         proj4string(bu) <- CRS(proj4string(map00))
  #         proj4string(map00) <- CRS(proj4string(map00))
  #         bu$PPPTNAME <- as.character(bu$PPPTNAME)
  #         bu <- crop(bu,map00)
  # 
  #         #plot(bu,add=T)
  #         bumat <- data.frame(NBUILTUP=rep(0,nrow(map00)),BUILTUP=rep("",nrow(map00)))
  #         bumat$BUILTUP <- as.character(bumat$BUILTUP)
  #         if(length(bu)>0){
  #           o <- over(as(map00, "SpatialPolygons"),bu,returnList=T)
  #           for(i in 1:length(o)){
  #             bumat$NBUILTUP[i] <- length(unique(o[[i]]$PPPTNAME))
  #             bumat$BUILTUP[i] <- paste0(sort(unique(o[[i]]$PPPTNAME)),collapse=", ")
  #           }}
  #         bumat$BUILTUP <- gsub("NA, |NA|, NA","",bumat$BUILTUP)
  #         head(bumat)
  #         row.names(bumat) <- row.names(map00)
  #         for(j in seq_along(names(bumat))){
  #           z <- bumat[,names(bumat)[j]]
  #           map00$TEMP_NAME <- z
  #           names(map00)[names(map00)%in%"TEMP_NAME"] <- names(bumat)[j]
  #         }
  #         head(map00)
  # 
  # 
  #         ##########################
  #         ## Petroleum
  #         ##########################
  # 
  #         #pt <- readShapePoly("Input/GIS/Covariates/Petroleum/PETRO_Onshore_080907",proj4string=CRS(proj4string(map00)))
  #         load("Input/GIS/Covariates2/PRIO_PETROLEUM.RData")
  #         proj4string(petro) <- CRS(proj4string(map00))
  #         proj4string(map00) <- CRS(proj4string(map00))
  #         petro$NAME <- as.character(petro$NAME)
  #         pt <- petro
  #         pt <- crop(pt,map00)
  #         ##plot(pt)
  #         length(pt)
  # 
  #         ptmat <- data.frame(NPETRO=rep(0,nrow(map00)),PETRO=rep("",nrow(map00)))
  #         ptmat$PETRO <- as.character(ptmat$PETRO)
  #         if(length(pt)>0){
  #           o <- over(as(map00, "SpatialPolygons"),pt,returnList=T)
  #           i <- 1
  #           for(i in 1:length(o)){
  #             ptmat$NPETRO[i] <- length(unique(o[[i]]$NAME))
  #             ptmat$PETRO[i] <- paste0(sort(unique(o[[i]]$NAME)),collapse=", ")
  #           }}
  #         head(ptmat)
  #         row.names(ptmat) <- row.names(map00)
  #         for(j in seq_along(names(ptmat))){
  #           z <- ptmat[,names(ptmat)[j]]
  #           map00$TEMP_NAME <- z
  #           names(map00)[names(map00)%in%"TEMP_NAME"] <- names(ptmat)[j]
  #         }
  #         head(map00)
  # 
  # 
  # 
  #         ##########################
  #         ## Capital cities
  #         ##########################
  # 
  #         #cities <- readShapePoints("Input/GIS/Covariates/GlobalGIS/mapbase/f_pol/ESRI_cities",proj4string=CRS(proj4string(map00)))
  #         load("Input/GIS/Covariates2/GGIS_CITIES.RData")
  #         proj4string(cities) <- CRS(proj4string(map00))
  #         juba <- data.frame(CITY_NAME="Juba",GMI_ADMIN="",ADMIN_NAME="",FIPS_CNTRY="",CNTRY_NAME="",STATUS="National capital",POP_RANK="",POP_CLASS="",PORT_ID="")
  #         juba <- SpatialPointsDataFrame(coords = data.frame(x1=31.601111,x2=4.871944),data=juba, proj4string = CRS(proj4string(cities)))
  #         cities <- spRbind(cities,juba)
  # 
  #         o <- !is.na(over(cities, as(map00, "SpatialPolygons")))
  #         cities <- cities[o,]
  #         cities <- cities[grep("capital",cities$STATUS),]
  # 
  #         if(length(cities)==0){
  #           map00$DIST2PROVCAP <- NA
  #           map00$DIST2CAP <- NA
  #         }
  # 
  #         if(length(cities)>0){
  #           dmat <- crossdist(coordinates(map00)[,1],coordinates(map00)[,2],coordinates(cities)[,1],coordinates(cities)[,2])
  #           capz <- grepl("National",cities$STATUS)&grepl("capital",cities$STATUS)
  #           capz
  #           if(length(capz)>1){capz <- grepl("National",cities$STATUS)&grepl("capital",cities$STATUS)&cities@data[capz,"FIPS_CNTRY"]%in%map00@data[1,"ISO2"]}
  #           if(length(map00)==1){capmat <- data.frame(DIST2PROVCAP=apply(dmat,1,function(x){min(x,na.rm=T)})*111.32,DIST2CAP=min(dmat[,capz],na.rm=T)*111.32)}
  #           if(length(map00)>1){capmat <- data.frame(DIST2PROVCAP=apply(dmat,1,function(x){min(x,na.rm=T)})*111.32,DIST2CAP=dmat[,capz]*111.32)}
  #           head(capmat)
  #           row.names(capmat) <- row.names(map00)
  #           names(capmat)
  #           for(j in seq_along(names(capmat))){
  #             z <- capmat[,names(capmat)[j]]
  #             map00$TEMP_NAME <- z
  #             names(map00)[names(map00)%in%"TEMP_NAME"] <- names(capmat)[j]
  #           }
  #         }
  #         head(map00)
  # 
  #         ##########################
  #         ## Road
  #         ##########################
  # 
  #         cnt.r <- cntz[k]
  #         if(cntz[k]=="SSD"){cnt.r <- "SDN"}
  # 
  #         if(!paste0("gRoads_",cnt.r,".RData")%in%dir("Input/GIS/Covariates/gRoads/CountryFiles/RData/")){
  #           rlmat <- data.frame(ROAD_XING=rep(NA,nrow(map00)),ROAD_LENGTH=rep(NA,nrow(map00)))
  #         }
  #         if(paste0("gRoads_",cnt.r,".RData")%in%dir("Input/GIS/Covariates/gRoads/CountryFiles/RData/")){
  #           if(cnt.r!="IRQ"){load(paste0("Input/GIS/Covariates/gRoads/CountryFiles/RData/gRoads_",cnt.r,".RData"));}
  #           if(cnt.r=="IRQ"){rd <- readShapeLines(paste("Input/GIS/Covariates2/Diva/",cnt.r,"_roads",sep=""))}
  # 
  #           # Intersect
  #           rd.l <- gIntersection(rd,map00,byid=c(F,T),drop_lower_td=TRUE)
  # 
  #           # Compute # intersections on border & road lengths
  #           rlmat <- data.frame(ROAD_XING=rep(0,nrow(map00)),ROAD_LENGTH=rep(0,nrow(map00)))
  #           i <- 1
  #           l <- 1
  # 
  #           for(i in 1:length(attributes(rd.l)[[1]])){
  #             rlmat[attributes(rd.l)[[1]][[i]]@ID,"ROAD_XING"] <- length(attributes(rd.l)[[1]][[i]]@Lines)
  #             lsums <- c()
  #             for(l in 1:length(attributes(rd.l)[[1]][[i]]@Lines)){
  #               dmat <- as.matrix(dist(attributes(rd.l)[[1]][[i]]@Lines[[l]]@coords))
  #               dsum <- c()
  #               for(k0 in 1:(nrow(dmat)-1)){dsum[k0] <- dmat[k0+1,k0]}
  #               lsums[l] <- sum(dsum)*111.32}
  #             rlmat[attributes(rd.l)[[1]][[i]]@ID,"ROAD_LENGTH"] <- sum(lsums)
  #           }
  #           rlmat[is.na(rlmat[,"ROAD_LENGTH"]),"ROAD_LENGTH"] <- 0
  #           rlmat <- rlmat[row.names(map00),]
  #         }
  #         row.names(rlmat) <- row.names(map00)
  #         for(j in seq_along(names(rlmat))){
  #           z <- rlmat[,names(rlmat)[j]]
  #           map00$TEMP_NAME <- z
  #           names(map00)[names(map00)%in%"TEMP_NAME"] <- names(rlmat)[j]
  #         }
  #         # summary(map00)
  #         head(map00)
  # 
  #         # # Sanity check
  #         # i <- 3
  #         # par(mar=c(0,0,0,0))
  #         # plot(map00,xlim=bbox(map00[i,])[1,],ylim=bbox(map00[i,])[2,])
  #         # plot(rd,col="red",add=T)
  #         # plot(map00[i,],border="blue",add=T)
  # 
  # 
  #         ##########################
  #         ## Area
  #         ##########################
  # 
  #         map00$AREA_KM2 <- sapply(slot(map00, "polygons"), slot, "area")*12365.1613
  #         map00$ROAD_DENSITY <- map00$ROAD_LENGTH/map00$AREA_KM2
  #         head(map00)
  #         #print(summary(map00))
  # 
  #       }
  # 
  #       ##########################
  #       ## Save
  #       ##########################
  # 
  #       save(map00,file=paste0("Output/Output_Covariates/Covariates_",cntz[k],"_adm",admz,".RData"))
  # 
  #     # Close loop: adm exceptions
  #     }}
  # 
  #   # Close if statement
  #   }
  # 
  #   # Close loop: adm
  # }

  
  # #######################################################################################
  # #######################################################################################
  # ## PRIO GRID
  # #######################################################################################
  # #######################################################################################
  # 
  # # Only execute if file is missing
  # if(paste0("Covariates_",cntz[k],"_priogrid.RData")%in%filez.old){
  #   
  #   # Load Prio Grid
  #   #map0 <- readShapePoly("Data/PRIOGRID/priogrid_cell")
  #   #save(map0,file="Data/PRIOGRID/PRIOGRID.RData")
  #   #k=1
  #   load("Input/GIS/Borders/PRIOGRID/PRIOGRID.RData")
  #   #print("prio")
  #   print(paste(cntz[k],"prio"))
  #   
  #   
  #   
  #   # Load GADM crop layer
  #   if(!paste0(cntz[k],"_adm",0,".rds",sep="")%in%dir("Input/GIS/Borders/GADM/")){download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",0,".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))}
  #   gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",0,".rds",sep=""))
  #   proj4string(map0) <- proj4string(gadm)
  #   
  #   # # Russia subset
  #   # if(cntz[k]%in%c("RUS")){
  #   #   gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",1,".rds",sep=""))
  #   #   reg.cauc <- c("Adygey","Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","Krasnodar","North Ossetia","Stavropol'")
  #   #   reg.skfo <- c("Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","North Ossetia","Stavropol'")
  #   #   reg.ufo <- c("Adygey","Astrakhan'","Kalmyk","Krasnodar","Rostov","Volgograd")
  #   #   gadm <- gadm[gadm$NAME_1%in%c(reg.cauc),]
  #   # }
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
  #   if(!paste0(cntz[k],"_adm",ifelse(cntz[k]%in%noadm2,ifelse(cntz[k]%in%noadm1,0,1),2),".rds")%in%dir("Input/GIS/Borders/GADM/")){download.file(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",cntz[k],"_adm",ifelse(cntz[k]%in%noadm2,ifelse(cntz[k]%in%noadm1,0,1),2),".rds"),paste("Input/GIS/Borders/GADM/",cntz[k],"_adm",ifelse(cntz[k]%in%noadm2,ifelse(cntz[k]%in%noadm1,0,1),2),".rds",sep=""))}
  #   gadm2 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_adm",ifelse(cntz[k]%in%noadm2,ifelse(cntz[k]%in%noadm1,0,1),2),".rds",sep=""))
  #   gvarz <- names(gadm2)
  #   o0 <- gIntersection(map.crop,gadm2,byid = TRUE,drop_lower_td = TRUE)
  #   o0_data <- data.frame(JOINT_ID=row.names(o0),ID1=sapply(strsplit(row.names(o0)," "),"[",1),ID2=sapply(strsplit(row.names(o0)," "),"[",2))
  #   row.names(o0_data) <- row.names(o0)
  #   o0 <- SpatialPolygonsDataFrame(o0,data=o0_data)
  #   o0$AREA <- sqdeg2sqkm(gArea(o0,byid = TRUE))
  #   o0 <- o0[order(o0$ID1,o0$AREA),]
  #   o0 <- o0[!duplicated(o0$ID1,fromLast = TRUE),]
  #   map.crop$ID1 <- row.names(map.crop)
  #   gadm2$ID2 <- row.names(gadm2)
  #   o0 <- merge(o0,gadm2,by="ID2",all.x=T,all.y=F)
  #   map.crop <- merge(map.crop,o0[,c("ID1",gvarz)],by="ID1",all.x=T,all.y=F)
  #   map.crop$ID1 <- NULL
  #   head(map.crop)
  #   
  #   # par(mar=rep(0,4)); plot(map00); #points(events[,c("LONG","LAT")],cex=.3,col="red")
  #   
  #   
  #   # ##########################
  #   # ## Merge with covariates (1)
  #   # ##########################
  #   #
  #   # source("Code/step4_covariates/step4x_covariates_agg.R")
  #   # map00 <- covariates_agg(map00=map.crop)
  #   # head(map00)
  #   # summary(map00)
  #   
  #   ##########################
  #   ## Merge with covariates (2)
  #   ##########################
  #   
  #   map00 <- map.crop
  #   {
  #     #######################
  #     ## Population density
  #     #######################
  #     
  #     ## Load raster
  #     yrz <- c(1990,1995,2000)
  #     for(yy in 1:length(yrz)){
  #       year <- yrz[yy] # 1990, 1995 or 2000
  #       r <- raster(paste("Input/GIS/Covariates/POP_",year,"_25/glds",substr(year,3,4),"ag.asc",sep=""))
  #       proj4string(r) <- CRS(proj4string(map00))
  #       WKTs <- paste("POLYGON((",bbox(map00)[1,1],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,1],"))")
  #       test <- readWKT(WKTs)
  #       r <- crop(r,test)
  #       z <- extract(r,map00,fun=mean,factors=T,buffer=1000000, small=T,na.rm=T)
  #       map00$TEMP_NAME <- z
  #       names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("POP_",year)
  #     }
  #     
  #     head(as.data.frame(map00))
  #     
  #     
  #     
  #     #######################
  #     ## Elevation
  #     #######################
  #     ## Load raster
  #     r <- raster("Input/GIS/Covariates/DEM_geotiff/alwdgg.tif")
  #     #load("RawInput/GIS/Covariates2/ETOPO5_DEM.RData")
  #     WKTs <- paste("POLYGON((",bbox(map00)[1,1],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,1],"))")
  #     test <- readWKT(WKTs)
  #     proj4string(r) <- CRS(proj4string(map00))
  #     r <- crop(r, test)
  #     ##plot(r)
  #     
  #     # Mean
  #     z <- extract(r,map00,fun=mean,factors=T,buffer=1000000, small=T,na.rm=T)
  #     map00$TEMP_NAME <- z
  #     names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("ELEV_MEAN")
  #     # SD
  #     z <- extract(r,map00,fun=sd,factors=T,buffer=1000000, small=T,na.rm=T)
  #     map00$TEMP_NAME <- z
  #     names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("ELEV_SD")
  #     # Max
  #     z <- extract(r,map00,fun=max,factors=T,buffer=1000000, small=T,na.rm=T)
  #     map00$TEMP_NAME <- z
  #     names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("ELEV_MAX")
  #     head(as.data.frame(map00))
  #     
  #     
  #     
  #     #######################
  #     ## Land cover (NOTE: takes longer than other to run, therefore commented out)
  #     #######################
  #     
  #     ## Load raster
  #     r <- raster("Input/GIS/Covariates/GLCC/gblulcgeo20.tif")
  #     #load("RawInput/GIS/Covariates2/GLCC_LANDCOVER.RData")
  #     proj4string(r) <- CRS(proj4string(map00))
  #     WKTs <- paste("POLYGON((",bbox(map00)[1,1],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,1],",",bbox(map00)[1,2],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,2],",",bbox(map00)[1,1],bbox(map00)[2,1],"))")
  #     test <- readWKT(WKTs)
  #     r <- crop(r,test)
  #     ##plot(r)
  #     unique(r)
  #     
  #     # IGBP Land Cover Legend
  #     # Value   Description
  #     # 1 	Evergreen Needleleaf Forest
  #     # 2 	Evergreen Broadleaf Forest
  #     # 3 	Deciduous Needleleaf Forest
  #     # 4 	Deciduous Broadleaf Forest
  #     # 5 	Mixed Forest
  #     # 6 	Closed Shrublands
  #     # 7 	Open Shrublands
  #     # 8 	Woody Savannas
  #     # 9 	Savannas
  #     # 10 	Grasslands
  #     # 11 	Permanent Wetlands
  #     # 12 	Croplands
  #     # 13 	Urban and Built-Up
  #     # 14 	Cropland/Nanical Vegetation Mosaic
  #     # 15 	Snow and Ice
  #     # 16 	Barren or Sparsely Vegetated
  #     # 17 	Water Bodies
  #     # 99 	Interrupted Areas (Goodes Homolosine Projection)
  #     # 100 	Missing Data
  #     
  #     forest <- c(1:5)
  #     wetlands <- c(11,17)
  #     desert <- c(6:7,8,9:10,16)
  #     farm <- c(12,14)
  #     urban <- c(13)
  #     
  #     # Desert/steppe
  #     rf <- 1*(r%in%desert)
  #     z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
  #     map00$TEMP_NAME <- z
  #     names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("OPEN_TERRAIN")
  #     
  #     # Forest
  #     rf <- 1*(r%in%forest)
  #     z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
  #     map00$TEMP_NAME <- z
  #     names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("FOREST")
  #     
  #     # Wetland
  #     rf <- 1*(r%in%wetlands)
  #     z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
  #     map00$TEMP_NAME <- z
  #     names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("WETLAND")
  #     
  #     # Farmland
  #     rf <- 1*(r%in%farm)
  #     z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
  #     map00$TEMP_NAME <- z
  #     names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("FARMLAND")
  #     
  #     # Urban
  #     rf <- 1*(r%in%urban)
  #     z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
  #     map00$TEMP_NAME <- z
  #     names(map00)[names(map00)%in%"TEMP_NAME"] <- paste0("URBAN")
  #     
  #     head(as.data.frame(map00))
  #     
  #     
  #     
  #     
  #     #######################
  #     ## Ethnic: GREG
  #     #######################
  #     
  #     #gg <- readShapePoly("Input/GIS/Covariates/GREG/GREG",proj4string=CRS(proj4string(map00)))
  #     load("Input/GIS/Covariates2/GREG_ETHNIC.RData")
  #     proj4string(greg) <- CRS(proj4string(map00))
  #     proj4string(map00) <- CRS(proj4string(map00))
  #     g <- greg
  #     g <- crop(g,map00)
  #     g$G1SHORTNAM <- as.character(g$G1SHORTNAM)
  #     ##plot(g)
  #     gregmat <- data.frame(GREG_NGROUPS=rep(0,nrow(map00)),GREG_GROUPS=rep("",nrow(map00)))
  #     gregmat$GREG_GROUPS <- as.character(gregmat$GREG_GROUPS)
  #     if(length(g)>0){
  #       o <- over(as(map00, "SpatialPolygons"),g,returnList=T)
  #       for(i in 1:length(o)){
  #         gregmat$GREG_NGROUPS[i] <- length(unique(o[[i]]$G1SHORTNAM))
  #         gregmat$GREG_GROUPS[i] <- paste0(sort(unique(o[[i]]$G1SHORTNAM)),collapse=", ")
  #       }}
  #     head(gregmat)
  #     row.names(gregmat) <- row.names(map00)
  #     for(j in seq_along(names(gregmat))){
  #       z <- gregmat[,names(gregmat)[j]]
  #       map00$TEMP_NAME <- z
  #       names(map00)[names(map00)%in%"TEMP_NAME"] <- names(gregmat)[j]
  #     }
  #     head(map00)
  #     
  #     #######################
  #     ## Language
  #     #######################
  #     
  #     #ll <- readShapePoly("~/Documents/GDELT/Covariates/gmi_geodata/lang/langa",proj4string=CRS(proj4string(map00)))
  #     load("Input/GIS/Covariates2/WLMS_LANGUAGE.RData")
  #     proj4string(lang) <- CRS(proj4string(map00))
  #     ll <- lang
  #     ll$NAME2 <- as.character(ll$NAME2)
  #     proj4string(map00) <- CRS(proj4string(map00))
  #     #ll <- crop(ll,map00)
  #     
  #     langmat <- data.frame(WLMS_NLANG=rep(0,nrow(map00)),WLMS_LANGS=rep("",nrow(map00)))
  #     langmat$WLMS_LANGS <- as.character(langmat$WLMS_LANGS)
  #     if(length(ll)>0){
  #       o <- over(as(map00, "SpatialPolygons"),ll,returnList=T)
  #       for(i in 1:length(o)){
  #         langmat$WLMS_NLANG[i] <- length(unique(o[[i]]$NAME2))
  #         langmat$WLMS_LANGS[i] <- paste0(sort(unique(o[[i]]$NAME2)),collapse=", ")
  #       }}
  #     head(langmat)
  #     row.names(langmat) <- row.names(map00)
  #     for(j in seq_along(names(langmat))){
  #       z <- langmat[,names(langmat)[j]]
  #       map00$TEMP_NAME <- z
  #       names(map00)[names(map00)%in%"TEMP_NAME"] <- names(langmat)[j]
  #     }
  #     head(map00)
  #     
  #     
  #     
  #     ##########################
  #     ## Built-up areas
  #     ##########################
  #     
  #     #bu <- readShapePoly("Input/GIS/Covariates/GlobalGIS/mapbase/e_social/vpf/builtupa",proj4string=CRS(proj4string(map00)))
  #     load("Input/GIS/Covariates2/GGIS_BUILTUP.RData")
  #     proj4string(bu) <- CRS(proj4string(map00))
  #     proj4string(map00) <- CRS(proj4string(map00))
  #     bu$PPPTNAME <- as.character(bu$PPPTNAME)
  #     bu <- crop(bu,map00)
  #     
  #     #plot(bu,add=T)
  #     bumat <- data.frame(NBUILTUP=rep(0,nrow(map00)),BUILTUP=rep("",nrow(map00)))
  #     bumat$BUILTUP <- as.character(bumat$BUILTUP)
  #     if(length(bu)>0){
  #       o <- over(as(map00, "SpatialPolygons"),bu,returnList=T)
  #       for(i in 1:length(o)){
  #         bumat$NBUILTUP[i] <- length(unique(o[[i]]$PPPTNAME))
  #         bumat$BUILTUP[i] <- paste0(sort(unique(o[[i]]$PPPTNAME)),collapse=", ")
  #       }}
  #     bumat$BUILTUP <- gsub("NA, |NA|, NA","",bumat$BUILTUP)
  #     head(bumat)
  #     row.names(bumat) <- row.names(map00)
  #     for(j in seq_along(names(bumat))){
  #       z <- bumat[,names(bumat)[j]]
  #       map00$TEMP_NAME <- z
  #       names(map00)[names(map00)%in%"TEMP_NAME"] <- names(bumat)[j]
  #     }
  #     head(map00)
  #     
  #     
  #     ##########################
  #     ## Petroleum
  #     ##########################
  #     
  #     #pt <- readShapePoly("Input/GIS/Covariates/Petroleum/PETRO_Onshore_080907",proj4string=CRS(proj4string(map00)))
  #     load("Input/GIS/Covariates2/PRIO_PETROLEUM.RData")
  #     proj4string(petro) <- CRS(proj4string(map00))
  #     proj4string(map00) <- CRS(proj4string(map00))
  #     petro$NAME <- as.character(petro$NAME)
  #     pt <- petro
  #     pt <- crop(pt,map00)
  #     ##plot(pt)
  #     length(pt)
  #     
  #     ptmat <- data.frame(NPETRO=rep(0,nrow(map00)),PETRO=rep("",nrow(map00)))
  #     ptmat$PETRO <- as.character(ptmat$PETRO)
  #     if(length(pt)>0){
  #       o <- over(as(map00, "SpatialPolygons"),pt,returnList=T)
  #       i <- 1
  #       for(i in 1:length(o)){
  #         ptmat$NPETRO[i] <- length(unique(o[[i]]$NAME))
  #         ptmat$PETRO[i] <- paste0(sort(unique(o[[i]]$NAME)),collapse=", ")
  #       }}
  #     head(ptmat)
  #     row.names(ptmat) <- row.names(map00)
  #     for(j in seq_along(names(ptmat))){
  #       z <- ptmat[,names(ptmat)[j]]
  #       map00$TEMP_NAME <- z
  #       names(map00)[names(map00)%in%"TEMP_NAME"] <- names(ptmat)[j]
  #     }
  #     head(map00)
  #     
  #     
  #     
  #     ##########################
  #     ## Capital cities
  #     ##########################
  #     
  #     #cities <- readShapePoints("Input/GIS/Covariates/GlobalGIS/mapbase/f_pol/ESRI_cities",proj4string=CRS(proj4string(map00)))
  #     load("Input/GIS/Covariates2/GGIS_CITIES.RData")
  #     proj4string(cities) <- CRS(proj4string(map00))
  #     juba <- data.frame(CITY_NAME="Juba",GMI_ADMIN="",ADMIN_NAME="",FIPS_CNTRY="",CNTRY_NAME="",STATUS="National capital",POP_RANK="",POP_CLASS="",PORT_ID="")
  #     juba <- SpatialPointsDataFrame(coords = data.frame(x1=31.601111,x2=4.871944),data=juba, proj4string = CRS(proj4string(cities)))
  #     cities <- spRbind(cities,juba)
  #     
  #     o <- !is.na(over(cities, as(map00, "SpatialPolygons")))
  #     cities <- cities[o,]
  #     cities <- cities[grep("capital",cities$STATUS),]
  #     
  #     if(length(cities)==0){
  #       map00$DIST2PROVCAP <- NA
  #       map00$DIST2CAP <- NA
  #     }
  #     
  #     if(length(cities)>0){
  #       dmat <- crossdist(coordinates(map00)[,1],coordinates(map00)[,2],coordinates(cities)[,1],coordinates(cities)[,2])
  #       capz <- grepl("National",cities$STATUS)&grepl("capital",cities$STATUS)
  #       capz
  #       if(length(capz)>1){capz <- grepl("National",cities$STATUS)&grepl("capital",cities$STATUS)&cities@data[capz,"FIPS_CNTRY"]%in%map00@data[1,"ISO2"]}
  #       if(length(map00)==1){capmat <- data.frame(DIST2PROVCAP=apply(dmat,1,function(x){min(x,na.rm=T)})*111.32,DIST2CAP=min(dmat[,capz],na.rm=T)*111.32)}
  #       if(length(map00)>1){capmat <- data.frame(DIST2PROVCAP=apply(dmat,1,function(x){min(x,na.rm=T)})*111.32,DIST2CAP=dmat[,capz]*111.32)}
  #       head(capmat)
  #       row.names(capmat) <- row.names(map00)
  #       names(capmat)
  #       for(j in seq_along(names(capmat))){
  #         z <- capmat[,names(capmat)[j]]
  #         map00$TEMP_NAME <- z
  #         names(map00)[names(map00)%in%"TEMP_NAME"] <- names(capmat)[j]
  #       }
  #     }
  #     head(map00)
  #     
  #     ##########################
  #     ## Road
  #     ##########################
  #     
  #     cnt.r <- cntz[k]
  #     if(cntz[k]=="SSD"){cnt.r <- "SDN"}
  #     
  #     if(!paste0("gRoads_",cnt.r,".RData")%in%dir("Input/GIS/Covariates/gRoads/CountryFiles/RData/")){
  #       rlmat <- data.frame(ROAD_XING=rep(NA,nrow(map00)),ROAD_LENGTH=rep(NA,nrow(map00)))
  #     }
  #     if(paste0("gRoads_",cnt.r,".RData")%in%dir("Input/GIS/Covariates/gRoads/CountryFiles/RData/")){
  #       if(cnt.r!="IRQ"){load(paste0("Input/GIS/Covariates/gRoads/CountryFiles/RData/gRoads_",cnt.r,".RData"));}
  #       if(cnt.r=="IRQ"){rd <- readShapeLines(paste("Input/GIS/Covariates2/Diva/",cnt.r,"_roads",sep=""))}
  #       
  #       # Intersect
  #       rd.l <- gIntersection(rd,map00,byid=c(F,T),drop_lower_td=TRUE)
  #       
  #       # Compute # intersections on border & road lengths
  #       rlmat <- data.frame(ROAD_XING=rep(0,nrow(map00)),ROAD_LENGTH=rep(0,nrow(map00)))
  #       i <- 1
  #       l <- 1
  #       
  #       for(i in 1:length(attributes(rd.l)[[1]])){
  #         rlmat[attributes(rd.l)[[1]][[i]]@ID,"ROAD_XING"] <- length(attributes(rd.l)[[1]][[i]]@Lines)
  #         lsums <- c()
  #         for(l in 1:length(attributes(rd.l)[[1]][[i]]@Lines)){
  #           dmat <- as.matrix(dist(attributes(rd.l)[[1]][[i]]@Lines[[l]]@coords))
  #           dsum <- c()
  #           for(k0 in 1:(nrow(dmat)-1)){dsum[k0] <- dmat[k0+1,k0]}
  #           lsums[l] <- sum(dsum)*111.32}
  #         rlmat[attributes(rd.l)[[1]][[i]]@ID,"ROAD_LENGTH"] <- sum(lsums)
  #       }
  #       rlmat[is.na(rlmat[,"ROAD_LENGTH"]),"ROAD_LENGTH"] <- 0
  #       rlmat <- rlmat[row.names(map00),]
  #     }
  #     row.names(rlmat) <- row.names(map00)
  #     for(j in seq_along(names(rlmat))){
  #       z <- rlmat[,names(rlmat)[j]]
  #       map00$TEMP_NAME <- z
  #       names(map00)[names(map00)%in%"TEMP_NAME"] <- names(rlmat)[j]
  #     }
  #     # summary(map00)
  #     head(map00)
  #     
  #     # # Sanity check
  #     # i <- 3
  #     # par(mar=c(0,0,0,0))
  #     # plot(map00,xlim=bbox(map00[i,])[1,],ylim=bbox(map00[i,])[2,])
  #     # plot(rd,col="red",add=T)
  #     # plot(map00[i,],border="blue",add=T)
  #     
  #     
  #     ##########################
  #     ## Area
  #     ##########################
  #     
  #     map00$AREA_KM2 <- sapply(slot(map00, "polygons"), slot, "area")*12365.1613
  #     map00$ROAD_DENSITY <- map00$ROAD_LENGTH/map00$AREA_KM2
  #     head(map00)
  #     #print(summary(map00))
  #     
  #   }
  #   
  #   
  #   
  #   ##########################
  #   ## Save
  #   ##########################
  #   
  #   save(map00,file=paste0("Output/Output_Covariates/Covariates_",cntz[k],"_priogrid.RData"))
  #   
  #   # Close if statement
  # }
  # 
  
  
  ##############################################
  ##############################################
  # Close loop 
  ##############################################
  ##############################################
  
# # Close loop: countries (single core)
# })
  
# Close loop: countries (Linux/Mac)
},mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)

#   # Close loop (Windows)
# }
# stopCluster(cl)
