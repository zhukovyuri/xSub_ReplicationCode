rm(list=ls())

setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#source("Code/functions.R")

## Install & load packages (all at once)
list.of.packages <- c("maptools","classInt","raster","sp","rgeos","parallel","foreach","doParallel","countrycode","PBSmapping")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)



# ###########################################
# ## Global maps
# ###########################################
# 
# ## Load event data
# srzz <- c("ACLED","GED","PITF","SCAD","yzChechnya","yzCaucasus2000","yzLibya","yzUkraine2014")
# space.agg <- c("adm0","adm1","adm2","priogrid")
# time.agg <- c("year","month","week","day")
# space.ix <- c("ID_0","ID_1","ID_2","PRIO_GID")
# time.ix <- c("YEAR","YRMO","WID","TID")
# noadm1 <- c("MLT")
# noadm2 <- c("LBY","LSO","BHR","COM","ARE","ISR","KWT","MLT","SAU")
# s <- 4
# k <- 3
# a <- 4
# 
# # Loop over sources
# for(s in 1:4){print(srzz[s])
# 
# data <- load(paste0("Upload/data_rdata_combined/",srzz[s],"_",space.agg[a],"_",time.agg[1],".RData"))
# data <- get(data); rm(mat.filez)
# cntz <- sort(unique(data$ISO))
# 
# # Load world map
# data("wrld_simpl")
# 
# # # GADM
# # if(a!=4){
# #   gadm0 <- wrld_simpl[wrld_simpl$ISO3%in%cntz,]
# #   gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz[k],"_",space.agg[a],".rds",sep=""))
# # }
# 
# # PRIO
# if(a==4){
#   load("Input/GIS/Borders/PRIOGRID/PRIOGRID.RData")
#   gadm0 <- wrld_simpl[wrld_simpl$ISO3%in%cntz,]
#   proj4string(map0) <- proj4string(gadm0)
#   coords0 <- as.data.frame(map0)[,c("xcoord","ycoord")]
#   sp.data0 <- SpatialPoints(coords0,proj4string=CRS(proj4string(gadm0)))
#   sp.box <- as(extent(bbox(gadm0)),"SpatialPolygons")
#   proj4string(sp.box) <- proj4string(gadm0)
#   o <- over(map0, as(sp.box, "SpatialPolygons"))
#   map.crop <- map0[which(!is.na(o)),]
#   o <- over(map.crop, as(gadm0, "SpatialPolygons"))
#   map.crop <- map.crop[which(!is.na(o)),]
#   names(map.crop) <- paste0("PRIO_",toupper(names(map.crop)))
#   gadm <- map.crop;rm(map0,coords0,sp.data0,sp.box,o,map.crop)
# }
# 
# # Date range
# drange <- paste0("(",range(as.character(data$YEAR),na.rm=T)[1],"-",range(as.character(data$YEAR),na.rm=T)[2],")")
#           
# # Find level of analysis
# space.ag0 <- sapply(strsplit(as.character(data$UNIT_LEVEL[1]), "_"), "[[", 1)
# time.ag0 <- sapply(strsplit(as.character(data$UNIT_LEVEL[1]), "_"), "[[", 2)
# space.ix0 <- space.ix[match(space.ag0,space.agg)]
# time.ix0 <- time.ix[match(time.ag0,time.agg)]    
# # print(paste0(srzz[s],"_",space.ag0))
#         
# # Create map layers
# agg.data <- aggregate(data[,c("SIDEA_ANY","SIDEB_ANY","ACTION_ANY")],by=list(UNIT=data[,space.ix0]),FUN=function(x){sum(x,na.rm=T)})
# gadm <- merge(gadm,agg.data,by.x=space.ix0,by.y="UNIT",all.x=T,all.y=F)
# data("wrld_simpl")
# bb <- bbox(wrld_simpl[wrld_simpl$POP2005>50000,])
# bb <- matrix(c(-145,140,-50,70),2,2,byrow=T)
# bb[1,1] <- bb[1,1]-(bb[1,2]-bb[1,1])/8
# bb[1,2] <- bb[1,2]+(bb[1,2]-bb[1,1])/8
# bb[2,1] <- bb[2,1]-(bb[2,2]-bb[2,1])/8
# bb[2,2] <- bb[2,2]+(bb[2,2]-bb[2,1])/8
# WKTs <- paste("POLYGON((",bb[1,1],bb[2,1],",",bb[1,2],bb[2,1],",",bb[1,2],bb[2,2],",",bb[1,1],bb[2,2],",",bb[1,1],bb[2,1],"))")
# test <- readWKT(WKTs)
# data("wrld_simpl")
# wrld_simpl <- spTransform(wrld_simpl,proj4string(gadm))
# #proj4string(test) <- proj4string(gadm)
# basemap <- crop(x = wrld_simpl,y=test)
# 
# # Color
# res.palette <- colorRampPalette(c("beige","red","darkred"), alpha=.25)
# cols.pal <- res.palette(5)
# 
# # # B&W
# # cols.pal <- c("#CCCCCC", "#BCBCBC", "#ACACAC", "#9C9C9C", "#8C8C8C")
# v <- c(0, 1, 10, 50, 100, max(500,max(gadm$ACTION_ANY,na.rm=T)+1))
# cols <- cols.pal[findInterval(gadm$ACTION_ANY, v)]
# 
# # Plot
# png(paste0("Output/Maps/map_000_",srzz[s],"_",space.ag0,".png"),height=4.5,width=10,units = "in",res=300)
# par(mar=rep(0,4))
# plot(test,col="lightgrey",border=NA,density=30)
# plot(test,col="lightgrey",border=NA,density=30,angle=-45,add=T)
# plot(basemap,add=T,col="gray95",border="gray60",lwd=.5)
# plot(gadm,col=cols,add=T,border=NA)
# plot(basemap,add=T,col=NA,border="gray60",lwd=.5)
# legend(x="bottomleft",fill=c("white"),legend=c(paste0(srzz[s]," events ",drange)),bg=rgb(1,1,1,.75),ncol=1,cex=.75,border=NA,box.lwd=0)
# legend(x="bottomright",fill=c(cols.pal),legend=c("0","<10","<50","<100",">100"),bg=rgb(1,1,1,.75),ncol=6,cex=.75,border=NA,box.lwd=0)
# dev.off()
# }


# ###########################################
# ## Global maps (from individual country files)
# ###########################################
# 
# 
# 
# ## Load event data
# srzz <- c("ABA_Darfur","ACLED","Beissinger_Protest","Beissinger_Riot","Beissinger_Ukraine","ESOC_Afghanistan","ESOC_Iraq","ESOC_Iraq_WITS","ESOC_Mexico_drug_related_murders","ESOC_Mexico_homicide","ESOC_Pakistan","ESOC_Pakistan_WITS","FM_France","GED","NVMS","PITF","SCAD","Sullivan_Guatemala","yzCaucasus2000","yzChechnya","yzLibya","yzUkraine2014" )
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
# 
# filez <- dir("Upload/data_rdata_country/")
# filez <- filez[grep("_year.RData",filez)]
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
# filez.i <- filez[grep(cntz.dup[i],filez)][grep("adm0",filez[grep(cntz.dup[i],filez)])]
# filez.size <- c()
# filez.nevents <- c()
# for(j in 1:length(filez.i)){
# filez.size <- c(filez.size,file.info(paste0("Upload/data_rdata_country/",filez.i[j]))$size)
# load(paste0("Upload/data_rdata_country/",filez.i[j]))
# filez.nevents <- c(filez.nevents,sum(indata$ACTION_ANY,na.rm=T))
# }
# filez.pick <- filez.i[which(filez.nevents==max(filez.nevents))][1]
# srz.pick <- gsub(paste0("_",cntz.dup[i],"_adm0_year.RData"),"",filez.pick)
# filez.i2 <- filez[grep(cntz.dup[i],filez)][grep(paste0(srz.pick,"_",cntz.dup[i]),filez[grep(cntz.dup[i],filez)])]
# filez.i2
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
# palz <- c("gray90","dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4")
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
# s <- 1
# for(s in 1:length(space.agg)){print(space.agg[s])
#   
#   # list of files at this level
#   a <- space.agg[s]
#   filez.agg <- filez[grep(a,filez)]
#   
#   if(a=="adm1"){
#     cntz.agg <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez[grep(space.agg[s],filez)]),"_"),"[[",2)
#     cntz.agg0 <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez[grep(space.agg[s-1],filez)]),"_"),"[[",2)
#     noadm <- cntz.agg0[!cntz.agg0%in%cntz.agg]
#     filez.agg <- c(filez.agg,filez[grepl("adm0",filez)&cntz%in%noadm])
#   }
#   
#   if(a=="adm2"){
#     cntz.agg <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez[grep(space.agg[s],filez)]),"_"),"[[",2)
#     cntz.agg1 <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez[grep(space.agg[s-1],filez)]),"_"),"[[",2)
#     cntz.agg0 <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez[grep(space.agg[s-2],filez)]),"_"),"[[",2)
#     noadm0 <- cntz.agg0[!cntz.agg0%in%cntz.agg1]
#     noadm <- cntz.agg1[!cntz.agg1%in%cntz.agg]
#     filez.agg <- c(filez.agg,filez[grepl("adm1",filez)&cntz%in%noadm],filez[grepl("adm0",filez)&cntz%in%noadm0])
#   }
#   
#   cntz.agg <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez.agg),"_"),"[[",2)
#   admz.agg <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez.agg),"_"),"[[",3)
#   
#   
#   # Draw world plot
#   png(paste0("Upload/graphics/maps/map_000_",a,".png"),height=4.5,width=10,units = "in",res=300)
#   par(mar=rep(0,4))
#   plot(test,col="gray90",border=NA,density=30)
#   plot(test,col="gray90",border=NA,density=30,angle=-45,add=T)
#   plot(basemap,add=T,col="gray95",border="gray60",lwd=.5)    
#   
#   # Loop over countries
#   k <- 107
#   filez.agg
#   for(k in 1:length(filez.agg)){
#   
#     print(paste0(filez.agg[k]))
#     
#     # Load country file
#     data <- load(paste0("Upload/data_rdata_country/",filez.agg[k]))
#     data <- get(data); rm(indata)
#     cntz.agg[k]
#     a2 <- admz.agg[k]
#     timz <- gsub("_|\\.RData","",sapply(strsplit(filez.agg[k],a2),"[[",2))
#     timz  
#     srz <- gsub(paste0("_",cntz.agg[k],"_",a2,"_",timz,".RData"),"",filez.agg[k])
#     srz
#     
#     # Country borders
#     gadm0 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz.agg[k],"_adm",0,".rds",sep=""))
#     
#     # Load map
#     if(grepl("adm",a2)){
#       gadm <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz.agg[k],"_",a2,".rds",sep=""))
#     }
#     
#     # Russia subset
#     if(cntz.agg[k]=="RUS"&(!grepl("adm0",a2))){
#       gadm1 <- readRDS(paste0("Input/GIS/Borders/GADM/",cntz.agg[k],"_adm",1,".rds",sep=""))
#       reg.cauc <- c("Adygey","Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","Krasnodar","North Ossetia","Stavropol'")
#       reg.skfo <- c("Chechnya","Dagestan","Ingush","Kabardin-Balkar","Karachay-Cherkess","North Ossetia","Stavropol'")
#       reg.ufo <- c("Adygey","Astrakhan'","Kalmyk","Krasnodar","Rostov","Volgograd")
#       gadm1 <- gadm1[gadm1$NAME_1%in%c(reg.cauc),]
#       gadm00 <- gadm0
#       gadm0 <- crop(gadm0,gadm1)
#       gadm <- crop(gadm,gadm1)
#     }
#     
#     # PRIO
#     if(a=="priogrid"){
#       load("Input/GIS/Borders/PRIOGRID/PRIOGRID.RData")
#       proj4string(map0) <- proj4string(gadm0)
#       coords0 <- as.data.frame(map0)[,c("xcoord","ycoord")]
#       sp.data0 <- SpatialPoints(coords0,proj4string=CRS(proj4string(gadm0)))
#       sp.box <- as(extent(bbox(gadm0)),"SpatialPolygons")
#       proj4string(sp.box) <- proj4string(gadm0)
#       o <- over(map0, as(sp.box, "SpatialPolygons"))
#       map.crop <- map0[which(!is.na(o)),]
#       o <- over(map.crop, as(gadm0, "SpatialPolygons"))
#       map.crop <- map.crop[which(!is.na(o)),]
#       names(map.crop) <- paste0("PRIO_",toupper(names(map.crop)))
#       gadm <- map.crop;rm(map0,coords0,sp.data0,sp.box,o,map.crop)
#     }
#     
#     # CLEA
#     if(a=="clea"){
#       clea.cnt <- dir("Input/GIS/Borders/CLEA/")[grep(countrycode(cntz.agg[k], origin = "iso3c",destination = "country.name"),dir("Input/GIS/Borders/CLEA/"))]
#       map0 <- readShapePoly(paste0(paste0("Input/GIS/Borders/CLEA/",clea.cnt),"/",sapply(strsplit(dir(paste0("Input/GIS/Borders/CLEA/",clea.cnt))[1],"\\."),"[[",1)))
#       proj4string(map0) <- proj4string(gadm0)
#       head(map0)
#       coords0 <- coordinates(map0)
#       sp.data0 <- SpatialPoints(coords0,proj4string=CRS(proj4string(gadm0)))
#       sp.box <- as(extent(bbox(gadm0)),"SpatialPolygons")
#       proj4string(sp.box) <- proj4string(gadm0)
#       o <- over(map0, as(sp.box, "SpatialPolygons"))
#       map.crop <- map0[which(!is.na(o)),]
#       o <- over(map.crop, as(gadm0, "SpatialPolygons"))
#       map.crop <- map.crop[which(!is.na(o)),]
#       names(map.crop) <- paste0("CLEA_",toupper(names(map.crop)))
#       names(map.crop) <- gsub("\\_\\d{4}","",names(map.crop))
#       gadm <- map.crop;rm(map0,coords0,sp.data0,sp.box,o,map.crop)
#     }
#     
#     # Find level of analysis
#     space.ag0 <- a2
#     time.ag0 <- "year"
#     space.ix0 <- space.ix[match(space.ag0,space.agg)]
#     time.ix0 <- time.ix[match(time.ag0,time.agg)]
#     
#     # Create map layers
#     agg.data <- aggregate(data[,c("SIDEA_ANY","SIDEB_ANY","ACTION_ANY")],by=list(UNIT=data[,space.ix0]),FUN=function(x){sum(x,na.rm=T)})
#     head(agg.data)
#     gadm <- merge(gadm,agg.data,by.x=space.ix0,by.y="UNIT",all.x=T,all.y=F)
#     head(gadm)
#     
#     # Color
#     v <- c(0, 1, 10, 50, 100, max(500,max(gadm$ACTION_ANY,na.rm=T)+1))
#     if(grepl("yzChechnya|yzLibya",filez.agg[k])){v <- c(0, 1, 10, 100, 1000, max(10000,max(gadm$ACTION_ANY,na.rm=T)+1))}
#     if(grepl("yzCaucasus2000|yzUkraine2014",filez.agg[k])){v <- c(0, 10, 100, 1000, 10000, max(100000,max(gadm$ACTION_ANY,na.rm=T)+1))}
#     if((srz%in%c("ACLED","GED")&cntz.agg[k]%in%c("SLE"))|(srz%in%c("GED")&cntz.agg[k]%in%c("NPL"))){v <- c(0, 10, 100, 1000, 10000, max(100000,max(gadm$ACTION_ANY,na.rm=T)+1))}
#     if(a=="adm0"){v <- c(0, 10, 1000, 5000, 10000, max(100000,max(gadm$ACTION_ANY,na.rm=T)+1))}
#     cols <- cols.pal[findInterval(gadm$ACTION_ANY, v)]
#     
#     # Add layer to plot
#     if(cntz.agg[k]=="RUS"&(!grepl("adm0",a2))){plot(gadm00,col=palz[1],add=T,border="white",lwd=ifelse(a%in%c("adm2","priogrid"),.05,.1))}
#     plot(gadm,col=cols,add=T,border="white",lwd=ifelse(a%in%c("adm2","priogrid"),ifelse(a%in%c("priogrid"),.00001,.05),.1))
#     if(!(cntz.agg[k]=="RUS"&(!grepl("adm0",a2)))){plot(gadm0,col=NA,border=rgb(0.1,0.1,0.1,.25),lwd=.5,add=T)}
#     if(cntz.agg[k]=="RUS"&(!grepl("adm0",a2))){plot(gadm00,col=NA,border=rgb(0.1,0.1,0.1,.25),lwd=.5,add=T)}
#     
#   }
#   
#   # Finish plot
#   # plot(basemap,add=T,col=NA,border="gray60",lwd=.5)
#   legend(x="bottomleft",fill=c("white"),legend=c(paste0("Level of violence (per ",space.lab[s],")")),bg=rgb(1,1,1,.75),ncol=1,cex=.75,border=NA,box.lwd=0)
#   if(a!="adm0"){legend(x="bottomright",fill=c(cols.pal),legend=c("0","<10","<50","<100",">100"),bg=rgb(1,1,1,.75),ncol=6,cex=.75,border=NA,box.lwd=0)}
#   if(a=="adm0"){legend(x="bottomright",fill=c(cols.pal),legend=c("<10","<1000","<5000","<10,000",">10,000"),bg=rgb(1,1,1,.75),ncol=6,cex=.75,border=NA,box.lwd=0)}
#   
#   dev.off()
# 
#   
# }





###########################################
## Individual country maps
###########################################

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
srzz <- c("ABADarfur","ACLED","Beissinger_Protest","Beissinger_Riot","Beissinger_Ukraine","ESOC_Afghanistan","ESOC_Iraq","ESOC_Iraq_WITS","ESOC_Mexico_drug_related_murders","ESOC_Mexico_homicide","ESOC_Pakistan","ESOC_Pakistan_WITS","FM_France","GED","NVMS","PITF","SCAD","Sullivan_Guatemala","yzCaucasus2000","yzChechnya","yzLibya","yzUkraine2014" )
space.agg <- c("adm0","adm1","adm2","priogrid","clea")
time.agg <- c("year","month","week","day")
space.ix <- c("ID_0","ID_1","ID_2","PRIO_GID","CLEA_CST")
time.ix <- c("YEAR","YRMO","WID","TID")
noadm1 <- c("MLT")
noadm2 <- c("LBY","LSO","BHR","COM","ARE","ISR","KWT","MLT","SAU")
s <- 4
k <- 1
a <- 3
filez <- dir("Upload/data_rdata_country/")
filez <- filez[grep("_year.RData",filez)]
# Remaining only
# filez <- filez[!gsub("_year.RData","",filez)%in%gsub("map_|.png","",dir("Upload/graphics/maps/"))]
# filez <- filez[!gsub("_year.RData","",filez)%in%gsub("map_|.png","",dir("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/maps/"))]
filez <- filez[!gsub("_year.RData","",filez)%in%gsub("map_|.png","",dir("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/maps_small/"))]

# filez <- filez[!grepl("priogrid",filez)]
cntz <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez),"_"),"[[",2)
admz <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez),"_"),"[[",3)

# Load world map
wrld_gadm <- readShapePoly("Input/GIS/Covariates/GADM_World/gadm28_adm0")
proj4string(wrld_gadm) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

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
k <- 24
filez[k]
write.file <- 'R_progress.txt'
mclapply(1:length(filez),function(k){print(filez[k])
  # monitor progress
  file.create(write.file)
  fileConn<-file(write.file)
  writeLines(paste0(k,'/',length(filez),' ',round(k/length(filez),4)), fileConn)
  close(fileConn)
  # monitor from a console with
  # tail -c +0 -f ~/Dropbox2/Dropbox\ \(Zhukov\ research\ team\)/XSub/Data/R_progress.txt  
  
  # # Windows
  # foreach(k=1:length(cntz),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(cntz[k])
  # lapply(list.of.packages, require, character.only = TRUE)
  
  print(paste0(filez[k]))
  
  data <- load(paste0("Upload/data_rdata_country/",filez[k]))
  data <- get(data); rm(indata)
  cntz[k]
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
        gadm <- crop(gadm,gadm1)
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
        cnm <- countrycode(cntz[k], origin = "iso3c",destination = "country.name")
        if(cnm%in%c("Russian Federation")){cnm <- "Russia"}
        cnm <- gsub(" ","",cnm)
        # cnm <- gsub("DominicanRepublic","Dominican Republic",cnm)
        # cnm <- gsub("SaintLucia","Saint Lucia",cnm)
        # cnm <- gsub("MexicoPR","Mexico",cnm)
        # cnm <- gsub("SouthAfrica","South Africa",cnm)
        clea.cnt <- dir("Input/GIS/Borders/CLEA/Beta/")[grepl(cnm,dir("Input/GIS/Borders/CLEA/Beta"))&grepl("^GRED_",dir("Input/GIS/Borders/CLEA/Beta"))&grepl(".shp$",dir("Input/GIS/Borders/CLEA/Beta"))]
        map0 <- readShapePoly(paste0("Input/GIS/Borders/CLEA/Beta/",gsub(".shp$","",clea.cnt)))
        proj4string(map0) <- proj4string(gadm0)
        names(map0) <- paste0("CLEA_",toupper(names(map0)))
        clea.yr <- sapply(strsplit(clea.cnt,"_"),"[",3)
        clea.cst <- gsub(paste0("^",map0$CLEA_CTR[1],"0+"), "\\1", map0$CLEA_LINK, perl = TRUE)
        map0$CLEA_CST <- paste0(map0$CLEA_CTR[1],".",clea.yr,".",clea.cst)
        head(map0)
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
      print(filez[k])
      
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

      # Create map layers
      agg.data <- aggregate(data[,c("SIDEA_ANY","SIDEB_ANY","ACTION_ANY")],by=list(UNIT=data[,space.ix0]),FUN=function(x){sum(x,na.rm=T)})
      head(agg.data)
      gadm <- merge(gadm,agg.data,by.x=space.ix0,by.y="UNIT",all.x=T,all.y=F)
      head(gadm)
      bb <- bbox(gadm0)
      bb[1,1] <- bb[1,1]-(bb[1,2]-bb[1,1])/8
      bb[1,2] <- bb[1,2]+(bb[1,2]-bb[1,1])/8
      bb[2,1] <- bb[2,1]-(bb[2,2]-bb[2,1])/8
      bb[2,2] <- bb[2,2]+(bb[2,2]-bb[2,1])/8
      WKTs <- paste("POLYGON((",bb[1,1],bb[2,1],",",bb[1,2],bb[2,1],",",bb[1,2],bb[2,2],",",bb[1,1],bb[2,2],",",bb[1,1],bb[2,1],"))")
      test <- readWKT(WKTs)
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
      if(!((!grepl("yz",filez[k]))&cntz[k]=="RUS")){
        basemap <- crop(x = basemap,y=test)
      }
      if((!grepl("yz",filez[k]))&cntz[k]=="RUS"){
      basemap <- SpatialPolygons2PolySet(basemap)
      basemap.clipped <- clipPolys(basemap, xlim = bbox(test)[1,], ylim = bbox(test)[2,])
      basemap <- PolySet2SpatialPolygons(basemap.clipped, close_polys=TRUE)
      }
      
      # # Color
      palz <- c("gray90","dodgerblue1","dodgerblue2")
      # palz <- c("gray90","red","darkred")
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
      if(cntz[k]%in%"COD"){cname <- "D.R. Congo"}
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
      cname.xy <- coordinates(test)
      
      
      # Plot (thumbnail)
      # png(paste0("Upload/graphics/maps/map_",gsub("_year.RData","",filez[k]),".png"),height=3,width=3,units = "in",res=300)
      png(paste0("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/maps_small/map_",gsub("_year.RData","",filez[k]),".png"),height=2,width=2,units = "in",res=100)
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
      
      # # Plot
      # # png(paste0("Upload/graphics/maps/map_",gsub("_year.RData","",filez[k]),".png"),height=3,width=3,units = "in",res=300)
      # png(paste0("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/maps/map_",gsub("_year.RData","",filez[k]),".png"),height=3,width=3,units = "in",res=300)
      # layout(matrix(c(rep(1,11),2), 12, 1, byrow = TRUE))
      # par(mar=rep(0,4),xpd=NA)
      # plot(test,col="lightgrey",border=NA,density=30,xlim=bbox(test)[1,],ylim=bbox(test)[2,])
      # plot(test,col="lightgrey",border=NA,density=30,angle=-45,add=T)
      # plot(basemap,add=T,col="gray95",border="gray60",lwd=.5)
      # plot(gadm,col=cols,add=T,border="white",lwd=ifelse(a=="priogrid",ifelse(cntz[k]%in%"RUS"&(!grepl("yz",filez[k])),.05,.35),ifelse(a=="adm2",ifelse(cntz[k]%in%"RUS",.1,.2),.5)))
      # plot(gadm0,col=NA,border=rgb(0.1,0.1,0.1,.25),lwd=2,add=T)
      # #text(x=cname.xy[1],y=cname.xy[2],labels=paste(cname,drange,sep="\n"),cex=1.5,font=2)
      # text(x=cname.xy[1],y=cname.xy[2],labels=cname,cex=2,font=2)
      # par(mar=rep(0,4))
      # plot.new()
      # legend(x="top",fill=cols.pal,legend=paste0(c("","<","<","<",">"),v[c(1:4,4)]),bty="n",ncol=5,cex=.9,border=NA)
      # dev.off()
      # 
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




###########################################
## Individual country timelines
###########################################

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
srzz <- c("ABADarfur","ACLED","BeissingerProtest","BeissingerRiot","BeissingerUkraine","ESOCAfghanistanWITS","ESOCIraqSIGACT","ESOCIraqWITS","ESOCMexicoDrugRelatedMurders","ESOCMexicoHomicide","ESOCPakistanBFRS","ESOCPakistanWITS","FM","GED","NVMS","PITF","SCAD","SullivanGuatemala","yzCaucasus2000","yzChechnya","yzLibya","yzUkraine2014" )
space.agg <- c("adm0","adm1","adm2","priogrid","clea")
time.agg <- c("year","month","week","day")
space.ix <- c("ID_0","ID_1","ID_2","PRIO_GID","CLEA_CST")
time.ix <- c("YEAR","YRMO","WID","TID")
noadm1 <- c("MLT")
noadm2 <- c("LBY","LSO","BHR","COM","ARE","ISR","KWT","MLT","SAU")
s <- 4
k <- 1
a <- 3
filez <- dir("Upload/data_rdata_country/")
filez <- filez[grep("adm0",filez)]
# # Only remaining
# filez <- filez[!gsub("_adm0|_adm1|_adm2|_priogrid|_clea|.RData","",filez)%in%gsub("time_|.png","",dir("Upload/graphics/time/general/"))]
cntz <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez),"_"),"[[",2)
admz <- sapply(strsplit(gsub(paste(srzz,collapse="|"),"",filez),"_"),"[[",3)

# # Load world map
# wrld_gadm <- readShapePoly("Input/GIS/Covariates/GADM_World/gadm28_adm0")
# proj4string(wrld_gadm) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

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
k <- 10
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
  
  print(paste0(filez[k]))
  
  data <- load(paste0("Upload/data_rdata_country/",filez[k]))
  data <- get(data); rm(indata)
  cntz[k]
  a <- admz[k]
  a
  timz <- gsub("_|\\.RData","",sapply(strsplit(filez[k],a),"[[",2))
  timz  
  srz <- gsub(paste0("_",cntz[k],"_",a,"_",timz,".RData"),"",filez[k])
  srz
  
  # # Color
  palz <- c("gray90","dodgerblue1","dodgerblue2")

  # Ticker
  datez <- seq(as.Date("1900-01-01"), as.Date("2018-01-01"), by="days")
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
  
  # Date range
  minyr <- as.numeric(as.character(indata.t[which(indata.t$ACTION_ANY>0)[1],"YEAR"]))
  if(minyr>=1989){yrm <- 198812}
  if(minyr>=1979&minyr<1989){yrm <- 197812}
  if(minyr>=1969&minyr<1979){yrm <- 196812}
  if(minyr>=1959&minyr<1969){yrm <- 195812}
  if(minyr>=1949&minyr<1959){yrm <- 194812}
  if(minyr>=1939&minyr<1949){yrm <- 193812}

  # ## PLOT
  # # general
  # X <- indata.t[indata.t$YRMO>yrm,]
  # X$TID0 <- 1:nrow(X)
  # tail(X)
  # if(sum(X$ACTION_ANY)==0){ylimz <- l.j <- 0:1}
  # if(sum(X$ACTION_ANY)>0){ylimz <- range(X$ACTION_ANY,na.rm=T); l.j <- pretty(range(X$ACTION_ANY,na.rm=T))}
  # png(paste0("Upload/graphics/time/general/time_",srz,"_",cntz[k],"_",timz,".png"),height=3,width=10,units="in",res=300)
  # par(mar=c(2,3,0.5,0.5))
  # plot(x=X$TID0,y=X$ACTION_ANY,ylim=ylimz,xaxt="n",yaxt="n",bty="n",col=NA)
  # axis(1,at =  which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),labels=NA)
  # axis(1,at=which(grepl("001$|501$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),labels=substr(X$YRMO,1,4)[which(grepl("001$|501$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE)))],lwd=0,lwd.ticks=2)
  # # axis(1,at=grep("00101$|50101$",X$DATE),labels=substr(X$YRMO,1,4)[grep("00101$|50101$",X$DATE)],lwd=0,lwd.ticks=2)
  # l.i <- pretty(which(X$LABS!=""))
  # l.i <- l.i[l.i<=nrow(X)]
  # l.j <- l.j[l.j%%1==0]
  # # axis(1,at = l.i,labels=substr(X$YRMO,1,4)[l.i+1],tick = FALSE,cex=.8)
  # axis(2,at = l.j,las=1,cex.axis=.8)
  # rect(xleft = X$TID0[1]-1,xright = X$TID0[length(X$TID0)]+1,ybottom = l.j[1],ytop = l.j[length(l.j)],col = "gray90",border = NA)
  # abline(h=l.j,col="white",lwd=1)
  # abline(h=l.j+diff(l.j)[1]/2,col="white",lwd=.5)
  # 
  # if(yrm==198812){
  # if(timz=="year"){
  #   rect(xleft = X$TID0,xright = X$TID0+1,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  #   abline(v=X$TID0,col="white",lwd=.5)
  # }
  # if(timz%in%c("month")){
  #   abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #   rect(xleft = X$TID0+.2,xright = X$TID0+.8,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  # }
  # if(timz%in%c("week")){
  #   abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #   rect(xleft = X$TID0,xright = X$TID0+1,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  # }
  # if(timz%in%c("day")){
  #   abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #   segments(x0 = X$TID0,x1 = X$TID0,y0 = rep(0,nrow(X)),y1 = X$ACTION_ANY,col=palz[length(palz)],lwd=.5,lend=2)
  # }
  # }
  # 
  # if(yrm==197812){
  # if(timz=="year"){
  #   rect(xleft = X$TID0,xright = X$TID0+1,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  #   abline(v=X$TID0,col="white",lwd=.5)
  # }
  # if(timz%in%c("month")){
  #   abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #   rect(xleft = X$TID0+.2,xright = X$TID0+.8,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  # }
  # if(timz%in%c("week")){
  #   abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #   rect(xleft = X$TID0,xright = X$TID0+1,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  # }
  # if(timz%in%c("day")){
  #   abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #   segments(x0 = X$TID0,x1 = X$TID0,y0 = rep(0,nrow(X)),y1 = X$ACTION_ANY,col=palz[length(palz)],lwd=.5,lend=2)
  # }
  # }
  # 
  # if(yrm<=196812){
  #   if(timz=="year"){
  #     rect(xleft = X$TID0,xright = X$TID0+1,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  #     abline(v=X$TID0,col="white",lwd=.5)
  #   }
  #   if(timz%in%c("month")){
  #     abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #     rect(xleft = X$TID0+.05,xright = X$TID0+.95,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  #   }
  #   if(timz%in%c("week")){
  #     abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #     segments(x0 = X$TID0,x1 = X$TID0,y0 = rep(0,nrow(X)),y1 = X$ACTION_ANY,col=palz[length(palz)],lwd=.5,lend=2)
  #   }
  #   if(timz%in%c("day")){
  #     abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #     segments(x0 = X$TID0,x1 = X$TID0,y0 = rep(0,nrow(X)),y1 = X$ACTION_ANY,col=palz[length(palz)],lwd=.5,lend=2)
  #   }
  # }
  # dev.off()
  # 
  # 
  # # general (low res)
  # X <- indata.t[indata.t$YRMO>yrm,]
  # X$TID0 <- 1:nrow(X)
  # tail(X)
  # if(sum(X$ACTION_ANY)==0){ylimz <- l.j <- 0:1}
  # if(sum(X$ACTION_ANY)>0){ylimz <- range(X$ACTION_ANY,na.rm=T); l.j <- pretty(range(X$ACTION_ANY,na.rm=T))}
  # png(paste0("Upload/graphics/time_small/general/time_",srz,"_",cntz[k],"_",timz,".png"),height=3,width=10,units="in",res=75)
  # par(mar=c(2,3,0.5,0.5))
  # plot(x=X$TID0,y=X$ACTION_ANY,ylim=ylimz,xaxt="n",yaxt="n",bty="n",col=NA)
  # axis(1,at =  which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),labels=NA)
  # axis(1,at=which(grepl("001$|501$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),labels=substr(X$YRMO,1,4)[which(grepl("001$|501$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE)))],lwd=0,lwd.ticks=2)
  # # axis(1,at=grep("00101$|50101$",X$DATE),labels=substr(X$YRMO,1,4)[grep("00101$|50101$",X$DATE)],lwd=0,lwd.ticks=2)
  # l.i <- pretty(which(X$LABS!=""))
  # l.i <- l.i[l.i<=nrow(X)]
  # l.j <- l.j[l.j%%1==0]
  # # axis(1,at = l.i,labels=substr(X$YRMO,1,4)[l.i+1],tick = FALSE,cex=.8)
  # axis(2,at = l.j,las=1,cex.axis=.8)
  # rect(xleft = X$TID0[1]-1,xright = X$TID0[length(X$TID0)]+1,ybottom = l.j[1],ytop = l.j[length(l.j)],col = "gray90",border = NA)
  # abline(h=l.j,col="white",lwd=1)
  # abline(h=l.j+diff(l.j)[1]/2,col="white",lwd=.5)
  # 
  # if(yrm==199812){
  #   if(timz=="year"){
  #     rect(xleft = X$TID0,xright = X$TID0+1,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  #     abline(v=X$TID0,col="white",lwd=.5)
  #   }
  #   if(timz%in%c("month")){
  #     abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #     rect(xleft = X$TID0+.2,xright = X$TID0+.8,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  #   }
  #   if(timz%in%c("week")){
  #     abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #     rect(xleft = X$TID0,xright = X$TID0+1,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  #   }
  #   if(timz%in%c("day")){
  #     abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #     segments(x0 = X$TID0,x1 = X$TID0,y0 = rep(0,nrow(X)),y1 = X$ACTION_ANY,col=palz[length(palz)],lwd=.5,lend=2)
  #   }
  # }
  # 
  # if(yrm==197812){
  #   if(timz=="year"){
  #     rect(xleft = X$TID0,xright = X$TID0+1,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  #     abline(v=X$TID0,col="white",lwd=.5)
  #   }
  #   if(timz%in%c("month")){
  #     abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #     rect(xleft = X$TID0+.2,xright = X$TID0+.8,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  #   }
  #   if(timz%in%c("week")){
  #     abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #     rect(xleft = X$TID0,xright = X$TID0+1,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  #   }
  #   if(timz%in%c("day")){
  #     abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #     segments(x0 = X$TID0,x1 = X$TID0,y0 = rep(0,nrow(X)),y1 = X$ACTION_ANY,col=palz[length(palz)],lwd=.5,lend=2)
  #   }
  # }
  # 
  # if(yrm<=196812){
  #   if(timz=="year"){
  #     rect(xleft = X$TID0,xright = X$TID0+1,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  #     abline(v=X$TID0,col="white",lwd=.5)
  #   }
  #   if(timz%in%c("month")){
  #     abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #     rect(xleft = X$TID0+.05,xright = X$TID0+.95,ybottom = rep(0,nrow(X)),ytop = X$ACTION_ANY,col=palz[length(palz)],border=NA)
  #   }
  #   if(timz%in%c("week")){
  #     abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #     segments(x0 = X$TID0,x1 = X$TID0,y0 = rep(0,nrow(X)),y1 = X$ACTION_ANY,col=palz[length(palz)],lwd=.5,lend=2)
  #   }
  #   if(timz%in%c("day")){
  #     abline(v=which(grepl("01$",X$YRMO)&(!duplicated(X$YRMO,fromLast = FALSE))),lwd=.5,col="white")
  #     segments(x0 = X$TID0,x1 = X$TID0,y0 = rep(0,nrow(X)),y1 = X$ACTION_ANY,col=palz[length(palz)],lwd=.5,lend=2)
  #   }
  # }
  # dev.off()
  # 
  
  # start-finish
  dat.events <- which(indata.t$ACTION_ANY>0)
  padz <- 5*c(1,12,52,365)[match(timz,c("year","month","week","day"))]
  X <- indata.t[indata.t$YRMO>=indata.t$YRMO[max(1,-padz+dat.events[1])]&indata.t$YRMO<=indata.t$YRMO[min(nrow(indata.t), padz+max(dat.events))],]
  head(X)
  if(nrow(na.omit(X))==0){X <- indata.t[indata.t$YRMO>198812,]; ylimz <- l.j <- 0:1}
  if(nrow(na.omit(X))>0){ylimz <- range(X$ACTION_ANY,na.rm=T); l.j <- pretty(range(X$ACTION_ANY,na.rm=T))}
  X$TID0 <- 1:nrow(X)
  # png(paste0("Upload/graphics/time/startfinish/time_",srz,"_",cntz[k],"_",timz,".png"),height=3,width=10,units="in",res=300)
  png(paste0("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/time/time_",srz,"_",cntz[k],"_",timz,".png"),height=3,width=10,units="in",res=300)
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
  
  
  
  # start-finish
  dat.events <- which(indata.t$ACTION_ANY>0)
  padz <- 5*c(1,12,52,365)[match(timz,c("year","month","week","day"))]
  X <- indata.t[indata.t$YRMO>=indata.t$YRMO[max(1,-padz+dat.events[1])]&indata.t$YRMO<=indata.t$YRMO[min(nrow(indata.t), padz+max(dat.events))],]
  head(X)
  if(nrow(na.omit(X))==0){X <- indata.t[indata.t$YRMO>198812,]; ylimz <- l.j <- 0:1}
  if(nrow(na.omit(X))>0){ylimz <- range(X$ACTION_ANY,na.rm=T); l.j <- pretty(range(X$ACTION_ANY,na.rm=T))}
  X$TID0 <- 1:nrow(X)
  # png(paste0("Upload/graphics/time_small/startfinish/time_",srz,"_",cntz[k],"_",timz,".png"),height=3,width=10,units="in",res=75)
  png(paste0("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/graphics/time_small/time_",srz,"_",cntz[k],"_",timz,".png"),height=3,width=10,units="in",res=300)
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
  
  
 
      
  # Close a loop
  
  
  # # Close loop (single core)
  # }
  
  # Close loop: countries (Linux/Mac)
},mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)

# # Close loop (Windows)
# }
# stopCluster(cl)





