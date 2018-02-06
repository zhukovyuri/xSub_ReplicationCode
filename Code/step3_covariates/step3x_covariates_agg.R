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
  z <- data.frame(POP=z)
  names(z) <- paste0("POP_",year)
  row.names(z) <- row.names(map00)
  map00 <- spCbind(map00,z)
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
z <- data.frame(ELEV_MEAN=z)
row.names(z) <- row.names(map00)
map00 <- spCbind(map00,z)
# SD
z <- extract(r,map00,fun=sd,factors=T,buffer=1000000, small=T,na.rm=T)
z <- data.frame(ELEV_SD=z)
row.names(z) <- row.names(map00)
map00 <- spCbind(map00,z)
# Max
z <- extract(r,map00,fun=max,factors=T,buffer=1000000, small=T,na.rm=T)
z <- data.frame(ELEV_MAX=z)
row.names(z) <- row.names(map00)
map00 <- spCbind(map00,z)
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
head(z)
z <- data.frame(OPEN_TERRAIN=z)
row.names(z) <- row.names(map00)
map00 <- spCbind(map00,z)
# Forest
rf <- 1*(r%in%forest)
z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
head(z)
z <- data.frame(FOREST=z)
row.names(z) <- row.names(map00)
map00 <- spCbind(map00,z)
# Wetland
rf <- 1*(r%in%wetlands)
z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
head(z)
z <- data.frame(WETLAND=z)
row.names(z) <- row.names(map00)
map00 <- spCbind(map00,z)
# Farmland
rf <- 1*(r%in%farm)
z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
head(z)
z <- data.frame(FARMLAND=z)
row.names(z) <- row.names(map00)
map00 <- spCbind(map00,z)
# Urban
rf <- 1*(r%in%urban)
z <- extract(rf,map00,fun=mean,factors=T,buffer=100000, small=T,na.rm=T)
head(z)
z <- data.frame(URBAN=z)
row.names(z) <- row.names(map00)
map00 <- spCbind(map00,z)
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
map00 <- spCbind(map00,gregmat)
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
map00 <- spCbind(map00,langmat)
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
map00 <- spCbind(map00,bumat)
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
map00 <- spCbind(map00,ptmat)
summary(map00)
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
if(length(map00)==1){capmat <- data.frame(DIST2PROVCAP=apply(dmat,1,min)*111.32,DIST2CAP=min(dmat[,capz])*111.32)}
if(length(map00)>1){capmat <- data.frame(DIST2PROVCAP=apply(dmat,1,min)*111.32,DIST2CAP=dmat[,capz]*111.32)}
head(capmat)
row.names(capmat) <- row.names(map00)
map00 <- spCbind(map00,capmat)
}
summary(map00)


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
map00 <- spCbind(map00,rlmat)
summary(map00)
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
