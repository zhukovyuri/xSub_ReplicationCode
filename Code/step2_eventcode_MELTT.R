rm(list=ls())


## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
if(grepl("w64",sessionInfo()[[1]][[1]])){setwd("D:/Dropbox (Zhukov research team)/XSub/Data/")}
if(grepl("Ubuntu 18.04 LTS"
         ,sessionInfo()[[4]])){setwd("/mnt/oldhdd/home/zhukov/Dropbox (Zhukov research team)/XSub/Data/")}

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","parallel","meltt")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")

# # Event types
# source("Code/step2_eventcode/step2x_eventType_function.R")
# source("Code/step2_eventcode/step2x_event_types_list.R")

# General vs. specific event categories
specific <- TRUE

# High-precision events only?
precis <- TRUE

# Undirected events?
undir <- FALSE

#############################
## Load event data
#############################

# Data sources
srzz <- dir("Output")
srzz <- srzz[!srzz%in%c("Maps","Output_ACD","Output_ESOC","Output_FM","Output_SullivanGuatemala","Output_cdRwanda","Output_Covariates","Output_Weather","OLD","Output_ACLED0","Output_GED0")]
srzz <- srzz[!grepl("MELTT",srzz)]
srzz <- gsub("Output_","",srzz)
if(!undir){srzz <- srzz[!srzz%in%c("ACLED","GED")]}

s <- 7
ss <- 1

# Combine all events in list
event.list <- lapply(seq_along(srzz),function(s){print(s)
  filez.events <- paste0(paste0("Output/Output_",srzz[s],"/Events/"),dir(paste0("Output/Output_",srzz[s],"/Events")))
  filez.event.types <- paste0(paste0("Output/Output_",srzz[s],"/Events/EventType/"),dir(paste0("Output/Output_",srzz[s],"/Events/EventType")))
  filez.events <- filez.events[!grepl("000.RData$",filez.events)]
  filez.events <- filez.events[grepl(".RData$",filez.events)]
  filez.event.types <- filez.event.types[grepl(".RData$",filez.event.types)]
  if(length(filez.events)){
    event.list2 <- lapply(seq_along(filez.events),function(ss){#print(ss)
      load(filez.events[ss])
      # Combine with specific categories?
      if(specific){
        load(filez.event.types[ss])
        events <- cbind(events,events0[,-1])
      }
      # Filter by precision?
      if(precis){
        events <- events[which(events$GEOPRECISION%in%c("settlement","adm2")&events$TIMEPRECISION%in%"day"),]
      }
      # Recode undirected dyads
      if(undir){
        events1 <- events
        events1$DYAD_A_B <- events1$DYAD_B_A <- apply(events[,c("DYAD_A_B","DYAD_B_A")],1,function(x){max(x,na.rm=T)})
        events1$DYAD_A_C <- events1$DYAD_C_A <- apply(events[,c("DYAD_A_C","DYAD_C_A")],1,function(x){max(x,na.rm=T)})
        events1$DYAD_A_D <- events1$DYAD_D_A <- apply(events[,c("DYAD_A_D","DYAD_D_A")],1,function(x){max(x,na.rm=T)})
        events1$DYAD_B_C <- events1$DYAD_C_B <- apply(events[,c("DYAD_B_C","DYAD_C_B")],1,function(x){max(x,na.rm=T)})
        events1$DYAD_B_D <- events1$DYAD_D_B <- apply(events[,c("DYAD_B_D","DYAD_D_B")],1,function(x){max(x,na.rm=T)})
        events1$DYAD_C_D <- events1$DYAD_D_C <- apply(events[,c("DYAD_C_D","DYAD_D_C")],1,function(x){max(x,na.rm=T)})
        events1$INITIATOR_SIDEA <- events1$TARGET_SIDEA <- apply(events[,c("INITIATOR_SIDEA","TARGET_SIDEA")],1,function(x){max(x,na.rm=T)})
        events1$INITIATOR_SIDEB <- events1$TARGET_SIDEB <- apply(events[,c("INITIATOR_SIDEB","TARGET_SIDEB")],1,function(x){max(x,na.rm=T)})
        events1$INITIATOR_SIDEC <- events1$TARGET_SIDEC <- apply(events[,c("INITIATOR_SIDEC","TARGET_SIDEC")],1,function(x){max(x,na.rm=T)})
        events1$INITIATOR_SIDED <- events1$TARGET_SIDED <- apply(events[,c("INITIATOR_SIDED","TARGET_SIDED")],1,function(x){max(x,na.rm=T)})
        events1$SIDEA_ANY <- apply(events[,c("INITIATOR_SIDEA","TARGET_SIDEA")],1,function(x){max(x,na.rm=T)})
        events1$SIDEB_ANY <- apply(events[,c("INITIATOR_SIDEB","TARGET_SIDEB")],1,function(x){max(x,na.rm=T)})
        events1$SIDEC_ANY <- apply(events[,c("INITIATOR_SIDEC","TARGET_SIDEC")],1,function(x){max(x,na.rm=T)})
        events1$SIDED_ANY <- apply(events[,c("INITIATOR_SIDED","TARGET_SIDED")],1,function(x){max(x,na.rm=T)})
        events1$SIDEA_IND <- 1*(apply(events[,c("INITIATOR_SIDEA","TARGET_SIDEA")],1,function(x){max(x,na.rm=T)})==1&events$ACTION_IND==1)
        events1$SIDEB_IND <- 1*(apply(events[,c("INITIATOR_SIDEB","TARGET_SIDEB")],1,function(x){max(x,na.rm=T)})==1&events$ACTION_IND==1)
        events1$SIDEC_IND <- 1*(apply(events[,c("INITIATOR_SIDEC","TARGET_SIDEC")],1,function(x){max(x,na.rm=T)})==1&events$ACTION_IND==1)
        events1$SIDED_IND <- 1*(apply(events[,c("INITIATOR_SIDED","TARGET_SIDED")],1,function(x){max(x,na.rm=T)})==1&events$ACTION_IND==1)
        events1$SIDEA_DIR <- 1*(apply(events[,c("INITIATOR_SIDEA","TARGET_SIDEA")],1,function(x){max(x,na.rm=T)})==1&events$ACTION_DIR==1)
        events1$SIDEB_DIR <- 1*(apply(events[,c("INITIATOR_SIDEB","TARGET_SIDEB")],1,function(x){max(x,na.rm=T)})==1&events$ACTION_DIR==1)
        events1$SIDEC_DIR <- 1*(apply(events[,c("INITIATOR_SIDEC","TARGET_SIDEC")],1,function(x){max(x,na.rm=T)})==1&events$ACTION_DIR==1)
        events1$SIDED_DIR <- 1*(apply(events[,c("INITIATOR_SIDED","TARGET_SIDED")],1,function(x){max(x,na.rm=T)})==1&events$ACTION_DIR==1)
        events1$SIDEA_PRT <- 1*(apply(events[,c("INITIATOR_SIDEA","TARGET_SIDEA")],1,function(x){max(x,na.rm=T)})==1&events$ACTION_PRT==1)
        events1$SIDEB_PRT <- 1*(apply(events[,c("INITIATOR_SIDEB","TARGET_SIDEB")],1,function(x){max(x,na.rm=T)})==1&events$ACTION_PRT==1)
        events1$SIDEC_PRT <- 1*(apply(events[,c("INITIATOR_SIDEC","TARGET_SIDEC")],1,function(x){max(x,na.rm=T)})==1&events$ACTION_PRT==1)
        events1$SIDED_PRT <- 1*(apply(events[,c("INITIATOR_SIDED","TARGET_SIDED")],1,function(x){max(x,na.rm=T)})==1&events$ACTION_PRT==1)
        if(nrow(events1)>0){events1[,grep("^SIDEC|^INITIATOR_SIDEC|^DYAD_C",names(events1))] <- 0}
        events[,grep("^INITIATIOR_|^TARGET_|^DYAD_|^SIDE",names(events))] <- events1[,grep("^INITIATIOR_|^TARGET_|^DYAD_|^SIDE",names(events1))]
        
      }
      events
    })
    events.0 <- do.call(rbind,event.list2)
    events.0
  }
})

cbind(srzz,sapply(event.list,nrow))

# # Make sure columns are same
# srzz[sapply(event.list,ncol)>median(sapply(event.list,ncol))]
# sapply(event.list,names)[sapply(event.list,ncol)>median(sapply(event.list,ncol))]

# Drop empty list items
srzz <- srzz[sapply(event.list,length)>0]
event.list <- event.list[sapply(event.list,length)>0]
srzz <- srzz[sapply(event.list,nrow)>0]
event.list <- event.list[sapply(event.list,nrow)>0]

# Re-order by size
srzz <- srzz[order(sapply(event.list,nrow))]
event.list <- event.list[order(sapply(event.list,nrow))]

sapply(event.list,ncol)

# Merge

event.mat <- do.call(rbind,event.list)
head(event.mat)
dim(event.mat)

# Check missing dates
range(event.mat$DATE)
head(event.mat[which(event.mat$DATE<19660000),])
dim(event.mat[which(is.na(event.mat$DATE)),])
summary(event.mat[which(is.na(event.mat$DATE)),])
summary(event.mat$ACTION_ANY)



#############################
## Prepare data for MELTT
#############################

head(event.mat)
event.meltt <- event.mat
event.meltt$date <- as.Date(event.mat$DATE,format="%Y%m%d")
event.meltt$longitude <- as.numeric(as.character(event.mat$LONG))
event.meltt$latitude <- as.numeric(as.character(event.mat$LAT))
event.meltt$LAT <- event.meltt$LONG <- event.meltt$DATE <- NULL

sum(is.na(event.mat$LAT))
sum(is.na(event.meltt$latitude))

# Drop missing locations & dates
head(event.mat[which(is.na(event.meltt$latitude)),])
event.meltt <- event.meltt[!is.na(event.meltt$latitude),]
event.meltt <- event.meltt[!is.na(event.meltt$date),]

# Replace missing with 0
event.varz <- names(event.meltt)[grep("^INITIATOR|^TARGET|^ACTION|^SIDE|^DYAD",names(event.meltt))]
event.meltt[,event.varz][is.na(event.meltt[,event.varz])] <- 0

# Simplify categories
event.varz1 <- names(event.meltt)[grepl("^ISO3$|^INITIATOR|^TARGET|^ACTION|^SIDE|^DYAD",names(event.meltt))]

# Loop by windows
windowz <- data.frame(t=rep(1:2,2),s=rep(c(1,5),each=2))
ww <- 1
for(ww in 1:nrow(windowz)){
  
  # Loop by source
  head(event.meltt)
  srzz <- as.character(unique(event.meltt$SOURCE))
  
  data1 <- event.meltt[event.meltt$SOURCE%in%srzz[1],]
  s <- 3
  for(s in seq_along(srzz)[-1]){print(paste(s,"/",srzz[s],"/",paste0("MELTT",windowz$s[ww],"km",windowz$t[ww],"d",ifelse(undir,"B","A"))))
    data2 <- event.meltt[event.meltt$SOURCE%in%srzz[s],]
    data2 <- data2[which(!is.na(data2$ISO3)),]
    # taxonomies list
    event_taxonomies <- lapply(seq_along(event.varz1),function(vv){
      tx <- rbind(
        data.frame(data.source="data1",base.categories=sort(unique(event.meltt[,event.varz1[vv]])),x=sort(unique(event.meltt[,event.varz1[vv]])))
        ,
        data.frame(data.source="data2",base.categories=sort(unique(event.meltt[,event.varz1[vv]])),x=sort(unique(event.meltt[,event.varz1[vv]])))
      )
      names(tx)[3] <- event.varz1[vv]
      tx
    })
    names(event_taxonomies) <- event.varz1
    
    # Combine
    head(data2)
    meltt.out <- meltt(data1,data2,taxonomies = event_taxonomies,twindow = windowz$t[ww],spatwindow = windowz$s[ww])
    print(meltt.out)
    meltt.out <- meltt.data(meltt.out)
    data1 <- meltt.out[,c("date","latitude","longitude",event.varz1)]
  }
  event.out <- data1
  head(event.out)
  
  # Restore variables
  event.out$SOURCE <- paste0("MELTT",windowz$s[ww],"km",windowz$t[ww],"d",ifelse(specific,"B","A"))
  event.out$CONFLICT <- countrycode(event.out$ISO3,origin = "iso3c",destination = "country.name")
  event.out$COWN <- countrycode(event.out$ISO3,origin = "iso3c",destination = "cown")
  event.out$COWC <- countrycode(event.out$ISO3,origin = "iso3c",destination = "cowc")
  event.out$DATE <- gsub("-","",event.out$date)
  event.out$LAT <- event.out$latitude
  event.out$LONG <- event.out$longitude
  event.out$GEOPRECISION <- "settlement"
  event.out$TIMEPRECISION <- "day"
  event.out <- event.out[,names(event.mat)]
  head(event.out)
  
  # Save (by country)
  disag <- sort(unique(as.character(event.out$ISO3)))
  for(j in seq_along(disag)){
    events <- event.out[event.out$ISO3%in%disag[j],]
    save(events,file=paste0("Output/Output_MELTT",windowz$s[ww],"km",windowz$t[ww],"d",ifelse(undir,"B","A"),"/Events/MELTT",windowz$s[ww],"km",windowz$t[ww],"d",ifelse(undir,"B","A"),"_Events_",countrycode(disag[j],origin = "iso3c",destination = "iso3c"),".RData"))  
  }
  
  # Close window loop
}

