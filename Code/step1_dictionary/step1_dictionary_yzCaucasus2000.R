rm(list=ls())


## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

## Load custom functions
source("Code/functions.R")

# #############################
# ## Clean up data
# #############################
# 
# # Load events
# yz.raw <- load("Input/Events/Zhukov/Russia/EVENTS.RData")
# yz.raw <- get(yz.raw); rm(events)
# head(yz.raw)
# names(yz.raw)
# 
# # Convert dummies to factors
# yz.raw$ACTORS <- ""
# actor.varz <- c(8:24,61:69)
# actors0 <- apply(yz.raw[,actor.varz], 1, function(x){which(x == 1)})
# i <- 14
# temp.list <- lapply(1:nrow(yz.raw),function(i){#print(i)
#   mat <- yz.raw[i,]
#   mat
#   if(sum(yz.raw[i,actor.varz])==0){
#     mat$ACTORS <- ""
#   }
#   if(sum(yz.raw[i,actor.varz])==1){
#     mat$ACTORS <- names(actors0[[i]])
#   }
#   if(sum(yz.raw[i,actor.varz])>1){
#     mat <- yz.raw[rep(i,sum(yz.raw[i,actor.varz])),]
#     mat$ACTORS <- names(actors0[[i]])
#   }
#   mat
# })
# yz.raw <- do.call(rbind,temp.list)
# row.names(yz.raw) <- 1:nrow(yz.raw)
# 
# i <- 1
# yz.raw$TYPE <- ""
# type.varz <- c(25:60)
# type0 <- apply(yz.raw[,type.varz], 1, function(x){which(x == 1)})
# i <- 1
# for(i in 1:nrow(yz.raw)){yz.raw$TYPE[i] <- paste(names(type0[[i]]),collapse=" ")}
# 
# # Write to file
# tail(yz.raw)
# save(yz.raw,file="Input/Events/Zhukov/Russia/EVENTS_clean.RData")

#############################
## Create actor dictionary
#############################

load("Input/Events/Zhukov/Russia/EVENTS_clean.RData")

# Loop by country (first line finds k where you left off)
subdata <- yz.raw
length(sort(unique(subdata$ACTORS)))
actorlist <- actorList(subdata,sidea="ACTORS",sideb="ACTORS")
save(actorlist,file=paste0("Dictionaries/yzCaucasus2000/yzCaucasus2000_Actors.RData"))
