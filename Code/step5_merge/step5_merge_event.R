rm(list=ls())

## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
if(grepl("w64",sessionInfo()[[1]][[1]])){setwd("D:/Dropbox (Zhukov research team)/XSub/Data/")}
if(grepl("Ubuntu 18.04 LTS"
         ,sessionInfo()[[4]])){setwd("/mnt/oldhdd/home/zhukov/Dropbox (Zhukov research team)/XSub/Data/")}

## Install & load packages (all at once)
list.of.packages <- c("parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# Parallel computing
mcoptions <- list(preschedule=FALSE, set.seed=TRUE)
ncores <- detectCores()
# cl <- makeCluster(ncores)
# registerDoParallel(cl)

## Install & load packages (all at once)
list.of.packages <- c("parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

#source("Code/functions.R")
source("Code/step5_merge/step5x_variable_select.R")


##########################################
# Event counts + covariates, No lags
##########################################

srzz <- gsub("Output_","",dir("Output"))
srzz <- srzz[!srzz%in%c("Maps","ACD","ESOC","Covariates","Weather","OLD","cdRwanda","FM","SullivanGuatemala")]
space.agg <- c("adm0","adm1","adm2","priogrid","clea")
space.ix <- c("ID_0","ID_1","ID_2","PRIO_GID","CLEA_CST")
time.agg <- c("year","month","week","day")
time.ix <- c("YEAR","YRMO","WID","TID")
srzz
s <- 17; srzz[s]

# Census of missing
filez.full <- unlist(lapply(seq_along(srzz),function(s){dir(paste0("Output/Output_",srzz[s],"/Events"))}))
filez.full <- filez.full[!grepl("EventType|ACLED_v|000",filez.full)]
filez.rename.full <- gsub(".RData","_event.RData",gsub("_Events_","_",filez.full))
filez.full <- filez.full[!filez.rename.full%in%dir("Upload/data_rdata_event/")]
filez.rename.full <- filez.rename.full[!filez.rename.full%in%dir("Upload/data_rdata_event/")]

# Loop over sources
s <- 1
for(s in 1:length(srzz)){print(srzz[s])

  # Source select
  # s <- 1
  srz <- srzz[s]
  filez <- dir(paste0("Output/Output_",srz,"/Events"))
  filez <- filez[!grepl("EventType|ACLED_v",filez)]
  filez.rename <- gsub(".RData","_event.RData",gsub("_Events_","_",filez))
  
  # Remaining
  filez <- filez[!filez.rename%in%dir("Upload/data_rdata_event/")]
  filez.rename <- filez.rename[!filez.rename%in%dir("Upload/data_rdata_event/")]
  
  i0 <- 1; filez[i0]; i <- i0
  # filez <- filez[grep("clea",filez)]
  filez
  
  # # Single core
  # for(i in i0:length(filez)){print(paste0(filez[i]," (",i," of ",length(filez),")"))
  
  # Linux/Mac, multi-core
  # stopCluster(cl)
  mclapply(i0:length(filez),function(i){#print(paste0(filez[i]," (",i," of ",length(filez),")"))
  
    # # Windows, multi-core
    # foreach(i=i0:length(filez),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez[i]," (",i," of ",length(filez),")"))
    # lapply(list.of.packages, require, character.only = TRUE)
    
    # Events
    indata <- load(paste0("Output/Output_",srz,"/Events/",filez[i]))
    indata <- get(indata)
    summary(indata)
    
    if(!grepl("^MELTT",srz)){
      # Event Types
      dir("Output/Output_ACLED/Events/EventType/")
      indata.x <- load(paste0("Output/Output_",srz,"/Events/EventType/",gsub("_Events_","_EventType_",filez[i])))
      indata.x <- get(indata.x)
      # indata$ID_TEMP <- indata.x$ID_TEMP
      names(indata)
      names(indata.x)
    
      # Merge events & types
      indata <- cbind(indata,indata.x)
      head(indata)
    }
    
    # Missing columns (create empty column if missing)
    miss.cols <- c(event.type)[!c(event.type)%in%names(indata)]
    if(length(miss.cols)==0){
      indata <- indata[,c(event.type)]
    }
    if(length(miss.cols)>0){
      miss.mat <- as.data.frame(matrix(NA,nrow=nrow(indata),ncol=length(miss.cols)))
      names(miss.mat) <- miss.cols
      row.names(miss.mat) <- row.names(indata)
      indata <- cbind(indata,miss.mat)
      indata <- indata[,c(event.type)]
    }
    
    # Reorder rows
    indata <- indata[,c(event.type)]
    indata <- indata[order(indata$DATE,indata$LAT,indata$LONG),]
    row.names(indata) <- 1:nrow(indata)
    head(indata)
    
    
    # Write to file
    if("XSub"%in%dir()){
      write.csv(as.data.frame(indata), file=(paste0("~/XSub_upload/data_csv_event/",gsub("RData","csv",filez.rename)[i])),row.names = FALSE,na = "")
      }
    if("Dropbox2"%in%dir()){
      write.csv(as.data.frame(indata), file=(paste0("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/data_csv_event/",gsub("RData","csv",filez.rename)[i])),row.names = FALSE,na = "")
      }
    if(grepl("w64",sessionInfo()[[1]][[1]])){
      write.csv(as.data.frame(indata), file=(paste0("D:/Dropbox (Zhukov research team)/XSub_upload/data_csv_event/",gsub("RData","csv",filez.rename)[i])),row.names = FALSE,na = "")
      }
    if(grepl("Ubuntu 18.04 LTS",sessionInfo()[[4]])){
      write.csv(as.data.frame(indata), file=(paste0("/mnt/oldhdd/home/zhukov/Dropbox (Zhukov research team)/XSub_upload/data_csv_event/",gsub("RData","csv",filez.rename)[i])),row.names = FALSE,na = "")  
      }
    save(indata, file=(paste0("Upload/data_rdata_event/",filez.rename[i])))
    
    
    # # Close loop, Single core
    # }
    
    # Close loop: countries (Linux/Mac)
  },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
  
  # # Close loop (Windows)
  # }
  # stopCluster(cl)


  # Close sources loop
}

