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
ncores <- min(12,detectCores())

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


# Census of missing
filez.full <- unlist(lapply(seq_along(srzz),function(s){dir(paste0("Output/Output_",srzz[s]))}))
filez.full <- filez.full[!grepl("Events|ACLED_v|GED_v|000",filez.full)]
filez.full <- filez.full[!grepl("clea_day|adm2_day|grid_day|MDV",filez.full)]
filez.full <- filez.full[!filez.full%in%dir("Upload/data_rdata_panel/")]
# filez.full

s <- 17; srzz[s]

# Loop over sources
for(s in length(srzz):1){print(srzz[s])
# for(s in 16:23){print(srzz[s])
  
  # Source select
  # s <- 1
  srz <- srzz[s]
  filez <- dir(paste0("Output/Output_",srz,"/"))
  filez <- filez[!grepl("Events|ACLED_v|GED_v",filez)]
  filez <- filez[!grepl("clea_day|adm2_day|grid_day|_MDV_",filez)]
  
  # Remaining
  filez <- filez[!filez%in%dir("Upload/data_rdata_panel/")]
  
  # Only run if file list non-empty
  if(length(filez)>0){
    
    # Order by file size
    filez <- filez[order(file.size(paste0("Output/Output_",srz,"/",filez)))]
    
    # filez <- filez[!grepl("_POL_",filez)]
    i0 <- 1; filez[i0]; i <- i0
    # filez <- filez[grep("clea",filez)]
    filez
    
    # # Single core
    # for(i in i0:length(filez)){print(paste0(filez[i]," (",i," of ",length(filez),")"))
    #   tryCatch({
    
    # # Linux/Mac, multi-core
    # mclapply(i0:length(filez),function(i){print(paste0(filez[i]," (",i," of ",length(filez),")"))
    
    # Windows, multi-core
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    foreach(i=i0:length(filez),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez[i]," (",i," of ",length(filez),")"))
    lapply(list.of.packages, require, character.only = TRUE)
      
      # Find space/time id's
      space.ag0 <- sapply(strsplit(gsub(srz,"",filez[i]), "_"), "[[", 3)
      time.ag0 <- gsub(".RData|.csv","",sapply(strsplit(gsub(srz,"",filez[i]), "_"), "[[", 4))
      space.ix0 <- space.ix[match(space.ag0,space.agg)]
      time.ix0 <- time.ix[match(time.ag0,time.agg)]
      
      # Events
      indata <- load(paste0("Output/Output_",srz,"/",filez[i]))
      indata <- get(indata)
      summary(indata)
      
      # Covariates
      indata.x <- load(paste0("Output/Output_Covariates/Covariates_",sapply(strsplit(gsub(srz,"",filez[i]), "_"), "[[", 2),"_",space.ag0,".RData"))
      indata.x <- get(indata.x)
      names(indata)
      names(indata.x)
      
      # Weather
      indata.w <- load(paste0("Output/Output_Weather/Weather_",sapply(strsplit(gsub(srz,"",filez[i]), "_"), "[[", 2),"_",space.ag0,".RData"))
      indata.w <- get(indata.w)
      if(time.ix0=="YEAR"){
        indata.w$YEAR <- as.numeric(substr(indata.w$YRMO,1,4))
        indata.w$YRMO <- NULL
        indata.w1 <- aggregate(data.frame(TEMP=indata.w[,"TEMP"]),by=list(ISO=indata.w$ISO,SPX=indata.w[,space.ix0],YEAR=indata.w$YEAR),FUN=function(x){mean(x,na.rm=TRUE)})
        indata.w2 <- aggregate(data.frame(RAIN=indata.w[,"RAIN"]),by=list(ISO=indata.w$ISO,SPX=indata.w[,space.ix0],YEAR=indata.w$YEAR),FUN=function(x){sum(x,na.rm=TRUE)})
        indata.w <- merge(indata.w1,indata.w2,by=c("ISO","SPX","YEAR"),all=T)
        names(indata.w)[names(indata.w)=="SPX"] <- space.ix0
      }
      head(indata.w)
      
      # Missing columns (create empty column if missing)
      if(time.ix0=="YRMO"){indata$YEAR <- substr(indata$YRMO,1,4)}
      miss.cols <- c(varz.id,varz.event)[!c(varz.id,varz.event)%in%names(indata)]
      if(length(miss.cols)>0){
        miss.mat <- as.data.frame(matrix(NA,nrow=nrow(indata),ncol=length(miss.cols)))
        names(miss.mat) <- miss.cols
        row.names(miss.mat) <- row.names(indata)
        indata <- cbind(indata,miss.mat)
        indata <- indata[,c(varz.id,varz.event)]
      }
      miss.cols <- c(space.ix0,varz.x)[!c(space.ix0,varz.x)%in%names(indata.x@data)]
      if(length(miss.cols)>0){
        miss.mat <- as.data.frame(matrix(NA,nrow=nrow(indata.x@data),ncol=length(miss.cols)))
        names(miss.mat) <- miss.cols
        row.names(miss.mat) <- row.names(indata.x@data)
        indata.x@data <- cbind(indata.x@data,miss.mat)
        indata.x@data <- indata.x@data[,c(space.ix0,varz.x)]
      }
      c(space.ix0,varz.x)[!c(space.ix0,varz.x)%in%names(indata)]
      names(indata)
      
      # Merge events & covariates
      indata <- merge(indata,indata.x@data[,c(space.ix0,varz.x)],by=space.ix0,all.x=T,all.y=F)
      if(time.ix0=="YEAR"){indata <- merge(indata,indata.w,by=c("ISO",space.ix0,"YEAR"),all.x=T,all.y=F)}
      if(time.ix0!="YEAR"){indata <- merge(indata,indata.w,by=c("ISO",space.ix0,"YRMO"),all.x=T,all.y=F)}
      indata <- indata[,c(varz.id,varz.x,varz.w,varz.event)]
      indata <- indata[order(indata[,space.ix0],indata[,time.ix0]),]
      row.names(indata) <- 1:nrow(indata)
      head(indata)
      
      
      # Write to file
      if("XSub"%in%dir()){
        write.csv(as.data.frame(indata), file=(paste0("~/XSub_upload/data_csv_panel/",gsub("RData","csv",filez)[i])),row.names = FALSE,na = "")
      }
      if("Dropbox2"%in%dir()){
        write.csv(as.data.frame(indata), file=(paste0("~/Dropbox2/Dropbox (Zhukov research team)/XSub_upload/data_csv_panel/",gsub("RData","csv",filez)[i])),row.names = FALSE,na = "")
      }
      if(grepl("w64",sessionInfo()[[1]][[1]])){
        write.csv(as.data.frame(indata), file=(paste0("D:/Dropbox (Zhukov research team)/XSub_upload/data_csv_panel/",gsub("RData","csv",filez)[i])),row.names = FALSE,na = "")
      }
      if(grepl("Ubuntu 18.04 LTS",sessionInfo()[[4]])){
        write.csv(as.data.frame(indata), file=(paste0("/mnt/oldhdd/home/zhukov/Dropbox (Zhukov research team)/XSub_upload/data_csv_panel/",gsub("RData","csv",filez)[i])),row.names = FALSE,na = "")  
      }
      save(indata, file=(paste0("Upload/data_rdata_panel/",filez[i])))
      
      # # Close loop, Single core
      # }, error=function(e){})
      # }
      
    #   # Close loop: countries (Linux/Mac)
    # },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # Close loop (Windows)
    }
    stopCluster(cl)
    
    # Close if statement
  }
  
  # Close sources loop
}

