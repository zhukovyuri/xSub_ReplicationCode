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


# Census of missing
filez.full <- unlist(lapply(seq_along(srzz),function(s){dir(paste0("Output/Output_",srzz[s]))}))
filez.full <- filez.full[!grepl("Events|ACLED_v|GED_v|000",filez.full)]
filez.full <- filez.full[!grepl("clea_day|adm2_day|grid_day|MDV",filez.full)]
filez.full <- filez.full[!filez.full%in%dir("Upload/data_rdata_panel/")]
filez.full

s <- 17; srzz[s]

# Loop over sources
# for(s in length(srzz):1){print(srzz[s])
for(s in 21){print(srzz[s])
  
  # Source select
  # s <- 1
  srz <- srzz[s]
  filez <- dir(paste0("Output/Output_",srz,"/"))
  filez <- filez[!grepl("Events|ACLED_v|GED_v",filez)]
  filez <- filez[!grepl("clea_day|adm2_day|grid_day",filez)]
  
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
    
    # Linux/Mac, multi-core
    # stopCluster(cl)
    # write.file <- 'R_progress.txt'
    mclapply(i0:length(filez),function(i){print(paste0(filez[i]," (",i," of ",length(filez),")"))
      #   # monitor progress
      #   file.create(write.file)
      #   fileConn<-file(write.file)
      #   writeLines(paste0(i,'/',length(filez),' ',round(i/length(filez),4)), fileConn)
      #   close(fileConn)
      #   # monitor from a console with
      #   # tail -c +0 -f ~/Dropbox2/Dropbox\ \(Zhukov\ research\ team\)/XSub/Data/R_progress.txt  
      
      # # Windows, multi-core
      # foreach(i=i0:length(filez),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez[i]," (",i," of ",length(filez),")"))
      # lapply(list.of.packages, require, character.only = TRUE)
      
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
      
      # Close loop: countries (Linux/Mac)
    },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # # Close loop (Windows)
    # }
    # stopCluster(cl)
    
    # Close if statement
  }
  
  # Close sources loop
}

# 
# ##########################################
# # Event counts + covariates, time lags (NOTE: takes long to run, therefore commented out)
# ##########################################
# 
# # default: t-1 lag, 30, 90 day treatment windows
# 
# srzz <- gsub("Output_","",dir("Output"))
# srzz <- srzz[!srzz%in%c("Maps","ACD","ESOC","Covariates","Weather","OLD","cdRwanda","FM","SullivanGuatemala")]
# space.agg <- c("adm0","adm1","adm2","priogrid","clea")
# space.ix <- c("ID_0","ID_1","ID_2","PRIO_GID","CLEA_CST")
# time.agg <- c("year","month","week","day")
# time.ix <- c("YEAR","YRMO","WID","TID")
# 
# # Select source
# s <- 3
# srz <- srzz[s]
# filez <- dir(paste0("Output/Output_",srz,"/"))
# if(substr(srz,1,2)!="yz"){filez <- filez[!grepl("RUS|POL|USA|Events|day|year",filez)]}
# if(substr(srz,1,2)=="yz"){filez <- filez[!grepl("POL|USA|Events|day|year",filez)]}
# # filez <- filez[!filez%in%dir("Upload/data_rdata_lags/")] # Limit to remaining
# i0 <- 1; filez[i0]; i <- i0
# filez
# 
# # # Single core
# # for(i in i0:length(filez)){print(paste0(filez[i]," (",i," of ",length(filez),")"))
# 
# # Linux/Mac, multi-core
# stopCluster(cl)
# mclapply(i0:length(filez),function(i){print(paste0(filez[i]," (",i," of ",length(filez),")"))
# 
# # # Windows, multi-core
# # foreach(i=i0:length(filez),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez[i]," (",i," of ",length(filez),")"))
# # lapply(list.of.packages, require, character.only = TRUE)
# 
# 
#   # Find space/time id's
#   space.ag0 <- sapply(strsplit(as.character(filez[i]), "_"), "[[", 3)
#   time.ag0 <- gsub(".RData|.csv","",sapply(strsplit(as.character(filez[i]), "_"), "[[", 4))
#   space.ix0 <- space.ix[match(space.ag0,space.agg)]
#   time.ix0 <- time.ix[match(time.ag0,time.agg)]
# 
#   # Events
#   indata <- load(paste0("Output/Output_",srz,"/",filez[i]))
#   indata <- get(indata)
# 
#   # Covariates
#   indata.x <- load(paste0("Output/Output_Covariates/Covariates_",sapply(strsplit(as.character(filez[i]), "_"), "[[", 2),"_",space.ag0,".RData"))
#   indata.x <- get(indata.x)
# 
#   # Weather
#   indata.w <- load(paste0("Output/Output_Weather/Weather_",sapply(strsplit(as.character(filez[i]), "_"), "[[", 2),"_",space.ag0,".RData"))
#   indata.w <- get(indata.w)
#   if(time.ix0=="YEAR"){
#     indata.w$YEAR <- as.numeric(substr(indata.w$YRMO,1,4))
#     indata.w$YRMO <- NULL
#     indata.w1 <- aggregate(data.frame(TEMP=indata.w[,"TEMP"]),by=list(ISO=indata.w$ISO,SPX=indata.w[,space.ix0],YEAR=indata.w$YEAR),FUN=function(x){mean(x,na.rm=TRUE)})
#     indata.w2 <- aggregate(data.frame(RAIN=indata.w[,"RAIN"]),by=list(ISO=indata.w$ISO,SPX=indata.w[,space.ix0],YEAR=indata.w$YEAR),FUN=function(x){sum(x,na.rm=TRUE)})
#     indata.w <- merge(indata.w1,indata.w2,by=c("ISO","SPX","YEAR"),all=T)
#     names(indata.w)[names(indata.w)=="SPX"] <- space.ix0
#   }
#   names(indata.w)
# 
#   # Missing columns (create empty column if missing)
#   if(time.ix0=="YRMO"){indata$YEAR <- substr(indata$YRMO,1,4)}
#   miss.cols <- c(varz.id,varz.event)[!c(varz.id,varz.event)%in%names(indata)]
#   if(length(miss.cols)>0){
#     miss.mat <- as.data.frame(matrix(NA,nrow=nrow(indata),ncol=length(miss.cols)))
#     names(miss.mat) <- miss.cols
#     row.names(miss.mat) <- row.names(indata)
#     indata <- cbind(indata,miss.mat)
#     indata <- indata[,c(varz.id,varz.event)]
#   }
#   miss.cols <- c(space.ix0,varz.x)[!c(space.ix0,varz.x)%in%names(indata.x@data)]
#   if(length(miss.cols)>0){
#     miss.mat <- as.data.frame(matrix(NA,nrow=nrow(indata.x@data),ncol=length(miss.cols)))
#     names(miss.mat) <- miss.cols
#     row.names(miss.mat) <- row.names(indata.x@data)
#     indata.x@data <- cbind(indata.x@data,miss.mat)
#     indata.x@data <- indata.x@data[,c(space.ix0,varz.x)]
#   }
# 
#   # Event lags & time windows
#   disag.t <- sort(unique(indata[,time.ix0]))
#   disag.s <- sort(unique(indata[,space.ix0]))
#   j <- 1
#   t <- 1
#   lorder <- 1
# 
#   # # Simple lag
#   # indata <- streg.sort(indata, unitvar=space.ix0, timevar=time.ix0)
#   # data.lag <- indata[,varz.event]
#   # print(paste0("Time lags... (N=",length(disag.s),")"));
#   # for(j in 1:length(varz.event)){#print(j)
#   #   data.lag[,j]  <- streg.tlag(indata, unitvar=space.ix0, timevar=time.ix0, lagvar=varz.event[j], order=lorder)
#   # }
#   # varz.event.lag <- paste0(varz.event,"_t1")
#   # names(data.lag) <- varz.event.lag
#   # data.lag <- cbind(indata[,c(space.ix0,time.ix0)],data.lag)
#   # head(data.lag)
# 
# 
#   # Time windows
#   j <- 4
#   print(paste0("Time windows... (N=",length(disag.s),", T=",length(disag.t),")"));
#   list.tw <- lapply(1:length(disag.s),function(j){#print(paste(j,"of",length(disag.s)))
#     sub.data <- indata[indata[,space.ix0]%in%disag.s[j],]
#     sub.data <- sub.data[order(sub.data[,time.ix0]),]
#     disag.t2 <- sort(sub.data[,time.ix0])
# 
#     # Find window
#     if(time.ag0=="year"){ # +/- 1-3 years
#       tvec <- 1:length(disag.t2)
#       ntvec30 <- which((tvec-2<0)|(tvec-length(tvec)+1>0))
#       tvec30 <- which(!(tvec-2<0)&!(tvec-length(tvec)+1>0))
#       ntvec90 <- which((tvec-4<0)|(tvec-length(tvec)+3>0))
#       tvec90 <- which(!(tvec-4<0)&!(tvec-length(tvec)+3>0))
#       tw30 <- 1
#       tw90 <- 3
#     }
#     if(time.ag0=="month"){ # +/- 1-3 months
#       tvec <- 1:length(disag.t2)
#       ntvec30 <- which((tvec-2<0)|(tvec-length(tvec)+1>0))
#       tvec30 <- which(!(tvec-2<0)&!(tvec-length(tvec)+1>0))
#       ntvec90 <- which((tvec-4<0)|(tvec-length(tvec)+3>0))
#       tvec90 <- which(!(tvec-4<0)&!(tvec-length(tvec)+3>0))
#       tw30 <- 1
#       tw90 <- 3
#     }
#     if(time.ag0=="week"){ # +/- 4-12 weeks
#       tvec <- 1:length(disag.t2)
#       ntvec30 <- which((tvec-5<0)|(tvec-length(tvec)+4>0))
#       tvec30 <- which(!(tvec-5<0)&!(tvec-length(tvec)+4>0))
#       ntvec90 <- which((tvec-13<0)|(tvec-length(tvec)+12>0))
#       tvec90 <- which(!(tvec-13<0)&!(tvec-length(tvec)+12>0))
#       tw30 <- 4
#       tw90 <- 12
#     }
#     if(time.ag0=="day"){ # +/- 30-90 days
#       tvec <- 1:length(disag.t2)
#       ntvec30 <- which((tvec-31<0)|(tvec-length(tvec)+30>0))
#       tvec30 <- which(!(tvec-31<0)&!(tvec-length(tvec)+30>0))
#       ntvec90 <- which((tvec-91<0)|(tvec-length(tvec)+90>0))
#       tvec90 <- which(!(tvec-91<0)&!(tvec-length(tvec)+90>0))
#       tw30 <- 30
#       tw90 <- 90
#     }
# 
#     # # NA's for early/late time units
#     # list.tw1 <- as.data.frame(matrix(NA,nrow=length(ntvec),ncol=length(varz.event)*4))
#     # names(list.tw1) <- paste0(rep(varz.event,each=4),rep(c("_pre30","_pre90","_post30","_post90"),4))
#     # list.tw1 <- as.data.frame(c(sub.data[ntvec,c(space.ix0,time.ix0)],list.tw1))
# 
#     # Create empty holder matrix
#     tvec <- 1:length(disag.t2)
#     holder.mat <- as.data.frame(matrix(NA,nrow=length(tvec),ncol=length(varz.event)*5))
#     names(holder.mat) <- paste0(rep(varz.event,each=5),rep(c("_t1","_pre30","_pre90","_post30","_post90"),5))
#     holder.mat <- cbind(sub.data[tvec,c(space.ix0,time.ix0)],holder.mat)
# 
#     # aggregate event counts for rest
#     t0 <- 272
#     list.tw2 <- lapply(tvec,function(t0){#print(t0)
#       if(t0>1){
#         holder.mat[t0,paste0(varz.event,"_t1")] <- sub.data[sub.data[,time.ix0]%in%disag.t2[t0-1],c(varz.event)][1,]
#       }
#       if(t0%in%tvec30){
#         holder.mat[t0,paste0(varz.event,"_pre30")] <- colSums(sub.data[sub.data[,time.ix0]%in%disag.t2[t0-tw30:1],c(varz.event)])
#         holder.mat[t0,paste0(varz.event,"_post30")] <- colSums(sub.data[sub.data[,time.ix0]%in%disag.t2[t0+tw30:1],c(varz.event)])
#       }
#       if(t0%in%tvec90){
#         holder.mat[t0,paste0(varz.event,"_pre90")] <- colSums(sub.data[sub.data[,time.ix0]%in%disag.t[t0-tw90:1],c(varz.event)])
#         holder.mat[t0,paste0(varz.event,"_post90")] <- colSums(sub.data[sub.data[,time.ix0]%in%disag.t[t0+tw90:1],c(varz.event)])
#       }
# 
#       holder.mat[t0,]
# 
#       # tw.data90 <- sub.data[sub.data[,time.ix0]%in%disag.t[t0-tw90:1],c(varz.event)]
#       # names(tw.data90) <- paste0(varz.event,"_pre90")
#       # tw.data30 <- sub.data[sub.data[,time.ix0]%in%disag.t[t0-tw30:1],c(varz.event)]
#       # names(tw.data30) <- paste0(varz.event,"_pre30")
#       # pw.data90 <- sub.data[sub.data[,time.ix0]%in%disag.t[t0+tw90:1],c(varz.event)]
#       # names(pw.data90) <- paste0(varz.event,"_post90")
#       # pw.data30 <- sub.data[sub.data[,time.ix0]%in%disag.t[t0+tw30:1],c(varz.event)]
#       # names(pw.data30) <- paste0(varz.event,"_post30")
# 
#       # tw.data <- cbind(sub.data[t0,c(space.ix0,time.ix0)],holder.mat[t0,])
#       # tw.data <- as.data.frame(tw.data)
#       # tw.data
#     })
# 
#     #list.tw2 <- do.call(rbind,list.tw2)
#     mat.tw <- do.call(rbind,list.tw2)
# 
#     # merge
#     #mat.tw <- rbind(list.tw1,list.tw2)
#     mat.tw <- mat.tw[order(mat.tw[,time.ix0]),]
# 
#     # Output
#     mat.tw
#   })
#   data.tw <- do.call(rbind,list.tw)
#   summary(data.tw)
# 
#   # Merge events & lags
#   #indata0 <- merge(indata,data.lag,by=c(space.ix0,time.ix0),all.x=T,all.y=F)
#   indata0 <- merge(indata,data.tw,by=c(space.ix0,time.ix0),all.x=T,all.y=F)
#   head(indata0)
# 
#   # Merge events & covariates
#   indata1 <- merge(indata0,indata.x@data[,c(space.ix0,varz.x)],by=space.ix0,all.x=T,all.y=F)
#   if(time.ix0=="YEAR"){indata1 <- merge(indata1,indata.w,by=c("ISO",space.ix0,"YEAR"),all.x=T,all.y=F)}
#   if(time.ix0!="YEAR"){indata1 <- merge(indata1,indata.w,by=c("ISO",space.ix0,"YRMO"),all.x=T,all.y=F)}
#   indata1 <- indata1[,c(varz.id,varz.x,varz.w,varz.event,paste0(rep(varz.event,each=5),rep(c("_t1","_pre30","_pre90","_post30","_post90"),5)))]
#   indata1 <- indata1[order(indata1[,space.ix0],indata1[,time.ix0]),]
#   row.names(indata1) <- 1:nrow(indata1)
#   tail(indata1)
# 
#   # Write to file
#   # write.csv(as.data.frame(indata1), file=(paste0("Upload/data_csv_lags/",gsub("RData","csv",filez)[i])),row.names = FALSE)
#   save(indata1, file=(paste0("Upload/data_rdata_lags/",filez[i])))
# 
#   # # Close loop, Single core
#   # }
# 
# # Close loop: countries (Linux/Mac)
# },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
# 
# # # Close loop (Windows)
# # }
# # stopCluster(cl)