rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")

## Install & load packages (all at once)
list.of.packages <- c("parallel","foreach","doParallel","seqinr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# Parallel computing
mcoptions <- list(preschedule=FALSE, set.seed=TRUE)
ncores <- detectCores()
cl <- makeCluster(ncores)
registerDoParallel(cl)

# Install & load packages (all at once)
list.of.packages <- c("parallel","foreach","doParallel","seqinr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


###############################
# List of files
###############################

srzz <- c("ACLED","GED","PITF","SCAD","yzChechnya","yzCaucasus2000","yzLibya","yzUkraine2014","ACD","ESOC")
space.agg <- c("adm0","adm1","adm2","priogrid","clea")
space.ix <- c("ID_0","ID_1","ID_2","PRIO_GID","CLEA_CST")
time.agg <- c("year","month","week","day")
time.ix <- c("YEAR","YRMO","WID","TID")

# Select source
s <- 10
for(s in 10:10){
srz <- srzz[s]

######################################
# Files (non-ESOC)
######################################

if(srz!="ESOC"){
  filez <- dir(paste0("Output/Output_",srz,"/"))
  filez <- filez[sapply(strsplit(as.character(file.info(paste0("Output/Output_",srz,"/",filez))$mtime)," "),"[",1) <as.Date("2017-02-17")]
  filez.events <- dir(paste0("Output/Output_",srz,"/Events"))
  filez.events <- filez.events[!grepl("000",filez.events)]
  filez.adm.wk <- filez[grepl("_week",filez)&grepl("_adm",filez)]
  filez.adm.dy <- filez[grepl("_day",filez)&grepl("_adm",filez)]
  filez.prio.wk <- filez[grepl("_week",filez)&grepl("_prio",filez)]
  filez.prio.dy <- filez[grepl("_day",filez)&grepl("_prio",filez)]
  filez.clea.wk <- filez[grepl("_week",filez)&grepl("_clea",filez)]
  filez.clea.dy <- filez[grepl("_day",filez)&grepl("_clea",filez)]

  ####################
  # Adm week
  ####################
  
  if(length(filez.adm.wk)>0){
    
    i0 <- 1  
    
    # # Single core
    # for(i in i0:length(filez.adm.wk)){print(paste0(filez.adm.wk[i]," (",i," of ",length(filez.adm.wk),")"))
    
    # Linux/Mac, multi-core
    # stopCluster(cl)
    mclapply(i0:length(filez.adm.wk),function(i){print(paste0(filez.adm.wk[i]," (",i," of ",length(filez.adm.wk),")"))
      
      # # Windows, multi-core
      # foreach(i=i0:length(filez.adm.wk),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez.adm.wk[i]," (",i," of ",length(filez.adm.wk),")"))
      # lapply(list.of.packages, require, character.only = TRUE)
      
      # Load
      load(paste0("Output/Output_",srz,"/",filez.adm.wk[i]))
      head(adm.week$WID)
      
      # Fix
      adm.week$WID <- adm.week$WID + 2366 # (add number of weeks b/w 1900 and 1945)
      # adm.week$TID <- adm.week$TID + 16564 # (add number of days b/w 1900 and 1945) 
      
      # Save
      save(adm.week,file=paste0("Output/Output_",srz,"/",filez.adm.wk[i]))
      rm(adm.week)
      
      # # Close loop, Single core
      # }
      
      # Close loop: countries (Linux/Mac)
    },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # # Close loop (Windows)
    # }
    # stopCluster(cl)
    
  }
  
  ####################
  # Adm day
  ####################
  
  if(length(filez.adm.dy)>0){
    
    # # Single core
    # for(i in i0:length(filez.adm.dy)){print(paste0(filez.adm.dy[i]," (",i," of ",length(filez.adm.dy),")"))
    
    # Linux/Mac, multi-core
    # stopCluster(cl)
    mclapply(i0:length(filez.adm.dy),function(i){print(paste0(filez.adm.dy[i]," (",i," of ",length(filez.adm.dy),")"))
      
      # # Windows, multi-core
      # foreach(i=i0:length(filez.adm.dy),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez.adm.dy[i]," (",i," of ",length(filez.adm.dy),")"))
      # lapply(list.of.packages, require, character.only = TRUE)
      
      # Lod
      load(paste0("Output/Output_",srz,"/",filez.adm.dy[i]))
      head(adm.day$WID)
      head(adm.day$TID)
      
      # Fix
      adm.day$WID <- adm.day$WID + 2366 # (add number of weeks b/w 1900 and 1945)
      adm.day$TID <- adm.day$TID + 16564 # (add number of days b/w 1900 and 1945) 
      
      # Save
      save(adm.day,file=paste0("Output/Output_",srz,"/",filez.adm.dy[i]))
      rm(adm.day)
      
      # # Close loop, Single core
      # }
      
      # Close loop: countries (Linux/Mac)
    },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # # Close loop (Windows)
    # }
    # stopCluster(cl)
    
  }
  
  ####################
  # Prio week
  ####################
  
  if(length(filez.prio.wk)>0){
    
    # # Single core
    # for(i in i0:length(filez.prio.wk)){print(paste0(filez.prio.wk[i]," (",i," of ",length(filez.prio.wk),")"))
    
    # Linux/Mac, multi-core
    # stopCluster(cl)
    mclapply(i0:length(filez.prio.wk),function(i){print(paste0(filez.prio.wk[i]," (",i," of ",length(filez.prio.wk),")"))
      
      # # Windows, multi-core
      # foreach(i=i0:length(filez.prio.wk),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez.prio.wk[i]," (",i," of ",length(filez.prio.wk),")"))
      # lapply(list.of.packages, require, character.only = TRUE)
      
      # Load
      load(paste0("Output/Output_",srz,"/",filez.prio.wk[i]))
      head(adm.week)
      
      # Fix
      adm.week$WID <- adm.week$WID + 2366 # (add number of weeks b/w 1900 and 1945)
      # adm.week$TID <- adm.week$TID + 16564 # (add number of days b/w 1900 and 1945) 
      
      # Save
      save(adm.week,file=paste0("Output/Output_",srz,"/",filez.prio.wk[i]))
      rm(adm.week)
      
      # # Close loop, Single core
      # }
      
      # Close loop: countries (Linux/Mac)
    },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # # Close loop (Windows)
    # }
    # stopCluster(cl)
    
  }
  
  ####################
  # Prio day
  ####################
  
  if(length(filez.prio.dy)>0){
    
    # # Single core
    # for(i in i0:length(filez.prio.dy)){print(paste0(filez.prio.dy[i]," (",i," of ",length(filez.prio.dy),")"))
    
    # Linux/Mac, multi-core
    # stopCluster(cl)
    mclapply(i0:length(filez.prio.dy),function(i){print(paste0(filez.prio.dy[i]," (",i," of ",length(filez.prio.dy),")"))
      
      # # Windows, multi-core
      # foreach(i=i0:length(filez.prio.dy),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez.prio.dy[i]," (",i," of ",length(filez.prio.dy),")"))
      # lapply(list.of.packages, require, character.only = TRUE)
      
      # Load
      load(paste0("Output/Output_",srz,"/",filez.prio.dy[i]))
      head(adm.day)
      
      # Fix
      adm.day$WID <- adm.day$WID + 2366 # (add number of weeks b/w 1900 and 1945)
      adm.day$TID <- adm.day$TID + 16564 # (add number of days b/w 1900 and 1945)
      
      # Save
      save(adm.day,file=paste0("Output/Output_",srz,"/",filez.prio.dy[i]))
      rm(adm.day)
      
      # # Close loop, Single core
      # }
      
      # Close loop: countries (Linux/Mac)
    },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # # Close loop (Windows)
    # }
    # stopCluster(cl)
    
  }
 
  ####################
  # Clea week
  ####################
  
  if(length(filez.clea.wk)>0){
    
    # # Single core
    # for(i in i0:length(filez.clea.wk)){print(paste0(filez.clea.wk[i]," (",i," of ",length(filez.clea.wk),")"))
    
    # Linux/Mac, multi-core
    # stopCluster(cl)
    mclapply(i0:length(filez.clea.wk),function(i){print(paste0(filez.clea.wk[i]," (",i," of ",length(filez.clea.wk),")"))
      
      # # Windows, multi-core
      # foreach(i=i0:length(filez.clea.wk),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez.clea.wk[i]," (",i," of ",length(filez.clea.wk),")"))
      # lapply(list.of.packages, require, character.only = TRUE)
      
      # Load
      load(paste0("Output/Output_",srz,"/",filez.clea.wk[i]))
      head(adm.week$WID)
      
      # Fix
      adm.week$WID <- adm.week$WID + 2366 # (add number of weeks b/w 1900 and 1945)
      # adm.week$TID <- adm.week$TID + 16564 # (add number of days b/w 1900 and 1945) 
      
      # Save
      save(adm.week,file=paste0("Output/Output_",srz,"/",filez.clea.wk[i]))
      rm(adm.week)
      
      # # Close loop, Single core
      # }
      
      # Close loop: countries (Linux/Mac)
    },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # # Close loop (Windows)
    # }
    # stopCluster(cl)
    
  }
 
  ####################
  # Clea day
  ####################
  
  if(length(filez.clea.dy)>0){
    
    # # Single core
    # for(i in i0:length(filez.clea.dy)){print(paste0(filez.clea.dy[i]," (",i," of ",length(filez.clea.dy),")"))
    
    # Linux/Mac, multi-core
    # stopCluster(cl)
    mclapply(i0:length(filez.clea.dy),function(i){print(paste0(filez.clea.dy[i]," (",i," of ",length(filez.clea.dy),")"))
      
      # # Windows, multi-core
      # foreach(i=i0:length(filez.clea.dy),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez.clea.dy[i]," (",i," of ",length(filez.clea.dy),")"))
      # lapply(list.of.packages, require, character.only = TRUE)
      
      # Load
      load(paste0("Output/Output_",srz,"/",filez.clea.dy[i]))
      head(adm.day)
      
      # Fix
      adm.day$WID <- adm.day$WID + 2366 # (add number of weeks b/w 1900 and 1945)
      adm.day$TID <- adm.day$TID + 16564 # (add number of days b/w 1900 and 1945)
      
      # Save
      save(adm.day,file=paste0("Output/Output_",srz,"/",filez.clea.dy[i]))
      rm(adm.day)
      
      # # Close loop, Single core
      # }
      
      # Close loop: countries (Linux/Mac)
    },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # # Close loop (Windows)
    # }
    # stopCluster(cl)
    
  }

}


######################################
## ESOC files
######################################

if(srz=="ESOC"){
  dirz <- dir(paste0("Output/Output_",srz,"/"))
  e <- 1
  # Loop over ESOC datasets
  for(e in 1:length(dirz)){
    print(dirz[e])
  
  filez <- dir(paste0("Output/Output_",srz,"/",dirz[e]))
  filez <- filez[sapply(strsplit(as.character(file.info(paste0("Output/Output_",srz,"/",dirz[e],"/",filez))$mtime)," "),"[",1) <as.Date("2017-02-18")]
  filez.adm.wk <- filez[grepl("_week",filez)&grepl("_adm",filez)]
  filez.adm.dy <- filez[grepl("_day",filez)&grepl("_adm",filez)]
  filez.prio.wk <- filez[grepl("_week",filez)&grepl("_prio",filez)]
  filez.prio.dy <- filez[grepl("_day",filez)&grepl("_prio",filez)]
  filez.clea.wk <- filez[grepl("_week",filez)&grepl("_clea",filez)]
  filez.clea.dy <- filez[grepl("_day",filez)&grepl("_clea",filez)]

  
  ####################
  # Adm week
  ####################
  
  if(length(filez.adm.wk)>0){
    
    i0 <- 1  
    
    # # Single core
    # for(i in i0:length(filez.adm.wk)){print(paste0(filez.adm.wk[i]," (",i," of ",length(filez.adm.wk),")"))
    
    # Linux/Mac, multi-core
    # stopCluster(cl)
    mclapply(i0:length(filez.adm.wk),function(i){print(paste0(filez.adm.wk[i]," (",i," of ",length(filez.adm.wk),")"))
      
      # # Windows, multi-core
      # foreach(i=i0:length(filez.adm.wk),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez.adm.wk[i]," (",i," of ",length(filez.adm.wk),")"))
      # lapply(list.of.packages, require, character.only = TRUE)
      
      # Load
      load(paste0("Output/Output_",srz,"/",dirz[e],"/",filez.adm.wk[i]))
      head(adm.week$WID)
      
      # Fix
      adm.week$WID <- adm.week$WID + 2366 # (add number of weeks b/w 1900 and 1945)
      # adm.week$TID <- adm.week$TID + 16564 # (add number of days b/w 1900 and 1945) 
      
      # Save
      save(adm.week,file=paste0("Output/Output_",srz,"/",dirz[e],"/",filez.adm.wk[i]))
      rm(adm.week)
      
      # # Close loop, Single core
      # }
      
      # Close loop: countries (Linux/Mac)
    },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # # Close loop (Windows)
    # }
    # stopCluster(cl)
    
  }
  
  ####################
  # Adm day
  ####################
  
  if(length(filez.adm.dy)>0){
    
    # # Single core
    # for(i in i0:length(filez.adm.dy)){print(paste0(filez.adm.dy[i]," (",i," of ",length(filez.adm.dy),")"))
    
    # Linux/Mac, multi-core
    # stopCluster(cl)
    mclapply(i0:length(filez.adm.dy),function(i){print(paste0(filez.adm.dy[i]," (",i," of ",length(filez.adm.dy),")"))
      
      # # Windows, multi-core
      # foreach(i=i0:length(filez.adm.dy),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez.adm.dy[i]," (",i," of ",length(filez.adm.dy),")"))
      # lapply(list.of.packages, require, character.only = TRUE)
      
      # Lod
      load(paste0("Output/Output_",srz,"/",dirz[e],"/",filez.adm.dy[i]))
      head(adm.day$WID)
      head(adm.day$TID)
      
      # Fix
      adm.day$WID <- adm.day$WID + 2366 # (add number of weeks b/w 1900 and 1945)
      adm.day$TID <- adm.day$TID + 16564 # (add number of days b/w 1900 and 1945) 
      
      # Save
      save(adm.day,file=paste0("Output/Output_",srz,"/",dirz[e],"/",filez.adm.dy[i]))
      rm(adm.day)
      
      # # Close loop, Single core
      # }
      
      # Close loop: countries (Linux/Mac)
    },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # # Close loop (Windows)
    # }
    # stopCluster(cl)
    
  }
  
  ####################
  # Prio week
  ####################
  
  if(length(filez.prio.wk)>0){
    
    # # Single core
    # for(i in i0:length(filez.prio.wk)){print(paste0(filez.prio.wk[i]," (",i," of ",length(filez.prio.wk),")"))
    
    # Linux/Mac, multi-core
    # stopCluster(cl)
    mclapply(i0:length(filez.prio.wk),function(i){print(paste0(filez.prio.wk[i]," (",i," of ",length(filez.prio.wk),")"))
      
      # # Windows, multi-core
      # foreach(i=i0:length(filez.prio.wk),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez.prio.wk[i]," (",i," of ",length(filez.prio.wk),")"))
      # lapply(list.of.packages, require, character.only = TRUE)
      
      # Load
      load(paste0("Output/Output_",srz,"/",dirz[e],"/",filez.prio.wk[i]))
      head(adm.week)
      
      # Fix
      adm.week$WID <- adm.week$WID + 2366 # (add number of weeks b/w 1900 and 1945)
      # adm.week$TID <- adm.week$TID + 16564 # (add number of days b/w 1900 and 1945) 
      
      # Save
      save(adm.week,file=paste0("Output/Output_",srz,"/",dirz[e],"/",filez.prio.wk[i]))
      rm(adm.week)
      
      # # Close loop, Single core
      # }
      
      # Close loop: countries (Linux/Mac)
    },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # # Close loop (Windows)
    # }
    # stopCluster(cl)
    
  }
  
  ####################
  # Prio day
  ####################
  
  if(length(filez.prio.dy)>0){
    
    # # Single core
    # for(i in i0:length(filez.prio.dy)){print(paste0(filez.prio.dy[i]," (",i," of ",length(filez.prio.dy),")"))
    
    # Linux/Mac, multi-core
    # stopCluster(cl)
    mclapply(i0:length(filez.prio.dy),function(i){print(paste0(filez.prio.dy[i]," (",i," of ",length(filez.prio.dy),")"))
      
      # # Windows, multi-core
      # foreach(i=i0:length(filez.prio.dy),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez.prio.dy[i]," (",i," of ",length(filez.prio.dy),")"))
      # lapply(list.of.packages, require, character.only = TRUE)
      
      # Load
      load(paste0("Output/Output_",srz,"/",dirz[e],"/",filez.prio.dy[i]))
      head(adm.day)
      
      # Fix
      adm.day$WID <- adm.day$WID + 2366 # (add number of weeks b/w 1900 and 1945)
      adm.day$TID <- adm.day$TID + 16564 # (add number of days b/w 1900 and 1945)
      
      # Save
      save(adm.day,file=paste0("Output/Output_",srz,"/",dirz[e],"/",filez.prio.dy[i]))
      rm(adm.day)
      
      # # Close loop, Single core
      # }
      
      # Close loop: countries (Linux/Mac)
    },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # # Close loop (Windows)
    # }
    # stopCluster(cl)
    
  }
  
  ####################
  # Clea week
  ####################
  
  if(length(filez.clea.wk)>0){
    
    # # Single core
    # for(i in i0:length(filez.clea.wk)){print(paste0(filez.clea.wk[i]," (",i," of ",length(filez.clea.wk),")"))
    
    # Linux/Mac, multi-core
    # stopCluster(cl)
    mclapply(i0:length(filez.clea.wk),function(i){print(paste0(filez.clea.wk[i]," (",i," of ",length(filez.clea.wk),")"))
      
      # # Windows, multi-core
      # foreach(i=i0:length(filez.clea.wk),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez.clea.wk[i]," (",i," of ",length(filez.clea.wk),")"))
      # lapply(list.of.packages, require, character.only = TRUE)
      
      # Load
      load(paste0("Output/Output_",srz,"/",dirz[e],"/",filez.clea.wk[i]))
      head(adm.week$WID)
      
      # Fix
      adm.week$WID <- adm.week$WID + 2366 # (add number of weeks b/w 1900 and 1945)
      # adm.week$TID <- adm.week$TID + 16564 # (add number of days b/w 1900 and 1945) 
      
      # Save
      save(adm.week,file=paste0("Output/Output_",srz,"/",dirz[e],"/",filez.clea.wk[i]))
      rm(adm.week)
      
      # # Close loop, Single core
      # }
      
      # Close loop: countries (Linux/Mac)
    },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # # Close loop (Windows)
    # }
    # stopCluster(cl)
    
  }
  
  ####################
  # Clea day
  ####################
  
  if(length(filez.clea.dy)>0){
    
    # # Single core
    # for(i in i0:length(filez.clea.dy)){print(paste0(filez.clea.dy[i]," (",i," of ",length(filez.clea.dy),")"))
    
    # Linux/Mac, multi-core
    # stopCluster(cl)
    mclapply(i0:length(filez.clea.dy),function(i){print(paste0(filez.clea.dy[i]," (",i," of ",length(filez.clea.dy),")"))
      
      # # Windows, multi-core
      # foreach(i=i0:length(filez.clea.dy),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(paste0(filez.clea.dy[i]," (",i," of ",length(filez.clea.dy),")"))
      # lapply(list.of.packages, require, character.only = TRUE)
      
      # Load
      load(paste0("Output/Output_",srz,"/",dirz[e],"/",filez.clea.dy[i]))
      head(adm.day)
      
      # Fix
      adm.day$WID <- adm.day$WID + 2366 # (add number of weeks b/w 1900 and 1945)
      adm.day$TID <- adm.day$TID + 16564 # (add number of days b/w 1900 and 1945)
      
      # Save
      save(adm.day,file=paste0("Output/Output_",srz,"/",dirz[e],"/",filez.clea.dy[i]))
      rm(adm.day)
      
      # # Close loop, Single core
      # }
      
      # Close loop: countries (Linux/Mac)
    },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
    
    # # Close loop (Windows)
    # }
    # stopCluster(cl)
    
  }
  
  
  }

  
}
}