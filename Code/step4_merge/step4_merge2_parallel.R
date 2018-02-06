rm(list=ls())

## Set directory
setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")

source("Code/detachPackages.R")



######################################
## Aggregate (without lags)
######################################

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","spatstat","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# Parallel computing
mcoptions <- list(preschedule=FALSE, set.seed=TRUE)
ncores <- detectCores()
# cl <- makeCluster(ncores)
# registerDoParallel(cl)

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","spatstat","parallel","foreach","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# List files
srzz <- c("ABADarfur","ACLED","BeissingerProtest","BeissingerRiot","BeissingerUkraine","ESOCAfghanistanWITS","ESOCIraqSIGACT","ESOCIraqWITS","ESOCMexicoDrugRelatedMurders","ESOCMexicoHomicide","ESOCPakistanBFRS","ESOCPakistanWITS","FM","GED","NVMS","PITF","SCAD","SullivanGuatemala","yzCaucasus2000","yzChechnya","yzLibya","yzUkraine2014" )
space.agg <- c("adm0","adm1","adm2","priogrid","clea")
space.ix <- c("ID_0","ID_1","ID_2","PRIO_GID","CLEA_CST")
time.agg <- c("year","month","week","day")
time.ix <- c("YEAR","YRMO","WID","TID")
levz <- paste0(rep(space.agg,each=length(time.agg)),"_",rep(time.agg,length(space.agg)))
levz <- levz[!grepl("adm2_day|priogrid_day|clea_day",levz)]
levz
s <- 2; srzz[s]

# # Loop through sources
# for(s in 1:length(srzz)){

# Linux/Mac
# stopCluster(cl)
mclapply(1:length(srzz),function(s){print(srzz[s])

# # Windows
# foreach(s=1:4,.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(srzz[s])
#   lapply(list.of.packages, require, character.only = TRUE)

  srz <- srzz[s]
  filez <- dir(paste0("Output/Output_",srz,"/"))
  filez <- filez[!grepl("Events",filez)]

  # Census
  filez.out <- dir(paste0("Upload/data_rdata_country/"))
  filez[!filez%in%filez.out] # Missing

  # Filter by source
  filez.s <- filez.out[grep(srz,filez.out)]

  lev <- 15
  # Loop over units of analysis
  list.lev <- lapply(1:length(levz),function(lev){#print(lev)
    print(paste(srz,levz[lev]))

    # Filter by level
    filez.l <- filez.s[grep(levz[lev],filez.s)]
    if(length(filez.l)>0){
      
    i <- 1
    # Loop over files
    list.filez <- lapply(1:length(filez.l),function(i){#print(paste(i,"of",length(filez.l)))

      # Find space/time id's
      space.ag0 <- sapply(strsplit(as.character(filez.l[i]), "_"), "[[", 3)
      time.ag0 <- gsub(".RData|.csv","",sapply(strsplit(as.character(filez.l[i]), "_"), "[[", 4))
      space.ix0 <- space.ix[match(space.ag0,space.agg)]
      time.ix0 <- time.ix[match(time.ag0,time.agg)]

      # Load file
      ndata <- load(paste0("Upload/data_rdata_country/",filez.l[i]))
      indata <- get(ndata);rm(ndata)

      # Create unit ids
      temp.id <- paste0(indata[,"ISO"],"_",indata[,space.ix0],"_",indata[,time.ix0])
      indata <- cbind(data.frame(SOURCE=srz,UNIT_ID=temp.id,UNIT_LEVEL=levz[lev]),indata)
      indata

      # Close i loop
    })
    mat.filez <- do.call(rbind,list.filez)
    rm(list.filez)
    sort(unique(mat.filez$ISO))
    head(mat.filez)
    save(mat.filez, file=paste0("Upload/data_rdata_combined/",srz,"_",levz[lev],".RData"))
    rm(mat.filez)
    }

    # Close lev loop
  })


# # Close s loop
# }

# Close loop: countries (Linux/Mac)
},mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)

# # Close loop (Windows)
# }
# stopCluster(cl)


# ######################################
# ## Aggregate (with lags)
# ######################################
# 
# ## Install & load packages (all at once)
# list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","spatstat","parallel","foreach","doParallel")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
# lapply(list.of.packages, require, character.only = TRUE)
# 
# # # Parallel computing
# # mcoptions <- list(preschedule=FALSE, set.seed=TRUE)
# # ncores <- detectCores()
# # cl <- makeCluster(ncores)
# # registerDoParallel(cl)
# # 
# # ## Install & load packages (all at once)
# # list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","spatstat","parallel","foreach","doParallel")
# # new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
# # lapply(list.of.packages, require, character.only = TRUE)
# 
# # List files
# srzz <- c("ACLED","GED","PITF","SCAD","yzChechnya","yzCaucasus2000","yzLibya","yzUkraine2014","ACD")
# space.agg <- c("adm0","adm1","adm2","priogrid","clea")
# space.ix <- c("ID_0","ID_1","ID_2","PRIO_GID","CLEA_CST")
# time.agg <- c("year","month","week","day")
# time.ix <- c("YEAR","YRMO","WID","TID")
# levz <- paste0(rep(space.agg,each=length(time.agg)),"_",rep(time.agg,length(space.agg)))
# levz <- levz[!grepl("year|day",levz)]
# 
# s <- 1
# 
# # # Loop through sources
# # for(s in 1:length(srzz)){
# 
# # Linux/Mac
# stopCluster(cl)
# mclapply(5:length(srzz),function(s){print(srzz[s])
# 
# # # Windows
# # foreach(s=1:length(srzz),.options.multicore=mcoptions,.verbose=TRUE,.errorhandling = "remove") %dopar% {print(cntz[k])
# #   lapply(list.of.packages, require, character.only = TRUE)
# 
#   srz <- srzz[s]
#   filez <- dir(paste0("Output/Output_",srz,"/"))
#   filez <- filez[!grepl("Events|year|day",filez)]
# 
#   # Census
#   filez.out <- dir(paste0("Upload/data_rdata_lags/"))
#   filez[!filez%in%filez.out] # Missing
# 
#   # Filter by source
#   filez.s <- filez.out[grep(srz,filez.out)]
# 
#   lev <- 1
#   # Loop over units of analysis
#   list.lev <- lapply(1:length(levz),function(lev){
#     print(paste(srz,levz[lev]))
# 
#     # Filter by level
#     filez.l <- filez.s[grep(levz[lev],filez.s)]
# 
#     i <- 1
#     # Loop over files
#     list.filez <- lapply(1:length(filez.l),function(i){#print(paste(i,"of",length(filez.l)))
# 
#       # Find space/time id's
#       space.ag0 <- sapply(strsplit(as.character(filez.l[i]), "_"), "[[", 3)
#       time.ag0 <- gsub(".RData|.csv","",sapply(strsplit(as.character(filez.l[i]), "_"), "[[", 4))
#       space.ix0 <- space.ix[match(space.ag0,space.agg)]
#       time.ix0 <- time.ix[match(time.ag0,time.agg)]
# 
#       # Load file
#       indata <- load(paste0("Upload/data_rdata_lags/",filez.l[i]))
#       indata <- get(indata)
# 
#       # Create unit ids
#       temp.id <- paste0(indata[,"ISO"],"_",indata[,space.ix0],"_",indata[,time.ix0])
#       indata <- cbind(data.frame(SOURCE=srz,UNIT_ID=temp.id,UNIT_LEVEL=levz[lev]),indata)
#       indata
# 
#     # Close i loop
#     })
#     mat.filez <- do.call(rbind,list.filez)
#     rm(list.filez)
#     sort(unique(mat.filez$ISO))
#     save(mat.filez, file=paste0("Upload/data_rdata_lags_combined/",srz,"_",levz[lev],".RData"))
#     rm(mat.filez)
# 
#   # Close lev loop
#   })
# 
# 
# # # Close s loop
# # }
# 
# # Close loop: countries (Linux/Mac)
# },mc.preschedule = FALSE, mc.set.seed = TRUE,mc.silent = FALSE, mc.cores = ncores)
# 
# # # Close loop (Windows)
# # }
# # stopCluster(cl)
