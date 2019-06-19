rm(list=ls())


setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
#source("Code/functions.R")

## Install & load packages (all at once)
list.of.packages <- c("maptools","classInt","raster","sp","rgeos","parallel","foreach","doParallel","countrycode","PBSmapping","mgcv","xtable")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)




######################################################################################
######################################################################################
######################################################################################
## Census
######################################################################################
######################################################################################
######################################################################################


filez.all <- dir("Upload/data_rdata_event/")
# filez.all <- filez.all[!grepl("ACLED0|GED0",filez.all)]
cntz <- sort(unique(sapply(strsplit(filez.all,"_"),"[",2)))
k <- 43
k <- which(cntz=="IRQ")
i <- 7
cnt.list <- lapply(seq_along(cntz),function(k){print(paste0(k,"/",cntz[k]))
  filez <- dir("Upload/data_rdata_event/")[grep(paste0("_",cntz[k],"_"),dir("Upload/data_rdata_event/"))]
  filez <- filez[!grepl("ACLED0|GED0",filez)]
  srz <- sapply(strsplit(filez,"_"),"[",1)
  cnt.list2 <- lapply(seq_along(filez),function(i){#print(i)
    load(paste0("Upload/data_rdata_event/",filez[i]))    
    srz.i <- as.character(indata$SOURCE[1])
    if(grepl("^MELTT",srz.i)){srz.i <- srz[i]}
    cntn.i <- countrycode(cntz[k],"iso3c","country.name")
    if(cntz[k]%in%"COD"){cntn.i <- "D.R.C."}
    if(cntz[k]%in%"IRN"){cntn.i <- "Iran"}
    if(cntz[k]%in%"LAO"){cntn.i <- "Laos"}
    if(cntz[k]%in%"TZA"){cntn.i <- "Tanzania"}
    if(cntz[k]%in%"BOL"){cntn.i <- "Bolivia"}
    if(cntz[k]%in%"RUS"){cntn.i <- "Russia"}
    if(cntz[k]%in%"MKD"){cntn.i <- "Macedonia (FYROM)"}
    if(cntz[k]%in%"PSE"){cntn.i <- "Palestinian Ter."}
    if(cntz[k]%in%"SYR"){cntn.i <- "Syria"}
    if(cntz[k]%in%"VEN"){cntn.i <- "Venezuela"}
    if(cntz[k]%in%"VNM"){cntn.i <- "Vietnam"}
    if(cntz[k]%in%"MDA"){cntn.i <- "Moldova"}
    if(cntz[k]%in%"CAF"){cntn.i <- "C.A.R."}
    if(cntz[k]%in%"GNQ"){cntn.i <- "Eq. Guinea"}
    if(cntz[k]%in%"GMB"){cntn.i <- "The Gambia"}
    if(cntz[k]%in%"GBR"){cntn.i <- "United Kingdom"}
    if(cntz[k]%in%"USA"){cntn.i <- "United States"}
    
    n.i <- nrow(indata)
    head(indata)
    dates.i <- range(as.numeric(as.character(indata$DATE)),na.rm=T)
    dates.i <- paste0(substr(dates.i,1,4),collapse="-")
    # levels.i <- dir("Upload/data_rdata_panel/")[grep(paste0(srz[k],"_",cntz[k]),dir("Upload/data_rdata_panel/"))]
    # levels.i <- paste0(paste0(sort(unique(sapply(strsplit(gsub(".RData","",levels.i),"_"),"[",3))),collapse=", ")," / ",paste0(sort(unique(sapply(strsplit(gsub(".RData","",levels.i),"_"),"[",4))),collapse=", "))
    data.frame(Num=k,Country=as.character(cntn.i),ISO3=as.character(cntz[k]),Events=formatC(n.i,big.mark = ","),Dates=dates.i,Source=srz.i)
  })
  cnt.mat2 <- do.call(rbind,cnt.list2)  
  cnt.mat2 <- rbind(cnt.mat2[-grep("MELTT",cnt.mat2$Source),],  cnt.mat2[grep("MELTT",cnt.mat2$Source),])
  if(nrow(cnt.mat2)>0){
    cnt.mat2$Num <- as.character(cnt.mat2$Num)
    cnt.mat2$Num <- paste0("\\hline ",cnt.mat2$Num)
    cnt.mat2$Country <- as.character(cnt.mat2$Country)
    cnt.mat2$ISO3 <- as.character(cnt.mat2$ISO3)
    cnt.mat2$Dates <- as.character(cnt.mat2$Dates)
    cnt.mat2$Num[-1] <- ""
    cnt.mat2$Country[-1] <- ""
    cnt.mat2$ISO3[-1] <- ""
    cnt.mat2
  }
})
cnt.mat <- do.call(rbind,cnt.list)
grep("Inf",cnt.mat$Dates)
cnt.mat[grep("Inf",cnt.mat$Dates),"Dates"] <- ""
row.names(cnt.mat) <- 1:nrow(cnt.mat)
nrow(cnt.mat)
addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste("\\hline \\\\",
                             "\\endhead \\\\",
                             "\\hline \\\\",sep=""))
print.xtable(xtable(cnt.mat),file = "DataCensus/ForCodebook/dataCensus_v3.tex",tabular.environment = "longtable",add.to.row = addtorow,hline.after=c(-1),include.rownames = FALSE)
