rm(list=ls())


## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
if(grepl("w64",sessionInfo()[[1]][[1]])){setwd("D:/Dropbox (Zhukov research team)/XSub/Data/")}
if(grepl("Ubuntu 18.04 LTS"
         ,sessionInfo()[[4]])){setwd("/mnt/oldhdd/home/zhukov/Dropbox (Zhukov research team)/XSub/Data/")}


## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# Exceptions
source("Code/step3_aggregate/step3x_aggregate_admex.R")

# File census
srzz <- dir("Output")[grepl("^Output_",dir("Output"))&(!grepl("_ACD|_cdRw|_Cov|_FM|_Sul|_Wea",dir("Output")))]
srzz <- gsub("^Output_","",srzz)
censuz <- data.frame(SOURCE=srzz,REMAIN_PCT=NA,REMAIN_CLEA=NA,REMAIN_ADM=NA,REMAIN_PRIO=NA,stringsAsFactors = FALSE)
i <- 24
for(i in seq_along(srzz)){
  srzm <- srzz[i]
  filez <- dir(paste0("Output/Output_",srzm,"/Events"))
  filez <- filez[grep("Events",filez)]
  cntz <- gsub(".RData","",sapply(strsplit(filez,"_"),"[",3))
  cntz <- cntz[cntz!="000"]
  # CLEA
  cleaz <- dir("Input/GIS/Borders/CLEA/Beta")
  cleaz <- cleaz[grep("GRED",cleaz)]
  cleaz <- sapply(strsplit(gsub("[0-9]|GRED_","",cleaz),"_"),"[",1)
  cleaz <- gsub("DominicanRepublic","Dominican Republic",cleaz)
  cleaz <- gsub("SaintLucia","Saint Lucia",cleaz)
  cleaz <- gsub("MexicoPR","Mexico",cleaz)
  cleaz <- gsub("SouthAfrica","South Africa",cleaz)
  cleaz <- countrycode(cleaz,"country.name","iso3c")
  cleaz <- cleaz[!is.na(cleaz)]
  cleaz <- sort(unique(cleaz))
  cntz.cleaz <- cleaz[cleaz%in%cntz]
  filez.all.1 <- ""
  if(length(cntz.cleaz)>0){
    filez.all.1 <- paste0(srzm,"_",rep(cntz.cleaz,each=4),"_clea_",c("year","month","week","day"),".RData")
    if(grepl("^MELTT|^NIRI",srzm)){filez.all.1 <- filez.all.1[!grepl("clea_day",filez.all.1)]}
  }
  # GADM
  filez.all.2 <- paste0(srzm,"_",rep(cntz,each=4*3),"_adm",rep(0:2,each=4),"_",rep(c("year","month","week","day"),3),".RData")
  filez.all.2 <- filez.all.2[!grepl(paste0(c(paste0(noadm1,"_adm1"),paste0(noadm2,"_adm2")),collapse="|"),filez.all.2)]
  filez.all.2 <- filez.all.2[!grepl("adm2_day",filez.all.2)]
  # PRIO
  filez.all.3 <- paste0(srzm,"_",rep(cntz,each=4),"_priogrid_",c("year","month","week","day"),".RData")
  filez.all.3 <- filez.all.3[!grepl("priogrid_day",filez.all.3)]
  # Combine
  filez.all <- c(filez.all.1,filez.all.2,filez.all.3)
  filez.all <- filez.all[filez.all!=""]
  if(grepl("ESOCMexico",srzm)){
    filez.all <- filez.all[!grepl("_day|_month|_week",filez.all)]
  }
  if(grepl("NIRI",srzm)){
    filez.all <- filez.all[!grepl("_day",filez.all)]
  }
  
  # Store
  
  dir(paste0("Output/Output_",srzm))
  
  filez.all[!filez.all%in%dir(paste0("Output/Output_",srzm))]
  censuz$REMAIN_PCT[i] <- round(1-mean(filez.all%in%dir(paste0("Output/Output_",srzm))),2)*100
  censuz$REMAIN_CLEA[i] <- sum(grepl("_clea",filez.all[!filez.all%in%dir(paste0("Output/Output_",srzm))]))
  censuz$REMAIN_ADM[i] <- sum(grepl("_adm",filez.all[!filez.all%in%dir(paste0("Output/Output_",srzm))]))
  censuz$REMAIN_PRIO[i] <- sum(grepl("_prio",filez.all[!filez.all%in%dir(paste0("Output/Output_",srzm))]))
  
}

print(censuz)