
rm(list=ls())

## Set directory
setwd("~/")
if("XSub"%in%dir()){setwd("~/XSub/Data/")}
if("Dropbox2"%in%dir()){setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")}
if(grepl("w64",sessionInfo()[[1]][[1]])){setwd("D:/Dropbox (Zhukov research team)/XSub/Data/")}

# File census
source("Code/ScratchPad/step3x_census.R")
censuz <<- censuz
srzz <- censuz$SOURCE
source("Code/ScratchPad/step3x_finishloop_par.R")
print(srz)

while(censuz[censuz$SOURCE%in%c(srz),"REMAIN_PCT"]>thres){
  
  srz0 <- srz
  if(grepl("MELTT",srz)){
    srz0 <- gsub("MELTT","MELTT_",srz0)
    srz0 <- gsub("km|d","",srz0)
  }
  source(paste0("Code/ScratchPad/step3_aggregate_",srz0,".R"))
  source("Code/ScratchPad/step3x_census.R")
  censuz <<- censuz
  srzz <- censuz$SOURCE
  source("Code/ScratchPad/step3x_finishloop_par.R")
  print(srz)
}
