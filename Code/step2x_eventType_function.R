eventType <- function(subdata,idvar,textvar,term.type=term.type,
                      types.specific=types.specific,types.general=types.general){
  

  # Create empty matrix
  events <- as.data.frame(matrix(NA,nrow=nrow(subdata),
                                 ncol=(1+length(types.specific)+length(types.general))))
  names(events) <- c(idvar,types.specific,types.general)
  events[,idvar] <- subdata[,idvar]
  
  # Specific event categories
  for(k in 1:length(types.specific)){
    events[,types.specific[k]] <- 0
    strz <- paste0(row.names(term.type)[term.type[,types.specific[k]]==1],
                   collapse="|")
    if(strz!=""){events[grep(strz,subdata[,textvar]),types.specific[k]] <- 1}
  }
  
  # General event categories
  events[,types.general] <- 0
  events$ANY <- 1
  events$DIR <- apply(events[,c("AMBUSH","ARREST","FIREFIGHT","KIDNAP","KILLING",
                                "KILLING_A")],1,function(x){1*(sum(x)>0)})
  events$IND <- apply(events[,c("AIRSTRIKE","ARMOR","ARTILLERY","BOMB","CHEMICAL",
                                "CIV_ABUSE","DISPLACE","RAID","ROCKET","SIEGE","STORM","SUICIDE","TERROR")],1,
                      function(x){1*(sum(x)>0)}) 
  events$PRT <- apply(events[,c("PROTEST","PROTEST_V","RIOT")],1,
                      function(x){1*(sum(x)>0)})
  names(events) <- c(idvar,paste0("ACTION_",types.specific),paste0("ACTION_",
                                                                   types.general))
  
  return(events)
}
