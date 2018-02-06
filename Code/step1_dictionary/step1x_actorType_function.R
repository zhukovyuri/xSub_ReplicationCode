actorType <- function(data.raw,textvar,actor.type=actor.type){
  
  # Create empty matrix
  actors <- as.data.frame(matrix(NA,nrow=nrow(data.raw),ncol=(1+ncol(actor.type))))
  names(actors) <- c(textvar,names(actor.type))
  actors[,textvar] <- data.raw[,textvar]
  
  # Actor categories
  for(k in 1:ncol(actor.type)){
    actors[,names(actor.type)[k]] <- 0
    strz <- paste0(row.names(actor.type)[actor.type[,names(actor.type)[k]]==1],collapse="|")
    if(strz!=""){actors[grep(strz,data.raw[,textvar]),names(actor.type)[k]] <- 1}
  }
  
  # # General Actor type categories
  # actors[,names(actor.type)] <- 0
  # actors$OTH <- 1
  # actors$CIV <- apply(actors[,c("CIVILIAN","CITIZEN")],1,function(x){1*(sum(x)>0)})
  # actors$GOV <- apply(actors[,c("MILITARY","SOLDIERS","GOVERNMENT FORCES","ARMY")],1,
  #                     function(x){1*(sum(x)>0)}) 
  # actors$REB <- apply(actors[,c("REBELS")],1,
  #                     function(x){1*(sum(x)>0)})
  # names(actors) <- c(textvar,paste0("ACTION_",types.specific),paste0("ACTION_",
  #                                                                  types.general))
  
  return(actors)
}



# ###################
# ## Sample code
# ###################
# 
# # Load data
# data.raw <- read.dta("Input/Events/ESOC/WITS/Paksitan Geo-referenced WITS Data (2004-2009)/PK_geo_wits_20100608.dta")
# head(data.raw)
# textvar <- "subject"
# 
# # Load actor dictionary
# load("Dictionaries/ActorTypes/Combined_ActorDictionary.RData")
# 
# # Run function
# actors <- actorType(data.raw,textvar,actor.type=actor.type)
# head(actors)
