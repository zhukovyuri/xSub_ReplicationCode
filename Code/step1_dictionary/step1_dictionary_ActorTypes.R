rm(list=ls())


## Set directory
# setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","tm")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)



#############################
## ESOC: Iraq WITS
#############################

## Load raw data
esoc.raw <- read.dta("Input/Events/ESOC/WITS/Iraq WITS Data/iraq_wits_attacks_v3.dta")
data <- esoc.raw; rm(esoc.raw)
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)
#data$Type <- NA
#data$Type <- "Killing"
#save(data, file="Input/Events/ESOC/Mexico/esoc_MEX_drug_related_murders.RData")

# Extract event descriptions 
data$TEXT <- data$SUBJECT
textvar <- "TEXT"
articles <- data[,textvar]
tail(articles)
articles[200:210]


# Create corpus
docs <- Corpus(VectorSource(articles))
docs

## Natural language processing
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
patterns <- c("/|@|\\|","<.+?>","\\t")
docs2 <- docs
for(p in 1:length(patterns)){docs2 <- tm_map(docs2, toSpace, patterns[p])}
docs2 <- tm_map(docs2, PlainTextDocument)
docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, stripWhitespace)
docs2 <- tm_map(docs2, removeWords, stopwords("english"))
docs2 <- tm_map(docs2, stemDocument)
docs2 <- tm_map(docs2, removePunctuation)
docs2 <- tm_map(docs2, removeNumbers)
docsDTM <- DocumentTermMatrix(docs2)
docsDTM <- removeSparseTerms(docsDTM, 0.9995)

# Extract terms
terms <- docsDTM$dimnames$Terms
terms

# Actor types
types.actors <- c("SIDEA","SIDEB","SIDEC","SIDED")

# Create dictionary table (specific types)
actor.type <- as.data.frame(matrix("",nrow=length(terms),ncol=length(types.actors)))
row.names(actor.type) <- terms
names(actor.type) <- types.actors
actor.type <- edit(actor.type)

for(j in 1:ncol(actor.type)){actor.type[,j] <- as.factor(as.character(actor.type[,j]));actor.type[is.na(actor.type[,j]),j] <- ""}
head(actor.type)
save(actor.type,file="Dictionaries/ActorTypes/ESOC/ESOC_Iraq_WITS_ActorDictionary.RData")


#############################
## ESOC: Pakistan WITS
#############################

## Load raw data
esoc.raw <- read.dta("Input/Events/ESOC/WITS/Paksitan Geo-referenced WITS Data (2004-2009)/PK_geo_wits_20100608.dta")
data <- esoc.raw; rm(esoc.raw)
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)
#data$Type <- NA
#data$Type <- "Killing"
#save(data, file="Input/Events/ESOC/Mexico/esoc_MEX_drug_related_murders.RData")

# Extract event descriptions 
data$TEXT <- data$SUBJECT
textvar <- "TEXT"
articles <- data[,textvar]
tail(articles)
articles[200:210]

# Create corpus
docs <- Corpus(VectorSource(articles))
docs

## Natural language processing
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
patterns <- c("/|@|\\|","<.+?>","\\t")
docs2 <- docs
for(p in 1:length(patterns)){docs2 <- tm_map(docs2, toSpace, patterns[p])}
docs2 <- tm_map(docs2, PlainTextDocument)
docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, stripWhitespace)
docs2 <- tm_map(docs2, removeWords, stopwords("english"))
docs2 <- tm_map(docs2, stemDocument)
docs2 <- tm_map(docs2, removePunctuation)
docs2 <- tm_map(docs2, removeNumbers)
docsDTM <- DocumentTermMatrix(docs2)
docsDTM <- removeSparseTerms(docsDTM, 0.9995)

# Extract terms
terms <- docsDTM$dimnames$Terms
terms

# Actor types
types.actors <- c("SIDEA","SIDEB","SIDEC","SIDED")

# Create dictionary table (specific types)
actor.type <- as.data.frame(matrix("",nrow=length(terms),ncol=length(types.actors)))
row.names(actor.type) <- terms
names(actor.type) <- types.actors
actor.type <- edit(actor.type)

for(j in 1:ncol(actor.type)){actor.type[,j] <- as.factor(as.character(actor.type[,j]));actor.type[is.na(actor.type[,j]),j] <- ""}
head(actor.type)
save(actor.type,file="Dictionaries/ActorTypes/ESOC/ESOC_Pakistan_WITS_ActorDictionary.RData")




######################################################################
######################################################################
## Create combined event dictionary: ESOC
######################################################################
######################################################################

rm(list=ls())

# Load dictionaries
load("Dictionaries/ActorTypes/ESOC/ESOC_Iraq_WITS_ActorDictionary.RData")
actor.type$TERM <- row.names(actor.type)
t1 <- actor.type; rm(actor.type)
load("Dictionaries/ActorTypes/ESOC/ESOC_Pakistan_WITS_ActorDictionary.RData")
actor.type$TERM <- row.names(actor.type)
t2 <- actor.type; rm(actor.type)

# Merge
t0 <- rbind(t1,t2)

# Variable names
varz <- names(t0)[!names(t0)%in%"TERM"]

for(j in 1:length(varz)){t0[,varz[j]] <- as.numeric(as.character(t0[,varz[j]]))}
t0 <- aggregate(t0[,varz],by=list(TERM=t0$TERM),function(x){1*(sum(x,na.rm=T)>0)})
dim(t0)

# Clean
j <- 4
print(varz[j]); t0$TERM[t0[,varz[j]]==1]
t0[t0$TERM%in%c("administ") ,"SIDEA"] <- 1

# Add terms
v_c <- c("rebel")
v_n <- "SIDEB"
for(k in 1:length(v_c)){
  t_add <- as.data.frame(matrix(0,nrow=1,ncol=length(varz)+1))
  names(t_add) <- names(t0)
  t_add$TERM <- as.character(t_add$TERM)
  t_add$TERM <- v_c[k]
  t_add[,v_n] <- 1
  t0 <- rbind(t0,t_add)
}
t0 <- t0[order(t0$TERM),]
row.names(t0) <- t0$TERM
summary(t0)
dim(t0)

# Save
actor.type <- t0
actor.type$TERM <- NULL
save(actor.type,file="Dictionaries/ActorTypes/Combined_ActorDictionary.RData")
# load("Dictionaries/ActorTypes/Combined_ActorDictionary.RData")

actor.type[1:10,]
head(actor.type)
