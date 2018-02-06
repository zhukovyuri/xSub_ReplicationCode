rm(list=ls())

## Set directory
#setwd("~/Dropbox2/Dropbox (Zhukov research team)/XSub/Data/")
# setwd("F:/Dropbox (Zhukov research team)/XSub/Data/")
setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")

## Install & load packages (all at once)
list.of.packages <- c("gdata","countrycode","maptools","foreign","plotrix","sp","raster","rgeos","gdata","tm")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


#############################
## GED
#############################


## Load raw data
load("Input/Events/UCDP_GED/ged30.RData")
data <- ged.raw
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)

# Extract event descriptions 
data$TEXT <- data$SOURCE_ARTICLE
textvar <- "TEXT"
articles <- data[,textvar]

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
docsDTM <- removeSparseTerms(docsDTM, 0.999)

# Extract terms
terms <- docsDTM$dimnames$Terms
terms

# Event types
source("Code/step2_eventcode/step2x_event_types_list.R")

# Create dictionary table (specific types)
term.type <- as.data.frame(matrix("",nrow=length(terms),ncol=length(types.specific)))
row.names(term.type) <- terms
names(term.type) <- types.specific
term.type <- edit(term.type)
for(j in 1:ncol(term.type)){term.type[,j] <- as.factor(as.character(term.type[,j]));term.type[is.na(term.type[,j]),j] <- ""}
head(term.type)
summary(term.type)
#save(term.type,file="Dictionaries/EventTypes/GED/GED_EventDictionary.RData")



#############################
## SCAD
#############################
rm(list=ls())


## Load raw data
dir("Input/Events/SCAD")
load("Input/Events/SCAD/scad_Africa_32.RData")
data <- scad.raw
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)

# Extract event descriptions 
data$TEXT <- data$ISSUENOTE
textvar <- "TEXT"
articles <- data[,textvar]

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
docsDTM <- removeSparseTerms(docsDTM, 0.998)

# Extract terms
terms <- docsDTM$dimnames$Terms
terms

# Event types
source("Code/step2_eventcode/step2x_event_types_list.R")

# Create dictionary table (specific types)
term.type <- as.data.frame(matrix("",nrow=length(terms),ncol=length(types.specific)))
row.names(term.type) <- terms
names(term.type) <- types.specific
term.type <- edit(term.type)
for(j in 1:ncol(term.type)){term.type[,j] <- as.factor(as.character(term.type[,j]));term.type[is.na(term.type[,j]),j] <- ""}
head(term.type)
summary(term.type)
#save(term.type,file="Dictionaries/EventTypes/SCAD/SCAD_EventDictionary.RData")


#############################
## ACLED
#############################
rm(list=ls())

## Load raw data
data <- read.csv("Input/Events/ACLED/ACLED_Asia/acled_Asia_2015-2016.csv",encoding="utf-8")
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)

# Extract event descriptions 
data$TEXT <- paste(data$EVENT_TYPE, data$NOTES)
data$TEXT <- gsub("No change of territory","noterritorychange",data$TEXT)
data$TEXT <- gsub("Violence against civilians","anticivilian",data$TEXT)
data$TEXT <- iconv(data$TEXT,"WINDOWS-1252","UTF-8")
data$TEXT <- tolower(data$TEXT)
textvar <- "TEXT"
articles <- data[,textvar]


# Create corpus
docs <- Corpus(VectorSource(articles))
docs

## Natural language processing
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
patterns <- c("/|@|\\|","<.+?>","\\t")
#patterns <- c("/|@|\\|","<.+?>","\\t","\\\U3e61633c","\\\U3e65383c","\\\U3e32643c","\\\U3e33643c","\\\U3e66383c","\\\U3e35643c","\\\U3e35393c","\\\U3e31393c","\\\U3e64383c","\\\U3e31643c","\\\U3e31643c","\\\U3e38383c","\\\U3e30643c","\\\U3e65613c","\\\U3e34643c","\\\U3e39383c","\\\U3e62383c","\\\U3e39633c","\\\U3e39393c","\\\U3e37633c","\\\U3e38633c","\\\U3e32383c","\\\U3e35383c","\\\U3e33613c","\\\U3e34623c","\\\U3e65393c","\\\U3e35653c","\\\U3e33383c","\\\U3e33383c","\\\U3e32633c","\\\U3e37393c","\\\U3e37383c","\\\U3e63393c","\\\U3e61393c","\\\U3e38643c","\\\U3e31613c","\\\U3e31653c","\\\U3e65633c","\\\U3e30393c","\\\U3e32653c")
docs2 <- docs
for(p in 1:length(patterns)){docs2 <- tm_map(docs2, toSpace, patterns[p])}
docs2 <- tm_map(docs2, PlainTextDocument)
docs2 <- tm_map(docs2, content_transformer(tolower),lazy=TRUE)
docs2 <- tm_map(docs2, stripWhitespace,lazy=TRUE)
docs2 <- tm_map(docs2, removeWords, stopwords("english"),lazy=TRUE)
docs2 <- tm_map(docs2, stemDocument,lazy=TRUE)
docs2 <- tm_map(docs2, removePunctuation,lazy=TRUE)
docs2 <- tm_map(docs2, removeNumbers,lazy=TRUE)
docsDTM <- DocumentTermMatrix(docs2)
docsDTM <- removeSparseTerms(docsDTM, 0.998)

# Extract terms
terms <- docsDTM$dimnames$Terms
terms

# Event types
source("Code/step2_eventcode/step2x_event_types_list.R")

# Create dictionary table (specific types)
term.type <- as.data.frame(matrix("",nrow=length(terms),ncol=length(types.specific)))
row.names(term.type) <- terms
names(term.type) <- types.specific
term.type <- edit(term.type)
for(j in 1:ncol(term.type)){term.type[,j] <- as.factor(as.character(term.type[,j]));term.type[is.na(term.type[,j]),j] <- ""}
head(term.type)
summary(term.type)
save(term.type,file="Dictionaries/EventTypes/ACLED/ACLED_Asia_EventDictionary.RData")





#############################
## PITF (20160101_20161231)
#############################
rm(list=ls())

## Load raw data
load("Input/Events/PITF/pitf_20160101_20161231.RData")
data <- pitf.raw; rm(pitf.raw)
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)

# Extract event descriptions 
data$TEXT <- paste(gsub(" |\\/","",gdata:::trim(data$WEAPONS)),gsub(" |\\/","",gdata:::trim(data$COLLATERAL.DAMAGE)),data$DESCRIPTION)
data$TEXT <- gsub("NotCollateralDamage|NoCollateralDamage","NotCollDamage",data$TEXT)
data$TEXT <- iconv(data$TEXT,"WINDOWS-1252","UTF-8")
data$TEXT <- tolower(data$TEXT)
textvar <- "TEXT"
articles <- data[,textvar]


# Create corpus
docs <- Corpus(VectorSource(articles))
docs

## Natural language processing
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
patterns <- c("/|@|\\|","<.+?>","\\t")
#patterns <- c("/|@|\\|","<.+?>","\\t","\\\U3e61633c","\\\U3e65383c","\\\U3e32643c","\\\U3e33643c","\\\U3e66383c","\\\U3e35643c","\\\U3e35393c","\\\U3e31393c","\\\U3e64383c","\\\U3e31643c","\\\U3e31643c","\\\U3e38383c","\\\U3e30643c","\\\U3e65613c","\\\U3e34643c","\\\U3e39383c","\\\U3e62383c","\\\U3e39633c","\\\U3e39393c","\\\U3e37633c","\\\U3e38633c","\\\U3e32383c","\\\U3e35383c","\\\U3e33613c","\\\U3e34623c","\\\U3e65393c","\\\U3e35653c","\\\U3e33383c","\\\U3e33383c","\\\U3e32633c","\\\U3e37393c","\\\U3e37383c","\\\U3e63393c","\\\U3e61393c","\\\U3e38643c","\\\U3e31613c","\\\U3e31653c","\\\U3e65633c","\\\U3e30393c","\\\U3e32653c")
docs2 <- docs
for(p in 1:length(patterns)){docs2 <- tm_map(docs2, toSpace, patterns[p])}
docs2 <- tm_map(docs2, PlainTextDocument)
docs2 <- tm_map(docs2, content_transformer(tolower),lazy=TRUE)
docs2 <- tm_map(docs2, stripWhitespace,lazy=TRUE)
docs2 <- tm_map(docs2, removeWords, stopwords("english"),lazy=TRUE)
docs2 <- tm_map(docs2, stemDocument,lazy=TRUE)
docs2 <- tm_map(docs2, removePunctuation,lazy=TRUE)
docs2 <- tm_map(docs2, removeNumbers,lazy=TRUE)
docsDTM <- DocumentTermMatrix(docs2)
docsDTM <- removeSparseTerms(docsDTM, 0.999)

# Extract terms
terms <- docsDTM$dimnames$Terms
terms

# Event types
source("Code/step2_eventcode/step2x_event_types_list.R")

# Create dictionary table (specific types)
term.type <- as.data.frame(matrix("",nrow=length(terms),ncol=length(types.specific)))
row.names(term.type) <- terms
names(term.type) <- types.specific
term.type <- edit(term.type)
for(j in 1:ncol(term.type)){term.type[,j] <- as.factor(as.character(term.type[,j]));term.type[is.na(term.type[,j]),j] <- ""}
head(term.type)
summary(term.type)
#save(term.type,file="Dictionaries/EventTypes/PITF/PITF_20160101_20161231_EventDictionary.RData")



#############################
## PITF (2013_2015)
#############################
rm(list=ls())

## Load raw data
load("Input/Events/PITF/pitf_2013_2015.RData")
data <- pitf.raw; rm(pitf.raw)
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)

# Extract event descriptions 
data$TEXT <- paste(gsub(" |\\/","",gdata:::trim(data$WEAPONS)),gsub(" |\\/","",gdata:::trim(data$COLLATERAL.DAMAGE)),data$DESCRIPTION)
data$TEXT <- gsub("NotCollateralDamage|NoCollateralDamage","NotCollDamage",data$TEXT)
data$TEXT <- iconv(data$TEXT,"WINDOWS-1252","UTF-8")
data$TEXT <- tolower(data$TEXT)
textvar <- "TEXT"
articles <- data[,textvar]


# Create corpus
docs <- Corpus(VectorSource(articles))
docs

## Natural language processing
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
patterns <- c("/|@|\\|","<.+?>","\\t")
#patterns <- c("/|@|\\|","<.+?>","\\t","\\\U3e61633c","\\\U3e65383c","\\\U3e32643c","\\\U3e33643c","\\\U3e66383c","\\\U3e35643c","\\\U3e35393c","\\\U3e31393c","\\\U3e64383c","\\\U3e31643c","\\\U3e31643c","\\\U3e38383c","\\\U3e30643c","\\\U3e65613c","\\\U3e34643c","\\\U3e39383c","\\\U3e62383c","\\\U3e39633c","\\\U3e39393c","\\\U3e37633c","\\\U3e38633c","\\\U3e32383c","\\\U3e35383c","\\\U3e33613c","\\\U3e34623c","\\\U3e65393c","\\\U3e35653c","\\\U3e33383c","\\\U3e33383c","\\\U3e32633c","\\\U3e37393c","\\\U3e37383c","\\\U3e63393c","\\\U3e61393c","\\\U3e38643c","\\\U3e31613c","\\\U3e31653c","\\\U3e65633c","\\\U3e30393c","\\\U3e32653c")
docs2 <- docs
for(p in 1:length(patterns)){docs2 <- tm_map(docs2, toSpace, patterns[p])}
docs2 <- tm_map(docs2, PlainTextDocument)
docs2 <- tm_map(docs2, content_transformer(tolower),lazy=TRUE)
docs2 <- tm_map(docs2, stripWhitespace,lazy=TRUE)
docs2 <- tm_map(docs2, removeWords, stopwords("english"),lazy=TRUE)
docs2 <- tm_map(docs2, stemDocument,lazy=TRUE)
docs2 <- tm_map(docs2, removePunctuation,lazy=TRUE)
docs2 <- tm_map(docs2, removeNumbers,lazy=TRUE)
docsDTM <- DocumentTermMatrix(docs2)
docsDTM <- removeSparseTerms(docsDTM, 0.9975)

# Extract terms
terms <- docsDTM$dimnames$Terms
terms

# Event types
source("Code/step2_eventcode/step2x_event_types_list.R")

# Create dictionary table (specific types)
term.type <- as.data.frame(matrix("",nrow=length(terms),ncol=length(types.specific)))
row.names(term.type) <- terms
names(term.type) <- types.specific
term.type <- edit(term.type)
for(j in 1:ncol(term.type)){term.type[,j] <- as.factor(as.character(term.type[,j]));term.type[is.na(term.type[,j]),j] <- ""}
head(term.type)
summary(term.type)
#save(term.type,file="Dictionaries/EventTypes/PITF/PITF_2013_2015_EventDictionary.RData")



#############################
## PITF (1995-2012)
#############################
rm(list=ls())

## Load raw data
load("Input/Events/PITF/pitf_1995_2012.RData")
data <- pitf.raw; rm(pitf.raw)
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)

# Extract event descriptions 


gsub("NotCollateralDamage|NoCollateralDamage","NotCollDamage",gsub(" |\\/","",gdata:::trim(data$COLLATERAL.DAMAGE)))
data$TEXT <- paste(gsub(" |\\/","",gdata:::trim(data$WEAPONS)),gsub(" |\\/","",gdata:::trim(data$COLLATERAL.DAMAGE)),data$DESCRIPTION)
data$TEXT <- gsub("NotCollateralDamage|NoCollateralDamage","NotCollDamage",data$TEXT)
data$TEXT <- iconv(data$TEXT,"WINDOWS-1252","UTF-8")
data$TEXT <- tolower(data$TEXT)
textvar <- "TEXT"
articles <- data[,textvar]


# Create corpus
docs <- Corpus(VectorSource(articles))
docs

## Natural language processing
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
patterns <- c("/|@|\\|","<.+?>","\\t")
#patterns <- c("/|@|\\|","<.+?>","\\t","\\\U3e61633c","\\\U3e65383c","\\\U3e32643c","\\\U3e33643c","\\\U3e66383c","\\\U3e35643c","\\\U3e35393c","\\\U3e31393c","\\\U3e64383c","\\\U3e31643c","\\\U3e31643c","\\\U3e38383c","\\\U3e30643c","\\\U3e65613c","\\\U3e34643c","\\\U3e39383c","\\\U3e62383c","\\\U3e39633c","\\\U3e39393c","\\\U3e37633c","\\\U3e38633c","\\\U3e32383c","\\\U3e35383c","\\\U3e33613c","\\\U3e34623c","\\\U3e65393c","\\\U3e35653c","\\\U3e33383c","\\\U3e33383c","\\\U3e32633c","\\\U3e37393c","\\\U3e37383c","\\\U3e63393c","\\\U3e61393c","\\\U3e38643c","\\\U3e31613c","\\\U3e31653c","\\\U3e65633c","\\\U3e30393c","\\\U3e32653c")
docs2 <- docs
for(p in 1:length(patterns)){docs2 <- tm_map(docs2, toSpace, patterns[p])}
docs2 <- tm_map(docs2, PlainTextDocument)
docs2 <- tm_map(docs2, content_transformer(tolower),lazy=TRUE)
docs2 <- tm_map(docs2, stripWhitespace,lazy=TRUE)
docs2 <- tm_map(docs2, removeWords, stopwords("english"),lazy=TRUE)
docs2 <- tm_map(docs2, stemDocument,lazy=TRUE)
docs2 <- tm_map(docs2, removePunctuation,lazy=TRUE)
docs2 <- tm_map(docs2, removeNumbers,lazy=TRUE)
docsDTM <- DocumentTermMatrix(docs2)
docsDTM <- removeSparseTerms(docsDTM, 0.99)

# Extract terms
terms <- docsDTM$dimnames$Terms
terms

# Event types
source("Code/step2_eventcode/step2x_event_types_list.R")

# Create dictionary table (specific types)
term.type <- as.data.frame(matrix("",nrow=length(terms),ncol=length(types.specific)))
row.names(term.type) <- terms
names(term.type) <- types.specific
term.type <- edit(term.type)
for(j in 1:ncol(term.type)){term.type[,j] <- as.factor(as.character(term.type[,j]));term.type[is.na(term.type[,j]),j] <- ""}
head(term.type)
summary(term.type)
#save(term.type,file="Dictionaries/EventTypes/PITF/PITF_1995_2012_EventDictionary.RData")



#############################
## Zhukov: Chechnya
#############################


## Load raw data
load("Input/Events/Zhukov/Chechnya/CHECHNYA_Events.RData")
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)

# Extract event descriptions 
data$TEXT <- data$TYPE
textvar <- "TEXT"
articles <- data[,textvar]

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
# docsDTM <- removeSparseTerms(docsDTM, 0.999)

# Extract terms
terms <- docsDTM$dimnames$Terms
terms

# Event types
source("Code/step2_eventcode/step2x_event_types_list.R")

# Create dictionary table (specific types)
term.type <- as.data.frame(matrix("",nrow=length(terms),ncol=length(types.specific)))
row.names(term.type) <- terms
names(term.type) <- types.specific
term.type <- edit(term.type)
for(j in 1:ncol(term.type)){term.type[,j] <- as.factor(as.character(term.type[,j]));term.type[is.na(term.type[,j]),j] <- ""}
head(term.type)
summary(term.type)
# save(term.type,file="Dictionaries/EventTypes/yzChechnya/yzChechnya_EventDictionary.RData")


#############################
## Zhukov: Caucasus
#############################


## Load raw data
load("Input/Events/Zhukov/Russia/EVENTS_clean.RData")
data <- yz.raw; rm(yz.raw)
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)

# Extract event descriptions 
data$TEXT <- data$TYPE
textvar <- "TEXT"
articles <- data[,textvar]

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
# docsDTM <- removeSparseTerms(docsDTM, 0.999)

# Extract terms
terms <- docsDTM$dimnames$Terms
terms

# Event types
source("Code/step2_eventcode/step2x_event_types_list.R")

# Create dictionary table (specific types)
term.type <- as.data.frame(matrix("",nrow=length(terms),ncol=length(types.specific)))
row.names(term.type) <- terms
names(term.type) <- types.specific
term.type <- edit(term.type)
for(j in 1:ncol(term.type)){term.type[,j] <- as.factor(as.character(term.type[,j]));term.type[is.na(term.type[,j]),j] <- ""}
head(term.type)
summary(term.type)
# save(term.type,file="Dictionaries/EventTypes/yzCaucasus2000/yzCaucasus2000_EventDictionary.RData")


#############################
## ESOC: Pakistan, Mexico
#############################

## Load raw data
#load("Input/Events/ESOC/BFRS/esoc_PK_v10.RData")
#load("Input/Events/ESOC/Mexico/esoc_MEX_homicide.RData")
#load("Input/Events/ESOC/Mexico/esoc_MEX_drug_related_murders.RData")
#load("Input/Events/ESOC/SIGACT/esoc_iraq_week_district.RData")
#load("Input/Events/ESOC/WITS/Afghanistan Geo-referenced WITS Data (2005-2009)/AF_geo_wits_20101105.RData")
#load("Input/Events/ESOC/WITS/Iraq WITS Data/Iraq_WITS_GEO.RData")
data <- read.dta("Input/Events/ESOC/WITS/Paksitan Geo-referenced WITS Data (2004-2009)/PK_geo_wits_20100608.dta")

data <- data; rm(esoc.data)
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)
colnames(data)

# Extract event descriptions 
data$TEXT <- data$SUBJECT
textvar <- "TEXT"
articles <- data[,textvar]

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
docsDTM <- removeSparseTerms(docsDTM, 0.999)

# Extract terms
terms <- docsDTM$dimnames$Terms
terms

# Event types
source("Code/step2_eventcode/step2x_event_types_list.R")

# Create dictionary table (specific types)
term.type <- as.data.frame(matrix("",nrow=length(terms),ncol=length(types.specific)))
row.names(term.type) <- terms
names(term.type) <- types.specific
term.type <- edit(term.type)
for(j in 1:ncol(term.type)){term.type[,j] <- as.factor(as.character(term.type[,j]));term.type[is.na(term.type[,j]),j] <- ""}
head(term.type)
summary(term.type)
#save(term.type,file="Dictionaries/EventTypes/ESOC/ESOC_Mexico_homicide_EventDictionary.RData")
#save(term.type,file="Dictionaries/EventTypes/ESOC/ESOC_Mexico_drug_related_murders_EventDictionary.RData")
#save(term.type, file="Dictionaries/EventTypes/ESOC/ESOC_AF_geo_wits_20101105_EventDictionary.RData")
#save(term.type, file="Dictionaries/EventTypes/ESOC/ESOC_IRQ_WITS_EventDictionary.RData")
save(term.type, file="Dictionaries/EventTypes/ESOC/ESOC_PAK_WITS_EventDictionary.RData")

#############################
## Davenport: Northern Ireland
#############################

## Load raw data
load("Input/Events/Davenport/Northern Ireland/NOrthernIreland_20140211.RData")
##no text decription - skipping this step. 

data <- davenport.raw; rm(davenport.raw)
classez<-c();for(j in 1:ncol(data)){classez[j]<-class(data[,j]);if(classez[j]=="factor"){data[,j]<-as.character(data[,j])}}
names(data) <- toupper(names(data))
head(data)
#data$Type <- NA
#data$Type <- "Killing"
#save(data, file="Input/Events/ESOC/Mexico/esoc_MEX_drug_related_murders.RData")

# Extract event descriptions 
data$TEXT <- data$SUMMARY
textvar <- "TEXT"
articles <- data[,textvar]

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
docsDTM <- removeSparseTerms(docsDTM, 0.999)

# Extract terms
terms <- docsDTM$dimnames$Terms
terms

# Event types
source("Code/step2_eventcode/step2x_event_types_list.R")

# Create dictionary table (specific types)
term.type <- as.data.frame(matrix("",nrow=length(terms),ncol=length(types.specific)))
row.names(term.type) <- terms
names(term.type) <- types.specific
term.type <- edit(term.type)
for(j in 1:ncol(term.type)){term.type[,j] <- as.factor(as.character(term.type[,j]));term.type[is.na(term.type[,j]),j] <- ""}
head(term.type)
summary(term.type)
#save(term.type,file="Dictionaries/EventTypes/ESOC/ESOC_Mexico_homicide_EventDictionary.RData")
#save(term.type,file="Dictionaries/EventTypes/ESOC/ESOC_Mexico_drug_related_murders_EventDictionary.RData")
#save(term.type, file="Dictionaries/EventTypes/ESOC/ESOC_AF_geo_wits_20101105_EventDictionary.RData")


####################
#Sullivan: Guatemala 
####################

## Load raw data
load("Input/Events/Sullivan/ceh-massacres.RData")
head(Sullivan.raw)
##no text decription - skipping this step. 

######################################################################
######################################################################
## Create combined event dictionary
######################################################################
######################################################################

rm(list=ls())

# Load dictionaries
load("Dictionaries/EventTypes/GED/GED_EventDictionary.RData")
term.type$TERM <- row.names(term.type)
t1 <- term.type; rm(term.type)
load("Dictionaries/EventTypes/SCAD/SCAD_EventDictionary.RData")
term.type$TERM <- row.names(term.type)
t2 <- term.type; rm(term.type)
load("Dictionaries/EventTypes/ACLED/ACLED_EventDictionary.RData")
term.type$TERM <- row.names(term.type)
t3 <- term.type; rm(term.type)
load("Dictionaries/EventTypes/PITF/PITF_20160101_20161231_EventDictionary.RData")
term.type$TERM <- row.names(term.type)
t4 <- term.type; rm(term.type)
load("Dictionaries/EventTypes/PITF/PITF_2013_2015_EventDictionary.RData")
term.type$TERM <- row.names(term.type)
t5 <- term.type; rm(term.type)
load("Dictionaries/EventTypes/PITF/PITF_1995_2012_EventDictionary.RData")
term.type$TERM <- row.names(term.type)
t6 <- term.type; rm(term.type)
load("Dictionaries/EventTypes/yzChechnya/yzChechnya_EventDictionary.RData")
term.type$TERM <- row.names(term.type)
t7 <- term.type; rm(term.type)
load("Dictionaries/EventTypes/yzCaucasus2000/yzCaucasus2000_EventDictionary.RData")
term.type$TERM <- row.names(term.type)
t8 <- term.type; rm(term.type)
t9 <- load("Dictionaries/EventTypes/ACLED/ACLED_Asia_EventDictionary.RData")
term.type$TERM <- row.names(term.type)
t10 <- load("Dictionaries/EventTypes/ESOC/ESOC_PK_EventDictionary.RData")
term.type$TERM <- row.names(term.type)
t11 <- load("Dictionaries/EventTypes/ESOC/ESOC_Mexico_homicide_EventDictionary.RData")
term.type$TERM <- row.names(term.type)
t12 <- load("Dictionaries/EventTypes/ESOC/ESOC_AF_geo_wits_20101105_EventDictionary.RData")
term.type$TERM <- row.names(term.type)

# Merge
t0 <- rbind(t1,t2,t3,t4,t5,t6,t7,t8, t9, t10, t11, t12)

# Variable names
varz <- names(t0)[!names(t0)%in%"TERM"]

for(j in 1:length(varz)){t0[,varz[j]] <- as.numeric(as.character(t0[,varz[j]]))}
t0 <- aggregate(t0[,varz],by=list(TERM=t0$TERM),function(x){1*(sum(x,na.rm=T)>0)})
dim(t0)

# Clean
j <- 24
print(varz[j]); t0$TERM[t0[,varz[j]]==1]
t0[t0$TERM%in%c("launch") ,"AIRSTRIKE"] <- 0
t0[t0$TERM%in%c("helicopt") ,"AMBUSH"] <- 0
t0[t0$TERM%in%c("apc","tank") ,"ARMOR"] <- 1
t0[t0$TERM%in%c("assail","attack","attacks","battl","battle","battlegovern","battlenonst","battlenoterritorychang","clash","clashes","combat","fight","fighting","fire","round") ,"FIREFIGHT"] <- 0
t0[t0$TERM%in%c("casualti","dead","death","die","fatalitiesaspx","kill","killed","massacr") ,"KILLING"] <- 0
t0[t0$TERM%in%c("captur","surround") ,"OCCUPY"] <- 0
t0[t0$TERM%in%c("nonviol") ,"PROPERTY"] <- 0
t0[t0$TERM%in%c("antigovern","strike","opponents","oppos","opposit") ,"PROTEST"] <- 0
t0[t0$TERM%in%c("offens","operation","operations") ,"RAID"] <- 0
t0[t0$TERM%in%c("repres") ,"RIOTCONTROL"] <- 0
t0[t0$TERM%in%c("tear","tearga") ,"RIOTCONTROL"] <- 1
t0[t0$TERM%in%c("loot") ,"ROBBERY"] <- 0
t0[t0$TERM%in%c("launch","rock") ,"ROCKET"] <- 0
t0[t0$TERM%in%c("land","move") ,"STORM"] <- 0
t0[t0$TERM%in%c("bomber") ,"SUICIDE"] <- 0
t0[t0$TERM%in%c("alqaeda") ,"TERROR"] <- 0

# Add terms
v_c <- c("chemical","mustard","sarin")
v_n <- "CHEMICAL"
for(k in 1:length(v_c)){
t_add <- as.data.frame(matrix(0,nrow=1,ncol=length(varz)+1))
names(t_add) <- names(t0)
t_add$TERM <- as.character(t_add$TERM)
t_add$TERM <- v_c[k]
t_add[,v_n] <- 1
t0 <- rbind(t0,t_add)
}
v_c <- c("molotov")
v_n <- "PROTEST_V"
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
head(t0)
dim(t0)

# Save
term.type <- t0
term.type$TERM <- NULL
save(term.type,file="Dictionaries/EventTypes/Combined_EventDictionary.RData")
load("Dictionaries/EventTypes/Combined_EventDictionary.RData")

head(term.type)
