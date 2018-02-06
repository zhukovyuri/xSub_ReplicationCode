rm(list=ls())
#setwd("~/Dropbox (Zhukov research team)/XSub/Data")
setwd("C:/Users/nadiya/Dropbox (Zhukov research team)/XSub/Data")
library(gdata)

## Install & load packages (all at once)
list.of.packages <- c("gdata","xtable")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)


########
## SCAD: manual fix
########

# Angola
load("Dictionaries/SCAD/SCAD_Africa/SCAD_AGO_Actors.RData")
actorlist
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB!="Opposition Parties (2012 - 2012) Angola"]
actorlist$actors_GOV <- c(actorlist$actors_GOV,"Opposition Parties (2012 - 2012) Angola")
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB!="Opposition leader (2004 - 2004) Angola"]
actorlist$actors_GOV <- c(actorlist$actors_GOV,"Opposition leader (2004 - 2004) Angola")
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB!="Opposition party (2004 - 2004) Angola"]
actorlist$actors_GOV <- c(actorlist$actors_GOV,"Opposition party (2004 - 2004) Angola")
#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_AGO_Actors.RData")

# DRC
load("Dictionaries/SCAD/SCAD_Africa/SCAD_COD_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Alliance of Democratic Forces for the Liberation of Congo-Zaire (1997 - 1997)  Democratic Republic of the Congo")
actorlist$actors_GOV <- c(actorlist$actors_GOV,"Alliance of Democratic Forces for the Liberation of Congo-Zaire (1997 - 1997)  Democratic Republic of the Congo")
#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_COD_Actors.RData")

# Egypt
load("Dictionaries/SCAD/SCAD_Africa/SCAD_EGY_Actors.RData")
actorlist
actorlist$actors_GOV <- actorlist$actors_REB
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Ajnad Misr (2015 - 2015) Egypt"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Anti-government activists (2005 - 2008) Egypt"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Anti-government demonstrators (2013 - 2013) Egypt"]
#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_OTH)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_GOV)
#actorlist$actors_REB <- new_actors3

actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB!="Muslim Brotherhood opponents (2013 - 2013) Egypt"]
actorlist$actors_GOV <- c(actorlist$actors_GOV, "Muslim Brotherhood opponents (2013 - 2013) Egypt")
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Muslim Brotherhood cleric (2013 - 2013) Egypt"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Muslim Brotherhood Governors (2013 - 2013) Egypt"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Muslim Brotherhood guards (2013 - 2013) Egypt"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Muslim Brotherhood Office (2013 - 2013) Egypt"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Muslim Brotherhood party office (2013 - 2013) Egypt"]
actorlist$actors_REB <- c(actorlist$actors_REB, "Muslim Brotherhood cleric (2013 - 2013) Egypt", "Muslim Brotherhood Governors (2013 - 2013) Egypt", "Muslim Brotherhood guards (2013 - 2013) Egypt", "Muslim Brotherhood Office (2013 - 2013) Egypt", "Muslim Brotherhood party office (2013 - 2013) Egypt")
#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_EGY_Actors.RData")

# Kenya
load("Dictionaries/SCAD/SCAD_Africa/SCAD_KEN_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Democratic Party (1992 - 1992) Kenya")
actorlist$actors_GOV <- c(actorlist$actors_OTH, "Democratic Party (1992 - 1992) Kenya")
actorlist$actors <- c(actorlist$actors,"Odinga supporters (1996 - 2013) Kenya")
actorlist$actors_GOV <- c(actorlist$actors_OTH, "Odinga supporters (1996 - 2013) Kenya")
actorlist$actors <- c(actorlist$actors,"Odinga Supporters (2007 - 2007) Kenya")
actorlist$actors_GOV <- c(actorlist$actors_OTH, "Odinga Supporters (2007 - 2007) Kenya")

actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Odinga Supporters (2007 - 2007) Kenya"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Odinga supporters (1996 - 2013) Kenya"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Democratic Party (1992 - 1992) Kenya"]
#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_OTH)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_REB)
#actorlist$actors_GOV <- new_actors3
#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_KEN_Actors.RData")

# Lesotho
load("Dictionaries/SCAD/SCAD_Africa/SCAD_LSO_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"All Basotho Convention (2012 - 2012) Lesotho")
actorlist$actors_GOV <- c(actorlist$actors_OTH, "All Basotho Convention (2012 - 2012) Lesotho")

actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="All Basotho Convention (2012 - 2012) Lesotho"]
#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_OTH)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_REB)
#actorlist$actors_GOV <- new_actors3
#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_LSO_Actors.RData")


# Libya
load("Dictionaries/SCAD/SCAD_Africa/SCAD_LBY_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"army officer (2014 - 2014) Libya")
actorlist$actors_GOV <- c(actorlist$actors_OTH, "army officer (2014 - 2014) Libya")

actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="army officer (2014 - 2014) Libya"]
#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_OTH)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_REB)
#actorlist$actors_GOV <- new_actors3
#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_LBY_Actors.RData")

# # Mali
load("Dictionaries/SCAD/SCAD_Africa/SCAD_MLI_Actors.RData")
# actorlist
# #actorlist$actors <- c(actorlist$actors,"French tourists (2011 - 2011) Mali")
# actorlist$actors_OTH <- c(actorlist$actors_OTH,"French government (2006 - 2006) Mali")
# actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV !="French government (2006 - 2006) Mali"] 
# actorlist$actors <- c(actorlist$actors,"mob (2015 - 2015) Mali")
# actorlist$actors_OTH <- c(actorlist$actors_OTH,"mob (2015 - 2015) Mali")
# actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="United Front for the Protection of Democracy and the Republic (FUDR) (2012 - 2012) Mali"]
# actorlist$actors_REB <- c(actorlist$actors_REB,"United Front for the Protection of Democracy and the Republic (FUDR) (2012 - 2012) Mali")
save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_MLI_Actors.RData")
load("Dictionaries/SCAD/SCAD_Africa/SCAD_MLI_Actors.RData")

# Nigeria
load("Dictionaries/SCAD/SCAD_Africa/SCAD_NGA_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Ansaru (2013 - 2013) Nigeria")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"Ansaru (2013 - 2013) Nigeria")
actorlist$actors <- c(actorlist$actors,"Niger government (1994 - 1994) Nigeria")
actorlist$actors_OTH <- c(actorlist$actors_GOV,"Niger government (1994 - 1994) Nigeria")
actorlist$actors <- c(actorlist$actors,"Nigerian Television Authority - state-run (2012 - 2012) Nigeria")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"Nigerian Television Authority - state-run (2012 - 2012) Nigeria")
actorlist$actors <- c(actorlist$actors,"President (2000 - 2010) Nigeria")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"President (2000 - 2010) Nigeria")
actorlist$actors <- c(actorlist$actors,"prison (2014 - 2014) Nigeria")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"prison (2014 - 2014) Nigeria")
actorlist$actors <- c(actorlist$actors,"Prisoners (2001 - 2007) Nigeria")
actorlist$actors_OTH <- c(actorlist$actors_GOV,"Prisoners (2001 - 2007) Nigeria")
actorlist$actors <- c(actorlist$actors,"Vice president (2007 - 2007) Nigeria")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"Vice president (2007 - 2007) Nigeria")


actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Ansaru (2013 - 2013) Nigeria"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Nigerian Television Authority - state-run (2012 - 2012) Nigeria"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="President (2000 - 2010) Nigeria"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Vice president (2007 - 2007) Nigeria"]

actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Prisoners (2001 - 2007) Nigeria"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Niger government (1994 - 1994) Nigeria"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="prisoners (2014 - 2014) Nigeria"]

actorlist$actors_OTH <- c(actorlist$actors_OTH, "prisoners (2014 - 2014) Nigeria", "Niger government (1994 - 1994) Nigeria","Prisoners (2001 - 2007) Nigeria" )
#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_OTH)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_REB)
#actorlist$actors_GOV <- new_actors3

actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB!="Bring Back Our Girls (2015 - 2015) Nigeria"]
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB!="Bring Back Our Girls movement (2014 - 2014) Nigeria"]
actorlist$actors_OTH <- c(actorlist$actors_OTH, "Bring Back Our Girls movement (2014 - 2014) Nigeria", "Bring Back Our Girls (2015 - 2015) Nigeria")
#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_NGA_Actors.RData")


# Republic of Congo
load("Dictionaries/SCAD/SCAD_Africa/SCAD_COG_Actors.RData")
actorlist
actorlist_GOV <- actorlist$actors_GOV[actorlist_GOV !="NA"]
save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_COG_Actors.RData")

# Senegal
load("Dictionaries/SCAD/SCAD_Africa/SCAD_SEN_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Senegalese peacekeepers (2001 - 2001) Senegal")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Senegalese peacekeepers (2001 - 2001) Senegal")
#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_SEN_Actors.RData")

# Sierra Leone
load("Dictionaries/SCAD/SCAD_Africa/SCAD_SLE_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Kamajor fighters (2001 - 2001) Sierra Leone")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"Kamajor fighters (2001 - 2001) Sierra Leone")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Kamajor fighters (2001 - 2001) Sierra Leone"]
#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_OTH)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_REB)
#actorlist$actors_GOV <- new_actors3
#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_SLE_Actors.RData")

# Somalia
load("Dictionaries/SCAD/SCAD_Africa/SCAD_SOM_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"African Union (2005 - 2010) Somalia")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"African Union (2005 - 2010) Somalia")
actorlist$actors <- c(actorlist$actors,"African Union and Somali troops (2012 - 2012) Somalia")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"African Union and Somali troops (2012 - 2012) Somalia")
actorlist$actors <- c(actorlist$actors,"African Union base (2014 - 2015) Somalia")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"African Union base (2014 - 2015) Somalia")
actorlist$actors <- c(actorlist$actors,"African Union forces (2014 - 2014) Somalia")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"African Union forces (2014 - 2014) Somalia")
actorlist$actors <- c(actorlist$actors,"African Union soldiers (2015 - 2015) Somalia")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"African Union soldiers (2015 - 2015) Somalia")
actorlist$actors <- c(actorlist$actors,"African Union troops (2014 - 2015) Somalia")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"African Union troops (2014 - 2015) Somalia")
actorlist$actors <- c(actorlist$actors,"AU warplanes (2014 - 2014) Somalia")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"AU warplanes (2014 - 2014) Somalia")
actorlist$actors <- c(actorlist$actors,"soldiers and African Union forces (2014 - 2014) Somalia")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"soldiers and African Union forces (2014 - 2014) Somalia")

actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="soldiers and African Union forces (2014 - 2014) Somalia"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="AU warplanes (2014 - 2014) Somalia"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="African Union troops (2014 - 2015) Somalia"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="African Union soldiers (2015 - 2015) Somalia"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!= "African Union forces (2014 - 2014) Somalia"] 
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="African Union base (2014 - 2015) Somalia"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="African Union and Somali troops (2012 - 2012) Somalia"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="African Union (2005 - 2010) Somalia"]
#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_OTH)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_REB)
#actorlist$actors_GOV <- new_actors3
#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_SOM_Actors.RData")

# South Africa
load("Dictionaries/SCAD/SCAD_Africa/SCAD_ZAF_Actors.RData")
actorlist
#actorlist$actors <- c(actorlist$actors,"Nelson Mandela (1994 - 1994) South Africa")
actorlist$actors_GOV <- c(actorlist$actors_GOV,"City government (2002 - 2002) South Africa")
actorlist$actors_CIV <- actorlist$actors_CIV[actorlist$actors_CIV !="City government (2002 - 2002) South Africa"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Sex crimes (2011 - 2011) South Africa"]
actorlist$actors <- actorlist$actors[actorlist$actors !="Sex crimes (2011 - 2011) South Africa"]
#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_ZAF_Actors.RData")

# Sudan
load("Dictionaries/SCAD/SCAD_Africa/SCAD_SDN_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"John Garang supporters (2005 - 2005) Sudan")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"John Garang supporters (2005 - 2005) Sudan")
actorlist$actors <- c(actorlist$actors,"Physically disabled civilians (2004 - 2004) Sudan")
actorlist$actors_CIV <- c(actorlist$actors_OTH,"Physically disabled civilians (2004 - 2004) Sudan")

actorlist$actors_CIV <- "Physically disabled civilians (2004 - 2004) Sudan"
actorlist$actors_CIV <- c("Men in civilian clothes (2012 - 2012) Sudan", "civilians, villages (2014 - 2014) Sudan","civilians (2014 - 2014) Sudan","Civilians (2000 - 2011) Sudan","civilian convoy (2014 - 2014) Sudan","Citizens (2012 - 2012) Sudan")
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Men in civilian clothes (2012 - 2012) Sudan"]

actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="civilians, villages (2014 - 2014) Sudan"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="civilians (2014 - 2014) Sudan"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Civilians (2000 - 2011) Sudan"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="civilian convoy (2014 - 2014) Sudan"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Citizens (2012 - 2012) Sudan"]


#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_OTH)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_REB)
#actorlist$actors_GOV <- new_actors3

#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_SDN_Actors.RData")



# Togo
load("Dictionaries/SCAD/SCAD_Africa/SCAD_TGO_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Let's Save Togo (2014 - 2014) Togo")
actorlist$actors_REB <- c(actorlist$actors_OTH,"Let's Save Togo (2014 - 2014) Togo")
actorlist$actors <- c(actorlist$actors,"Rally for the Togolese People (1992 - 1992) Togo")
actorlist$actors_GOV <- c(actorlist$actors_REB,"Rally for the Togolese People (1992 - 1992) Togo")

actorlist$actors_REB <- c("Anti-government demonstrators (2012 - 2012) Togo","anti-Eyadema supporters (1991 - 1991) Togo","demonstrators (1991 - 1992) Togo","Opposition groups led by Let's Save Togo (2012 - 2012) Togo","protesters (1991 - 2015) Togo")
actorlist$actors_GOV <- c("central government (1990 - 2006) Togo","Eyadema (1998 - 1998) Togo","Eyadema supporters (1992 - 1997) Togo","Gen. Eyadema (1992 - 1992) Togo","Gen. Eyadema supporters (1991 - 1991) Togo","General Eyadema (1991 - 1991) Togo","Government (2009 - 2012) Togo","government (2014 - 2015) Togo","Government and opposition (2012 - 2012) Togo","Government supporters (2012 - 2012) Togo","Jean Pierre Fabre - opposition movement leader (2012 - 2012) Togo","Let's Save Togo, Opposition Coalition (2013 - 2013) Togo","Military forces (2009 - 2009) Togo","National Alliance for Change (2011 - 2014) Togo","police (1992 - 2006) Togo","presidential guard (1993 - 1993) Togo","pro-government supporters (2002 - 2005) Togo","public sector workers (1991 - 1991) Togo","Ruling party officials (2009 - 2009) Togo","Security forces (2012 - 2012) Togo","supporters of Eyadema (1991 - 1991) Togo","Supporters of Jean-Pierre Fabre (2013 - 2013) Togo","Republic Front for Change (FRAC) (2010 - 2010) Togo","Supporters of President Faure Gnassingbe (2010 - 2010) Togo","Togo Government (2013 - 2013) Togo","Togo Security Forces (2013 - 2013) Togo","Togolese army (1993 - 1993) Togo","Union for the Forces of Change (2009 - 2010) Togo","Union for the Republic (2014 - 2014) Togo")

#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_OTH)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_REB)
#actorlist$actors_GOV <- new_actors3

#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_TGO_Actors.RData")

# Tunisia
load("Dictionaries/SCAD/SCAD_Africa/SCAD_TUN_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Ben Jeddou (2014 - 2014) Tunisia")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"Ben Jeddou (2014 - 2014) Tunisia")
actorlist$actors <- c(actorlist$actors,"Ridha Charfeddine (2015 - 2015) Tunisia")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"Ridha Charfeddine (2015 - 2015) Tunisia")
actorlist$actors <- c(actorlist$actors,"Tunisian Justice System (2012 - 2012) Tunisia")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"Tunisian Justice System (2012 - 2012) Tunisia")

actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Ben Jeddou (2014 - 2014) Tunisia"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Ridha Charfeddine (2015 - 2015) Tunisia"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Tunisian Justice System (2012 - 2012) Tunisia"]
#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_OTH)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_REB)
#actorlist$actors_GOV <- new_actors3

#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_TUN_Actors.RData")

# Uganda
load("Dictionaries/SCAD/SCAD_Africa/SCAD_UGA_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Jobless Brotherhood Group (2014 - 2014) Uganda")
actorlist$actors_REB <- c(actorlist$actors_OTH,"Jobless Brotherhood Group (2014 - 2014) Uganda")

actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Jobless Brotherhood Group (2014 - 2014) Uganda"]
#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_OTH)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_GOV)
#actorlist$actors_REB <- new_actors3
#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_UGA_Actors.RData")

# Zambia
load("Dictionaries/SCAD/SCAD_Africa/SCAD_ZMB_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Journalists at state-run Times of Zambia (2012 - 2012) Zambia")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"Journalists at state-run Times of Zambia (2012 - 2012) Zambia")
actorlist$actors <- c(actorlist$actors,"United National Independence Party (1991 - 1991) Zambia")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"United National Independence Party (1991 - 1991) Zambia")

actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Journalists at state-run Times of Zambia (2012 - 2012) Zambia"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="United National Independence Party (1991 - 1991) Zambia"]

#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_OTH)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_REB)
#actorlist$actors_GOV <- new_actors3
#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_ZMB_Actors.RData")

# Zimbabwe
load("Dictionaries/SCAD/SCAD_Africa/SCAD_ZWE_Actors.RData")
actorlist
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV !="Bruce Wharton, US envoy (2013 - 2013) Zimbabwe"]
actorlist$actors_OTH <- c(actorlist$actors_OTH, "Bruce Wharton, US envoy (2013 - 2013) Zimbabwe")
#actorlist$actors <- actorlist$actors[actorlist$actors !="army job seekers (1997 - 1997) Zimbabwe ? (G/R/C/O/Y/?)"]
#actorlist$actors <- c(actorlist$actors,"youth supporters of Grace Mugabe (2014 - 2014) Zimbabwe")
#actorlist$actors_GOV <- c(actorlist$actors_REB,"youth supporters of Grace Mugabe (2014 - 2014) Zimbabwe")
#actorlist$actors_GOV <- c(actorlist$actors,"Police (2001 - 2014) Zimbabwe")
#save(actorlist,file="Dictionaries/SCAD/SCAD_Africa/SCAD_ZWE_Actors.RData")



####################
## SCAD_LatinAmerica
####################

# Cuba
load("Dictionaries/SCAD/SCAD_LatinAmerica/SCAD_CUB_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Cuban Commission for Human Rights and National Reconciliation (2005 - 2005) Cuba")
actorlist$actors_OTH <- c(actorlist$actors_GOV,"Cuban Commission for Human Rights and National Reconciliation (2005 - 2005) Cuba")

actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Cuban Commission for Human Rights and National Reconciliation (2005 - 2005) Cuba"]

#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_GOV)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_REB)
#actorlist$actors_OTH <- new_actors3
#save(actorlist,file="Dictionaries/SCAD/SCAD_LatinAmerica/SCAD_CUB_Actors.RData")

# Guatemala
load("Dictionaries/SCAD/SCAD_LatinAmerica/SCAD_GTM_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Indian civilians (2000 - 2000) Guatemala")
actorlist$actors_CIV <- c(actorlist$actors_OTH,"Indian civilians (2000 - 2000) Guatemala")
actorlist$actors <- c(actorlist$actors,"Interior Minister (2012 - 2012) Guatemala")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"Interior Minister (2012 - 2012) Guatemala")

#actorlist$actors_GOV <- c(actorlist$actors_GOV,"Interior Minister (2012 - 2012) Guatemala")
#actorlist$actors_CIV <- c("Indian civilians (2000 - 2000) Guatemala", "Civilians (1992 - 2012) Guatemala")
#new_actors <- setdiff(actorlist$actors, actorlist$actors_OTH)
#new_actors2 <- setdiff(new_actors, actorlist$actors_REB)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_CIV)
#actorlist$actors_GOV <- new_actors3

#save(actorlist,file="Dictionaries/SCAD/SCAD_LatinAmerica/SCAD_GTM_Actors.RData")

# Haiti
load("Dictionaries/SCAD/SCAD_LatinAmerica/SCAD_HTI_Actors.RData")
actorlist
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Army officer (1994 - 1994) Haiti"]
actorlist$actors_GOV <- c(actorlist$actors_GOV, "Army officer (1994 - 1994) Haiti")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Front for the Advancement and Progress of  Haiti members (1994 - 1994) Haiti"]
actorlist$actors_REB <- c(actorlist$actors_REB, "Front for the Advancement and Progress of  Haiti members (1994 - 1994) Haiti")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Front for the Advancement and Progress of Haiti (1993 - 1995) Haiti"]
actorlist$actors_REB <- c(actorlist$actors_REB, "Front for the Advancement and Progress of Haiti (1993 - 1995) Haiti")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Front for the Advancement and Progress of Haiti headquarters (1994 - 1994) Haiti"]
actorlist$actors_REB <- c(actorlist$actors_REB, "Front for the Advancement and Progress of Haiti headquarters (1994 - 1994) Haiti")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Villagers suspected of supporting rebels (1994 - 1994) Haiti"]
actorlist$actors_REB <- c(actorlist$actors_REB, "Villagers suspected of supporting rebels (1994 - 1994) Haiti")
#save(actorlist,file="Dictionaries/SCAD/SCAD_LatinAmerica/SCAD_HTI_Actors.RData")

# Jamaica
load("Dictionaries/SCAD/SCAD_LatinAmerica/SCAD_JAM_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Hannah Town gang (area loyal to People's National Party) (2001 - 2001) Jamaica")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"Hannah Town gang (area loyal to People's National Party) (2001 - 2001) Jamaica")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Hannah Town gang (area loyal to People's National Party) (2001 - 2001) Jamaica"]
#new_actors <- setdiff(actorlist$actors, actorlist$actors_OTH)
#new_actors2 <- setdiff(new_actors, actorlist$actors_REB)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_CIV)
#actorlist$actors_GOV <- new_actors3
#save(actorlist,file="Dictionaries/SCAD/SCAD_LatinAmerica/SCAD_JAM_Actors.RData")

# Mexico
load("Dictionaries/SCAD/SCAD_LatinAmerica/SCAD_MEX_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Female activist (2010 - 2010) Mexico")
actorlist$actors_OTH <- c(actorlist$actors_GOV,"Female activist (2010 - 2010) Mexico")
actorlist$actors <- c(actorlist$actors,"Tourism secretary (2009 - 2009) Mexico")
actorlist$actors_GOV <- c(actorlist$actors_OTH,"Tourism secretary (2009 - 2009) Mexico")
actorlist$actors <- c(actorlist$actors,"Arellano Felix cartel / Tijunana cartel gunmen (1998 - 1998) Mexico")
actorlist$actors_REB <- c(actorlist$actors_OTH,"Arellano Felix cartel / Tijunana cartel gunmen (1998 - 1998) Mexico")

actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Female activist (2010 - 2010) Mexico"]
actorlist$actors_GOV <- c(actorlist$actors_GOV, "Andres Manuel Lopez Obrador (2008 - 2013) Mexico","Andres Manuel Lopez Obrador supporters (2007 - 2007) Mexico",)
#new_actors <- setdiff(actorlist$actors, actorlist$actors_GOV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_CIV)
actorlist$actors_REB <- c("Alleged cartel members (2011 - 2011) Mexico","Alleged drug traffickers (2009 - 2011) Mexico","Alleged leftist rebels (1999 - 1999) Mexico","Anti government protesters (2003 - 2003) Mexico","Anti government rebels (1993 - 1993) Mexico","Antigovernment protesters (2003 - 2003) Mexico","Arellano Felix cartel / Tijunana cartel gunmen (1998 - 1998) Mexico","Arellano Felix cartel / Tijunana cartel gunmen (1998 - 1998) Mexico","Armed citizen militias (2013 - 2013) Mexico","Arturo Beltran Leyva (2009 - 2009) Mexico","Arturo Guzman (2004 - 2004) Mexico","Beltran Leyva drug cartel (2009 - 2010) Mexico","Beltran Leyva faction (2010 - 2010) Mexico","Beltran Leyva gunmen (2012 - 2012) Mexico","Carrillo Guentes organization (2008 - 2008) Mexico","Cartel gunmen (2009 - 2010) Mexico","Cartel hitmen (2009 - 2009) Mexico","Cartel members (2010 - 2010) Mexico","Cartel organized demonstrators (2009 - 2009) Mexico","Comando Jaramillista Morelense 23 de Mayo (2004 - 2004) Mexico","EZLN (1994 - 2006) Mexico","EZLN rebels (1995 - 1995) Mexico","EZLN supporters (1999 - 1999) Mexico","Guadalajara drug cartel (1998 - 1998) Mexico","Gulf Cartel (2005 - 2008) Mexico","Gulf cartel (2007 - 2007) Mexico","Gulf cartel leader Antonio Ezequiel Cardenas Guillen (2010 - 2010) Mexico","Ismael Zambada / Sinaloa cartel (2002 - 2002) Mexico","Juarez Cartel (2008 - 2009) Mexico","Juarez Cartel gunmen (2010 - 2010) Mexico","Knights Templar drug cartel (2013 - 2013) Mexico","La Familia cartel (2009 - 2009) Mexico","La Familia drug cartel (2010 - 2013) Mexico","Michoacan drug cartel (2006 - 2006) Mexico","Milenio drug smuggling cartel (2005 - 2005) Mexico","New Generation (2011 - 2011) Mexico","New Generation Cartel (2013 - 2013) Mexico","People's Revolutionary Armed Forces (2001 - 2001) Mexico","Presumed Zetas (2012 - 2012) Mexico","Ramon Arellano Felix (2002 - 2002) Mexico","Rebel supporters (1998 - 1999) Mexico","Rebel sympathizers (1998 - 1998) Mexico","Rebels (1994 - 1994) Mexico","Revolutionary Armed Forces of the People (2001 - 2001) Mexico","Revolutionary Clandestine Workers' Party-People's Union","Revolutionary Confederation of Workers and Peasants (2003 - 2003) Mexico","Rival Zetas (2011 - 2011) Mexico","Sinaloa cartel (1992 - 2012) Mexico","Sinaloa Cartel (2007 - 2009) Mexico","Sinaloa cartel gunmen (2012 - 2012) Mexico","Sinaloa cartel hitmen (2009 - 2009) Mexico","Suspected Beltran-Leyva cartel gunmen (2009 - 2009) Mexico","Suspected cartel gunmen (2010 - 2011) Mexico","Suspected cartel members (2011 - 2011) Mexico","Suspected Zetas (2009 - 2009) Mexico","Tijuana cartel (1992 - 1993) Mexico","Tijuana cartel / Arellano Felix organization (2002 - 2002) Mexico","Vigilante militia (2013 - 2013) Mexico","Zapatista rebels (2000 - 2004) Mexico","Zapatista supporters (1997 - 2012) Mexico","Zapatista symathizers (2004 - 2004) Mexico","Zapatista sympathizers (2001 - 2001) Mexico","Zapatistas (1997 - 2006) Mexico","Zeta cartel (2011 - 2011) Mexico","Zetas (2008 - 2012) Mexico")
#new_actors3 <- setdiff(new_actors2, actorlist$actors_REB)
#actorlist$actors_OTH <- new_actors3
#save(actorlist,file="Dictionaries/SCAD/SCAD_LatinAmerica/SCAD_MEX_Actors.RData")


########
## ACLED
########

# Angola
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_AGO_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors_CIV,"Civilians (Democratic Republic of Congo) (Inf - -Inf) Angola")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (Democratic Republic of Congo) (Inf - -Inf) Angola"]
actorlist$actors <- c(actorlist$actors_CIV,"Civilians (International) (Inf - -Inf) Angola")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (International) (Inf - -Inf) Angola"]
actorlist$actors <- c(actorlist$actors_CIV,"Civilians (South Africa) (Inf - -Inf) Angola")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (South Africa) (Inf - -Inf) Angola"]
actorlist$actors <- c(actorlist$actors_CIV,"Civilians (Togo) (Inf - -Inf) Angola")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (Togo) (Inf - -Inf) Angola"]
actorlist$actors <- c(actorlist$actors_CIV,"Civilians (Zambia) (Inf - -Inf) Angola")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (Zambia) (Inf - -Inf) Angola"]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_AGO_Actors.RData")

# Algeria
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_DZA_Actors.RData")
actorlist
#actorlist$actors <- c(actorlist$actors,"Zintan Ethnic Militia (Libya) (Inf - -Inf) Algeria")
#actorlist$actors_OTH <- c(actorlist$actors_REB,"Zintan Ethnic Militia (Libya) (Inf - -Inf) Algeria")
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="Zintan Ethnic Militia (Libya) (Inf - -Inf) Algeria"]
#actorlist$actors_REB <- 
actorlist$actors_OTH <- setdiff(actorlist$actors_OTH, actorlist$actors_REB)
##OTH
actorlist$actors <- c(actorlist$actors,"Islamist Militia (Algeria)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Islamist Militia (Algeria)")
actorlist$actors <- c(actorlist$actors,"Unidentified Armed Group (Algeria)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Unidentified Armed Group (Algeria)")
actorlist$actors <- c(actorlist$actors,"Military Forces of Morocco (1999-)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Military Forces of Morocco (1999-)")
actorlist$actors <- c(actorlist$actors,"Protesters (Tunisia)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Protesters (Tunisia)")
actorlist$actors <- c(actorlist$actors,"Rioters (Algeria)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Rioters (Algeria)")
actorlist$actors <- c(actorlist$actors,"Military Forces of Germany")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Military Forces of Germany") 
actorlist$actors <- c(actorlist$actors,"Military Forces of Mali (2002-2012)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Military Forces of Mali (2002-2012)") 
actorlist$actors <- c(actorlist$actors,"Unidentified Armed Group (Tunisia)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Unidentified Armed Group (Tunisia)")
actorlist$actors <- c(actorlist$actors,"Police Forces of Morocco (1999-)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Police Forces of Morocco (1999-)")
actorlist$actors <- c(actorlist$actors,"Unidentified Armed Group (Morocco)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Unidentified Armed Group (Morocco)")
actorlist$actors <- c(actorlist$actors,"Unidentified Armed Group (Libya)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Unidentified Armed Group (Libya)")
actorlist$actors <- c(actorlist$actors,"Protesters (International)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Protesters (International)")
actorlist$actors <- c(actorlist$actors,"Government of Mali (2002-2012)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Government of Mali (2002-2012)")
actorlist$actors <- c(actorlist$actors,"Unidentified Ethnic Militia (Algeria)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Unidentified Ethnic Militia (Algeria)")
actorlist$actors <- c(actorlist$actors,"Protesters (Egypt)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Protesters (Egypt)")
actorlist$actors <- c(actorlist$actors,"Unidentified Armed Group (Mali)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Unidentified Armed Group (Mali)")
actorlist$actors <- c(actorlist$actors,"Protesters (Morocco)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Protesters (Morocco)")
actorlist$actors <- c(actorlist$actors,"Saharawi Communal Militia (Morocco)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Saharawi Communal Militia (Morocco)")
actorlist$actors <- c(actorlist$actors,"FIS: Islamic Salvation Front")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"FIS: Islamic Salvation Front")
actorlist$actors <- c(actorlist$actors,"Patriots Militia")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Patriots Militia")
actorlist$actors <- c(actorlist$actors,"Military Forces of Mauritania (1984-2005)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Military Forces of Mauritania (1984-2005)")
actorlist$actors <- c(actorlist$actors,"Al Qaeda")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Al Qaeda")
actorlist$actors <- c(actorlist$actors,"Salafia Jihadia")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Salafia Jihadia")
actorlist$actors <- c(actorlist$actors,"Saharawi Communal Militia (Morocco)")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Saharawi Communal Militia (Morocco)")
 

##CIV
actorlist$actors <- c(actorlist$actors,"Civilians (Morocco)")
actorlist$actors_CIV <- c(actorlist$actors_CIV,"Civilians (Morocco)")
actorlist$actors <- c(actorlist$actors,"Civilians (International)")
actorlist$actors_CIV <- c(actorlist$actors_CIV,"Civilians (International)")
actorlist$actors <- c(actorlist$actors,"Civilians (Mali)")
actorlist$actors_CIV <- c(actorlist$actors_CIV,"Civilians (Mali)")
actorlist$actors <- c(actorlist$actors,"Civilians (Libya)")
actorlist$actors_CIV <- c(actorlist$actors_CIV,"Civilians (Libya)")


#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_DZA_Actors.RData")

# Burundi
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_BDI_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors, "CNDD-FDD-Bakenyarerugamba: National Council for the Defence of Democracy (Bakenyarerugamba Faction) (Inf - -Inf) Burundi")
actorlist$actors_REB <- c(actorlist$actors_GOV, "CNDD-FDD-Bakenyarerugamba: National Council for the Defence of Democracy (Bakenyarerugamba Faction) (Inf - -Inf) Burundi")
actorlist$actors <- c(actorlist$actors,"CNDD-FDD-Imbonerakure: National Council for the Defence of Democracy (Imbonerakure Faction) (Inf - -Inf) Burundi")
actorlist$actors_REB <- c(actorlist$actors_GOV,"CNDD-FDD-Imbonerakure: National Council for the Defence of Democracy (Imbonerakure Faction) (Inf - -Inf) Burundi")
actorlist$actors <- c(actorlist$actors,"CNDD-FDD: National Council for the Defence of Democracy-Forces for the Defence of Democracy (Inf - -Inf) Burundi")
actorlist$actors_REB <- c(actorlist$actors_GOV, "CNDD-FDD: National Council for the Defence of Democracy-Forces for the Defence of Democracy (Inf - -Inf) Burundi")
actorlist$actors <- c(actorlist$actors,"CNDD: National Council for Democracy and Development (Inf - -Inf) Burundi")
actorlist$actors_REB <- c(actorlist$actors_GOV, "CNDD: National Council for Democracy and Development (Inf - -Inf) Burundi")
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV !="CNDD-FDD-Bakenyarerugamba: National Council for the Defence of Democracy (Bakenyarerugamba Faction) (Inf - -Inf) Burundi"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV !="CNDD-FDD-Imbonerakure: National Council for the Defence of Democracy (Imbonerakure Faction) (Inf - -Inf) Burundi"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV !="CNDD-FDD: National Council for the Defence of Democracy-Forces for the Defence of Democracy (Inf - -Inf) Burundi"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV !="CNDD: National Council for Democracy and Development (Inf - -Inf) Burundi"]
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="Government of Burundi (2005-) (Inf - -Inf) Burundi"]
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="Military Forces of Burundi (1996-2005) (Inf - -Inf) Burundi"]

actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="Military Forces of Burundi (2005-) (Inf - -Inf) Burundi"]
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="MSD: Movement for Solidarity and Democracy (Inf - -Inf) Burundi"]
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="Police Forces of Burundi (1996-2005) (Inf - -Inf) Burundi"]
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="Police Forces of Burundi (2005-)  (Inf - -Inf) Burundi"]
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="Police Forces of Burundi (2005-) (Inf - -Inf) Burundi"]
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="Police Forces of Burundi (2005-) Counter Terrorism Unit (Inf - -Inf) Burundi"]
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="Police Forces of Burundi (2005-) National Intelligence Service (Inf - -Inf) Burundi"]
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="Police Forces of Burundi (2005-) Rapid Mobile Intervention Group (Inf - -Inf) Burundi"]
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="UPD-Z: Union for Peace and Development (Zigamibanga Faction) (Inf - -Inf) Burundi"]
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="UPRONA: Union for National Progress (Inf - -Inf) Burundi"]

actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="Amizero yAbarundi (Inf - -Inf) Burundi"]
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="Military Forces of Burundi (2005-) National Intelligence Agency (Inf - -Inf) Burundi"]
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB !="Police Forces of Burundi (2005-) API Unit (Inf - -Inf) Burundi"]

actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (Democratic Republic of Congo) (Inf - -Inf) Burundi"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (International) (Inf - -Inf) Burundi"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (Rwanda) (Inf - -Inf) Burundi"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (Tanzania) (Inf - -Inf) Burundi"]

actorlist$actors_CIV <- c(actorlist$actors_CIV,"Civilians (Democratic Republic of Congo) (Inf - -Inf) Burundi")
actorlist$actors_CIV <- c(actorlist$actors_CIV,"Civilians (International) (Inf - -Inf) Burundi")
actorlist$actors_CIV <- c(actorlist$actors_CIV,"Civilians (Rwanda) (Inf - -Inf) Burundi")
actorlist$actors_CIV <- c(actorlist$actors_CIV,"Civilians (Tanzania) (Inf - -Inf) Burundi")
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_BDI_Actors.RData")

##Benin
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_BEN_Actors.RData")
actorlist
actorlist$actors <- actorlist$actors[actorlist$actors !=" (Inf - -Inf) Benin"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !=" (Inf - -Inf) Benin"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (International) (Inf - -Inf) Benin"]
actorlist$actors_CIV <- c(actorlist$actors_CIV,"Civilians (International) (Inf - -Inf) Benin")
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_BEN_Actors.RData")

##Burkina Faso
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_BFA_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV,"Civilians (International) (Inf - -Inf) Burkina Faso")
actorlist$actors_CIV <- c(actorlist$actors_CIV,"Civilians (Ivory Coast) (Inf - -Inf) Burkina Faso")
actorlist$actors_CIV <- c(actorlist$actors_CIV,"Civilians (Mali) (Inf - -Inf) Burkina Faso")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (International) (Inf - -Inf) Burkina Faso"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (Ivory Coast) (Inf - -Inf) Burkina Faso"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (Mali) (Inf - -Inf) Burkina Faso"]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_BFA_Actors.RData")

##Botswana
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_BWA_Actors.RData")
actorlist
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Police Forces of Zimbabwe (1987-) (Inf - -Inf) Botswana")
actorlist$actors_CIV <- c(actorlist$actors_CIV,"Civilians (Namibia) (Inf - -Inf) Botswana")
actorlist$actors_CIV <- c(actorlist$actors_CIV,"Civilians (Zimbabwe) (Inf - -Inf) Botswana")
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV !="Police Forces of Zimbabwe (1987-) (Inf - -Inf) Botswana"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (Namibia) (Inf - -Inf) Botswana"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Civilians (Zimbabwe) (Inf - -Inf) Botswana"]
actorlist$actors_REB <- c(actorlist$actors_REB,"Protesters (Botswana) (Inf - -Inf) Botswana")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Protesters (Botswana) (Inf - -Inf) Botswana"]
actorlist$actors_REB <- c(actorlist$actors_REB,"Rioters (Botswana) (Inf - -Inf) Botswana")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Rioters (Botswana) (Inf - -Inf) Botswana"]

#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_BwA_Actors.RData")


# Central African Republic
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_CAF_Actors.RData")
actorlist
#actorlist$actors <- c(actorlist$actors,"Anti-Balaka (Inf - -Inf) Central African Republic")
#actorlist$actors_GOV <- c(actorlist$actors_REB,"Anti-Balaka (Inf - -Inf) Central African Republic")
dif1 <- setdiff(actorlist$actors, actorlist$actors_REB)
dif2 <- setdiff(dif1, actorlist$actors_OTH)
dif3 <- setdiff(dif2, actorlist$actors_CIV)
actorlist$actors_GOV <- dif3
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_CAF_Actors.RData")

# Cameroon
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_CMR_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_CMR_Actors.RData")

# Republic of Congo
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_COG_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_COG_Actors.RData")

# Djibouti
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_DJI_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_DJI_Actors.RData")

# Egypt
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_EGY_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_EGY_Actors.RData")

# Eritrea
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_ERI_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_ERI_Actors.RData")

# Ethiopia
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_ETH_Actors.RData")
actorlist
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_ETH_Actors.RData")

# Gabon
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_GAB_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_GAB_Actors.RData")

# Ghana
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_GHA_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_GHA_Actors.RData")

# Guinea
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_GIN_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_GIN_Actors.RData")


# Gambia
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_GMB_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_GMB_Actors.RData")

# Guinea-Bissau
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_GNB_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_GNB_Actors.RData")


# Equatorial Guinea
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_GNQ_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_GNQ_Actors.RData")

# India
load("Dictionaries/ACLED/NK/ACLED_Asia/ACLED_IND_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"PREPAK-Pro: People's Revolutionary Party of Kanleipak (Progressive) (Inf - -Inf) India")
actorlist$actors_REB <- c(actorlist$actors_REB,"PREPAK-Pro: People's Revolutionary Party of Kanleipak (Progressive) (Inf - -Inf) India")
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_IND_Actors.RData")

# Ivory Coast
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_CIV_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Mutiny of Military Forces of Burkina Faso (2015-) (Inf - -Inf) Ivory Coast")
actorlist$actors_OTH <- c(actorlist$actors_REB,"Mutiny of Military Forces of Burkina Faso (2015-) (Inf - -Inf) Ivory Coast")
actorlist$actors <- c(actorlist$actors,"Protesters (Burkina Faso) (Inf - -Inf) Ivory Coast")
actorlist$actors_OTH <- c(actorlist$actors_REB,"Protesters (Burkina Faso) (Inf - -Inf) Ivory Coast")
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_CIV_Actors.RData")

# Kenya
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_KEN_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
actorlist$actors_GOV <- c(actorlist$actors_GOV, "Government of Kenya (2002-2013) (Inf - -Inf) Kenya")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Government of Kenya (2002-2013) (Inf - -Inf) Kenya"]
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Government of Kenya (2002-2013)"]
#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_REB)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_OTH)
#actorlist$actors_GOV <- new_actors3
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_KEN_Actors.RData")

# Liberia
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_LBR_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_LBR_Actors.RData")

# Lesotho
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_LSO_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_LSO_Actors.RData")

# Morocco
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_MAR_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_MAR_Actors.RData")

# Mali
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_MLI_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_MLI_Actors.RData")


# Mozambique
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_MOZ_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_MOZ_Actors.RData")


# Malawi
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_MWI_Actors.RData")
actorlist
setdiff(actorlist$actors_GOV, actorlist$actors_CIV)
setdiff(actorlist$actors_GOV, actorlist$actors_REB)
actorlist$actors_GOV <- setdiff(actorlist$actors_GOV, actorlist$actors_OTH)
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Civilians (Malawi) (Inf - -Inf) Malawi"]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_MWI_Actors.RData")

# Mauritania
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_MRT_Actors.RData")
actorlist
#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_REB)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_GOV)
#actorlist$actors_OTH <- new_actors3
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_MRT_Actors.RData")

# Namibia
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_NAM_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_NAM_Actors.RData")

# Nigeria
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_NGA_Actors.RData")
actorlist
#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_REB)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_OTH)
#actorlist$actors_GOV <- new_actors3
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
actorlist$actors_GOV <- c(actorlist$actors_GOV, "Government of Nigeria (1999-2015) (Inf - -Inf) Nigeria")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Government of Nigeria (1999-2015) (Inf - -Inf) Nigeria"]
#actorlist$actors <- c(actorlist$actors,"Government of Nigeria (1999-2015) (Inf - -Inf) Nigeria")
#actorlist$actors_GOV <- c(actorlist$actors_OTH,"Government of Nigeria (1999-2015) (Inf - -Inf) Nigeria")
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_NGA_Actors.RData")

# Niger
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_NER_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_NER_Actors.RData")

# Rwanda
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_RWA_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_RWA_Actors.RData")

# Somalia
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_SOM_Actors.RData")
actorlist
actorlist$actors_REB <- c(actorlist$actors_REB, "HI: Hizbul Islam (Inf - -Inf) Somalia")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="HI: Hizbul Islam (Inf - -Inf) Somalia"]
actorlist$actors_GOV <- c(actorlist$actors_GOV, "Military Forces of United States (Inf - -Inf) Somalia")
actorlist$actors <- c(actorlist$actors, "Military Forces of United States (Inf - -Inf) Somalia")
actorlist$actors_GOV <- c(actorlist$actors_GOV, "Militia (Anti-Al Shabaab) (Inf - -Inf) Somalia")
actorlist$actors_REB <- actorlist$actors_REB[actorlist$actors_REB!="Militia (Anti-Al Shabaab) (Inf - -Inf) Somalia"]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_SOM_Actors.RData")

# Sudan
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_SDN_Actors.RData")
actorlist
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Government of South Sudan (2011-) (Inf - -Inf) Sudan"]
actorlist$actors_OTH <- c(actorlist$actors_OTH, "Government of South Sudan (2011-) (Inf - -Inf) Sudan")
actorlist$actors_GOV <- actorlist$actors_GOV[actorlist$actors_GOV!="Military Forces of Southern Sudan (2005-2011) Joint Integrated Units (Inf - -Inf) Sudan"]
actorlist$actors_OTH <- c(actorlist$actors_OTH, "Military Forces of Southern Sudan (2005-2011) Joint Integrated Units (Inf - -Inf) Sudan")

#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_SDN_Actors.RData")


# Senegal
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_SEN_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_SEN_Actors.RData")

# Sierra Leone
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_SLE_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_SLE_Actors.RData")


# South Sudan
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_SSD_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_SSD_Actors.RData")

# Togo
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_TGO_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
actorlist$actors_REB <- c(actorlist$actors_REB, "Rioters (Togo) (Inf - -Inf) Togo")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Rioters (Togo) (Inf - -Inf) Togo"]
actorlist$actors_REB <- c(actorlist$actors_REB, "Protesters (Togo) (Inf - -Inf) Togo")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Protesters (Togo) (Inf - -Inf) Togo"]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_TGO_Actors.RData")

# Tunisia
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_TUN_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_TUN_Actors.RData")

# Tanzania
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_TZA_Actors.RData")
actorlist
#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_OTH)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_GOV)
#actorlist$actors_REB <- new_actors3
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
actorlist$actors <- c(actorlist$actors,"Sungu Sungu (Inf - -Inf) Tanzania")
actorlist$actors_REB <- c(actorlist$actors_REB,"Sungu Sungu (Inf - -Inf) Tanzania")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Sungu Sungu (Inf - -Inf) Tanzania"]
actorlist$actors_REB <- c(actorlist$actors_REB,"Protesters (Tanzania) (Inf - -Inf) Tanzania")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Protesters (Tanzania) (Inf - -Inf) Tanzania"]
actorlist$actors_REB <- c(actorlist$actors_REB,"Rioters (Tanzania) (Inf - -Inf) Tanzania")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Rioters (Tanzania) (Inf - -Inf) Tanzania"]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_TZA_Actors.RData")

# Uganda
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_UGA_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
actorlist$actors_REB <- c(actorlist$actors_REB,"Protesters (Uganda) (Inf - -Inf) Uganda")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Protesters (Uganda) (Inf - -Inf) Uganda"]
actorlist$actors_REB <- c(actorlist$actors_REB,"Rioters (Uganda) (Inf - -Inf) Uganda")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Rioters (Uganda) (Inf - -Inf) Uganda"]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_UGA_Actors.RData")

# South Africa
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_ZAF_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
actorlist$actors_REB <- c(actorlist$actors_REB,"Protesters (South Africa) (Inf - -Inf) South Africa")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Protesters (South Africa) (Inf - -Inf) South Africa"]
actorlist$actors_REB <- c(actorlist$actors_REB,"Rioters (South Africa) (Inf - -Inf) South Africa")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Rioters (South Africa) (Inf - -Inf) South Africa"]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_ZAF_Actors.RData")


# Zambia
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_ZMB_Actors.RData")
actorlist
#new_actors <- setdiff(actorlist$actors, actorlist$actors_CIV)
#new_actors2 <- setdiff(new_actors, actorlist$actors_REB)
#new_actors3 <- setdiff(new_actors2, actorlist$actors_GOV)
#actorlist$actors_OTH <- new_actors3
actorlist$actors_OTH <- c(actorlist$actors_OTH,"ZANU-PF: Zimbabwe African National Union-Patriotic Front (Inf - -Inf) Zambia")
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
actorlist$actors_REB <- c(actorlist$actors_REB,"Protesters (Zambia) (Inf - -Inf) Zambia")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Protesters (Zambia) (Inf - -Inf) Zambia"]
actorlist$actors_REB <- c(actorlist$actors_REB,"Rioters (Zambia) (Inf - -Inf) Zambia")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Rioters (Zambia) (Inf - -Inf) Zambia"]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_ZMB_Actors.RData")

# Zimbabwe
load("Dictionaries/ACLED/NK/ACLED_Africa/ACLED_ZWE_Actors.RData")
actorlist
actorlist$actors_CIV <- c(actorlist$actors_CIV, actorlist$actors_OTH[grepl("Civilians",actorlist$actors_OTH)])
actorlist$actors_OTH <- actorlist$actors_OTH[!grepl("Civilians",actorlist$actors_OTH)]
actorlist$actors_REB <- c(actorlist$actors_REB,"Protesters (Zimbabwe) (Inf - -Inf) Zimbabwe")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Protesters (Zimbabwe) (Inf - -Inf) Zimbabwe"]
actorlist$actors_REB <- c(actorlist$actors_REB,"Rioters (Zimbabwe) (Inf - -Inf) Zimbabwe")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Rioters (Zimbabwe) (Inf - -Inf) Zimbabwe"]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Africa/ACLED_ZWE_Actors.RData")


#######
## ACLED Asia
#######

# Bangladesh
load("Dictionaries/ACLED/NK/ACLED_Asia/ACLED_BGD_Actors.RData")
actorlist
actorlist$actors_REB <- c(actorlist$actors_REB,"Protesters (Bangladesh) (Inf - -Inf) Bangladesh")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH!="Protesters (Bangladesh) (Inf - -Inf) Bangladesh"]
#save(actorlist,file="Dictionaries/ACLED/NK/ACLED_Asia/ACLED_BGD_Actors.RData")



##########
#Davenport 
##########
#Northern Ireland
load("Dictionaries/Davenport/Davenport_NorthernIreland_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Protest Church")
actorlist$actors_OTH <- c(actorlist$actors_GOV,"Protest Church")
#save(actorlist,file="Dictionaries/Davenport/Davenport_NorthernIreland_Actors.RData")

########
## PITF:1995-2012
########
# Palestinian Territory
load("Dictionaries/PITF/PITF_1995_2012/PITF_PSE_Actors.RData")
actorlist
actorlist$actors <- c(actorlist$actors,"Multiple Perpetrators (State and Non-State) (Inf - -Inf) PSE")
actorlist$actors_OTH <- c(actorlist$actors_OTH,"Multiple Perpetrators (State and Non-State) (Inf - -Inf) PSE")
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="Non-State, Internal, No State Sanction (Inf - -Inf) PSE"]
actorlist$actors_OTH <- actorlist$actors_OTH[actorlist$actors_OTH !="State Perpetrator (Inf - -Inf) PSE"]
#save(actorlist, file="Dictionaries/PITF/PITF_1995_2012/PITF_PSE_Actors.RData")

# Central African Republic
load("Dictionaries/PITF/PITF_1995_2012/PITF_CAF_Actors.RData")
actorlist
actorlist$actors_GOV <- c(actorlist$actors_GOV,"State Perpetrator (Inf - -Inf) CAF")
#save(actorlist, file="Dictionaries/PITF/PITF_1995_2012/PITF_CAF_Actors.RData")

########
## GED: NK/YZ disagreement
########

filez.nk <- dir("Dictionaries/GED")
filez.nk <- filez.nk[grep(".RData",filez.nk)]

filez.yz <- dir("Dictionaries/GED/YZ")
filez.yz <- filez.yz[grep(".RData",filez.yz)]

# Variable names
varz <- c("actors","actors_GOV","actors_REB","actors_CIV","actors_OTH")

i <- 74
k <- 1

# Begin i loop
file.list <- lapply(1:length(filez.nk),function(i){

# Load NK files
dict.nk <- load(paste0("Dictionaries/GED/",filez.nk[i]))
dict.nk <- get(dict.nk); rm(actorlist)

# Load YZ files

if(!filez.nk[i]%in%filez.yz){
varlist <- data.frame(COUNTRY=cnt,CATEGORY="",nk_not_yz="",yz_not_nk="")
}

if(filez.nk[i]%in%filez.yz){
dict.yz <- load(paste0("Dictionaries/GED/YZ/",filez.nk[i]))
dict.yz <- get(dict.yz); rm(actorlist)

# Begin k loop
varlist <- lapply(1:length(varz),function(k){
if(length(dict.nk[varz[k]][[1]])>0){dict.nk[varz[k]][[1]] <- trim(sapply(strsplit(dict.nk[varz[k]][[1]],split="\\(\\d{4}"), '[', 1))}
if(length(dict.yz[varz[k]][[1]])>0){dict.yz[varz[k]][[1]] <- trim(sapply(strsplit(dict.yz[varz[k]][[1]],split="\\(\\d{4}"), '[', 1))}

# Country name
cnt <- gsub("GED_|_Actors.RData","",filez.nk[i])
# Variable name
varnam <- varz[k]
# Included by NK, not YZ
nk_not_yz <- setdiff(dict.nk[[varz[k]]],dict.yz[[varz[k]]])
# Included by YZ, not NK
yz_not_nk <- setdiff(dict.yz[[varz[k]]],dict.nk[[varz[k]]])
# Max length
max_length <- max(length(nk_not_yz),length(yz_not_nk))

# Save table
if(max_length>0){
var.df <- data.frame(COUNTRY=cnt,CATEGORY=varnam,nk_not_yz=c(nk_not_yz,rep("",max_length-length(nk_not_yz))),yz_not_nk=c(yz_not_nk,rep("",max_length-length(yz_not_nk))))
}
if(max_length==0){
var.df <- data.frame(COUNTRY=cnt,CATEGORY=varnam,nk_not_yz="",yz_not_nk="")
}
var.df
# Close k loop
})
# Close if statement
}
do.call(rbind,varlist)
# Close i loop
})

file.df <- do.call(rbind,file.list)

print.xtable(xtable(file.df,include.rownames=FALSE),file="Dictionaries/DictionaryCheck/GED/GED_nk_yz_raw.tex",include.rownames=FALSE)
head(file.df)




########
## PITFs
########

rm(list=ls())

filez.nk <- dir("Dictionaries/PITF/PITF_1995_2012")
filez.nk <- filez.nk[grep(".RData",filez.nk)]

# Variable names
varz <- c("actors","actors_GOV","actors_REB","actors_CIV","actors_OTH")

i <- 74
k <- 1

# Begin i loop
file.list <- lapply(1:length(filez.nk),function(i){

# Load NK files
dict.nk <- load(paste0("Dictionaries/PITF/PITF_1995_2012/",filez.nk[i]))
dict.nk <- get(dict.nk); rm(actorlist)

# Begin k loop
varlist <- lapply(1:length(varz),function(k){
if(length(dict.nk[varz[k]][[1]])>0){dict.nk[varz[k]][[1]] <- trim(sapply(strsplit(dict.nk[varz[k]][[1]],split="\\(\\d{4}|\\(Inf"), '[', 1))}

# Country name
cnt <- gsub("PITF_|_Actors.RData","",filez.nk[i])
# Variable name
varnam <- varz[k]
# Included by NK
nk_in <- dict.nk[[varz[k]]]
# Max length
max_length <- max(length(nk_in))

# Save table
if(max_length>0){
var.df <- data.frame(COUNTRY=cnt,CATEGORY=varnam,nk_in=c(nk_in,rep("",max_length)))
}
if(max_length==0){
var.df <- data.frame(COUNTRY=cnt,CATEGORY=varnam,nk_in="")
}
var.df
# Close k loop
})

do.call(rbind,varlist)
# Close i loop
})

file.df <- do.call(rbind,file.list)

print.xtable(xtable(file.df,include.rownames=FALSE),file="Dictionaries/DictionaryCheck/PITF/PITF_1995_2012_nk_raw.tex",include.rownames=FALSE)


########################
## Building Latex tables
## for Actor Dictionaries
######################## 

##deleting extra portion from the actorlist
#actorlist_2 = lapply(actorlist, function(x) gsub(" \\(Inf - -Inf\\) Angola", "", x))
actorlist_2 = lapply(actorlist, function(x) gsub(" Zambia", "", x))
actorlist_5 <- actorlist_2[-1]
#actorlist_5 <- actorlist
actorlist_5

listToDataFrame <- function(li) {
  lengths <- sapply(li, FUN = length)
  max.length <- max(lengths)
  list.2 <- list() 
  for (i in 1:length(li)) {
    list.2[[i]] <- c(li[[i]], rep("", max.length - length(li[[i]])))
  }
  names(list.2) <- names(li)
  return(as.data.frame(list.2))
}
dat <- listToDataFrame(actorlist_5)

#xtable(dat)

library(stringr)
str_extract(string=actorlist_5$actors_GOV, pattern="[:digit:]+ - [:digit:]+")

years <- sapply(actorlist_5, FUN = str_extract, pattern="[:digit:]+ - [:digit:]+")
year.set <- unique(unlist(years))

combineYears <- function(x, year.set, years) {
  new.list <- list(
    c1=rep(NA, length(year.set)), 
    c2=rep(NA, length(year.set)),
    c3=rep(NA, length(year.set)), 
    c4=rep(NA, length(year.set)), 
    c5=rep(NA, length(year.set))
  )
  for (i in 1:length(year.set)) {
    new.list[[1]][i] <- year.set[i]
    for (j in 2:length(new.list)) {
      ind <- which(years[[j-1]] == year.set[i])
      new.list[[j]][i] <- paste0(x[[j-1]][ind], collapse = ", ") 
    }
  }
  return(new.list)
}

out <- combineYears(actorlist_5, year.set, years)
xtable(as.data.frame(out))
