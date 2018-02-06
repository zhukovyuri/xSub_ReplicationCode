#########################################
## Variable selection
#########################################


varz.id <- c(
  # ID variables
  "ID_0", # GADM country ID (numeric)
  "ISO", # ISO3 country ID (string)
  "NAME_0", # country name (string)
  "ID_1", # GADM province ID (numeric)
  "NAME_1", # province name (string)
  "ID_2", # GADM district ID (numeric)
  "NAME_2", # district name (string)
  "CLEA_CST", # CLEA constituency ID (numeric)
  "CLEA_CST_N", # CLEA constituency name (string)
  "PRIO_GID", # PRIO-GRID cell ID (numeric)
  "PRIO_XCOORD", # longitude of cell centroid (numeric)
  "PRIO_YCOORD", # latitude of cell centroid (numeric)
  "PRIO_COL", # PRIO-GRID column number (numeric)
  "PRIO_ROW", # PRIO-GRID row number (numeric)
  "TID", # day ID (numeric)
  "YEAR", # year (numeric)
  "YRMO", # year-month (YYYYMM) (numeric)
  "WID", # week ID (numeric)
  "DATE", # date (YYYYMMDD) (numeric)
  ""); varz.id <- varz.id[-length(varz.id)]
  varz.id
  
  # Event counts
  varz.event <- c(
    "INITIATOR_SIDEA", # A: incumbent government, pro-govt militia, third party acting on incumbent's behalf
    "INITIATOR_SIDEB", # B: rebels, anti-government militia, third party acting on rebels' behalf, and other armed groups directly challenging the government
    "INITIATOR_SIDEC", # C: local militia, tribe, other non-state actors \textit{not} directly challenging the government
    "INITIATOR_SIDED", # D: civilians
    "TARGET_SIDEA", # 
    "TARGET_SIDEB", # 
    "TARGET_SIDEC", # 
    "TARGET_SIDED", # 
    "ACTION_ANY", # any use of force
    "ACTION_IND", # indiscriminate force (e.g. indirect fire, shelling, air strikes, chemical weapons)
    "ACTION_SEL", # selective force (e.g. direct fire, arrest, assassination)
    "ACTION_PRT", # protest
    "SIDEA_ANY", # 
    "SIDEA_IND", # 
    "SIDEA_SEL", # 
    "SIDEA_PRT", # 
    "SIDEB_ANY", # 
    "SIDEB_IND", # 
    "SIDEB_SEL", # 
    "SIDEB_PRT", # 
    "SIDEC_ANY", # 
    "SIDEC_IND", # 
    "SIDEC_SEL", # 
    "SIDEC_PRT", # 
    "SIDED_ANY", # 
    "SIDED_IND", # 
    "SIDED_SEL", # 
    "SIDED_PRT", # 
    ""); varz.event <- varz.event[-length(varz.event)]
  varz.event
  
  # Covariates: political, socio-economic, ethno-linguistic and geographic (PSEG)
  varz.x <- c(
    "POP_1990", # population per square kilometer, 1990 (GPW v3) (numeric)
    "POP_1995", # population per square kilometer, 1995 (GPW v3) (numeric)
    "POP_2000", # population per square kilometer, 2000 (GPW v3) (numeric)
    "ELEV_MEAN", # average elevation, meters (ETOPO 05 DEM) (numeric)
    "ELEV_SD", # standard deviation of elevation, meters (ETOPO 05 DEM) (numeric)
    "ELEV_MAX", # maximum elevation, meters (ETOPO 05 DEM) (numeric)
    "OPEN_TERRAIN", # proportion of land covered by open terrain (shrublands, savannah, grasslands, barren or sparsely vegetated) (GLCC)
    "FOREST", # proportion of land covered by evergreen, deciduous or mixed forest (GLCC)
    "WETLAND", # proportion of land covered by wetlands or water bodies (GLCC) 
    "FARMLAND", # proportion of land covered by croplands (GLCC)
    "GREG_NGROUPS", # number of local ethnic groups (GREG) (numeric)
    "GREG_GROUPS", # names of local ethnic groups (GREG) (string)
    "WLMS_NLANG", # number of local languages (WLMS) (numeric)
    "WLMS_LANGS", # names of local languages (WLMS) (string)
    "NBUILTUP", # number of built-up areas (GGIS) (numeric)
    "BUILTUP", # names of built-up areas (GGIS) (string)
    "NPETRO", # number of petroleum fields (PRIO) (numeric)
    "PETRO", # names of petroleum fields (PRIO) (string)
    "DIST2PROVCAP", # distance to nearest provincial capital, km (GGIS) (numeric) 
    "DIST2CAP", # distance to national capital, km (GGIS) (numeric)
    "ROAD_XING", # number of primary, secondary roads crossing border (DCW) (numeric)
    "ROAD_LENGTH", # length of primary, secondary roads, km (DCW) (numeric)
    "AREA_KM2", # area of spatial unit, km$^2$ (numeric)
    "ROAD_DENSITY", # road density, km/km$^2$ (DCW) (numeric)
    ""); varz.x <- varz.x[-length(varz.x)]
  varz.x
  
  # Weather
  varz.w <- c(
    "TEMP", # average monthly air temperature (NOAA) (numeric)
    "RAIN", # total montly precipitation (NOAA) (numeric)
    ""); varz.w <- varz.w[-length(varz.w)]
  varz.w
  
  