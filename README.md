xSub Replication Code 
=====================

This repository contains R code to replicate all data files at
<http://www.x-sub.org>.

-   xSub version 2.0, October 2018.

System Requirements
===================

Successful execution of xSub replication scripts requires:

-   4-core processor, 16 GB RAM (recommended: 16-core, 64 GB RAM)

-   Linux operating system (tested on Ubuntu 16.04.3 LTS, 64-bit)

-   user account with read/write privileges

-   R installation (tested on R version 3.4.4)

-   GDAL installation (tested on GDAL 1.11.3)

-   R packages (installs automatically if missing):
    `gdata, countrycode, maptools, foreign, plotrix, sp, raster, rgeos, spatstat, parallel, foreach, meltt`

Compilation is also possible with a Windows operating system, but user
will need to manually replace parallelization routines from the
`parallel` R package with analogous routines from `foreach` (currently
commented-out in the code).

Workspace
=========

This repository contains several directories for input and output xSub
data files, and code. Users should clone the following directory
structure prior to executing R scripts.

-   `/Code`\
    (event extraction, classification and aggregation code)

-   `/Dictionaries`\
    (actor and event dictionaries)

-   `/Input`\
    (not included in GitHub repository)\
    (all input data files, various formats)

    -   `/Input/Events`\
        (violent event data)

    -   `/Input/GIS directory`\
        (raw data for control variables, GIS layers, borders, etc.)

-   `/Output`\
    (not included in GitHub repository)\
    (intermediate output data files **not** created for website upload,
    csv format)

    -   `/Output/Output_XXX`\
        (aggregated event counts from data source XXX)

    -   `/Output/Output_Covariates`\
        (aggregated control variables, various sources)

-   `/Data/Upload`\
    (not included in GitHub repository)\
    (output data files created for website upload, csv format)

    -   `/Upload/data_csv_country`\
        (aggregated event counts, .csv format, by country, by source)

Overview of Code
================

The process of converting raw input datasets into web-ready aggregates
entails five steps:

1.  **Actor and action dictionaries** (`Code/step1_dictionary/`).
    Because each data source uses a unique actor and action typology,
    xSub uses a separate dictionary for each conflict and source to map
    disparate actors mentioned in event reports to our categories of
    `SIDEA`, `SIDEB`, `SIDEC`, `SIDED`, and to map reported actions to
    our categories of `ANY`, `DIR`, `IND`, `PRT`.

    -   <span>*To create your own actor and event dictionaries*</span>:
        For each data source that requires an actor dictionary (e.g.
        GED, SCAD, etc.), there is a corresponding script
        (`step1_dictionary_GED.R`, `step1_dictionary_SCAD.R`, etc.),
        which runs through every actor mentioned in the raw data, and
        queries the user to specify whether the actor
        is government/rebel/civilian/other. xSub uses a different source
        code (`step1_dictionary_EventType.R`) to similarly create a
        multi-source action dictionary from event descriptions in the
        raw text data.

    -   <span>*To use pre-existing xSub dictionaries*</span>: skip this
        step and proceed to step 2.

2.  **Event coding and aggregation** (`Code/step2_eventcode/`). Once an
    actor dictionary for a data source is created, the next step is to
    classify individual events and aggregate them to consistent
    spatio-temporal units of analysis. For each data source, there is a
    set of (usually) two aggregation scripts:

    1.  `step2_eventcode1_XXX.R`: Code to classify events by actor and
        tactics, using dictionaries created in step 1.

    2.  `step2_eventcode2_XXX.R`: Code to sum individual events by
        administrative unit (country, province, district) or PRIO grid
        cell, and unit of time (year, month, week, day), and write
        aggregate event counts to file, for each country and level of
        aggregation, in RData format.

3.  **Covariate coding and aggregation** (`Code/step3_covariates/`). In
    addition to violent events, xSub also includes data on political,
    socio-economic, ethno-linguistic and geographic covariates, for each
    country, at each level of analysis. This process can proceed
    independently of event coding (steps 1 and 2), and is governed by
    the code `step3_covariates_parallel.R` (also,
    `step3_weather_parallel.R`). Most of the raw data used in this step
    are in the format of GIS shapefiles or raster images, with global
    coverage. This code creates boundary shapefiles corresponding to
    each spatial level of analysis – administrative unit (country,
    province, district) or PRIO grid cell – and calculates summary
    statistics for the relevant covariates within each spatial unit. It
    writes the output data to file, for each country and level of
    aggregation, in RData format.

4.  **Merge and export** (`Code/step4_merge/`, depends on
    `Code/step4x_variable_select.R`). The file `step4_merge1.R` merges
    the event counts with covariates, for each country and level of
    analysis, and writes the individual country files to disk. The
    `step4_merge2.R` script concatenates the individual files into a
    large matrix, for each data source and level of aggregation, and
    writes the file to disk, in csv format. These are the processed csv
    files that are uploaded to the xSub server for public use.

5.  **Maps and graphics** (`Code/step5_maps/`). In addition to event
    coding, aggregation and merging files, we include scripts to
    replicate the data visualizations featured on the xSub website
    (`step5_maps.R`).

Input Data Sources
==================

Note that, due to file size and usage restrictions, the repository
currently provides empty directories for input and output data files. To
replicate xSub data, users will need to obtain the original data from
third-party sources, and place them into their corresponding
subdirectories in the `/Input` folder.

Spatial geometries
------------------

The following boundary shapefiles should be placed in the
`/Input/GIS/Borders` directory:

-   [Database of Global Administrative
    Areas (GADM)](http://www.gadm.org/) / version 2.8

    -   Hijmans, Robert, Nell Garcia, and John Wieczorek. *GADM:
        database of global administrative areas, version 2.8* (2017).

    -   <http://www.gadm.org/>

-   [PRIO-GRID](http://grid.prio.org/) / version 2.0

    -   Tollefsen, Andreas Foro, Havard Strand, and Halvard Buhaug.
        “PRIO-GRID: A unified spatial data structure.” *Journal of Peace
        Research* 49, no. 2 (2012): 363-374.

    -   <http://grid.prio.org/>

-   [GeoReferenced Electoral Districts
    Datasets (GRED)](http://electiondataarchive.org/datacenter-gred.html)
    / version Beta 2

    -   Kollman, Ken, Allen Hicken, Daniele Caramani, David Backer,
        David Lublin, Joel Selway, and Fabricio Vasselai. 2017.
        *GeoReferenced Electoral Districts Datasets (Beta)*. Ann Arbor,
        MI: Center for Political Studies, University of Michigan.

    -   <http://electiondataarchive.org/datacenter-gred.html>

Event data
----------

The following event datasets should be placed in the `/Input/Events`

-   [American Bar Association /
    Darfur](http://muse.jhu.edu/article/387199/pdf)

    -   Totten, Samuel. “The US Investigation into the Darfur Crisis and
        the US Government’s Determination of Genocide.” *Genocide
        Studies and Prevention* 1, no. 1 (2006): 57-78.

    -   <http://muse.jhu.edu/article/387199/pdf>

-   [Armed Conflict Location and Event Data
    Project (ACLED)](http://www.acleddata.com/) / version 8

    -   Raleigh, Clionadh, Andrew Linke, Hvard Hegre, and
        Joakim Karlsen. “Introducing ACLED: An armed conflict location
        and event dataset special data feature.” *Journal of Peace
        Research* 47, no. 5 (2010): 651-660.

    -   <http://www.acleddata.com/>

-   [Beissinger](https://www.princeton.edu/~mbeissin/) / Protest

    -   Beissinger, Mark R. *Nationalist mobilization and the collapse
        of the Soviet State*. Cambridge University Press, 2002.

    -   <https://www.princeton.edu/~mbeissin/>

-   [Beissinger](https://www.princeton.edu/~mbeissin/) / Riot

    -   Beissinger, Mark R. *Nationalist mobilization and the collapse
        of the Soviet State*. Cambridge University Press, 2002.

    -   <https://www.princeton.edu/~mbeissin/>

-   [Beissinger](https://www.princeton.edu/~mbeissin/) / Ukraine

    -   Beissinger, Mark R. *Nationalist mobilization and the collapse
        of the Soviet State*. Cambridge University Press, 2002.

    -   <https://www.princeton.edu/~mbeissin/>

-   [Empirical Studies of Conflict
    Project (ESOC)](https://esoc.princeton.edu/) / Afghanistan / WITS /
    2010 version

    -   Wigle, John. “Introducing the worldwide incidents tracking
        system (WITS).” *Perspectives on Terrorism* 4, no. 1 (2010).

    -   <https://esoc.princeton.edu/>

-   [Empirical Studies of Conflict
    Project (ESOC)](https://esoc.princeton.edu/) / Iraq / SIGACT /
    version 3

    -   Eli Berman, Jacob N. Shapiro, and Joseph H. Felter, “Can Hearts
        and Minds Be Bought? The Economics of Counterinsurgency in
        Iraq,” *Journal of Political Economy* 119, no. 4 (August 2011).

    -   <https://esoc.princeton.edu/>

-   [Empirical Studies of Conflict
    Project (ESOC)](https://esoc.princeton.edu/) / Iraq / WITS / version
    3

    -   Wigle, John. “Introducing the worldwide incidents tracking
        system (WITS).” *Perspectives on Terrorism* 4, no. 1 (2010).

    -   <https://esoc.princeton.edu/>

-   [Empirical Studies of Conflict
    Project (ESOC)](https://esoc.princeton.edu/) / Mexico / Drug-Related
    Murders

    -   Calderón, Gabriela, Gustavo Robles, Alberto Díaz-Cayeros, and
        Beatriz Magaloni. “The beheading of criminal organizations and
        the dynamics of violence in Mexico.” *Journal of Conflict
        Resolution* 59, no. 8 (2015): 1455-1485.

    -   <https://esoc.princeton.edu/>

-   [Empirical Studies of Conflict
    Project (ESOC)](https://esoc.princeton.edu/) / Pakistan / BFRS /
    2010 version

    -   de Mesquita, Bueno, Ethan C. Christine Fair, Jenna Jordan, Rasul
        Bakhsh Rais, and Jacob N. Shapiro. “The BFRS Political Violence
        in Pakistan Dataset.” (2013).

    -   <https://esoc.princeton.edu/>

-   [Empirical Studies of Conflict
    Project (ESOC)](https://esoc.princeton.edu/) / Pakistan / WITS /
    2010 version

    -   Wigle, John. “Introducing the worldwide incidents tracking
        system (WITS).” *Perspectives on Terrorism* 4, no. 1 (2010).

    -   <https://esoc.princeton.edu/>

-   [Ferwerda](http://www.jeremyferwerda.com/) and
    [Miller](http://www.nicholaslmiller.com/)

    -   Ferwerda, Jeremy, and Nicholas L. Miller. “Political devolution
        and resistance to foreign rule: A natural experiment.” *American
        Political Science Review* 108, no. 03 (2014): 642-660.

    -   <http://www.jeremyferwerda.com/>,
        <http://www.nicholaslmiller.com/>

-   [National Violence Monitoring System (NVMS)
    Indonesia](http://www.snpk-indonesia.com/)

    -   Barron, Patrick, Sana Jaffrey, and Ashutosh Varshney. “When
        Large Conflicts Subside: The Ebbs and Flows of Violence in
        Post-Suharto Indonesia.” *Journal of East Asian Studies* 16, no.
        02 (2016): 191-217.

    -   <http://www.snpk-indonesia.com/>

<<<<<<< HEAD
-   [Northern Ireland Research Initiative (NIRI)](https://niresearchinitiative.weebly.com/)
=======
-   [Northern Ireland Research Initiative (NIRI)]
    (https://niresearchinitiative.weebly.com/)
>>>>>>> 03e2704cb568f21664734b3943662c7de80426b8

    -   Davenport, Christian, Cyanne Loyle, and Christopher Sullivan. 
        Northern Ireland Research Initiative. (February 9, 2017).

    -   <https://niresearchinitiative.weebly.com/>

-   [Political Instability Task Force / Worldwide Atrocities
    Dataset](http://eventdata.parusanalytics.com/data.dir/atrocities.html)
    / version 1.1b1

    -   Schrodt, Philip A. and Jay Ulfelder, “Political Instability Task
        Force Atrocities Event Data Collection,” Version 1.1b1
        (September 12, 2016)

    -   <http://eventdata.parusanalytics.com/data.dir/atrocities.html>

-   [Social Conflict Analysis
    Database (SCAD)](https://www.strausscenter.org/scad.html)

    -   Salehyan, Idean, Cullen S. Hendrix, Jesse Hamner, Christina
        Case, Christopher Linebarger, Emily Stull, and
        Jennifer Williams. “Social conflict in Africa: A new database.”
        *International Interactions* 38, no. 4 (2012): 503-511.

    -   <https://www.strausscenter.org/scad.html>

-   [UCDP / Georeferenced Event Dataset](http://ucdp.uu.se/) / version
    171

    -   Sundberg, Ralph, and Erik Melander. “Introducing the UCDP
        Georeferenced Event Dataset.” *Journal of Peace Research*
        50, no. 4 (2013): 523-532.

    -   <http://ucdp.uu.se/>

-   [Zhukov](http://sites.lsa.umich.edu/zhukov/) / North Caucasus

    -   Toft, Monica Duffy, and Yuri M. Zhukov. “Islamists and
        nationalists: Rebel motivation and counterinsurgency in Russia’s
        North Caucasus.” *American Political Science Review* 109, no. 02
        (2015): 222-238.

    -   <http://sites.lsa.umich.edu/zhukov/>

-   [Zhukov](http://sites.lsa.umich.edu/zhukov/) / Chechnya

    -   Zhukov, Yuri M. *Theory of Indiscriminate Violence*. Ph.D.
        Dissertation, Harvard University (2014).

    -   <http://sites.lsa.umich.edu/zhukov/>

-   [Zhukov](http://sites.lsa.umich.edu/zhukov/) / Libya

    -   Baum, Matthew A., and Yuri M. Zhukov. “Filtering revolution:
        Reporting bias in international newspaper coverage of the Libyan
        civil war.” *Journal of Peace Research* 52, no. 3
        (2015): 384-400.

    -   <http://sites.lsa.umich.edu/zhukov/>

-   [Zhukov](http://sites.lsa.umich.edu/zhukov/) / Ukraine

    -   Zhukov, Yuri M. “Trading hard hats for combat helmets: The
        economics of rebellion in eastern Ukraine.” *Journal of
        Comparative Economics* 44, no. 1 (2016): 1-15.

    -   <http://sites.lsa.umich.edu/zhukov/>

Local demographics, geography, ethnicity, weather
-------------------------------------------------

The following geospatial datasets should be placed in the
`/Input/GIS/Covariates` directory:

-   [Gridded Population of the
    World (GPW)](http://sedac.ciesin.columbia.edu/data/collection/gpw-v3)
    / version 3

    -   Center for International Earth Science Information Network
        (CIESIN), Centro Internacional de Agricultura Tropical (CIAT).
        *Gridded Population of the World, Version 3 (GPWv3) Data
        Collection*. Palisades, NY: CIESIN, Columbia University, 2005.

    -   <http://sedac.ciesin.columbia.edu/data/collection/gpw-v3>

-   [5-minute gridded elevation
    data (ETOPO05)](https://www.ngdc.noaa.gov/mgg/global/etopo5.HTML) /
    2005 version

    -   Edwards, M. O. *Global gridded elevation and bathymetry (ETOPO5)
        digital raster data on a 5-minute geographic (lat× lon) 2160×
        4320 (centroid-registered) grid.* Boulder, CO: National Oceanic
        and Atmospheric Administration, 1989.

    -   <https://www.ngdc.noaa.gov/mgg/global/etopo5.HTML>

-   [Global Land Cover
    Characterization (GLCC)](https://lta.cr.usgs.gov/GLCC) / version 2

    -   Loveland, Thomas R., Bradley C. Reed, Jesslyn F. Brown,
        Donald O. Ohlen, Zhiliang Zhu, L. W. M. J. Yang, and James W.
        Merchant. “Development of a global land cover characteristics
        database and IGBP DISCover from 1 km AVHRR data.” *International
        Journal of Remote Sensing* 21, no. 6-7 (2000): 1303-1330.

    -   <https://lta.cr.usgs.gov/GLCC>

-   [Geo-Referencing of Ethnic
    Groups (GREG)](https://icr.ethz.ch/data/greg/) / 2010 version

    -   Weidmann, Nils B., Jan Ketil Rød, and Lars-Erik Cederman.
        “Representing Ethnic Groups in Space: A New Dataset.” *Journal
        of Peace Research* 47, no. 4 (2010): 491-99.

    -   <https://icr.ethz.ch/data/greg/>

-   [World Language Mapping
    System (WLMS)](http://www.worldgeodatasets.com/language) / version
    19

    -   Global Mapping International. *World Language Mapping System
        version 19*. Colorado Springs, CO: Global Mapping
        International, 2016.

    -   <http://www.worldgeodatasets.com/language>

-   [Global
    GIS (GGIS)](https://webgis.wr.usgs.gov/globalgis/datasets.htm) /
    version 6.2

    -   Hearn, P., Hare, T., Schruben, P., Sherrill, D., LaMar, C. and
        Tsushima, P. *Global GIS Global Coverage DVD*. Washington, DC:
        US Geological Survey, 2003.

    -   <https://webgis.wr.usgs.gov/globalgis/datasets.htm>

-   [PRIO petroleum
    dataset](https://www.prio.org/Data/Geographical-and-Resource-Datasets/Petroleum-Dataset/)
    / version 1.2

    -   Lujala, Päivi; Jan Ketil Rod and Nadia Thieme. “Fighting over
        Oil: Introducing A New Dataset,” *Conflict Management and Peace
        Science* 24, no. 3 (2007): 239-256.

    -   <https://www.prio.org/Data/Geographical-and-Resource-Datasets/Petroleum-Dataset/>

-   [Digital Chart of the
    World (DCW)](http://statisk.umb.no/ikf/gis/dcw/) / 1992 version

    -   Danko, David M. “The digital chart of the world project.”
        *Photogrammetric Engineering and Remote Sensing* 58
        (1992): 1125-1128.

    -   <http://statisk.umb.no/ikf/gis/dcw/>

-   [National Oceanic and Atmospheric Administration (NOAA) /
    temperature and
    precipitation](https://www.esrl.noaa.gov/psd/data/gridded/data.UDel_AirT_Precip.html)
    / version 4.01

    -   Matsuura, Kenji, and Cort J. Willmott. “Terrestrial
        precipitation: 1900-2010 gridded monthly time series.”Newark,
        DE: Department of Geography University of Delaware, 2015.

    -   <https://www.esrl.noaa.gov/psd/data/gridded/data.UDel_AirT_Precip.html>
