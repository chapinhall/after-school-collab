#---------------------------------------------#
#---------------------------------------------#
# CREATE DESCRIPTIVE SUMMARY STATISTICS	      # 
#                                             #
# Authors: Nick Mader, Ian Matthew Morey,     # 
#		    and Emily Wiegand		                  #  
#---------------------------------------------#
#---------------------------------------------#

## Set up workspace and designate/update file locations

  rm(list=ls())
  #myDir <- "/projects/Integrated_Evaluation_Youth_Support_Services"
  myDir <- "H:/Integrated Evaluation Project for YSS Providers"
  setwd(myDir)
  dataPath <- "./data/preprocessed-data/" # File path to locate and save data

  library(reshape)
  library(plyr)
  
  useScrambledData <- 0

## Load selected data

  try(detach(myData), silent=T)

  if (useScrambledData==1) {
    load(paste0(dataPath,"Scram.Rda"))
    myData <- Scram
    rm(Scram)
    myGraphOut <- paste0(myDir,"/demos/") 
  } else {
  	load("./data/preprocessed-data/CpsYss_PP13.Rda")
  	cNames <- colnames(CpsYss_PP13)
  	keepVars <- c("sid", "SchYear", "Stud_Tract", "schlid", "fGradeLvl", "grade_level",
                "Female", "disab", "fRace", grep("bRace", cNames, value=T), grep("bLunch", cNames, value=T),
                "readss", "mathss", "readgain", "mathgain", "readpl", "mathpl", "mathpl_ME", "readpl_ME", "mathss_pre", "readss_pre",
                "Pct_Attend", "Pct_Attend_pre", "bOnTrack", "bHsGrad",
                "Stud_X", "Stud_Y", grep("Tract_", cNames, value=T), "Stud_CcaNum", "Stud_CcaName",
                "fYssType", "fYssSite", "fAnyYss", "UsedYss") #"Pct_Absent", "Pct_Absent_pre",
      myData <- CpsYss_PP13[, keepVars]
	  myGraphOut <- paste0(myDir,"/output/")
      rm(CpsYss_PP13)} 

  attach(myData)
  
## XXX Will need to farm this into data prep code
  myData$bYmca     <- as.numeric(myData$fAnyYss=="YMCA")
  myData$fYmcaSite <- fYssSite

## Select variables to summarize

  cNames <- colnames(myData)
  ctsVars <- c("mathss", "readss", "mathgain", "readgain", "mathpl_ME", "readpl_ME", "Pct_Attend",
               grep("bRace",  cNames, value=T),
               grep("bLunch", cNames, value=T),
               grep("Tract_", cNames, value=T))  
  catVars <- c("mathpl", "readpl", "fGradeLvl", "fRace")
  #ctsVars <- c("mathss", "readss")
  
## Calculate summary statistics for continuous measures
  myOrg <- "Ymca" # Am setting this up to eventually be an lapply across organization names
  bOrg  <- get(paste0("b", myOrg))
  fOrg  <- factor(bOrg, levels = c(0, 1), labels=(c(paste0("Non-", myOrg), myOrg)))
  fSite <- get(paste0("f", myOrg, "Site"))
  ctsMean_byOrg     <- aggregate(myData[, ctsVars], list(fOrg                  ), mean, na.rm = T)
  ctsMean_bySite    <- aggregate(myData[, ctsVars], list(fOrg, fSite           ), mean, na.rm = T)
  ctsMean_byOrgGr   <- aggregate(myData[, ctsVars], list(fOrg,        fGradeLvl), mean, na.rm = T)
  ctsMean_bySiteGr  <- aggregate(myData[, ctsVars], list(fOrg, fSite, fGradeLvl), mean, na.rm = T)
  
  
  colnames(ctsMean_byOrg)[1]    <- "Org"; ctsMean_byOrg$Site <- paste("All", myOrg);   ctsMean_byOrg$Grade  <- "All"
  colnames(ctsMean_bySite)[1]   <- "Org"; colnames(ctsMean_bySite)[2] <- "Site";       ctsMean_bySite$Grade <- "All"
  colnames(ctsMean_byOrgGr)[1]  <- "Org"; ctsMean_byOrgGr$Site <- paste("All", myOrg); colnames(ctsMean_byOrgGr)[2]  <- "Grade"
  colnames(ctsMean_bySiteGr)[1] <- "Org"; colnames(ctsMean_bySiteGr)[2] <- "Site";     colnames(ctsMean_bySiteGr)[3] <- "Grade"
  
## Combine summary statistics across different measures into one data frame
  
  ctsMean <- rbind(ctsMean_byOrg, ctsMean_byOrgGr, ctsMean_bySite, ctsMean_bySiteGr)
  ctsMean$Org  <- as.character(ctsMean$Org)
  ctsMean$Site <- as.character(ctsMean$Site)  
  
    #statDFs <- list(ctsMean_bySiteGr, ctsMean_byAny, ctsMean_byAnyGr, ctsMean_bySite) # ctsMean_bySch, ctsMean_bySchGr
    #siteAsChar <- function(df){df$Site <- as.character(df$Site); return(df)}
    #statDFs2 <- lapply(statDFs, siteAsChar)
    #ctsMeans <- do.call("rbind", statDFs2)
  
    #ctsMean_bySch     <- aggregate(myData[, ctsVars], list(schlid),              mean, na.rm = T)
    #ctsMean_bySchGr   <- aggregate(myData[, ctsVars], list(schlid, fGradeLvl),   mean, na.rm = T)
    #colnames(ctsMean_bySch)[1]    <- "Site"; ctsMean_bySch$Grade  <- "All"; 
    #colnames(ctsMean_bySchGr)[1]  <- "Site"; colnames(ctsMean_bySchGr)[2] <- "Grade"


  
  
  
## Calculate summary statistics for categorical measures
    
###### ERW: Return to rework this section once I see what is needed for the graphs

  CatCalc <- function(v){
    table <- as.data.frame(prop.table(table(fAnyYss, myData[, v]), 1))
    table[, v] <- paste0(v,"_",table[, 2])
    #out <- cast(table, formula = as.formula(paste0("fAnyYss ~ ",v)), value="Freq")
    return(table)}



  x <- CatCalc(cbind(fAnyYss,mathpl,readpl))
  z <- aggregate(cbind(mathpl, readpl), list(fAnyYss), CatCalc)

  ds <- function(x){ deparse(substitute(x))}
    
  myFun <- function(v){
    table <- as.data.frame(prop.table(table(v)))
    #table[, ds(v)] <- ds(v) %&% "_" %&% table[, 1]
    #out <- cast(table, formula = as.formula(". ~ " %&% ds(v)), value="Freq")
    #return(out)
    return(table)
  }
  z <- aggregate(cbind(mathpl, readpl), list(fAnyYss), CatCalc2)
    
  simplerFun <- function(v){
    x <- prop.table(table(v))        
    dimnames(x)$v <- dimnames(x)$v
  }
  aggregate(cbind(mathpl, readpl), list(fAnyYss), simplerFun)
    
  GrDist <- table(fAnyYss, fGradeLvl)
  GrDistProp <- prop.table(GrDist, 1)  
  GrDistPropL <- as.data.frame(GrDistProp)  
  if (s != "All") {
    p <- prop.table(table(fGradeLvl[fShortSite==s]))
    df <- data.frame(cbind(s, data.frame(p)))
    colnames(df) <- colnames(GrDistPropL)
    GrDistPropL <- rbind(GrDistPropL, df)
  }
  
  
## Calculate average characteristics for representative peers in same schools
  # Representative peer group is defined as students who attend the same schools as students in the treatment program.
  # If 60% of treatment kids attend School H, and 40% attend School Y, 
  # the appropriate comparison for the mean of the treatment group is not the citywide average, but .6*School H's avg + .4*School Y's.
  # This is one way of correcting for the fact that partcipants are not broadly representative of the CPS system.
  # The below code calculates these averages both for the full treatment group and for each site.
  
 # Get school %s by population and treatment status
  SchProp_bySite <- prop.table(table(schlid, fYssSite), 2) # Percent of students from each school at each site
  SchProp_byAny  <- prop.table(table(schlid, fAnyYss), 2) # Percent of students from each school in treatment/control
  PropData <- SchProp_bySite # First analysis will use site averages
  
  
 # Create function that combines "peer" averages by proportionately weighting school averages
  PeerAvg_forSV <- function(s, v) {
    nonNAN <- !is.na(ctsMean_bySch[, v])
    cbind(s, v, weighted.mean(ctsMean_bySch[nonNAN, v], PropData[nonNAN, s]))
  }
    
  # Apply this function to for all variables at the site level
    
  Sites <- levels(fYssSite) # Remember that ctsVars is a list of all variables
  
  PeerAvgs_bySite <- t(mapply(PeerAvg_forSV,
                              rep(Sites,  times=length(ctsVars)),
                              rep(ctsVars, each=length(Sites))
                              ) )
  rownames(PeerAvgs_bySite) <- NULL
  PeerAvgs_bySite <- data.frame(PeerAvgs_bySite)
  colnames(PeerAvgs_bySite) <- c("Site", "ctsVar","Mean")
  PeerAvgs_bySite$Mean <- as.numeric(levels(PeerAvgs_bySite$Mean)[PeerAvgs_bySite$Mean]) # Convert factor to numeric

  # Transpose the resulting dataframe
  
  ctsMean_bySiteSchPeer <- cast(PeerAvgs_bySite, Site ~ ctsVar, value = "Mean")
  ctsMean_bySiteSchPeer$Site <- paste0(ctsMean_bySiteSchPeer$Site,"\nSch-Based Peers")
  ctsMean_bySiteSchPeer$Grade <- "All"
  
  # Repeat calculations at treatment group level (create a set of peers for T that attend the same schools as T students)
  
  PropData <- SchProp_byAny # Change proportional dataset
  Status <- levels(fAnyYss)
  PeerAvgs_byAny <- t(mapply(PeerAvg_forSV,
                             rep(Status,  length(ctsVars)),
                             rep(ctsVars, each=length(Status))
                             ) )
  rownames(PeerAvgs_byAny) <- NULL
  PeerAvgs_byAny <- data.frame(PeerAvgs_byAny)
  PeerAvgs_byAny$X3 <- as.numeric(levels(PeerAvgs_byAny$X3)[PeerAvgs_byAny$X3])
  ctsMean_byAnySchPeer <- cast(PeerAvgs_byAny, X1 ~ X2, value = "X3")
  colnames(ctsMean_byAnySchPeer)[1] <- "Site"
  ctsMean_byAnySchPeer$Site <- paste0(ctsMean_byAnySchPeer$Site,"\nSch-Based Peers")
  ctsMean_byAnySchPeer$Grade <- "All"
  
    # Combine peer calculations with existing ctsMeans table
  
  ctsMeans <- rbind(ctsMeans,ctsMean_bySiteSchPeer,ctsMean_byAnySchPeer)
  
  #---------------
  ### SAVE RESULTS
  #---------------
  
  if (useScrambledData==1) { 
    save(ctsMeans,    file = paste0(dataPath,"ctsMeans","_DEMO.Rda"))
  } else {  
    save(ctsMeans,    file = paste0(dataPath,"ctsMeans.Rda"))
    save(myData, file = paste0(dataPath, "subset_CpsYss_PP13.Rda"))
  }

