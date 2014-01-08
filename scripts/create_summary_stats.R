#---------------------------------------------#
#---------------------------------------------#
# CREATE DESCRIPTIVE SUMMARY DATA		    # 
#                                             #
# Authors: Nick Mader, Ian Matthew Morey,     # 
#		    and Emily Wiegand		    #  
#---------------------------------------------#
#---------------------------------------------#

## Set up workspace and designate/update file locations

  rm(list=ls())
  #myDir <- "/projects/Integrated_Evaluation_Youth_Support_Services/"
  myDir <- "H:/Integrated Evaluation Project for YSS Providers"
  setwd(myDir)
  dataPath <- "./data/preprocessed-data/" # File path to save scrambled data

  useScrambledData <- 0

## Load data

  try(detach(myData), silent=T)
  if (useScrambledData==1) {
    load("./data/preprocessed-data/Scram.Rda")
    myData <- Scram
    rm(Scram)
    myOutDir <- myDir %&% "/demos/"
    myGraphOut <- myOutDir
  } else {
    myData <- CpsYss_PP13
    myOutDir <- myDir %&% "/output/"
    myGraphOut <- myOutDir
  } 
  attach(myData)
  rm(CpsYss_PP13)


## Select variables to summarize




  ### SELECT VARIABLES FOR SUMMARY
  
    cNames <- colnames(myData)
    ctsVars <- c("mathss", "readss", "mathgain", "readgain", "mathpl_ME", "readpl_ME", "Pct_Attend", grep("bRace", cNames, value=T), grep("bLunch", cNames, value=T), grep("Tract_", cNames, value=T))  
    catVars <- c("mathpl", "readpl", "fGradeLvl", "fRace")
    #ctsVars <- c("mathss", "readss")
  
  ### CALCULATE SUMMARY STATISTICS
    
    # Continuous measures
    
      ctsMean_byAny     <- aggregate(myData[, ctsVars], list(fAnyYss),               mean, na.rm = T)
      ctsMean_byAnyGr   <- aggregate(myData[, ctsVars], list(fAnyYss, fGradeLvl),    mean, na.rm = T)
      ctsMean_bySite    <- aggregate(myData[, ctsVars], list(fShortSite),            mean, na.rm = T)
      ctsMean_bySiteGr  <- aggregate(myData[, ctsVars], list(fShortSite, fGradeLvl), mean, na.rm = T)
      ctsMean_bySch     <- aggregate(myData[, ctsVars], list(schlid),                mean, na.rm = T)
      ctsMean_bySchGr   <- aggregate(myData[, ctsVars], list(schlid, fGradeLvl),     mean, na.rm = T)
      
      colnames(ctsMean_byAny)[1]    <- "Site"; ctsMean_byAny$Grade <- "All"
      colnames(ctsMean_byAnyGr)[1]  <- "Site"; colnames(ctsMean_byAnyGr)[2] <- "Grade"
      colnames(ctsMean_bySite)[1]   <- "Site"; ctsMean_bySite$Grade <- "All"
      colnames(ctsMean_bySiteGr)[1] <- "Site"; colnames(ctsMean_bySiteGr)[2] <- "Grade"
      colnames(ctsMean_bySch)[1]    <- "Site"; ctsMean_bySch$Grade <- "All"
      colnames(ctsMean_bySchGr)[1]  <- "Site"; colnames(ctsMean_bySchGr)[2] <- "Grade"
    
    # Categorical measures
    
      CatCalc <- function(v){
        table <- as.data.frame(prop.table(table(fAnyYss, myData[, v]), 1))
        table[, v] <- v %&% "_" %&% table[, 2]
        #out <- cast(table, formula = as.formula("fAnyYss ~ " %&% v), value="Freq")
        return(table)
      }
      x <- CatCalc(cbind(mathpl, readpl))
      z <- aggregate(cbind(mathpl, readpl), list(fAnyYss), CatCalc)
    
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
  
  ### CREATE AVERAGES CHARACTERISTICS OF REPRESENTATIVE PEERS IN SAME SCHOOLS
  
    # Get Column %s
    SchProp_bySite <- prop.table(table(schlid, fShortSite), 2) # Get Column %s
    SchProp_byAny  <- prop.table(table(schlid, fAnyYss), 2) # Get Column %s
    
    # Create function to take average conditional on site designation and variable
    PeerAvg_forSV <- function(s, v) {
      nonNAN <- !is.na(ctsMean_bySch[, v])
      cbind(s, v, weighted.mean(ctsMean_bySch[nonNAN, v], PropData[nonNAN, s]))
    }  
    
    ### Calculate Averages by Yss Site ###
    
    PropData <- SchProp_bySite
    ByLvls <- levels(fShortSite)
    PeerAvgs_bySite <- t(mapply(PeerAvg_forSV,
                                           rep(ByLvls,  length(ctsVars)),
                                           rep(ctsVars, each=length(ByLvls))
                                          ) )
    rownames(PeerAvgs_bySite) <- NULL
    PeerAvgs_bySite <- data.frame(PeerAvgs_bySite)
    PeerAvgs_bySite$X3 <- as.numeric(levels(PeerAvgs_bySite$X3)[PeerAvgs_bySite$X3])
    ctsMean_bySiteSchPeer <- cast(PeerAvgs_bySite, X1 ~ X2, value = "X3")
    colnames(ctsMean_bySiteSchPeer)[1] <- "Site"
    ctsMean_bySiteSchPeer$Site <- ctsMean_bySiteSchPeer$Site %&% "\nSch-Based Peers"
    ctsMean_bySiteSchPeer$Grade <- "All"
    
    ### Calculate Averages by Any Yss ###
    
    PropData <- SchProp_byAny
    ByLvls <- levels(fAnyYss)
    PeerAvgs_byAny <- t(mapply(PeerAvg_forSV,
                                           rep(ByLvls,  length(ctsVars)),
                                           rep(ctsVars, each=length(ByLvls))
                                          ) )
    rownames(PeerAvgs_byAny) <- NULL
    PeerAvgs_byAny <- data.frame(PeerAvgs_byAny)
    PeerAvgs_byAny$X3 <- as.numeric(levels(PeerAvgs_byAny$X3)[PeerAvgs_byAny$X3])
    ctsMean_byAnySchPeer <- cast(PeerAvgs_byAny, X1 ~ X2, value = "X3")
    colnames(ctsMean_byAnySchPeer)[1] <- "Site"
    ctsMean_byAnySchPeer$Site <- ctsMean_byAnySchPeer$Site %&% "\nSch-Based Peers"
    ctsMean_byAnySchPeer$Grade <- "All"
  
  #---------------
  ### SAVE RESULTS
  #---------------
  
    save(ctsMean_byAny,    file = "./data/preprocessed-data/ctsMean_byAny.Rda")
    save(ctsMean_byAnyGr,  file = "./data/preprocessed-data/ctsMean_byAnyGr.Rda")
    save(ctsMean_bySite,   file = "./data/preprocessed-data/ctsMean_bySite.Rda")
    save(ctsMean_bySiteGr, file = "./data/preprocessed-data/ctsMean_bySiteGr.Rda")
    save(ctsMean_bySch,    file = "./data/preprocessed-data/ctsMean_bySch.Rda")
    save(ctsMean_bySchGr,  file = "./data/preprocessed-data/ctsMean_bySchGr.Rda")
    save(ctsMean_byAnySchPeer,  file = "./data/preprocessed-data/ctsMean_byAnySchPeer.Rda")
    save(ctsMean_bySiteSchPeer, file = "./data/preprocessed-data/ctsMean_bySiteSchPeer.Rda")
  


