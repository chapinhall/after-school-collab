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
  ds <- function(x){ deparse(substitute(x))}
  
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

## XXX Short term edits to make data set match eventual result (IMM is working on data prep)
  myData$Org[myData$fAnyYss=='YMCA'] <- "YMCA"
  myData$Org[is.na(myData$Org)] <- "None"
  myData$Site[myData$fAnyYss=='YMCA'] <- as.character(myData$fYssSite[myData$fAnyYss=='YMCA'])
  attach(myData)  
  
  
## Select variables to summarize

  cNames <- colnames(myData)
  ctsVars <- c("mathss", "readss", "mathgain", "readgain", "mathpl_ME", "readpl_ME", "Pct_Attend",
               grep("bRace",  cNames, value=T),
               grep("bLunch", cNames, value=T),
               grep("Tract_", cNames, value=T))  
  catVars <- c("mathpl", "readpl", "fGradeLvl")
  
  # Create dummy variables of catVar vlaues to be treated as continuous vars
  for (var in catVars) {
    cVar <- as.character(get(var))
    for(val in unique(cVar)){
      if (!is.na(val)) {
        newVar <- paste(var, val, sep="_")
        myData[, newVar] <- ifelse(cVar==val, 1, 0)
        ctsVars <- c(ctsVars, newVar)
      }
    }
  } # XXX There's likely a more elegant way to do this. Note that model.matrix(~0+var) drops observations with NAs, returning a vector of shorter length (which, at this stage, we don't want)


  
## Before calculating summary statistics, create a reduced dataset that includes only the necessary variables
  
  cNames <- colnames(myData)
  calcVars <- c("sid", "Org", "Site", "fGradeLvl", "SchYear", "schlid","mathss", "readss", "mathgain", "readgain", "Pct_Attend",
                grep("bRace",     cNames, value=T),
                grep("bLunch",    cNames, value=T),
                grep("Tract_",    cNames, value=T),
                grep("fGradeLvl_", cNames, value=T),
                grep("mathpl_",   cNames, value=T),
                grep("readpl_",   cNames, value=T)) 
  calcData <- myData[,calcVars]
  detach(myData)
  
  # Also, remove obs that didn't match to CPS and are NA for basically all the variables we will use for calculations
  calcData <- calcData[!is.na(calcData$schlid),]
  
  ### ERW: We need to look into who these IDs are and how they got mixed into the dataset.  They should theoretically be removed during
    ## data preparation unless there is a reason to keep them in.  I'm comfortable to drop now since NAs get dropped from all calcs.  But 
    ## some might have a few values?  How does that happen?  Fundamental question: are these people in the CPS master file?
  
    
## Calculate summary statistics
  attach(calcData)
  
  meanVars <- c("mathss", "readss", "mathgain", "readgain", "Pct_Attend",
                grep("bRace",     cNames, value=T),
                grep("bLunch",    cNames, value=T),
                grep("Tract_",    cNames, value=T),
                grep("GradeLvl_", cNames, value=T),
                grep("mathpl_",   cNames, value=T),
                grep("readpl_",   cNames, value=T)) 
  
  ctsMean_byOrg     <- aggregate(calcData[, meanVars], list(Org                  ), mean, na.rm = T)
  ctsMean_bySite    <- aggregate(calcData[, meanVars], list(Org, Site            ), mean, na.rm = T)
  ctsMean_byOrgGr   <- aggregate(calcData[, meanVars], list(Org,        fGradeLvl), mean, na.rm = T)
  ctsMean_bySiteGr  <- aggregate(calcData[, meanVars], list(Org, Site,  fGradeLvl), mean, na.rm = T)
    
  colnames(ctsMean_byOrg)[1]    <- "Org"; ctsMean_byOrg$Site <-   "All";           ctsMean_byOrg$Grade  <- "All";
  colnames(ctsMean_bySite)[1]   <- "Org"; colnames(ctsMean_bySite)[2] <- "Site";   ctsMean_bySite$Grade <- "All";
  colnames(ctsMean_byOrgGr)[1]  <- "Org"; ctsMean_byOrgGr$Site <- "All";           colnames(ctsMean_byOrgGr)[2]  <- "Grade";
  colnames(ctsMean_bySiteGr)[1] <- "Org"; colnames(ctsMean_bySiteGr)[2] <- "Site"; colnames(ctsMean_bySiteGr)[3] <- "Grade";
  
  
  # ERW: will need to add school year as another unit of analysis
  
  # NSM: Experimenting with data.table, which supposedly has performance advantages
      #DT <- data.table(ctsMean)
      #ctsMean_alt <- DT[,mean("mathss"), by = fOrg]

  
  ## Combine summary statistics across different measures into one data frame
  
  ctsMeans <- rbind(ctsMean_byOrg, ctsMean_byOrgGr, ctsMean_bySite, ctsMean_bySiteGr)
  ctsMeans$Org  <- as.character(ctsMeans$Org)
  ctsMeans$Site <- as.character(ctsMeans$Site)
  ctsMeans$Year <- "2012-13" # Placeholder before more years are introduced
  ctsMeans$Site[ctsMeans$Org=="None"] <- "None" # Need to distinguish these at the site level for plotting purposes (see gen_viz script)
  

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
## Calculate average characteristics for representative peers in same schools
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
  

  # The representative peer group is defined as students who attend the same schools as students in the treatment program.
  # If 60% of treatment kids attend School H, and 40% attend School Y, 
  # the appropriate comparison for the mean of the treatment group is not the citywide average, but .6*School H's avg + .4*School Y's.
  # This is one way of correcting for the fact that partcipants are not broadly representative of the CPS system.
  # The below code calculates these averages both for the full treatment group and for each site.
  
  
 # Calculate school means overall and by grade
  ctsMean_bySch     <- aggregate(calcData[, meanVars], list(schlid),              mean, na.rm = T)
  ctsMean_bySchGr   <- aggregate(calcData[, meanVars], list(schlid, fGradeLvl),   mean, na.rm = T)
  colnames(ctsMean_bySch)[1]    <- "Site"; ctsMean_bySch$Grade  <- "All"; 
  colnames(ctsMean_bySchGr)[1]  <- "Site"; colnames(ctsMean_bySchGr)[2] <- "Grade"
  
 # Get school %s by population and treatment status
  SchProp_bySite <- prop.table(table(schlid, Site), 2) # Percent of students from each school at each site
  #SchProp_byAny  <- prop.table(table(schlid, fAnyYss), 2) # Percent of students from each school in treatment/control
    ## ERW: Stopping this statement for now, since I dropped fAnyYss.  Not sure the value of this calculation?
    

  
  # ERW stopping here for now and focusing on getting graphs working with the reduced data set. Not sure yet what this data should look like.
  
  # Create function that combines "peer" averages by proportionately weighting school averages
  popPeerAvg <- function(pop, var) {
    validRows <- !is.na(ctsMean_bySch[, var])
    schWgt <- prop.table(table(schlid[myPopn])) # Need a way to guarantee that we get 0 entries
    cbind(s, v, weighted.mean(ctsMean_bySch[validRows, var], p[validRows, s]))
  }
  
 # Define the function to use and process averages
  calcPeerAvgs <- function(myPopn){
  
    
    
    avgCalcs <- t(mapply(PeerAvg_forSV,
                         rep(byVals,  times=length(ctsVars)),
                         rep(ctsVars, each=length(Sites)),
                         schProps) ) # XXX Can this be replaced by a ddply call to any advantage? ... it actually works pretty fast
    rownames(avgCalcs) <- NULL
    avgCalcs.df <- data.frame(avgCalcs)
    colnames(avgCalcs.df) <- c(byVar, "ctsVar", "mean")
    avgCalcs.df$Mean <- as.numeric(levels(avgCalcs.df$mean)[avgCalcs.df$mean]) # Convert factor to numeric
  
    # Transpose the resulting dataframe
    avgCalcs.wide <- cast(avgCalcs.df, Site ~ ctsVar, value = "mean")
    return(avgCalcs.wide)
  }
  
  peersBySite <- calcPeerAvgs(byVar = )
  ## Input needs to be some identified population, where we can do a probability distribution of school attendance.
  # This can be everyone within a given org (e.g. bYMCA == 1), or a given Site (fYssSite=="mySite"). Previously, the prop.table, was done across one of these indices,
  # which presumes a single, mutually exclusive index. 
  # Instead, we could do a table for a subset defined by a given condition. 
  
  
  avgCalcs.wide$Site <- paste0(avgCalcs.wide$Site,"\nSch-Based Peers")
  avgCalcs.wide$Grade <- "All"
  
  
  
  ##################################################################
  # Original code is here
  
  
  # Apply this function to for all variables at the site level
    
  Sites <- levels(fYssSite)
  
  PeerAvgs_bySite <- t(mapply(PeerAvg_forSV,
                              rep(Sites,  times=length(ctsVars)),
                              rep(ctsVars, each=length(Sites))
                              ) ) # XXX Can this be replaced by a ddply call to any advantage?
  rownames(PeerAvgs_bySite) <- NULL
  PeerAvgs_bySite <- data.frame(PeerAvgs_bySite)
  colnames(PeerAvgs_bySite) <- c("Site", "ctsVar", "mean")
  PeerAvgs_bySite$Mean <- as.numeric(levels(PeerAvgs_bySite$mean)[PeerAvgs_bySite$mean]) # Convert factor to numeric

  # Transpose the resulting dataframe
  
  ctsMean_bySiteSchPeer <- cast(PeerAvgs_bySite, Site ~ ctsVar, value = "mean")
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

