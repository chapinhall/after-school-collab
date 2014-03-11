#---------------------------------------------#
#---------------------------------------------#
# CREATE DESCRIPTIVE SUMMARY STATISTICS	      # 
#     FOR SYNTHETIC ANALYSIS (YEAR, oRG)      #
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
  
## Load selected data

    load(paste0(dataPath,"ScramPlusYrOrg.Rda"))
    myData <- Scram2
    rm(Scram2)
    myData$Site <- myData$cYssSite
  
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
  calcVars <- c("sid", "Org", "Site", "Year", "fGradeLvl", "SchYear", "schlid","mathss", "readss", "mathgain", "readgain", "Pct_Attend",
                grep("bRace",      cNames, value=T),
                grep("bLunch",     cNames, value=T),
                grep("Tract_",     cNames, value=T),
                grep("fGradeLvl_", cNames, value=T),
                grep("mathpl_",    cNames, value=T),
                grep("readpl_",    cNames, value=T)) 
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
  ctsMean_byOrgYr    <- aggregate(calcData[, meanVars], list(Org, Year),                  mean, na.rm = T)
  ctsMean_bySiteYr   <- aggregate(calcData[, meanVars], list(Org, Site, Year), mean, na.rm = T)
  
  # XXX testing replacement of these calculations with data.table calculations.
  #cd.dt <- data.table(calcData, key = Org)
  #ctsMean_byOrg.dt <- cd.dt[, ]
    
  colnames(ctsMean_byOrg)[1]    <- "Org" 
  ctsMean_byOrg$Site[ctsMean_byOrg$Org=="None"] <- "Non-Participants"
  ctsMean_byOrg$Site[ctsMean_byOrg$Org!="None"] <- paste0("All ",ctsMean_byOrg$Org[ctsMean_byOrg$Org!="None"]," Sites")
  ctsMean_byOrg$Grade  <- "All Grades"
  ctsMean_byOrg$Year   <- "All Years"
  
  colnames(ctsMean_bySite)[1]   <- "Org"
  colnames(ctsMean_bySite)[2]   <- "Site"
  ctsMean_bySite$Grade          <- "All Grades"
  ctsMean_bySite$Year            <- "All Years"
  
  colnames(ctsMean_byOrgGr)[1]  <- "Org"
  ctsMean_byOrgGr$Site[ctsMean_byOrgGr$Org=="None"] <- "Non-Participants"
  ctsMean_byOrgGr$Site[ctsMean_byOrgGr$Org!="None"] <- paste0("All ",ctsMean_byOrgGr$Org[ctsMean_byOrgGr$Org!="None"]," Sites")
  colnames(ctsMean_byOrgGr)[2]  <- "Grade"
  ctsMean_byOrgGr$Year          <- "All Years"
  
  colnames(ctsMean_bySiteGr)[1] <- "Org"
  colnames(ctsMean_bySiteGr)[2] <- "Site"
  colnames(ctsMean_bySiteGr)[3] <- "Grade"
  ctsMean_bySiteGr$Year         <- "All Years"
  
  colnames(ctsMean_byOrgYr)[1]    <- "Org" 
  colnames(ctsMean_byOrgYr)[2]    <- "Year"
  ctsMean_byOrgYr$Site[ctsMean_byOrgYr$Org=="None"] <- "Non-Participants"
  ctsMean_byOrgYr$Site[ctsMean_byOrgYr$Org!="None"] <- paste0("All ",ctsMean_byOrgYr$Org[ctsMean_byOrgYr$Org!="None"]," Sites")
  ctsMean_byOrgYr$Grade  <- "All Grades"
  
  colnames(ctsMean_bySiteYr)[1]   <- "Org"
  colnames(ctsMean_bySiteYr)[2]   <- "Site"
  colnames(ctsMean_bySiteYr)[3]   <- "Year"
  ctsMean_bySiteYr$Grade          <- "All Grades"
  
  # NSM: Experimenting with data.table, which supposedly has performance advantages
      #DT <- data.table(ctsMean)
      #ctsMean_alt <- DT[,mean("mathss"), by = fOrg]

  
  ## Combine summary statistics across different measures into one data frame
  
  ctsMeans <- rbind(ctsMean_byOrg, ctsMean_byOrgGr, ctsMean_bySite, ctsMean_bySiteGr, ctsMean_byOrgYr, ctsMean_bySiteYr)
  ctsMeans$Org  <- as.character(ctsMeans$Org)
  ctsMeans$Site <- as.character(ctsMeans$Site)
  
  ## Rotate from wide to long - easier for plotting
  ctsMeansLong <- reshape(ctsMeans, direction = 'long', varying = list(names(ctsMeans)[2:51]), v.names = "Mean", timevar = "Variable", times = (names(ctsMeans)[2:51]))
  ctsMeansLong <- subset(ctsMeansLong, select = -c(id))
  
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
  
 # Get school %s by population and treatment status - ERW: if below is working right this can be removed.
  SchProp_bySite <- prop.table(table(schlid, Site), 2) # Percent of students from each school at each site
  SchProp_byAny  <- prop.table(table(schlid, Org), 2) # Percent of students from each school in org/control

  
  # Develop functions to get peer information for a given org and site
  wgtedmean <- function(school, proportions, means) {
    meanval <- proportions[,c(school)] * means[,c(school)]
    return(meanval)
  }
  
  PeerAvg_bySite <- function(site, org, var) {
    restData <- calcData[calcData$Org %in% c("None", org),]
    schoolprop <- prop.table(table(restData$schlid, restData$Site), 2)
    
    sitedata <- schoolprop[,site]
    siteprops <- sitedata[sitedata > 0]
    schools <- paste0("s",names(siteprops))
    names(siteprops) <- schools
    df <- as.data.frame(t(as.data.frame(siteprops)))
    
    schooldata <- ctsMean_bySch[,c("Site",var)]
    schoolmeans <- schooldata[,2]
    schools2 <- paste0("s",schooldata$Site)
    names(schoolmeans) <- schools2
    df2 <- as.data.frame(t(as.data.frame(schoolmeans)))
    
    means <- lapply(colnames(df), wgtedmean, df, df2)
    peeravg <- sum(unlist(means))
    results <- c(org, site, var, peeravg)
    return(results)
  }
  
  
  # Using these to generate peer means by site (done for one org for now, but easily generalized across orgs)
  
  org <- "YMCA"
  orgsites <- calcData$Site[calcData$Org==org]
  orgsitelist <- unique(orgsites)[!is.na(unique(orgsites))]
  ctsMean_bySitePeer <- as.data.frame(t(mapply(PeerAvg_bySite, site = rep(orgsitelist, each = length(meanVars)), var = meanVars, org = org)))
  colnames(ctsMean_bySitePeer) <- c("Org","Site", "Variable", "Mean")
  ctsMean_bySitePeer$Site <- paste0(ctsMean_bySitePeer$Site,"\nSch-Based Peers")
  
  # XXX: Why are there NaN's here?  (And elsewhere in calculated means).  Need to track these anomalies down and check data.
  # TBD: peer based averages by grade, flexibility by year.
  
  ctsMean_bySitePeer$Grade <- "All Grades"
  ctsMean_bySitePeer$Year <- "2012-13"
  
  
  
  ##################################################################################################################
  
  ###### ERW: I can't follow anything below this - 
  ######         functions reference a lot of variables that are not defined and not given as inputs...?
  ######      Regardless, I think most of it is replaced by the above.
  
  
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
    return(avgCalcs.df)
    
    # Transpose the resulting dataframe
    #avgCalcs.wide <- cast(avgCalcs.df, Site ~ ctsVar, value = "mean")
    #return(avgCalcs.wide)
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
  
  ctsMeans <- rbind(ctsMeansLong, ctsMean_bySitePeer)
  
  if (useScrambledData==1) { 
    save(ctsMeansLong,    file = paste0(dataPath,"ctsMeans","_DEMO.Rda"))
  } else {  
    save(ctsMeansLong,    file = paste0(dataPath,"ctsMeans.Rda"))
    save(myData, file = paste0(dataPath, "subset_CpsYss_PP13.Rda"))
  }

