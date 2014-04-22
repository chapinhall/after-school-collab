#---------------------------------------------#
#---------------------------------------------#
# CREATE DESCRIPTIVE SUMMARY STATISTICS	      # 
#                                             #
# Authors: Nick Mader, Ian Matthew Morey,     # 
#		    and Emily Wiegand		                  #  
#---------------------------------------------#
#---------------------------------------------#

#------------------------------------------------------
## Set up workspace and designate/update file locations
#------------------------------------------------------

  rm(list=ls())
  try(setwd("/projects/Integrated_Evaluation_Youth_Support_Services"))
  try(setwd("H:/Integrated Evaluation Project for YSS Providers"))
  
  dataPath <- "./data/preprocessed-data/" # File path to locate and save data

  library("reshape")
  library("plyr")
  library("data.table")
  ds <- function(x){ deparse(substitute(x))}
  "%&%" <- function(...) { paste(..., sep="")}
  
  useScrambledData <- 0

#--------------------
## Load selected data
#--------------------

  try(detach(myData), silent=T)

  if (useScrambledData==1) {
    load(dataPath %&% "Scram.Rda")
    myData <- Scram
    rm(Scram)
  } else {
  	load(dataPath %&% "CpsAcsYss_PP13.Rda")
    myData <- CpsAcsYss_PP13
    rm(CpsAcsYss_PP13)
  } 

## XXX Short term edits to make data set match eventual result -- NSM: this should be handled by bringing in the program-by-time data construction coming out of combine-data.r
## NOTE THAT THESE NEED TO BE DIFFERENT WITH SCRAMBLED DATA.
  myData$Org[myData$fAnyYss=='YMCA'] <- "YMCA"
  myData$Org[is.na(myData$Org)] <- "None"
  myData$Site[myData$fAnyYss=='YMCA'] <- as.character(myData$fYssSite[myData$fAnyYss=='YMCA'])
  #attach(myData)  
  
#-------------------------------
## Select variables to summarize
#-------------------------------

  # Create dummy variables of catVar values to be treated as continuous vars
  catVars <- c("mathpl", "readpl", "fGradeLvl")
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
  
  cNames <- colnames(myData)
  subVars <- c("sid", "Org", "Site", "fGradeLvl", "fGradeGrp_K5_68_HS", "SchYear", "schlid","mathss", "readss", "mathgain", "readgain", "Pct_Attend", "bOnTrack", "bHsGrad",
                grep("bRace",      cNames, value=T),
                grep("bLunch",     cNames, value=T),
                grep("Tract_",     cNames, value=T),
                grep("fGradeLvl_", cNames, value=T),
                grep("mathpl_",    cNames, value=T),
                grep("readpl_",    cNames, value=T)) 
  calcData <- myData[, subVars]
  #detach(myData)
  
  # Also, remove obs that didn't match to CPS and are NA for basically all the variables we will use for calculations
  calcData <- calcData[!is.na(calcData$schlid),]
  
  ### ERW: We need to look into who these IDs are and how they got mixed into the dataset.  They should theoretically be removed during
  ## data preparation unless there is a reason to keep them in.  I'm comfortable to drop now since NAs get dropped from all calcs.  But 
  ## some might have a few values?  How does that happen?  Fundamental question: are these people in the CPS master file?


  
#------------------------------  
#------------------------------
## Calculate summary statistics
#------------------------------
#------------------------------

  calcData$All <- "All" # This creates a single "category" to allow us to specify an "all-in" calculation rather than a subset
  attach(calcData)
  
  descVars <- c("mathss", "readss", "mathgain", "readgain", "Pct_Attend",
                grep("bRace",     cNames, value=T),
                grep("bLunch",    cNames, value=T),
                grep("Tract_",    cNames, value=T),
                grep("GradeLvl_", cNames, value=T),
                grep("mathpl_",   cNames, value=T),
                grep("readpl_",   cNames, value=T)) 
  
  ## Establish a function to calculate mean, variance, N and se by arbitrary subgroups

  runStats <- function(data, byVars, myVars, name.cols = FALSE){
    myMeans <- aggregate(data[, myVars], list(data[, byVars]), mean, na.rm=T)
    myS2    <- aggregate(data[, myVars], list(data[, byVars]), function(x) var(x, na.rm=T))
    myNs    <- aggregate(data[, myVars], list(data[, byVars]), function(x) sum(!is.na(x)))
    myMeans$stat <- "mean"; myS2$stat <- "var"; myNs$stat <- "N"
    stack <- rbind(myMeans, myS2, myNs)
    
    if(name.cols == TRUE){
      byNames <- byVars
    } else {
      byNames <- paste0("Group.", 1:length(byVars))
    }
    colnames(stack)[1:length(byNames)] <- byNames
    
    longstack <- melt(stack, id=c(byNames, "stat"))
    out <- cast(longstack, as.formula(paste0(paste(c(byNames, "variable"), collapse="+"), "~ stat")))
    out$sd <- sqrt(out$var)
    out$var_mean <- out$var / out$N
    out$se_mean <- sqrt(out$var_mean)
    return(out)
  }

  stats.byOrg     <- runStats(data = calcData, byVars = c("Org", "All",  "All"               ), myVars = descVars)
  stats.bySite    <- runStats(data = calcData, byVars = c("Org", "Site", "All"               ), myVars = descVars)
  stats.byOrgGr   <- runStats(data = calcData, byVars = c("Org", "All",  "fGradeLvl"         ), myVars = descVars)
  stats.bySiteGr  <- runStats(data = calcData, byVars = c("Org", "Site", "fGradeLvl"         ), myVars = descVars)
  stats.byOrgGrp  <- runStats(data = calcData, byVars = c("Org", "All",  "fGradeGrp_K5_68_HS"), myVars = descVars)
  stats.bySiteGrp <- runStats(data = calcData, byVars = c("Org", "Site", "fGradeGrp_K5_68_HS"), myVars = descVars)
  
  stats.byOrgSiteGr <- rbind(stats.byOrg, stats.bySite, stats.byOrgGr, stats.bySiteGr, stats.byOrgGrp, stats.bySiteGrp)
  colnames(stats.byOrgSiteGr)[1:3] <- c("Org", "Site", "Grades")
  
  # ERW: will need to add school year as another unit of analysis
  
  # NSM: Experimenting with data.table, which supposedly has performance advantages
      #DT <- data.table(ctsMean)
      #ctsMean_alt <- DT[,mean("mathss"), by = fOrg]
  
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

    stats.bySch    <- runStats(data = calcData, byVars = c("schlid", "All"               ), myVars = descVars)
    stats.bySchGr  <- runStats(data = calcData, byVars = c("schlid", "fGradeLvl"         ), myVars = descVars)  
    stats.bySchGrp <- runStats(data = calcData, byVars = c("schlid", "fGradeGrp_K5_68_HS"), myVars = descVars)
    
    stats.bySchGr <- rbind(stats.bySch, stats.bySchGr, stats.bySchGrp)
    colnames(stats.bySchGr)[1:2] <- c("Sch", "Grade")  
  
  # Establish function to ensure consistent ordering of schools when we perform
  # NSM: this may not be strictly necessary since the aggregate() function in the runStats function, and the table()
  #      function below may likely ensure the same sorting of the "schlid" variable

    matchVals <- function(myList, findList){
      unlist(lapply(myList, function(x) which(findList %in% x)))
    }
  
  # Establish a function to calculate weighted statistics. This uses inner products to apply the weights and sum. Note that
  # while the means and N calculations are straight weighted means, the variance calculation requires more special treatment.
  # If a weighted mean characteristics for schools A and B, with weights c and d, is calculated as mu = c*\bar{x_A} + d*\bar{x_B}
  #    then the variance is 
  #         var(c*\bar{x_A} + d*\bar{x_B}) = c^2*s2_{\bar{X_A}} + d^2*s2_{\bar{X_A}}
  #    where the s2_{\bar{X_@}} is the variance of the mean statistics (in contrast to the variance of the X's). This is
  #    the variance that was calculated in the runStats() function above.
  
  # NSM: we should generalize this to levels besides site, in order to specify peers at the organization-wide level, or
  #      within grades and/or years.
  peerStats.fn <- function(org, site, vars){
  
    # Identify the subset of individual-level file to be found
    fn.getSubset <- function(subvar, subval){
      if (subval == "All"){
        return(calcData[, subvar] == calcData[, subvar]) # Basically, a vector of "TRUE" values
      } else {
        return(calcData[, subvar] == subval) # Identify rows with specifically the right values
      }
    }
    mySchs <- calcData[getSubset("Org", org) & getSubset("Site", site) & getSubset("Grade", grade), "schlid"]
    # XXX To double-check: the original function got both "Org" and "Non-Org" records. I don't believe that's we need that
    # subset, and can just focus on the specific organization in question. Double-check on this
    
    # Calculate proportions data for given org
      schP <- prop.table(table(mySchs))
      schNames <- rownames(schP)
    
    # Create school-level variables
      schMeans <- stats.bySchGr[matchVals(stats.bySchGr$schlid, schNames) & stats.bySchGr$variable %in% vars,
                             c("schlid", "variable", "mean", "N", "var_mean")]
      mSch.mean     <- as.matrix(cast(schMeans, schlid ~ variable, value = "mean"))
      mSch.n        <- as.matrix(cast(schMeans, schlid ~ variable, value = "n"))
      mSch.var_mean <- as.matrix(cast(schMeans, schlid ~ variable, value = "var_mean"))
        # , by.row = T ... don't believe that this is needed with "as.matrix()"
    
    # Calculate weighted summary statistics
      nSch <- nrow(mSch.mean)
      nVars <- length(vars)
    
      mu  <- t(schP)   %*% mSch.mean
      n   <- t(schP)   %*% mSch.n
      var <- t(schP^2) %*% mSch.var_mean
  
      out <- data.frame(Org = org,
                        Site = paste0(site, "\nSch-Based Peers"),
                        Grade = grade,
                        variable = vars,
                        mean = t(mu), var_mean = t(v), se_mean = t(sqrt(v)))
    return(out)
  
  }
  
  #--------------------------------------------------------
  ## Call the peer averages function, and save and close up
  #--------------------------------------------------------
  
  org        <- "YMCA"
  subsetVars <- c("Org", "Site", "Grade")
  
  subsetVals <- unique(stats.byOrgSiteGr[, subsetVars])
  subsetVals <- subsetVals[!grepl("Non-", subsetVals$Org),]

  stats.peers <- mapply(peerStats.fn, org = subsetVals$Org, site = subsetVals$Site, grade = subsetVals$Grade, vars = descVars)
  # XXX Need to look at the form that this takes

  #ctsMean_bySitePeer <- as.data.frame(t(mapply(PeerAvg_bySite, site = rep(orgsitelist, each = length(meanVars)), var = meanVars, org = org)))
  #colnames(ctsMean_bySitePeer) <- c("Org","Site", "Variable", "Mean")
  
  # XXX: Why are there NaN's here?  (And elsewhere in calculated means).  Need to track these anomalies down and check data.
  # TBD: peer based averages by grade, flexibility by year. Could be done with just a flexible call to the mapply
  #   Pseudo code:
  #   1. in a given call, we give a series of variables and values (e.g. Org = "YMCA", Site = "All")
  #   2. this is converted to a subset of observations in the original data set, and a subset of calculations in the school
  #      data. The former requires smart handling since an "All" specification means no subsetting on that variable. The lattter
  #      can handle things more naturally, because "All" truly is a value that can be directly queried.
  #   3. Conditional on having this subset, the existing code will deliver the right calculations.
  #   4. Will need some way to tie together the output, based on how the information was fed in.
  
  # Implementing:
  # 1. Will feed this in using an mapply() call for the current code
  # 2. Mocking up code:
  
  
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

