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
  try(setwd("/projects/Integrated_Evaluation_Youth_Support_Services"), silent=T)
  try(setwd("H:/Integrated Evaluation Project for YSS Providers"), silent=T)
  
  dataPath <- "./data/preprocessed-data/" # File path to locate and save data

  library("reshape")
  #library("plyr")
  #library("data.table")
  ds <- function(x){ deparse(substitute(x))}
  "%&%" <- function(...) { paste(..., sep="")}
  paste0 <- function(...) { paste(..., sep="")}
  
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
  myData$Site[myData$fAnyYss=='YMCA'] <- as.character(myData$fShortSite[myData$fAnyYss=='YMCA'])
  #attach(myData)  
  
#-------------------------------
## Select variables to summarize
#-------------------------------

  cNames <- colnames(myData)
  descVars <- c("mathss", "readss", "mathgain", "readgain", "Pct_Attend", "bOnTrack", "bHsGrad", "bGender_Male", "bGender_Female",
                grep("bRace",     cNames, value=T),
                grep("bLunch",    cNames, value=T),
                grep("Tract_",    cNames, value=T),
                grep("GradeLvl_", cNames, value=T),
                grep("mathpl_",   cNames, value=T),
                grep("readpl_",   cNames, value=T),
                grep("disab_",    cNames, value=T),
                grep("_AgreeStronglyAgree", cNames, value=T),
                grep("_MostlyVeryLikeMe",   cNames, value=T),
                grep("EXPE_",               cNames, value=T),
                "HWTM_Week_Ge6") 

  # Create dummy variables of catVar values to be treated as continuous vars
  catVars <- c("mathpl", "readpl", "fGradeLvl")
  for (myVar in catVars) {
    cVar <- as.character(myData[, myVar])
    for(val in unique(cVar)){
      if (!is.na(val)) {
        newVar <- paste(myVar, val, sep="_")
        myData[, newVar] <- ifelse(cVar==val, 1, 0)
        descVars <- c(descVars, newVar)
      }
    }
  } # XXX There's likely a more elegant way to do this. Note that model.matrix(~0+var) drops observations with NAs, returning a vector of shorter length (which, at this stage, we don't want)
  
  subVars <- c("sid", "Org", "Site", "fGradeLvl", "fGradeGrp_K5_68_HS", "SchYear", "schlid", descVars) 
  calcData <- myData[, subVars]
  #detach(myData)
  
  # Also, remove obs that didn't match to CPS and are NA for basically all the variables we will use for calculations
  calcData <- calcData[!is.na(calcData$schlid), ]
  
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
  
  ## Establish a function to calculate mean, variance, N and se by arbitrary subgroups

  # Audit values
  # data = calcData; byVars = c("Org", "All",  "All"); myVars = descVars
  runStats <- function(data, byVars, myVars, name.cols = FALSE){
    
    byList <- lapply(byVars, function(x) data[, x])
    myMeans <- aggregate(data[, myVars], byList, mean, na.rm=T)
    myS2    <- aggregate(data[, myVars], byList, function(x) var(x, na.rm=T))
    myNs    <- aggregate(data[, myVars], byList, function(x) sum(!is.na(x)))
    myMeans$stat <- "mean"; myS2$stat <- "var"; myNs$stat <- "n"
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
    out$var_mean <- out$var / out$n
    out$se_mean <- sqrt(out$var_mean)
    return(out)
  }

  stats.byOrg     <- runStats(data = calcData, byVars = c("Org", "All",  "All"               ), myVars = descVars)
  stats.bySite    <- runStats(data = calcData, byVars = c("Org", "Site", "All"               ), myVars = descVars)
  stats.byOrgGr   <- runStats(data = calcData, byVars = c("Org", "All",  "fGradeLvl"         ), myVars = descVars)
  stats.bySiteGr  <- runStats(data = calcData, byVars = c("Org", "Site", "fGradeLvl"         ), myVars = descVars)
  stats.byOrgGrp  <- runStats(data = calcData, byVars = c("Org", "All",  "fGradeGrp_K5_68_HS"), myVars = descVars)
  stats.bySiteGrp <- runStats(data = calcData, byVars = c("Org", "Site", "fGradeGrp_K5_68_HS"), myVars = descVars)
  
  stats.org <- rbind(stats.byOrg, stats.bySite, stats.byOrgGr, stats.bySiteGr, stats.byOrgGrp, stats.bySiteGrp)
  colnames(stats.org)[1:3] <- c("Org", "Site", "Grade")
  
  # ERW: will need to add school year as another unit of analysis
  
  # NSM: Should experiment with data.table, which supposedly has performance advantages
  

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
    
    stats.sch <- rbind(stats.bySch, stats.bySchGr, stats.bySchGrp)
    colnames(stats.sch)[1:2] <- c("Sch", "Grade")  
  

  #-------------------------
  ## Create Helper Functions
  #-------------------------

    #---------
    # Create a function to return All records when "All" is specified (even though the value "All" does not appear).
    #---------
  
      # XXX Could also be replaced with conditions of e.g. "grepl(org, calcData$Org)" where "*" can be passed to org
        # ...except that some variables like Grade need special handling
      
      getSubset <- function(subvar, subval){
        if (subval == "All"){
          return(calcData$All == calcData$All) # XXX Basically, an inelegant way to return a vector of "TRUE" values
        } else if(subvar == "Grade") {
          
          # Separate handling for grade ranges versus individual grade levels
          if (subval %in% c("K-5", "6-8", "HS")){
            return(calcData$fGradeGrp_K5_68_HS == subval)
          } else {
            return(calcData$fGradeLvl == subval)
          }
          
        } else {
          return(calcData[, subvar] == subval) # Identify rows with specifically the right values
        }
      }
  
  #-------------------------------
  ## Establish Peer Stats Function
  #-------------------------------

    # Establish a function to calculate weighted statistics. This uses inner products to apply the weights and sum. Note that
    # while the means and N calculations are straight weighted means, the variance calculation requires more special treatment.
    # If a weighted mean characteristics for schools A and B, with weights c and d, is calculated as mu = c*\bar{x_A} + d*\bar{x_B}
    #    then the variance is 
    #         var(c*\bar{x_A} + d*\bar{x_B}) = c^2*s2_{\bar{X_A}} + d^2*s2_{\bar{X_A}}
    #    where the s2_{\bar{X_@}} is the variance of the mean statistics (in contrast to the variance of the X's). This is
    #    the variance that was calculated in the runStats() function above.
  
    peerStats.fn <- function(myProps, myVar, mySchStats){
      
      stats.props <- merge(mySchStats, myProps, by = "Sch")  # merge proportions into school-level stats data
      stats.props <- stats.props[!is.na(stats.props$mean), ] # remove school rows with missing calculations - XXX Could be refactored to be faster if dropping these all ahread of time
      stats.props$prop <- stats.props$prop / sum(stats.props$prop) # inflate the proportions to reflect any drops of schools with missing values
      stats.props$prop2 <- stats.props$prop^2 # This weight is necessary for variance calculations, noted above
      
      mu <- weighted.mean(stats.props$mean, stats.props$prop, na.rm = T)
      n  <- weighted.mean(stats.props$n, stats.props$prop, na.rm = T)
      s2 <- sum(stats.props$var_mean * stats.props$prop2)
      
      out <- data.frame(variable = myVar, mean = mu, n = n, var_mean = s2, se_mean = sqrt(s2))
      return(out)
    
    }
  
  #--------------------------------------------------------
  ## Call the peer averages function, and save and close up
  #--------------------------------------------------------
  
    subsetVars <- c("Org", "Site", "Grade")
    runList <- unique(stats.org[, subsetVars])
    runList <- runList[!grepl("None", runList$Org),]
  
    #stats.peers <- mapply(peerStats.fn, runList$Org, runList$Site, runList$Grade)

    # Attempting an alternate strategy to the mapply(), which doesn't like being returned many data frames
    # Initialize output data frame
    rl <- runList
    nRuns <- nrow(rl)
    stats.peers <- data.frame(Org = rep("", nRuns), Site = rep("", nRuns), Grade = rep("", nRuns), variable = rep("", nRuns),
                              mean = rep(0.0, nRuns), n = rep(0.0, nRuns), var_mean = rep(0.0, nRuns), se_mean = rep(0.0, nRuns))
    for (i in 1:nRuns){
      
      # Audit values
      #org = "YMCA"; site = "High Ridge YMCA"; grade = "All"
      #org = "YMCA"; site = "All"; grade = "All"
      org <- rl$Org[1]; site <- rl$Site[i]; grade <- rl$Grade[i]
      
      mySchs <- calcData[getSubset("Org", org) & getSubset("Site", site) & getSubset("Grade", grade), "schlid"]
      schP <- data.frame(prop.table(table(mySchs)))
      colnames(schP) <- c("Sch", "prop")
      
      # Get school-level statistics for schools involving in the current calculation
      schStats <- stats.sch[stats.sch$Sch %in% schP$Sch &
                            stats.sch$Grade == grade &
                            stats.sch$variable %in% descVars,
                            c("Sch", "variable", "mean", "n", "var_mean")]
      
      # Set up header for identifying calculations
      outHeader <- data.frame(Org = rl$Org[i], Site = rl$Site[i], Grade = rl$Grade[i])
      
      print(paste("org =", org, ", site =", site, ", Grade =", grade))
      
      # Run calculations variable by variable since different variables by school may have different numbers of NAs to drop
      for (v in descVars){
        
        peerStats <- peerStats.fn(myProps = schP, myVar = v, mySchStats = schStats[schStats$variable == v, ])
        stats.peers <- rbind(stats.peers, 
                             cbind(outHeader, peerStats))
        
      }
    }

    stats.peers$Site <- paste0(stats.peers$Site, " Sch-Based Peers")

    descStats    <- rbind(stats.org[, colnames(stats.peers)], stats.peers)
    descStats$plusminus <- descStats$se_mean * 1.96
    descStats$id <- paste(descStats$Org, descStats$Site, descStats$Grade, descStats$variable, sep="_")
    
    save(descStats, file = paste0(dataPath, "descStats.Rda"))
    write.csv(descStats, file = paste0(dataPath, "descStats.csv"))

# XXX: Why are there NaN's here?  (And elsewhere in calculated means).  Need to track these anomalies down and check data.