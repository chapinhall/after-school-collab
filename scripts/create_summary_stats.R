#---------------------------------------------#
#---------------------------------------------#
# CREATE DESCRIPTIVE SUMMARY STATISTICS	      # 
#                                             #
# Authors: Nick Mader, Ian Matthew Morey,     # 
#		    Emily Wiegand, Kim Foley              #  
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
  library("data.table")
  source("./scripts/create_summary_stats_helper_functions.R")
  
  useScrambledData <- 0

#--------------------
## Load selected data
#--------------------

  try(detach(myData), silent=T)

  if (useScrambledData==1) {
    load(dataPath %&% "Scram.Rda")
    myData <- Scram
    rm(Scram)
    orglist <- c("Org Alpha", "Org Beta")
  } else {
  	load(dataPath %&% "EnrCpsAcs.Rda")
    myData <- EnrCpsAcs
    rm(EnrCpsAcs)
    
    orglist <- c("YMCA", "ASM", "CHASI", "UCAN") # UPDATE HERE AS WE HAVE DATA FOR NEW PARTNERS TO INCLUDE IN TOTALS.
    
  } 

  
#-------------------------------
## Select variables to summarize
#-------------------------------

  # Create individual variance statistics for My Voice, My School variables
#     for (m in c("AcadEng", "Comm", "EmHealth", "Parent", "Peer", "Study", "Safety", "Connect", "Respect")){ # Not yet in the data set... 
#       seVar <- paste0("MVMS_", m, "_se")
#       s2Var <- paste0("MVMS_", m, "_s2")
#       myData[, s2Var] <- myData[, seVar]^2
#       myData <- myData[, -which(colnames(myData) %in% seVar)]
#     }

  # Build a list of descriptive variables
  cNames <- colnames(myData)
  descVars <- c("bOnTrack", "bGender_Male", "bGender_Female", "bIEP", "iep_test", "lep_test", "lunch_test",
                grep("Pct_Attend",cNames, value=T),
                grep("isat_",     cNames, value=T),
                grep("explore_",  cNames, value=T),
                grep("plan_",     cNames, value=T),
                grep("psae_",     cNames, value=T),
                grep("bRace",     cNames, value=T),
                grep("bLunch",    cNames, value=T),
                grep("Tract_",    cNames, value=T),
                grep("MVMS_.+[^se]$", cNames, value=T)) # This gets anything that starts with "MVMS", and doesn't end with "se"
                #grep("MVMS_",     cNames, value=T)) # "bHsGrad", 

  # Create dummy variables of catVar values to be treated as continuous vars
  catVars <- c("isat_mathpl", "isat_readpl", "fGradeLvl")
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
  descVars <- descVars[!(descVars %in% c("isat_mathpl", "isat_readpl"))] # Remove character variables
  
  subVars <- c("sid", "org", "Collab", orglist, "site", "program", "fGradeLvl", "fGradeGrp_K5_68_HS", "year", "schlid", descVars) 
  calcData <- myData[, subVars]
  rm(myData)
  
#------------------------------  
#------------------------------
## Calculate summary statistics
#------------------------------
#------------------------------  

  # Run Calculations across various summary levels
    print("Initiating calculations for full collaborative")
    stats.byCollab    <- runStats(byOrgVar = "Collab", byYearVar = "year")
    stats.byCollabGr  <- runStats(byOrgVar = "Collab", byYearVar = "year", byGradeVar = "fGradeLvl")
    stats.byCollabGrp <- runStats(byOrgVar = "Collab", byYearVar = "year", byGradeVar = "fGradeGrp_K5_68_HS")
    stats.collab      <- rbind(stats.byCollab, stats.byCollabGr, stats.byCollabGrp)    
  
    stats.org          <- stats.collab
    rm(stats.collab, stats.byCollab, stats.byCollabGr, stats.byCollabGrp)

    # Run statistics for each partner organizations
    for (myOrg in orglist){
      print(paste0("Initiating calculations for ", myOrg))
      stats.byOrg     <- runStats(byOrgVar = myOrg, byYearVar = "year")
      stats.byOrgGrp  <- runStats(byOrgVar = myOrg, byYearVar = "year", byGradeVar = "fGradeGrp_K5_68_HS")
      stats.byOrgGr   <- runStats(byOrgVar = myOrg, byYearVar = "year", byGradeVar = "fGradeLvl")
      stats.bySite    <- runStats(byOrgVar = myOrg, byYearVar = "year", bySiteVar = "site")
      stats.byProg    <- runStats(byOrgVar = myOrg, byYearVar = "year", byProgramVar = "program")
      stats.org       <- rbind(stats.org, stats.byOrg, stats.bySite, stats.byProg, stats.byOrgGrp, stats.byOrgGr) #stats.bySiteGr, stats.bySiteGrp, stats.byProgGr, stats.byProgGrp)
    }
    
  colnames(stats.org)[1:5] <- c("org", "site", "program", "grade", "year")
  
  rm(stats.byOrg, stats.byOrgGrp, stats.byOrgGr, stats.bySite, stats.byProg) # stats.bySiteGr, stats.bySiteGrp, stats.byProgGr, stats.byProgGrp)

  
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
## Calculate average characteristics for representative peers in same schools
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
  
  # The representative peer group is defined as students who attend the same schools as students in the treatment program.
  # If 60% of treatment kids attend School H, and 40% attend School Y, the appropriate comparison for the mean of the served
  # population is not the citywide average, but .6*School H's avg + .4*School Y's. 
  # This is one way of addressing for the fact that partcipants are not broadly representative of the CPS system.
  # The below code calculates these averages both for the served population in aggregate and for each site.  
  
  #-------------------------------
  ## Establish Peer Stats Function
  #-------------------------------

    # Establish a function to calculate weighted statistics. This uses the weighted.mean function to apply the weights and sum. Note that
    # while the means and N calculations are straight weighted means, the variance calculation requires more special treatment.
    # If a weighted mean characteristics for schools A and B, with weights c and d, is calculated as mu = c*\bar{x_A} + d*\bar{x_B}
    #    then the variance is 
    #         var(c*\bar{x_A} + d*\bar{x_B}) = c^2*s2_{\bar{X_A}} + d^2*s2_{\bar{X_A}}
    #    where the s2_{\bar{X_@}} is the variance of the mean statistics (in contrast to the variance of the X's). This is
    #    the variance that was calculated in the runStats() function above.
  
  #--------------------------------------------------------
  ## Call the peer averages function, and save and close up
  #--------------------------------------------------------
  
    subsetVars <- c("org", "site", "program", "grade", "year")
    runList <- unique(stats.org[, subsetVars]) # Necessary because stats.org is one line per variable
    rl <- runList[!grepl("Non", runList$org),] # Remove all non-org runs, so that we only calculate school-based peers for served populations

    # XXX Could refactor this for loop to run as an lapply() instead by first creating a list
    #     from the rows of the rl table of combinations

    nRuns <- nrow(rl)
    stats.peers <- NULL

    for (i in 1:nRuns){
      
      # Audit values
      # org = "YMCA"; site = "High Ridge"; grade = "All"; program = 'All'; year = "2013"
      # org = "YMCA"; site = "All"; grade = "All"
      org <- rl$org[i]; site <- rl$site[i]; program <- rl$program[i]; grade <- rl$grade[i]; year <- rl$year[i];

      myFocals <- calcData[getSubset("org", org) & 
                           getSubset("site", site) &
                           getSubset("program", program) &
                           getSubset("grade", grade) &
                           getSubset("year", year),
                           c("sid", "schlid", descVars)]
      myFocals.dups <- duplicated(myPeers[, c("sid", "schlid"))
      myFocals <- myFocals[!myFocals.dups,]
      
      myPeers <- calcData[calcData$year == year &
                          !(calcData$sid %in% myFocals$sid), c("sid", "schlid", descVars])
      myPeers.dups <- duplicated(myPeers[, c("sid", "schlid")])
      myPeers <- myPeers[!myPeers.dups,]
      myPeers <- data.table(myPeers, key = "schlid")
      
      # Set up header for identifying calculations
      outHeader <- data.frame(org = rl$org[i], site = rl$site[i], program = rl$program[i], grade = rl$grade[i], year = rl$year[i])
      
      print(paste("Performing run ", i, " of ", nRuns, "or ", round(i/nRuns, 3)*100, "% done. Run is for: org =", org, ", site =", site, ", program =", program, ", grade =", grade, ", year =", year))
      
      # Run calculations variable by variable since different variables by school may have different numbers of NAs to drop
      for (v in descVars){
        
        # Get proportions of schools represented by focal youth involved in the calculation
          myFocalSchs <- myFocals$schlid[!is.na(myFocals[, v])]
          schP <- data.frame(prop.table(table(myFocalSchs)))
          colnames(schP) <- c("sch", "prop")
        
        # Calculate characteristics of peers, excluding focal youth
          peerStats_bySch <- runStats(data = myPeers, vars = v, bySiteVar = "schlid") # Note: omission of all byVar arguments uses all observations
        
        # Get weighted average
          peerStat <- peerStats.fn(myProps = schP, myVar = v, mySchStats = peerStats_bySch[peerStats_bySch$variable == v, ])
          stats.peers <- rbind(stats.peers, cbind(outHeader, peerStat))
        
      }
    }

    stats.peers$site <- paste(stats.peers$site, "Sch-Based Peers")

    
## Compile and save output

    descStats    <- rbind(stats.org[, colnames(stats.peers)], stats.peers)
    descStats$plusminus <- descStats$se_mean * 1.96
    descStats$id <- paste(descStats$org, descStats$site, descStats$program, descStats$grade, descStats$year, descStats$variable, sep="_")
    # Suppress calculations for small or invalid calculations
    descStats$plusminus[descStats$n  < 10] <- NA
    descStats$plusminus[descStats$n == NA] <- NA
    descStats$mean[descStats$n  < 10]      <- NA
    descStats$mean[descStats$n == NA]      <- NA

    save(descStats, file = paste0(dataPath, "descStats.Rda"))
    write.csv(descStats, file = paste0(dataPath, "descStats.csv"))
    #descStats <- read.csv(paste0(dataPath, "descStats.csv"))

    combos <- unique(descStats[, c("org", "site", "program", "grade", "year")])
    combos$id <- paste(combos$org, combos$site, combos$program, combos$grade, combos$year, sep="_")
    combos$gradefilter <- ifelse(combos$grade!="All" & (combos$site!="All" | combos$program!="All"), 1, 0)
    rownames(combos) <- NULL

    orgYearCombos <- data.frame(unique(combos[!grepl("Non-", combos$org), c("org", "year")]))
      colnames(orgYearCombos) <- c("org", "year")
    orgCombos <- data.frame(org=unique(orgYearCombos[, c("org")]))

    for (f in c("combos", "orgYearCombos", "orgCombos")){
      d <- get(f); rownames(d) <- NULL
      write.csv(d, file = paste0(dataPath, f %&% ".csv"))
    }
    
    