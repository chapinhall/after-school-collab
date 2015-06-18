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
  
  dataPath <- "./data/constructed-data/" # File path to locate and save data
  #scriptPath <- "~/GitHub/after-school-collab/scripts"
  scriptPath <- "./code"

  library("reshape2")
  #library("plyr")
  library("data.table")
  library("pastecs")
  library("foreach")
  library("doParallel")
  source(paste0(scriptPath, "/create_summary_stats_helper_functions.R"))
  
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
  	load(p0(dataPath, "EnrCpsAcs.Rda"))
    myData <- EnrCpsAcs
    rm(EnrCpsAcs)
    orglist <- grepv("[^Collab|None]", unique(myData$org))
    #orglist <- c("YMCA", "ASM", "CHaA", "UCAN", "CPL", "CHA") # UPDATE HERE AS WE HAVE DATA FOR NEW PARTNERS TO INCLUDE IN TOTALS.
  } 

#---------------------------------------------------------
## Select which program categories to use for calculations
#---------------------------------------------------------
  
  # ASM sites are too detailed and too numerous to be a useful level of summary.
  #   Instead, we will generate "site" calculations based on site type, e.g. "Church", "Community Center", etc.
  myData$site[myData$org == "ASM"] <- myData$sitetype[myData$org == "ASM"]
  myData$bIEP[is.na(myData$bIEP)] <- 0

#-------------------------------
## Select variables to summarize
#-------------------------------

  # Build a list of descriptive variables
  cNames <- colnames(myData)
  descVars <- c("bGender_Male", "bGender_Female", "bIEP", "bEll", "bOnTrack", "promoted_nextyr", "retained_nextyr", 
                "bAlleg", "bAlleg_yr", "bSubstant", "bSubstant_yr", "bFoster", "bRelative", "bInstitution", "bPlacement", "inCYCIS", "bDCFScase",
                grepv("onTimeGrad_",   cNames),
                grepv("Pct_Attend",    cNames),
                grepv("Pct_NonAbsent", cNames),
                grepv("isat_",         cNames),
                grepv("nwea_",         cNames),
                grepv("explore_",      cNames),
                grepv("plan_",         cNames),
                grepv("psae_",         cNames),
                grepv("bRace",         cNames),
                grepv("bLunch",        cNames),
                grepv("Tract_",        cNames),
                grepv("MVMS_.+[^se]$", cNames)) # This gets anything that starts with "MVMS", and doesn't end with "se"
                # "inCYCIS", "bDCFScase", "bFoster", "bRelative", "bInstitution", "bPlacement",

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
  }
  descVars <- descVars[!(descVars %in% c("isat_mathpl", "isat_readpl"))] # Remove character variables

  # XXX Need to revisit where these variables are being generated as a character
  convVars <- c("explore_compss", "plan_compss", "plan_mathss", "plan_readss", "plan_engss", "plan_sciess")
  for (v in convVars){
    myData[, v]  <- as.numeric(myData[, v])  
  }
  
  subVars <- c("sid", "org", "Collab", orglist, "site", "program", "region", "network_org", "Subset_Schools", "Subset_CommAreas", "progtype", "program_cyds", "progtype_cyds",
               "schlname", "fGradeLvl", "fGradeGrp_K5_68_HS", "year", "schlid", descVars) 
  calcData <- myData[, subVars]
  rm(myData)
  

#------------------------------
#------------------------------
## Calculate summary statistics
#------------------------------
#------------------------------

  # Create standard list of slices cuts to calculate
    sliceList <- c("all", "site", "program", "fGradeLvl", "fGradeGrp_K5_68_HS", "bGender_Male", "bLunch_FR") # XXX Later include dosage categories
    allOrgList <- c("Collab", "CPS", orglist)
    allSlices <- cbind(rep(allOrgList, each  = length(sliceList)),
                       rep(sliceList,  times = length(allOrgList)),
                       "year")
    allSlicesComma <- t(sapply(1:nrow(allSlices), function(r) c(allSlices[r, 1], paste(allSlices[r, 2:ncol(allSlices)], collapse = ","))))
      # NSM: we are migrating from org/Non-org calculations, to subsetting just on org, and using "CPS" overall as a benchmark.
      # This is achieved by running the paste in the above line from the 2nd column on, rather than for all columns on the row.

  # Add special orders for organizations
    allSlicesComma <- rbind(allSlicesComma,
                            c("ASM",  "region,year"),
                            c("CHaA", "network_org,year"),
                            c("CHA",  "Subset_CommAreas,year"),
                            c("CHA",  "Subset_Schools,year"),
                            c("UCAN", "progtype,year"),
                            c("UCAN", "progtype_cyds,year"),
                            c("UCAN", "program_cyds,year"))

  # Format runs in data frame, and determine which slices are for 
    dfSliceRuns <- data.frame(allSlicesComma, stringsAsFactors = F)
    colnames(dfSliceRuns) <- c("org", "sliceVars")

    # Remove certain combinations that aren't insightful
    dfSliceRuns <- dfSliceRuns[!(dfSliceRuns$org  == "CPS"             & grepl("(site)|(program)", dfSliceRuns$sliceVars)),]
    dfSliceRuns <- dfSliceRuns[!(dfSliceRuns$org %in% c("UCAN", "CHA") & grepl("site",             dfSliceRuns$sliceVars)),]
    #dfSliceRuns$inclNon <- ifelse(grepl(p0("(", paste(c(withinOrgSlices, "region", "term"), collapse = ")|("),")"), dfSliceRuns$sliceVars), 0, 1)
      # At this point, we are never including "non-org" lines.
     
  # Prepare organization specific files and information
    calcData$all <- 1
    myEnv <- new.env()
    for (org in grepv("[^Collab|CPS]", allOrgList)){
        assign(p0(org, "Data"), calcData[calcData$org == org,], envir = myEnv)
    }
    assign("CollabData", unique(calcData[calcData$Collab == "Collab",]), envir = myEnv)
    assign("CPSData", calcData, envir = myEnv)

  #----------------------------------
  ### Loop calculations using foreach
  #----------------------------------

    statNames <- rownames(stat.desc(calcData[1:1000,]))
    keepStats <- c("mean", "SE.mean", "nbr.val")

    nCores <- 10 # Using fewer than all of the cores to avoid using all resources
    cl <- makeCluster(nCores)
    registerDoParallel(cl)
    getDoParName()
    getDoParWorkers()

    system.time({
    statsOut <- foreach (r = 1:nrow(dfSliceRuns), .combine = rbind) %dopar% { # %dopar%
      library("data.table")
      library("pastecs")
      library("reshape2")
      
      #try(detach(dfSliceRuns[r,]), silent = T)
      #attach(dfSliceRuns[r,]) # Spills "org" and "sliceVars" into the environment
      org <- dfSliceRuns$org[r]; sliceVars <- dfSliceRuns$sliceVars[r] #; inclNon <- dfSliceRuns$inclNon[r]
      print(dfSliceRuns[r,])
      
      vSliceVars <- strsplit(sliceVars, ",")[[1]]
      fmSliceVars <- paste(vSliceVars, collapse = " + ")
      useData <- get(p0(org, "Data"), envir = myEnv)
      
      dups <- duplicated(useData[, c("sid", vSliceVars)])
      undupData <- useData[!dups,]
      dtUseData <- data.table(undupData, by = sliceVars)
      dtStats <- dtUseData[, lapply(.SD, stat.desc), by = sliceVars, .SDcols = descVars]
      dfStats <- data.frame(dtStats)
      dfStats$stat <- statNames
      dfStats <- dfStats[dfStats$stat %in% keepStats,]

      dfStats_l <- melt(dfStats, id = c(vSliceVars, "stat"), variable = "variable")
      dfStats <- dcast(dfStats_l, as.formula(p0(fmSliceVars, " + variable ~ stat")))
      
      # XXX This is a bit hard-coded -- return to generalize it
      colnames(dfStats)[1:2] <- c("sliceVal", "year")
      dfStats$sliceVar <- vSliceVars[1]
      dfStats$population <- org

      return(dfStats)
    }
    })

  stopCluster(cl)

  
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

  #---------------------------
  ## Prepare data for the runs
  #---------------------------

  # Cache data set for schools of all youth in schools attended by anyone served by each org
    for (y in 2012:2014){
      calcDataY <- subset(calcData, year == y)
      for (org in grepv("[^CPS]", allOrgList)){
        #print(paste("Working on org", org, "and year", y))
        orgSchools <- unique(calcDataY$schlid[calcDataY[, org] == org])
        assign(p0(org, "OwnSchools", y),
               subset(calcDataY, schlid %in% orgSchools & year == y),
               envir = myEnv)
      }
    }

  # Set up parallel loop across all org/slicevar/sliceval rows, each of which pull that org's data
    subsetVars <- c("population", "sliceVar", "sliceVal", "year")
    runList <- unique(statsOut[, subsetVars]) # Necessary because stats.org is one line per variable
    naRows <- apply(runList, 1, function(x) any(is.na(x)))
    runList <- runList[!naRows, ]
    rl <- runList[!grepl("Non|CPS", runList$population),] # Remove all non-org runs, so that we only calculate school-based peers for served populations

  #------------------------------------------------------------
  ## Call the peer averages function for each org's calculation
  #------------------------------------------------------------

    nCores <- 10 # Using fewer than all of the cores to avoid using all resources
    cl <- makeCluster(nCores)
    registerDoParallel(cl)
    getDoParName()
    getDoParWorkers()

    system.time({
    PeerStatsOut <- foreach (r = 1:nrow(rl), .combine = rbind) %dopar% {
      library("data.table")
      library("pastecs")
      library("reshape2")
      
      #try(detach(rl[r,]), silent = T)
      #attach(rl[r,]) # Spills variables "population", "year", "sliceVar" and "sliceVal" into the environment
      population <- rl$population[r]; year <- rl$year[r]; sliceVar <- rl$sliceVar[r]; sliceVal <- rl$sliceVal[r]
      
      # XXX These operations were previously done with data.table objects. Consider reinstituting
      # that approach. Note that df[, var] is equivalently accessed as dt[, var, with = F]
      
      # Identify schools attended by this slice of served youth
        dfSameSch <- get(p0(population, "OwnSchools", year),
                         envir = myEnv)
        dfFocals <- subset(dfSameSch,
                           subset = as.vector(dfSameSch[, sliceVar] == sliceVal &
                                              !is.na(dfSameSch[, sliceVar]) &
                                              dfSameSch[, population] == population),
                           select = c("sid", "schlid", descVars))
        dfFocals.dups <- duplicated(dfFocals[, c("sid", "schlid")])
        dfFocals      <- dfFocals[!dfFocals.dups,]
        focalSchs     <- unique(dfFocals$schlid)
      
      # Identify records of unserved served peers in these schools
        peerRows <- dfSameSch$year == year &
                    dfSameSch$schlid %in% focalSchs &
                    !(dfSameSch$sid %in% dfFocals$sid)
        dfPeers      <- subset(dfSameSch, subset = as.vector(peerRows), select = c("sid", "schlid", descVars))
        dfPeers.dups <- duplicated(dfPeers[, c("sid", "schlid")])
        dfPeers      <- dfPeers[!dfPeers.dups,]
      
      # Calculate the characteristics of these peers by school
        dtPeers <- data.table(dfPeers, key = "schlid")
        dtPeerStats <- dtPeers[, lapply(.SD, stat.desc), by = "schlid", .SDcols = descVars]
        dfPeerStats <- data.frame(dtPeerStats)
        dfPeerStats$stat <- statNames
        dfPeerStats <- subset(dfPeerStats, subset = stat %in% c("mean", "SE.mean", "nbr.val"))
        
        dfPeerStats_l <- melt(dfPeerStats, id = c("schlid", "stat"), variable = "descVar")
        dfPeerStats <- dcast(dfPeerStats_l, schlid + descVar ~ stat)
      
      # Set up header for identifying calculations
        outHeader <- data.frame(population = population, year = year, sliceVar = sliceVar, sliceVal = sliceVal)
      
      # Calculate the proportions to use for each variable, since different variables by school may have
      #   different numbers of NAs to drop
      
      allVarCalcs <- NULL
      for (v in descVars){
        
        # Get proportions of schools represented by focal youth involved in the calculation
          vFocalSchs <- dfFocals$schlid[!is.na(dfFocals[, v])]
          if (length(vFocalSchs) == 0) next # Some variables are not available in a given year. In this case, skip that variable in the loop
          dfSchProps <- data.frame(prop.table(table(vFocalSchs)))
          colnames(dfSchProps) <- c("schlid", "prop")
        
        # Calculate characteristics of peers, excluding focal youth
          peerStats_bySch <- dfPeerStats[!is.na(dfPeerStats$mean), ]
          try(peerStats_bySch[peerStats_bySch$n == 1, c("SE.mean")] <- 0, silent=T)
            # XXX This is a bit of a stop-gap, since some calculations have zero rows after
        
        # Get weighted average
          peerStat <- peerStats.fn(myProps = dfSchProps, myVar = v, mySchStats = peerStats_bySch[peerStats_bySch$descVar == v, ])
          allVarCalcs <- rbind(allVarCalcs, cbind(outHeader, peerStat))
        
      } # End of loop across descVars

      return(allVarCalcs)
    }
    }) # End timing

    stopCluster(cl)

    PeerStatsOut$population <- paste(PeerStatsOut$population, "Sch Peers")
    

#-------------------------
## Compile and save output
#-------------------------
    
  # Combine all calculations
    colnames(statsOut)[cn(statsOut) == "descVar"] <- "variable"
    descStats <- rbind(statsOut, PeerStatsOut)

  # Clean and add features to the output
    descStats$sliceVar[grep("(fGradeLvl)|(fGradeGrp)", descStats$sliceVar)] <- "grade"
    descStats$id <- with(descStats, paste(population, p0(sliceVar, ":", sliceVal), year, variable, sep = "_"))
    descStats$plusminus <- descStats$SE.mean * 1.96

  # Suppress calculations for small or invalid calculations
    descStats <- within(descStats, {
      plusminus[nbr.val  < 10] <- NA
      plusminus[nbr.val == NA] <- NA
      mean[nbr.val  < 10]      <- NA
      mean[nbr.val == NA]      <- NA
    })

    save(descStats, file = paste0(dataPath, "descStats.Rda"))

  # Output cuts of the descriptive output for each partner
    for (org in allOrgList){
      myStats <- rbind(subset(descStats, grepl(org  , descStats$population)),
                       subset(descStats, grepl("CPS", descStats$population)))
      myStats <- myStats[!is.na(myStats$sliceVal),]
      if( org == "Collab") { # Further subset Collab to non-site/program specific measures
        myStats <- myStats[grepl("all|bGender_Male|bLunch_FR|grade", myStats$sliceVar),]
      }
      d <- subset(myStats, select = c("mean", "nbr.val", "SE.mean", "plusminus", "id"))
      combos <- unique(myStats[, c("population", "sliceVal", "sliceVar", "year")])
      slices <- combos[!grepl("Peers", combos$population), ] # This will maintain the "CPS" 
      slicesXYears <- unique(slices[, c("population", "sliceVal", "sliceVar")])
      slicesXYears <- slicesXYears[!grepl("CPS", slicesXYears$population),] # This will discard the CPS
      sliceVars <- unique(slices$sliceVar)
        # This ordering is intentional, because it allows pasting of the first to columns, based on filtering by the latter two
      
      write.csv(d, file = p0(dataPath, "descStats_", org, ".csv"), row.names = F)
      for (f in c("combos", "slices", "slicesXYears", "sliceVars")){
        write.csv(get(f), file = p0(dataPath, f, "_", org, ".csv"), row.names = F)  
      }
    }



