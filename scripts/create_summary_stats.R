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
  try(setwd("H:/Integrated Evaluation Project for YSS Providers/Analysis/"), silent=T)
  
  dataPath <- "./data/constructed-data/" # File path to locate and save data
  scriptPath <- "~/GitHub/after-school-collab/scripts"

  library("reshape2")
  #library("plyr")
  library("data.table")
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
  	load(dataPath %&% "EnrCpsAcs.Rda")
    myData <- EnrCpsAcs
    rm(EnrCpsAcs)    
    orglist <- c("YMCA", "ASM", "CHaA", "UCAN", "CPL") # UPDATE HERE AS WE HAVE DATA FOR NEW PARTNERS TO INCLUDE IN TOTALS.
  } 

#---------------------------------------------------------
## Select which program categories to use for calculations
#---------------------------------------------------------
  
  # ASM sites are too detailed and too numerous to be a useful level of summary.
  #   Instead, we will generate "site" calculations based on site type, e.g. "Church", "Community Center", etc.
  myData$site[myData$org == "ASM"] <- myData$sitetype[myData$org == "ASM"]

#-------------------------------
## Select variables to summarize
#-------------------------------

  # Build a list of descriptive variables
  cNames <- colnames(myData)
  descVars <- c("bGender_Male", "bGender_Female", "bIEP", "bEll", "iep_test", "lep_test", "lunch_test",
                "bOnTrack", "promoted_nextyr", "retained_nextyr",
                grep("onTimeGrad_", cNames, value = T),
                grep("Pct_Attend",  cNames, value=T),
                grep("isat_",       cNames, value=T),
                grep("nwea_",       cNames, value=T),
                grep("explore_",    cNames, value=T),
                grep("plan_",       cNames, value=T),
                grep("psae_",       cNames, value=T),
                grep("bRace",       cNames, value=T),
                grep("bLunch",      cNames, value=T),
                grep("Tract_",      cNames, value=T),
                grep("MVMS_.+[^se]$", cNames, value=T)) # This gets anything that starts with "MVMS", and doesn't end with "se"
                # "bHsGrad", 

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
  convVars <- c("lunch_test", "explore_compss", "plan_compss", "plan_mathss", "plan_readss", "plan_engss", "plan_sciess")
  for (v in convVars){
    myData[, v]  <- as.numeric(myData[, v])  
  }
  
  subVars <- c("sid", "org", "Collab", orglist, "site", "program", "region", "schlname", "fGradeLvl", "fGradeGrp_K5_68_HS", "year", "schlid", descVars) 
  calcData <- myData[, subVars]
  rm(myData)
  

#------------------------------  
#------------------------------
## Calculate summary statistics
#------------------------------
#------------------------------  

  # Create standard list of slices cuts to calculate
    withinOrgSlices <- c("site", "program") # These are comparisons that only make sense within organization, and won't get "non-org" calculations
    standardSlices  <- c("fGradeLvl", "fGradeGrp_K5_68_HS", "bGender_Male", "bLunch_FR")
    sliceList <- c(withinOrgSlices, standardSlices) # XXX Later include dosage categories
    allOrgList <- c("Collab", orglist)
    allSlices <- cbind(rep(allOrgList, each  = length(sliceList)),
                       rep(sliceList,  times = length(allOrgList)),
                       "year")
    allSlicesComma <- sapply(1:nrow(allSlices), function(r) c(allSlices[r, 1], paste(allSlices[r,], collapse = ",")))

  # Add special orders for organizations
    allSlicesComma <- rbind(t(allSlicesComma),
                            c("ASM", "ASM,region,year"),
                            c("ASM", "ASM,term,year"))

  # Format runs in data frame, and determine which slices are for 
    dfSliceRuns <- data.frame(allSlicesComma, stringsAsFactors = F)
    colnames(dfSliceRuns) <- c("org", "sliceVars")
    dfSliceRuns$inclNon <- ifelse(grepl(p0("(", paste(c(withinOrgSlices, "region", "term"), collapse = ")|("),")"), dfSliceRuns$sliceVars), 0, 1)
     
  # Prepare organization specific files and information
  # XXX Check in DFSS code what environment these objects are created in
    dtCalcData <- data.table(calcData) # key will be assigned in the loop for the appropriate by
    myEnv <- new.env()
    for (o in allOrgList){
      # Prep data subset to each org
        assign(p0("dt", o, "Only"), data.table(calcData[calcData[,o] == o,]), envir = myEnv)
      
      # Get list of all variables that or has data slices
      # XXX May not be necessary if we melt all org slice calculations into an "org", "year", "slicevar", "sliceVal" format
#         allSlices <- paste(dfSliceRuns$sliceVars[dfSliceRuns$org == o], collapse = ",")
#         assign(p0(o, "Slices"), unique(strsplit(allSlices, ",")[[1]]), envir = myEnv)
    }

  # Loop calculations using foreach

    statNames <- rownames(calcData(data[1,])) 
    keepStats <- c("mean", "SE.mean", "nbr.val")

    statsOut <- foreach (r = 1:nrow(dfSliceRuns). combine = rbind) %dopar% {
      attach(dfSliceRuns[r,]) # Spills "org", "sliceVars", and "includeNon" into the environment
      if (includeNon == 1){
        useData <- dtCalcData
      } else {
        useData <- get(p0(org, "Only"), envir = myEnv) # XXX update environment
      }
      dtUseData <- data.table(UseData, by = sliceVars)
      dtStats <- dtUseData[, lapply(.SD, stat.desc), by = sliceVars, .SDcols = descVars]
      dfStats <- data.frame(dtStats)
      dfStats$stat <- statNames
      dfStats <- dfStats[dfStats$stat %in% keepStats,]
      # XXX May need to melt and cast this, to get summarized variables long and stats wide
      
      # Fill in missing columns
#       orgSlices <- get(p0(org, "Slices"), envir = myEnv) # XXX update environment
#       missingSlices <- orgSlices[!(orgSlices %in% cn(dfStats)]
#       dfStats[, missingSlices] <- "All"

      # Melt data set into (for now) three columns: "Participation (i.e. org, non-org, sch-based-peers)", "Slice Var", "Slice Val"
      dfStats_l <- melt(dfStats, by = c(org, "year"), variable.name = "sliceVar", value.name = "sliceVal")
      return(dfStats_l)
    }

  # Compile output across organizations and generate id
  # XXX May not need this if each of the orgs' output data formats are the same: org, year, slice var, slice val
    
#     for (o in allOrgList){
#       trialAppend <- lStatsOut[grepl(o, names(lStatsOut))]
      # XXX The following is pending how the melt and combination goes
#       orgSlices <- get(p0(org, "Slices")) # XXX update environment
#       trialAppend$id <- sapply(1:nrow(trialAppend), 
#                                function(r)
#                                  paste(paste(varList, trialAppend[r, varList], paste = ":"), collapse = "_"))
        # Will yield <var1>:<val1>_<var2>:<val2>_...
#     }

# Apply subset based on slice var and slice val
  # Apply calculations across all descriptive variables in ordinary for loop
# Post processing to harmonize multiple grade columns ("fGradeLvl", "fGradeGrp_K5_68_HS") into one ("grade")

  
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
    for (org in allOrgList){
      orgSlices <- get(p0(org, "Slices")) # XXX update environment
      orgSchools <- unique(calcData$schlid[calcData[, org] == org])
      assign(p0(o, "OwnSchools"),
             calcData[calcData$schlid %in% orgSchools,],
             envir = myEnv)
    }

  # Set up parallel loop across all org/slicevar/sliceval rows, each of which pull that org's data
    subsetVars <- c("org", "sliceVar", "sliceVal")
    runList <- unique(stats.org[, subsetVars]) # Necessary because stats.org is one line per variable
    naRows <- apply(runList, 1, function(x) any(is.na(x)))
    runList <- runList[!naRows, ]
    rl <- runList[!grepl("Non", runList$org),] # Remove all non-org runs, so that we only calculate school-based peers for served populations

  #------------------------------------------------------------
  ## Call the peer averages function for each org's calculation
  #------------------------------------------------------------

    stats.peers <- foreach (r = 1:nrow(rl), .combine = rbind){
      attach(rl[r,]) # Spills variables "org", "year", "sliceVar" and "sliceVal" into the environment
      
      # XXX These operations were previously done with data.table objects. Consider reinstituting
      # that approach. Note that df[, var] is equivalently accessed as dt[, var, with = F]
      
      # Identify schools attended by this slice of served youth
        dfSameSch <- get(p0(org, "OwnSchools"),
                         envir = myEnv)
        dfFocals <- subset(dtOwnSch,
                           subset = as.vector([dtOwnSch[, sliceVar] == sliceVal),
                           select = c("sid", "schlid", descVars)]
        dfFocals.dups <- duplicated(dfFocals[, c("sid", "schlid")])
        dfFocals      <- dfFocals[!dfFocals.dups,]
        focalsSchs     <- unique(dfFocals$schlid)
      
      # Identify records of served peers in these schools
      peerRows <- dfSameSch$year == year &
                  dfSameSch[, sliceVar] == sliceVal &
                  dfSameSch$schlid %in% focalSchs &
                  !(dfSameSch$sid %in% dfFocal$sid)
      dfPeers      <- subset(dfSameSch, subset = as.vector(peerRows), select = c("sid", "schlid", descVars))
      dfPeers.dups <- duplicated(dfPeers[, c("sid", "schlid")])
      dfPeers      <- dfPeers[!dfPeers.dups,]
      
      # Calculate the characteristics of these peers by school
        dtPeers <- data.table(dfPeers, key = "schlid")
        dtPeerStats <- dtPeers[, lapply(.SD, stat.desc), by = "schlid", .SDcols = descVars]
        dfPeerStats <- data.frame(dtPeerStats)
        dfPeerStats$stat <- statNames
        dfPeerStats <- subset(dfPeerStats, subset = stat %in% c("mean", "SE.mean", "nbr.val"))
        # XXX May need to melt and cast this to get the variables long and the stats wide
      
      # Set up header for identifying calculations
      outHeader <- data.frame(org = org, year = year, sliceVar = sliceVar, sliceVal = sliceVal)
      
      # Calculate the proportions to use for each variable, since different variables by school may have
      #   different numbers of NAs to drop
#       print(paste("Performing run", i, "of ", nRuns, "or ", round(i/nRuns, 3)*100, "% done. Run is for: org =", org, ", site =", site, ", program =", program, ", schlname =", schlname, ", grade =", grade, ", year =", year))
      
      allVarCalcs <- NULL
      for (v in descVars){
        
        # Get proportions of schools represented by focal youth involved in the calculation
          vFocalSchs <- myFocals$schlid[!is.na(dfFocals[, v])]
          if (length(vFocalSchs) == 0) next # Some variables are not available in a given year. In this case, skip that variable in the loop
          dfSchProps <- data.frame(prop.table(table(vFocalSchs)))
          colnames(dfSchProps) <- c("sch", "prop")
        
        # Calculate characteristics of peers, excluding focal youth
          peerStats_bySch <- dfPeerStats[!is.na(dfPeerStats$mean), ]
          try(peerStats_bySch[peerStats_bySch$n == 1, c("SE.mean")] <- 0, silent=T)
            # XXX This is a bit of a stop-gap, since some calculations have zero rows after
        
        # Get weighted average
          peerStat <- peerStats.fn(myProps = dfSchProps, myVar = v, mySchStats = peerStats_bySch)
          allVarCalcs <- rbind(allVarCalcs, cbind(outHeader, peerStat))
        
      } # End of loop across descVars

      return(allVarCalcs)
    }

    stats.peers$org <- paste(stats.peers$org, "Sch-Based Peers")
    colnames(stats.peers)[cn(stats.peers)=="schl"] <- "schlname"
    

#-------------------------
## Compile and save output
#-------------------------
    
  # Consolidate the grade variables and update record IDs
    XXX$grade = ifelse(XXX$fGradeLvl != "All",
                       XXX$fGradeLvl,
                       XXX$fGradeGrp_K5_68_HS)
    # XXX Need to update the id here

  descStats    <- rbind(stats.org[, colnames(stats.peers)], stats.peers)
  descStats$plusminus <- descStats$SE.mean * 1.96
  descStats <- within(descStats, {
    id <- paste(org, site, program, schlname, grade, year, variable, sep="_")
    
    # Suppress calculations for small or invalid calculations
    plusminus[nbr.val  < 10] <- NA
    plusminus[nbr.val == NA] <- NA
    mean[nbr.val  < 10]      <- NA
    mean[nbr.val == NA]      <- NA
  })

  save(descStats, file = paste0(dataPath, "descStats.Rda"))

  # Subset the file for handling, since the total size is getting to be unnecessarily unwieldy. At one point, this was 47mb. This subsetting halves the size.
  
  descStatsOut <- descStats[, c("mean", "nbr.val", "SE.mean", "plusminus", "id")] 
  write.csv(descStatsOut, file = paste0(dataPath, "descStats.csv"), row.names = F)

  combos <- unique(descStats[, c("org", "site", "program", "schlname", "grade", "year")])
  combos$id <- paste(combos$org, combos$site, combos$program, combos$grade, combos$year, sep="_")
  combos$gradefilter <- ifelse(combos$grade!="All" & (combos$site!="All" | combos$program!="All"), 1, 0)
  rownames(combos) <- NULL

  combosXYears <- unique(combos[, !grepl("year|id", colnames(combos))])

  orgYearCombos <- data.frame(unique(combos[!grepl("Non-", combos$org), c("org", "year")]))
    colnames(orgYearCombos) <- c("org", "year")

  orgCombos <- data.frame(org=unique(orgYearCombos[, c("org")]))

  for (f in c("combos", "combosXYears", "orgYearCombos", "orgCombos")){
    d <- get(f); rownames(d) <- NULL
    write.csv(d, file = paste0(dataPath, f %&% ".csv"))
  }

