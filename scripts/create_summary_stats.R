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
  #library("data.table")
  ep <- function(x){ eval(parse(TEXT = x))}
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
    orglist <- c("Org Alpha", "Org Beta")
  } else {
  	load(dataPath %&% "EnrCpsAcs.Rda")
    myData <- EnrCpsAcs
    rm(EnrCpsAcs)
    
    orglist <- c("YMCA", "ASM", "CHASI") # UPDATE HERE AS WE HAVE DATA FOR NEW PARTNERS TO INCLUDE IN TOTALS.
    
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
                grep("Tract_",    cNames, value=T))
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

  calcData$all <- "All" # This creates a single "category" to allow us to specify an "all-in" calculation rather than a subset
  
  ## Establish a function to calculate mean, variance, N and se by arbitrary subgroups

  ###First, we need to designate our by variables and filters.  See Google Drive "Desired Summary Stats" for reference.
      # ByVars1 -> ORG: set to "Org", "Collab", or one of the Org values (for comparing that org to not that org) - never makes sense to set this to all
      # ByVars2 -> SITE: ALL or site
      # ByVars3 -> PROGRAM: ALL or program
      # ByVars4 -> GRADE: ALL or grade
      # ByVars5 -> YEAR: ALL or year

  runStats <- function(data, byVars, myVars){

    # If we'll be looking at program or site level variation, will only be working with one org:
    if(all(byVars[2:3]=="all")) {
      useData <- data
    } else {
      useData <- data[!grepl("Non", data[,byVars[1]]), ]
    }

    # Then, determine what other by variables are needed - one line per student/by variable
    toFilter <- sapply(byVars, function(x) x != 'all')
    filterVars <- as.character(byVars[toFilter])
    keepRows <- !duplicated(useData[,c("sid", filterVars)])
    useData <- useData[keepRows,]
    
    # Calculate various summary statistics
    byList <- lapply(byVars, function(x) useData[, x])
    myMeans <- aggregate(useData[, myVars], byList, mean, na.rm=T)
    myS2    <- aggregate(useData[, myVars], byList, function(x) var(x, na.rm=T))
    myNs    <- aggregate(useData[, myVars], byList, function(x) sum(!is.na(x)))
    myMeans$stat <- "mean"; myS2$stat <- "var"; myNs$stat <- "n"
    stack <- rbind(myMeans, myS2, myNs)
    
    # Reshape the data set to have records by byVars, and statistics going across
    # XXX This could probably be done by sequential merges after the calculations, but this code is just about as short
    byNames <- grep("Group.", colnames(stack), value = T)
    longstack <- melt(stack, id=c(byNames, "stat"))
    out <- cast(longstack, as.formula(paste0(paste(c(byNames, "variable"), collapse="+"), "~ stat")))
    out$sd <- sqrt(out$var)
    out$var_mean <- out$var / out$n
    out$se_mean <- sqrt(out$var_mean)
    return(out)
  }


  # Run Calculations across various summary levels

  print("Initiating calculations for full collaborative")
  stats.byCollab     <- runStats(data = calcData, byVars = c("Collab", "all" , "all"    , "all"               , "year"), myVars = descVars); print("The big all-in calculation has been completed")
  stats.byCollabGr   <- runStats(data = calcData, byVars = c("Collab", "all" , "all"    , "fGradeLvl"         , "year"), myVars = descVars)
  stats.byCollabGrp  <- runStats(data = calcData, byVars = c("Collab", "all" , "all"    , "fGradeGrp_K5_68_HS", "year"), myVars = descVars)
  stats.collab       <- rbind(stats.byCollab, stats.byCollabGr, stats.byCollabGrp)    

  stats.org          <- stats.collab
  rm(stats.collab, stats.byCollab, stats.byCollabGr, stats.byCollabGrp)

  for (myOrg in orglist){
    print(paste0("Initiating calculations for ", myOrg))
    stats.byOrg     <- runStats(data = calcData, byVars = c(myOrg, "all" , "all"    , "all"               , "year"), myVars = descVars); print("The big all-in calculation has been completed")
    stats.byOrgGrp  <- runStats(data = calcData, byVars = c(myOrg, "all" , "all"    , "fGradeGrp_K5_68_HS", "year"), myVars = descVars)
    stats.byOrgGr   <- runStats(data = calcData, byVars = c(myOrg, "all" , "all"    , "fGradeLvl"         , "year"), myVars = descVars)
    stats.bySite    <- runStats(data = calcData, byVars = c(myOrg, "site", "all"    , "all"               , "year"), myVars = descVars)
    stats.byProg    <- runStats(data = calcData, byVars = c(myOrg, "all" , "program", "all"               , "year"), myVars = descVars)
    stats.bySiteGr  <- runStats(data = calcData, byVars = c(myOrg, "site", "all"    , "fGradeLvl"         , "year"), myVars = descVars)
    stats.bySiteGrp <- runStats(data = calcData, byVars = c(myOrg, "site", "all"    , "fGradeGrp_K5_68_HS", "year"), myVars = descVars)
    stats.byProgGr  <- runStats(data = calcData, byVars = c(myOrg, "all" , "program", "fGradeLvl"         , "year"), myVars = descVars)
    stats.byProgGrp <- runStats(data = calcData, byVars = c(myOrg, "all" , "program", "fGradeGrp_K5_68_HS", "year"), myVars = descVars)
    
    # No longer assembling individual datasets by org to avoid hardcoding org names (using flexibility of orglist)
    stats.org       <- rbind(stats.org, stats.byOrg, stats.bySite, stats.byProg, stats.byOrgGr, stats.bySiteGr, stats.byOrgGrp, stats.bySiteGrp, stats.byProgGr, stats.byProgGrp))
    }
  
  colnames(stats.org)[1:5] <- c("org", "site", "program", "grade", "year")

  rm(stats.byOrg, stats.byOrgGrp, stats.byOrgGr, stats.bySite, stats.byProg, stats.bySiteGr, stats.bySiteGrp, stats.byProgGr, stats.byProgGrp)

  
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
  
  
  # Calculate school means overall and by grade

    stats.bySch    <- runStats(data = calcData, byVars = c("schlid", "all"               , "year"), myVars = descVars)
    stats.bySchGr  <- runStats(data = calcData, byVars = c("schlid", "fGradeLvl"         , "year"), myVars = descVars)  
    stats.bySchGrp <- runStats(data = calcData, byVars = c("schlid", "fGradeGrp_K5_68_HS", "year"), myVars = descVars)
    
    stats.sch <- rbind(stats.bySch, stats.bySchGr, stats.bySchGrp)
    colnames(stats.sch)[1:3] <- c("sch", "grade", "year") 
    rm(stats.bySch, stats.bySchGr, stats.bySchGrp)
  

  #-------------------------
  ## Create Helper Functions
  #-------------------------

    #---------
    # Create a function to return the appropriate observations for calculating school peer proportions
    # (i.e. returning all records when "All" is specified, even though the value "All" does not appear)
    #---------
  
      # Because grade and org need special handling, can't use conditions (e.g. "grepl(org, calcData$Org)" where "*" can be passed to org)
      
      getSubset <- function(subvar, subval){
        if (subval == "All"){
        
          return(calcData$all == calcData$all) # XXX Basically, an inelegant way to return a right-sized vector of "TRUE" values
        } else if(subvar == "grade") {
          
          # Separate handling for grade ranges versus individual grade levels
          if (subval %in% c("K-5", "6-8", "HS")){
            return(calcData$fGradeGrp_K5_68_HS == subval)
          } else {
            return(calcData$fGradeLvl == subval)
          }
        } else if(subvar == "org") {
          
          #Separate handling for full collab versus individual orgs
          if (subval=='Collab') {
              return(calcData$Collab == subval)
            } else {
              return(calcData$org == subval)
            }
        } else {
          return(calcData[, subvar] == subval) # Identify rows with specifically the right values
        }
      }
  
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
  
    peerStats.fn <- function(myProps, myVar, mySchStats){
      
      stats.props <- merge(mySchStats, myProps, by = "sch")  # merge proportions into school-level stats data
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
  
    subsetVars <- c("org", "site", "program", "grade", "year")
    runList <- unique(stats.org[, subsetVars]) # Necessary because stats.org is one line per variable
    rl <- runList[!grepl("Non", runList$org),] # Remove all non-org runs, so that we only calculate school-based peers for served populations

    # stats.peers <- mapply(peerStats.fn, rl$Org, rl$Site, rl$Grade)
    # Using a loop rather thn mapply() because mapply doesn't like being returned many data frames

    nRuns <- nrow(rl)
    stats.peers <- NULL

    for (i in 1:nRuns){
      
      # Audit values
      #org = "YMCA"; site = "High Ridge"; grade = "All"; program = 'All'; year = "2013"
      #org = "YMCA"; site = "All"; grade = "All"

      org <- rl$org[i]; site <- rl$site[i]; program <- rl$program[i]; grade <- rl$grade[i]; year <- rl$year[i];
      
      mySchs <- calcData[getSubset("org", org) & getSubset("site", site) & getSubset("program", program) & getSubset("grade", grade) & getSubset("year", year), c("sid", "schlid")]
      
      # Need each SID once - if any subset value is all, might double count individuals who have two lines in that subset
      # Audit values with High Ridge illustrate this well
      mySchs <- unique(mySchs)[,"schlid"] # Note that schlid will be non-unique - this is unique list at the student level
      schP <- data.frame(prop.table(table(mySchs)))
      colnames(schP) <- c("sch", "prop")
      
      # Get school-level statistics for schools involving in the current calculation
      schStats <- stats.sch[stats.sch$sch %in% schP$sch &
                            stats.sch$grade == grade &
                            stats.sch$variable %in% descVars,
                            c("sch", "variable", "mean", "n", "var_mean")]
      
      # Set up header for identifying calculations
      outHeader <- data.frame(org = rl$org[i], site = rl$site[i], program = rl$program[i], grade = rl$grade[i], year = rl$year[i])
      
      print(paste("Performing run ", i, " of ", nRuns, "or ", round(i/nRuns, 3)*100, "% done. Run is for: org =", org, ", site =", site, ", program =", program, ", grade =", grade, ", year =", year))
      
      # Run calculations variable by variable since different variables by school may have different numbers of NAs to drop
      for (v in descVars){
        
        peerStats <- peerStats.fn(myProps = schP, myVar = v, mySchStats = schStats[schStats$variable == v, ])
        # stats.peers[i,] <- cbind(outHeader, peerStats) ... this does not work to replace the full row in stats.peers
        stats.peers <- rbind(stats.peers, cbind(outHeader, peerStats))
        
      }
    }

    stats.peers$site <- paste(stats.peers$site, "Sch-Based Peers")

    descStats    <- rbind(stats.org[, colnames(stats.peers)], stats.peers)
    descStats$plusminus <- descStats$se_mean * 1.96
    descStats$id <- paste(descStats$org, descStats$site, descStats$program, descStats$grade, descStats$year, descStats$variable, sep="_")
    
    save(descStats, file = paste0(dataPath, "descStats.Rda"))
    write.csv(descStats, file = paste0(dataPath, "descStats.csv"))

    combos <- unique(descStats[, c("org", "site", "program", "grade", "year")])
    combos$id <- paste(combos$org, combos$site, combos$program, combos$grade, combos$year, sep="_")
    combos$gradefilter <- ifelse(combos$grade!="All" & (combos$site!="All" | combos$program!="All"), 1, 0)
    write.csv(combos, file = paste0(dataPath, "combos.csv"))

# XXX: Why are there NaN's here?  (And elsewhere in calculated means).  Need to track these anomalies down and check data.
