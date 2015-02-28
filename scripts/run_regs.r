#---------------------------------------------#
#---------------------------------------------#
# RUN OUTCOME-ORIENTED REGRESSION ESTIMATION  # 
#                                             #
# Authors: Nick Mader, Ian Matthew Morey,     # 
#  	    and Emily Wiegand		                  #
#---------------------------------------------#
#---------------------------------------------#

## Set up workspace and designate/update file locations

  rm(list=ls())
  library("ggplot2")
  library("reshape2")
  library("plyr")
  library("lme4")
  myDir <- "/projects/Integrated_Evaluation_Youth_Support_Services/"
  setwd(myDir)
  dataPath <- "./data/constructed-data/"
  load("./data/constructed-data/CpsOrg_PP.Rda")
  source("~/GitHub/after-school-collab/scripts/helper-functions.r")

#--------------------------------#
#--------------------------------#
### 1. Load Data and Set Specs ###
#--------------------------------#
#--------------------------------#

  ### Preliminary data steps
  
    # Misc rescaling and data conversion
      d_pp$fSch <- factor(d_pp$schlid)
      d_pp$Pct_NonAbsent100     <- d_pp$Pct_NonAbsent*100
      d_pp$Pct_NonAbsent100_lag <- d_pp$Pct_NonAbsent_lag*100
      d_pp$nGradeLvl <- f.to.n(d_pp$fGradeLvl)
      d_pp <- d_pp[!grepl("PK|KG|Unk|Ungr", d_pp$fGradeLvl),]
  
    # Check how gains vary across grades
      d_pp$isat_mathss_gain <- with(d_pp, isat_mathss - isat_mathss_lag)
      d_pp$isat_readss_gain <- with(d_pp, isat_readss - isat_readss_lag)
      aggregate(d_pp[, c("isat_mathss_gain", "isat_readss_gain")], list(d_pp$fGradeLvl), mean, na.rm = T)
  
    # Subset the data
      d_sub <- subset(d_pp, year == 2014)
  
  ### Set up regression lists
    # Dependent Vars 
      depVarList <- c("Pct_Attend100", "isat_mathss", "isat_readss", "bOnTrack") # , "bHsGrad"
        depVarNames <- c("School Attendance Rate", "ISAT Math Score", "ISAT Reading Score", "On-Track Status") # , "Graduating Seniors"
        depVarYLabs <- c("Sch Attendance, % Units", "Scale Score Units", "Scale Score Units", "Impact on Prob of Being On-Track") #, "Impact on HS Graduation")
    # Regressors
      raceXs <- grepv("bRace", cn(d_pp))
        raceXs <- raceXs[-which(raceXs %in% c("bRace_W", "bRace_NonW"))]
      regVars <- c("bLunch_F", "bLunch_R", raceXs, "bGender_Female", "bIEP", "bEll")
        #"Tract_ViolentCrimes_PerHundr", "Tract_Pct_IncRatKidsLt6_Lt100", "Tract_Pct_NonEnglLangSpokenAtHome"
      lagXs <- c("Pct_Attend100_lag", "isat_mathss_lag", "isat_readss_lag")
      mvmsXs <- grepv("MVMS_.+[^e]$", cn(d_pp))
    # Treatment vars
      collabBin <- "bCollab"
      orgBin <- c("bASM", "bCHaA", "bUCAN", "bYMCA")
      collabHoursCatDoseTrt <-   grepv("Collab_fdosage_hours", cn(d_pp))
      collabDaysCatDoseTrt  <-   grepv("Collab_fdosage_days",  cn(d_pp))
      collabHoursCtsDoseTrt <- c(grepv("Collab_dosage_hours",  cn(d_pp)), collabBin)
      collabDaysCtsDoseTrt  <- c(grepv("Collab_dosage_days",   cn(d_pp)), collabBin)
      orgHoursCatDoseTrt <-   grepv("[^Collab]_fdosage_hours", cn(d_pp))
      orgDaysCatDoseTrt  <-   grepv("[^Collab]_fdosage_days",  cn(d_pp))
      orgHoursCtsDoseTrt <- c(grepv("[^Collab]_dosage_hours",  cn(d_pp)), orgBin)
      orgDaysCtsDoseTrt  <- c(grepv("[^Collab]_dosage_days",   cn(d_pp)), orgBin)  
    # Site/Program Treatment indicators
      siteBin <- grepv("^org_site", cn(d_pp))
  
#----------------------------------------------#
#----------------------------------------------#
### 2. Set up regression and post-estimation ###
#----------------------------------------------#
#----------------------------------------------#
    
  # Set up data to use for troubleshooting code
    myData <- d_sub[d_sub$fGradeLvl=="7",]
    myY <- "isat_mathss"
    myLabel <- "Gr8"
    myTrt <- siteBin
    myXs <- c(regVars, "lep_test", mvmsXs)
    for (v in myTrt){
      myData[is.na(myData[, v]), v] <- 0
    }
    summary(myData[, myTrt])
  
  ### Set up function for linear regression estimation
  runLms <- function(myData, myY, myLabel, myTrt, myXs, lagXs){
    plusTrt <- plusPaste(myTrt)
    plusXs  <- plusPaste(myXs)
    plusLag <- plusPaste(lagXs)
    sumReg <- function(s){ summary(lm(as.formula(paste0(myY, "~", s)), data = myData))}
    rawReg  <- sumReg(plusPaste(plusTrt))
    preReg  <- sumReg(plusPaste(c(plusTrt, plusLag)))
    nowReg  <- sumReg(plusPaste(c(plusTrt, plusXs)))
    adjReg  <- sumReg(plusPaste(c(plusTrt, plusLag, plusXs)))
    schReg  <- sumReg(plusPaste(c(plusTrt, plusLag, plusXs)) %&% " + factor(schlid)")
    for (spec in c("raw", "pre", "now", "adj", "sch")){
      
      # Pull coefficients from regression
      d <- get(spec%&%"Reg")
      betas <- data.frame(d$coeff[, c("Estimate", "Std. Error")])
      if (spec == "sch") betas <- betas[-grep("schlid", rownames(betas)), ]
      colnames(betas) <- c("b", "se")
      betas$x <- rownames(betas)
      rownames(betas) <- NULL
      betas$t <- betas$b / betas$se
      betas$p <- 2*(1 - pnorm(abs(betas$t)))
      betas$sig <- ifelse(betas$p < 0.01, "***", 
                       ifelse(betas$p < 0.05, "**", 
                              ifelse(betas$p < 0.10, "*", "")))
      
      # Add fit statistics
      fitStats <- data.frame(x = c("AdjR2", "N", "df"),
                             b = c(d$adj.r.squared, length(d$residuals), d$df[2]))
      fitStats[, cn(betas)[!(cn(betas) %in% c("x", "b"))]] <- NA # Add remainder columns, to be filled with NAs
      betas <- rbind(betas, fitStats)

      # Obtain counts of non-zero values by treatment variable.
      #   This is intended to identify binary indicators with small cells
      rowsInReg <- rownames(myData) %in% names(d$residuals)
      countNon0 <- function(x) sum(x!=0)
      non0TrtCounts <- apply(myData[rowsInReg, myTrt], 2, countNon0)
      dfNon0TrtCounts <- data.frame("x" = as.character(names(non0TrtCounts)), non0Count = non0TrtCounts, stringsAsFactors = FALSE)
        rownames(dfNon0TrtCounts) <- NULL
      betas <- merge(betas, dfNon0TrtCounts, by = "x", all.x = T)
      
      # Add post-estimation statistics and names
      betas$plusminus <- 1.96*betas$se
      betas$ll <- betas$b - betas$plusminus
      betas$ul <- betas$b + betas$plusminus
      betas$y <- myY
      betas$spec <- spec
      betas$label <- myLabel
      
      # Create unique id for each estimate and output
      betas <- within(betas, id <- paste(y, label, spec, x, sep = "_"))
      assign(spec %&% "Out", betas[, c("spec", "y", "x", "b", "se", "non0Count", "plusminus", "ll", "ul", "t", "p", "sig", "label", "id")])
    }
    return(do.call(rbind, list(rawOut, preOut, nowOut, adjOut, schOut)))
    
  }
  system.time(test <- runLms(myData, myY, myLabel, myTrt, myXs, lagXs))
  
#----------------------------------------------#
#----------------------------------------------#
### 3. Loop Estimation For Multiple Outcomes ###
#----------------------------------------------#
#----------------------------------------------#
  
  estOut <- NULL
  for (myTrtGrp in c("siteBin", "orgBin")) { # "collabBin", "collabHoursCatDoseTrt", "collabHoursCtsDoseTrt" # "collabDaysCatDoseTrt", "collabDaysCtsDoseTrt")
    trtVars <- get(myTrtGrp)
    
    print(paste0("Running trtVars: ", myTrtGrp))
        
    ## Impacts on ISAT scores
      isatOut <- NULL
      for (myT in c("isat_mathss", "isat_readss")){
        for (myG in c("All")){ # 4:8, 
          print(paste0("--> Running Estimation for ", myT, " for grade ", myG))
          
          # Specify Xs and data
          Xs <- c(regVars, "lep_test")
          if (myG >= 6 | myG == "All") Xs <- c(Xs, mvmsXs)
          if (myG == "All") {
            d <- d_sub[d_sub$nGradeLvl %in% 4:8, ]
            Xs <- c(regVars, "fGradeLvl")
          } else {
            d <- d_sub[d_sub$nGradeLvl == myG, ]
          }
          
          # Run regs
          b <-  runLms(myData = d, myY = myT, myLabel = myG, myTrt = trtVars, myXs = myXs,
                       lagXs = c("Pct_Attend100_lag", "isat_mathss_lag", "isat_readss_lag"))
          
          # Rescale output
          mu_gain <- mean(d[, myT] - d[, myT %&% "_lag"], na.rm=T)
          rescaleVars <- c("b", "se", "plusminus")
          nonStatsRows <- !(b$x %in% c("AdjR2", "N", "df"))
          b[nonStatsRows, rescaleVars] <-
            sapply(rescaleVars, function(r) b[nonStatsRows, r] / mu_gain)
          b$ll <- b$b - 1.96*b$se
          b$ul <- b$b + 1.96*b$se
          
          isatOut <- rbind(isatOut, b)
          
        } # End of loop across grades        
      } # End of loop across test outcomes
       
    ## Impacts on School Attendance scores
      attendOut <- NULL
      for (myG in c("All")){ # 2:12, 
        print(paste0("--> Running Estimation for School Attendance for grade ", myG))
        Xs <- regVars
        if (myG >= 6 | myG == "All") Xs <- c(Xs, mvmsXs)
        if (myG == "All"){
          d <- d_sub[d_sub$nGradeLvl %in% 2:12, ]
          Xs <- c(regVars, "fGradeLvl")
        } else {
          d <- d_sub[d_sub$nGradeLvl == myG, ]
        }
        
        b <-  runLms(myData = d, myY = "Pct_Attend100", myLabel = myG, myTrt = trtVars, myXs = Xs,
                     lagXs = c("Pct_Attend100_lag"))
        attendOut <- rbind(attendOut, b)
      }
    
    # Impacts on onTrack status
      ontrackOut <- NULL
      myG <- 9
      print(paste0("--> Running Estimation for OnTrack for grade ", myG))
      Xs <- c(regVars, mvmsXs)
      d <- d_sub[d_sub$nGradeLvl == myG, ]
      
      b <-  runLms(myData = d, myY = "bOnTrack", myLabel = myG, myTrt = trtVars, myXs = Xs,
                   lagXs = c("Pct_Attend100_lag", "isat_mathss_lag", "isat_readss_lag"))
      ontrackOut <- b
    
    # Sample selection into reporting of MVMS
#       mvmsMissOut <- NULL  
#       for (myG in c(7:12, "All")) {
#         print(p0("--> Running Estimation around selection for MVMS missing, for grade", myG))
#         Xs <- c(regVars)
#         d <- d_sub[d_sub$nGradeLvl == myG, ]
#         b <-  runLms(myData = d, myY = MVMS_miss, myLabel = paste(myMvms, myG, sep="-"), myTrt = trtVars, myXs = Xs,
#                        lagXs = c("Pct_Attend100_lag", "isat_mathss_lag", "isat_readss_lag"))
#           mvmsOut <- rbind(mvmsMissOut, b)
#       }
    
    # Impacts on MVMS Outcomes
      mvmsOut <- NULL
      for (myMvms in mvmsVars){
        for (myG in c("All")){ # 7:12
          print(paste0("--> Running Estimation for MVMS for outcome ", myMvms, " and grade ", myG))
          Xs <- c(regVars)
          if (myG == "All"){
            d <- d_sub[d_sub$nGradeLvl %in% 7:12, ]  
          } else {
            d <- d_sub[d_sub$nGradeLvl == myG, ]  
          }         
          b <-  runLms(myData = d, myY = myMvms, myLabel = paste(myMvms, myG, sep="-"), myTrt = trtVars, myXs = Xs,
                       lagXs = c("Pct_Attend100_lag", "isat_mathss_lag", "isat_readss_lag"))
          mvmsOut <- rbind(mvmsOut, b)
        } # End of loop across grades
        # XXX Consider a pooled estimation here (including GradeLvl as a factor)
      } # End of loop across measures
      
    # Collect estimates and output
    
      trtOut <- rbind(isatOut, attendOut, ontrackOut, mvmsOut)
      trtOut$trtGrp <- myTrtGrp
      trtOut$id <- paste0(trtOut$id, "_", myTrtGrp)
      #assign(myTrtGrp %&% "Out", trtOut)
    
    estOut <- rbind(estOut, trtOut)
    
  } # End of loop across groups of treatment variables
  
  #write.csv(estOut, file = "./output/Regression Estimates - site- and org-level estimates.csv")
  write.csv(estOut, file = "./output/Regression Estimates - multiple outcomes - multiple treatment specs.csv")
  

