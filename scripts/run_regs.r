#---------------------------------------------#
#---------------------------------------------#
# RUN OUTCOME-ORIENTED REGRESSION ESTIMATION  # 
#                                             #
# Authors: Nick Mader, Ian Matthew Morey,     # 
#		    and Emily Wiegand		                  #
#---------------------------------------------#
#---------------------------------------------#

## Set up workspace and designate/update file locations

  rm(list=ls())
  #myDir <- "/projects/Integrated_Evaluation_Youth_Support_Services/"
  myDir <- "H:/Integrated Evaluation Project for YSS Providers"
  setwd(myDir)
  dataPath <- "./data/preprocessed-data/"
  load("./data/preprocessed-data/CpsOrg_PP.Rda")
  
## Set up useful libraries and functions   
   
  comment <- function(...){}
  "%&%"   <- function(...){ paste(..., sep="") }
  paste0  <- function(...){ paste(..., sep="") }
  plus    <- function(...){ paste(..., collapse = "+") }
  cn      <- function(x){ colnames(x) }
  ep      <- function(x){ eval(parse(text = x))}
  f.to.c  <- function(f){ return(levels(f)[f]) }
  f.to.n  <- function(f){ return(as.numeric(levels(f)[f])) }
  


#--------------------------------#
#--------------------------------#
### 1. Load Data and Set Specs ###
#--------------------------------#
#--------------------------------#

  ### Preliminary data steps
  
    # Misc rescaling and data conversion
      d_pp$fSch <- factor(d_pp$schlid)
      d_pp$Pct_Attend100 <- d_pp$Pct_Attend*100
      d_pp$Pct_Attend100_lag <- d_pp$Pct_Attend_lag*100
      d_pp$nGradeLvl <- f.to.n(d_pp$fGradeLvl)
  
    # Fill in missing MVMS values
      d_pp$MVMS_miss <- 0
      mvmsVars <- grep("MVMS_.+[^se]$", cn(d_pp), value = T)
      for (m in mvmsVars){
        mNa <- is.na(d_pp[, m])
        d_pp[mNa, m] <- 0
        d_pp$MVMS_miss[mNa] <- 1
      }
  
    # Subset the data
      d_sub <- d_pp[d_pp$year %in% 2012:2013, ]
  
  ### Set up regression lists
    # Dependent Vars 
      depVarList <- c("Pct_Attend100", "isat_mathss", "isat_readss", "bOnTrack") # , "bHsGrad"
        depVarNames <- c("School Attendance Rate", "ISAT Math Score", "ISAT Reading Score", "On-Track Status") # , "Graduating Seniors"
        depVarYLabs <- c("Sch Attendance, % Units", "Scale Score Units", "Scale Score Units", "Impact on Prob of Being On-Track") #, "Impact on HS Graduation")
    # Regressors
      raceXs <- grep("bRace", cn(d_pp), value = T)
        raceXs <- raceXs[-which(raceXs %in% c("bRace_W", "bRace_NonW"))]
      regVars <- c("bLunch_F", "bLunch_R", raceXs, "bGender_Female", "bIEP", "Tract_ViolentCrimes_PerHundr", "Tract_Pct_IncRatKidsLt6_Lt100", "Tract_Pct_NonEnglLangSpokenAtHome")
      lagXs <- c("Pct_Attend100_lag", "isat_mathss_lag", "isat_readss_lag")
      mvmsXs <- grep("MVMS_.+[^e]$", cn(d_pp), value = T)
    # Treatment vars
      collabBin <- "bCollab"
      orgBin <- c("bASM", "bCHASI", "bUCAN", "bYMCA")
      collabHoursCatDoseTrt <-   grep("Collab_fdosage_hours", cn(d_pp), value = T)
      collabDaysCatDoseTrt  <-   grep("Collab_fdosage_days",  cn(d_pp), value = T)
      collabHoursCtsDoseTrt <- c(grep("Collab_dosage_hours",  cn(d_pp), value = T), collabBin)
      collabDaysCtsDoseTrt  <- c(grep("Collab_dosage_days",   cn(d_pp), value = T), collabBin)
      orgHoursCatDoseTrt <- grep("[^Collab]_fdosage_hours", cn(d_pp), value = T)
      orgDaysCatDoseTrt  <- grep("[^Collab]_fdosage_days",  cn(d_pp), value = T)
      orgHoursCtsDoseTrt <- c(grep("[^Collab]_dosage_hours",  cn(d_pp), value = T), orgBin)
      orgDaysCtsDoseTrt  <- c(grep("[^Collab]_dosage_days",   cn(d_pp), value = T), orgBin)  

  
#----------------------------------------------#
#----------------------------------------------#
### 2. Set up regression and post-estimation ###
#----------------------------------------------#
#----------------------------------------------#
    
  # Set up data to use for trials
    myData <- d_sub[d_sub$fGradeLvl=="7",]
    myY <- "isat_mathss"
    myLabel <- "Gr8"
    myTrt <- orgBin
    myXs <- c(regVars, "lep_test", mvmsXs)
    for (v in myTrt){
      myData[is.na(myData[, v]), v] <- 0
    }
    summary(myData[, myTrt])
  
  ### Set up function for linear regression estimation
  runLms <- function(myData, myY, myLabel, myTrt, myXs, lagXs){
    plusTrt <- plus(myTrt)
    plusXs  <- plus(myXs)
    plusLag <- plus(lagXs)
    sumReg <- function(s){ summary(lm(as.formula(paste0(myY, "~", s)), data = myData))}
    rawReg  <- sumReg(plus(plusTrt))
    preReg  <- sumReg(plus(c(plusTrt, plusLag)))
    nowReg  <- sumReg(plus(c(plusTrt, plusXs)))
    adjReg  <- sumReg(plus(c(plusTrt, plusLag, plusXs)))
    schReg  <- sumReg(plus(c(plusTrt, plusLag, plusXs)) %&% " + factor(schlid)")
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
      fitStats[, cn(betas)[!(cn(betas) %in% c("x", "b"))]] <- NA
      betas <- rbind(betas, fitStats)
      
      # Add post-estimation statistics and names
      betas$plusminus <- 1.96*betas$se
      betas$ll <- betas$b - betas$plusminus
      betas$ul <- betas$b + betas$plusminus
      betas$y <- myY
      betas$spec <- spec
      betas$label <- myLabel
      
      # Create unique id for each estimate and output
      betas <- within(betas, id <- paste(y, label, spec, x, sep = "_"))
      assign(spec %&% "Out", betas[, c("spec", "y", "x", "b", "se", "plusminus", "ll", "ul", "t", "p", "sig", "id")])
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
  for (myTrtGrp in c("collabBin", "orgBin", "collabHoursCatDoseTrt", "collabHoursCtsDoseTrt")) { # "collabDaysCatDoseTrt", "collabDaysCtsDoseTrt")
    trtVars <- get(myTrtGrp)
    
    print(paste0("Running trtVars: ", myTrtGrp))
        
    ## Impacts on ISAT scores
      isatOut <- NULL
      for (myG in 4:8){
        for (myT in c("isat_mathss", "isat_readss")){
          print(paste0("--> Running Estimation for ", myT, " for grade ", myG))
          
          # Specify Xs and data
          Xs <- c(regVars, "lep_test")
          if (myG >= 6 | myG == "All") Xs <- c(Xs, mvmsXs)
          d <- d_sub[d_sub$nGradeLvl == myG, ]
          
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
          
        } # End of loop across test outcomes
      } # End of loop across grades
       
    ## Impacts on School Attendance scores
      attendOut <- NULL
      for (myG in 2:12){
        print(paste0("--> Running Estimation for School Attendance for grade ", myG))
        Xs <- regVars
        if (myG >= 6 | myG == "All") Xs <- c(Xs, mvmsXs)
        d <- d_sub[d_sub$nGradeLvl == myG, ]
        
        b <-  runLms(myData = d, myY = "Pct_Attend", myLabel = myG, myTrt = trtVars, myXs = Xs,
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
    
    # Impacts on MVMS Outcomes
      mvmsOut <- NULL
      for (myG in 7:12){
        for (myMvms in mvmsVars){
          print(paste0("--> Running Estimation for MVMS for outcome ", y, " and grade ", myG))
          Xs <- c(regVars) # mvmsXs
          d <- d_sub[d_sub$nGradeLvl == myG, ]
          
          b <-  runLms(myData = d, myY = myMvms, myLabel = paste(myMvms, myG, sep="-"), myTrt = trtVars, myXs = Xs,
                       lagXs = c("Pct_Attend100_lag", "isat_mathss_lag", "isat_readss_lag"))
          mvmsOut <- rbind(mvmsOut, b)
        }
      }
      
    # Collect estimates and output
    
      trtOut <- rbind(isatOut, attendOut, ontrackOut)
      trtOut$trtGrp <- myTrtGrp
      trtOut$id <- paste0(trtOut$id, "_", myTrtGrp)
      #assign(myTrtGrp %&% "Out", trtOut)
    
    estOut <- rbind(estOut, trtOut)
    
  } # End of loop across groups of treatment variables
  
  write.csv(estOut, file = "./output/Regression Estimates - multiple outcomes - multiple treatment specs.csv")
  

#---------------------------------------------#
#---------------------------------------------#
### Generate displays of regression results ###
#---------------------------------------------#
#---------------------------------------------#

  regOut <- read.csv(file = "./output/Regression Estimates - multiple outcomes - multiple treatment specs.csv")
  
  
    # Identify sites with 10 or more observations
#         rowsInReg <- rownames(regData) %in% names(adjReg$residuals)
#         SiteNs_Reg <- table(regData$fShortSite[rowsInReg])
#         SiteNames_NGe10 <- names(sort(SiteNs_Reg[SiteNs_Reg >= 10]))
#         SiteNames_NLt10 <- names(sort(SiteNs_Reg[SiteNs_Reg <  10]))
#         cSiteNames_NLt10 <- paste(SiteNames_NLt10, collapse = ", ")
#         print("Sites " %&% cSiteNames_NLt10 %&% " are excluded due to fewer than 10 valid observations.")
#         
#         plotEff <- as.data.frame(adjReg$coeff[grep(trtVar, rownames(adjReg$coeff)), ])
#         plotEff$myX <- sub(trtVar, "", rownames(plotEff))
#         plotEff <- plotEff[plotEff$myX %in% SiteNames_NGe10,]
#         
#         colnames(plotEff) <- c("b", "se", "t", "p", "myX")
#         
#         estOrder <- order(plotEff$b)
#         plotEff$myX <- factor(plotEff$myX, levels = plotEff$myX[estOrder])
#         plotEff <- plotEff[estOrder,]
#       