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
  #myDir <- "/projects/Integrated_Evaluation_Youth_Support_Services/"
  myDir <- "H:/Integrated Evaluation Project for YSS Providers"
  setwd(myDir)
  dataPath <- "./data/preprocessed-data/"
  load("./data/preprocessed-data/CpsOrg_PP.Rda")
  
## Set up useful libraries and functions   
   
  comment <- function(...){}
  "%&%"   <- function(...){ paste(..., sep="") }
  paste0  <- function(...){ paste(..., sep="") }
  p0      <- function(...){ paste0(...) }
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
  
    # Create standardized ISAT measures
      vars_m0s1 <- ddply(d_pp, .(fGradeLvl), summarize, 
                           isat_mathss_m0s1 = scale(isat_mathss),
                           isat_readss_m0s1 = scale(isat_readss))
      d_pp <- cbind(d_pp, vars_m0s1[,2:ncol(vars_m0s1)])
  
      # Check output
        aggregate(isat_mathss_m0s1 ~ fGradeLvl, vars_m0s1, mean, na.rm = T)
        aggregate(isat_mathss_m0s1 ~ fGradeLvl, vars_m0s1, sd, na.rm = T)
        with(d_pp, cor(isat_mathss, isat_mathss_m0s1))
        with(d_pp[d_pp$fGradeLvl == 8,], cor(isat_mathss, isat_mathss_m0s1))
  
      # Check how gains vary across grades
        d_pp$isat_mathss_gain <- with(d_pp, isat_mathss - isat_mathss_lag)
        d_pp$isat_readss_gain <- with(d_pp, isat_readss - isat_readss_lag)
        aggregate(d_pp[, c("isat_mathss_gain", "isat_readss_gain")], list(d_pp$fGradeLvl), mean, na.rm = T)
  
    # Fill in missing MVMS values
      d_pp$MVMS_miss <- 0
      mvmsVars <- grep("MVMS_.+[^se]$", cn(d_pp), value = T)
      for (m in mvmsVars){
        mNa <- is.na(d_pp[, m])
        d_pp[mNa, m] <- 0
        d_pp$MVMS_miss[mNa] <- 1
      }
  
    # Subset the data
      d_sub <- d_pp[d_pp$year %in% 2013, ] # 2012:
  
  ### Set up regression lists
    # Dependent Vars 
      depVarList <- c("Pct_Attend100", "isat_mathss", "isat_readss", "bOnTrack") # , "bHsGrad"
        depVarNames <- c("School Attendance Rate", "ISAT Math Score", "ISAT Reading Score", "On-Track Status") # , "Graduating Seniors"
        depVarYLabs <- c("Sch Attendance, % Units", "Scale Score Units", "Scale Score Units", "Impact on Prob of Being On-Track") #, "Impact on HS Graduation")
    # Regressors
      raceXs <- grep("bRace", cn(d_pp), value = T)
        raceXs <- raceXs[-which(raceXs %in% c("bRace_W", "bRace_NonW"))]
      regVars <- c("bLunch_F", "bLunch_R", raceXs, "bGender_Female", "bIEP")
        #"Tract_ViolentCrimes_PerHundr", "Tract_Pct_IncRatKidsLt6_Lt100", "Tract_Pct_NonEnglLangSpokenAtHome"
      lagXs <- c("Pct_Attend100_lag", "isat_mathss_lag", "isat_readss_lag")
      mvmsXs <- grep("MVMS_.+[^e]$", cn(d_pp), value = T)
    # Treatment vars
      collabBin <- "bCollab"
      orgBin <- c("bASM", "bCHASI", "bUCAN", "bYMCA")
      collabHoursCatDoseTrt <-   grep("Collab_fdosage_hours", cn(d_pp), value = T)
      collabDaysCatDoseTrt  <-   grep("Collab_fdosage_days",  cn(d_pp), value = T)
      collabHoursCtsDoseTrt <- c(grep("Collab_dosage_hours",  cn(d_pp), value = T), collabBin)
      collabDaysCtsDoseTrt  <- c(grep("Collab_dosage_days",   cn(d_pp), value = T), collabBin)
      orgHoursCatDoseTrt <-   grep("[^Collab]_fdosage_hours", cn(d_pp), value = T)
      orgDaysCatDoseTrt  <-   grep("[^Collab]_fdosage_days",  cn(d_pp), value = T)
      orgHoursCtsDoseTrt <- c(grep("[^Collab]_dosage_hours",  cn(d_pp), value = T), orgBin)
      orgDaysCtsDoseTrt  <- c(grep("[^Collab]_dosage_days",   cn(d_pp), value = T), orgBin)  
    # Site/Program Treatment indicators
      siteBin <- grep("^org_site", cn(d_pp), value = T)
  
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
    
      trtOut <- rbind(isatOut, attendOut, ontrackOut)
      trtOut$trtGrp <- myTrtGrp
      trtOut$id <- paste0(trtOut$id, "_", myTrtGrp)
      #assign(myTrtGrp %&% "Out", trtOut)
    
    estOut <- rbind(estOut, trtOut)
    
  } # End of loop across groups of treatment variables
  
  #write.csv(estOut, file = "./output/Regression Estimates - site- and org-level estimates.csv")
  write.csv(estOut, file = "./output/Regression Estimates - multiple outcomes - multiple treatment specs.csv")
  

#---------------------------------------------#
#---------------------------------------------#
### Generate displays of regression results ###
#---------------------------------------------#
#---------------------------------------------#

  #regOut <- read.csv(file = "./output/Regression Estimates - multiple outcomes - multiple treatment specs.csv")
  siteOut <- read.csv(file = "./output/Regression Estimates - site-level estimates.csv")
  orgOut <- read.csv(file = "./output/Regression Estimates - site- and org-level estimates.csv")
  
  yLabels <- list("isat_mathss" = c("ISAT Test Score - Math", "% of a year of growth"),
                    "isat_readss" = c("ISAT Test Score- Reading", "% of a year of growth"),
                    "Pct_Attend100" = c("Percent School Attendance", "% school day attendance"))
  
  #-------------------------------------
  ### Plot Site Comparisons of Estimates
  #-------------------------------------
    
    dChasi <- siteOut[grepl("CHASI", siteOut$x) & grepl("All_sch", siteOut$id),]
    dBigNs <- dChasi[dChasi$non0Count >= 10,]
    
    yLabels <- list("isat_mathss" = c("ISAT Test Score - Math", "% of a year of growth"),
                    "isat_readss" = c("ISAT Test Score- Reading", "% of a year of growth"),
                    "Pct_Attend100" = c("Percent School Attendance", "% school day attendance"))
    
    for (myY in c("isat_mathss", "isat_readss", "Pct_Attend100")){
      
      myLabels <- unlist(yLabels[myY])
      plotEff <- dBigNs[grepl(myY, dBigNs$id),]
      estOrder <- order(plotEff$b)
      plotEff$x <- gsub("org_siteCHASI_", "", plotEff$x)
      plotEff$x <- gsub("_", " ", plotEff$x)
      plotEff$x <- factor(plotEff$x, levels = plotEff$x[estOrder])
      
      # Generate comparison across Sites (within school specification)
      ggplot(data = plotEff) +
        geom_bar(aes(x = x, y = b, fill = b), stat = "identity", position = "dodge") +
        geom_errorbar(aes(x = x, ymin = ll, ymax = ul), position = position_dodge(width=0.5), width=0.25, size = 0.5) +
        scale_fill_gradient(low = "#4F91CE", high = "#78A360") + # "#4F91CE", "#78A360", "#FAAF5E"
        ggtitle(p0("Remaining Association Between\nProgram Enrollment and ", myLabels[1])) +
        xlab("Site") +
        ylab(myLabels[2]) +
        theme(title = element_text(size = 10), 
              axis.title = element_text(size = 10, face = "bold"),
              axis.text = element_text(size = 8, face = "bold"),
              legend.position = "none")
      
      ggsave(filename = p0("./output/figures/RegFig_CHASI_bySite_", myY, ".png"), dpi = 600, width = 4.67, height = 3.50)
    }
  
  #----------------------------------------------
  ### Plot Comparisons of Estimate Specifications
  #----------------------------------------------
    
    dOrg <- orgOut[grepl(".*bCHASI.*orgBin$", orgOut$id),]
    dBigNs <- dOrg[dOrg$non0Count >= 10, ]
    
    for (myY in c("isat_mathss", "isat_readss", "Pct_Attend100")){
      
      myLabels <- unlist(yLabels[myY])
      plotEff <- dBigNs[grepl(myY, dBigNs$id),]
      estOrder <- order(plotEff$b)
      plotEff$plotX <- sapply(plotEff$spec, function(s) switch(f.to.c(s),
                                                          "raw" = "Raw Diff",
                                                          "pre" = "Acad Traj",
                                                          "now" = "Demogr",
                                                          "adj" = "Traj + Demogr",
                                                          "sch" = "Traj + Demo\n+ Sch"))
      plotEff$plotX <- factor(plotEff$plotX, levels = c("Raw Diff", "Acad Traj", "Demogr", "Traj + Demogr", "Traj + Demo\n+ Sch"))
      
      # Generate comparison across Sites (within school specification)
      ggplot(data = plotEff) +
        geom_bar(aes(x = plotX, y = b, fill = b), stat = "identity", position = "dodge") +
        geom_errorbar(aes(x = plotX, ymin = ll, ymax = ul), position = position_dodge(width=0.5), width=0.25, size = 0.5) +
        #scale_fill_gradient(low = "#4F91CE", high = "#FAAF5E") + # "#4F91CE", "#78A360", "#FAAF5E"
        scale_fill_hue(h=c("#4F91CE", "#78A360"), l=40)
        ggtitle(p0("Comparison of Estimates Between\nCH+A Enrollment and ", myLabels[1])) +
        xlab("Statistical Controls") +
        ylab(myLabels[2]) +
        theme(title = element_text(size = 10), 
              axis.title = element_text(size = 10, face = "bold"),
              axis.text = element_text(size = 8, face = "bold"),
              legend.position = "none")
      
      ggsave(filename = p0("./output/figures/RegFig_CHASI_bySpec_", myY, ".png"), dpi = 600, width = 4.67, height = 3.50)
    }
  
