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
  library("mfx")
  library("foreach")
  library("doParallel")
  myDir <- "/projects/Integrated_Evaluation_Youth_Support_Services/"
  setwd(myDir)
  dataPath <- "./data/constructed-data/"
  source("~/GitHub/after-school-collab/scripts/helper-functions.r")
  

#--------------------------------#
#--------------------------------#
### 1. Load Data and Set Specs ###
#--------------------------------#
#--------------------------------#

  ### Load necessary data
  
    load("./data/constructed-data/CpsOrg_PP.Rda")
    varMap <- read.csv("./data/variable_name_mapping_used_for_estimation_code.csv", stringsAsFactors = F)[,1:2]
      # Note: because of the legacy of the file, Excel still thinks there should be 6 columns. Taking just the first two extract all of the real content.
  
  ### Preliminary data steps
  
    # Misc rescaling and data conversion
      d_pp$fSch <- factor(d_pp$schlid)
      d_pp$fYear <- factor(d_pp$year)
      d_pp$Pct_NonAbsent100     <- d_pp$Pct_NonAbsent*100
      d_pp$Pct_NonAbsent100_lag <- d_pp$Pct_NonAbsent_lag*100
      d_pp$nGradeLvl <- f.to.n(d_pp$fGradeLvl)
      d_pp <- d_pp[!(d_pp$fGradeLvl %in% c("PK", "KG", "Ungr", "Unknown")),]
  
    # Implement ad hoc data clean-up steps
      d_pp$bIEP[is.na(d_pp$bIEP)] <- 0
  
      # The timing of explore tests changed in 2014, where it appears that 
      d_pp$hasExploreMath     <- 1*!is.na(d_pp$explore_mathss)
      d_pp$hasExploreMath_lag <- 1*!is.na(d_pp$explore_mathss_lag)
      #aggregate(hasExploreMath_lag ~ hasExploreMath + year, d_pp, sum, na.rm = T)
      aggregate(hasExploreMath ~ fGradeLvl + year, d_pp, sum, na.rm = T)
  
      sidsMissingPres <- subset(d_pp, year == 2014 & fGradeLvl == 10, select = sid)
      pretests.for.Gr10Y2014 <- subset(d_pp, year == 2012 & sid %in% sidsMissingPres$sid, select = c("sid", "year", "fGradeLvl", "isat_mathss", "isat_readss"))
      pretests.for.Gr10Y2014 <- within(pretests.for.Gr10Y2014, {
        # alter data so that it will merge in the intended way
        year <- 2014
        isat_mathss_lag2 <- isat_mathss
        isat_readss_lag2 <- isat_readss
      })
      pretestsToMerge <- subset(pretests.for.Gr10Y2014, select = -c(isat_mathss, isat_readss, fGradeLvl))
      d_fill <- merge(d_pp, pretestsToMerge, by = c("sid", "year"), all.x = T)
      d_fill <- within(d_fill, {
        isat_mathss_lag <- ifelse(!is.na(isat_mathss_lag2), isat_mathss_lag2, isat_mathss_lag)
        isat_readss_lag <- ifelse(!is.na(isat_readss_lag2), isat_readss_lag2, isat_readss_lag)
      })
      #(mySub <- head(subset(d_fill, year == 2014 & fGradeLvl == 10, select = c("sid", "year", "fGradeLvl", "isat_mathss_lag", "isat_mathss_lag2"))))
      
      aggregate(isat_mathss_lag ~ fGradeLvl + year, d_fill, function(x) length(!is.na(x)))
        
    # Rename columns to replace all spaces and illegal characters
      cleanVals <- function(txt){
        symbolsClean <- chartr("()-", "___", txt)
        spaceClean <- gsub(" ", "_", symbolsClean)
        punctClean <- gsub(",|\\.|&|/|:|'|@", "", spaceClean)
        multClean <- gsub("_+", "_", punctClean)
        endClean <- gsub("_$", "", multClean)
        return(endClean)
      }
      cleanVals("ab-s/df(,) - .fa::b-sd&'f(,) &/- .:f)") # This is a test example to see if the function worked
      cleanColnames <- sapply(cn(d_fill), cleanVals)
      names(cleanColnames) <- NULL
      grep("\\.", cleanColnames)
      colnames(d_fill) <- cleanColnames
      #View(cbind(cn(d_fill), z))
  
    # Subset the data
      d_sub <- subset(d_fill, year == 2014)
      d_sub <- d_sub[, !grepl("_dosage_", cn(d_sub))] #  Drop continuously varying treatment columns which we won't use
      d_sub <- d_sub[, !grepl("_fdosage_.+[^(None)]$", cn(d_sub))] # Drop "fdosage" columns that do not end in "None". (These are the ones that include *all* dosage categories *including* a "none" category.)
  
    # Update the MVMS constructions
      # This is an ad-hoc attempt to specify a more parsimonious model, to reduce colinearity and clean some previously-uncertain data processing
      # This is starting by removing just MVMS_Respect and MVMS_Comm which have lower response rates in 2014. This may later involve an even more parsimonious specification.
      # See the "sandbox-for-mvms-analysis.R to see these demos
      
        mvmsXs <- c("MVMS_AcadEng", "MVMS_EmHealth", "MVMS_Parent", "MVMS_Peer", "MVMS_Study", "MVMS_Safety", "MVMS_Connect")
  
      # Reconstruct the "missing" variable
        d_sub$MVMS_Miss <- 0
        for (m in mvmsXs){
          d_sub$MVMS_Miss[d_sub[, m] == 0] <- 1
        }
      # Ensure that all responses are consistent with missing if any of them is, for the sake of clean estimation
        d_sub[d_sub$MVMS_Miss == 1, mvmsXs] <- 0
        colMeans(subset(d_sub, MVMS_Miss == 1, mvmsXs))
          # Looks like it worked
  
  #--------------------------
  ### Set up regression lists
  #--------------------------
  
    #depVars <- c("bOnTrack") # , "onTimeGrad_sophom", "onTimeGrad_junior", "onTimeGrad_senior"
    depVars <- c("Pct_NonAbsent", "isat_mathss", "isat_readss", "nwea_math_spring_rit", "nwea_read_spring_rit",
                 "explore_mathss", "explore_readss", "plan_mathss", "plan_readss", "psae_mathss", "psae_readss",
                 "bOnTrack", "onTimeGrad_sophom", "onTimeGrad_junior", "onTimeGrad_senior")
    binDeps <- c("bOnTrack", "onTimeGrad_sophom", "onTimeGrad_junior", "onTimeGrad_senior")
    myXs     <- c("bGender_Female", "bRace_B", "bRace_H", "bRace_As", "bRace_M", "bRace_O", "bLunch_R", "bLunch_F", "bIEP", "bEll",
                  "bAlleg", "bAlleg_yr", "bSubstant", "bSubstant_yr",
                  "Tract_Pct_IncRatKidsLt6_Lt100", "Tract_ViolentCrimes_PerHundr", "Tract_Pct_VacantUnits", "Tract_Pct_NonEnglLangSpokenAtHome") #"fYear",  ... omitting year controls, since we are running estimates separately by year
    lGrRange <- list(isat_mathss = 4:8, isat_readss = 4:8, nwea_math_spring_rit = 1:8, nwea_read_spring_rit = 1:8,
                     explore_mathss = 9, explore_readss = 9, plan_mathss = 10, plan_readss = 10, psae_mathss = 11, psae_readss = 11,
                     bOnTrack = 9, onTimeGrad_sophom = 10, onTimeGrad_junior = 11, onTimeGrad_senior = 12, Pct_NonAbsent = 4:12)
    lNonTestLags <- list(isat_mathss          = c("Pct_NonAbsent100_lag"),
                         isat_readss          = c("Pct_NonAbsent100_lag"),
                         nwea_math_spring_rit = c("Pct_NonAbsent100_lag"),
                         nwea_read_spring_rit = c("Pct_NonAbsent100_lag"),
                         explore_mathss       = c("Pct_NonAbsent100_lag"),
                         explore_readss       = c("Pct_NonAbsent100_lag"),
                         plan_mathss          = c("Pct_NonAbsent100_lag"),
                         plan_readss          = c("Pct_NonAbsent100_lag"),
                         psae_mathss          = c("Pct_NonAbsent100_lag"),
                         psae_readss          = c("Pct_NonAbsent100_lag"),
                         bOnTrack             = c("Pct_NonAbsent100_lag"),
                         onTimeGrad_sophom    = c("Pct_NonAbsent100_lag"),
                         onTimeGrad_junior    = c("Pct_NonAbsent100_lag"),
                         onTimeGrad_senior    = c("Pct_NonAbsent100_lag"),
                         Pct_NonAbsent        = c("Pct_NonAbsent100_lag")
                    )
    varList_TestLag <- c("isat_mathss", "isat_readss", "nwea_math_spring_rit", "nwea_read_spring_rit", "explore_mathss", "explore_readss",
                         "plan_mathss", "plan_readss", "psae_mathss", "psae_readss", "bOnTrack", "Pct_NonAbsent") # These are the dependent variables which use the below lagged controls
    lTestLagsByGrade <- list(Gr4  = c("isat_mathss_lag", "isat_readss_lag"),
                             Gr5  = c("isat_mathss_lag", "isat_readss_lag"),
                             Gr6  = c("isat_mathss_lag", "isat_readss_lag"),
                             Gr7  = c("isat_mathss_lag", "isat_readss_lag"),
                             Gr8  = c("isat_mathss_lag", "isat_readss_lag"),
                             Gr9  = c("isat_mathss_lag", "isat_readss_lag"), # XXX Will need to update this as the ISAT testing regime give way to NWEA
                             Gr10 = c("isat_mathss_lag", "isat_readss_lag"), # this is pending update of how timing of EPAS tests is handled "explore_mathss_lag", "explore_readss_lag"
                             Gr11 = c("plan_mathss_lag", "plan_readss_lag")) 
    #mvmsXs <- grepv("MVMS_.+[^se]$", cn(d_sub))
    #mvmsXs <- c("MVMS_AcadEng", "MVMS_EmHealth", "MVMS_Comm", "MVMS_Parent", "MVMS_Peer", "MVMS_Study", "MVMS_Safety", "MVMS_Connect", "MVMS_Respect", "MVMS_miss")
    mvmsXs <- mvmsXs # This is specified above in the data setup
  
    lXsByGrade <- list(Gr6 = mvmsXs, Gr7 = mvmsXs, Gr8 = mvmsXs, Gr9 = mvmsXs, Gr10 = mvmsXs, Gr11 = mvmsXs, Gr12 = mvmsXs)
    schCtrlsList <- c("SchFE") # , "SchRE"
    
    # Treatment vars
      trtCollabBin <- "bCollab"
      trtOrgBin <- c("bASM", "bCHaA", "bUCAN", "bYMCA", "bCPL", "bCHA")
      trtSiteBin <- grepv("^org_site", cn(d_sub))
      trtProgBin <- grepv("^org_prog", cn(d_sub))
  
      collabHoursCatDoseTrt <- grepv("Collab_fdosage_hours", cn(d_sub))
      collabDaysCatDoseTrt  <- grepv("Collab_fdosage_days",  cn(d_sub))
      orgHoursCatDoseTrt    <- grepv("[^(Collab)]_fdosage_hours", cn(d_sub))
      orgDaysCatDoseTrt     <- grepv("[^(Collab)]_fdosage_days",  cn(d_sub))
      #collabHoursCtsDoseTrt <- c(grepv("Collab_dosage_hours",  cn(d_sub)), collabBin)
      #collabDaysCtsDoseTrt  <- c(grepv("Collab_dosage_days",   cn(d_sub)), collabBin)
      #orgHoursCtsDoseTrt <- c(grepv("[^(Collab)]_dosage_hours",  cn(d_sub)), orgBin)
      #orgDaysCtsDoseTrt  <- c(grepv("[^(Collab)]_dosage_days",   cn(d_sub)), orgBin)  
    
    # For categorical treatments, use org/program hours of dosage if available, or days if not
      #opStubs <- unique(with(d_sub, paste(org, prog, sep = "_"))) ... don't have these fields... they only exist in the enrollment data files
      progStubs <- unique(gsub("_fdosage_(hours|days)_None", "", orgHoursCatDoseTrt))
      progCatDoseTrt <- sapply(progStubs, function(x) {
        hrsTable <- table(d_sub[, p0(x, "_fdosage_hours_None")])
        dayTable <- table(d_sub[, p0(x, "_fdosage_days_None")])
        print(x)
        if (length(hrsTable[hrsTable!=0]) != 1) {
          return(p0(x, "_fdosage_hours_None"))
        } else if (length(dayTable[dayTable!=0]) != 1){
          return(p0(x, "_fdosage_days_None"))
        }
      })
      progCatDoseTrt <- unlist(progCatDoseTrt); names(progCatDoseTrt) <- NULL
  
    # Specify final treatment run list
      lTrtRunList <- list(trtCollabBin = trtCollabBin, trtOrgBin = trtOrgBin, trtSiteBin = trtSiteBin, trtProgBin = trtProgBin, progCatDoseTrt = progCatDoseTrt)
  
    # Generate a list of all the treatments, in order of intended display, for Excel tables
      trtVarsOut <- NULL
      for (tr in lTrtRunList){
        trVars <- tr
        for (tv in trVars){
          print(paste("treat var is ", tv))
          if (class(d_sub[, tv])=="factor"){
            tvNames <- levels(d_sub[, tv])
            tvNames <- tvNames[!grepl("\\[None\\]", tvNames)]
            trtVarsOut <- c(trtVarsOut, p0(tv, tvNames))
          } else {
            trtVarsOut <- c(trtVarsOut, tv)
          }
        }
      }
  
#--------------------------------------------#
#--------------------------------------------#
###  Loop Estimation For Multiple Outcomes ###
#--------------------------------------------#
#--------------------------------------------#
    
  ### Set loop of runs
    regRuns <- NULL
    for (yr in 2014){
    for (myY in depVars) {
      myYGrades <- lGrRange[myY][[1]]
    for (myG in myYGrades){
    for (treatSpec in names(lTrtRunList)){
      treatVars <- lTrtRunList[treatSpec][[1]]
    for (method in schCtrlsList){
      regRuns <- rbind(regRuns, data.frame(myY = myY, myG = myG, myYear = yr, treatSpec = treatSpec, method = method, stringsAsFactors = F))
    }}}}}
  
  ### Run estimation specifications in parallel
  
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  system.time({
  regOut <- foreach(r = 1:nrow(regRuns), .combine = rbind) %dopar% {
    library(mfx)
    # Get run parameters
      for (runParm in cn(regRuns)){
        assign(runParm, regRuns[r, runParm])
      }
    
    # Subset data, and identify regression controls, by grade and outcome
      useData <- subset(d_sub, nGradeLvl == myG & year == myYear)
      myLags <- lNonTestLags[myY][[1]]
      if (myY %in% varList_TestLag) {
        testLags <- lTestLagsByGrade[p0("Gr", myG)][[1]]
        myLags <- c(myLags, testLags)
      }
      xsByGrade <- unlist(lXsByGrade[p0("Gr", myG)[[1]]])
      fmBase <- paste(myY, "~", paste(c(myXs, myLags, xsByGrade), collapse = " + "))
    
    # Subset treatment controls to those with meaningful variation, and calculate the number of non-zero/none-[None] values
      trtClass <- NULL
      treatVars <- lTrtRunList[treatSpec][[1]]
      for (tv in treatVars) trtClass <- c(trtClass, class(useData[, tv]))
      ctsTrtVars <- treatVars[trtClass == "numeric"]
      facTrtVars <- treatVars[trtClass == "factor"]
      # Figure out which data set will be used in regressions, based on drops due to non-treatment regressors
      regCtrls <- gsub("^\\s+|\\s+$", "", unlist(strsplit(chartr("~+", ",,", fmBase), ","))) # This separates the regression formula into a vector of the variable names
      regData <- na.omit(useData[, c(regCtrls, treatVars)]) # complete.cases() also apparently allows some flexibility to flag subset of columns
      
      useTrt <- NULL; trtVarNs <- NULL
      for (ctv in ctsTrtVars) {
        if (var(regData[, ctv] != 0, na.rm = T)) {
          useTrt <- c(useTrt, ctv)
          addNs <- data.frame(x = ctv, N = sum(useData[, ctv] != 0, na.rm = T))
          trtVarNs <- rbind(trtVarNs, addNs)
        }          
      }
      for (ftv in facTrtVars) {
        myTable <- table(regData[regData[, ftv] != "[None]", ftv])
        if (length(myTable[myTable!=0]) > 0) {
          useTrt <- c(useTrt, ftv)
          dfTable <- data.frame(myTable)
          dfTable$x <- p0(ftv, dfTable$Var1)
          trtVarNs <- rbind(trtVarNs, dfTable[!grepl("\\[None\\]", dfTable$x), c("x", "Freq")])
        }
      }
      colnames(trtVarNs) <- c("x", "N")
      fmTreat   <- plusPaste(c(fmBase, useTrt))

    # Run analysis based on specification of school effects
      if (method == "SchRE"){
        if (myY %in% binDeps) {
          addReg <- summary(logitmfx(as.formula(paste(fmTreat, "+ (1|fSch)")), data = useData))
        } else {
          addReg <- summary(lmer(    as.formula(paste(fmTreat, "+ (1|fSch)")), data = useData))
        }
      } else if (method == "SchFE"){
        if (myY %in% binDeps) {
          addReg <- logitmfx(  as.formula(paste(fmTreat, "+ fSch")), data = useData)
        } else {
          addReg <- summary(lm(as.formula(paste(fmTreat, "+ fSch ")), data = useData))
        }
      }
    
    # Save output
      myRegOut <- extractRegStats(addReg)
      myRegOut <- myRegOut[!grepl("fSch", myRegOut$x),]
      
      myRegOutSupr <- merge(myRegOut, trtVarNs, by = "x", all.x = T) # The "Supr" is for "supressed"
      for (suprCol in c("b", "se", "plusminus", "ul", "ll", "p", "t")){
        myRegOutSupr[ifelse(is.na(myRegOutSupr$N), FALSE, myRegOutSupr$N < 10), suprCol] <- NA
      } 
      myRegOutSupr[ifelse(is.na(myRegOutSupr$N), FALSE, myRegOutSupr$N < 10), "sigStars"] <- ""
      
    # Add documentation to the output which contains a statement about all of the controls in prose
      ctrls <- myRegOutSupr$x[!grepl(p0("N|df|(adj.r.squared)|", paste(useTrt, collapse = "|")), myRegOutSupr$x)]
      ctrlNames <- merge(ctrls, varMap, by.x = "x", by.y = "VarName", all.x = T)
      ctrlList <- p0("This statistical analysis accounts for youth differences in the following indicators: ", paste(ctrlNames$VarDesc[!is.na(ctrlNames$VarDesc)], collapse = ", "), ".")
      ctrlListRow <- sapply(cn(myRegOutSupr), function(x) NA)
      ctrlListRow$x <- "Controls"; ctrlListRow$descr <- ctrlList
      myRegOutSupr$descr <- ""
      myRegOutSupr <- rbind(myRegOutSupr, ctrlListRow)
    
      myRegOutSupr$outcome   <- myY
      myRegOutSupr$grade     <- myG
      myRegOutSupr$year      <- myYear
      myRegOutSupr$treatSpec <- treatSpec
      myRegOutSupr$method    <- method
      myRegOutSupr$id        <- paste(myY, myG, myYear, treatSpec, method, myRegOutSupr$x, sep = "_")
      #regOut <- rbind(regOut, myRegOutSupr)
      return(myRegOutSupr)
    
  } # End of parallelization loop
  }) # End of calculation of system time
  
  stopCluster(cl)
  
#----------------------------------------------------
### Rescale the estimates to more interpretable units
#----------------------------------------------------
  # This can be either typical growth in the outcome (probably only available for ISAT), or sd of the outcome
  
  # Calculate typical gains
    gainData <- within(d_sub[, c("nGradeLvl", "year", "isat_mathss", "isat_mathss_lag", "isat_readss", "isat_readss_lag")], {
      isat_mathss_gain <- isat_mathss - isat_mathss_lag
      isat_readss_gain <- isat_readss - isat_readss_lag
    })
    gains <- aggregate(cbind(isat_mathss_gain, isat_readss_gain) ~ nGradeLvl + year, gainData, mean, na.rm = T)
    gains_long <- within(melt(gains, id.vars = c("nGradeLvl", "year")), {
      outcome <- gsub("_gain", "", variable)
      grade <- nGradeLvl
      gain <- value
    })
    regOut_gain <- merge(regOut, gains_long[, c("outcome", "grade", "year", "gain")], by = c("outcome", "grade", "year"), all.x = T)
  
  # Calculate standard deviations
    sdCalcs <- NULL
    for (dv in depVars) {
      addCalcs <- aggregate(as.formula(p0(dv, "~ nGradeLvl + year")), d_sub, sd, na.rm = T)
      sdCalcs <- rbind(sdCalcs, melt(addCalcs, id.vars = c("nGradeLvl", "year")))
    }
    colnames(sdCalcs) <- c("grade", "year", "outcome", "sd")
    regOut_gainSds <- merge(regOut_gain, sdCalcs, by = c("outcome", "grade", "year"), all.x = T)
  
  # Calculate rescaled values
    regOut_rescales <- within(regOut_gainSds, {
      b_gain  <- as.numeric(b)  / gain
      se_gain <- as.numeric(se) / gain
      
      b_cohensd  <- as.numeric(b)  / sd
      se_cohensd <- as.numeric(se) / sd
    })
  
#-----------------------------------------
### Collapse estimates within grade groups
#-----------------------------------------
  # This improves the statistical power and interpretability of the estimates
  # Merge in grade levels
    gradeMap <- data.frame(matrix(c("PK", "K", as.character(1:12),
                                  rep("PK5", 7), rep("Gr68", 3), rep("GrHs", 4)), ncol = 2))
    colnames(gradeMap) <- c("grade", "GradeGroup")
    regOut.Trt   <- regOut_rescales[!is.na(regOut_rescales$N), ]
    regOut.TrtGr <- merge(regOut.Trt, gradeMap, by = "grade", all.x = T)
    regOut.TrtGr[, c("var", "var_gain", "var_cohensd")] <- regOut.TrtGr[, c("se", "se_gain", "se_cohensd")]^2

  # Summarize variables and weights across levels
    byVars    <- c("GradeGroup", "outcome", "method", "treatSpec", "x", "year")
    byVarsPlus <- paste(byVars, collapse = " + ")
    #regOut.TrtGr <- regOut.TrtGr[with(regOut.TrtGr, treatSpec == "trtOrgBin" & outcome == "isat_mathss" & grepl("CHaA", x)),] # XXX For the sake of troubleshooting
  
    # Form separate weights for the point estimates and variances
    trtGrpSums  <- aggregate(as.formula(paste("N ~", byVarsPlus)), regOut.TrtGr, sum, na.rm = T)
      colnames(trtGrpSums)[cn(trtGrpSums) == "N"] <- "GrpN"
    regOut.TrtGr <- merge(regOut.TrtGr, trtGrpSums, by = byVars)
    regOut.TrtGr <- within(regOut.TrtGr, {
      bWgt <-  N / GrpN
      vWgt <- (N / GrpN)^2
      # We need to hand-roll our weights, because weighted.mean() will not handle the variance weights properly, since they're
      #   not properly weights (i.e) they don't sum to 1 and shouldn't be coerced to
      b_estSummand   <-   b*bWgt;   b_gainSummand <-   b_gain*bWgt;   b_cohensdSummand <-   b_cohensd*bWgt
      var_estSummand <- var*vWgt; var_gainSummand <- var_gain*vWgt; var_cohensdSummand <- var_cohensd*vWgt
    })
    regOut.TrtGrAgg <- aggregate(regOut.TrtGr[, c("N", grepv("Summand", cn(regOut.TrtGr)))], as.list(regOut.TrtGr[, byVars]), sum, na.rm = T)
    colnames(regOut.TrtGrAgg) <- gsub("Summand", "", cn(regOut.TrtGrAgg))
    regOut.TrtGrAgg_eff <- melt(regOut.TrtGrAgg, id = c(byVars, "N"))
    regOut.TrtGrAgg_eff[, c("calc", "spec")] <- t(sapply(regOut.TrtGrAgg_eff$variable, function(x) strsplit(as.character(x), "_")[[1]]))
    regOut.TrtGrAgg_long <- dcast(regOut.TrtGrAgg_eff[, !grepl("variable", cn(regOut.TrtGrAgg_eff))],
                                  as.formula(p0(paste(c(byVars, "N", "spec"), collapse = "+"), " ~ calc")))
    regOut.TrtGrAgg_long <- rename(regOut.TrtGrAgg_long, c(GradeGroup = "grade"))
  
  # Create statements of variables controlled for in each analysis
    # The logic is: if the controls are the same for all grades, take that value. Otherwise, return "See statements of controls for the analysis of each grade separately"
    estByVars    <- c("outcome", "grade", "year", "treatSpec", "method")
    estByVarsGrp <- gsub("grade", "GradeGroup", estByVars)
    estByVarsPlus <- paste(estByVarsGrp, collapse = " + ")
    descInfo <- unique(regOut_rescales[grep("Controls", regOut_rescales$x), c(estByVars, "descr")])
    descInfoGr <- merge(descInfo, gradeMap, by = "grade")
    descInfoAgg   <- aggregate(as.formula(p0("descr ~ ", estByVarsPlus)), descInfoGr, function(x) return(c(n = length(unique(x)), first = x[1])))
    descInfoComb <- merge(descInfoGr, descInfoAgg, by = estByVarsGrp)
    descInfoComb$descr <- ifelse(descInfoComb$descr.y[,1] == 1, descInfoComb$descr.y[,2], "See statements of controls for each separate grade.")
    #View(descInfoComb[, c(estByVars, "descr")])
    descInfoGrComb <- rename(descInfoComb[, c(estByVarsGrp, "descr")], c(GradeGroup = "grade"))
  
  # Merge and construct output variables consistent with the main file
    # Put non-aggregated estimates into the same shape
      byVarsMod <- gsub("GradeGroup", "grade", byVars)
      regOut.TrtGr_mod <- rename(regOut.TrtGr, c(b = "b_est", var = "var_est"))
      regOut.TrtGr_eff <- melt(regOut.TrtGr_mod[, c(byVarsMod, "N", "b_est", "var_est", "b_gain", "var_gain", "b_cohensd", "var_cohensd")], id = c(byVarsMod, "N"))
      regOut.TrtGr_eff[, c("calc", "spec")] <- t(sapply(regOut.TrtGr_eff$variable, function(x) strsplit(as.character(x), "_")[[1]]))
      regOut.TrtGr_long <- dcast(regOut.TrtGr_eff[, !grepl("variable", cn(regOut.TrtGrAgg_eff))],
                                 as.formula(p0(paste(c(byVarsMod, "N", "spec"), collapse = "+"), " ~ calc")))
      
    # Combine aggregated and non-aggregated estimates, and add stats columns
      cnAgg <- cn(regOut.TrtGrAgg_long)
      cnNon <- cn(regOut.TrtGr_long)
      regOut.Trt <- rbind(regOut.TrtGr_long, regOut.TrtGrAgg_long)
      regOut.Trt <- within(regOut.Trt, {
        se <- sqrt(var)
        t <- b/se
        p <- 2*(1 - pnorm(abs(t)))
        sigStars <- sigStarsP(p)
        plusminus <- 1.96*se
        #ll <- b - plusminus
        #ul <- b + plusminus
      })
  
  # Output a slice of output to spot-check accurate results for the grade collapse
    spotCheck <- subset(regOut.Trt, outcome == "Pct_NonAbsent" & grade %in% c(9:12, "GrHs") & treatSpec == "trtOrgBin" & x == "bASM", select = c("outcome", "grade", "year", "treatSpec", "method", "spec", "x", "b", "se", "N"))
    write.csv(spotCheck, "./data/constructed-data/Data to spot check gr aggregations.csv")
  
#-----------------
### Output results
#-----------------
  
  # Handle final output
    regOut.NonTrt_supr  <- subset(regOut_rescales[, !grepl("cohensd|var", cn(regOut_rescales))], is.na(N))
    regOut.Trt_supr     <- regOut.Trt[, !grepl("var", cn(regOut.Trt))]
    
    for (col in c("b", "se", "plusminus", "p", "t", "sigStars")){ # "ul", "ll", 
      regOut.NonTrt_supr[is.na(regOut.NonTrt_supr[, col]), col] <- "-"
      regOut.Trt_supr[is.na(regOut.Trt_supr[, col]) | regOut.Trt_supr$N <= 10, col] <- "-"
    }
  
  # Subset our ordered list of treatment variables down to only those that appeared in the regression
    # This catches cases such as variables that were created for programs in the data, but with enrollment in school years beyond those we can analyze.
    trtVarsOut <- trtVarsOut[trtVarsOut %in% regOut.Trt_supr$x]
    #dropped <- trtVarsOut[!(trtVarsOut %in% regOut.Trt_supr$x)]
  
  # Add in descriptive information on variables that were controlled for
    ctrlMatchCols <- c("outcome", "grade", "year", "treatSpec", "method")
    descrInfoGr  <- regOut[regOut$descr != "", c(ctrlMatchCols, "descr")]
    descrInfoGrp <- descInfoGrComb
    descrInfo <- rbind(descrInfoGr, descrInfoGrp)
  
    regOut.Trt_Specs <- unique(regOut.Trt_supr[, c(ctrlMatchCols, "spec")])
    regOut.Trt_Ctrls <- merge(regOut.Trt_Specs, descrInfo, by = ctrlMatchCols)
    regOut.Trt_Ctrls$x <- "Controls"
    regOut.Trt_supr$descr <- ""
    cnT <- cn(regOut.Trt_supr); cnC <- cn(regOut.Trt_Ctrls)
    cols.to.fill <- cnT[!(cnT %in% cnC)]
    regOut.Trt_Ctrls[, cols.to.fill] <- "-"
    regOut.Trt_supr <- rbind(regOut.Trt_supr, regOut.Trt_Ctrls)

  # Comple IDs, column selection and supression details
    regOut.NonTrt_supr <- regOut.NonTrt_supr[, c("id", "b", "se", "b_gain", "se_gain", "plusminus", "sigStars")]
  
    regOut.Trt_supr$id <- with(regOut.Trt_supr, paste(outcome, grade, year, treatSpec, method, spec, x, sep = "_"))
    regOut.Trt_supr <- regOut.Trt_supr[, c("id", "b", "se", "plusminus", "sigStars", "N", "descr")]
    regOut.Trt_supr$sigStars <- ifelse(regOut.Trt_supr$sigStars == "-", "", regOut.Trt_supr$sigStars)
    
    write.csv(regOut.NonTrt_supr, "./data/constructed-data/regFactors.csv", row.names = FALSE)
    write.csv(regOut.Trt_supr,    "./data/constructed-data/regTrts.csv",    row.names = FALSE)
  
  # Output unique list of treatment effects by partner organization, for use in Excel lookups
    myOrgs <- gsub("^b", "", c(trtOrgBin, trtCollabBin))
    for (org in myOrgs){
      myTrtVars <- trtVarsOut[grepl(org, trtVarsOut)] # XXX There may be more elegant/reliable ways to ensure clean association of treatment variables with partners
      myTrtEsts <- regOut.Trt_supr[grepl(p0(org, "|Controls"), regOut.Trt_supr$id),]
      write.csv(myTrtEsts, p0("./data/constructed-data/regTrts_",    org, ".csv"), row.names = FALSE)
      write.csv(myTrtVars, p0("./data/constructed-data/regTrtVars_", org, ".csv"), row.names = FALSE)
    }
  
