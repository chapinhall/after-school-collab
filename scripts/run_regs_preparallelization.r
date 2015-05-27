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
      cleanColnames <- sapply(cn(d_pp), cleanVals)
      names(cleanColnames) <- NULL
      grep("\\.", cleanColnames)
      colnames(d_pp) <- cleanColnames
      #View(cbind(cn(d_pp), z))
  
    # Subset the data
      d_sub <- subset(d_pp, year == 2014)
      d_sub <- d_sub[, !grepl("_dosage_", cn(d_sub))] #  Drop continuously varying treatment columns which we won't use
      d_sub <- d_sub[, !grepl("_fdosage_.+[^(None)]$", cn(d_sub))] # Drop "fdosage" columns that do not end in "None". (These are the ones that include *all* dosage categories *including* a "none" category.)
  
  ### Set up regression lists
  
    depVars <- c("Pct_NonAbsent", "isat_mathss", "isat_readss", "nwea_math_spring_rit", "nwea_read_spring_rit", "explore_compss", "plan_mathss", "plan_readss") # "pct_present_nounex"
      # XXX "bOnTrack",  ... need to add this back in when 2013-14 OnTrack data is finally in
  
    myXs     <- c("bGender_Female", "bRace_B", "bRace_H", "bRace_As", "bRace_M", "bRace_O", "bLunch_R", "bLunch_F", "bIEP", "bEll",
                  "Tract_Pct_IncRatKidsLt6_Lt100", "Tract_ViolentCrimes_PerHundr", "Tract_Pct_VacantUnits", "Tract_Pct_NonEnglLangSpokenAtHome") #"fYear",  ... omitting year controls, since we are running estimates separately by year
    lGrRange <- list(isat_mathss = 4:8, isat_readss = 4:8, nwea_math_spring_rit = 1:8, nwea_read_spring_rit = 1:8,
                     explore_compss = 9, plan_mathss = 10, plan_readss = 10, bOnTrack = 9, Pct_NonAbsent = 4:12)
    lNonTestLags <- list(isat_mathss          = c("Pct_NonAbsent100_lag"),
                         isat_readss          = c("Pct_NonAbsent100_lag"),
                         nwea_math_spring_rit = c("Pct_NonAbsent100_lag"),
                         nwea_read_spring_rit = c("Pct_NonAbsent100_lag"),
                         explore_comps        = c("Pct_NonAbsent100_lag"),
                         bOnTrack             = c("Pct_NonAbsent100_lag"),
                         Pct_NonAbsent        = c("Pct_NonAbsent100_lag"),
                         plan_mathss          = c("Pct_NonAbsent100_lag"),
                         plan_readss          = c("Pct_NonAbsent100_lag"))
    varList_TestLag <- c("isat_mathss", "isat_readss", "nwea_math_spring_rit", "nwea_read_spring_rit", "explore_comp_std",
                         "plan_mathss", "plan_readss", "bOnTrack", "Pct_NonAbsent") 
    lTestLagsByGrade <- list(Gr4  = c("isat_mathss_lag", "isat_readss_lag"),
                             Gr5  = c("isat_mathss_lag", "isat_readss_lag"),
                             Gr6  = c("isat_mathss_lag", "isat_readss_lag"),
                             Gr7  = c("isat_mathss_lag", "isat_readss_lag"),
                             Gr8  = c("isat_mathss_lag", "isat_readss_lag"),
                             Gr9  = c("isat_mathss_lag", "isat_readss_lag"),
                             Gr10 = c("explore_compss_lag"),
                             Gr11 = c("plan_mathss_lag", "plan_readss_lag")) # XXX Will need to update this as the ISAT testing regime give way to NWEA
    #mvmsXs <- grepv("MVMS_.+[^se]$", cn(d_sub))
    mvmsXs <- c("MVMS_AcadEng", "MVMS_EmHealth", "MVMS_Comm", "MVMS_Parent", "MVMS_Peer", "MVMS_Study", "MVMS_Safety", "MVMS_Connect", "MVMS_Respect", "MVMS_miss")
  
    lXsByGrade <- list(Gr6 = mvmsXs, Gr7 = mvmsXs, Gr8 = mvmsXs, Gr9 = mvmsXs, Gr10 = mvmsXs, Gr11 = mvmsXs, Gr12 = mvmsXs)
    schCtrlsList <- c("SchFE") # , "SchRE"
    
    # Treatment vars
      trtCollabBin <- "bCollab"
      trtOrgBin <- c("bASM", "bCHaA", "bUCAN", "bYMCA", "bCPL")
      trtSiteBin <- grepv("^org_site", cn(d_sub))
      trtProgBin <- grepv("^org_prog", cn(d_sub))
  
      collabHoursCatDoseTrt <-   grepv("Collab_fdosage_hours", cn(d_sub))
      collabDaysCatDoseTrt  <-   grepv("Collab_fdosage_days",  cn(d_sub))
      orgHoursCatDoseTrt <-   grepv("[^(Collab)]_fdosage_hours", cn(d_sub))
      orgDaysCatDoseTrt  <-   grepv("[^(Collab)]_fdosage_days",  cn(d_sub))
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
      lTrtRunList <- list(trtOrgBin = trtOrgBin, trtSiteBin = trtSiteBin, trtProgBin = trtProgBin, progCatDoseTrt = progCatDoseTrt)
  
    # Generate a list of all the treatments, in order of intended display, for Excel tables
      trtVarsOut <- NULL
      for (tr in lTrtRunList){
        trVars <- tr
        for (tv in trVars){
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

# Set up trial values for auditing the kernel code
  myY <- "isat_mathss"
  myG <- 8
  treatSpec <- "progCatDoseTrt"
  method <- "SchFE"
  
### Set loop of runs
  
  
### Run estimation specifications in parallel
  
  regOut <- NULL
  for (myY in depVars) {
    
    myYGrades <- lGrRange[myY][[1]]
    for (myG in myYGrades){
      # Subset data, and identify regression controls, by grade and outcome
      useData <- d_sub[d_sub$nGradeLvl == myG,]
      myLags <- lNonTestLags[myY][[1]]
      if (myY %in% varList_TestLag) {
        testLags <- lTestLagsByGrade[p0("Gr", myG)][[1]]
        myLags <- c(myLags, testLags)
      }
      xsByGrade <- unlist(lXsByGrade[p0("Gr", myG)[[1]]])
      fmBase <- paste(myY, "~", paste(c(myXs, myLags, xsByGrade), collapse = " + "))
      
      for (treatSpec in names(lTrtRunList)){
        treatVars <- lTrtRunList[treatSpec][[1]]
        
        # Subset treatment controls to those with meaningful variation, and calculate the number of non-zero/none-[None] values
          trtClass <- NULL
          for (tv in treatVars) trtClass <- c(trtClass, class(useData[, tv]))
          ctsTrtVars <- treatVars[trtClass == "numeric"]
          facTrtVars <- treatVars[trtClass == "factor"]
          # Figure out which data set will be used in regressions, based on drops due to non-treatment regressors
          regCtrls <- gsub("^\\s+|\\s+$", "", unlist(strsplit(chartr("~+", ",,", fmBase), ",")))
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
    
        for (method in schCtrlsList){
    
          print(paste("Running outcome", myY, "for grade", myG, "for treatSpec", treatSpec, "with method", method))
          #print(paste("Specification (minus sch-based controls) is: ", fmTreat))
          if (method == "SchRE"){
            addReg <- summary(lmer(as.formula(paste(fmTreat, "+ (1|fSch)")), data = useData))
          } else if (method == "SchFE"){
            addReg <- summary(lm(  as.formula(paste(fmTreat, "+    fSch ")), data = useData))
          }

          # Save output
          myRegOut <- extractLmStats(addReg)
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
          
          myRegOutSupr$outcome <- myY
          myRegOutSupr$grade <- myG
          myRegOutSupr$id <- paste(myY, treatSpec, myG, method, myRegOutSupr$x, sep = "_")
          regOut <- rbind(regOut, myRegOutSupr)
        } # End of loop across sch level controls

      } # End of loop across treatment specifications
    
    } # End of loop across grades
    
  } # End of Loop across dependent variables

#----------------------------------------------------
### Rescale the estimates to more interpretable units
#----------------------------------------------------
  # This can be either typical growth in the outcome (probably only available for ISAT), or sd of the outcome
  
  # Calculate typical gains
    gainData <- within(d_sub[, c("nGradeLvl", "isat_mathss", "isat_mathss_lag", "isat_readss", "isat_readss_lag")], {
      isat_mathss_gain <- isat_mathss - isat_mathss_lag
      isat_readss_gain <- isat_readss - isat_readss_lag
    })
    gains <- aggregate(cbind(isat_mathss_gain, isat_readss_gain) ~ nGradeLvl, gainData, mean, na.rm = T)
    gains_long <- within(melt(gains, id.vars = "nGradeLvl"), {
      outcome <- gsub("_gain", "", variable)
      grade <- nGradeLvl
      gain <- value
    })
    regOut_gain <- merge(regOut, gains_long[, c("outcome", "grade", "gain")], by = c("outcome", "grade"), all.x = T)
  
  # Calculate standard deviations
    sdCalcs <- NULL
    for (dv in depVars) {
      addCalcs <- aggregate(as.formula(p0(dv, "~ nGradeLvl")), d_sub, sd, na.rm = T)
      sdCalcs <- rbind(sdCalcs, melt(addCalcs, id.var = "nGradeLvl"))
    }
    colnames(sdCalcs) <- c("grade", "outcome", "sd")
    regOut_gainSds <- merge(regOut_gain, sdCalcs, by = c("outcome", "grade"), all.x = T)
  
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
    regOut.Trt <- regOut[!is.na(regOut$trtSum), ]
    regOut.TrtGr <- merge(regOut.Trt, gradeMap, by = "grade", all.x = T)
    regOut.TrtGr$var <- regOut.TrtGr[, "Std. Error"]^2

  # Summarize variables and weights across levels
    varsToWgt <- c("Estimate", "var")
    byVars <- c("GradeGroup", "depvar", "method", "treatSpec", "xSpec", "x")
    byVarsComma <- paste(byVars, collapse = ",")
    byVarsPlus  <- paste(byVars, collapse = " + ")
    dtRegOut.TrtGr <- data.table(regOut.TrtGr, by = byVarsComma)
    dtRegOut.TrtGrpMean <- dtRegOut.TrtGr[, lapply(as.list(.SD)[varsToWgt], weighted.mean, w = trtSum), by = byVarsComma]
      # Note that the use of as.list() is presently a work-around to be able to use weighted.mean here. See http://stackoverflow.com/questions/26019346/weighted-means-for-several-columns-by-groups-in-a-data-table
    dfRegOut.TrtGrpSum  <- aggregate(as.formula(paste("trtSum ~", byVarsPlus)), regOut.TrtGr, sum, na.rm = T)
    dfRegOut.TrtGrp     <- merge(data.frame(dtRegOut.TrtGrpMean), dfRegOut.TrtGrpSum, by = byVars)
    dfRegOut.TrtGrp$var <- dfRegOut.TrtGrp$var / dfRegOut.TrtGrp$trtSum
  
  # Merge and construct output variables consistent with the main file
    dfRegOut.TrtGrp$grade <- dfRegOut.TrtGrp$GradeGroup
    dfRegOut.TrtGrp[, "Std. Error"] <- sqrt(dfRegOut.TrtGrp$var)
    # To reassign: p, sig stars, grade (should be assigned)
    dfRegOut.TrtGrp$p <- 2*(1 - pnorm(abs(dfRegOut.TrtGrp[, "Estimate"]/dfRegOut.TrtGrp[, "Std. Error"])))
    dfRegOut.TrtGrp$sigStars <- sigStarsP(dfRegOut.TrtGrp$p)

    regOut_comb <- rbind(regOut, dfRegOut.TrtGrp[, cn(regOut)]) # 
  
#-----------------
### Output results
#-----------------
  

  # Handle final output
    regOut_supr <- regOut_rescales
    for (col in c("b", "se", "plusminus", "ul", "ll", "p", "t", "b_gain", "se_gain", "b_cohensd", "se_cohensd")){
      regOut_supr[is.na(regOut_supr[, col]), col] <- "-"
    }
  
  # Output unique list of treatment effects, for use in Excel lookups
    myOrgs <- gsub("^b", "", trtOrgBin)
    trtVars <- unique(grepv(paste(myOrgs, collapse = "|"), regOut_supr$x))

### Write output
  write.csv(regOut_supr, "./data/constructed-data/regOut.csv", row.names = FALSE)
  write.csv(trtVarsOut, "./data/constructed-data/regTrtVars.csv", row.names = FALSE)


