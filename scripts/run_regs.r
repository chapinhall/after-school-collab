#---------------------------------------------#
#---------------------------------------------#
# SCRAMBLING DATA FOR ANALYSIS DEMOS          # 
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
   dataPath <- "./data/preprocessed-data/" # File path to save scrambled data
   
## Set up runs
   runRegs <- 1
   runLine <- 1
   runLogit <- 1
   
#--------------------------#
#--------------------------#
### Regression Estimates ###
#--------------------------#
#--------------------------#

if (1 == runRegs) {
  depVarList <- c("Pct_Attend", "isat_mathss", "isat_readss", "bOnTrack", "bHsGrad") 
    depVarNames <- c("Rate of School Attendance", "ISAT Math Score", "ISAT Reading Score", "On-Track Status", "Graduating Seniors")
    depVarYLabs <- c("Sch Attendance, % Units", "Scale Score Units", "Scale Score Units") #, "Impact on Prob of Being On-Track", "Impact on HS Graduation")
  regVars <- c("bLunch_F", "bLunch_R", "fRace", "Female", "fGradeLvl", "iep_test", "lep_test", "Tract_ViolentCrimes_PerHundr", "Tract_Pct_IncRatKidsLt6_Lt185")
  TrtVars <- c("org", "site") # 
  thinData <- myData[, c(depVarList, depVarList %&% "_pre", regVars, TrtVars)]
  thinData$org  <- relevel(thinData$org,  ref="None")
  thinData$site <- relevel(thinData$site, ref="None")
  
  RunRegs <- function(useData, myDepVarList, myTrtVars, suffix){
    for (depVar in myDepVarList){
  
      for (trtVar in TrtVars) {      
        
        print("Running depvar " %&% depVar %&% ", and trtVar " %&% trtVar)
        
        sGain <- paste0(depVar, " - ", depVar, "_pre")
        regData <- useData[!is.na(useData[,depVar]), ]
        adjReg  <- summary(lm(as.formula(depVar   %&% " ~ bLunch_F + bLunch_R + fRace + Female + fGradeLvl + mathss_pre + readss_pre + Pct_Attend_pre + " %&% trtVar), data = regData))
        
        if (trtVar == "org") {
          print("  Generating reg out by organization")
          rawReg  <- summary(lm(as.formula(depVar   %&% " ~ " %&% trtVar), data = regData))
          gainReg <- summary(lm(as.formula(I(sGain) %&% " ~ " %&% trtVar), data = regData))
          preReg  <- summary(lm(as.formula(paste0(depVar, " ~ ", trtVar, " + ", depVar, "_pre")), data = regData))
          Eff <- data.frame(rbind( rawReg$coeff["UsedYss", c("Estimate", "Std. Error")],
                                  gainReg$coeff["UsedYss", c("Estimate", "Std. Error")],
                                   preReg$coeff["UsedYss", c("Estimate", "Std. Error")],
                                   adjReg$coeff["UsedYss", c("Estimate", "Std. Error")]))
          rownames(Eff) <- c("1: Raw\nDiff", "2: Gain\nCalc", "3: Adj for\nLag", "4: Fully\nAdj")
          colnames(Eff) <- c("b", "se")
          Eff$myX <- rownames(Eff)
          
          Eff$ll <- Eff$b - 1.96*Eff$se
          Eff$ul <- Eff$b + 1.96*Eff$se
          
        } else {
          print("  Generating reg output by Site")
          
          # Identify sites with 10 or more observations
          rowsInReg <- rownames(regData) %in% names(adjReg$residuals)
          SiteNs_Reg <- table(regData$fShortSite[rowsInReg])
          SiteNames_NGe10 <- names(sort(SiteNs_Reg[SiteNs_Reg >= 10]))
          SiteNames_NLt10 <- names(sort(SiteNs_Reg[SiteNs_Reg <  10]))
          cSiteNames_NLt10 <- paste(SiteNames_NLt10, collapse = ", ")
          print("Sites " %&% cSiteNames_NLt10 %&% " are excluded due to fewer than 10 valid observations.")
          
          plotEff <- as.data.frame(adjReg$coeff[grep(trtVar, rownames(adjReg$coeff)), ])
          plotEff$myX <- sub(trtVar, "", rownames(plotEff))
          plotEff <- plotEff[plotEff$myX %in% SiteNames_NGe10,]
          
          colnames(plotEff) <- c("b", "se", "t", "p", "myX")
          
          estOrder <- order(plotEff$b)
          plotEff$myX <- factor(plotEff$myX, levels = plotEff$myX[estOrder])
          plotEff <- plotEff[estOrder,]
          plotEff$ll <- plotEff$b - 1.96*plotEff$se
          plotEff$ul <- plotEff$b + 1.96*plotEff$se
          
        }
        
      } # End of loop across "All treament" versus "by Site" analysis
      
    } # End of loop across dependent variables
    
  } # End of the RunRegs function definition
    
  RunRegs(useData=thinData, myDepVarList = depVarList, myTrtVars = TrtVars, suffix="")

} # End of Regression Analysis
  
  
  