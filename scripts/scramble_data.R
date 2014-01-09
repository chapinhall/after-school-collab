#---------------------------------------------#
#---------------------------------------------#
# SCRAMBLING DATA FOR ANALYSIS DEMOS	    # 
#                                             #
# Authors: Nick Mader, Ian Matthew Morey,     # 
#		    and Emily Wiegand		    #  
#---------------------------------------------#
#---------------------------------------------#

##Set up workspace and designate/update file locations

 rm(list=ls())
 #myDir <- "/projects/Integrated_Evaluation_Youth_Support_Services/"
 myDir <- "H:/Integrated Evaluation Project for YSS Providers"
 setwd(myDir)
 dataPath <- "./data/preprocessed-data/" # File path to save scrambled data

## Load data

 try(detach(CpsYss_PP13), silent=T)
 load("./data/preprocessed-data/CpsYss_PP13.Rda")
 cNames <- colnames(CpsYss_PP13)
 keepVars <- c("sid", "SchYear", "Stud_Tract", "schlid", "fGradeLvl", "grade_level",
                "Female", "disab", "fRace", grep("bRace", cNames, value=T), grep("bLunch", cNames, value=T),
                "readss", "mathss", "readgain", "mathgain", "readpl", "mathpl", "mathpl_ME", "readpl_ME", "mathss_pre", "readss_pre",
                "Pct_Attend", "Pct_Attend_pre", "bOnTrack", "bHsGrad",
                "Stud_X", "Stud_Y", grep("Tract_", cNames, value=T), "Stud_CcaNum", "Stud_CcaName",
                "fYssType", "fYssSite", "fAnyYss", "UsedYss") #"Pct_Absent", "Pct_Absent_pre",
                
  CpsYss_PP13 <- CpsYss_PP13[, keepVars]


## Copy the data and randomly resample it

Scram <- CpsYss_PP13
myN <- nrow(Scram)
reorder <- sample(myN, myN, replace = T)
Scram <- Scram[reorder,]
    
## Randomly reassign treatment
# Note: Treatment indicator is binary "UsedYss"

  # Determine percentage of treatment population to be reassigned
	pctToReassign <- 0.2

  # Identify n equivalent to that percent of treatment cases
	nToReassign <- round(pctToReassign*(sum(Scram$UsedYss))) 

  # Select n random control and treatment group rows
	nCps <- nrow(Scram[Scram$UsedYss==0,])
	CpsToReassign <- sample(1:nCps, nToReassign)
      nYss <- nrow(Scram[Scram$UsedYss==1,])
      YssToReassign <- sample(1:nYss, nToReassign)

  # Reassign those rows
      Pre <- Scram
      Scram$UsedYss[Pre$UsedYss==0][CpsToReassign] <- 1
      Scram$UsedYss[Pre$UsedYss==1][YssToReassign] <- 0

  # Confirm that n*2 observations have changed treatment status in final result
	scramCheck <- sum(1*(Pre$UsedYss != Scram$UsedYss))
      

# Assign new treatment cases to treatment sites and remove site name from new control cases
      
  # Get names of sites
      Scram$cYssSite <- as.character(Scram$fYssSite)
      siteNames <- unique(Scram$cYssSite)
      siteNames <- siteNames[siteNames!="None of the Above"]
      
  # Update site names for reassigned obs
      Scram$cYssSite[Pre$UsedYss==0][CpsToReassign] <- sample(siteNames, nToReassign, replace = T)
      Scram$cYssSite[Pre$UsedYss==1][YssToReassign] <- "None of the Above"
    
    # Mask site names
      PreRename <- Scram
      for (sn in sort(siteNames)) {
        siteIndex <- which(siteNames == sn)
        Scram$cYssSite[Scram$cYssSite == sn] <- LETTERS[siteIndex]
      }
      
#### ERW: Not sure I see the reason for these two lines.  Need to check in.
      Scram$fAnyYss  <- factor(Scram$UsedYss, levels=c(0,1), labels=c("Non-Org Alpha", "Org Alpha"))
      Scram$fShortSite <- factor(Scram$cYssSite)
 
## Clear memory and save data to file
      save(Scram, file = paste0(dataPath,"Scram.Rda"))
      rm(Pre, PreRename)
    
