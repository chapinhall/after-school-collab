#----------------------------------------------------#
#----------------------------------------------------#
# SANDBOX FOR EXPLORING PROPERTIES OF THE MVMS DATA  # 
#                                                    #
# Author: Nick Mader                                 # 
#                                                    #
#----------------------------------------------------#
#----------------------------------------------------#

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
  
### Load necessary data, and subset
  
  load("./data/constructed-data/CpsOrg_PP.Rda")
  
  mvmsVars <- grep("MVMS.*(?<!se|miss)$", cn(d_pp), value = T, perl = T)
  acadVars <- c("Pct_NonAbsent", "isat_mathss", "isat_readss", "explore_compss", "plan_mathss", "plan_readss")
  myXs     <- c("bGender_Female", "bRace_B", "bRace_H", "bRace_As", "bRace_M", "bRace_O", "bLunch_R", "bLunch_F", "bIEP", "bEll",
                "Tract_Pct_IncRatKidsLt6_Lt100", "Tract_ViolentCrimes_PerHundr", "Tract_Pct_VacantUnits", "Tract_Pct_NonEnglLangSpokenAtHome") #"fYear",  ... omitting year controls, since we are running estimates separately by year
  ostInds <- c("bASM", "bCHaA", "bUCAN", "bYMCA", "bCPL")
  d_mvms <- d_pp[, mvmsVars]
  d_mvms[d_pp$MVMS_miss,]
  table(d_pp$fGradeLvl[d_pp$MVMS_miss == 1])
  
  d_upd <- d_pp
  for(m in mvmsVars){
    d_upd[, p0(m, "NA")] <- ifelse(d_upd[, m] == 0, NA, d_upd[,m])
  }
  head(subset(d_upd[, c(grepv("MVMS_", cn(d_upd)), "fGradeLvl")], fGradeLvl == 9))
  
  d_9  <- subset(d_upd, fGradeLvl == 9, select =  c(grepv("MVMS_", cn(d_upd)), grepv("_lag$", cn(d_upd)), acadVars, myXs, ostInds, "bCollab", "fGradeLvl", "year"))  
  
### Examine correlations within MVMS
  (mvmsCor <- cor(d_9[, mvmsVars], use = "pairwise"))
  mvmsCor[mvmsCor == 1] <- NA
  summary(mvmsCor)
    # There are many extremely correlated values. Median values around 0.85, although MVMS_Parent is lower at 0.67, and MVMS_Respect is extremely low.
    # Might be due to years where they weren't assessed? See below for examination of non-response
  for (y in 2011:2014){
    print(paste("Reporting correlations for year", y))  
    (mvmsCor <- cor(d_9[d_9$year == y, mvmsVars], use = "pairwise"))
    mvmsCor[mvmsCor == 1] <- NA
    print(summary(mvmsCor))
  }
  # Here, *everything* is correlated at about 0.83, except for MVMS_Comm which has lower correlation in 2014. An at-least partial explanation is a lower response rate in 2014
  # MVMS_Respect is not available in 2014, and has incredibly low correlation--median of <0.01 in other years. Reason appears to be incredibly low response rate.
  
  
### Examine correlations of MVMS with academic outcomes and characteristics
  charsToCor <- c("Pct_NonAbsent", "bRace_H", "bRace_B", "bGender_Female", "bLunch_F", "bIEP", "bEll")
  mvmsToCor  <- grepv("MVMS_.+NA", cn(d_9))
  (charCor <- cor(d_9[d_9$year == 2014, c(charsToCor, mvmsToCor)], use = "pairwise")[mvmsToCor, charsToCor])
    # Observations of note (abs(rho)>0.1)
    # Non-abs: Peer, Study, Connect (all pos)
    # Hisp: parent (neg), study (neg)
    # Black: comm (neg), peer (neg)
    # Female: EmHealth (pos), study(pos)
    # FRL: Comm (neg), Peer (neg), Connect (neg) ... general neg
    # IEP: EmHealth(neg)
    # ELL: not much.
  
  
### Examine nature of non-response -- rates, and relationship with other predictors
  # Examine the extent of non-response. (Note that this cut of the data set doesn't yet have multiple years)
    for (m in mvmsVars){
      d_9[, p0(m, "_miss")] <- is.na(d_9[, p0(m, "NA")])
    }
  
  # Because non-response of MVMS variables varies across years, I'll start by picking response to one
    summary(glm(as.formula(p0("MVMS_AcadEng_miss ~ ", paste(c("Pct_NonAbsent",  myXs), collapse = "+"))), d_9[d_9$year == 2014,], family = "binomial"))
      # Significant predictors:
      #   Pos: black, IEP, Ell
      #   Neg: attendance
  
  # Examine total number of responses across years for 9th graders
  aggregate(MVMS_AcadEng_miss ~ year, d_9, sum)
#   countByYear <- function(m) aggregate(as.formula(p0(m, " ~ year")), d_9, sum)
#   (mvmsMissCounts <- sapply(mvmsMissVars, countByYear))
  aggregate(d_9[, mvmsMissVars], by = list(d_9$year), sum)
  aggregate(d_9[, mvmsMissVars], by = list(d_9$year), mean)
  
  
### Examine relationship between OST enrollment and levels of MVMS outcomes
  d_9_2014 <- subset(d_9, year == 2014)
  for(o in c(ostInds, "bCollab")){
    print(paste("doing stats for org", o))
    print(aggregate(d_9_2014[, p0(mvmsVars, "NA")], by = list(d_9_2014[, o]), mean, na.rm = T))
  }
  # Moderately uneven. A big explanation is the fact that few orgs are working with 9th graders.
  colMeans(d_9_2014[, c(ostInds, "bCollab")])
  
### Examine predictive power of MVMS when adding controls
  # Start with one (highly correlated) MVMS predictor as a test case
  mvmsVars
  mvmsVarsReduced <- mvmsVars[!grepl("Respect|Comm", mvmsVars)]
  summary(lm(Pct_NonAbsent ~ MVMS_AcadEngNA, d_9_2014)) # 20169 degrees of freedom
  summary(lm(Pct_NonAbsent ~ MVMS_StudyNA, d_9_2014)) # 20485 degrees of freedom
  summary(lm(Pct_NonAbsent ~ MVMS_StudyNA + MVMS_AcadEngNA, d_9_2014)) # 20058 degrees of freedom
  summary(lm(as.formula(p0("Pct_NonAbsent ~ ", paste(p0(mvmsVarsReduced, "NA"), collapse = "+"))), d_9_2014)) # 19842 degrees of freedom
  summary(lm(Pct_NonAbsent ~ MVMS_PeerNA + MVMS_StudyNA + MVMS_ConnectNA, d_9_2014)) # 20409 degrees of freedom ... 
    # This is the subset to factors with promising explanatory power. Effects are relatively small -- about 1% increase per 1 point increase in any MVMS measure.
  summary(lm(Pct_NonAbsent ~ MVMS_PeerNA + MVMS_StudyNA + MVMS_ConnectNA + Pct_NonAbsent_lag, d_9_2014)) # 20409 degrees of freedom ... 
  
  
  summary(lm(explore_compss ~ MVMS_StudyNA, d_9_2014)) #
  summary(lm(explore_compss ~ MVMS_PeerNA + MVMS_StudyNA + MVMS_ConnectNA, d_9_2014)) #
  summary(lm(as.formula(p0("explore_compss ~ ", paste(p0(mvmsVarsReduced, "NA"), collapse = "+"))), d_9_2014)) # 19842 degrees of freedom
  summary(lm(as.formula(p0("explore_compss ~ ", paste(p0(mvmsVarsReduced, "NA"), collapse = "+"))), d_9_2014)) # 19842 degrees of freedom
  summary(lm(as.formula(p0("explore_compss ~ ", paste(c(p0(mvmsVarsReduced, "NA"), "Pct_NonAbsent_lag", "isat_mathss_lag", "isat_readss_lag"), collapse = "+"))), d_9_2014)) # 19842 degrees of freedom
  summary(lm(as.formula(p0("explore_compss ~ ", paste(c(p0(mvmsVarsReduced, "NA"), myXs, "Pct_NonAbsent_lag", "isat_mathss_lag", "isat_readss_lag"), collapse = "+"))), d_9_2014)) # 19842 degrees of freedom
    sd(d_9_2014$explore_compss, na.rm = T) # SD is 3.6. Effect size is between 0.06 to 0.13
  # XXX Examine connection with longer-run outcomes, like association between MVMS and graduating on time a few years ahead.
  # XXX Develop GPA data.
  
### Attempt factor analysis for reducing factors
  # See: http://www.statmethods.net/advstats/factor.html
  # General reading for factor analysis: http://en.wikipedia.org/wiki/Exploratory_factor_analysis
  mydata <- d_pp[, ]
  fit <- factanal(mydata, 3, rotation="varimax")
  print(fit, digits=2, cutoff=.3, sort=TRUE)
  # plot factor 1 by factor 2 
  load <- fit$loadings[,1:2] 
  plot(load,type="n") # set up plot 
  text(load,labels=names(mydata),cex=.7) # add variable names
  
  
### Attempt multivariate predictions of MVMS using partner controls
  # Straight predictions
  
  # Add selection correction. Schools are the key exclusion, since they likely play a role in active vs. lax facilitation
  
  
  
  
  
  
  