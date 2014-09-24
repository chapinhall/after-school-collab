#-------------------------------------------------------------------#
#-------------------------------------------------------------------#
# GENERATE VISUALS TO EXPLAIN THE RATIONALE FOR REGRESSION ANALYSIS #
#                                                                   #
# Authors: Nick Mader                           		                #
#                                                                   #
#-------------------------------------------------------------------#
#-------------------------------------------------------------------#

## Set up workspace and designate/update file locations

  rm(list=ls())
  library("ggplot2")
  library("reshape")
  library("plyr")
  
  comment <- function(...){}
  "%&%"   <- function(...){ paste(..., sep="") }
  paste0  <- function(...){ paste(..., sep="") }
  p0      <- function(...){ paste0(...) }
  plus    <- function(...){ paste(..., collapse = "+") }
  cn      <- function(x){ colnames(x) }
  ep      <- function(x){ eval(parse(text = x))}
  f.to.c  <- function(f){ return(levels(f)[f]) }
  f.to.n  <- function(f){ return(as.numeric(levels(f)[f])) }

  myDir <- "H:/Integrated Evaluation Project for YSS Providers"
  setwd(myDir)
  set.seed(60637)
  dataPath <- "./data/preprocessed-data/"
  load("./data/preprocessed-data/CpsOrg_PP.Rda")
  
# Create a sample for the demonstration
  
  #d_pp$Pct_Attend100 <- d_pp$Pct_Attend*100
  d_pp$Pct_Attend100_lag <- d_pp$Pct_Attend_lag*100
  
  keepVars <- c("bCollab", "isat_mathss", "isat_mathss_lag", "Pct_Attend100_lag", "bIEP", "bLunch_FR")
  d_sub <- d_pp[d_pp$year == 2013 & d_pp$fGradeLvl == 8, keepVars]
  sampleN <- 200
  
  nonCollab <- d_sub[d_sub$bCollab==0, ]
  nonCollabSample <- nonCollab[sample(nrow(nonCollab), sampleN), ]
  
  Collab    <- d_sub[d_sub$bCollab==1, ]
  CollabSample <- Collab[sample(nrow(Collab), sampleN), ]
  
  mySample <- rbind(nonCollabSample, CollabSample)
  mySample$org <- factor(sapply(mySample$bCollab, function(x) switch(as.character(x), "0" = "Non-Org", "1" = "Org")))
  mySample$iep <- factor(sapply(mySample$bIEP, function(x) switch(as.character(x), "0" = "non-IEP", "1" = "IEP")))
  
  mySample <- na.omit(mySample)
  
# Compare average outcomes
  z <- aggregate(isat_mathss ~ bCollab, data = mySample, mean, na.rm = T)
  ggplot(data = z, aes(x = factor(bCollab), y = isat_mathss, fill = factor(bCollab))) +
    geom_bar(position = "dodge", stat = "identity") + 
    scale_x_discrete(labels = c("Non-Org", "Org")) +
    scale_y_continuous(limits = c(240, 290), oob = squish) +
    ylab("Average ISAT Score - Math") + xlab("") + 
    ggtitle("Comparison of Org vs. Non-Org Scores") + 
    theme(title = element_text(size = 10), 
              axis.title = element_text(size = 10, face = "bold"),
              axis.text = element_text(size = 8, face = "bold"),
              legend.position = "none")
  ggsave("./output/reg-explanation/Mean comparison.png", dpi = 600, width = 4.67, height = 3.50)

#---------------------------------
# Pairwise comparisons of outcomes
#---------------------------------
  
  savePlot <- function(name) ggsave(name, dpi = 600, width = 4.67, height = 3.50)
  for (xVar in c("isat_mathss_lag", "Pct_Attend100_lag")){
  
    myXLab <- switch(xVar,
                     "isat_mathss_lag" = "Last Year's ISAT Math Score",
                     "Pct_Attend100_lag" = "Last Year's School Day Attendance Rate")
    mySample$x <- mySample[, xVar]
    myXLimit <- switch(xVar, "isat_mathss_lag" = xlim(175, 325), "Pct_Attend100_lag" = xlim(92, 100))
    
    # Set Base
    pairBase <- ggplot() +
                  ylab("This Year's ISAT Math Score") + xlab(myXLab) + ylim(175, 325) + myXLimit +
                  ggtitle("Comparison of Org vs. Non-Org Scores") + 
                  theme(title = element_text(size = 10), 
                            axis.title = element_text(size = 10, face = "bold"),
                            axis.text = element_text(size = 8, face = "bold"))
    # Scatter
    pairBase + geom_point(data = mySample, aes(x = x, y = isat_mathss), size = 2)
      savePlot(p0("./output/reg-explanation/Math vs ", myXLab, ".png"))
    
    # Scatter + show which org is which
    pairBase + geom_point(data = mySample, aes(x = x, y = isat_mathss, colour = org), size = 2) +
      theme(legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size = 6),
            legend.title = element_blank())
    
    savePlot(p0("./output/reg-explanation/Math vs ", myXLab, ", by Org.png"))
    
    # Scatter + best fit lines
    pairBase + geom_point(data = mySample, aes(x = x, y = isat_mathss, colour = org), size = 2) +
      geom_smooth(data = mySample, aes(x = x, y = isat_mathss, colour = org), method = "lm", size = 1, se = FALSE) +
      theme(legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size = 6),
            legend.title = element_blank())
    
    savePlot(p0("./output/reg-explanation/Math vs ", myXLab, ", by Org, with Best Fit.png"))
    
    # Scatter + best fit lines + IEP
    pairBase + geom_point(data = mySample, aes(x = x, y = isat_mathss, colour = org, size = iep)) +
      scale_size_discrete(range = c(2, 4), name = "IEP") + 
      geom_smooth(data = mySample, aes(x = x, y = isat_mathss, colour = org), method = "lm", size = 1, se = FALSE) +
      theme(legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size = 6),
            legend.title = element_blank())
    
    savePlot(p0("./output/reg-explanation/Math vs ", myXLab, ", by Org, with Best Fit and IEP.png"))
  } 
  
#------------------
# Create regression
#------------------
  
  myReg <- lm(isat_mathss ~ isat_mathss_lag + Pct_Attend100_lag + bIEP + bCollab, data = mySample)
