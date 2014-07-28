## Generate Summary Tables and Figures to Communicate the new MVMS Scale

#---------------------------------------------#
#---------------------------------------------#
# GENERATE SUMMARY TABLES AND FIGURES TO      #
# COMMUNICATE THE NEW MVMS SCALE              #
#                                             #
# Authors: Nick Mader                         #
#                                             #
#---------------------------------------------#
#---------------------------------------------#

## Set up workspace and designate/update file locations

  rm(list=ls())
  #myDir <- "/projects/Integrated_Evaluation_Youth_Support_Services/"
  myDir <- "H:/Integrated Evaluation Project for YSS Providers"
  setwd(myDir)
  dataPath <- "./data/preprocessed-data/"
  
## Set up useful libraries and functions   
   
  library(ggplot2)
  comment <- function(...){}
  "%&%"   <- function(...){ paste(..., sep="") }
  paste0  <- function(...){ paste(..., sep="") }
  cn      <- function(x){ colnames(x) }
  ep      <- function(x){ eval(parse(text = x))}
  f.to.c  <- function(f){ return(levels(f)[f]) }
  f.to.n  <- function(f){ return(as.numeric(levels(f)[f])) }
  
## Set up the data
  
  load("./data/preprocessed-data/CpsOrg_PP.Rda")
  mvmsVars <- grep("MVMS_.+[^se]$", cn(d_pp), value = T)
  years <- unique(d_pp$year)
  d_m <- d_pp[d_pp$year %in% 2011:2013, c("year", mvmsVars)]
  
## Generate labels for the MVMS factors:
  
  mvmsLabels <- c("Academic Engagement", "Emotional Health", "Human & Social Resources in Community", "Parent Supportiveness",
                  "Peer Support for Academic Work", "Rigorous Study Habits", "Safety", "School Connectedness", "Student Classroom Behavior")
  names(mvmsLabels) <- c("MVMS_AcadEng", "MVMS_EmHealth", "MVMS_Comm", "MVMS_Parent", "MVMS_Respect", "MVMS_Study", "MVMS_Safety", "MVMS_Connect", "MVMS_Peer")
  
  # *** Note: it seems incongruous, but true that the "PEER" question bank in the MVMS codebook is described as "Student Classroom Behavior"
  # in the 5Essentials reports.
                  
## Generate a table with descriptive statistics by year and MVMS measure -- 
  getStats <- function(y, x) {
    m <- d_m[d_m$year == y, x]
    out <- data.frame(mean = mean(m, na.rm = T),
                      median = median(m, na.rm = T),
                      sd = sd(m, na.rm = T),
                      q25 = quantile(m, 0.25, na.rm = T),
                      q75 = quantile(m, 0.75, na.rm = T))
    out$variable  <- x
    out$year <- y
    out$id <- paste(x, y, sep = "_")
    return(out)
  }
  stats <- t(mapply(getStats,
                    rep(2011:2013, times = length(mvmsVars)),
                    rep(mvmsVars, each = length(2011:2013))))
  write.csv(stats, file = "./output/mvms-stats.csv")
  
  # Calculate and output correlations
    mvmsRho <- cor(d_m[, mvmsVars], use = "pairwise.complete.obs")
    write.csv("./output/mvms-pairwise-correlations.csv")
  
## Generate figures to show the distribution of values
  # Histogram (of only 2012-13, which is used for centering) for more lay audiences
  
  d_2013 <- d_m[d_m$year == 2013,]
  sapply(mvmsVars, function(m) {
    ggplot(data = d_2013) + 
      geom_histogram(aes_string(x = m), colour = "black", fill = "white") + # binwidth = 0.5, 
      ggtitle("Histogram of " %&% mvmsLabels[m] %&% ",\nAll youth in 2012-13") + 
      geom_vline(xintercept = mean(d_2013[, m], na.rm = T), size = 1, colour = "blue") +
      scale_y_continuous("Count of Youth in Bin") + scale_x_continuous("Youth Score")
    ggsave("./output/figures/MVMS Histogram - " %&% m %&% ".png", dpi = 200, width = 4.67, height = 3.5)
  })
  
  # Kernel density plots showing comparisons of distributions by year
  sapply(mvmsVars, function(m) {
    ggplot(data = d_m[!is.na(d_m[, m]),], aes_string(x = m, colour = "factor(year)")) + geom_density(adjust = 5) +
      ggtitle("Density plot of " %&% m) #+ stat_density(adjust = 6)
    ggsave("./output/figures/MVMS Density Across Years - " %&% m %&% ".png")
  })
  
  