#---------------------------------------------#
#---------------------------------------------#
### Generate displays of regression results ###
#---------------------------------------------#
#---------------------------------------------#
  # These visualizations use data files generated in the run_regs.r code.

  rm(list=ls())
  library("ggplot2")
  library("reshape2")
  library("plyr")
  library("lme4")
  myDir <- "/projects/Integrated_Evaluation_Youth_Support_Services/"
  setwd(myDir)

  source("~/GitHub/after-school-collab/scripts/helper-functions.r")


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
    
    dOrg <- orgOut[grepl(".*bCHaA.*orgBin$", orgOut$id),]
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
  
