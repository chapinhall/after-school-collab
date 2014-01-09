#---------------------------------------------#
#---------------------------------------------#
# MASTER FILE FOR DATA ANALYSIS AND REPORTING # 
# Preliminary Analysis with ggplot2           #
#                                             #
# Authors: Nick Mader, Ian Matthew Morey,	    #
#			and Emily Wiegand		    #  
#---------------------------------------------#
#---------------------------------------------#


## Set up workspace and designate/update file locations

  rm(list=ls())
  #myDir <- "/projects/Integrated_Evaluation_Youth_Support_Services/"
  myDir <- "H:/Integrated Evaluation Project for YSS Providers/"
  setwd(myDir)
  dataPath <- "./data/preprocessed-data/" # File path to locate and save data

  useScrambledData <- 0
  runDescGraphs    <- 0
  runRegs          <- 1

  if (useScrambledData==1) {
    myGraphOut <- paste0(myDir,"/demos/") 
    scramInd <- "_DEMO"
  } else {
    myGraphOut <- paste0(myDir,"/output/")
    scramInd <- ""
}
  
  library(plyr)
  library(reshape)
  library(ggplot2)
  library(car)
  library(foreign)
  library(scales)
  library(grid)
  
  myfill  <- c("#CC3300", "#660000") #orange/red and dark red
  myfill2 <- c("#CC6600", "#480000") #darker orange and maroon
  myfill3 <- c("#FF6600", "#480000") #lighter orange and maroon
  myfill4 <- c("#CC6600", "#006666") #darker orange and teal
  myfill5 <- c("#CC6600", "#003333") #darker orange and darker teal
  bluesFill <- c("#005555", "#000077")
  
  myRes <- 600
  myWidth <- 2800
  myHeight <- 2100



## Load data

  load(paste0(dataPath,"ctsMean_byAny",scramInd,".Rda"))
  load(paste0(dataPath,"ctsMean_byAnyGr",scramInd,".Rda"))
  load(paste0(dataPath,"ctsMean_bySite",scramInd,".Rda"))
  load(paste0(dataPath,"ctsMean_bySiteGr",scramInd,".Rda"))
  load(paste0(dataPath,"ctsMean_bySch",scramInd,".Rda"))
  load(paste0(dataPath,"ctsMean_bySchGr",scramInd,".Rda"))
  load(paste0(dataPath,"ctsMean_byAnySchPeer",scramInd,".Rda"))
  load(paste0(dataPath,"ctsMean_bySiteSchPeer",scramInd,".Rda"))

#---------------------------------------#
#---------------------------------------#
### Graphs for Non-Org, Org, and Site ###
#---------------------------------------#
#---------------------------------------#
  
if (1==runDescGraphs) {
  
  for (s in c("All", levels(fShortSite))) {
    print("Running graphs for " %&% s)
    
    dir.create(file.path(myOutDir, s), showWarnings = F)
    
    ctsPlotDataGr <- ctsMean_byAnyGr
    ctsPlotData <- rbind(ctsMean_byAny, ctsMean_byAnySchPeer[ctsMean_byAnySchPeer$Site == "Org Alpha\nSch-Based Peers",])
    ctsPlotData$Site <- factor(ctsPlotData$Site, levels= c("Non-Org Alpha", "Org Alpha\nSch-Based Peers", "Org Alpha")) 
    
    if (s == "All"){
      useFillCat <- c(myfill5)
      useFill <- c(myfill5, "#885533")
      myGraphOut <- myOutDir
    } else {
      useFillCat <- c(myfill5, "#885533")
      useFill <- c(myfill5, "#885533", "#BB2800")
      #useFill <- c()
      myGraphOut <- myOutDir %&% s %&% "/"
      ctsPlotDataGr <- rbind(ctsPlotDataGr, ctsMean_bySiteGr[ctsMean_bySiteGr$Site==s,])
      
      ctsPlotData   <- rbind(ctsMean_byAny,
                             ctsMean_bySiteSchPeer[ctsMean_bySiteSchPeer$Site==s %&% "\nSch-Based Peers", ],
                             ctsMean_bySite[ctsMean_bySite$Site==s, ])
      ctsPlotData$Site <- factor(ctsPlotData$Site, levels= c("Non-Org Alpha", "Org Alpha", s %&% "\nSch-Based Peers", s)) 
    }
    
    # Grade distribution
    
    GrDist <- table(fAnyYss, fGradeLvl)
    GrDistProp <- prop.table(GrDist, 1)  
    GrDistPropL <- as.data.frame(GrDistProp)  
    if (s != "All") {
      p <- prop.table(table(fGradeLvl[fShortSite==s]))
      df <- data.frame(cbind(s, data.frame(p)))
      colnames(df) <- colnames(GrDistPropL)
      GrDistPropL <- rbind(GrDistPropL, df)
    }
    
    png(file = myGraphOut %&% "CpsVsOrg_GradeLvl_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
      Plot_GradeHist <- ggplot(data=GrDistPropL, aes(x=fGradeLvl, y=Freq, fill=fAnyYss)) + 
        geom_bar(stat="identity", position="dodge", width=0.7) + 
        ggtitle("Distribution of Grade Levels") + 
        xlab("Grade") + ylab("% in Each Grade") + 
        theme(axis.title=element_text(size = 8)) +
        scale_y_continuous(labels = percent) +
        theme(legend.position="bottom") + 
        scale_fill_manual(values=useFillCat) +  
        guides(fill=guide_legend(title=NULL))
      print(Plot_GradeHist)
    dev.off()
    
    #Demographics - % free lunch
    
    ctsPlotData$mLabel <- sprintf("%.1f%%", ctsPlotData$bLunch_FR*100)
    png(file=myGraphOut %&% "CpsVsOrg_Lunch_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
      Plot_Frl <- ggplot(data=ctsPlotData, aes(x=Site, y=bLunch_FR)) +
        geom_bar(stat="identity", position="dodge", aes(fill=Site), width=0.7) +
        geom_text(data=ctsPlotData, aes(x=Site, y=bLunch_FR, label=mLabel, vjust=-1), size = 6) +
        ggtitle("% Free/Reduced Price Lunch") +
        ylab("Proportion on Free/Reduced Price Lunch") +
        scale_y_continuous(labels = percent, breaks=seq(0.75, 1.0, 0.05), limits=c(0.75, 1.0), oob = squish) +
        theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 8)) +
        theme(legend.position="none") +
        scale_fill_manual(values=useFill) +
        guides(fill=guide_legend(title=NULL))
      print(Plot_Frl)
    dev.off()
    
    
    # Demographics - % race
    
    DemRc <- table(fAnyYss, fRace)
    DemRcProp <- prop.table(DemRc, 1)
    DemRcPropL <- as.data.frame(DemRcProp)
    if (s != "All") {
      p <- prop.table(table(fRace[fShortSite==s]))
      df <- data.frame(cbind(s, data.frame(p)))
      colnames(df) <- colnames(DemRcPropL)
      DemRcPropL <- rbind(DemRcPropL, df)
    }
    
    png(file=myGraphOut %&% "CpsVsOrg_RaceEth_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
      Plot_RaceEth <- ggplot(data=DemRcPropL, aes(x=fRace, y=Freq, fill=fAnyYss)) +
        geom_bar(stat="identity", position="dodge", width=0.7) +
        ggtitle("Comparison of Race/Ethnicity") +
        ylab("% in Each Race/Eth Group") +
        scale_y_continuous(labels = percent) +
        theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 8))
      print(Plot_RaceEth)
    dev.off()
    
    
    
    # Neighborhood characteristics
      DemNbr <- melt(ctsPlotData[, c("Site", "Tract_Pct_LtHsEd", "Tract_Pct_VacantUnits", "Tract_ViolentCrimes_PerHundr", "Tract_Pct_IncRatKidsLt6_Lt100", "Tract_Pct_NonEnglLangSpokenAtHome")], id=("Site"))
      
      DemNbr$VarName[DemNbr$variable=="Tract_Pct_LtHsEd"] <- "Prop'n <\nHS Ed"
      DemNbr$VarName[DemNbr$variable=="Tract_Pct_VacantUnits"] <- "Prop'n Units\nVacant"
      DemNbr$VarName[DemNbr$variable=="Tract_ViolentCrimes_PerHundr"] <- "Violent Crimes\nPer 100"
      DemNbr$VarName[DemNbr$variable=="Tract_Pct_IncRatKidsLt6_Lt100"] <- "Prop'n Kids\nin Poverty"
      DemNbr$VarName[DemNbr$variable=="Tract_Pct_NonEnglLangSpokenAtHome"] <- "Prop'n No Engl\nat Home"
      
      DemNbr$mLabel <- sprintf("%.1f", DemNbr$value)
      
      png(file = myGraphOut %&% "CpsVsOrg_Neighborhood_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
        Plot_Nbhd <- ggplot(data=DemNbr, aes(x=VarName, y = value, fill=Site)) + 
          geom_bar(stat="identity", position="dodge", width=0.7) +
          #geom_text(data=DemNbr, aes(x=VarName, y=value, label=mLabel, vjust=-1), position = position_dodge(width=0.7), size=5) +
          ggtitle("Comparison of Youth Neighborhoods") +
          theme(legend.position="bottom") +
          theme(axis.text.x=element_text(size=7)) +
          theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
          scale_fill_manual(values=useFill) +
          guides(fill=guide_legend(title=NULL))
        print(Plot_Nbhd)
      dev.off()
    
    # Plot comparison of only Violent crimes -- (good for a single test case)
      CrimeOnly <- DemNbr[DemNbr$variable == "Tract_ViolentCrimes_PerHundr",]
      CrimeOnly <- CrimeOnly[CrimeOnly$Site != "Org Alpha\nSch-Based Peers",]
      png(file = myGraphOut %&% "CpsVsOrg_Crime_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
        Plot_Crime <- ggplot(data=CrimeOnly, aes(x=Site, y=value, fill=Site)) + 
          geom_bar(stat="identity", position="dodge", width=0.7) +
          #geom_text(data=DemNbr, aes(x=VarName, y=value, label=mLabel, vjust=-1), position = position_dodge(width=0.7), size=5) +
          ggtitle("Comparison of Crime\nin Youth Neighborhoods") +
          theme(legend.position="bottom") +
          theme(axis.text=element_text(size=7), axis.title.x=element_blank()) +
          ylab("Violent Crimes per 100 Residents") +
          scale_fill_manual(values=useFillCat) +
          guides(fill=guide_legend(title=NULL))
        print(Plot_Crime)
      dev.off()
    
   # Histogram of warning/below/meets/exceeds for Math
      HistProfMth <- table(fAnyYss, mathpl)
      HistProfMthProp <- prop.table(HistProfMth, 1)
      HistProfMthPropL <- as.data.frame(HistProfMthProp)
      if (s != "All") {
        pl <- prop.table(table(mathpl[fShortSite==s]))
        df <- data.frame(cbind(s, data.frame(pl)))
        colnames(df) <- colnames(HistProfMthPropL)
        HistProfMthPropL <- rbind(HistProfMthPropL, df)
      }
      
      HistProfMthPropL$mLabel <- sprintf("%.1f%%", HistProfMthPropL$Freq*100)
      
      png(file = myGraphOut %&% "CpsVsOrg_TestPlMath_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
        Plot_TestPlMath <- ggplot(data=HistProfMthPropL, aes(x=mathpl, y=Freq, fill=fAnyYss)) + 
          geom_bar(stat="identity", position="dodge", width=0.7) + 
          #geom_text(data=HistProfMthPropL, aes(x=mathpl, y=Freq, label=mLabel, vjust=-1), position = position_dodge(width=0.7), size = 6) + #vjust=-1, hjust=HAdj, size=7)) +
          ggtitle("Tested Proficiency\nLevels - Math") + 
          ylab("% in Performance Category") + 
          scale_y_continuous(labels = percent) +
          theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 7), legend.position="bottom") +
          scale_fill_manual(values=useFillCat) +  
          guides(fill=guide_legend(title=NULL))
        print(Plot_TestPlMath)
      dev.off()
    
    # Histogram of warning/below/meets/exceeds for Reading
    
      HistProfRd <- table(fAnyYss, readpl)
      HistProfRdProp <- prop.table(HistProfRd, 1)
      HistProfRdPropL <- as.data.frame(HistProfRdProp)
      if (s != "All") {
        pl <- prop.table(table(readpl[fShortSite==s]))
        df <- data.frame(cbind(s, data.frame(pl)))
        colnames(df) <- colnames(HistProfRdPropL)
        HistProfRdPropL <- rbind(HistProfRdPropL, df)
      }
    
      HistProfRdPropL$mLabel <- sprintf("%.1f%%", HistProfRdPropL$Freq*100)
      
      png(file = myGraphOut %&% "CpsVsOrg_TestPlRead_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
        Plot_TestPlRead <- ggplot(data=HistProfRdPropL, aes(x=readpl, y=Freq, fill=fAnyYss)) + 
          geom_bar(stat="identity", position="dodge", width=0.7) + 
          #geom_text(data=HistProfRdPropL, aes(x=readpl, y=Freq, label=mLabel, vjust=-1), position = position_dodge(width=0.7)) +
          ggtitle("Tested Proficiency\nLevels - Reading") + 
          ylab("% in Performance Category") +
          scale_y_continuous(labels = percent) +
          theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 7), legend.position="bottom") +
          scale_fill_manual(values=useFillCat) +	
          guides(fill=guide_legend(title=NULL))
        print(Plot_TestPlRead)
      dev.off()
      
    # Average Meets/Exceeds Calculations by Grade
    
      MEbyGr_non0 <- ctsPlotDataGr[ctsPlotDataGr$Grade %in% 3:8, ]
      MEbyGr_non0$Site <- factor(MEbyGr_non0$Site, levels=c("Non-YMCA", "YMCA", s))
      MEbyGr_non0$mLabel <- sprintf("%.1f%%", MEbyGr_non0$mathpl_ME*100)
    
      png(file = myGraphOut %&% "CpsVsOrg_MathME_by_Gr_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
        MEByGr_math <- ggplot(data=MEbyGr_non0, aes(x=Grade, y=mathpl_ME, fill=Site)) + 
          geom_bar(stat="identity", position="dodge", width=0.7) + 
          #geom_text(data=MEbyGr_non0, aes(x=Grade, y=mathpl_ME, label=mLabel, vjust=-1), position = position_dodge(width=0.7)) +
          ggtitle("% Meets/Exceeds Standard\nfor Math Proficiency") + 
          xlab("Grade") + ylab("% Meets/Exceeds") +
          scale_y_continuous(labels = percent) + 
          scale_fill_manual(values=useFill) +  
          guides(fill=guide_legend(title=NULL)) +
          theme(legend.position="bottom", axis.text=element_text(size = 7))
        print(MEByGr_math)
      dev.off()
    
      png(file = myGraphOut %&% "CpsVsOrg_ReadME_by_Gr_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
        MEByGr_read <- ggplot(data=MEbyGr_non0, aes(x=Grade, y=readpl_ME, fill=Site)) + 
          geom_bar(stat="identity", position="dodge", width=0.7) + 
          #geom_text(data=MEbyGr_non0, aes(x=Grade, y=readpl_ME, label=mLabel, vjust=-1), position = position_dodge(width=0.7)) +
          ggtitle("% Meets/Exceeds Standard\nfor Reading Proficiency") + 
          xlab("Grade") + ylab("% Meets/Exceeds") +
          scale_y_continuous(labels = percent) +
          scale_fill_manual(values=useFill) +  
          guides(fill=guide_legend(title=NULL)) +
          theme(legend.position="bottom", axis.text=element_text(size = 7))
        print(MEByGr_read)
      dev.off()
    
    # Average Test Scores by Grade
    
      TestByGr <- melt(ctsPlotDataGr[, c("Site", "Grade", "mathss", "readss")], id=c("Site", "Grade"))
      TestByGr <- ctsPlotDataGr[, c("Site", "Grade", "mathss", "readss")]
      TestByGr <- TestByGr[TestByGr$Grade %in% 3:8,]
      
      TestByGr$mLabel <- sprintf("%.1f", TestByGr$mathss)
      TestByGr$rLabel <- sprintf("%.1f", TestByGr$readss)
    
      png(file = myGraphOut %&% "CpsVsOrg_MathTestSs_by_Gr_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
        Plot_MathByGr <- ggplot(data=TestByGr[TestByGr$Grade %in% 3:8,], aes(x=Grade, y=mathss, fill=Site)) + 
          geom_bar(stat="identity", position="dodge", width=0.7) + 
          #geom_text(data=TestByGr, aes(x=Grade, y=mathss, label=mLabel, vjust=-1), position = position_dodge(width=0.7)) +
          ggtitle("Math Scores by Grade") + 
          scale_fill_manual(values=useFill) +  
          xlab("Grade") + ylab("Average Test Scale Score") +
          guides(fill=guide_legend(title=NULL)) +
          theme(legend.position="bottom", axis.title = element_text(size=7))
        print(Plot_MathByGr)
      dev.off()
    
      png(file = myGraphOut %&% "CpsVsOrg_ReadTestSs_by_Gr_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
        Plot_ReadByGr <- ggplot(data=TestByGr[TestByGr$Grade %in% 3:8,], aes(x=Grade, y=readss, fill=Site)) + 
          geom_bar(stat="identity", position="dodge", width=0.7) + 
          #geom_text(data=TestByGr, aes(x=Grade, y=readss, label=rLabel, vjust=-1, size=7), position = position_dodge(width=0.7)) +
          ggtitle("Reading Scores by Grade") + 
          scale_fill_manual(values=useFill) +  
          xlab("Grade") + ylab("Average Test Scale Score") +
          guides(fill=guide_legend(title=NULL)) +
          theme(legend.position="bottom", axis.title = element_text(size=7))
        print(Plot_ReadByGr)
      dev.off()
    
    
    # Average test gains
    
      AvgGain <- melt(ctsPlotData[, c("Site", "mathgain", "readgain")], id="Site")
      AvgGain$VarName[AvgGain$variable=="mathgain"] <- "Scale Score\nGain - Math"
      AvgGain$VarName[AvgGain$variable=="readgain"] <- "Scale Score\nGain - Reading"
  
      AvgGain$mLabel <- sprintf("%.1f", AvgGain$value)
      
      png(file = myGraphOut %&% "CpsVsOrg_TestGain_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
        Plot_AvgGain <- ggplot(data=AvgGain, aes(x=VarName, y=value, fill=Site)) + 
          geom_bar(stat="identity", position="dodge", width=0.7) +
          geom_text(data=AvgGain, aes(x=VarName, y=value, label=mLabel, vjust=-1), position = position_dodge(width=0.7)) +
          ggtitle("Average Test Score Gain") +
          ylab("Average Scale Score Gain") +  
          theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 8), legend.position="bottom") +
          scale_fill_manual(values=useFill) +
          guides(fill=guide_legend(title=NULL))
        print(Plot_AvgGain)
      dev.off()
      
    
    # Average Percent Attendance

      PctAtt <- ctsPlotData[, c("Site", "Pct_Attend")]
      PctAtt$mLabel <- sprintf("%.1f%%", PctAtt$Pct_Attend*100)
      
      png(file = myGraphOut %&% "CpsVsOrg_PctAtt_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
        Plot_PctAtt <- ggplot(data=PctAtt, aes(x=Site, y=Pct_Attend)) +
          geom_bar(stat="identity", position="dodge", aes(fill=rownames(PctAtt)), width=0.7) +
          geom_text(data=PctAtt, aes(x=Site, y=Pct_Attend, label=mLabel, vjust=-1), size=6) +
          ggtitle("Average School Attendance") +
          ylab("Average Rate of School Attendance") + 
          scale_y_continuous(labels = percent, breaks=seq(0.5, 1.0, 0.1), limits=c(0.5, 1.0), oob=squish) +
          theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 7), legend.position="none") +
          scale_fill_manual(values=useFill) +
          guides(fill=guide_legend(title=NULL))
        print(Plot_PctAtt)
      dev.off()
      
    # Rate of Attendance by Grade
    
      AttByGr <- ctsPlotDataGr[, c("Site", "Grade", "Pct_Attend")]
      AttByGr$mLabel <- sprintf("%.1f%%", AttByGr$Pct_Attend*100)
    
      png(file = myGraphOut %&% "CpsVsOrg_SchAtt_by_ElemGr_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
        Plot_PctElemAttByGr <- ggplot(data=AttByGr[AttByGr$Grade %in% 1:8,], aes(x=Grade, y=Pct_Attend, fill=Site)) + 
          geom_bar(stat="identity", position="dodge", width=0.7) + 
          #geom_text(data=AttByGr[AttByGr$Grade %in% 1:8,], aes(x=Grade, y=Pct_Attend, label=mLabel, vjust=-1, hjust=HAdj, size=7)) +
          ggtitle("School Attendance by Grade") + 
          scale_fill_manual(values=useFillCat) +  
          xlab("Grade") + ylab("% Days Attending School") +
          scale_y_continuous(labels = percent, breaks=seq(0.5,1.0,0.1), limits=c(0.5, 1.0), oob = squish) +
          guides(fill=guide_legend(title=NULL)) +
          theme(legend.position="bottom")
        print(Plot_PctElemAttByGr)
      dev.off()
    
      png(file = myGraphOut %&% "CpsVsOrg_SchAtt_by_HsGr_AnyYss.png", res = myRes, width = myWidth, height = myHeight)
        Plot_PctHsAttGr <- ggplot(data=AttByGr[AttByGr$Grade %in% 9:12,], aes(x=Grade, y=Pct_Attend, fill=Site)) + 
          geom_bar(stat="identity", position="dodge", width=0.7) + 
          ggtitle("School Attendance by Grade") + 
          #geom_text(data=AttByGr[AttByGr$Grade %in% 9:12,], aes(x=Grade, y=Pct_Attend, label=mLabel, vjust=-1, hjust=HAdj, size=7)) +
          xlab("Grade") + ylab("% Days Attending School") +
          scale_y_continuous(labels = percent, breaks=seq(0.5,1.0,0.1), limits=c(0.5, 1.0), oob = squish) +
          scale_fill_manual(values=useFillCat) +  
          guides(fill=guide_legend(title=NULL)) +
          theme(legend.position="bottom")
        print(Plot_PctHsAttGr)
      dev.off() 
  
    
  } # End of loop across sites
  
  
#-------------------------------------------#
#-------------------------------------------#
### Generate Graphs Across Multiple Sites ###
#-------------------------------------------#
#-------------------------------------------#

  # Set up list of variables to go across
  mySiteCompareSpecs <- list(c("Tract_ViolentCrimes_PerHundr", "Violent Crimes per 100 Residents\nin Youth's Neighborhoods"),
                             c("Tract_Pct_LtHsEd", "% of Adults With Less than HS Ed\nin Youth's Neighborhoods"), 
                             c("Tract_Pct_IncRatKidsLt6_Lt100", "% of Children Below Poverty Line\nin Youth's Neighborhoods"),
                             c("mathpl_ME",  "Rates of Meets/Exceeds on ISAT Math"),
                             c("readpl_ME",  "Rates of Meets/Exceeds on ISAT Reading"),
                             c("Pct_Attend", "School Attendance Rate"),
                             c("bRace_B",    "% Composition of Af.Am. Youth"),
                             c("bRace_H",    "% Composition of Hispanic Youth"),
                             c("bRace_W",    "% Composition of White Youth"),
                             c("bRace_NonW", "% Composition of Non-White Youth"),
                             c("bLunch_FR",  "% Free Lunch Status by Site"))
  myVars <- sapply(mySiteCompareSpecs, function(m) m[1] ) 
  siteCalc <- aggregate(cbind(myData[, myVars]), list(myData$fShortSite), mean, na.rm=T)
    colnames(siteCalc)[1] <- "fShortSite"
  siteNs <- aggregate(rep(1, nrow(myData)), list(fShortSite), sum, na.rm=T)
    colnames(siteNs) <- c("fShortSite", "N")
  siteCalc <- merge(siteCalc, siteNs, by = "fShortSite")
  

  for (l in mySiteCompareSpecs) {
    myVar <- l[1]; myVarLab  <- l[2]
    myYBreaks <- NULL
    myYLim <- NULL
    myYTitle <- ""
    if (myVar == "bLunch_FR")                    { myYBreaks <- seq(0.5, 1, 0.1);  myYLim = c(0.5, 1) } else
    if (myVar == "Pct_Attend")                   { myYBreaks <- seq(0.75, 1, 0.05); myYLim = c(0.75, 1) } else
    if (myVar == "Tract_ViolentCrimes_PerHundr") { myYBreaks <- seq(0, 2.2, 0.2);  myYLim = c(0, 2.2);
                                                   myYTitle <- "Crimes per 100 Residents" }
    if (myVar == "Tract_ViolentCrimes_PerHundr") { 
      myYLabels <- sprintf("%.1f", myYBreaks)
    } else {
      myYLabels <- sprintf("%.0f%%", 100*myYBreaks)
    }
   
    # PREPARE CALCULATIONS OF CURRENT VARIABLE
    # Identify the CPS average value, remove "None of the Above", and put sites in order of their values for this variable
    cpsVal <- siteCalc[, myVar][siteCalc$fShortSite == "None of the Above"]
    calcPlot <- siteCalc[-which(siteCalc$fShortSite %in% "None of the Above"),]
    
    siteValOrder <- order(calcPlot[, myVar])
    calcPlot$fShortSite <- factor(calcPlot$fShortSite, levels=calcPlot$fShortSite[siteValOrder])
    calcPlot <- calcPlot[siteValOrder, ]
    calcPlot$myY <- calcPlot[, myVar]
    calcPlot <- calcPlot[!is.na(calcPlot$myY), ]
  
    # PREPARE GRAPH ELEMENTS BASED ON TYPE OF GRAPH
      # ... A decent color wheel guide: http://coolmaxhot.com/graphics/hex-colorsA.gif
    for (runSite in c("All", as.character(calcPlot$fShortSite))) {
      #print("Working on runSite " %&% runSite)
      hLineColor <- "#CC0000"
      myXLabels <- as.character(calcPlot$fShortSite %&% " n=" %&% calcPlot$N)
      
      if (runSite == "All") {
        barFill <- "#003366"
        bySiteOut <- myOutDir
      } else {
        barFill <- rep("#000033", length(calcPlot$fShortSite));
        barFill[calcPlot$fShortSite == runSite] <- "#0066CC"
        myXLabels[myXLabels != runSite] <- ""
        bySiteOut <- myOutDir  %&% runSite %&% "/"
      }
      
      png(file = paste(bySiteOut, "BySite_", myVar, "_", runSite, ".png", sep=""), res = myRes, width = myWidth, height = myHeight)
        myPlot <- ggplot(data=calcPlot, aes(x=fShortSite, y=myY)) + 
          ggtitle(myVarLab) + 
          geom_bar(stat="identity", position="dodge", width=0.7, fill = barFill) +
          geom_abline(aes(colour="CPS Avg", intercept = cpsVal, slope = 0), linetype="dashed", size = 0.75, show_guide = TRUE) +
          scale_colour_manual(values = hLineColor, name="") +
          theme(legend.position = c(0.8, 0.2) ) + 
          scale_x_discrete(labels=myXLabels) +
          ylab(myYTitle) + xlab("") +
          coord_flip() +
          theme(axis.text.y  = element_text(size=7),
                axis.text.x  = element_text(size = 7), #angle=0, vjust = 0.5, hjust = 0,
                legend.key.size = unit(0.1, "cm"), legend.title=element_blank()) # , 
          
        if (!is.null(myYLim)) {
          myPlot <- myPlot + scale_y_continuous(breaks = myYBreaks, limits = myYLim, labels = myYLabels, oob = squish)
        } else {
          myPlot <- myPlot + scale_y_continuous(labels = percent)
        }
        
        print(myPlot)
      dev.off()
    }
    print("Just finished var " %&% myVar)
  }


#--------------------------------------------------#
### Generate Scatter Plot Showing Multiple Sites ###
#--------------------------------------------------#
  
  # This should show a scatter plot for all sites, plotting % on FRL (y) vs. % children in poverty (x)

  siteCalc_NoNon <- siteCalc[siteCalc$fShortSite != "None of the Above",]
  Frl_vs_FRLThres_NoNon <- ctsMean_bySite[ctsMean_bySite$Site != "None of the Above", c("Site", "bLunch_FR", "Tract_Pct_IncRatKidsLt6_Lt185")]
  
  png(file = myOutDir %&% "PctFrl_vs_PctPov.png", res = myRes, width = myWidth, height = myHeight)
    Frl_vs_Pov <- ggplot(data = Frl_vs_FRLThres_NoNon, aes(x = Tract_Pct_IncRatKidsLt6_Lt185, y = bLunch_FR)) +
      geom_point() +
      #geom_point(aes(size = siteCalc_NoNon$N)) + scale_size_area() + 
      scale_y_continuous(labels = percent, breaks=seq(0.0, 1.0, 0.25), limits=c(0,1)) +
      scale_x_continuous(labels = percent, breaks=seq(0.0, 1.0, 0.25), limits=c(0,1)) +
      xlab("% of Children Under 185% of Fed Poverty Line") + ylab("% of Children on Free/\nReduced Lunch") +
      theme(axis.title = element_text(size = 8), axis.text = element_text(size = 7)) +
      guides(size=guide_legend(title="# Youth"))
    print(Frl_vs_Pov)
  dev.off()
} # End of Descriptive Graph Generation
  

#--------------------------#
#--------------------------#
### Regression Estimates ###
#--------------------------#
#--------------------------#

if (1 == runRegs) {
  depVarList <- c("Pct_Attend", "mathss", "readss") #, "bOnTrack", "bHsGrad") ... we don't yet have enough HS-aged youth participating in programming in linked data to be able to run onTrack and high school graduation analyses
    depVarNames <- c("School Attendance", "Math Score", "Reading Score") #, "On-Track Status", "HS Graduated")
    depVarYLabs <- c("Sch Attendance, % Units", "Scale Score Units", "Scale Score Units") #, "Impact on Prob of Being On-Track", "Impact on HS Graduation")
  regVars <- c("bLunch_F", "bLunch_R", "fRace", "Female", "fGradeLvl")
  TrtVars <- c("UsedYss", "fShortSite") # 
  thinData <- myData[, c(depVarList, depVarList %&% "_pre", regVars, TrtVars)]
  thinData$fShortSite <- relevel(thinData$fShortSite, ref="None of the Above")
  
  RunRegs <- function(useData, myDepVarList, myTrtVars, suffix){
    for (depVar in myDepVarList){
  
      for (trtVar in TrtVars) {      
        
        print("Running depvar " %&% depVar %&% ", and trtVar " %&% trtVar)
        
        sGain <- paste0(depVar, " - ", depVar, "_pre")
        regData <- useData[!is.na(useData[,depVar]), ]
        adjReg  <- summary(lm(as.formula(depVar   %&% " ~ bLunch_F + bLunch_R + fRace + Female + fGradeLvl + mathss_pre + readss_pre + Pct_Attend_pre + " %&% trtVar), data = regData))
        
        if (trtVar == "UsedYss") {
          print("  Generating plot for aggregate effects")
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
          
          png(file = paste(myOutDir, "Impact_", depVar, "_", trtVar, suffix, ".png", sep = ""), res = myRes, width = myWidth, height = myHeight)
            Plot_RegAll <- ggplot(data=Eff, aes(x=myX, y=b, fill=myX)) +
              geom_bar(stat="identity", position="dodge") +
              geom_errorbar(aes(ymin=ll, ymax=ul), width=0.1, position=position_dodge(0.1)) +
              ggtitle("Programming Impacts\non " %&% depVarNames[depVarList == depVar]) +
              ylab(depVarYLabs[depVarList == depVar]) +
              theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 8)) +
              guides(fill=guide_legend(title=NULL)) + scale_fill_hue(h=c(100,200), l=40) +
              theme(legend.position="none")
            print(Plot_RegAll)
            rm(Eff)
          dev.off()
          
          ### Ad hoc generation of a graph for a blog post
          EffSub <- data.frame(rbind(preReg$coeff["UsedYss", c("Estimate", "Std. Error")],
                                  adjReg$coeff["UsedYss", c("Estimate", "Std. Error")]))
          rownames(EffSub) <- c("1: Separating\nOnly Prior\nScore", "2: Separating\nAll Observed\nDifferences")
          colnames(EffSub) <- c("b", "se")
          EffSub$myX <- rownames(EffSub)
          
          addLab <- ""
          if (depVar %in% c("mathss", "readss")) {
            gain <- mean(myData[,substr(depVar, 1, 4) %&% "gain"], na.rm=T)
            EffSub$b  <- EffSub$b  / gain
            EffSub$se <- EffSub$se / gain
            addLab <- "% Year's Change in\n"
          }
          
          # Create confidence interval
          EffSub$ll <- EffSub$b - 1.96*EffSub$se
          EffSub$ul <- EffSub$b + 1.96*EffSub$se
          
          # Plot
          png(file = paste(myOutDir, "ImpactSubset_", depVar, "_", trtVar, suffix, ".png", sep = ""), res = myRes, width = myWidth, height = myHeight)
            Plot_RegAll <- ggplot(data=EffSub, aes(x=myX, y=b, fill=myX)) +
              geom_bar(stat="identity", position="dodge") +
              geom_errorbar(aes(ymin=ll, ymax=ul), width=0.1, position=position_dodge(0.1)) +
              ggtitle("Programming Impacts\non " %&% depVarNames[depVarList == depVar]) +
              ylab(addLab %&% depVarYLabs[depVarList == depVar]) + scale_y_continuous(labels = percent) +
              theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 8)) +
              guides(fill=guide_legend(title=NULL)) + scale_fill_hue(h=c(100,200), l=40) +
              theme(legend.position="none")
            print(Plot_RegAll)
            #rm(EffSub)
          dev.off()
          
        } else {
          print("  Generating plot by Site")
          
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
          
          png(file = paste(myOutDir, "Impact_", depVar, "_", trtVar, suffix, ".png", sep=""), res = myRes, width = myWidth, height = myHeight)
            Plot_RegSite <- ggplot(data=plotEff, aes(x=myX, y=b, fill=myX)) +
              geom_bar(stat="identity", position="dodge") +
              geom_errorbar(aes(ymin=ll, ymax=ul), width=0.1, position=position_dodge(0.1)) +
              ggtitle("Programming Impacts\non " %&% depVarNames[depVarList == depVar]) +
              ylab(depVarYLabs[depVarList == depVar]) +
              theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 8)) +
              guides(fill=guide_legend(title=NULL)) + scale_fill_hue(h=c(100,200), l=40) +
              theme(legend.position="none", axis.text=element_text(size=8), axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1)) +
              xlab("Site Name")
            print(Plot_RegSite)
          dev.off()
        }
        
      } # End of loop across "All treament" versus "by Site" analysis
      
    } # End of loop across dependent variables
    
  } # End of the RunRegs function definition
    
  RunRegs(useData=thinData, myDepVarList = depVarList, myTrtVars = TrtVars, suffix="")

} # End of Regression Analysis
  
  
  