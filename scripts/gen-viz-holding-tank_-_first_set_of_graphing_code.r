
# Graph Neighborhood characteristics - this graph is different from above
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


#-------------------------------#
#-------------------------------#
### Plot Regression Estimates ###
#-------------------------------#
#-------------------------------#

# NSM: To fit this into the graphing format determined above, need:
#    (1) standard error bars: geom_errorbar(aes(ymin=ll, ymax=ul), width=0.1, position=position_dodge(0.1)) +
#    (2) possibilitty for color ramp: scale_fill_hue(h=c(100,200), l=40)
if (runRegGraphs == 1) {
  # Plot across models
  png(file = paste(myOutDir, "Impact_", depVar, "_", trtVar, suffix, ".png", sep = ""), res = myRes, width = myWidth, height = myHeight)
  Plot_RegAll <- ggplot(data=Eff, aes(x=myX, y=b, fill=myX)) +
    geom_bar(stat="identity", position="dodge") +
    geom_errorbar(aes(ymin=ll, ymax=ul), width=0.1, position=position_dodge(0.1)) +
    ggtitle("Increasingly Nuanced Analysis\nof " %&% depVarNames[depVarList == depVar]) +
    ylab(depVarYLabs[depVarList == depVar]) +
    theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 8)) +
    guides(fill=guide_legend(title=NULL)) + scale_fill_hue(h=c(100,200), l=40) +
    theme(legend.position="none")
  print(Plot_RegAll)
  rm(Eff)
  dev.off()
  
  # Plot across sites
  png(file = paste(myOutDir, "Impact_", depVar, "_", trtVar, suffix, ".png", sep=""), res = myRes, width = myWidth, height = myHeight)
  Plot_RegSite <- ggplot(data=plotEff, aes(x=myX, y=b, fill=myX)) +
    geom_bar(stat="identity", position="dodge") +
    geom_errorbar(aes(ymin=ll, ymax=ul), width=0.1, position=position_dodge(0.1)) +
    ggtitle("Remaining Association\nwith " %&% depVarNames[depVarList == depVar]) +
    ylab(depVarYLabs[depVarList == depVar]) +
    theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 8)) +
    guides(fill=guide_legend(title=NULL)) + scale_fill_hue(h=c(100,200), l=40) +
    theme(legend.position="none", axis.text=element_text(size=8), axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1)) +
    xlab("Site Name")
  print(Plot_RegSite)
  dev.off()
} # End of loop to run reg graphs