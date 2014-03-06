#################################################################
#------- Emily's code that Nick attempted to adapt above --------
#################################################################

getPlotData <- function(graphVal, site){
  if (graphVal=="RaceEth")    {PlotData <- as.data.frame(prop.table(table(fAnyYss, fRace),1))}
  if (graphVal=="GradeLvl")   {PlotData <- as.data.frame(prop.table(table(fAnyYss, fGradeLvl),1))}
  if (graphVal %in% c("Lunch", "MathGain", "ReadGain", "PctAtt") )     {PlotData <- ctsMeans[(ctsMeans$Grade=="All") & (is.element(ctsMeans$Site,c(org, paste0("Non-", org),paste0(org,"\nSch-Based Peers")))), ]}
  if (graphVal=="TestPlMath") {PlotData <- as.data.frame(prop.table(table(fAnyYss,mathpl),1))}
  if (graphVal=="TestPlRead") {PlotData <- as.data.frame(prop.table(table(fAnyYss,readpl),1))}
  if (graphVal %in% c("MathME_by_Gr", "ReadME_by_Gr","MathTestSs_by_Gr","ReadTestSs_by_Gr", "SchAtt_by_ElemGr")) {PlotData <- ctsMeans[(ctsMeans$Grade %in% 3:8) & (is.element(ctsMeans$Site,c(org,paste0("Non-", org),paste0(org, "\nSch-Based Peers")))),]}
  if (graphVal %in% c("SchAtt_by_ElemGr")) {PlotData <- ctsMeans[(ctsMeans$Grade %in% 1:8) & (is.element(ctsMeans$Site,c(org,paste0("Non-", org),paste0(org, "\nSch-Based Peers")))),]}
  if (graphVal %in% c("SchAtt_by_HsGr")) {PlotData <- ctsMeans[(ctsMeans$Grade %in% 9:12) & (is.element(ctsMeans$Site,c(org,paste0("Non-", org),paste0(org, "\nSch-Based Peers")))),]}
  if (site=="All") {
    return(PlotData)
  } else {
    propTableFun <- function(PlotData, varName, site){ # NSM: Why is this function defined within the other function?
      df <- data.frame(cbind(site, data.frame(prop.table(table(varName[fYssSite==site])))))
      colnames(df) <- colnames(PlotData)
      PlotData <- rbind(PlotData, df)
      return(PlotData)
    }
    if (graphVal=="RaceEth")    {PlotData <- propTableFun(PlotData, fRace, site)}
    if (graphVal=="GradeLvl")   {PlotData <- propTableFun(PlotData, fGradeLvl, site)}
    if (graphVal=="TestPlMath") {PlotData <- propTableFun(PlotData, mathpl, site)}
    if (graphVal=="TestPlRead") {PlotData <- propTableFun(PlotData, readpl, site)}
    if (graphVal %in% c("Lunch", "MathGain", "ReadGain", "PctAtt") ) {PlotData <- ctsMeans[(ctsMeans$Grade=="All") & (is.element(ctsMeans$Site,c(org,paste0("Non-",org),paste0(site,"\nSch-Based Peers"),site))),]}
    if (graphVal %in% c("MathME_by_Gr", "ReadME_by_Gr", "MathTestSs_by_Gr", "ReadTestSs_by_Gr", "SchAtt_by_ElemGr")) {PlotData <- ctsMeans[(ctsMeans$Grade %in% 3:8) & (is.element(ctsMeans$Site,c(org,paste0("Non-",org),paste0(site,"\nSch-Based Peers"),site))),]}
    if (graphVal %in% c("SchAtt_by_ElemGr")) {PlotData <- ctsMeans[(ctsMeans$Grade %in% 1:8) & (is.element(ctsMeans$Site,c(org,paste0("Non-",org),paste0(site,"\nSch-Based Peers"),site))),]}
    if (graphVal %in% c("SchAtt_by_HsGr")) {PlotData <- ctsMeans[(ctsMeans$Grade %in% 9:12) & (is.element(ctsMeans$Site,c(org,paste0("Non-",org),paste0(site,"\nSch-Based Peers"),site))),]}
    return(PlotData)
  }  
}

# Define the customizable parameters for each variable
# Parameter order:  c(aesx, aesy, aesfill           # AESTHETICS
#  plotTitle,xlabel,ylabel,      # TITLES  
#  extra)                        # OTHER PARAMETERS

GradeLvl_params <-      c(aesx = "fGradeLvl", aesy = "Freq", aesfill = "fAnyYss", 
                          plotTitle = "Distribution of Grade Levels", xlabel = "Grade", ylabel = "% in Each Grade",
                          extra = "")
RaceEth_params <-       c(aesx = "fRace", aesy = "Freq", aesfill = "fAnyYss", 
                          plotTitle = "Comparison of Race/Ethnicity", xlabel = "", ylabel = "% in Each Race/Eth Group",
                          extra = "theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 8))")
Lunch_params <-         c(aesx = "Site", aesy = "bLunch_FR", aesfill = "Site", 
                          plotTitle = "% Free/Reduced Price Lunch", xlabel = "", ylabel = "Proportion on Free/Reduced Price Lunch",
                          extra = "") #"geom_text(data=data, aes(x=Site, y=bLunch_FR, label = sprintf('%.1f%%', data$bLunch_FR*100), vjust = -1), size = 6)")
TestPlMath_params <-    c(aesx = "mathpl",aesy = "Freq",aesfill = "fAnyYss",
                          plotTitle = "Tested Proficiency\nLevels - Math", xlabel = "", ylabel = "% in Performance Category",
                          extra = "theme(axis.title.x=element_blank(), axis.title.y=element_text(size=7))") #+ geom_text(data=data, aes(x=mathpl, y=Freq, label=sprintf('%.1f%%', data$Freq*100), vjust=-1), position = position_dodge(width=0.7), size = 6) + #vjust=-1, hjust=HAdj, size=7))
TestPlRead_params <-    c(aesx = "readpl",aesy = "Freq",aesfill = "fAnyYss",
                          plotTitle = "Tested Proficiency\nLevels - Reading", xlabel = "",  ylabel = "% in Performance Category",
                          extra = "theme(axis.title.x=element_blank(), axis.title.y=element_text(size=7))") #+ geom_text(data=data, aes(x=readpl, y=Freq, label=sprintf('%.1f%%', data$Freq*100), vjust=-1), position = position_dodge(width=0.7), size = 6) + #vjust=-1, hjust=HAdj, size=7))
MathME_by_Gr_params <-  c(aesx = "Grade", aesy = "mathpl_ME", aesfill = "Site",
                          plotTitle = "% Meets/Exceeds Standard\nfor Math Proficiency", xlabel = "Grade", ylabel = "% Meets/Exceeds",
                          extra = "theme(axis.text=element_text(size=7))") #+ geom_text(data=data, aes(x=Grade, y=mathpl_ME, label=sprintf('%.1f%%', data$mathpl_ME*100), vjust=-1), position = position_dodge(width=0.7))
ReadME_by_Gr_params <-  c(aesx = "Grade", aesy = "readpl_ME", aesfill = "Site",
                          plotTitle = "% Meets/Exceeds Standard\nfor Reading Proficiency", xlabel = "Grade", ylabel = "% Meets/Exceeds",
                          extra = "theme(axis.text=element_text(size=7))") #+ geom_text(data=data, aes(x=Grade, y=mathpl_ME, label=sprintf('%.1f%%', data$readpl_ME*100), vjust=-1), position = position_dodge(width=0.7))
MathTestSs_by_Gr_params <- c(aesx = "Grade", aesy = "mathss", aesfill = "Site",
                             plotTitle = "Math Scores by Grade", xlabel = "Grade", ylabel = "Average Test Scale Score",
                             extra = "theme(legend.position='bottom', axis.title=element_text(size=7))")
ReadTestSs_by_Gr_params <- c(aesx = "Grade", aesy = "readss", aesfill = "Site",
                             plotTitle = "Reading Scores by Grade",xlabel = "Grade", ylabel = "Average Test Scale Score",
                             extra = "theme(legend.position='bottom', axis.title=element_text(size=7))")
MathGain_params <-      c(aesx = "Site", aesy = "mathgain", aesfill = "Site",
                          plotTitle = "Average Test Score Gain - Math",xlabel = "", ylabel = "Average Test Scale Score",
                          extra = theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 8), legend.position="bottom") ) # + geom_text(data=AvgGain, aes(x=VarName, y=value, label=mLabel, vjust=-1), position = position_dodge(width=0.7))
ReadGain_params <-      c(aesx = "Site", aesy = "readgain", aesfill = "Site",
                          plotTitle = "Average Test Score Gain - Reading",xlabel = "", ylabel = "Average Test Scale Score",
                          extra = theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 8), legend.position="bottom") ) # + geom_text(data=AvgGain, aes(x=VarName, y=value, label=mLabel, vjust=-1), position = position_dodge(width=0.7))
## ERW: MathGain and ReadGain used to be on one graph, but we need to come up with a way to rework the data to do that.
PctAtt_params <-        c(aesx = "Site", aesy = "Pct_Attend", aesfill = "Site",
                          plotTitle = "Average School Attendance", xlabel = "", ylabel = "Average Rate of School Attendance",
                          extra = "scale_y_continuous(labels = percent, breaks=seq(0.5, 1.0, 0.1), limits=c(0.5, 1.0), oob=squish) + theme(axis.title.x=element_blank(), axis.title.y=element_text(size = 7), legend.position='none')" ) #+ geom_text(data=PctAtt, aes(x=Site, y=Pct_Attend, label=mLabel, vjust=-1), size=6)
SchAtt_by_ElemGr_params <- c(aesx = "Grade", aesy = "Pct_Attend", aesfill = "Site",
                             plotTitle = "School Attendance by Grade",xlabel = "Grade", ylabel = "% Days Attending School",
                             extra = "scale_y_continuous(labels = percent, breaks=seq(0.5,1.0,0.1), limits=c(0.5, 1.0), oob = squish) + theme(legend.position='bottom')") # + geom_text(data=AttByGr[AttByGr$Grade %in% 1:8,], aes(x=Grade, y=Pct_Attend, label=mLabel, vjust=-1, hjust=HAdj, size=7))
SchAtt_by_HsGr_params <- c(aesx = "Grade", aesy = "Pct_Attend", aesfill = "Site",
                           plotTitle = "School Attendance by Grade",xlabel = "Grade", ylabel = "% Days Attending School",
                           extra = "scale_y_continuous(labels = percent, breaks=seq(0.5,1.0,0.1), limits=c(0.5, 1.0), oob = squish) + theme(legend.position='bottom')") # + geom_text(data=AttByGr[AttByGr$Grade %in% 1:8,], aes(x=Grade, y=Pct_Attend, label=mLabel, vjust=-1, hjust=HAdj, size=7))


DemoPlots <- function(site, graphVal, data, aesx, aesy, aesfill, plotTitle, xlab, ylab, extra){
  useFillCat <- c(myfill5, "#885533")
  useFill <- c(myfill5, "#885533", "#BB2800")
  #    myGraphOut <- paste0(myOutDir,site,"/")
  myGraphOut <- paste0("g:/YSS_Graph_Testing",site,"/")
  Plot <- ggplot(data=data, aes_string(x=aesx, y=aesy, fill=aesfill)) +
    geom_bar(stat="identity", position="dodge", width=0.7) +
    ggtitle(plotTitle) + xlab(xlab) + ylab(ylab) +
    scale_y_continuous(labels = percent) +
    theme(legend.position = "bottom") +
    scale_fill_manual(values = useFill) +
    guides(fill = guide_legend(title = NULL)) + extra
  print(Plot)
  ggsave(filename = paste0(myGraphOut, "CpsVsOrg_", graphVal, "_AnyYss.png"), dpi = myRes, width = myWidth, height = myHeight)
  return(Plot)
}

BySite <- function(site,graphVal){
  print(paste("Running graphs for",site))
  #    dir.create(file.path(myOutDir,site,fsep=""),showWarnings = F) don't want to overwrite good graphs while testing
  dir.create(file.path("g:/YSS_Graph_Testing",site,fsep=""),showWarnings = F)
  params <- eval(parse(text = paste0(graphVal,"_params")))
  PlotData <- getPlotData(graphVal,site)
  DemoPlots(site, graphVal, PlotData, params["aesx"], params["aesy"], params["aesfill"], params["plotTitle"], params["xlabel"], params["ylabel"], eval(parse(text = params["extra"])))
}


# Test each graph as added

#  BySite("South Side YMCA","GradeLvl")
#  BySite("South Side YMCA","RaceEth")  
#  BySite("South Side YMCA","Lunch")
#  BySite("South Side YMCA","TestPlMath")
#  BySite("South Side YMCA","TestPlRead")
#  BySite("South Side YMCA","MathTestSs_by_Gr")
#  BySite("South Side YMCA","ReadTestSs_by_Gr")
#  BySite("South Side YMCA","MathGain")
#  BySite("South Side YMCA","ReadGain")
#  BySite("South Side YMCA", "PctAtt")
#  BySite("South Side YMCA", "SchAtt_by_ElemGr")
#  BySite("South Side YMCA", "SchAtt_by_HsGr")


# Ultimately, loop through varlist and site list like so:

mapply(BySite, rep(sites,  each=length(varsToGraph)), rep(varsToGraph, times=length(sites)))


