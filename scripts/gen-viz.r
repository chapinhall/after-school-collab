#------------------------------------------------#
#------------------------------------------------#
# MASTER FILE FOR GENERATING DATA VISUALIZATIONS # 
#                                                #
# Authors: Nick Mader, Ian Matthew Morey,	       #
#			and Emily Wiegand		                       #  
#------------------------------------------------#
#------------------------------------------------#

## NOTE -- THIS WAS FORKED FROM THE "anayze-and-report.r" file

## Set up workspace and designate/update file locations

  rm(list=ls())
  #myDir <- "/projects/Integrated_Evaluation_Youth_Support_Services/"
  myDir <- "H:/Integrated Evaluation Project for YSS Providers/"
  setwd(myDir)
  dataPath <- "./data/preprocessed-data/" # File path to locate and save data

  useScrambledData <- 0
  runDescGraphs    <- 1
  runRegGraphs     <- 0

  library(ggplot2)
  library(scales)

  myfill  <- c("#CC3300", "#660000") #orange/red and dark red
  myfill2 <- c("#CC6600", "#480000") #darker orange and maroon
  myfill3 <- c("#FF6600", "#480000") #lighter orange and maroon
  myfill4 <- c("#CC6600", "#006666") #darker orange and teal
  myfill5 <- c("#CC6600", "#003333") #darker orange and darker teal
  bluesFill <- c("#005555", "#000077")
  
  myRes <- 600
  myWidth  <- 4.67 #Using inches (for ggsave)
  myHeight <- 3.5 #Using inches (for ggsave)
  #Using pixels: myWidth <- 2800
  #Using pixels: myHeight <- 2100

## Load data - ERW: I don't think we need this kind of data anymore. 
  # Will need to rework the scrambled indicators to pull the appropriate ctsMeans, of course.

#  if (useScrambledData==1) {
#    load(paste0(dataPath,"Scram.Rda"))
#    myData <- Scram
#    rm(Scram)
#    myOutDir <- paste0(myDir,"demos/") 
#    scramInd <- "_DEMO"
#  } else {
#    load(paste0(dataPath,"subset_CpsYss_PP13.Rda"))
#    myOutDir <- paste0(myDir,"output/")
#    scramInd <- ""
#  }

#  try(detach(myData), silent=T)
#  attach(myData)

#  load(paste0(dataPath,"ctsMeans",scramInd,".Rda"))
  load(paste0(dataPath,"ctsMeans.Rda"))
  

#---------------------------------------#
#---------------------------------------#
### Graphs for Non-Org, Org, and Site ###
#---------------------------------------#
#---------------------------------------#

#if (1==runDescGraphs) {
  
  
  

    
  ## Canonical graphs are:
  # 1. Side-by-side historical histogram based on categorical variable -- e.g. grades served, races, served, tested proficiency categories
    
  
  # Grade distribution (categorical graph - special case where x and y are same)
  
  restricted <- ctsMeans[ctsMeans$Grade=="All",] # percentages are always 100% in grade-specific rows!
  
  # XXX Need to add x and y labels, plot title, and a sort order for the x axis.  Also scale_y_continuous(labels = percent_format()).
  ggplot(data=restricted) +
    stat_summary(aes(x="PK", y=fGradeLvl_PK, fill = Org),fun.y = mean, geom="bar", position = "dodge") +
    stat_summary(aes(x="1", y=fGradeLvl_1, fill = Org),fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="2", y=fGradeLvl_2, fill = Org),fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="3", y=fGradeLvl_3, fill = Org),fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="4", y=fGradeLvl_4, fill = Org),fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="5", y=fGradeLvl_5, fill = Org),fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="6", y=fGradeLvl_6, fill = Org),fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="7", y=fGradeLvl_7, fill = Org),fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="8", y=fGradeLvl_8, fill = Org),fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="9", y=fGradeLvl_9, fill = Org),fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="10", y=fGradeLvl_10, fill = Org),fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="11", y=fGradeLvl_11, fill = Org),fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="12", y=fGradeLvl_12, fill = Org),fun.y = mean, geom = "bar", position = "dodge")
    
  # ERW: this looks slightly different than graph on file, which may just reflect data changes, but worth
  # double checking numbers
 
  # Race (categorical graph)
  ggplot(data = ctsMeans) +
    stat_summary(aes(x="White", y=bRace_W, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="Black", y=bRace_B, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="Hispanic", y=bRace_H, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="Multiracial", y=bRace_M, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="Other", y=bRace_O, fill = Org), fun.y = mean, geom = "bar", position = "dodge")

  # ERW: Need Nick to check that I correctly interpreted these values (i.e M = "Multiracial") and that this is inclusive (leaving off "non-white" measure)
  

  # Math and Reading Buckets (categorical graph)
  ggplot(data = ctsMeans) +
    stat_summary(aes(x="Warning", y = mathpl_W, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="Below", y = mathpl_B, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="Meets", y = mathpl_M, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="Exceeds", y = mathpl_E, fill = Org), fun.y = mean, geom = "bar", position = "dodge")
  # Reading is obviously the same
  


  # 2. Comparison on a single (continuous) measure -- e.g. % free/reduced priced lunch
  #      * Non-org, org, and possibly school-based peers -- currently vertical bars, with value labels
  #      * Sites versus site (which shows district-level average as an overlaid line) -- currently horizontal bars, where axis labels are site name with sample size
  #           -Want this in both 
  
  
  # Free lunch
  
  ggplot(data = ctsMeans, aes(x = Org, y = bLunch_FR, fill = Org)) +
      stat_summary(fun.y = mean, geom = "bar", position = "dodge")
  # This can be written as follows to more closely resemble categorical measures:
  ggplot(data = ctsMeans) +
      stat_summary(aes(x= Org, y = bLunch_FR, fill = Org), fun.y = mean, geom = "bar", position = "dodge")
  

  
## wHAT INPUTS DO WE NEED TO SET UP GRAPHS AUTOMATICALLY?
  # 1. List of input variables.  If one, then continuous, if many (dummies), categorical, but we don't ever need to specify!
  # 2. If continuous, variable names for aesx (we can have this default to varname, so you don't always need to designate?)
  # 3. Scale of Y axis (can default to continuous and percent, since that will mostly be the case)
  # 4. All labels and titles (enter these)
  # 5. Unit of analysis - including data restrictions and fill.  Need to define these!

  
  

# Need a function to restrict data as need by org, site, grade (could add year to this in a future version)
  
  restrictData <- function (org, site = "All", grades = c("PK",1:12)) { 
    output <- ctsMeans[(ctsMeans$Org==org & ctsMeans$Site==site & ctsMeans$Grade %in% grades),]
    return(output)
  }
  
  orgs <- levels(as.factor(ctsMeans$Org))
  sites <- c("All",levels(as.factor(ctsMeans$Site))
  # ERW School based peers are not currently in data set, and depending on how they are incorporated it might change what this looks like.

  # Need to iterate through orgs and sites, restricting data and creating graphs  
  
  
  # A few funtions to help the plotting function
  
  ## First, creating a data frame of variable names and their labels - I think I can cut this, but committing for posterity first!
  varpairs <- function(varlist, varlabellist) {
   pairs <- data.frame(
              var=character(length=length(varlist)), 
              lab=character(length=length(varlist)),
              stringsAsFactors=FALSE)
   for (i in 1:length(varlist)) {
      pairs$var[i] <- varlist[i]
      pairs$lab[i] <- varlabellist[i]
    }
     return(pairs)
  }

  ## Create function to generate the text of the different stat_summary lines
  create_stat_sum <- function(var, label, fill) {
    string <- paste0("stat_summary(aes(x=",label,", y=",var,", fill=",fill,"), fun.y = mean, geom = 'bar', position = 'dodge') + ")
    return(string)
  }
  
  ## Main plotting function
  makePlot <- function(VarList, VarLabelList = VarList, orgname, site = "All", grades = c("PK",1:12), fill = Org){
    # Note that all inputs need to be stored as text strings
    #First, restrict data
      data <- ctsMeans[(ctsMeans$Org==org & ctsMeans$Site==site & ctsMeans$Grade %in% grades),]
    #Second, generate stat_summaries - so that you have the right number of lines
      pairs <- varpairs(VarList, VarLabelList) 
      stats <- mapply(create_stat_sum, VarList, VarLabelList, fill)  
      stats <- paste(stats, collapse='')
      
    
  }
  
    
makePlot <- function(data, grOrgs, grLvls, grGrades, grVars, showPeers = TRUE, aesx, xOrderVar = levels(aesx), aesy = grVars, aesfill, plotTitle, xlab, ylab, extra, outFile){  
  
  myPlot <- ggplot(data=d, aes_string(x=aesx, y=aesy, fill=aesfill)) +
    geom_bar(stat="identity", position="dodge", width=0.7) +
    ggtitle(plotTitle) + xlab(xlab) + ylab(ylab) +
    scale_y_continuous(labels = percent) +
    theme(legend.position = "bottom") +
    scale_fill_manual(values = useFill) +
    guides(fill = guide_legend(title = NULL)) + extra
  ggsave(filename = outFile, plot = myPlot, dpi = myRes, width = myWidth, height = myHeight)
  #return(Plot) ... NSM: is this necessary? ERW: Yes.  Otherwise, ggplot generates nothing if within a function.
  
  }
  
  
  
  
  
  
  
  ## Set default graph order.  ----- ERW: I think this needs to be reworked as we finalize data set structure.
  # Order is: non-org, org-peers, org, site-peers, site. Within a given level (e.g. among orgs, or among sites)
  Non  <- grepl("Non-",  ctsMean$Org)
  Peer <- grepl("Peer", ctsMean$Org)
  OrgLvl  <- grepl("All", ctsMean$Site)
  SiteLvl <- !(OrgLvl)
  
  ctsMean$orgGraphOrder[Non           & OrgLvl]  <- 1
  ctsMean$orgGraphOrder[Peer          & OrgLvl]  <- 2
  ctsMean$orgGraphOrder[!(Non | Peer) & OrgLvl]  <- 3
  ctsMean$orgGraphOrder[Non           & SiteLvl] <- 4 # This is the "None of the Above" category
  ctsMean$orgGraphOrder[Peer          & SiteLvl] <- 5
  ctsMean$orgGraphOrder[!(Non | Peer) & SiteLvl] <- 6
  
  
  # set-aside arguments: site, outFile = paste0("g:/YSS_Graph_Testing/",site,"/CpsVsOrg_", graphVal, "_AnyYss.png")
  # Note: xOrderVar will by default create an order based on organization levels and comparisons. Other choices may be to order by the y values, or alphabetically
  
  makePlot <- function(data, grOrgs, grLvls, grGrades, grVars, showPeers = TRUE, aesx, xOrderVar = levels(aesx), aesy = grVars, aesfill, plotTitle, xlab, ylab, extra, outFile){
    
    
    # Set the order for the x-variables to be displayed (releveling aesx)
    d[, aesx] <- factor(d[, aesx], levels = d[order(xOrderVar), aesx])
    
    
    # Set colors for each x (or fill? ) ... XXX Look into how colors are declared in different graph types
    
  
  
  
    
    
    
  varsToGraph <- c("GradeLvl", "RaceEth", "Lunch", "TestPlMath", "TestPlRead","MathME_by_Gr","ReadME_by_Gr", "MathTestSs_by_Gr", "ReadTestSs_by_Gr", "MathGain", "ReadGain", "PctAtt", "SchAtt_by_ElemGr", "SchAtt_by_HsGr")
  ## Would it be helpful to establish default x and y titles for variables?
      
  
  
  **** To handle certain grade-specific variables like ISAT averages, Attendance by Elem grade, or attendance by HS grades, we could handle that in the create_summary_stats.R code where a variable is
        defined for a subset of grades, is NA fo other grades, and where we drop NAs coming into the graphing code
    ### NSM To handle regressions, we could add an option regarding color scale, which would just create just a ramping color scale, as well as an option to add standard errors
  
  
  
  useFillCat <- c(myfill5, "#885533")
    useFill <- c(myfill5, "#885533", "#BB2800")
  
  
    
       
    myPlot <- ggplot(data=d, aes_string(x=aesx, y=aesy, fill=aesfill)) +
      geom_bar(stat="identity", position="dodge", width=0.7) +
      ggtitle(plotTitle) + xlab(xlab) + ylab(ylab) +
      scale_y_continuous(labels = percent) +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = useFill) +
      guides(fill = guide_legend(title = NULL)) + extra
    ggsave(filename = outFile, plot = myPlot, dpi = myRes, width = myWidth, height = myHeight)
    #return(Plot) ... NSM: is this necessary?
     
  }
  
  
  # 4. Comparison on multiple, thematically-linked measures -- e.g. neighborhood characteristics. This is sort of an extension of the comparison on a single (continuous) measure
  
    This is achieved the same as Graph 3, but where the aesx is its own construction. Just need to do special data prep ahead of time.
  
  
  # 5. Comparison of a single measure across sites (or programs across sites)
    
    Same as Graph 3, but with no fill since we do not want to compare within the x. Just have x and y. However, the x is a long list of sites, probably affiliated with a given organization. (Or we may compare
      sites across organizations if doing benchmarking activities. )
  
  # 6. Regression analysis, comparing specification -- e.g. raw vs. gain vs. lagged reg vs. full
  
    For now, the default is to use the ascending color scale no matter what, and to apply standard errors no matter what. By spec or by site should differ only in data fed in and determination of order.
  
  # 7. Regression analysis, comparing sites

  
  
  
  ## Within calls to canonical graphs, the arguments should be pretty straightforward. ERW's calls using "_params" are pretty simple. Could be even more simplified if calls were grouped by canonical graph
  
  
 
  
  # 2. Comparison on a single (continuous) measure -- e.g. % free/reduced priced lunch
  #      * Non-org, org, and possibly school-based peers -- currently vertical bars, with value labels
  
    myGraphOut <- paste0("g:/YSS_Graph_Testing",site,"/")
        Plot <- ggplot(data=data, aes_string(x=aesx, y=aesy, fill=aesfill)) +
        geom_bar(stat="identity", position="dodge", width=0.7) +
        ggtitle(plotTitle) + xlab(xlab) + ylab(ylab) +
        scale_y_continuous(labels = percent) +
        theme(legend.position = "bottom") +
        scale_fill_manual(values = useFill) +
        guides(fill = guide_legend(title = NULL)) + extra
      print(Plot)
  
  # 3. Comparison on a single (continuous measure, calculated/presented by category -- e.g. test scale scores or school attendance by grade)
  
  
  
  # 4. Comparison on multiple, thematically-linked measures -- e.g. neighborhood characteristics. This is sort of an extension of the comparison on a single (continuous) measure
  # 5. Comparison of a single measure across sites (or programs across sites)
  # 6. Regression analysis, comparing specification -- e.g. raw vs. gain vs. lagged reg vs. full
  # 7. Regression analysis, comparing sites
  
  
  
  
  ############################################################################################################
  
  
  
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
  }