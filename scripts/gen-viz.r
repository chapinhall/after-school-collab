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

  myfill  <- c("#CC3300", "#660000") #orange/red and dark red ... NSM - these are good YMCA colors
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
  ctsMeans <- ctsMeansLong

#---------------------------------------#
#---------------------------------------#
### Graphs for Non-Org, Org, and Site ###
#---------------------------------------#
#---------------------------------------#

#if (1==runDescGraphs) {

  
  makePlot <- function(VarList, VarLabelList = VarList, orgname, sitename = "All", grades = c("All")){
    data <- ctsMeans[
                (ctsMeans$Org %in% c(orgname, "None") & 
                 ctsMeans$Site %in% c(sitename, "All", "None") & 
                 ctsMeans$Grade %in% grades &
                 ctsMeans$Var %in% VarList),]
    print(data) # for debugging - remove later
    if (sitename != "All") {data$fill = data$Site} else {data$fill = data$Org} 
    #ERW: ggplot needs fill to be defined as part of dataframe to find it (aes won't find a variable "fill" otherwise)
    plot <- ggplot(data=data, aes(x=Variable, y=Mean, fill = fill)) + geom_bar(stat = 'identity', position = 'dodge')
    return(plot) # necessary to generate output!
  }

  # Still need: x axis labels (default to varname); 
      #scale of y axis (default to continuous/percent); 
      #all titles/labels; 
      #define fill colors
    
      
  # Testing function
  
  makePlot("bLunch_FR", orgname = "YMCA")
  makePlot("bLunch_FR", orgname = "YMCA", site = "South Side YMCA")
  makePlot(VarList = c("mathpl_W","mathpl_B","mathpl_M","mathpl_E"), VarLabelList = c("Warning","Below","Meets","Exceeds"), orgname="YMCA")

  # NSM: note to selves, around here we should also be mocking up loops across orgs, sites within orgs, multiple graph designs (with multiple formats and variable combos)
    # This step is ultimately what would be replaced in a Shiny app, where the sepcific combination of Org/level/variable, etc get chosen by the user

  orgs  <- levels(as.factor(ctsMeans$Org))
  sites <- levels(as.factor(ctsMeans$Site))
  # ERW School based peers are not currently in data set, and depending on how they are incorporated it might change what this looks like.
  # Need to iterate through orgs and sites, restricting data and creating graphs  
  
  
  
  #-------------------------------------------------------
  #### OLDER -- NICK'S CODE THAT IS UNDER ADAPTATION ABOVE
  #-------------------------------------------------------

  ## Set default graph order.  ----- ERW: I think this needs to be reworked as we finalize data set structure.
  # Order is: non-org, org-peers, org, site-peers, site. Within a given level (e.g. among orgs, or among sites)
  Non     <- grepl("Non-", ctsMean$Org)
  Peer    <- grepl("Peer", ctsMean$Org)
  OrgLvl  <- grepl("All",  ctsMean$Site)
  SiteLvl <- !(OrgLvl)
  
  ctsMean$orgGraphOrder[Non           & OrgLvl]  <- 1
  ctsMean$orgGraphOrder[Peer          & OrgLvl]  <- 2
  ctsMean$orgGraphOrder[!(Non | Peer) & OrgLvl]  <- 3
  ctsMean$orgGraphOrder[Non           & SiteLvl] <- 4 # This is the "None of the Above" category
  ctsMean$orgGraphOrder[Peer          & SiteLvl] <- 5
  ctsMean$orgGraphOrder[!(Non | Peer) & SiteLvl] <- 6
  
  # Note: xOrderVar will by default create an order based on organization levels and comparisons. Other choices may be to order by the y values, or alphabetically
  
  makePlot <- function(data, grOrgs, grLvls, grGrades, grVars, showPeers = TRUE, aesx, xOrderVar = levels(aesx), aesy = grVars, aesfill, plotTitle, xlab, ylab, extra, outFile){
    
    
    # Set the order for the x-variables to be displayed (releveling aesx)
    d[, aesx] <- factor(d[, aesx], levels = d[order(xOrderVar), aesx])
    
    # Set colors for each x (or fill? ) ... XXX Look into how colors are declared in different graph types

  varsToGraph <- c("GradeLvl", "RaceEth", "Lunch", "TestPlMath", "TestPlRead","MathME_by_Gr","ReadME_by_Gr", "MathTestSs_by_Gr", "ReadTestSs_by_Gr", "MathGain", "ReadGain", "PctAtt", "SchAtt_by_ElemGr", "SchAtt_by_HsGr")
  ## NSM: Would it be helpful to establish default x and y titles for variables?
  
  useFillCat <- c(myfill5, "#885533")
  useFill <- c(myfill5, "#885533", "#BB2800")
  
  
  }
  
  
  