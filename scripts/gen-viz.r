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
  

#---------------------------------------#
#---------------------------------------#
### Graphs for Non-Org, Org, and Site ###
#---------------------------------------#
#---------------------------------------#

#if (1==runDescGraphs) {
  
  ## Canonical graphs are:
  # 1. Side-by-side historical histogram based on categorical variable -- e.g. grades served, races, served, tested proficiency categories
  # NSM: checking my understanding, you broke these out following the traditional designations of "categorical", even though we're now creating continuous (well, binary) versions of each variable?
  #   and--any reason that we couldn't work this into continuous vars code under "2."?
    
  
  # Grade distribution (categorical graph - special case where x and y are same)
  
  restricted <- ctsMeans[ctsMeans$Grade=="All",] # percentages are always 100% in grade-specific rows!
  # NSM: could work this into general function by added a field where users can either supply restrictions, or supply the data set to which they apply restrictions themselves. (Well, which would work just about like this, e.g.: makePlot(useData = ctsMeans[ctsMeans$Grade=="All", ], ...) )
  
  # XXX Need to add x and y labels, plot title, and a sort order for the x axis.  Also scale_y_continuous(labels = percent_format()).
  # NSM: is this code--and for FRL and proficiency level graphs--because it doesn't fit the "makePlot" graphing syntax established below, or was this a trial syntax?
  ggplot(data=restricted) +
    stat_summary(aes(x="PK", y=fGradeLvl_PK, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="1",  y=fGradeLvl_1,  fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="2",  y=fGradeLvl_2,  fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="3",  y=fGradeLvl_3,  fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="4",  y=fGradeLvl_4,  fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="5",  y=fGradeLvl_5,  fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="6",  y=fGradeLvl_6,  fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="7",  y=fGradeLvl_7,  fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="8",  y=fGradeLvl_8,  fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="9",  y=fGradeLvl_9,  fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="10", y=fGradeLvl_10, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="11", y=fGradeLvl_11, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="12", y=fGradeLvl_12, fill = Org), fun.y = mean, geom = "bar", position = "dodge")
    
  # ERW: this looks slightly different than graph on file, which may just reflect data changes, but worth
  # double checking numbers
 
  # Race (categorical graph)
  ggplot(data = ctsMeans) +
    stat_summary(aes(x="White",       y = bRace_W, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="Black",       y = bRace_B, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="Hispanic",    y = bRace_H, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="Multiracial", y = bRace_M, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="Other",       y = bRace_O, fill = Org), fun.y = mean, geom = "bar", position = "dodge")

  # ERW: Need Nick to check that I correctly interpreted these values (i.e M = "Multiracial") and that this is inclusive (leaving off "non-white" measure)
  # NSM: We've got a data dictionary for CPS that can speak to this definitely. Don't have web access now, but can help point to it if it's not immediately clear where to find it off the website.
  

  # Math and Reading Buckets (categorical graph)
  ggplot(data = ctsMeans) +
    stat_summary(aes(x="Warning", y = mathpl_W, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="Below",   y = mathpl_B, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
    stat_summary(aes(x="Meets",   y = mathpl_M, fill = Org), fun.y = mean, geom = "bar", position = "dodge") +
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
    #Site level plots:
  ggplot(data = ctsMeans) +
    stat_summary(aes(x= Site, y = bLunch_FR, fill = Org), fun.y = mean, geom = "bar", position = "dodge")

  
## wHAT INPUTS DO WE NEED TO SET UP GRAPHS AUTOMATICALLY?
  # 1. List of input variables.  If one, then continuous, if many (dummies), categorical, but we don't ever need to specify!
  # 2. If continuous, variable names for aesx (we can have this default to varname, so you don't always need to designate?)
  # 3. Scale of Y axis (can default to continuous and percent, since that will mostly be the case)
  # 4. All labels and titles (enter these)
  # 5. Unit of analysis - including data restrictions and fill.  Need to define these!

  
  
  
  ## Create function to generate the text of the different stat_summary lines
  # NSM: made a stylistic (and not substantive) change to isolate the essence of the if/else statement, just for a little bit of tidyness.
    # The commented-out portions are just for comparison, but can be deleted.
  # NSM: why use "stat_summary"? Added generality over e.g. geom_<user specified geom> is fun.y argument, except that we'll generally have that precalculated (?)
  create_stat_sum <- function(var, label, fill, type, level) {
    if (type=="cat") {
      myAesx <- paste0("'", label, "'")
      #string <- paste0("stat_summary(aes(x='",label,"', y=",var,", fill=",fill,"), fun.y = mean, geom = 'bar', position = 'dodge')")
    } else {
      myAesx <- level
      #string <- paste0("stat_summary(aes(x=",level,",   y=",var,", fill=",fill,"), fun.y = mean, geom = 'bar', position = 'dodge')")
    }
    string <- paste0("stat_summary(aes(x = ", myAesx, 
                     ", y = ", var,
                     ", fill = ", fill,
                     "), fun.y = mean, geom = 'bar', position = 'dodge')")
    return(string)
  }
  
  ## Main plotting function
        #Note that all inputs except orgname need to be stored as text strings
  makePlot <- function(VarList, VarLabelList = VarList, type = "cat", level = "Org", orgname, site = "All", grades = c("PK",1:12), fill = level){
    data <- ctsMeans[(ctsMeans$Org %in% c(orgname, "None") & ctsMeans$Site %in% c(site, "All", "None") & ctsMeans$Grade %in% grades),]
    nVars <- length(VarList)
    statsums <- mapply(create_stat_sum, var = VarList, label = VarLabelList, fill = rep(fill, nVars), type = rep(type, nVars), level = rep(level, nVars))  
    statsums <- paste(statsums, collapse = "+")
    print(statsums) # for debugging purposes - remove from final function
    plot <- ggplot(data) + eval(parse(text = statsums))
    return(plot) # ERW: Note - ggplot doesn't return anything within a function without an explicit statement (either print() or return()).
  }
  
  # Demonstrate this function in action a few times!
  
  makePlot("bLunch_FR", type = "cont", orgname = "YMCA")
  makePlot("bLunch_FR", type = "cont", orgname = "YMCA", level = "Site", site = "South Side YMCA")
  
  makePlot(VarList = c("mathpl_W","mathpl_B","mathpl_M","mathpl_E"), VarLabelList = c("Warning","Below","Meets","Exceeds"), orgname="YMCA")
  # ERW: this line is not working, but the code generated by statsums seems right?  Needs more troubleshooting.
  # NSM: first thought--the mapply may need fill, type, and level to be repeated an equivalent # of times. Have changed e.g. "fill = fill" to "fill = rep(fill, nVars)"
  
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
  
  
  