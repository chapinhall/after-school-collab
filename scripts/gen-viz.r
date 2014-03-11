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
  runDescGraphs    <- 0
  runRegGraphs     <- 0

  library(ggplot2)
  library(scales)

  YMCAfill  <- c("#ED1C24", "#F47920", "#92278F") # red, orange, and purple - hex codes from YMCA standards (http://marketingfrankenstein.weebly.com/uploads/6/9/2/0/6920095/graphic_standards_for_3rd_parties.pdf)
  ASMfill <- c("#3F227C", "#A4B635", "#211F21") # purple, lime, and grey - hex codes from ASM website colors
  
  # Other old fills we could use
  ##myfill1 <- c("#CC3300", "#660000") #orange/red and dark red ... NSM - these are good YMCA colors
  ##myfill2 <- c("#CC6600", "#480000") #darker orange and maroon
  ##myfill3 <- c("#FF6600", "#480000") #lighter orange and maroon
  ##myfill4 <- c("#CC6600", "#006666") #darker orange and teal
  ##myfill5 <- c("#CC6600", "#003333") #darker orange and darker teal
  ##bluesfill <- c("#005555", "#000077")
  
  
  myRes <- 600
  myWidth  <- 4.67 #Using inches (for ggsave)
  myHeight <- 3.5 #Using inches (for ggsave)
  #Using pixels: myWidth <- 2800
  #Using pixels: myHeight <- 2100

## Load data
  if (useScrambledData==1) {
      myOutDir <- paste0(myDir,"demos/") 
      scramInd <- "_DEMO"
  } else {
      myOutDir <- paste0(myDir,"output/")
      scramInd <- ""
  }

  load(paste0(dataPath,"ctsMeans",scramInd,".Rda"))
  ctsMeans <- ctsMeansLong
  rm(ctsMeansLong)
  
#---------------------------------------#
#---------------------------------------#
### Graphs for Non-Org, Org, and Site ###
#---------------------------------------#
#---------------------------------------#

##### Define general plotting function  ##########
  
  makePlot <- function(
                VarList, orgname, sitename = "All", grades = c("All Grades"),  # main parameters - restrict data and shape graph
                title = '', ylab = '', xlab = '',                              # titles
                yscaletype = percent,                                          # to see raw means rather than percents, set yscaletype = waiver()
                xnames = waiver()                                              # vector of variable names in the same order as the variables in varlist
                      ){
    
    # Restrict dataset based on primary parameters
    
        data <- ctsMeans[
                (ctsMeans$Org %in% c(orgname, "None") & 
                 ctsMeans$Site %in% c(sitename, paste0("All ",orgname," Sites"), "Non-Participants") & 
                 ctsMeans$Grade %in% grades &
                 ctsMeans$Variable %in% VarList),]
    
    # Make sure that graph will display variables from L to R in the order specified in VarList
    
        data$Variable <- factor(data$Variable, levels = VarList) 
    
    # Print dataset for debugging purposes (remove this once all is final)
    
        print(data)

    # Define the fill value as a part of the data frame (aes.fill can only equal a variable in the df)
    
        if (sitename != "All") { data$fill = data$Site } else { data$fill = data$Org } 
    
    # Define the fill to match organization
    
        useFill <- eval(parse(text = paste0(orgname,"fill")))
     
    # Create plot
    
       plot <- ggplot(data=data, aes(x=Variable, y=Mean, fill = fill)) + 
                geom_bar(stat = 'identity', position = 'dodge', width=0.7) +
                ggtitle(title) +
                scale_y_continuous(labels = yscaletype, name = ylab) +
                scale_x_discrete(name = xlab, labels = xnames) +
                guides(fill = guide_legend(title = NULL)) + theme(legend.position = 'bottom') +
                scale_fill_manual(values = useFill)
    
    # Output plot to saved file
    
    #    myGraphOut <- paste0(myOutDir,orgname,"/",sitename,"/")
    #    print(myGraphOut)
    #    ggsave(filename = paste0(myGraphOut, "CpsVsOrg",VarList[1],".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight)
    
    # Return plot - this displays the plot once the function has run
    
        return(plot)
  }


  
  # Samples of function in action - this code can be removed eventually
  
  makePlot("bLunch_FR", orgname = "YMCA", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch')
  makePlot(title = 'Math Test Scores', VarList = c("mathpl_W","mathpl_B","mathpl_M","mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"), orgname="YMCA", sitename = "South Side YMCA")
  

  
######  Define Variable-Specific Criteria #########

  Vars <- c("bLunch_FR","bRace_B","bRace_H","bRace_W","bRace_M","bRace_O","fGradeLvl_PK","fGradeLvl_K","fGradeLvl_1",
            "fGradeLvl_2","fGradeLvl_3","fGradeLvl_4","fGradeLvl_5","fGradeLvl_6","fGradeLvl_7","fGradeLvl_8",
            "fGradeLvl_9","fGradeLvl_10", "fGradeLvl_11","fGradeLvl_12","mathgain","mathpl_W","mathpl_B","mathpl_M",
            "mathpl_E","mathpl_ME","mathss","Pct_Attend","readgain","readpl_W","readpl_B","readpl_M","readpl_E",
            "readpl_ME","readss")
  
  VarGroup <- Vars
  metadata <- data.frame(Vars,VarGroup,stringsAsFactors = FALSE)
  metadata$VarGroup[metadata$Vars %in% c("bRace_B","bRace_H","bRace_W","bRace_M","bRace_O")] <- "Race"
  metadata$VarGroup[metadata$Vars %in% c("fGradeLvl_PK","fGradeLvl_K","fGradeLvl_1","fGradeLvl_2","fGradeLvl_3","fGradeLvl_4",
                                         "fGradeLvl_5","fGradeLvl_6","fGradeLvl_7","fGradeLvl_8","fGradeLvl_9","fGradeLvl_10",
                                         "fGradeLvl_11","fGradeLvl_12")] <- "Grades"
  metadata$VarGroup[metadata$Vars %in% c("mathpl_W","mathpl_B","mathpl_M","mathpl_E")] <- "MathLevel"
  metadata$VarGroup[metadata$Vars %in% c("readpl_W","readpl_B","readpl_M","readpl_E")] <- "ReadLevel"
  
  metadata$VarXNames <- c("Free/Reduced Lunch","Black","Hispanic","White","Multiracial","Other","PK","K","1","2","3","4","5","6",
                          "7","8","9","10", "11","12","Score Gain - Math","Warning","Below","Meets","Exceeds","Meets/Exceeds",
                          "Math Scores","Percent Attending","Score Gain - Reading","Warning","Below","Meets","Exceeds",
                          "Meets/Exceeds","Reading Scores")
  # Other data to add to this data frame - titles, xlabels, ylabels.  
  # Also values for yscaletype (= waiver()) for scores and other graphs that shouldn't be graphed as percents.
  
  
  # For categorical graphs, need to create lists of variables and variable labels that go together on one graph
  # These have already been defined above by VarGroup
  createlists <- function(group, tobegrouped) {
    list <- tobegrouped[metadata$VarGroup == group]
    return(list)}
  
  vargroups <- levels(as.factor(metadata$VarGroup))
  varlists <- sapply(vargroups, createlists, tobegrouped = metadata$Vars)
  xnamelists <- sapply(vargroups, createlists, tobegrouped = metadata$VarXNames)
  
  
  
#######  Generate Graphs Iteratively ##############
  
  
if (1==runDescGraphs) {
  
  # ERW: This is the general idea - it's working.
  listtest <- lapply(varlists, makePlot, orgname = "YMCA")
  
  #ERW: But to add more inputs, we need mapply, and THIS ISN'T WORKING - ONCE IT IS, ITERATION SHOULD BE ALL SET.  
  # This is with just varlists and xnames specified, but will include titles, etc.
  mapplyprobs <- mapply(makePlot, VarList = varlists, xnames = xnamelists, orgname = "YMCA")
  iterate <- function(orgname,sitename) <- {
    mapply(makePlot, VarList = varlists, xnames = xnamelists, orgname = orgname, sitename = sitename)
  }
  
  #Once this is working, can iterate across orgs and sites.
    
  orgs  <- levels(as.factor(ctsMeans$Org))
    
  orgGraph <- (orgname) {
    sites <- unique(ctsMeans$Site[ctsMeans$Org == orgname])
    lapply(sites, iterate, orgname = orgname)
  }

  lapply(orgs, OrgGraph)
  

  
  # Next step: Fix mapply.  
    
  # After that: 
      #incorporate school based peers into graphing utility
      #incorporate multiple years and allow for iteration over years
      #introduce order var (i.e. order in which bars display - Org, Non-Org, Site...etc.) - see old code samples below
  
  # Also:
      # add org to org comparisons
      # year to year comparisons
    #For these, need to create fake data that simulates multiple orgs/multiple years (before ASM is ready)
  
  
  
  #-------------------------------------------------------
  #### Nick's old code for OrderVar
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