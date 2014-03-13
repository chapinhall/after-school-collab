#------------------------------------------------#
#------------------------------------------------#
# MASTER FILE FOR GENERATING DATA VISUALIZATIONS # 
#                                                #
# Authors: Nick Mader, Ian Matthew Morey,	       #
#			and Emily Wiegand		                       #  
#------------------------------------------------#
#------------------------------------------------#

## Set up workspace and designate/update file locations


  rm(list=ls())
  #myDir <- "/projects/Integrated_Evaluation_Youth_Support_Services/"
  myDir <- "H:/Integrated Evaluation Project for YSS Providers/"
  setwd(myDir)
  dataPath <- "./data/preprocessed-data/" # File path to locate and save data

  useScrambledData <- 1
  runDescGraphs    <- 1
  runRegGraphs     <- 0

  library(ggplot2)
  library(scales)

  YMCAfill  <- c("#ED1C24", "#92278F", "#0089D0", "#F47920", "01A490") # hex codes from YMCA standards (http://marketingfrankenstein.weebly.com/uploads/6/9/2/0/6920095/graphic_standards_for_3rd_parties.pdf)
  ASMfill <- c("#3F227C", "#A4B635", "#211F21") # purple, lime, and grey - hex codes from ASM website colors
  OrgvOrgfill <- c("#000053", "#8F162B", "#A0A1A3", "#DA5F45")

  Alphafill  <- c("#ED1C24", "#92278F", "#0089D0", "#F47920", "#01A490") 
  Betafill <- c("#3F227C", "#A4B635", "#211F21")


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

  ctsMeans$Site <- paste0("Site ", ctsMeans$Site)
  ctsMeans$Site[ctsMeans$Site=="Site All Alpha Sites"] <- "All Alpha Sites"  
  ctsMeans$Site[ctsMeans$Site=="Site All Beta Sites"] <- "All Beta Sites"  
  ctsMeans$Site[ctsMeans$Site=="Site Non-Participants"] <- "Non-Participants"  
  
#---------------------------------------#
#---------------------------------------#
### Graphs for Non-Org, Org, and Site ###
#---------------------------------------#
#---------------------------------------#

##### Define general plotting function  ##########
  
  makePlot <- function(
                VarList, orgnames, sitenames = c("All"), 
                grades = c("All Grades"), years = c("All Years"),              # main parameters - restrict data and shape graph
                title = '', ylab = '', xlab = '',                              # titles
                yscaletype = "percent",                                          # to see raw means rather than percents, set yscaletype = waiver()
                xnames = waiver()                                              # vector of variable names in the same order as the variables in varlist
                      ){
    
    # Restrict dataset based on primary parameters
    
        data <- ctsMeans[
                (ctsMeans$Org %in% c(orgnames, "None") & 
                 ctsMeans$Site %in% c(sitenames, 
                                      paste0("All ",orgnames[1]," Sites"),
                                      paste0("All ",orgnames[2]," Sites"),
                                      paste0("All ",orgnames[3]," Sites"),
                                      "Non-Participants") & 
                 ctsMeans$Grade %in% grades &
                 ctsMeans$Year %in% years &
                 ctsMeans$Variable %in% VarList),]
    
    # Convert year to numeric for plots over time
        
        if (years[1] != "All Years") {data$Year <- as.numeric(data$Year)}
    
    # Make sure that graph will display variables from L to R in the order specified in VarList
    
        data$Variable <- factor(data$Variable, levels = VarList) 
    
    # Print dataset for debugging purposes (remove this once all is final)
    
        print(data)

    # Define the fill value as a part of the data frame (aes.fill can only equal a variable in the df)
    
        if (sitenames[1] != "All") { data$fill = data$Site } else { data$fill = data$Org }
            
    
    # Define the fill to match organization
    
        if (is.na(orgnames[2])) { useFill <- eval(parse(text = paste0(orgnames,"fill"))) 
          } else { useFill <- OrgvOrgfill }    
             
    # Create plot
    
      if (years[1] == "All Years") {
            plot <- ggplot(data=data, aes(x=Variable, y=Mean, fill = fill)) + 
                    geom_bar(stat = 'identity', position = 'dodge', width=0.7) +
                    ggtitle(title) +
                    scale_y_continuous(labels = eval(parse(text = yscaletype)), name = ylab) +
                    scale_x_discrete(name = xlab, labels = xnames) +
                    guides(fill = guide_legend(title = NULL)) + theme(legend.position = 'bottom') +
                    scale_fill_manual(values = useFill) +
                    theme(axis.title.y = element_text(size = 8))
            timeind <- ""    
      } else {
            plot <- ggplot(data=data, aes(x=Year, y=Mean, color = fill)) +
                    geom_point() + geom_line() +
                    scale_color_manual(values = useFill) +
                    ggtitle(title) +
                    scale_y_continuous(labels = eval(parse(text = yscaletype)), name = ylab) +
                    scale_x_continuous(name = xlab) +
                    guides(color = guide_legend(title = NULL)) + theme(legend.position = 'bottom') +
                    theme(axis.title.y = element_text(size = 8))
            timeind <- "OverTime"  
      }
            
    # Output plot to saved file
    
        if (is.na(orgnames[2])) {
            if (is.na(sitenames[2])) {
              myGraphOut <- paste0(myOutDir,orgnames[1],"/",sitenames[1],"/")
              dir.create(myGraphOut, showWarnings = FALSE)
              ggsave(filename = paste0(myGraphOut, "CpsvOrg_",VarList[1],timeind,".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight) 
            } else {
              myGraphOut <- paste0(myOutDir,orgnames[1],"/SitevSite/")
              dir.create(myGraphOut, showWarnings = FALSE)
              ggsave(filename = paste0(myGraphOut, "SitevSite_",VarList[1],timeind,".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight) 
            }
        } else {
          myGraphOut <- paste0(myOutDir,"OrgvOrg/")
          dir.create(myGraphOut, showWarnings = FALSE)
          ggsave(filename = paste0(myGraphOut,VarList[1],timeind,".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight) 
        }
                    
    
    # Return plot - this displays the plot once the function has run
    
        return(plot)
  }


  
  # Samples of function in action - this code can be removed eventually
  
  # Continuous Plot - for one org, multiple orgs, one site, and multiple sites
  makePlot("bLunch_FR", orgnames = "Alpha", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch')
  makePlot("bLunch_FR", orgnames = c("Alpha", "Beta"), title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch')
  makePlot("bLunch_FR", orgnames = "Beta", sitenames = "Site R", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch')
  makePlot("bLunch_FR", orgnames = "Alpha", sitenames = c("Site A","Site B","Site C"), title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch')
    # Note that we'll need a different plot format for more than 3 or so sites - really need labelled axis rather than relying on fill

  # Categorical Plot - same four options
  makePlot(title = 'Math Test Scores', VarList = c("mathpl_W","mathpl_B","mathpl_M","mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"), orgnames="Alpha")
  makePlot(title = 'Math Test Scores', VarList = c("mathpl_W","mathpl_B","mathpl_M","mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"), orgnames=c("Alpha", "Beta"))
  makePlot(title = 'Math Test Scores', VarList = c("mathpl_W","mathpl_B","mathpl_M","mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"), orgnames=c("Beta"), sitenames = "Site P")
  makePlot(title = 'Math Test Scores', VarList = c("mathpl_W","mathpl_B","mathpl_M","mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"), orgnames=c("Alpha"), sitenames = c("Site A", "Site B", "Site C"))

  # Plotting Over Time
  makePlot("bLunch_FR", orgnames = "Alpha", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = c("2008","2009","2010","2011","2012"))    
  makePlot("bLunch_FR", orgnames = c("Alpha", "Beta"), title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = c("2008","2009","2010","2011","2012"))
  makePlot("bLunch_FR", orgnames = "Beta", sitenames = "Site R", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = c("2008","2009","2010","2011","2012"))
  makePlot("bLunch_FR", orgnames = "Alpha", sitenames = c("Site A","Site B","Site C"), title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = c("2008","2009","2010","2011","2012"))

  
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


  # For categorical graphs, need to create lists of variables and variable labels that go together on one graph
  # These have already been defined above by VarGroup
  createlists <- function(group, tobegrouped) {
    list <- tobegrouped[metadata$VarGroup == group]
    return(list)}
  
  vargroups <- levels(as.factor(metadata$VarGroup))
  varlists <- sapply(vargroups, createlists, tobegrouped = metadata$Vars)
  xnamelists <- sapply(vargroups, createlists, tobegrouped = metadata$VarXNames)

  titlelist <- c("% Free/Reduced Price Lunch",
                 "Distribution of Grade Levels",
                 "Average Test Score Gain - Math",
                 "Tested Proficiency\nLevels - Math",
                 "% Meets/Exceeds Standards\nfor Math Proficiency",
                 "Math Scores by Grade",
                 "Average School Attendance",
                 "Comparison of Race/Ethnicity",
                 "Average Test Score Gain - Reading",
                 "Tested Proficiency\nLevels - Reading",
                 "% Meets/Exceeds Standards\nfor Reading Proficiency",
                 "Reading Scores by Grade")

  ylablist <- c("Proportion on Free/Reduced Price Lunch",
                "% in Each Grade",
                "Average Scale Score Gain",
                "% in Performance Category", 
                "% Meets/Exceeds",
                "Average Test Scale Score",
                "Average Rate of School Attendance",
                "% in Each Race/Eth Group",
                "Average Scale Score Gain",
                "% in Performance Category",
                "% Meets/Exceeds",
                "Average Test Scale Score")
                

  yscalelist <- rep("percent", 12)
  yscalelist[c(3,6,9,12)] <- "waiver()"



#######  Generate Graphs Iteratively ##############
  
  
if (1==runDescGraphs) {
  
  # Demonstrating how these graphs can be generated for single organization
  # Note that mapply does not generate graphs locally, but the ggsave still executes.
   #mapplyprobs <- mapply(makePlot, VarList = varlists, 
   #                                xnames = xnamelists, 
   #                                title = titlelist,
   #                                ylab = ylablist, 
   #                                yscaletype = yscalelist, orgname = "Alpha")
  
  iterate <- function(orgname, sitename = "All") {
    mapply(makePlot, VarList = varlists, xnames = xnamelists, title = titlelist, ylab = ylablist,
           yscaletype = yscalelist, orgnames = orgname, sitenames = sitename)
  }
  
  orgs  <- levels(as.factor(ctsMeans$Org))
  orgs  <- orgs[orgs != "None"]
    
  orgGraph <- function(orgname) {
    sites <- unique(ctsMeans$Site[ctsMeans$Org == orgname])
    lapply(sites, iterate, orgname = orgname)
  }

  lapply(orgs, orgGraph)
  
}
  
  # Next steps: 
      #incorporate school based peers into graphing utility
      #streamline data generation to make one script for demo data and one for non-demo
      #clean out unneeded scripts from GH
      #small tweaks to these graphs (see about combining test scores, break things out across grades, etc.)
      #introduce order var (i.e. order in which bars display - Org, Non-Org, Site...etc.) - see old code samples below
      #think about what kind of site v site, org v org, and year v year plots should be standard and code those
  
  
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