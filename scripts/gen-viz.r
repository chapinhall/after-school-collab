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

  useScrambledData <- 0
  runDescGraphs    <- 1


  library(ggplot2)
  library(scales)

  allOrgs <- c("YMCA", "ASM", "CHASI", "UCAN", "Collab")
  
  YMCAfill    <- c("#ED1C24", "#92278F", "#0089D0", "#F47920", "01A490") # hex codes from YMCA standards (http://marketingfrankenstein.weebly.com/uploads/6/9/2/0/6920095/graphic_standards_for_3rd_parties.pdf)
  ASMfill     <- c("#3F227C", "#A4B635", "#211F21") # purple, lime, and grey - hex codes from ASM website
  CHASIfill   <- c("#4F91CE", "#78A360", "#FAAF5E") #light blue, green, orange - hex codes from CH+A website
  UCANfill    <- c("#006854", "#5C3D87", "#CDDA44", "#5A5A5A") #dark teal, purple, lime, gray 
  Collabfill  <- c("#000053", "#8F162B", "#A0A1A3")
  OrgvOrgfill <- c("#000053", "#8F162B", "#A0A1A3", "#DA5F45", "#005555", "#000000")
  # Chapin Colors: navy, maroon, grey, coral, teal, black

  # If graphing more than 3 sites, fill is set to a default rainbow() palette
  
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

  load(paste0(dataPath,"descStats",scramInd,".Rda"))
  
  
## Small data change - move org level school based peers indicator to Org variable
  # Also change null ASM org to "Unknown"
  
  descStats$org[descStats$site=='All Sch-Based Peers']  <- paste(descStats$org[descStats$site=='All Sch-Based Peers'], 'Sch-Based Peers')
  descStats$site[descStats$site=='All Sch-Based Peers'] <- 'All'
  descStats$site[descStats$site==''] <- 'Unknown'
  
#---------------------------------------#
#---------------------------------------#
### Graphs for Non-Org, Org, and Site ###
#---------------------------------------#
#---------------------------------------#

##### Define general plotting function  ##########
  
  makePlot <- function(
                VarList, orgnames, orgcomp = 1,
                sitenames = c("All"), sitecomp = 1,
                prognames = c("All"), grades = c("All"), 
                years = c('All'),                        # main parameters - restrict data and shape graph
                title = '', ylab = '', xlab = '',        # titles
                yscaletype = "percent",                  # to see raw means rather than percents, set yscaletype = waiver()
                xnames = waiver()                        # vector of variable names in the same order as the variables in varlist
                      ){

    # Print status for auditing purposes
        print(paste("Graphing", orgnames, VarList, "for sites", sitenames, "for programs", prognames,"for years", years, "for grades", grades))
    
    # Restrict dataset based on primary parameters
  
        if (prognames[1] != 'All' | sitenames[1] != 'All') {orgcomp = 0}
        orglist <- NULL
        sitelist <- NULL
    
        if (orgcomp==1) {
          for (o in orgnames) {
            orglist <- c(orglist, unique(grep(o, descStats$org, value=T)))      # Include both Org and non-Org obs
          }
        } else {
          orglist <- orgnames
        }
        
        if (sitecomp==1) {
          for (s in sitenames) {
            sitelist <- c(sitelist, unique(grep(s, descStats$site, value=T)))  # Include school based peers
          }  
        } else {
          sitelist <- sitenames
        }
    
    
        if (years[1] == 'All') {useYr <- unique(descStats$year)} else {useYr <- years}
        
        data <- descStats[
                (descStats$org      %in% orglist  & 
                 descStats$site     %in% sitelist & 
                 descStats$program  %in% prognames &
                 descStats$grade    %in% grades    &
                 descStats$year     %in% useYr     &
                 descStats$variable %in% VarList), ]
    
        #data <- data[!is.na(mean(data$mean, na.rm = T)),] # Remove obs that are NA for mean
        data <- data[!is.na(data$mean), ] # Remove obs that are NA for mean
    
    if (nrow(data)==0) {
      print("No data")
    } else {
    
    # Convert year to numeric for plots over time
        
      if (years[1] != "All") {data$year <- as.numeric(data$year)}
    
    # Make sure that graph will display variables from L to R in the order specified in VarList
    
      data$variable <- factor(data$variable, levels = VarList) 
    
    # Print dataset for debugging purposes (remove this once all is final)
    
     # print(data)
    
    # Define the fill value as a part of the data frame (aes.fill can only equal a variable in the df)
    
      if (sitenames[1] != "All") { 
        data$fill = data$site 
      } else if (prognames[1] != 'All') { 
        data$fill = data$program
      } else {
        data$fill = data$org
      }
    
    # Define the fill to match organization
    
      if (length(sitenames) <= 3 & length(prognames <= 3)) {
        if (is.na(orgnames[2])) { 
          useFill <- eval(parse(text = paste0(orgnames,"fill"))) 
        } else { 
          useFill <- OrgvOrgfill
        }
      } else {
        useFill <- rainbow(max(length(sitenames), length(prognames)))
      }
           
    # Create plot
    
      if (is.na(years[2])) { # Generate plots across single year
        plot <- ggplot(data=data, aes(x=variable, y=mean, fill = fill)) + 
                geom_bar(stat = 'identity', position = 'dodge', width=0.7) +
                ggtitle(title) +
                scale_y_continuous(labels = eval(parse(text = yscaletype)), name = ylab) +
                scale_x_discrete(name = xlab, labels = xnames) +
                guides(fill = guide_legend(title = NULL)) + theme(legend.position = 'bottom') +
                scale_fill_manual(values = useFill) +
                theme(axis.title.y = element_text(size = 8))
        timeind <- ""    
      } else { # Generate plots comparing multiple years
        plot <- ggplot(data=data, aes(x=year, y=mean, color = fill)) +
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
    
        if (is.na(orgnames[2])) { # Output for graphs within one organization
            if (is.na(sitenames[2]) & is.na(prognames[2])) {
              myGraphOut <- paste0(myOutDir,orgnames[1],"/",sitenames[1],"/")
              dir.create(myGraphOut, showWarnings = FALSE)
              ggsave(filename = paste0(myGraphOut, "CpsvOrg_",VarList[1],timeind,".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight) 
            } else if (is.na(sitenames[2])){
              myGraphOut <- paste0(myOutDir,orgnames[1],"/ProgramvProgram/")
              dir.create(myGraphOut, showWarnings = FALSE)
              ggsave(filename = paste0(myGraphOut, "ProgramvProgram_",VarList[1],timeind,".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight)           
            } else if (is.na(prognames[2])){
              myGraphOut <- paste0(myOutDir,orgnames[1],"/SitevSite/")
              dir.create(myGraphOut, showWarnings = FALSE)
              ggsave(filename = paste0(myGraphOut, "SitevSite_",VarList[1],timeind,".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight) 
            }
        } else { # Output for graphs comparing organizations
          myGraphOut <- paste0(myOutDir,"OrgvOrg/")
          dir.create(myGraphOut, showWarnings = FALSE)
          ggsave(filename = paste0(myGraphOut,VarList[1],timeind,".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight) 
        }
                    
    # Return plot - this displays the plot once the function has run
    
      return(plot)
      }
  }


  
  # Samples of function in action - this code can be removed eventually
  
#   # Continuous Plot - for one org, multiple orgs, one site, and multiple sites
#   makePlot("bLunch_FR", orgnames = "YMCA", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = '2013')
#   makePlot("bLunch_FR", orgnames = c("ASM", "CHASI", "YMCA"), orgcomp = 0, title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch')
#   makePlot("bLunch_FR", orgnames = "Collab", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch')
#   makePlot("bLunch_FR", orgnames = "YMCA", sitenames = "High Ridge", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = '2013')
#   makePlot("bLunch_FR", orgnames = "ASM", prognames = c("Gallery","Words","Site C"), title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = '2012')
#     # Note that we'll need a different plot format for more than 3 or so sites - really need labelled axis rather than relying on fill
# 
#   # Categorical Plot - same four options
#   makePlot(title = 'Math Test Scores', VarList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"), orgnames="CHASI")
#   makePlot(title = 'Math Test Scores', orgcomp = 0, VarList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"), orgnames=c("YMCA", "CHASI"))
#   makePlot(title = 'Math Test Scores', orgnames = 'Collab', VarList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"))
#   makePlot(title = 'Math Test Scores', VarList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"), orgnames=c("ASM"), prognames = "Gallery")
#   makePlot(title = 'Math Test Scores', VarList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"), orgnames=c("YMCA"), sitenames = c("South Side", "Lake View", "Irving Park"), sitecomp = 0)
# 
#   # Plotting Over Time
#   makePlot("bLunch_FR", orgnames = "CHASI", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = c("2012", "2013"))    
#   makePlot("bLunch_FR", orgnames = c("CHASI", "YMCA", "ASM", "Collab"), orgcomp = 0, title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = c("2012", "2013"))
#   makePlot("bLunch_FR", orgnames = "Collab", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = c("2012", "2013"))

  
######  Define Variable-Specific Criteria #########

  Vars <- c("bLunch_FR","bRace_B","bRace_H","bRace_W","bRace_M","bRace_O","fGradeLvl_PK","fGradeLvl_K","fGradeLvl_1",
            "fGradeLvl_2","fGradeLvl_3","fGradeLvl_4","fGradeLvl_5","fGradeLvl_6","fGradeLvl_7","fGradeLvl_8",
            "fGradeLvl_9","fGradeLvl_10", "fGradeLvl_11","fGradeLvl_12","isat_mathss_gain","isat_mathpl_W","isat_mathpl_B",
            "isat_mathpl_M", "isat_mathpl_E","isat_mathpl_ME","isat_mathss","Pct_Attend","isat_readss_gain","isat_readpl_W",
            "isat_readpl_B","isat_readpl_M","isat_readpl_E","isat_readpl_ME","isat_readss")
  
  VarGroup <- Vars
  metadata <- data.frame(Vars, VarGroup, stringsAsFactors = FALSE)
  metadata$VarGroup[metadata$Vars %in% c("bRace_B","bRace_H","bRace_W","bRace_M","bRace_O")] <- "Race"
  metadata$VarGroup[metadata$Vars %in% c("fGradeLvl_PK","fGradeLvl_K","fGradeLvl_1","fGradeLvl_2","fGradeLvl_3","fGradeLvl_4",
                                         "fGradeLvl_5","fGradeLvl_6","fGradeLvl_7","fGradeLvl_8","fGradeLvl_9","fGradeLvl_10",
                                         "fGradeLvl_11","fGradeLvl_12")] <- "Grades"
  metadata$VarGroup[metadata$Vars %in% c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E")] <- "ISATMathLevel"
  metadata$VarGroup[metadata$Vars %in% c("isat_readpl_W","isat_readpl_B","isat_readpl_M","isat_readpl_E")] <- "ISATReadLevel"
  
  metadata$VarXNames <- c("Free/Reduced Lunch","Black","Hispanic","White","Multiracial","Other","PK","K","1","2","3","4","5","6",
                          "7","8","9","10", "11","12","ISAT Score Gain - Math","Warning","Below","Meets","Exceeds","Meets/Exceeds",
                          "ISAT Math Scores","Percent Attending","ISAT Score Gain - Reading","Warning","Below","Meets","Exceeds",
                          "Meets/Exceeds","ISAT Reading Scores")


  # For categorical graphs, need to create lists of variables and variable labels that go together on one graph
  # These have already been defined above by VarGroup
  createlists <- function(group, tobegrouped) {
    list <- tobegrouped[metadata$VarGroup == group]
    return(list)
  }
  
  vargroups <- levels(as.factor(metadata$VarGroup))
  varlists <- sapply(vargroups, createlists, tobegrouped = metadata$Vars)
  xnamelists <- sapply(vargroups, createlists, tobegrouped = metadata$VarXNames)

  titlelist <- c("% Free/Reduced Price Lunch",
                 "Distribution of Grade Levels",
                 "Average ISAT Test Score Gain - Math",
                 "ISAT Tested Proficiency\nLevels - Math",
                 "% Meets/Exceeds ISAT Standards\nfor Math Proficiency",
                 "ISAT Math Scores by Grade",
                 "Average School Attendance",
                 "Comparison of Race/Ethnicity",
                 "Average ISAT Test Score Gain - Reading",
                 "ISAT Tested Proficiency\nLevels - Reading",
                 "% Meets/Exceeds ISAT Standards\nfor Reading Proficiency",
                 "ISAT Reading Scores by Grade")

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
  
  orgGraph <- function(orgname) {
    sites <- unique(descStats$site[descStats$org == orgname])
    sites <- sites[!grepl('Sch-Based Peers', sites)]
    lapply(sites, iterate, orgname = orgname)
  }

  lapply(allOrgs, orgGraph)
  
}
  
  # Next steps: 
      #streamline data generation to make one script for demo data and one for non-demo
      #small tweaks to these graphs (see about combining test scores, break things out across grades, etc.)
      #introduce order var (i.e. order in which bars display - Org, Non-Org, Site...etc.) - see old code samples below
      #think about what kind of site v site, org v org, and year v year plots should be standard and code those
      #add new variables (additional test scores, IEP status, etc)
  
  
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
	
