#------------------------------------------------#
#------------------------------------------------#
# MASTER FILE FOR GENERATING DATA VISUALIZATIONS # 
#                                                #
# Authors: Nick Mader, Ian Matthew Morey,	       #
#			and Emily Wiegand		                       #  
#------------------------------------------------#
#------------------------------------------------#

#------------------------------------------------------
## Set up workspace and designate/update file locations
#------------------------------------------------------

  rm(list=ls())
  library(ggplot2)
  library(scales)
  library(reshape)

  setwd("~/GitHub/after-school-collab")
  source("./scripts/helper-functions.r") # XXX Need a way to identify the folder of the current file
  myDir <- "H:/Integrated Evaluation Project for YSS Providers/"
  setwd(myDir)
  dataPath <- "./data/constructed-data/" # File path to locate and save data

  useScrambledData <- 0
  runDescGraphs    <- 1

  allOrgs <- c("YMCA", "ASM", "CHaA", "UCAN", "Collab")
  
  YMCAfill    <- c("#ED1C24", "#92278F", "#0089D0", "#F47920", "01A490") # hex codes from YMCA standards (http://marketingfrankenstein.weebly.com/uploads/6/9/2/0/6920095/graphic_standards_for_3rd_parties.pdf)
  ASMfill     <- c("#3F227C", "#A4B635", "#211F21") # purple, lime, and grey - hex codes from ASM website
  CHaAfill    <- c("#4F91CE", "#78A360", "#FAAF5E") #light blue, green, orange - hex codes from CH+A website
  UCANfill    <- c("#006854", "#5C3D87", "#CDDA44", "#5A5A5A") #dark teal, purple, lime, gray 
  Collabfill  <- c("#000053", "#8F162B", "#A0A1A3")
  OrgvOrgfill <- c("#000053", "#8F162B", "#A0A1A3", "#DA5F45", "#005555", "#000000")
  # Chapin Colors: navy, maroon, grey, coral, teal, black
  neutralFill <- c(#3399FF)

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
  myWidth  <- 4.67 # Using inches (for ggsave). This is 2800 in pixels.
  myHeight <- 3.50 # Using inches (for ggsave). This is 2100 in pixels.
  
#-----------------------
## Load and Prepare Data
#-----------------------

  ## Load data
    if (useScrambledData==1) {
        myOutDir <- paste0(myDir,"demos/") 
        scramInd <- "_DEMO"
    } else {
        myOutDir <- paste0(myDir,"output/")
        scramInd <- ""
    }
  
    load(p0(dataPath, "descStats", scramInd, ".Rda"))
    
  ## Small data change - move designation of "all school based peers" to the name of the "org"
    descStats$org[descStats$site=='All Sch-Based Peers']  <- paste(descStats$org[descStats$site=='All Sch-Based Peers'], 'Sch-Based Peers')
    descStats$site[descStats$site=='All Sch-Based Peers'] <- 'All'
    descStats$site[descStats$site==''] <- 'Unknown'
  
  ## Simplify Site Names
    descStats$site <- gsub(" School Of Excellence", "", descStats$site)
    descStats$site <- gsub(" Elementary", "", descStats$site)
    descStats$site <- gsub(" Academy", "", descStats$site)
    gsub("Howe", "", descStats$site)
    
  ## Prepare factor orders to go from "non-org", "sch-based peers", "own value"
    for (v in c("org", "site")){
      vVals <- unique(descStats[, v])
      nonVals  <- grep("Non-",  vVals, value = T)
      peerVals <- grep("Peers", vVals, value = T)
      valVals  <- vVals[!(vVals %in% c(nonVals, peerVals))]
      descStats[, v] <- factor(descStats[, v], levels = c(nonVals, peerVals, valVals))
    }
  
#---------------------------------------#
#---------------------------------------#
### Graphs for Non-Org, Org, and Site ###
#---------------------------------------#
#---------------------------------------#

##### Define general plotting function  ##########
  
  makePlot <- function(
                orgList = allOrgs, siteList = c("All"), progList = c("All"), gradeList = c("All"), yearList = c('All'),
                compareAcross = "Benchmarks", varList, varNames = "",                                    
                title = '', ylab = '', xlab = '', yscaletype = "percent"
              ){    
              # compareAcross can take values "Benchmarks", "Orgs", "Sites", "Programs", "Years", "Vars"
              # varNames is vector of x-axis labels, in the same order as the variables in varList
              # to see raw means rather than percents, set yscaletype = "comma"

    # Print status for auditing purposes
      print(paste("Graphing orgList =", orgList, "varList =", varList, "for sites", siteList, "for programs", progList,"for years", years, "for grades", gradeList))
    
    # Check consistency of function call based on all args
      
      stopifnot((compareAcross != "Years" & length(yearList) == 1) |
                (compareAcross == "Years" & length(yearList)  > 1))
      stopifnot((compareAcross != "Vars"  & length(varList)  == 1) |
                (compareAcross == "Vars"  & length(varList)   > 1))
      
      if (compareAcross == "Years"){
        stopifnot(length(yearList) > 1, length(orgList) == 1, length(siteList) == 1, length(progList) == 1, siteList == "All" | progList == "All")
        myOrgs <- orgList; mySites <- siteList; myProgs <- progList
      } else if (compareAcross == "Vars"){
        stopifnot(length(varList) > 1,  length(orgList) == 1, length(siteList) == 1, length(progList) == 1, siteList == "All" | progList == "All")
        myOrgs <- orgList; mySites <- siteList; myProgs <- progList
      } else if (compareAcross == "Orgs") { # orgList[1] != "All" & ... XXX Could simplify this with a switch()
        stopifnot(length(orgList) > 1)
        myOrgs <- orgList; mySites <- "All"; myProgs <- "All"
      } else if (compareAcross == "Sites"){
        stopifnot(length(orgList) == 1, length(siteList) > 1)
        myOrgs <- orgList; mySites <- siteList; myProgs <- "All"
      } else if (length(progList) > 1) {
        myOrgs <- orgList; mySites <- "All"; myProgs <- progList
      } else if (compareAcross == "Benchmarks"){
        if (length(orgList) == 1){
          stopifnot(orgList != "All")
          myOrgs <- grep(paste(orgList, collapse="|"), descStats$org, value = T)
          mySites <- "All"; myProgs <- "All" # Enforcing that this must by site- and program-wide
        } else if (length(siteList) == 1){
          stopifnot(length(orgList) != 1, orgList != "All")
          myOrgs <- orgList; myProgs <- "All" # Enforcing (for now) that site must be across all programs (since we're not currently cross-classifying)
          mySites <- grep(paste(siteList, collapse="|"), descStats$site[descStats$org == orgList], value = T)
        } else if (length(progList) == 1){
          stopifnot(orgList[1] != "All", progList != "All")
          myOrgs <- orgList; mySites <- "All"
          myProgs <- grep(paste(progList, collapse="|"), descStats$program[descStats$org %in% orgList], value = T)
        } else {
          stop("Could not identify the benchmark comparison that was requested")
        }
      } else {
        stop("Count not identify the comparison that was requested")
      }
      
      # Subset to specified orgs, sites, programs, gradeList, years, and variables
      myData <- descStats[with(descStats, org      %in% myOrgs    &
                                          site     %in% mySites   &
                                          program  %in% progList  &
                                          grade    %in% gradeList &
                                          year     %in% myYears   &
                                          variable %in% varList), ]
  
      myData <- myData[!is.na(myData$mean), ] # Remove obs that are NA for mean
      myData$site <- factor(as.character(myData$site)) # Resets the levels to only the specified sites
    
    if (nrow(myData)==0) stop("Had no rows of data in the specified request")
    
    # Convert year to numeric for plots over time
      myData$year <- as.numeric(myData$year)
    
    # Make sure that graph will display variables from L to R in the order specified in varList
      myData$variable <- factor(myData$variable, levels = varList) 
    
    # Define the fill value as a part of the data frame (in the ggplot statement, aes.fill can only equal a variable in the df)
      # XXX Could change this over to a switch()
      if (compareAcross == "Sites") { 
        myData$fill <- myData$site 
      } else if (compareAcross == "Programs") { 
        myData$fill <- myData$program
      } else if (compareAcross == "Orgs") {
        myData$fill <- myData$org
      }
    
    # Define the fill to match organization
      if (length(siteList) <= 3 & length(progList <= 3)) {
        if (compareAcross != "Orgs") { 
          useFill <- get(p0(orgList, "fill"))
        } else { 
          useFill <- OrgvOrgfill
        }
      } else {
        useFill <- neutralFill
        # rainbow(max(length(siteList), length(progList)))
      }
           
    # Create plot
    
      if (compareAcross != "Years") {
        plot <- ggplot(data = myData, aes(x = variable, y = mean, fill = fill)) + 
                  geom_bar(stat = 'identity', position = 'dodge', width = 0.7) +
                  ggtitle(title) + 
                  scale_y_continuous(labels = ep(yscaletype), name = ylab, breaks = waiver()) +
                  scale_x_discrete(name = xlab, labels = varNames) +
                  guides(fill = guide_legend(title = NULL)) + theme(legend.position = 'bottom') +
                  scale_fill_manual(values = useFill) +
                  theme(axis.title.y = element_text(size = 8))
      } else { # Generate plots comparing multiple years
        plot <- ggplot(data = myData, aes(x = year, y = mean, color = fill)) +
                geom_point() + geom_line() +
                scale_color_manual(values = useFill) +
                ggtitle(title) +
                scale_y_continuous(lables = ep(yscaletype), name = ylab, breaks = waiver()) +
                scale_x_continuous(name = xlab) +
                guides(color = guide_legend(title = NULL)) + theme(legend.position = 'bottom') +
                theme(axis.title.y = element_text(size = 8))
      }
            
    # Output plot to saved file
    
      # XXX Come back and rearrange this. There should be a way to more simply assign prefix (e.g. "CpsvOrg_" or "SitevSite_")
      #     and the graph location in the conditionals, and do the dir.create() and ggsave() outside the conditionals.
      # XXX Should be a more elegant way to determine the conditional. Consider creating a categorical argument, e.g.
      #     compareLvl = "x", where x \in {org, site, program, year}, and a check is made to make sure that 
      #     the levels besides the compareLvl are properly specified. E.g., if compareLvl = site, then a single org must be
      #     specified, and program must be "All" (since we currently do not calculate cross-classifications of program-by-site
      #     enrollments), and year is a single value that is not "All".
      if (is.na(orgList[2])) { # I.e. Output for graphs within one organization
          if (is.na(siteList[2]) & is.na(progList[2])) { # I.e. if it's for all sites and all programs
            myGraphOut <- p0(myOutDir, orgList[1], "/", siteList[1], "/")
            dir.create(myGraphOut, showWarnings = FALSE)
            ggsave(filename = p0(myGraphOut, "CpsvOrg_", varList[1], timeind, ".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight)
          } else if (is.na(siteList[2])){ # I.e. if it's across sites
            myGraphOut <- p0(myOutDir, orgList[1], "/SitevSite/")
            dir.create(myGraphOut, showWarnings = FALSE)
            ggsave(filename = p0(myGraphOut, "SitevSite_",varList[1],timeind,".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight)           
          } else if (is.na(progList[2])){
            myGraphOut <- p0(myOutDir,orgList[1],"/ProgramvProgram/")
            dir.create(myGraphOut, showWarnings = FALSE)
            ggsave(filename = p0(myGraphOut, "ProgramvProgram_", varList[1], timeind, ".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight) 
          }
      } else { # Output for graphs comparing organizations
        myGraphOut <- p0(myOutDir, "OrgvOrg/")
        dir.create(myGraphOut, showWarnings = FALSE)
        ggsave(filename = p0(myGraphOut, varList[1], timeind, ".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight) 
      }
                  
      # Return plot - this displays the plot once the function has run
        return(plot)

  } # End of the MakePlot() function

  # Samples of function in action - this code can be removed eventually
  makePlot("bLunch_FR", orgList = "CHaA", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = '2013')  
  makePlot(c("isat_mathpl_ME", "isat_readpl_ME"), orgList = "CHaA", title = "ISAT Proficiency - Meets Exceeds", ylab = '% Meets/Exceeds', years = '2013', varNames = c("Math", "Reading"))
  makePlot(c("Tract_ViolentCrimes_PerHundr"), orgList = "CHaA", title = "Neighborhood Violence", ylab = 'Violent Crimes Per 100 Residents', years = '2013', yscaletype = "comma")
  makePlot(c("Tract_ViolentCrimes_PerHundr", "Tract_Pct_LtHsEd"), orgList = "CHaA", title = "Neighborhood Characteritics", ylab = '', years = '2013', yscaletype = "comma", varNames = c("Violent Crimes\nper 100", "Adults < HS Ed"))
  makePlot(c("bRace_B", "bRace_H"), orgList = "CHaA", title = "Youth Race/Ethnicity", ylab = '% of Youth', years = '2013', varNames = c("African American", "Hispanic"))

  CHaAsites <- unique(as.character(descStats$site[descStats$org == "CHaA"]))
  CHaAsites <- CHaAsites[!grepl("All|Peers", CHaAsites)]
  makePlot("bLunch_FR", orgList = "CHaA", siteList = CHaAsites, sitecomp = 0, title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = '2013')
  makePlot("isat_mathpl_ME", orgList = "CHaA", siteList = CHaAsites, sitecomp = 0, title = "ISAT Math Proficiency - Meets Exceeds", ylab = '% Meets/Exceeds', years = '2013')
  makePlot("Tract_ViolentCrimes_PerHundr", orgList = "CHaA", siteList = CHaAsites, sitecomp = 0, title = "Neighborhood Violence", ylab = 'Violent Crimes Per 100 Residents', years = '2013', yscaletype = "comma")
  makePlot("bRace_B", orgList = "CHaA", siteList = CHaAsites, sitecomp = 0, title = "Youth Race - African American", ylab = '% African American Youth', years = '2013')
  makePlot("bRace_H", orgList = "CHaA", siteList = CHaAsites, sitecomp = 0, title = "Youth Race - Hispanic", ylab = '% Hispanic Youth', years = '2013')  
  
  # Categorical Plot - same four options
  makePlot(title = 'Math Test Scores', varList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), varNames = c("Warning","Below","Meets","Exceeds"), orgList="CHaA")
  makePlot(title = 'Math Test Scores', orgcomp = 0, varList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), varNames = c("Warning","Below","Meets","Exceeds"), orgList=c("YMCA", "CHaA"))
  makePlot(title = 'Math Test Scores', orgList = 'Collab', varList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), varNames = c("Warning","Below","Meets","Exceeds"))
  makePlot(title = 'Math Test Scores', varList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), varNames = c("Warning","Below","Meets","Exceeds"), orgList=c("ASM"), progList = "Gallery")
  makePlot(title = 'Math Test Scores', varList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), varNames = c("Warning","Below","Meets","Exceeds"), orgList=c("YMCA"), siteList = c("South Side", "Lake View", "Irving Park"), sitecomp = 0)

  # Plotting Over Time
  makePlot("bLunch_FR", orgList = "CHaA", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = c("2012", "2013"))
  makePlot("bLunch_FR", orgList = c("CHaA", "YMCA", "ASM", "Collab"), orgcomp = 0, title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = c("2012", "2013"))
  makePlot("bLunch_FR", orgList = "Collab", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = c("2012", "2013"))

#---------------------------------------#
#---------------------------------------#
### Design Different Graphs To Be Run ###
#---------------------------------------#
#---------------------------------------#

  # Create a function to receive a list of variables and names, and organize it into a data frame
  sortVars <- function(...){
    x <- c(...)
    odds  <- x[seq(1, length(x), by = 2)]
    evens <- x[seq(2, length(x), by = 2)]
    df <- data.frame(var = odds, varName = evens)
    return(df)
  }

  #-------------------------------------
  # Specify the list of graphs to be run
  #-------------------------------------
  graphsList <-
    c(list(Group = "Lunch", Title = "Youth Receiving Free/Reduced Price Lunch", yLabel = "% Receiving Free/Reduced Price Lunch", yscale = "percent",
        Vars = sortVars("bLunch_FR", "Free/Reduced Lunch")),
      list(Group = "Grade", Title = "Distribution of Grade Levels", yLabel = "% in Each Grade", yscale = "percent",
        Vars = sortVars("fGradeLvl_PK", "PK",
                        "fGradeLvl_K",  "K",
                        "fGradeLvl_1",  "1",
                        "fGradeLvl_2",  "2",
                        "fGradeLvl_3",  "3",
                        "fGradeLvl_4",  "4",
                        "fGradeLvl_5",  "5",
                        "fGradeLvl_6",  "6",
                        "fGradeLvl_7",  "7",
                        "fGradeLvl_8",  "8",
                        "fGradeLvl_9",  "9",
                        "fGradeLvl_10", "10",
                        "fGradeLvl_11", "11",
                        "fGradeLvl_12", "12")),
      list(Group = "Race", Title = "Racial/Ethnic Composition", yLabel = "% of Youth in Group", yscale = "percent",
        Vars = sortVars("bRace_B", "Black",
                        "bRace_H", "Hispanic",
                        "bRace_W", "White",
                        "bRace_M", "Multiracial",
                        "bRace_O", "Other")),
      list(Group = "ISAT Gain", Title = "Raw Gain in ISAT Scores\nSpring to Spring", yLabel = "Raw Gain in Scale Score", yscale = "comma",
        Vars = sortVars("isat_mathss_gain", "Gain - Math",
                        "isat_readss_gain", "Gain - Reading")),
      list(Group = "ISAT PL Math", Title = "Proficiency Levels for ISAT Math", yLabel = "% in Proficiency Category", yscale = "percent",
        Vars = sortVars("isat_mathpl_W", "Warning",
                        "isat_mathpl_B", "Below",
                        "isat_mathpl_M", "Meets",
                        "isat_mathpl_E", "Exceeds")),
      list(Group = "ISAT PL Reading", Title = "Proficiency Levels for ISAT Reading", yLabel = "% in Proficiency Category", yscale = "percent",
        Vars = sortVars("isat_readpl_W", "Warning",
                        "isat_readpl_B", "Below",
                        "isat_readpl_M", "Meets",
                        "isat_readpl_E", "Exceeds")),
      list(Group = "ISAT ME", Title = "Percent of Youth Who Meet or\nExceed ISAT Proficiency Standards", yLabel = "% Meet/Exceed", yscale = "percent",
        Vars = sortVars("isat_mathpl_ME", "Meets/Exceeds",
                        "isat_readpl_ME", "Meets/Exceeds")),
      list(Group = "Attendance", Title = "Rate of School Day Attendance", yLabel = "% Days Attended\n(Excluding Excused Absences)", yscale = "percent",
        Vars = sortVars("Pct_Attend", "Percent Attending")))

#-------------------------------#
#-------------------------------#
### Loop Generation of Graphs ###
#-------------------------------#
#-------------------------------#

if (1==runDescGraphs) {

  # Run all graphs for a given org, site, and program
  runAllGraphTypes <- function(orgname, sitename = "All", progname = "All") {
    mapply(makePlot, varList = varlists, varNames = xnamelists, title = titlelist, ylab = ylablist,
           yscaletype = yscalelist, orgList = orgname, siteList = sitename, progList = progname)
  }
  
  # Run graphs for 
  runGraphsForOrg_bySite <- function(orgname) {
    sites <- unique(descStats$site[descStats$org == orgname])
    sites <- sites[!grepl('Sch-Based Peers', sites)]
    lapply(sites, runAllGraphTypes, orgname = orgname)
  }

  lapply(allOrgs, runGraphsForOrg)
  
}
  
# Talking through how a categorical "compare across" argument would go:
  # values are in {"orgs", "sites", "programs", "year", "benchmarks" }
  # based on selection, there's a check for aux info that is submitted:
  #   - if "orgs": it ignores any site, program and benchmark arguments, and wants an individual year
  #   - if "sites":    it requires a single org, wants an individual year, and ignores program
  #   - if "programs": it requires a single org, wants an individual year, and ignores site
  #   - if "years":    it requires a single org, requires either site or program to be "All", and ignores
  #   - if "benchmarks": it requires a single org, and site or program can be set to "All" or specified, but both can't be specified
  #   - if "vars": ...

# Next steps: 
    # streamline data generation to make one script for demo data and one for non-demo
    # think about what kind of site v site, org v org, and year v year plots should be standard and code those
    # add new variables (NWEA test scores, IEP/ELL status, neighborhood characteristics, etc)

