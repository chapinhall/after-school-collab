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

  #----------------#
  # Specify Colors #
  #----------------# 
    # Manual converter to check values is here: http://rgb.to/

    YMCAfill    <- c("#ED1C24", "#92278F", "#0089D0", "#F47920", "#01A490") # hex codes from YMCA standards (http://marketingfrankenstein.weebly.com/uploads/6/9/2/0/6920095/graphic_standards_for_3rd_parties.pdf)
    ASMfill     <- c("#3F227C", "#A4B635", "#211F21") # purple, lime, and grey - hex codes from ASM website
    CHaAfill    <- c("#4F91CE", "#78A360", "#FAAF5E") #light blue, green, orange - hex codes from CH+A website
    UCANfill    <- c("#006854", "#5C3D87", "#CDDA44", "#5A5A5A") #dark teal, purple, lime, gray 
    Collabfill  <- c("#000053", "#8F162B", "#A0A1A3")
    OrgvOrgfill <- c("#000053", "#8F162B", "#A0A1A3", "#DA5F45", "#005555", "#000000")
    # Chapin Colors: navy, maroon, grey, coral, teal, black
    neutralFill <- c(#3399FF)
  
    Alphafill  <- c("#ED1C24", "#92278F", "#0089D0", "#F47920", "#01A490") 
    Betafill <- c("#3F227C", "#A4B635", "#211F21")
  
    # Convert colors to HSV (hue, saturation, value (i.e. intensity))
      hex2hsv <- function(hex) rgb2hsv(col2rgb(hex))
      # hex2hsv("#000000"); hex2hsv("#FFFFFF")

  # Other old fills we could use
  ##myfill1 <- c("#CC3300", "#660000") #orange/red and dark red ... NSM - these are good YMCA colors
  ##myfill2 <- c("#CC6600", "#480000") #darker orange and maroon
  ##myfill3 <- c("#FF6600", "#480000") #lighter orange and maroon
  ##myfill4 <- c("#CC6600", "#006666") #darker orange and teal
  ##myfill5 <- c("#CC6600", "#003333") #darker orange and darker teal
  ##bluesfill <- c("#005555", "#000077")
  
  myRes <- 600
  myWidth  <- 4.67 # Using inches (for ggsave). This is 2800 in pixels. (600 pixels to an inch)
  myHeight <- 3.50 # Using inches (for ggsave). This is 2100 in pixels. (600 pixels to an inch)
  
#-----------------------
## Load and Prepare Data
#-----------------------

  ## Load data
    if (useScrambledData==1) {
        myOutDir <- p0(myDir, "demos/") 
        scramInd <- "_DEMO"
    } else {
        myOutDir <- p0(myDir, "output/")
        scramInd <- ""
    }
  
    load(p0(dataPath, "descStats", scramInd, ".Rda"))
    #allOrgs <- c("YMCA", "ASM", "CHaA", "UCAN", "Collab")
    allOrgs <- unique(descStats$org)

    # Convert colors for all organizations
      for (o in c(allOrgs, "Collab", "OrgvOrg")){
        assign(p0(o, "fill_hsv"), sapply(get(p0(o, "fill")), hex2hsv))
      }  
    
  ## Small data change - move designation of "all school based peers" to the name of the "org"
    descStats$org[ descStats$site=="All Sch-Based Peers"] <- paste(descStats$org[descStats$site=="All Sch-Based Peers"], "Sch-Based Peers")
    descStats$site[descStats$site=="All Sch-Based Peers"] <- "All"
    descStats$site[descStats$site==""] <- "Unknown"
  
  ## Simplify Site Names
  # XXX Move this to general data prep
    descStats$site <- gsub(" School Of Excellence", "", descStats$site)
    descStats$site <- gsub(" Elementary", "", descStats$site)
    descStats$site <- gsub(" Academy", "", descStats$site)
    gsub("Howe", "", descStats$site)
    
  ## Prepare factor orders to go from "non-org", "sch-based peers", "own value"
    for (v in c("org", "site")){
      vals.u <- unique(descStats[, v])
      nonVals  <- grep("Non-",  vals.u, value = T)
      peerVals <- grep("Peers", vals.u, value = T)
      ownVals  <- vals.u[!(vals.u %in% c(nonVals, peerVals))]
      descStats[, v] <- factor(descStats[, v], levels = c(nonVals, peerVals, ownVals))
    }
  
#---------------------------------------#
#---------------------------------------#
### Graphs for Non-Org, Org, and Site ###
#---------------------------------------#
#---------------------------------------#

##### Define general plotting function  ##########
  
  makePlot <- function(
                orgList = allOrgs, siteList = c("All"), progList = c("All"), yearList = c("All"), gradeList = c("All"), 
                compareAcross = "Benchmarks", varList, varNames = "",
                outName = "", title = "", ylab = "", xlab = "", yscaletype = "percent"
              ){    
              # compareAcross can take values "Benchmarks", "Orgs", "Sites", "Programs", "Years", "Vars".
              # XXX in the future, could create a comparison by grade
              # varNames is a vector of x-axis labels, in the same order as the variables in varList
              # To see raw means rather than percents, set yscaletype = "comma"

    # Print status for auditing purposes
      pc <- function(x) paste(x, collapse = ", ")
      print(paste("Graphing orgList =", pc(orgList), 
                  "for varList ",  pc(varList),
                  "for sites ",    pc(siteList),
                  "for programs ", pc(progList),
                  "for years ",    pc(years),
                  "for grades ",   pc(gradeList)))
      
    # Check consistency of function call based on all args
      
      # XXX Could convert this to a switch() to be a little more elegant. However, would still need to handle exceptions.
      if (compareAcross == "Years"){
        stopifnot(length(yearList) > 1,
                  all(c(length(orgList), length(siteList), length(progList)) == 1),
                  any(c(siteList, progList) == "All")) # i.e., we're not allowing both site and program to be specified
        myOrgs <- orgList; mySites <- siteList; myProgs <- progList
        
      } else if (compareAcross == "Vars"){
        stopifnot(length(varList) > 1,
                  all(c(length(orgList), length(siteList), length(progList)) == 1),
                  any(c(siteList, progList) == "All") # i.e., we're not allowing both site and program to be specified
        myOrgs <- orgList; mySites <- siteList; myProgs <- progList
        
      } else if (compareAcross == "Orgs") {
        stopifnot(length(orgList) > 1,
                  all(c(length(varList), length(yearList)) == 1))
        myOrgs <- orgList; mySites <- "All"; myProgs <- "All"
        
      } else if (compareAcross == "Sites"){
        stopifnot(length(orgList) == 1, length(siteList) > 1)
        myOrgs <- orgList; mySites <- siteList; myProgs <- "All"
        
      } else if (compareAcross == "Programs") {
        myOrgs <- orgList; mySites <- "All"; myProgs <- progList
        
      } else if (compareAcross == "Benchmarks"){
        stopifnot( all(c(length(varList), length(yearList))==1) )
        if (length(orgList) == 1 & all(siteList == "All") & all(programList == "All")){
          stopifnot(orgList != "All")
          mySites <- "All"; myProgs <- "All" # Enforcing that this must by site- and program-wide
          myOrgs <- grep(paste(orgList, collapse="|"), descStats$org, value = T)
            # Need this grep to get all of the benchmark values (i.e. non- and peer-) related to the site
          
        } else if (length(siteList) == 1){
          stopifnot(length(orgList) == 1, orgList != "All", siteList != "All")
          myOrgs <- orgList; myProgs <- "All" # Enforcing (for now) that site must be across all programs (since we're not currently cross-classifying)
          mySites <- grep(paste(siteList, collapse="|"), descStats$site[descStats$org %in% orgList], value = T)
            # Need this grep to get all of the benchmark values (i.e. non- and peer-) related to the site
          
        } else if (length(progList) == 1){
          stopifnot(length(orgList) == 1, orgList != "All", progList != "All")
          myOrgs <- orgList; mySites <- "All" # Enforcing (for now) that site must be across all sites (since we're not currently cross-classifying)
          myProgs <- grep(paste(progList, collapse="|"), descStats$program[descStats$org %in% orgList], value = T)
            # Need this grep to get all of the benchmark values (i.e. non- and peer-) related to the program
          
        } else {
          stop("Could not identify the benchmark comparison that was requested")
        }
      } else {
        stop("Count not identify the comparison that was requested")
      }
      
    # Subset to specified orgs, sites, programs, gradeList, years, and variables
      myData <- descStats[with(descStats, org      %in% myOrgs   &
                                          site     %in% mySites  &
                                          program  %in% myProgs  &
                                          year     %in% yearList &
                                          variable %in% varList  &
                                          grade    %in% gradeList), ]
  
      myData <- myData[!is.na(myData$mean), ] # Remove obs that are NA for mean
      if (nrow(myData)==0) stop("Had no rows of data in the specified request")

    # Convert year to numeric for plots over time
      myData$year <- as.numeric(myData$year)
    # Resets factor levels to only the specified sites and programs
      myData$site    <- factor(as.character(myData$site))       
      myData$program <- factor(as.character(myData$program))
      
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
      } else if (compareAcross == "Benchmarks" & ) {
        
      }
      # compareAcross can take values "Benchmarks", "Years", "Vars".
    
    # Define the fill to match organization
      if (compareAcross == "Orgs" | length(orgList) > 1 ){
        myPalette <- OrgvOrgfill
      } else {
        myPalette <- get(p0(myOrgs, "fill"))
      }
    
      nColors <- length(unique(myData$fill))
      if (nColors <= length(myPalette)) {
        useFill <- myPalette
      } else { 
        # Create a sequence of colors between the first two colors specified
        myHsvs <- rgb2hsv(col2rgb(myPalette[1:2]))
        useFill <- hsv(seq(myHsvs["h", 1], myHsvs["h", 2], length.out = nColors),
                       seq(myHsvs["s", 1], myHsvs["s", 2], length.out = nColors),
                       seq(myHsvs["v", 1], myHsvs["v", 2], length.out = nColors))
      }
           
    # Create plot
    
      if (compareAcross != "Years") {
        myPlot <- ggplot(data = myData, aes(x = variable, y = mean, fill = fill)) + 
                    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
                    ggtitle(title) + 
                    scale_y_continuous(labels = ep(yscaletype), name = ylab, breaks = waiver()) +
                    scale_x_discrete(name = xlab, labels = varNames) +
                    guides(fill = guide_legend(title = NULL)) + theme(legend.position = "bottom") +
                    scale_fill_manual(values = useFill) +
                    theme(axis.title.y = element_text(size = 8))
      } else { # Generate plots comparing multiple years
        myPlot <- ggplot(data = myData, aes(x = year, y = mean, color = fill)) +
                    geom_point() + geom_line() +
                    ggtitle(title) +
                    scale_y_continuous(labels = ep(yscaletype), name = ylab, breaks = waiver()) +
                    scale_x_continuous(name = xlab) +
                    guides(color = guide_legend(title = NULL)) + theme(legend.position = "bottom") +
                    scale_color_manual(values = useFill) +
                    theme(axis.title.y = element_text(size = 8))
      }
    
    #---------------#
    # Save the plot #
    #---------------#
      
      yearTag <- ifelse(compareAcross == "Years", "", p0("_year", yearList)) # yearList should be "All" or an individual value
    
      # Where the graph was at the single organizational aggregate level
      if ((compareAcross %in% c("Sites", "Programs")) | 
          (compareAcross %in% c("Years", "Vars", "Benchmarks") & all(c(siteList, programList) == "All"))){
        if (myOrgs == "All" | length(orgList) != 1) stop("In process of saving, the specified organization was not consistent with the saving rules (indv org level)")
        saveFileName <- p0("Across", compareAcross, "_for_", myOrgs, "_Aggregate", yearTag, ".png")
        myGraphOut <- p0(myOutDir, myOrgs, "/")
        
      # Where the graph was at the org-by-site level
      } else if (mySites != "All" & length(mySites == 1))) {
        if any(myOrgs != "All", myPrograms != "All") stop("In process of saving, the specified organization was not consistent with the saving rules (site level)")
        saveFileName <- p0("Across", compareAcross, "_", outName, "_for_", mySites, yearTag, ".png")
        myGraphOut <- p0(myOutDir, myOrgs, "/Sites/", mySites, "/")
      
      # Where the graph was at the org-by-program level
      } else if (myPrograms != "All") & length(myPrograms) == 1) {
        if any(myOrgs != "All", mySites != "All") stop("In process of saving, the specified organization was not consistent with the saving rules (program level)")
        saveFileName <- p0("Across", compareAcross, "_", outName, "_for_", myPrograms, yearTag, ".png")
        myGraphOut <- p0(myOutDir, myOrgs, "/Programs/", myPrograms, "/")
        
      # Where the graph was at the cross-org level
      } else if (compareAcross == "Orgs"){
        if (length(orgList) == 1) stop("In process of saving, the specified organization was not consistent with the saving rules (cross org)")
        saveFileName <- p0("AcrossOrgs_", outName, yearTag, ".png")
        myGraphOut <- myOutDir
      } else {
        stop("Encountered unknown condition for saving the plot")
      }
      dir.create(myGraphOut, showWarnings = FALSE)
      ggsave(filename = p0(myGraphOut, saveFileName), plot = myPlot, dpi = myRes, width = myWidth, height = myHeight)        
    
      # Return plot - this displays the plot once the function has run
      return(myPlot)

  } # End of the makePlot() function


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
  
