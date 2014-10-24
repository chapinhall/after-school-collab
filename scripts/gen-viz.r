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
  myDir <- "H:/Integrated Evaluation Project for YSS Providers/"
  setwd(myDir)
  dataPath <- "./data/constructed-data/" # File path to locate and save data

  useScrambledData <- 0
  runDescGraphs    <- 1

  library(ggplot2)
  library(scales)
  library(reshape)
  comment <- function(...){}
  "%&%"   <- function(...){ paste(..., sep="") }
  paste0  <- function(...){ paste(..., sep="") }
  p0      <- function(...){ paste0(...) }
  plus    <- function(...){ paste(..., collapse = "+") }
  cn      <- function(x){ colnames(x) }
  ep      <- function(x){ eval(parse(text = x))}
  f.to.c  <- function(f){ return(levels(f)[f]) }
  f.to.n  <- function(f){ return(as.numeric(levels(f)[f])) }

  allOrgs <- c("YMCA", "ASM", "CHaA", "UCAN", "Collab")
  
  YMCAfill    <- c("#ED1C24", "#92278F", "#0089D0", "#F47920", "01A490") # hex codes from YMCA standards (http://marketingfrankenstein.weebly.com/uploads/6/9/2/0/6920095/graphic_standards_for_3rd_parties.pdf)
  ASMfill     <- c("#3F227C", "#A4B635", "#211F21") # purple, lime, and grey - hex codes from ASM website
  CHaAfill    <- c("#4F91CE", "#78A360", "#FAAF5E") #light blue, green, orange - hex codes from CH+A website
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
  myWidth  <- 4.67 #Using inches (for ggsave). This is 2800 in pixels.
  myHeight <- 3.50 #Using inches (for ggsave). This is 2100 in pixels.
  
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
    
  ## Small data change - move org level school based peers indicator to Org variable    
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
                VarList, xnames = "",               # xnames is vector of x-axis labels, in the same order as the variables in varlist
                orgnames = allOrgs,    orgcomp = 1,
                sitenames = c("All"), sitecomp = 1,
                prognames = c("All"), grades = c("All"), years = c('All'),
                title = '', ylab = '', xlab = '',
                yscaletype = "percent"){            # to see raw means rather than percents, set yscaletype = "comma"

    # Print status for auditing purposes
      print(paste("Graphing", orgnames, VarList, "for sites", sitenames, "for programs", prognames,"for years", years, "for grades", grades))
    
    # Restrict dataset based on specifications for the call
      orglist  <- NULL
      sitelist <- NULL
    
      if (prognames[1] != 'All' | sitenames[1] != 'All') { orgcomp = 0 }
  
      # If comparing at the level of the organization
      if (orgcomp==1) {
        for (o in orgnames) { # Add all "non-" and "sch-based peer" calculations
          orglist <- c(orglist, unique(grep(o, descStats$org, value=T)))      # Include both Org and non-Org obs
          # XXX I believe we could get the same effect from orglist <- grep(paste(orgnames, collapse="|"), descStats$org, value = T)
        }
      } else {
        orglist <- orgnames
      }
      
      # If comparing at the level of the site
      if (sitecomp==1) {
        for (s in sitenames) { # Add all "non-" and "sch-based peer" calculations
          sitelist <- c(sitelist, unique(grep(s, descStats$site, value=T)))  # Include school based peers
          # XXX See above for grep() alternative
        }  
      } else {
        sitelist <- sitenames
      }
  
      # Handle number of years represented. If "All" is submitted, expand it to all years in the data.
      if (years[1] == 'All') { useYr <- unique(descStats$year) } else { useYr <- years }
      
      # Subset to specified orgs, sites, programs, grades, years, and variables
      data <- descStats[with(descStats, org %in% orglist &
                                        site %in% sitelist &
                                        program %in% prognames &
                                        grade %in% grades &
                                        year %in% useYr &
                                        variable %in% VarList), ]
  
      data <- data[!is.na(data$mean), ] # Remove obs that are NA for mean
      data$site <- factor(as.character(data$site)) # Resets the levels to only the specified sites
    
    if (nrow(data)==0) {
      print("No data")
    } else {
    
    # Convert year to numeric for plots over time
      data$year <- as.numeric(data$year)
    
    # Make sure that graph will display variables from L to R in the order specified in VarList
      data$variable <- factor(data$variable, levels = VarList) 
    
    # Define the fill value as a part of the data frame (in the ggplot statement, aes.fill can only equal a variable in the df)
      # XXX If we took the approach of creating a categorical "compare across" argument, we could make this a switch()
      if (sitenames[1] != "All") { 
        data$fill <- data$site 
      } else if (prognames[1] != 'All') { 
        data$fill <- data$program
      } else {
        data$fill <- data$org
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
        # XXX Need to replace this with less vivid colors
      }
           
    # Create plot
    
      if (is.na(years[2])) { # I.e. generate plots across single year
        plot <- ggplot(data=data, aes(x=variable, y=mean, fill = fill)) + 
                  geom_bar(stat = 'identity', position = 'dodge', width = 0.7) +
                  ggtitle(title) + 
                  scale_y_continuous(labels = eval(parse(text = yscaletype)), name = ylab, breaks = waiver()) +
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
                scale_y_continuous(lables = eval(parse(text = yscaletype)), name = ylab, breaks = waiver()) +
                scale_x_continuous(name = xlab) +
                guides(color = guide_legend(title = NULL)) + theme(legend.position = 'bottom') +
                theme(axis.title.y = element_text(size = 8))
        timeind <- "OverTime"  
      }
            
    # Output plot to saved file
    
      # XXX Come back and rearrange this. There should be a way to more simply assign prefix (e.g. "CpsvOrg_" or "SitevSite_")
      #     and the graph location in the conditionals, and do the dir.create() and ggsave() outside the conditionals.
      # XXX Should be a more elegant way to determine the conditional. Consider creating a categorical argument, e.g.
      #     compareLvl = "x", where x \in {org, site, program, year}, and a check is made to make sure that 
      #     the levels besides the compareLvl are properly specified. E.g., if compareLvl = site, then a single org must be
      #     specified, and program must be "All" (since we currently do not calculate cross-classifications of program-by-site
      #     enrollments), and year is a single value that is not "All".
      if (is.na(orgnames[2])) { # I.e. Output for graphs within one organization
          if (is.na(sitenames[2]) & is.na(prognames[2])) { # I.e. if it's for all sites and all programs
            myGraphOut <- p0(myOutDir, orgnames[1], "/", sitenames[1], "/")
            dir.create(myGraphOut, showWarnings = FALSE)
            ggsave(filename = p0(myGraphOut, "CpsvOrg_", VarList[1], timeind, ".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight)
          } else if (is.na(sitenames[2])){ # I.e. if it's across sites
            myGraphOut <- p0(myOutDir, orgnames[1], "/SitevSite/")
            dir.create(myGraphOut, showWarnings = FALSE)
            ggsave(filename = p0(myGraphOut, "SitevSite_",VarList[1],timeind,".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight)           
          } else if (is.na(prognames[2])){
            myGraphOut <- p0(myOutDir,orgnames[1],"/ProgramvProgram/")
            dir.create(myGraphOut, showWarnings = FALSE)
            ggsave(filename = p0(myGraphOut, "ProgramvProgram_", VarList[1], timeind, ".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight) 
          }
      } else { # Output for graphs comparing organizations
        myGraphOut <- p0(myOutDir, "OrgvOrg/")
        dir.create(myGraphOut, showWarnings = FALSE)
        ggsave(filename = p0(myGraphOut, VarList[1], timeind, ".png"), plot = plot, dpi = myRes, width = myWidth, height = myHeight) 
      }
                  
      # Return plot - this displays the plot once the function has run
        return(plot)
    
      } # End of If checking for whether there is data to plot
  } # End of the MakePlot() function

  # Samples of function in action - this code can be removed eventually
  makePlot("bLunch_FR", orgnames = "CHaA", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = '2013')  
  makePlot(c("isat_mathpl_ME", "isat_readpl_ME"), orgnames = "CHaA", title = "ISAT Proficiency - Meets Exceeds", ylab = '% Meets/Exceeds', years = '2013', xnames = c("Math", "Reading"))
  makePlot(c("Tract_ViolentCrimes_PerHundr"), orgnames = "CHaA", title = "Neighborhood Violence", ylab = 'Violent Crimes Per 100 Residents', years = '2013', yscaletype = "comma")
  makePlot(c("Tract_ViolentCrimes_PerHundr", "Tract_Pct_LtHsEd"), orgnames = "CHaA", title = "Neighborhood Characteritics", ylab = '', years = '2013', yscaletype = "comma", xnames = c("Violent Crimes\nper 100", "Adults < HS Ed"))
  makePlot(c("bRace_B", "bRace_H"), orgnames = "CHaA", title = "Youth Race/Ethnicity", ylab = '% of Youth', years = '2013', xnames = c("African American", "Hispanic"))

  CHaAsites <- unique(as.character(descStats$site[descStats$org == "CHaA"]))
  CHaAsites <- CHaAsites[!grepl("All|Peers", CHaAsites)]
  makePlot("bLunch_FR", orgnames = "CHaA", sitenames = CHaAsites, sitecomp = 0, title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = '2013')
  makePlot("isat_mathpl_ME", orgnames = "CHaA", sitenames = CHaAsites, sitecomp = 0, title = "ISAT Math Proficiency - Meets Exceeds", ylab = '% Meets/Exceeds', years = '2013')
  makePlot("Tract_ViolentCrimes_PerHundr", orgnames = "CHaA", sitenames = CHaAsites, sitecomp = 0, title = "Neighborhood Violence", ylab = 'Violent Crimes Per 100 Residents', years = '2013', yscaletype = "comma")
  makePlot("bRace_B", orgnames = "CHaA", sitenames = CHaAsites, sitecomp = 0, title = "Youth Race - African American", ylab = '% African American Youth', years = '2013')
  makePlot("bRace_H", orgnames = "CHaA", sitenames = CHaAsites, sitecomp = 0, title = "Youth Race - Hispanic", ylab = '% Hispanic Youth', years = '2013')  
  
  # Categorical Plot - same four options
  makePlot(title = 'Math Test Scores', VarList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"), orgnames="CHaA")
  makePlot(title = 'Math Test Scores', orgcomp = 0, VarList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"), orgnames=c("YMCA", "CHaA"))
  makePlot(title = 'Math Test Scores', orgnames = 'Collab', VarList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"))
  makePlot(title = 'Math Test Scores', VarList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"), orgnames=c("ASM"), prognames = "Gallery")
  makePlot(title = 'Math Test Scores', VarList = c("isat_mathpl_W","isat_mathpl_B","isat_mathpl_M","isat_mathpl_E"), xnames = c("Warning","Below","Meets","Exceeds"), orgnames=c("YMCA"), sitenames = c("South Side", "Lake View", "Irving Park"), sitecomp = 0)

  # Plotting Over Time
  makePlot("bLunch_FR", orgnames = "CHaA", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = c("2012", "2013"))
  makePlot("bLunch_FR", orgnames = c("CHaA", "YMCA", "ASM", "Collab"), orgcomp = 0, title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = c("2012", "2013"))
  makePlot("bLunch_FR", orgnames = "Collab", title = "% Free/Reduced Price Lunch", ylab = 'Proportion on Free/Reduced Price Lunch', years = c("2012", "2013"))

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

  # Specify the list of graphs to be run
  graphsList <-
    c(list(Group = "Lunch", Title = "Youth Receiving Free/Reduced Price Lunch", yLabel = "% Receiving Free/Reduced Price Lunch", yscale = "percent",
        Vars = sortVars("bLunch_FR", "Free/Reduced Lunch")),
      list(Group = "Grade", Title = "Distribution of Grade Levels", yLabel = "% in Each Grade", yscale = "percent",
        Vars = sortVars("fGradeLvl_PK", "PK",
                        "fGradeLvl_K", "K",
                        "fGradeLvl_1", "1",
                        "fGradeLvl_2", "2",
                        "fGradeLvl_3", "3",
                        "fGradeLvl_4", "4",
                        "fGradeLvl_5", "5",
                        "fGradeLvl_6", "6",
                        "fGradeLvl_7", "7",
                        "fGradeLvl_8", "8",
                        "fGradeLvl_9", "9",
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
    mapply(makePlot, VarList = varlists, xnames = xnamelists, title = titlelist, ylab = ylablist,
           yscaletype = yscalelist, orgnames = orgname, sitenames = sitename, prognames = progname)
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
  # If there's an error, can issue a stop() statement. See https://stat.ethz.ch/R-manual/R-devel/library/base/html/stop.html

# Next steps: 
    # streamline data generation to make one script for demo data and one for non-demo
    # small tweaks to these graphs (see about combining test scores, break things out across grades, etc.)
    # think about what kind of site v site, org v org, and year v year plots should be standard and code those
      # Set up a way to include organization's average value when doing a given program or site
    # add new variables (NWEA test scores, IEP/ELL status, neighborhood characteristics, etc)

