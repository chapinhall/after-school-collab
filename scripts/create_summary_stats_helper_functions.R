#----------------------------------
# # # TABLE OF CONTENTS
#----------------------------------

#1. Generally useful functions
#2. runStats() function which obtains mean, variance, N, se for given data set
#3. getSubset() function, which returns rows satisfying the given variable and value conditions;
#     and peerStats.fn(), which calculates mean, n, and variance based on school means and proportions
#4. 

#----------------------------------
# # # 1. Generally useful functions
#----------------------------------

  ep <- function(x){ eval(parse(text = x))}
  "%&%" <- function(...) { paste(..., sep="")}
  paste0 <- function(...) { paste(..., sep="")}
  p0     <- function(...) { paste(..., sep="")}
  prop.case = function(str) { # Thanks to John Myles White http://www.johnmyleswhite.com/notebook/2009/02/25/text-processing-in-r/
    substr(str, 1, 1) = toupper(substr(str, 1, 1))
    return(str)
  }
  cn <- function(x) colnames(x) # Just creates a short alias for getting column names
  
#-----------------------------------------------------------------------------------------
# # # 2. Establish a function to calculate mean, variance, N and se by arbitrary subgroups
#-----------------------------------------------------------------------------------------

  # byOrgVar=""; byProgramVar=""; bySiteVar=""; byGradeVar=""; byYearVar=""; byOrgVar = "Collab"; byYearVar = "year"
  # XXX Could users just specify variable names that define the "by", rather than filling in each slot?
  #   We actually do want some slots specifically indicated, e.g. different variables that distinguish org.
  #   In practice, what we want to generalize is the extra cut, beyond org/site/prog. That would be grade or dose, or region.
  #   Or do we want more than one custom slice, e.g. org by region by dosage category?
  #   Perhaps we could do multiple unspecified slices, but carry all information around.
  #     E.g., byList <- c("ASM", "Region", "Dose"), which produces 6 columns: Slice#Var, Slice#Val, for # \in {1, 2, 3}
  #     That could be interpretable by the peer calculations, which could subset and fetch based on knowing the variables.
  #     The output tables would have to change, where things wouldn't necessarily be aligned to Org, Site, and Program, but
  #     rather slice 1, slice 2, etc, where the slices are e.g. "Enrollment: YMCA", "Site: Sage", "Dosage: 0-100 hours".
  #     To get those labels, need to map variable names and values into alias. One tricky example is mapping {"bYmca", 1}
  #     into "YMCA" and "Non-YMCA". Could have a post-processing section which addresses this by looking through a subset
  #     of classifications, like whether the slice variable fits within a given list, e.g. organization, 
  #     That process could deliver fields like "Slide#Label", and it's those labels that go into the "Combo" output.
  fnCount <- function(x) sum(!is.na(x))
  runStats <- function(data = calcData, vars = descVars, byOrgVar="", byProgramVar="", bySiteVar="", bySchlVar="", byGradeVar="", byYearVar=""){

    # If we'll be looking at program or site level variation, discard "non-<that site/that prog>" records:
      if(byProgramVar=="" & bySiteVar=="" & bySchlVar=="") {
        useData <- data
      } else if (byOrgVar %in% orglist) { 
        useData <- data[data$org==byOrgVar, ]
      } else { # XXX Can't recall what cases fall here
        useData <- data
      }
      
    # Determine what other by variables are needed, and unduplicate records
      potentialBys <- c(byOrgVar, byProgramVar, bySiteVar, bySchlVar, byGradeVar, byYearVar)
      byVars <- potentialBys[potentialBys!=""]
      keepRows <- !duplicated(useData[, c("sid", byVars)])
      useData <- useData[keepRows,]
    
    # Calculate various summary statistics
      bys <- unique(byVars)
      byVars.str <- paste(bys, collapse = ",")
      useDt <- data.table(useData, key = byVars.str)
      
      # XXX Could simplify with stat.desc as a replacement of the one-off functions
      # XXX Note that the pastecs package has the function stat.desc which calculates a wide range of useful descriptive stats
      dtMeans <- useDt[, lapply(.SD, mean, na.rm=T), by = byVars.str, .SDcols = vars]
      dtS2    <- useDt[, lapply(.SD, var,  na.rm=T), by = byVars.str, .SDcols = vars]
      dtNs    <- useDt[, lapply(.SD, fnCount),       by = byVars.str, .SDcols = vars]
      dtMeans$stat <- "mean"; dtS2$stat <- "var"; dtNs$stat <- "n"
      stack <- rbind(dtMeans, dtS2, dtNs)
          
    # Reshape the data set to have records by byVars, and statistics going across
      longstack <- melt(stack, id=c(byVars, "stat"))
      out <- cast(longstack, as.formula(paste0(paste(c(byVars, "variable"), collapse="+"), "~ stat")))
      out$sd <- sqrt(out$var)
      out$var_mean <- out$var / out$n
      out$se_mean <- sqrt(out$var_mean)

    # Generate uniform output columns
      outBys <- c("org", "site", "program", "schl", "grade", "year")
      for (outCol in outBys){
        colVar <- paste0("by", prop.case(outCol), "Var")
        if (get(colVar) == ""){
          out[, outCol] <- "All"
        } else {
          out[, outCol] <- out[ get(colVar)]
        }
      }
    
    # Keep only final by-vars and calculations
      out <- out[, c(outBys, "variable", "mean", "n", "var", "sd", "var_mean", "se_mean")]
      return(out)
  }

#----------------------------------------------------------------------
# # # 3. Helper Functions for Generating School-Based Peer Calculations
#----------------------------------------------------------------------

  #---------
  # Create a function to return the appropriate observations for calculating school peer proportions
  # (i.e. returning all records when "All" is specified, even though the value "All" does not appear)
  #---------
  # Because grade and org need special handling, can't use conditions (e.g. "grepl(org, calcData$Org)" where "*" can be passed to org)

  getSubset <- function(subvar, subval){
    if (subval == "All"){
      return(rep(TRUE, nrow(calcData))) # XXX Basically, an inelegant way to return a right-sized vector of "TRUE" values
    } else if(subvar == "grade") {
      
      # Separate handling for grade ranges versus individual grade levels
      if (subval %in% c("PK-5", "Gr6-8", "HS")){
        return(calcData$fGradeGrp_K5_68_HS == subval)
      } else {
        return(calcData$fGradeLvl == subval)
      }
    } else if(subvar == "org") {
      
      #Separate handling for full collab versus individual orgs
      if (subval=="Collab") {
        return(calcData$Collab == subval)
      } else {
        return(calcData$org == subval)
      }
    } else {
      return(calcData[, subvar, with=F] == subval)
        # Identify rows with specifically the right values. Note that the "with=F" argument is syntax
        # required because calcData is a data.table. See e.g. http://stackoverflow.com/questions/13383840/select-multiple-columns-in-data-table-r
    }
  }

  #----------------------------------------------------
  # Generate statistics using weighted mean calculation
  #----------------------------------------------------

    # This uses the weighted.mean function to apply the weights and sum. Note that while the means and N calculations are straight 
    #   weighted means, the variance calculation requires more special treatment.
    # If a weighted mean characteristics for schools A and B, with weights c and d, is calculated as mu = c*\bar{x_A} + d*\bar{x_B}
    #    then the variance is 
    #         var(c*\bar{x_A} + d*\bar{x_B}) = c^2*s2_{\bar{X_A}} + d^2*s2_{\bar{X_A}}
    #    where the s2_{\bar{X_@}} is the variance of the mean statistics (in contrast to the variance of the X's). This is
    #    the variance that was calculated in the runStats() function above.

  peerStats.fn <- function(myProps, myVar, mySchStats){
    
    stats.props <- merge(mySchStats, myProps, by = "schlid")  # merge proportions into school-level stats data
    stats.props <- stats.props[!is.na(stats.props$mean), ] # remove school rows with missing calculations - XXX Could be refactored to be faster if dropping these all ahread of time
    stats.props$prop <- stats.props$prop / sum(stats.props$prop) # inflate the proportions to reflect any drops of schools with missing values
    stats.props$prop2 <- stats.props$prop^2 # This weight is necessary for variance calculations, indicated in the method notes
    
    mean <- weighted.mean(stats.props$mean, stats.props$prop, na.rm = T)
    nbr.val  <- weighted.mean(stats.props$n, stats.props$prop, na.rm = T)
    s2 <- sum(stats.props$var_mean * stats.props$prop2)
    
    out <- data.frame(variable = myVar, mean = mean, nbr.val = nbr.val, SE.mean = sqrt(s2))
    return(out)
  
  }
