##
## This code represents a series of attempts to learn the functionality of data.table() and test compare its performance in accuracy and speed with aggregate()
## The current conclusion is that data.table() is substantially (factor of 10 or more) faster than data.frame() methods, with as (or more) simple of syntax
## The next step would be to take the syntax that has been explored here and to fit it to the syntax of the "create_summary_stats.R" code
##

  library(data.table)
  cd <- calcData # The calcData data set is produced in the first half of the "create_summary_stats.R", code

  #------------------------------------------
  # Compare single-variable mean calculations
  #------------------------------------------
    dt <- data.table(calcData)
    system.time({
      setkey(dt, cYMCA)
      mu.dt <- dt[, mean(isat_mathss, na.rm=T), by=cYMCA]
    }) # 0.25 seconds
    system.time(mu.df <- aggregate(cd$isat_mathss, list(cd$cYMCA), mean, na.rm=T)) # 4.65 seconds
    mu.dt; mu.df # Results are the same
  
  #----------------------------------------------
  # Comparing multiple-variable mean calculations
  #----------------------------------------------
    dt <- data.table(calcData)
    system.time({
      setkey(dt, cYMCA)
      mu.dt <- dt[, lapply(.SD, mean, na.rm=T), by=cYMCA, .SDcols=descVars]
    }) #2.23 seconds
    system.time(mu.df <- aggregate(cd[, descVars], list(cd$cYMCA), mean, na.rm=T)) # 24.86 seconds
    cbind(t(mu.dt), t(mu.df)) # Results look the same

  #-----------------------------------
  # Comparing multiple-by calculations
  #-----------------------------------
    dt <- data.table(calcData)
    system.time({
      setkey(dt, cYMCA, site, year)
      mu.dt <- dt[, lapply(.SD, mean, na.rm=T), by="cYMCA,site,year", .SDcols=descVars]
    }) #2.11 seconds
    system.time(mu.df <- aggregate(cd[, descVars], list(cd$cYMCA, cd$site, cd$year), mean, na.rm=T)) # 32.52 seconds
    
    #cbind(t(mu.dt), t(mu.df)) # Results look the same
    colnames(mu.df)[1:3] <- c("cYMCA", "site", "year")
    z <- merge(mu.df, mu.dt, by = c("cYMCA", "site", "year"))
    for(d in descVars){ #c("bRace_NonW")
      print(d)
      print(all(z[, d %&% ".x"] == z[, d %&% ".y"]))
      z[, d %&% "_diff"] <- z[, d %&% ".x"] - z[, d %&% ".y"]
    }  
    View(z[, c("cYMCA", "site", "year", "isat_readpl_W.x", "isat_readpl_W.y", "isat_readpl_W_diff")])
    # ... In comparison, there are some differences, but they are machine rounding error

  #--------------------------------
  # Comparing multiple calculations
  #--------------------------------
    dt <- data.table(calcData)
    system.time({
      setkey(dt, cYMCA)
      mu.dt <- dt[, list(mean(isat_mathss, na.rm=T), var(isat_mathss, na.rm=T)), by=cYMCA]
    }) # 0.25 seconds
    system.time(mu.df <- aggregate(cd$isat_mathss, list(cd$cYMCA), var, na.rm=T)) # 4.65 seconds
    mu.dt; mu.df # Results are the same
  
  
  
  