#---------------------------------------
#
# Sandbox to parallelize code operations
#
#---------------------------------------

# For reference
# http://www.r-bloggers.com/parallel-r-loops-for-windows-and-linux/

#--------------
# Set things up
#--------------

# Set environment
  rm(list = ls())
  # setwd()

# Load Libraries
  library(foreach)
  library(doParallel)
  library(pastecs)
  #library(doMC)

# Set run sizes and functions
  R <- 5000
  n <- 10000
  e <- rnorm(n)
  x <- runif(n)  
  y <- x + e
  df <- data.frame(y, x)

  myInv <- function(nRows) {
    m <- matrix(runif(nRows^2), nrow = nRows)
    return(solve(m))
  }
  myInv(20)

  getBeta <- function(d){
    reg <- lm(y ~ x, d)
    return(coef(reg)[2])
  }
  getBeta(df)
  bootSample <- function(d){
    n <- nrow(d)
    d[sample(1:n, n, replace = T),]
  }
  oneBootRep <- function(d){
    getBeta(bootSample(d))
  }
  oneBootDesc <- function(d){
    library(pastecs)
    stat.desc(bootSample(d))
  }

  
#---------------------
# Time various methods
#---------------------

# Loop
  boots <- NULL
  desc <- NULL # matrix(rep(0, times = ncol(df)*nrow(stat.desc(df))), nrow = nrow(stat.desc(df)))
  system.time({
    for (i in 1:R) {
      bootFor <- rbind(boots, cbind(i, oneBootRep(df)))
    }
  })
  system.time({
    for (i in 1:R) {
      descFor <- rbind(desc, cbind(i, oneBootDesc(df)))
    }
  })

# Apply
  system.time(bootApply <- sapply(1:R, function(x) cbind(x, oneBootRep(df) )))
  system.time(descApply <- sapply(1:R, function(x) cbind(x, oneBootDesc(df))))

# doParallel 
  # in serial
    system.time(bootSerial <- foreach(1:R) %do% oneBootRep(df))
    system.time(descSerial <- foreach(1:R) %do% oneBootDesc(df))

  # from Unix prompt, can get number of processors from unix prompt -- -- cat /proc/cpuinfo | grep ^proc
    #cl <- makeCluster(3)
    #registerDoParallel(cl)

    registerDoParallel(cores = detectCores())
    getDoParName()
    getDoParWorkers()
    system.time(bootParallel <- foreach(1:R, .combine = rbind) %dopar% oneBootRep(df))
    system.time(descParallel <- foreach(1:R, .combine = rbind) %dopar% oneBootDesc(df))
  