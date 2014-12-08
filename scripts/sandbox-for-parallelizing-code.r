#---------------------------------------
#
# Sandbox to parallelize code operations
#
#---------------------------------------

# For reference
# http://www.r-bloggers.com/parallel-r-loops-for-windows-and-linux/
# http://www.r-bloggers.com/the-wonders-of-foreach/

library(rbenchmark)
library(foreach)
library(doParallel)
library(doMC)

r <- 100 # repetitions of task
n <- 100 # number of rows in matrix

myInv <- function(x) {
    m <- matrix(runif(x^2), nrow = x)
    return(solve(m)[1,])
}

#---
# Time a loop
#---
  system.time({
    for (i in 1:r){
      myInv(n)  
    }
  })

#---
# Time an apply
#---
  system.time(sapply(1:r, function(x) myInv(n)))

#---
# Time the doParallel code
#---

  # First, try the code in serial
    system.time(foreach(1:r) %do% myInv(n))

  # from Unix prompt, can get number of processors from unix prompt -- -- cat /proc/cpuinfo | grep ^proc
    registerDoMC(12)
    system.time(x <- foreach(1:r) %dopar% myInv(n))

