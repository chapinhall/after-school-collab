#---------------------------------------
#
# Sandbox to parallelize code operations
#
#---------------------------------------

# For reference
# http://www.r-bloggers.com/parallel-r-loops-for-windows-and-linux/

library(foreach)
library(doParallel)
library(doMC)

myInv <- function(n) {
    m <- matrix(runif(n^2), nrow = n)
    return(solve(m))
}
system.time(for (i in 1:100) myInv(1e4))

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
    cl <- makeCluster(3)
    registerDoParallel(3)
    system.time(x <- foreach(i=1:100) %dopar% sqrt(i))
    system.time(x <- foreach(1:r) %dopar% myInv(n))
  
>>>>>>> 08d99fe850281d64510bad88da2eca1042d829f0
