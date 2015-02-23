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

systeforeach (i in 1:100){
  myInv
}
