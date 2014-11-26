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
system.time(solve(matrix(runif(1000000), nrow = 1000)))

foreach