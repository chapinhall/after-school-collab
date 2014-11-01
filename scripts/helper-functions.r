"%&%"  <- function(...) paste(..., sep="")
paste0 <- function(...) paste(..., sep="") # Necessary for less up-to-date versions of R
p0     <- function(...) paste0(...) # an even shorter version of paste0()
comment <- function(...){}
ep      <- function(x){ eval(parse(text = x))}
f.to.c <- function(f){ return(levels(f)[f]) }
f.to.n <- function(f){ return(as.numeric(levels(f)[f])) }
cn <- function(x) colnames(x)
compCols <- function(a, b) {
   cna <- cn(a); cnb <- cn(b)
   u <- unique(c(cna, cnb))
   return(cbind(u, u %in% cna, u %in% cnb))
}