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
# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}