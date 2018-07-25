


#  Thinning _function

thinning <- function(rawdat, nfinal)
  
{
  #
  nrows <- NROW(rawdat)
  #  Warning here!
  byc    <- nrows / nfinal
  cuts   <- seq(1, nrows, by = byc)
  lc     <- length(cuts)
  output <- data.frame()
  for (i in 1:lc)
  {
    if (is.null(ncol(rawdat))) {
      abc    <- rawdat[cuts[i]]
      output <- rbind(output, abc)
      
    } else{
      abc    <- rawdat[cuts[i], ]
      output <- rbind(output, abc)
    }
  }
  colnames(output) <- ncol(output)
  return(output)
  
}
