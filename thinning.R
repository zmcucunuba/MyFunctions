


#  Thinning _function

thinning <- function(rawdat,  # dataset with x number of rows
                     nfinal   # final number of rows wanted
                    )
  
{
  #
  nrows <- NROW(rawdat)
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
