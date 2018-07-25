


#  Thinning _function

thinning <- function(rawdat,  # dataset with x number of rows (or vector with x number of components)
                     nfinal   # final number of rows (or components) wanted
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
    if (is.null(ncol(rawdat))) { # When it is a vector 
      abc    <- rawdat[cuts[i]]
      output <- rbind(output, abc)
      
    } else{  # When it is a dataset
      abc    <- rawdat[cuts[i], ]
      output <- rbind(output, abc)
    }
  }
  colnames(output) <- ncol(output)
  return(output)
  
}
