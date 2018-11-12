
dd <- readRDS('dd_example.RDS')

median_binned <- function (dd){

  dd$cum <- NA
  
  for (k in seq_along(dd$age_class)){
    if (k == 1) {dd$cum[k] <- dd$cases[k]} 
    else {
      dd$cum[k] <- dd$cases[k] + dd$cum[k-1]
    }
  }
  
  
  Nt <- sum(dd$cases)# Total Frequency
  median_location <- Nt/2
  vector_values <- rep(dd$age_class, dd$cases)
  median_class  <-  vector_values[median_location]
  median_class_location <- which(dd$age_class== median_class)
  
  # frequency in the class
  Fc <- dd$cases[median_class_location] 
  
  # Frequency below the class
  Fm <- dd$cum[median_class_location-1]
  
  # Boundaries of the class
  Lb <- (dd$age_min[median_class_location] + 
           dd$age_max[median_class_location -1]) / 2 
  Db <- Lb - dd$age_max[median_class_location-1]
  Ub <- dd$age_max [median_class_location] + Db
  Cs <- Ub - Lb# Class Interval Size
  
  median_value <- Lb + 1/2  * (Nt - Fc)/Fm * Cs
  return(median_value)
  
}

Age_infection <- median_binned(dd)
