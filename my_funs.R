


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


#Taking out observations from a dataset

'%out%' <- function(x,y)!('%in%'(x,y))


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


recover_line_list <- function(test){
  # test: a dataset with some columns : example (date_not, admin1, admin2, city, sex, n_cases)
  
  line_list <- data.frame()
  for (i in 1:length(test$admin1)){
    
    d <- test[i,]
    n <- d$n_cases
    
    new_data <- do.call("rbind", replicate(n, d, simplify = FALSE))
    line_list <- rbind(line_list, new_data)
  }
  
  
  return(line_list)
}


get_empty_plot <- function() {
  d <- data.frame(x= 1:10, y = 1:10)
  ggplot(d, aes(x, y)) + 
    geom_blank() + ylab('') + xlab ('') + theme_void()
  
}

fMovileAverage <- function(d, value,days) {
  
  d$movil_average <- NA
  d$value <- value
  days_average <- days-1
  d <- as.data.frame(d)
  
  for (i in rev(days:NROW(d$date))) {
    values_days <- slice (d, (i-days_average):i)
    average_deaths_days <- sum(values_days$value)/days
    d$movil_average[i] <- (average_deaths_days)
  }
  
  for (j in 1:(days-1)) {
    values_days <- slice (d, j:(j+1))
    average_deaths_days <- sum(values_days$value)/(days-1)
    d$movil_average[j] <- (average_deaths_days)
  }
  
  # d$movil_average[1:(days-1)] <- sum(d$value[1:(days-1)]) / (days-1)
  
  return(d$movil_average)
}



get_age_class <- function( vector_age) {
  
  dat0 <- data.frame(age = vector_age, age_class = NA)

  dat0$age_class[dat0$age < 20] <- "<20"
  dat0$age_class[dat0$age >=20 & dat0$age  <= 29] <- "20-29"
  dat0$age_class[dat0$age >=30 & dat0$age  <= 39] <- "30-39"
  dat0$age_class[dat0$age >=40 & dat0$age  <= 49] <- "40-49"
  dat0$age_class[dat0$age >=50 & dat0$age  <= 59] <- "50-59"
  dat0$age_class[dat0$age >=60 & dat0$age  <= 69] <- "60-69"
  dat0$age_class[dat0$age >=70 & dat0$age  <= 79] <- "70-79"
  dat0$age_class[dat0$age >=80] <- "80+"
  
  return(dat0$age_class)
  
}


