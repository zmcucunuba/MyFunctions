
#-------------------------------------------------------------
#              Recovering a linelist from a grouped dataset
#----------------------------------------------------------------

# NOTE: This procidure is very slow, try first with a few rows only. 

test <- data.set(sex = sa )

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




