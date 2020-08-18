
get_empty_plot <- function() {
  d <- data.frame(x= 1:10, y = 1:10)
  ggplot(d, aes(x, y)) + 
    geom_blank() + ylab('') + xlab ('') + theme_void()
  
}


