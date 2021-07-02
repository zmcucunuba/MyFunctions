
rm(list = ls())

library(tidyverse)

nombres_estudiantes <- c("Liliana", "Jose", "Jenny", "Zulma", "Percy", "Carolina")


for (i in nombres_estudiantes) {
  
  if(i == "Percy") {
    print (paste("Econtré a Percy en la posición",  which(nombres == i))) 
    }
  
}










for (i in nombres) {
  if(i == "Liliana") {
    print (paste("Econtré a Liliana en la posición",  which(nombres == i)))
  }
}


for (i in nombres) {
  if(str_detect(i, "a")) {
    print (paste("Econtré una letra a en la posición",  which(nombres == i)))
  }
}





library(rio)
dd <- import("https://covid.ourworldindata.org/data/owid-covid-data.csv", format ="csv")
mis_paises <- c("Chile", "Colombia", "Peru", "Argentina", "Panama" )
dd <- dd %>% filter(location %in% mis_paises)
dd$new_deaths[is.na(dd$new_deaths)] <- 0

for (i in mis_paises) {
  tmp <- dd %>% filter (location == i) 
  dates_of_interest <- as.Date(unique(tmp$date))
  
    for (k in seq_along(dates_of_interest)) {
      if(tmp$new_deaths[tmp$date == dates_of_interest[k]] > 1000) {
        
        print(paste("En", i, "en fecha", dates_of_interest[k], "se reportaron más de 1000 fallecimientos"))
      }
    }
  }
    

  

