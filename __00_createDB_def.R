library(tidyverse)

covid_m <- tibble()
seen_ids <- c()
 
folder_mes <- "202008"
folder <- paste("datos_csv/", folder_mes, sep="")
for(f in list.files(folder,include.dirs = FALSE)) {
  print(f)
  fd <- read_csv(paste(folder, f, sep="/"), na=c("9999-99-99"), 
                 col_types=cols(PAIS_ORIGEN=col_character())) 

  fd$MUERTO <- 0
  fd$MUERTO[!is.na(fd$FECHA_DEF)] <- 1
  
  fd <- fd %>% 
    filter(MUERTO == 1) %>%
    filter(!(ID_REGISTRO %in% seen_ids))
  
  seen_ids <- c(seen_ids, fd$ID_REGISTRO)
  covid_m <- bind_rows(covid_m, fd)

}

#saveRDS(covid_m, file=paste("datos/covid",max(fd$FECHA_ACTUALIZACION),".rds", sep=""))