########################################################################
# Crea bases de datos diferenciales de defunciones. Es decir, un solo registro 
# por defunción. El objetivo es evitar cargar en memoria todas las bases 
# para el análisis.
########################################################################


library(tidyverse)
library(vroom)


add_death_incrementalDB <- function(folder, last_db=NULL) {
  if (!is.null(last_db)) {
    covid_m <- readRDS(last_db)
    seen_ids <- unique(covid_m$ID_REGISTRO_RES)
  } else {
    covid_m <- tibble()
    seen_ids <- c()
    
  }
  for(f in list.files(folder,include.dirs = FALSE)) {
    print(f)
    fd <- vroom(paste(folder, f, sep="/"), na=c("9999-99-99"), col_types=cols(PAIS_ORIGEN=col_character(), FECHA_DEF=col_date())) 
    fd <- fd %>% filter(!is.na(FECHA_DEF))
    
    if(!("RESULTADO" %in% colnames(fd))) {
      fd$RESULTADO <- 0
      fd$RESULTADO[fd$CLASIFICACION_FINAL <= 3] <- 1
    } 
    fd$ID_REGISTRO_RES <- paste(fd$ID_REGISTRO, fd$RESULTADO, sep = "-") 

    fd <- fd %>% 
      filter(!is.na(FECHA_DEF)) %>%
      filter(!(ID_REGISTRO_RES %in% seen_ids)) 
    
    seen_ids <- c(seen_ids, fd$ID_REGISTRO_RES)
    covid_m <- bind_rows(covid_m, fd)
    
  }
  saveRDS(covid_m, file=paste("datos/covid_def_incremental_",max(fd$FECHA_ACTUALIZACION),".rds", sep=""))
  return(covid_m)
}


# covid_deaths <- add_death_incrementalDB("datos_csv/202004", last_db = NULL)
# covid_deaths <- add_death_incrementalDB("datos_csv/202005", last_db = "./datos/covid_def_incremental_2020-04-30.rds")
# covid_deaths <- add_death_incrementalDB("datos_csv/202006", last_db = "./datos/covid_def_incremental_2020-05-31.rds")
# covid_deaths <- add_death_incrementalDB("datos_csv/202007", last_db = "./datos/covid_def_incremental_2020-06-30.rds")
# covid_deaths <- add_death_incrementalDB("datos_csv/202008", last_db = "./datos/covid_def_incremental_2020-07-31.rds")
# covid_deaths <- add_death_incrementalDB("datos_csv/202009", last_db = "./datos/covid_def_incremental_2020-08-31.rds")
# 
# 
# covid_deaths <- add_death_incrementalDB("datos_csv/202010", last_db = "./datos/covid_def_incremental_2020-09-30.rds")
# covid_deaths <- add_death_incrementalDB("datos_csv/202011", last_db = "./datos/covid_def_incremental_2020-10-31.rds")
# covid_deaths <- add_death_incrementalDB("datos_csv/202012", last_db = "./datos/covid_def_incremental_2020-11-30.rds")


covid_deaths <- add_death_incrementalDB("datos_csv/new", last_db = "./datos/covid_def_incremental_2021-02-01.rds")






