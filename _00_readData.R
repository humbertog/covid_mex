



covid <- tibble()

for(f in list.files("datos",include.dirs = FALSE)) {
  fd <- read_csv(paste("datos", f, sep="/"), na=c("9999-99-99"), col_types=cols(PAIS_ORIGEN=col_character())) %>% 
    select(FECHA_ACTUALIZACION,ID_REGISTRO, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, RESULTADO)
  covid <- bind_rows(covid, fd)
  
}



# PREprocessing
covid$MUERTO <- 0
covid$MUERTO[!is.na(covid$FECHA_DEF)] <- 1
covid$RESULTADO2  <- "positivo"
covid$RESULTADO2[covid$RESULTADO == 2] <- "negativo"
covid$RESULTADO2[covid$RESULTADO == 3] <- "sospechoso"