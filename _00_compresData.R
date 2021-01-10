
#------------------------------------------
# Add it to the selected .rds

nfile <- "datos_csv/202007/200730COVID19MEXICO.csv"

fd <- 
  read_csv(nfile, na=c("9999-99-99"), 
           col_types=cols(PAIS_ORIGEN=col_character())) %>% 
  select(FECHA_ACTUALIZACION,ID_REGISTRO, FECHA_INGRESO, FECHA_SINTOMAS, 
         FECHA_DEF, RESULTADO, ENTIDAD_RES)


covid_m <- readRDS("datos/covid202007.rds")

covid_m <- bind_rows(covid_m, fd)

saveRDS(covid_m, file="", sep="")

#------------------------------------------
# Join 2 rds
covid_m <- readRDS("datos/covid202009_1.rds")
covid_m2 <- readRDS("datos/covid20200920-30.rds")

covid_m <- bind_rows(covid_m, covid_m2)
saveRDS(covid_m, file="datos/covid202009.rds")


#------------------------------------------
# Do it in chunks: 

covid_m <- tibble()


folder_mes <- "20201001-05"
folder <- paste("datos_csv/", folder_mes, sep="")

for(f in list.files(folder,include.dirs = FALSE)) {
  fd <- read_csv(paste(folder, f, sep="/"), na=c("9999-99-99"), col_types=cols(PAIS_ORIGEN=col_character())) %>%
    select(FECHA_ACTUALIZACION,ID_REGISTRO, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, RESULTADO, ENTIDAD_RES)
  covid_m <- bind_rows(covid_m, fd)

}

saveRDS(covid_m, file=paste("datos/covid",folder_mes,".rds", sep=""))
