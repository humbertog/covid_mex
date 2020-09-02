

covid202004 <- readRDS("datos/covid202004.rds")
covid202005 <- readRDS("datos/covid202005.rds")
covid202006 <- readRDS("datos/covid202006.rds")
covid202007 <- readRDS("datos/covid202007.rds")

covid <- bind_rows(covid202004, covid202005, covid202006, covid202007)

rm("covid202004", "covid202005", "covid202006", "covid202007")

# PREprocessing
covid$MUERTO <- 0
covid$MUERTO[!is.na(covid$FECHA_DEF)] <- 1
covid$RESULTADO2  <- "positivo"
covid$RESULTADO2[covid$RESULTADO == 2] <- "negativo"
covid$RESULTADO2[covid$RESULTADO == 3] <- "sospechoso"