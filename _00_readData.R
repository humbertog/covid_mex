

covid202004 <- readRDS("datos/covid202004.rds")
covid202005 <- readRDS("datos/covid202005.rds")
covid202006 <- readRDS("datos/covid202006.rds")
covid202007 <- readRDS("datos/covid202007.rds")
covid202008 <- readRDS("datos/covid202008.rds")
covid202009 <- readRDS("datos/covid202009.rds")
covid202010 <- readRDS("datos/covid20201001-05.rds")

covid <- bind_rows(covid202004, covid202005, covid202006, covid202007, covid202008, covid202009,covid202010)

rm("covid202004", "covid202005", "covid202006", "covid202007", "covid202008", "covid202009", "covid202010")
gc()

# PREprocessing
covid$MUERTO <- 0
covid$MUERTO[!is.na(covid$FECHA_DEF)] <- 1
covid$RESULTADO2  <- "positivo"
covid$RESULTADO2[covid$RESULTADO == 2] <- "negativo"
covid$RESULTADO2[covid$RESULTADO == 3] <- "sospechoso"