library(R2jags)
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(vroom)


mod <- "model34"
# Runs the model for dates from fecha_min_val to fecha_max_val
# To run maodel for only one date set fecha_min_val = fecha_max_val
fecha_min_val <- as.Date("2020-12-01")
fecha_max_val <- as.Date("2021-01-14")
fechas_val <- seq.Date(from=fecha_min_val, to=fecha_max_val, by = "1 day")


# Reads data and selects only the confirmed COVID19 cases
covid_def <- readRDS("datos/covid_def_incremental_2021-01-28.rds")
covid_def <- covid_def %>% filter(RESULTADO == 1)

# Remove the IDs that do not appear on the last DB. The extra cases in our DB 
# may be explained by reclassification or errors. We ASSUME that they are 
# reclassification and thus we delete them so we have the same number of deaths 
# reported by the authorities.
last_db <- vroom("datos_csv/202101/20210128.zip", na=c("9999-99-99"), col_types=cols(PAIS_ORIGEN=col_character(), FECHA_DEF=col_date())) 
last_db <- last_db %>% filter(CLASIFICACION_FINAL <= 3, FECHA_DEF >= "2020-03-01") 
covid_def <- covid_def %>% filter(ID_REGISTRO %in% c(last_db$ID_REGISTRO[last_db$CLASIFICACION_FINAL <= 3]))
gc()




# Obtains the lags
covid_def_lag <- 
  covid_def %>% 
  mutate(SEMANA_DEF = as.numeric(cut(FECHA_DEF, seq(as.Date("2020-03-02"), as.Date("2021-12-26"), by="1 week")))) %>%
  mutate(SEMANA_ACTUALIZACION = as.numeric(cut(FECHA_ACTUALIZACION, seq(as.Date("2020-03-02"), as.Date("2021-12-26"), by="1 week")))) %>%
  mutate(lag_dia = as.numeric(FECHA_ACTUALIZACION - FECHA_DEF)) %>%
  mutate(lag_semana = SEMANA_ACTUALIZACION - SEMANA_DEF) %>%
  select(ID_REGISTRO, FECHA_DEF, FECHA_ACTUALIZACION, SEMANA_DEF, SEMANA_ACTUALIZACION, lag_dia, lag_semana) %>%
  filter(!is.na(SEMANA_DEF)) %>%
  arrange(lag_semana)

# Semana 18 corresponde 
sem_min_fit <- 14

covid_def_lag %>% distinct(FECHA_DEF, SEMANA_DEF) %>% arrange(FECHA_DEF) %>% print(n=1000)

semanas_date <- covid_def_lag %>% group_by(SEMANA_DEF) %>% summarise(fecha=min(FECHA_DEF) + 3)

