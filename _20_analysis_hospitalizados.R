library(R2jags)
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(vroom)
library(cowplot)

source("_00_initialization.R")


# Obtains the lags
covid_def_lag <- 
  covid_def %>% 
  filter(ENTIDAD_RES == "09") %>%
  filter(!is.na(FECHA_DEF)) 



muertes_diarias <- 
  covid_def_lag %>%
  group_by(FECHA_DEF) %>%
  summarise(n=n())

hosp <- read_csv("datos/personas-hospitalizadas-en-hospitales-de-zmvm.csv")
  
hosp_m <- 
  hosp %>%
  left_join(muertes_diarias, by=c("fecha"="FECHA_DEF")) %>%
  filter(fecha <= "2021-01-01")

hosp_m %>%
  ggplot() +
  geom_line(aes(fecha, hospitalizados_totales_cdmx)) +
  geom_line(aes(fecha, camas_intubados_cdmx)) +
  geom_line(aes(fecha, n*20))


# La proporcion no es constante e incrementa para los periodos de mayor número
# de contagios => las personas llegan en peor estado y/o la capacidad de 
# correcta atencion disminuye (mas enfermos por personal)
hosp_m %>%
  ggplot() +
  geom_line(aes(fecha, n/hospitalizados_totales_cdmx)) +
  geom_line(aes(fecha, n/camas_intubados_cdmx), colour="blue") 


hm <- 
  hosp_m %>%
  select(hospitalizados_totales_cdmx, camas_intubados_cdmx, n) %>%
  drop_na()

val <- ccf(x=as.integer(hm$hospitalizados_totales_cdmx), y=hm$n,  lag.max=5)
val2 <- ccf(x=as.integer(hm$camas_intubados_cdmx), y=hm$n,  lag.max=5)


# Ambulatorios vs hospitalizados

muertes_hosp_amb <- 
  covid_def_lag %>%
  group_by(FECHA_DEF, TIPO_PACIENTE) %>%
  summarise(n=n())

muertes_hosp_amb %>%
  filter(FECHA_DEF <= "2021-01-01") %>%
  ggplot() +
  geom_line(aes(FECHA_DEF, n, colour=as.factor(TIPO_PACIENTE)))


muertes_hosp_amb %>%
  filter(FECHA_DEF <= "2021-01-01") %>%
  filter(TIPO_PACIENTE==1) %>%
  ggplot() +
  geom_line(aes(FECHA_DEF, n))
  

