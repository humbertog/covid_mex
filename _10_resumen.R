#library(MASS)
library(tidyverse)
library(RColorBrewer)

source("_readData.R")

maxfecha <- as.Date("2020-05-19")



# -------------------------------------------------------------------
# NUMERO DE CASOS POR FECHA DE CORTE
# -------------------------------------------------------------------

covid_fecha_corte <-
  covid %>%
  group_by(FECHA_ACTUALIZACION, RESULTADO2, MUERTO) %>%
  summarise(n=n())  %>%
  group_by()



# Totales (positivo + negativo + sospechoso)
covid_fecha_corte %>%
  group_by(FECHA_ACTUALIZACION, RESULTADO2) %>%
  summarise(n=sum(n)) %>%
  ggplot() +
  geom_col(aes(FECHA_ACTUALIZACION, n, fill=RESULTADO2)) +
  theme_bw() +
  scale_fill_brewer(palette="Set1")+
  ggtitle("Número de registros por fecha de actualización de la base")
  
  

# -------------------------------------------------------------------
# DEFUNCIONES X FECHA ACTUALIZACION
# -------------------------------------------------------------------


# Totales (defunciones)
covid_fecha_corte %>%
  filter(MUERTO ==1) %>%
  ggplot() +
  geom_col(aes(FECHA_ACTUALIZACION, n, fill=RESULTADO2)) +
  theme_bw() +
  scale_fill_brewer(palette="Set1")+
  ggtitle("Número de defunciones registradas en cada base por resultado del test")


# Defunciones confirmadas por covid
covid_fecha_corte %>%
  filter(MUERTO ==1, RESULTADO2 == "positivo") %>%
  ggplot() +
  geom_col(aes(FECHA_ACTUALIZACION, n, fill=RESULTADO2)) +
  theme_bw() +
  scale_fill_brewer(palette="Set1")+
  ggtitle("Número de defunciones confirmadas registradas en cada base")


# Defunciones confirmadas por covid (cambio)
covid_fecha_corte %>%
  filter(MUERTO ==1, RESULTADO2 == "positivo") %>%
  mutate(nprev = lag(n, order_by = FECHA_ACTUALIZACION)) %>%
  mutate(n_cambio = n- nprev) %>%
  ggplot() +
  geom_col(aes(FECHA_ACTUALIZACION, n_cambio, fill=RESULTADO2)) +
  theme_bw() +
  scale_fill_brewer(palette="Set1")+
  ggtitle("Defunciones confirmadas diarias registradas en cada base")

# Incremento en ll número de defunciones confirmadas. A mediados de mayo 
# 300 nuevos registros son agregados a la base cada día



# -------------------------------------------------------------------
# NUMERO DE TESTS DIARIOS
# -------------------------------------------------------------------

covid_fecha_corte %>%
  filter(RESULTADO2 != "sospechoso") %>%
  group_by(FECHA_ACTUALIZACION) %>%
  summarise(n=sum(n)) %>%
  mutate(nprev = lag(n, order_by = FECHA_ACTUALIZACION)) %>%
  mutate(n_cambio = n- nprev) %>%
  ggplot(aes(FECHA_ACTUALIZACION, n_cambio)) +
  geom_col() +
  geom_smooth(se=FALSE) +
  theme_bw() +
  scale_fill_brewer(palette="Set1")+
  ggtitle("Tests diarios registrados en cada base")

# Es posible ver un incremento en el número de tests diarios.
# El número de casos sospechosos es > 25,000 y el número de 
# tests diarios es alrededor de 4,500



# -------------------------------------------------------------------
# RESULTADOS DE LA ULTIMA ACTUALIZACION
# -------------------------------------------------------------------

# N de casos por fecha de ingreso a la unidad médica
covid %>%
  filter(FECHA_ACTUALIZACION == maxfecha) %>%
  group_by(FECHA_INGRESO, RESULTADO2) %>%
  summarise(n=n()) %>%
  ggplot() +
  geom_col(aes(FECHA_INGRESO, n, fill=RESULTADO2)) +
  theme_bw() +
  scale_fill_brewer(palette="Set1")+
  xlab("fecha de ingreso a unidad médica") +
  ggtitle("Número de casos positivos, negativos y sospechosos")



# Casos acumulados por tipo
covid %>%
  filter(FECHA_ACTUALIZACION == maxfecha) %>%
  filter(FECHA_INGRESO >= "2020-03-01") %>%
  group_by(FECHA_INGRESO, RESULTADO2) %>%
  summarise(n=n()) %>%
  group_by(RESULTADO2) %>%
  arrange(FECHA_INGRESO) %>%
  mutate(cumn = cumsum(n)) %>%
  ggplot() +
  geom_line(aes(FECHA_INGRESO, cumn)) +
  facet_grid(RESULTADO2~.) +
  xlab("fecha de ingreso a unidad médica") +
  ggtitle("Número de casos positivos, negativos y en espera")


# El número de casos sospechosos aumenta de manera exponencial para
# las últimas fechas, lo que puede significar que se ha llegado 
# al momento de saturación, y los tests son insuficientes


# N de defunciones por fecha de defuncion
covid %>%
  filter(MUERTO ==1 ) %>%
  filter(FECHA_ACTUALIZACION == maxfecha, FECHA_DEF >= "2020-03-01") %>%
  group_by(FECHA_DEF, RESULTADO2) %>%
  summarise(n=n()) %>%
  ggplot() +
  geom_col(aes(FECHA_DEF, n, fill=RESULTADO2)) +
  facet_grid(RESULTADO2~.) +
  theme_bw() +
  scale_fill_brewer(palette="Set1")+
  xlab("fecha de defunción") +
  ggtitle("Número de defunciones por fecha de defuncion")




# El número de muertos con resultado de test negativo se
# comporta de manera más o menos constante, lo cual es de 
# esperarse. Sin embargo, el número de casos de defunciones 
# sospechosos va en aumento. Como el número de muertes con resultado 
# negativo es constante, parecería que la mayoría de los casos sospechosos
# son de hecho positivos. 


# N de defunciones por fecha de defuncion
covid %>%
  filter(MUERTO ==1 ) %>%
  filter(FECHA_ACTUALIZACION == maxfecha, FECHA_DEF >= "2020-03-01") %>%
  group_by(FECHA_DEF, RESULTADO2) %>%
  summarise(n=n()) %>%
  ggplot() +
  geom_col(aes(FECHA_DEF, n, fill=RESULTADO2)) +
  theme_bw() +
  scale_fill_brewer(palette="Set1")+
  xlab("fecha de ingreso a unidad médica") +
  ggtitle("Número de defunciones por fecha de defuncion")


# N de defunciones por fecha de defuncion (acumulado)
covid %>%
  filter(MUERTO ==1 ) %>%
  filter(FECHA_ACTUALIZACION == maxfecha, FECHA_DEF >= "2020-03-01") %>%
  group_by(FECHA_DEF, RESULTADO2) %>%
  summarise(n=n()) %>%
  group_by(RESULTADO2) %>%
  arrange(FECHA_DEF) %>%
  mutate(cumn = cumsum(n)) %>%
  ggplot() +
  geom_line(aes(FECHA_DEF, cumn)) +
  facet_grid(RESULTADO2~.) +
  theme_bw() +
  scale_fill_brewer(palette="Set1")+
  xlab("fecha de defunción") +
  ggtitle("Número acumulado de defunciones por fecha de defuncion")


# -------------------------------------------------------------------
# Numero de muertos por fecha de defuncion segun dia de reporte

covid %>% 
  filter(RESULTADO2 == "positivo", MUERTO==1) %>%
  group_by(FECHA_ACTUALIZACION,FECHA_DEF) %>%
  summarise(n=n()) %>%
  arrange(FECHA_ACTUALIZACION,FECHA_DEF) %>%
  filter(FECHA_DEF >= "2020-03-01") %>%
  mutate(cumn = cumsum(n)) %>%
  ggplot() +
  geom_line(aes(FECHA_DEF, cumn, colour=as.factor(FECHA_ACTUALIZACION))) +
  xlab("fecha de defuncion") +
  ggtitle("Número acumulado de muertos por fecha de defunción, según día de reporte")

# Incremento en el número de muertos en días pasados
# La variación es alta, incluso para actualizaciones consecutivas y 
# retrasos de más de dos semanas
  







  