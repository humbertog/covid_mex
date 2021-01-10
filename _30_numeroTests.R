#library(MASS)
library(tidyverse)
library(RColorBrewer)
library(forecast)
library(stringr)


source("_00_readData.R")

maxfecha <- as.Date("2020-09-20")



# -------------------------------------------------------------------
# NUMERO DE CASOS POR FECHA DE CORTE
# -------------------------------------------------------------------

covid_fecha_corte1 <-
  covid %>%
  filter(FECHA_ACTUALIZACION >= "2020-05-01", FECHA_ACTUALIZACION < "2020-08-01") %>%
  filter(RESULTADO2 != "sospechoso") %>%
  group_by(FECHA_ACTUALIZACION, ENTIDAD_RES) %>%
  summarise(n=n())  %>%
  group_by() %>%
  arrange(ENTIDAD_RES) 


covid_fecha_corte2 <-
  covid %>%
  filter(FECHA_ACTUALIZACION >= "2020-08-01") %>%
  filter(RESULTADO2 != "sospechoso") %>%
  group_by(FECHA_ACTUALIZACION, ENTIDAD_RES) %>%
  summarise(n=n())  %>%
  group_by() %>%
  arrange(ENTIDAD_RES) 


covid_fecha_corte <- bind_rows(covid_fecha_corte1, covid_fecha_corte2)

# -------------------------------------------------------------------
# NUMERO DE TESTS DIARIOS
# -------------------------------------------------------------------
tests <-
  covid_fecha_corte %>%
  left_join(covid_fecha_corte %>% 
              mutate(FECHA_ACTUALIZACION = FECHA_ACTUALIZACION + 1),
            by =c("FECHA_ACTUALIZACION" = "FECHA_ACTUALIZACION",
                  "ENTIDAD_RES"= "ENTIDAD_RES")
            )


tests <-
  tests %>%
  mutate(tests_diarios = n.x- n.y) %>%
  arrange(ENTIDAD_RES, FECHA_ACTUALIZACION) %>%
  group_by(ENTIDAD_RES) %>%
  mutate(ma7 = ma(tests_diarios,14)) %>% 
  group_by()



tests_total <- 
  tests %>%
  group_by(FECHA_ACTUALIZACION) %>%
  summarise(tests_diarios = sum(tests_diarios, na.rm=TRUE)) %>%
  mutate(ma7 = ma(tests_diarios,14))


tests_total_nocdmx <- 
  tests %>%
  filter(ENTIDAD_RES !="09") %>%
  group_by(FECHA_ACTUALIZACION) %>%
  summarise(tests_diarios = sum(tests_diarios, na.rm=TRUE)) %>%
  mutate(ma7 = ma(tests_diarios,14))


tests_total %>%
  ggplot() +
  #geom_col(aes(FECHA_ACTUALIZACION, tests_diarios)) +
  geom_line(aes(FECHA_ACTUALIZACION, ma7/119530753 *1000), colour = "blue", size=1) +
  #geom_line(aes(FECHA_ACTUALIZACION, ma7/(119530753-8918653) *1000), colour = "red", size=1, data=tests_total_nocdmx) +
  theme_bw() +
  scale_fill_brewer(palette="Set1")+
  ggtitle("Tests diarios registrados en cada base")


#
poblacion <- read_csv("poblacion_estados.csv") %>%
  mutate(CLAVE = str_pad(CLAVE, 2, pad = "0"))

print(poblacion, n=40)

tests %>%
  left_join(poblacion, by=c("ENTIDAD_RES" = "CLAVE")) %>%
  filter(ENTIDAD_RES %in% c("03","08","09", "10", "11", "15", "16", "19", "22", "32")) %>%
  ggplot() +
  geom_line(aes(FECHA_ACTUALIZACION, ma7/POBLACION * 1000, colour=ENTIDAD_RES)) 
  #geom_line(aes(FECHA_ACTUALIZACION, ma7/119530753 *1000), colour = "blue", size=1, data=tests_total) + 
  #geom_line(aes(FECHA_ACTUALIZACION, ma7/119530753 *1000), colour = "red", size=1, data=tests_total_nocdmx) 


tests %>%
  left_join(poblacion, by=c("ENTIDAD_RES" = "CLAVE")) %>%
  filter(ENTIDAD_RES %in% c("02", "04", "05", "06", "07", "12", "13", "14", "17", "18", "20", "21", "23")) %>%
  ggplot() +
  geom_line(aes(FECHA_ACTUALIZACION, ma7/POBLACION * 1000, colour=ENTIDAD_RES)) 


tests %>%
  left_join(poblacion, by=c("ENTIDAD_RES" = "CLAVE")) %>%
  filter(ENTIDAD_RES %in% c("32")) %>%
  ggplot() +
  geom_line(aes(FECHA_ACTUALIZACION, ma7/POBLACION * 1000, colour=ENTIDAD_RES)) 


tests %>% filter(ENTIDAD_RES == "05") %>% print(n=200)


tests %>%
  left_join(poblacion, by=c("ENTIDAD_RES" = "CLAVE")) %>%
#  filter(ENTIDAD_RES %in% c("02", "26", "25", "30", "27", "21", "09", "15", "14", "05")) %>%
  ggplot() +
  geom_line(aes(FECHA_ACTUALIZACION, ma7/POBLACION * 1000, colour=ENTIDAD_RES))


tests %>%
  filter(ENTIDAD_RES != "09") %>%
  print(n=100)

tests_total_nocdmx %>%
  print(n=100)
