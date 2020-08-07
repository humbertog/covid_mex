
library(R2jags)
library(lubridate)
library(tidyverse)
library(RColorBrewer)


source("_00_readData.R")



fecha_max_val <- as.Date("2020-07-10")
fecha_min_fit <- as.Date("2020-04-12")


covid_def_lag <- 
  covid %>% 
  filter(FECHA_ACTUALIZACION <= fecha_max_val) %>%
  filter(MUERTO == 1, RESULTADO2 == "positivo") %>%
  filter(FECHA_DEF >= fecha_min_fit) %>%
  filter(FECHA_ACTUALIZACION >= fecha_min_fit) %>%
  group_by(ID_REGISTRO, FECHA_DEF) %>%
  summarise(FECHA_REG = min(FECHA_ACTUALIZACION)) %>%
  mutate(lag = as.integer(FECHA_REG - FECHA_DEF)) %>%
  group_by() 


estados <- 
  covid %>%
  distinct(ENTIDAD_RES, ID_REGISTRO)


poblacion <- read_csv("poblacion_estados.csv") %>%
  mutate(CLAVE = str_pad(CLAVE, 2, pad = "0"))

covid_def_lag_edo <-
  covid_def_lag %>%
  left_join(estados, by="ID_REGISTRO") %>%
  left_join(poblacion, by=c("ENTIDAD_RES" = "CLAVE")) 


lag_summaries <-
  covid_def_lag_edo %>%
  group_by(ESTADO) %>%
  summarise(mean_lag=mean(lag), median_lag=median(lag)) %>%
  group_by() 



lag_summaries_nal <-
  covid_def_lag_edo %>%
  summarise(mean_lag=mean(lag), median_lag=median(lag)) 

lag_summaries %>% arrange(median_lag) %>% print(n=100)


covid_def_lag_edo %>%
  ggplot(aes(x=reorder(ESTADO,lag,na.rm = TRUE), y=lag)) +
  geom_boxplot(alpha=.5) +
  geom_point(aes(ESTADO, mean_lag), data=lag_summaries, colour="blue", size=1) +
  theme_bw() +
  theme(legend.position = c(.8,.55),
        axis.title= element_text(size=13),
        axis.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=14, angle=90, hjust=0.95,vjust=0.2),
        legend.title =  element_blank(),
        legend.text = element_text(size = 13),
        plot.title = element_text(size=14),
        plot.subtitle = element_text(size=10)
  ) +
  ylab("número de días de retraso") +
  ggtitle("Retraso en el registro de defunciones por estado", 
          subtitle = "Número de días transcurridos entre las fechas de defunción y de registro.\nLos estados están ordenados por la media del número de días de retraso.")

  ggsave(paste("reportes_def/lag_",fecha_max_val,"_edo.png", sep=""),  width = 180, height = 180 * 2/3, units = "mm")





