library(R2jags)
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(parallel)


# Load data
source("_00_readData.R")



# Runs the model for dates from fecha_min_val to fecha_max_val
# To run maodel for only one date set fecha_min_val = fecha_max_val

fecha_max_val <- as.Date("2020-06-10")
fecha_min_val <- as.Date("2020-06-10")



fechas_val <- seq.Date(from=fecha_min_val, to=fecha_max_val, by = "1 day")


for (ii in 1:length(fechas_val)) {
  maxfecha <- fechas_val[ii]
  fecha_pred <- maxfecha - 7


# Load model estimates
load(paste("mcmc_defunciones/", maxfecha, "-model1.RData", sep=""))
load(paste("mcmc_defunciones/", maxfecha, "-model2.RData", sep=""))
load(paste("mcmc_defunciones/", maxfecha, "-model12.RData", sep=""))



covid_fecha_corte <-
  covid %>%
  filter(FECHA_ACTUALIZACION <= maxfecha) %>%
  group_by(FECHA_ACTUALIZACION, RESULTADO2, MUERTO) %>%
  summarise(n=n())  %>%
  group_by()


# Incremento en el número de muertos en días pasados
# La variación es alta, incluso para actualizaciones consecutivas y 
# retrasos de más de dos semanas

# ----------------------------------------------------------------
# Calcula el número de casos nuevos entre una base y otra, tomando como 
# referencia FECHA_BASE
# ----------------------------------------------------------------


covid_def <- 
  covid %>% 
  filter(MUERTO == 1, RESULTADO2 == "positivo") %>%
  select(-FECHA_INGRESO, -FECHA_SINTOMAS, -RESULTADO)




# ----------------------------------------------------------------
# Resultados del modelo
# ----------------------------------------------------------------


# Proportions and number of missing cases
ps_mod2 <- modelo1$BUGSoutput$mean$p
NN_mod2 <- modelo1$BUGSoutput$mean$NN
NN_q25_mod2 <- modelo1$BUGSoutput$summary[paste("NN[", 1:length(NN_mod2), "]", sep=""),"2.5%"]
NN_q975_mod2 <- modelo1$BUGSoutput$summary[paste("NN[", 1:length(NN_mod2), "]", sep=""),"97.5%"]




ps_mod3 <- modelo2$BUGSoutput$mean$p
NN_mod3 <- modelo2$BUGSoutput$mean$NN
NN_q25_mod3 <- modelo2$BUGSoutput$summary[paste("NN[", 1:length(NN_mod3), "]", sep=""),"2.5%"]
NN_q975_mod3 <- modelo2$BUGSoutput$summary[paste("NN[", 1:length(NN_mod3), "]", sep=""),"97.5%"]



as.data.frame(modelo2$BUGSoutput$sims.list$p[sample(3000, 1000),]) %>%
  mutate(sim=1:1000) %>%
  gather(key="key", value="value", -sim) %>%
  arrange(sim) %>%
  mutate(lag=rep(1:length(ps_mod3), 1000)) %>%
  ggplot() +
  geom_point(aes(lag, value, colour=sim), alpha=.1)

# % de casos faltantes en cada actualizacion

plag <- bind_rows(tibble(lag=1:length(ps_mod2), plag = ps_mod2, modelo="Model 1"),
                  tibble(lag=1:length(ps_mod3),plag = ps_mod3, modelo="Model 2"))

plag %>%
  ggplot() +
  geom_line(aes(lag, plag, colour=modelo)) +
  theme_bw() +
  scale_colour_brewer(palette="Set1") +
  scale_y_continuous("% de casos faltantes que se registraran días después", breaks=seq(0,.125,.025), limits = c(0,.15), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous("días",breaks=seq(0,84,7), limits = c(0,84)) +
  theme(legend.position = "right",
        plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10)) +
  ggtitle("Porcentaje de defunciones confirmadas faltantes que se registran en días posteriores", 
          subtitle="Las bases en cada corte tienen un subregistro de casos que serán incluidos en días posteriores\nUn porcentaje del subregistro p1 se agregará 1 día después, p2 2 días después,...")

ggsave(paste("reportes_defunciones/laglambda_",maxfecha,".png", sep=""), width = 200, height = 200 * 2/3, units = "mm")

plag %>%
  group_by(modelo) %>%
  arrange(lag) %>%
  mutate(cump = cumsum(plag)) %>%
  ggplot() +
  theme_bw() +
  geom_line(aes(lag, cump, colour=modelo)) +
  scale_colour_brewer(palette="Set1") +
  scale_y_continuous("% acumulado de casos faltantes que se registraran días después", breaks=seq(0,1,.1), limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous("días",breaks=seq(0,84,7), limits = c(0,84)) +
  theme(legend.position = "right",
        plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10)) +
  ggtitle("Porcentaje aPorcentaje acumulado de confirmadas faltantes que se registran en días posteriores", 
          subtitle="Las bases en cada corte tienen un subregistro de casos que serán incluidos en días posteriores\nUn porcentaje del subregistro p1 se agregará 1 día después, p2 2 días después,...")
  ggtitle(   ggtitle( ggtitle(
ggsave(paste("reportes_defunciones/laglambda_cum_",maxfecha,".png", sep=""), width = 200, height = 200 * 2/3, units = "mm")


# numero de casos faltantes
fb <- sort(unique(covid_def$FECHA_ACTUALIZACION))[1:length(NN_mod2)]
NN <- bind_rows(tibble(FECHA_CORTE=fb, N_missing = NN_mod2, modelo="Model 1"),
                tibble(FECHA_CORTE=fb,N_missing = NN_mod3, modelo="Model 2"))

NN %>%
  ggplot() +
  geom_col(aes(FECHA_CORTE, N_missing, fill=modelo), position="dodge")


# 
covid_muertes_cambio <- bind_rows(tibble(FECHA_ACTUALIZACION = fb, RESULTADO2="positivo", MUERTO=1, n = NN_mod2, tipo="No registradas", modelo="Model 1"),
                                  tibble(FECHA_ACTUALIZACION = fb, RESULTADO2="positivo", MUERTO=1, n = NN_mod3, tipo="No registradas", modelo="Model 2"))
  
  
# Defunciones confirmadas por covid
covid_muertes_estimadas <- 
  bind_rows(covid_fecha_corte %>%
              filter(MUERTO ==1, RESULTADO2 == "positivo") %>%
              mutate(tipo="Registradas", modelo="Model 1"),
            covid_fecha_corte %>%
              filter(MUERTO ==1, RESULTADO2 == "positivo") %>%
              mutate(tipo="Registradas", modelo="Model 2"),
            covid_muertes_cambio
            )
            
  
covid_fecha_def_max <-
  covid_def %>%
  filter(FECHA_ACTUALIZACION == maxfecha) %>%
  group_by(FECHA_DEF) %>%
  summarise(n=n()) %>%
  arrange(FECHA_DEF) %>%
  mutate(cumn = cumsum(n)) 
  

intervalos <-
  covid_muertes_estimadas %>%
  group_by(FECHA_ACTUALIZACION, RESULTADO2, MUERTO, modelo) %>%
  summarise(n=sum(n)) %>%
  group_by() %>%
  arrange(modelo, FECHA_ACTUALIZACION) %>%
  filter(FECHA_ACTUALIZACION <= fecha_pred) %>%
  mutate(lowint=c(NN_mod2 - NN_q25_mod2, NN_mod3-NN_q25_mod3),
         upint =c(NN_q975_mod2 - NN_mod2, NN_q975_mod3-NN_mod3))

covid_muertes_estimadas %>%
  ggplot() +
  geom_col(aes(FECHA_ACTUALIZACION, n, fill=tipo)) +
  geom_line(aes(FECHA_DEF, cumn), data=covid_fecha_def_max %>% filter(FECHA_DEF >= "2020-03-15"), size=1) +
  geom_errorbar(aes(FECHA_ACTUALIZACION, ymin=n-lowint, ymax=n+upint), data=intervalos)+
  theme_bw() +
  facet_grid(.~modelo) +
  scale_y_continuous("número acumulado de defunciones confirmadas", breaks=seq(0,30000,2000), limits = c(0,30000)) +
  scale_x_date("fecha de corte", breaks = seq.Date(from=as.Date("2020-03-15"), to=as.Date("2020-06-30"), by="2 weeks"), 
               limits=c(as.Date("2020-03-15"), as.Date("2020-06-30")),
               date_labels = "%m-%d") +
  scale_fill_brewer(name = "Type", labels = c("estimated", "observed"),palette="Set1") +
  theme(legend.position = c(.2,.8),
        axis.title= element_text(size=12),
        axis.text = element_text(size=11),
        legend.title =  element_text(size = 12),
        legend.text = element_text(size = 11)
  ) +
  ggtitle("Estimación de defunciones confirmadas acumulados aún no registrados en cada fecha de corte", 
          subtitle="La línea negra es el acumulado básandose en la última fecha de corte.\nLa estimación toma como referencia la fecha de defunción de los síntomas.\nLa diferencia entre dos fechas es una aproximación del número de personas que murieron entre esas dos fechas.")

  ggsave(paste("reportes_defunciones/subregistro_",maxfecha,".png", sep=""),  width = 250, height = 250 * 2/3, units = "mm")
  
  
  covid_muertes_estimadas %>%
    filter(modelo == "Model 2") %>%
    filter(FECHA_ACTUALIZACION <= fecha_pred) %>%
    group_by(modelo, FECHA_ACTUALIZACION) %>%
    summarise(n=sum(n)) %>%
    group_by() %>%
    mutate(nprev = lag(n, order_by = FECHA_ACTUALIZACION)) %>%
    mutate(n_cambio = n- nprev) %>%
    ggplot() +
    geom_col(aes(FECHA_ACTUALIZACION, n_cambio, fill="estimados"))  +
    geom_col(aes(FECHA_DEF, n, fill="observados"), data=covid_fecha_def_max) +
    theme_bw() +
    scale_y_continuous("número de casos confirmados", breaks=seq(0,1500,100), limits = c(0,1500)) +
    scale_x_date("fecha de aparición de síntomas", breaks = seq.Date(from=as.Date("2020-03-01"), to=as.Date("2020-06-30"), by="2 weeks"), 
                 limits=c(as.Date("2020-03-01"), as.Date("2020-06-30")),
                 date_labels = "%m-%d") +
    scale_fill_brewer(name = "", labels = c("estimados", "observados"), palette="Set1") +
    ggtitle(paste("Estimación de defunciones confirmadas diarios por fecha de defunción (modelo 2)")) +
    theme(legend.position = c(.2,.8),
          axis.title= element_text(size=11),
          axis.text = element_text(size=11),
          legend.title =  element_text(size = 11),
          legend.text = element_text(size = 11),
          plot.title = element_text(size=13),
          plot.subtitle = element_text(size=10)
    ) 
  
  ggsave(paste("reportes_defunciones/subregistro_cambio_",maxfecha,".png", sep=""),  width = 250, height = 250 * 2/3, units = "mm")


}



# ----------------------------------------------------------------
# Tiempo de duplicación
# ----------------------------------------------------------------
fecha_tendencia <- as.Date("2020-05-07")

covid_muertes_estimadas_change <-
  covid_muertes_estimadas %>%
  group_by(FECHA_ACTUALIZACION, modelo) %>%
  summarise(n=sum(n)) %>%
  group_by() %>%
  filter(modelo=="Model 2") %>%
  filter(FECHA_ACTUALIZACION >= fecha_tendencia, FECHA_ACTUALIZACION <= "2020-05-28") %>%
  mutate(lag= as.numeric(FECHA_ACTUALIZACION -fecha_tendencia ))


covid_muertes_estimadas_obs <- 
  covid_muertes_estimadas %>%
  filter(tipo =="Registradas") %>%
  filter(modelo=="Model 2") %>%
  filter(FECHA_ACTUALIZACION >= fecha_tendencia, FECHA_ACTUALIZACION <= "2020-05-28")  %>%
  mutate(lag= as.numeric(FECHA_ACTUALIZACION -fecha_tendencia ))

covid_fecha_def_max_change <-
  covid_fecha_def_max %>%
  filter(FECHA_DEF >= fecha_tendencia, FECHA_DEF <= "2020-05-28")  %>%
  mutate(lag= as.numeric(FECHA_DEF -fecha_tendencia ))
  

# Tendencias

# Casos estimados
summary(lm(log(n) ~ lag, data=covid_muertes_estimadas_change))

# Totales fecha de corte
summary(lm(log(n) ~ lag, data=covid_muertes_estimadas_obs))

# Ultima base
summary(lm(log(cumn) ~ lag, data=covid_fecha_def_max_change))




log(2) /  0.0428253


log(2) /0.0532089


log(2) / 0.0365131


