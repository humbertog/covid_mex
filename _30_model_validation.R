library(R2jags)
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(parallel)


# Load data
source("_00_readData.R")

# Runs the model for dates from fecha_min_val to fecha_max_val
# To run maodel for only one date set fecha_min_val = fecha_max_val

fecha_max_val <- as.Date("2020-05-28")
fecha_min_val <- as.Date("2020-05-12")


#max_lag <- 28
max_lag <- 35

fechas_val <- seq.Date(from=fecha_min_val, to=fecha_max_val, by = "1 day")

fechas_bases <- sort(unique(covid$FECHA_ACTUALIZACION))
N_est <- tibble()
for (ii in 1:length(fechas_val)) {
  maxfecha <- fechas_val[ii]
  fecha_pred <- maxfecha - 3
  
  
  # Load model estimates
  load(paste("mcmc/maxlag", max_lag,"/", maxfecha, "-model1.RData", sep=""))
  load(paste("mcmc/maxlag", max_lag,"/", maxfecha, "-model2.RData", sep=""))
  
  
  # ----------------------------------------------------------------
  # Resultados del modelo
  # ----------------------------------------------------------------
  
  
  # Proportions and number of missing cases
  ps_mod1 <- modelo1$BUGSoutput$mean$p
  NN_mod1 <- modelo1$BUGSoutput$mean$NN
  NN_q25_mod1 <- modelo1$BUGSoutput$summary[paste("NN[", 1:length(NN_mod1), "]", sep=""),"2.5%"]
  NN_q975_mod1 <- modelo1$BUGSoutput$summary[paste("NN[", 1:length(NN_mod1), "]", sep=""),"97.5%"]
  
  ps_mod2 <- modelo2$BUGSoutput$mean$p
  NN_mod2 <- modelo2$BUGSoutput$mean$NN
  NN_q25_mod2 <- modelo2$BUGSoutput$summary[paste("NN[", 1:length(NN_mod2), "]", sep=""),"2.5%"]
  NN_q975_mod2 <- modelo2$BUGSoutput$summary[paste("NN[", 1:length(NN_mod2), "]", sep=""),"97.5%"]
  
  fec <- fechas_bases[fechas_bases <= fecha_pred]
  
  N_est_ii <- bind_rows(
    tibble(fecha_pred=fecha_pred, fecha=fec, model="Model 1", n= NN_mod1, type="missing"),
    tibble(fecha_pred=fecha_pred, fecha=fec, model="Model 2", n= NN_mod2, type="missing "))
  
  
  N_est <- bind_rows(N_est, N_est_ii)
  
}  
  

covid_fecha_corte <-
  covid %>%
  filter(MUERTO == 1, RESULTADO2 == "positivo") %>%
  filter(FECHA_ACTUALIZACION <= maxfecha) %>%
  group_by(FECHA_ACTUALIZACION, RESULTADO2, MUERTO) %>%
  summarise(n=n())  %>%
  group_by() %>%
  select( -MUERTO, -RESULTADO2)
  
  

N_est <- N_est %>% left_join(covid_fecha_corte, by=c("fecha" = "FECHA_ACTUALIZACION"))
  

N_est <- 
  N_est %>%
  mutate(n = n.x + n.y)
  


covid_fecha_def_max <-
  covid_def %>%
  filter(FECHA_ACTUALIZACION %in% as.Date(c("2020-05-13", "2020-05-20", "2020-05-27", "2020-05-28"))) %>%
  group_by(FECHA_ACTUALIZACION, FECHA_DEF) %>%
  summarise(n=n()) %>%
  arrange(FECHA_ACTUALIZACION) %>%
  mutate(cumn = cumsum(n))  %>%
  filter(FECHA_DEF >= "2020-03-15")
  

N_est %>% 
  ggplot() +
  geom_line(aes(fecha, n, group=fecha_pred, colour="estimated"), alpha=.7) +
  #geom_line(aes(FECHA_DEF, cumn, colour="observed"), data=covid_fecha_def_max %>% filter(FECHA_ACTUALIZACION == fecha_max_val), size=1, alpha=.7) +
  geom_line(aes(FECHA_DEF, cumn, group=FECHA_ACTUALIZACION, colour="observed"), data=covid_fecha_def_max) +
  facet_grid(.~model) +
  scale_color_brewer(name = "Type",  labels = c("estimated", "observed"),palette="Set1") +
  theme_bw()+
  scale_y_continuous("cumulative deaths", breaks=seq(0,15000,1000), limits = c(0,15000)) +
  scale_x_date("data base date", breaks = seq.Date(from=as.Date("2020-03-15"), to=as.Date("2020-05-31"), by="2 weeks"), 
               limits=c(as.Date("2020-03-15"), as.Date("2020-05-31")),
               date_labels = "%m-%d") +
  theme(legend.position = c(.2,.8),
        axis.title= element_text(size=13),
        axis.text = element_text(size=12),
        legend.title =  element_text(size = 13),
        legend.text = element_text(size = 12)
  )

ggsave(paste(fecha_max_val, "-validation.png"), width = 180, height = 180 * 2/3, units = "mm")

  
#  
covid_fecha_def_max <-
  covid_def %>%
  filter(FECHA_ACTUALIZACION %in% as.Date(c("2020-05-27"))) %>%
  group_by(FECHA_ACTUALIZACION, FECHA_DEF) %>%
  summarise(n=n()) %>%
  arrange(FECHA_ACTUALIZACION) %>%
  mutate(cumn = cumsum(n))  %>%
  filter(FECHA_DEF >= "2020-03-15")
  
  
