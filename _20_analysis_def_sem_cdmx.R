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
  mutate(SEMANA_DEF = as.numeric(cut(FECHA_DEF, seq(as.Date("2020-03-02"), as.Date("2021-12-26"), by="1 week")))) %>%
  mutate(SEMANA_ACTUALIZACION = as.numeric(cut(FECHA_ACTUALIZACION, seq(as.Date("2020-03-02"), as.Date("2021-12-26"), by="1 week")))) %>%
  mutate(lag_dia = as.numeric(FECHA_ACTUALIZACION - FECHA_DEF)) %>%
  mutate(lag_semana = SEMANA_ACTUALIZACION - SEMANA_DEF) %>%
  select(ID_REGISTRO, FECHA_DEF, FECHA_ACTUALIZACION, SEMANA_DEF, SEMANA_ACTUALIZACION, lag_dia, lag_semana) %>%
  filter(!is.na(SEMANA_DEF)) %>%
  arrange(lag_semana)


for (ii in 1:length(fechas_val)) {
  maxfecha <- fechas_val[ii]
  wday <- wday(maxfecha, week_start=1)
  # No considera ultima semana, al menos que sea completa
  maxsemana_base <-  unique(covid_def_lag$SEMANA_ACTUALIZACION[covid_def_lag$FECHA_ACTUALIZACION == maxfecha])
  if (wday == 7) {
    maxsemana <- maxsemana_base 
  } else {
    maxsemana <- maxsemana_base - 1
  }
  
  print(paste("Starting", maxfecha, "----------------------------------"))
  # Loac MCMC
  load(paste("mcmc_def/", mod, "_sem_cdmx/",  maxfecha, "-", mod, ".RData", sep=""))
  ps_mod3 <- colMeans(modelo$BUGSoutput$mean$p)
  NN_mod3 <- modelo$BUGSoutput$mean$NN
  NN_q25_mod3 <- modelo$BUGSoutput$summary[paste("NN[", 1:length(NN_mod3), "]", sep=""),"2.5%"]
  NN_q975_mod3 <- modelo$BUGSoutput$summary[paste("NN[", 1:length(NN_mod3), "]", sep=""),"97.5%"]
  

  # Estimated counts
  def_semana_pred <- 
    tibble(SEMANA_DEF = sem_min_fit:(sem_min_fit + length(NN_mod3) - 1) ,
           n_pred=NN_mod3, n_pred_q25=NN_q25_mod3, n_pred_q975=NN_q975_mod3
           )
  
  # Count the number of deaths in each week
  def_semana <- 
    covid_def_lag %>%
    filter(FECHA_ACTUALIZACION <= maxfecha) %>%
    #filter(SEMANA_ACTUALIZACION <= maxsemana_base) %>%
    group_by(SEMANA_DEF) %>%
    summarise(n_tot=n())
  
  # Count the number of late arrivals
  def_semana_lag <- 
    covid_def_lag %>%
    filter(FECHA_ACTUALIZACION <= maxfecha) %>%
    #filter(SEMANA_ACTUALIZACION <= maxsemana_base) %>%
    filter(lag_semana > 0) %>%
    group_by(SEMANA_DEF) %>%
    summarise(n_late=n())
             
  # Joins all
  def_semana_all <- 
    def_semana %>% 
    left_join(def_semana_lag) %>%
    left_join(def_semana_pred) %>%
    mutate(obs = n_tot, faltantes = ifelse(n_pred - n_late > 0, n_pred - n_late, 0)) %>%
    mutate(faltantes=ifelse(is.na(faltantes), 0, faltantes)) %>%
    mutate(faltantes = as.integer(faltantes)) 
  
    
  def_semana_all <- 
    def_semana_all %>%
    left_join(semanas_date)
  
  def_semana_all %>% print(n=100)
  
  # Plot estimated lagged deaths
  def_semana_all %>%
    left_join(semanas_date) %>%
    select(SEMANA_DEF, fecha, obs, faltantes) %>%
    gather(key="tipo", value="n", -SEMANA_DEF, -fecha) %>%
    ggplot() +
    geom_col(aes(fecha, n, fill=tipo), position="stack", width=6.5) +
    geom_errorbar(aes(fecha, ymin= n_tot + n_pred_q25 - n_late, ymax=n_tot + n_pred_q975 - n_late, colour="black"), 
                  data=def_semana_all, size=.1) +
    theme_bw() +
    scale_y_continuous("número de defunciones", breaks=seq(0,1500,100), limits = c(0,1500)) +
    scale_x_date("fecha de defunción", breaks = seq(as.Date("2020-03-01"), as.Date("2021-06-30"), by="2 month"), 
                 limits = c(as.Date("2020-03-01"), as.Date("2021-06-30")),
                 date_labels =  "%m-%Y") +
    #scale_x_date("semana de defunción", breaks = seq(0, 70, 5), limits = c(1,70)) +
    scale_fill_manual(name = "",  values=c("red", "grey10"), labels = c("predicciones", "observados")) +
    scale_color_manual(name = "", values=c("black"), labels = c("int. de probabilidad de 95%")) +
    ggtitle(paste("Número de muertes confirmadas por COVID-19 corregidas por el retraso en su registro (CDMX)", sep=""),
            subtitle = paste("La estimación se hace únicamente con los datos diarios publicados por la SSA, analizando la distribución\ndel número de semanas que transcurren entre el deceso y su registro. El modelo toma en cuenta semanas\ncompletas, por lo que las predicciones para las semanas más recientes no están disponibles.\nActualizado el",as.Date(maxfecha))) +
    theme(legend.position = c(0.2,0.8),
          axis.title= element_text(size=10),
          axis.text = element_text(size=10),
          #legend.title =  element_blank(),
          legend.title =  element_text("", size=0.1),
          legend.text = element_text(size = 10),
          plot.title = element_text(size=13),
          plot.subtitle = element_text(size=9)
    )  +
  ggsave(paste("reportes_def/", mod,"_sem_cdmx/def/lag_sem_",maxfecha,".png", sep=""),  width = 220, height = 220 * 2/3, units = "mm")
  
  
  #------------------------------------------------------------------------------ 
  # Acumuladas
  
  def_semana_all_cum <-def_semana_all
  def_semana_all_cum$n_pred[def_semana_all_cum$SEMANA_DEF < sem_min_fit] <- def_semana_all_cum$n_late[def_semana_all_cum$SEMANA_DEF < sem_min_fit]
  def_semana_all_cum$n_pred_q25[def_semana_all_cum$SEMANA_DEF < sem_min_fit] <- def_semana_all_cum$n_late[def_semana_all_cum$SEMANA_DEF < sem_min_fit]
  def_semana_all_cum$n_pred_q975[def_semana_all_cum$SEMANA_DEF < sem_min_fit] <- def_semana_all_cum$n_late[def_semana_all_cum$SEMANA_DEF < sem_min_fit]
  
  

  def_semana_all_cum$obs_cum <- cumsum(def_semana_all_cum$n_tot)
  def_semana_all_cum$pred_cum <- cumsum(def_semana_all_cum$obs + def_semana_all_cum$n_pred - def_semana_all_cum$n_late)
  def_semana_all_cum$pred_cum_q25 <- cumsum(def_semana_all_cum$obs + def_semana_all_cum$n_pred_q25 - def_semana_all_cum$n_late)
  def_semana_all_cum$pred_cum_q975 <- cumsum(def_semana_all_cum$obs + def_semana_all_cum$n_pred_q975 - def_semana_all_cum$n_late)
  
  
  plot_deaths_cum <-
    def_semana_all_cum %>%
    #gather(key="tipo", value="n", -SEMANA_DEF) %>%
    ggplot() +
    geom_ribbon(aes(fecha, ymin=pred_cum_q25, ymax=pred_cum_q975), alpha=.3) +
    geom_line(aes(fecha, obs_cum), colour="black") +
    geom_line(aes(fecha, pred_cum), colour="red") +
    theme_bw() +
    scale_y_continuous("número de defunciones", breaks=seq(0,25000,1000), limits = c(0,25000)) +
    scale_x_date("fecha de defunción", breaks = seq(as.Date("2020-03-01"), as.Date("2021-06-30"), by="2 month"), 
                 limits = c(as.Date("2020-03-01"), as.Date("2021-06-30")),
                 date_labels =  "%m-%Y") +
    #scale_x_continuous("semana de defunción", breaks = seq(0, 70, 4), limits = c(1,70)) +
    scale_fill_manual(name = "",  values=c("red", "grey10"), labels = c("predicciones", "observados")) +
    scale_color_manual(name = "", values=c("black"), labels = c("int. de probabilidad de 95%")) +
    ggtitle(paste("Número acumulado de muertes por COVID-19 corregidas por el retraso en su registro (CDMX)", sep=""),
            subtitle = paste("La estimación se hace únicamente con los datos diarios publicados por la SSA, analizando la distribución\ndel número de semanas que transcurren entre el deceso y su registro. El modelo toma en cuenta semanas\ncompletas, por lo que las predicciones para las semanas más recientes no están disponibles.\nActualizado el",as.Date(maxfecha))) +
    theme(legend.position = c(0.2,0.8),
          axis.title= element_text(size=10),
          axis.text = element_text(size=10),
          #legend.title =  element_blank(),
          legend.title =  element_text("", size=0.1),
          legend.text = element_text(size = 10),
          plot.title = element_text(size=13),
          plot.subtitle = element_text(size=9)
    ) 
  
  
  max_x <- max(def_semana_all_cum$fecha) + 7
  min_x <- max(def_semana_all_cum$fecha) - 35
  

  min_y <- def_semana_all_cum$pred_cum_q25[def_semana_all_cum$fecha == min_x]
  min_y <- floor(min_y / 1000) * 1000 
  max_y <- ceiling(max(def_semana_all_cum$pred_cum_q975, na.rm=TRUE) / 1000) * 1000 + 1000
  
  plot_deaths_cum_inset <-
    plot_deaths_cum + 
    theme_bw() +
    scale_y_continuous("número de defunciones", breaks=seq(0,max_y,1000), limits = c(min_y,max_y)) +
    scale_x_date("fecha de defunción", breaks = seq(as.Date("2020-03-01"), as.Date("2021-06-30"), by="1 week"), 
                 limits = c(min_x, max_x),
                 date_labels =  "%d-%m-%Y") +
    #scale_x_continuous("fecha de defunción", breaks = seq(0, 70, 4), limits = c(min_x,max_x)) +
    theme(legend.position = "",
          axis.title=element_blank(),
          axis.text = element_text(size=6),
          plot.title = element_blank(),
          plot.subtitle = element_blank()
    ) 
  
  plot.with.inset <-
    ggdraw() +
    draw_plot(plot_deaths_cum) +
    draw_plot(plot_deaths_cum_inset, x = .59, y = .1, width = .39, height = .39) +
    ggsave(paste("reportes_def/", mod,"_sem_cdmx/def_cum/cum_lag_sem_",maxfecha,".png", sep=""),  width = 220, height = 220 * 2/3, units = "mm")
  
  
  #------------------------------------------------------------------------------ 
  # Lags
  def_semana_lag_lag <- 
    covid_def_lag %>%
    filter(FECHA_ACTUALIZACION <= maxfecha) %>%
    filter(lag_semana > 0) %>%
    group_by(SEMANA_DEF, lag_semana) %>%
    summarise(n=n()) %>%
    group_by(SEMANA_DEF) %>%
    mutate(N=sum(n)) %>%
    group_by() %>%
    mutate(p = n/N)
  
  # Predicted
  pred_lag <- tibble(lag=1:(length(ps_mod3) ), prob=ps_mod3)
  
  def_semana_lag_lag %>%
    filter(SEMANA_DEF >= sem_min_fit) %>%
    ggplot() +
    geom_line(aes(lag_semana, p, group=SEMANA_DEF, colour=SEMANA_DEF)) +
    geom_line(aes(lag,prob), data=pred_lag, colour="red", size=1) +
    scale_x_continuous("semanas transcurridas entre las fechas de defuncíon y registro", breaks = seq(1,17,1),
                       limits=c(1,17)) +
    scale_y_continuous("% de registros",
                       breaks=seq(0,1,.1),
                       limits = c(0,1),
                       labels = scales::percent_format(accuracy = 1)) +
    scale_color_continuous(name = "Semana de defunción") +
    theme_bw() +
    theme(legend.position = c(0.8,0.55),
          axis.title= element_text(size=10),
          axis.text = element_text(size=10),
          legend.text = element_text(size = 10),
          plot.title = element_text(size=13),
          plot.subtitle = element_text(size=9)
          #legend.direction="horizontal"
    )  +
    ggtitle(paste("Retraso en el registro de defunciones confirmadas por COVID-19 (CDMX)"),
            subtitle = paste("Distribución del número de semanas transcurridas entre las fechas de defunción y de registro para diferentes\nfechas de corte en la base de datos de la SSA (azul) y su estimación (rojo). Aproximadamente, el 70% de las\nmuertes se registran en las dos primeras semanas.\nActualizado el ", as.Date(maxfecha))) +
    ggsave(paste("reportes_def/",mod, "_sem_cdmx/lag/lag_",maxfecha,".png", sep=""),  width = 220, height = 220 * 2/3, units = "mm")
  
  
  
}

