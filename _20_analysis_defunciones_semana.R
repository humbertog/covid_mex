library(R2jags)
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(vroom)



# Reads data and selects only the confirmed COVID19 cases
covid_def <- readRDS("datos/covid_def_incremental_2021-01-10.rds")
covid_def <- covid_def %>% filter(RESULTADO == 1)

# Remove the IDs that do not appear on the last DB. The extra cases in our DB 
# may be explained by reclassification or errors. We ASSUME that they are 
# reclassification and thus we delete them so we have the same number of deaths 
# reported by the authorities.
last_db <- vroom("datos_csv/202101/20210110.zip", na=c("9999-99-99"), col_types=cols(PAIS_ORIGEN=col_character(), FECHA_DEF=col_date())) 
last_db <- last_db %>% filter(CLASIFICACION_FINAL <= 3, FECHA_DEF >= "2020-03-01") 
covid_def <- covid_def %>% filter(ID_REGISTRO %in% c(last_db$ID_REGISTRO[last_db$CLASIFICACION_FINAL <= 3]))
gc()




# Runs the model for dates from fecha_min_val to fecha_max_val
# To run maodel for only one date set fecha_min_val = fecha_max_val
fecha_min_val <- as.Date("2020-06-07")
fecha_max_val <- as.Date("2021-01-10")
fechas_val <- seq.Date(from=fecha_min_val, to=fecha_max_val, by = "7 day")
mod <- "model34"


fecha_min_fit <- as.Date("2020-04-12")


# Obtains the lags
covid_def_lag <- 
  covid_def %>% 
  filter(FECHA_DEF >= as.Date()) %>%
  filter(FECHA_ACTUALIZACION >= fecha_min_fit) %>%
  mutate(SEMANA_DEF = as.numeric(cut(FECHA_DEF, seq(as.Date(""), as.Date("2021-01-31"), by="1 week")))) %>%
  mutate(SEMANA_ACTUALIZACION = as.numeric(cut(FECHA_ACTUALIZACION, seq(fecha_min_fit+1, as.Date("2021-01-31"), by="1 week")))) %>%
  mutate(lag_dia = as.numeric(FECHA_ACTUALIZACION - FECHA_DEF)) %>%
  mutate(lag_semana = SEMANA_ACTUALIZACION - SEMANA_DEF) %>%
  select(ID_REGISTRO, FECHA_DEF, FECHA_ACTUALIZACION, SEMANA_DEF, SEMANA_ACTUALIZACION, lag_dia, lag_semana) %>%
  arrange(lag_semana)




for (ii in 1:length(fechas_val)) {
  maxfecha <- fechas_val[ii]
  wday <- wday(maxfecha, week_start=1)
  # No considera ultima semana, al menos que sea completa
  maxsemana <-  unique(covid_def_lag$SEMANA_ACTUALIZACION[covid_def_lag$FECHA_ACTUALIZACION == maxfecha])
  if (wday == 7) {
    maxsemana <- maxsemana 
  } else {
    maxsemana <- maxsemana - 1
  }
  
  print(paste("Starting", maxfecha, "----------------------------------"))
  # Loac MCMC
  load(paste("mcmc_def/", mod, "_sem/",  maxfecha, "-", mod, ".RData", sep=""))
  ps_mod3 <- colMeans(modelo$BUGSoutput$mean$p)
  NN_mod3 <- modelo$BUGSoutput$mean$NN
  NN_q25_mod3 <- modelo$BUGSoutput$summary[paste("NN[", 1:length(NN_mod3), "]", sep=""),"2.5%"]
  NN_q975_mod3 <- modelo$BUGSoutput$summary[paste("NN[", 1:length(NN_mod3), "]", sep=""),"97.5%"]
  
  
  covid_def_lag_2 <- 
    covid_def_lag %>%
    filter(SEMANA_ACTUALIZACION <= maxsemana) %>%
    group_by(SEMANA_DEF, lag_semana) %>%
    summarise(n=n()) %>%
    group_by() %>%
    filter( lag_semana>0) %>%
    group_by(SEMANA_DEF) %>%
    mutate(N=sum(n)) %>%
    group_by()
  
  
  plag <- data.frame(lag_semana=1:length(ps_mod3),p=ps_mod3)
  
  
  a <- covid_def_lag_2 %>%
    group_by(lag_semana) %>%
    summarise(nn =sum(n), nmean = mean(n),num=n()) %>%
    mutate(pobs = nn /(num+1-lag_semana), pobs_mean=nmean / sum(nmean)) %>%
    group_by() 
  
  
  # plot
  plag_plot <- 
    covid_def_lag_2 %>%
    group_by(lag_semana) %>%
    summarise(nn =sum(n), nmean = mean(n),num=n()) %>%
    mutate(pobs = nn /(num), pobs_mean=nmean / sum(nmean)) %>%
    group_by() %>%
    mutate(pobs_sum = sum(pobs)) %>%
    mutate(pobs = pobs /sum(pobs, na.rm = TRUE) ) %>%
    arrange(lag_semana)
  
  
  lag_q50 <- sum(plag_plot$pobs * plag_plot$lag_semana)
  lag_q75 <- plag_plot$lag_semana[length(cumsum(plag_plot$pobs)[cumsum(plag_plot$pobs) <= .75])]
  lag_q95 <- plag_plot$lag_semana[length(cumsum(plag_plot$pobs)[cumsum(plag_plot$pobs) <= .90])]
  
  plag_plot %>% 
    ggplot(aes(lag_semana)) +
    geom_col(aes(y=pobs,fill="observado"), alpha=.7) +
    geom_line(aes(lag_semana, p,colour="estimado"), data=plag, size=1) +
    geom_vline(xintercept = c(lag_q75, lag_q95), colour="blue",linetype="dashed") +
    annotate(geom="text", x=c(lag_q75, lag_q95)+3, y=c(.50,.50), label=c("<-cuantil 75", "<-cuantil 90"), colour="blue") +
    scale_x_continuous("semanas transcurridas entre las fechas de defuncíon y registro", breaks = seq(0,40,5), 
                       limits=c(0,40,5)) +
    scale_y_continuous("probabilidad", 
                       breaks=seq(0,.60,.05), 
                       limits = c(0,.60), 
                       labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(name = "", values=c("red"), labels = c("ajuste")) +
    scale_fill_manual(name = "", values=c("black"), labels = c( "observado")) +
    theme_bw() +
    theme(legend.position = c(.8,.55),
          axis.title= element_text(size=13),
          axis.text = element_text(size=12),
          legend.title =  element_blank(),
          legend.text = element_text(size = 13),
          plot.title = element_text(size=14),
          plot.subtitle = element_text(size=10)
    )  +
    ggtitle(paste("Retraso en el registro de defunciones confirmadas por COVID-19"),
            subtitle = paste("Distribución del número de semanas transcurridas entre las fechas de defunción y de registro\nActualizado el ", as.Date(maxfecha)))
  
  ggsave(paste("reportes_def/",mod, "_sem/lag/lag_",maxfecha,".png", sep=""),  width = 180, height = 180 * 2/3, units = "mm")
  
  
  
  
  covid_cum_death <- 
    covid_def_lag %>%
    filter(SEMANA_ACTUALIZACION <= maxsemana) %>%
    group_by(SEMANA_DEF) %>%
    summarise(n=n()) %>%
    group_by()
  
  
  reg_same_day <- 
    covid_def_lag %>%
    filter(SEMANA_DEF == SEMANA_ACTUALIZACION) %>%
    group_by(SEMANA_DEF) %>%
    summarise(n=n())
  
  results <-tibble(SEMANA_DEF=seq(from=1, to=maxsemana-1), n = NN_mod3, nq25=NN_mod3-NN_q25_mod3, nq975=NN_q975_mod3-NN_mod3) 
  
  results2 <- 
    results %>%
    left_join(reg_same_day, by="SEMANA_DEF") %>%
    mutate(n.y=ifelse(is.na(n.y),0,n.y)) %>%
    mutate(npred=n.x + n.y) %>%
    select(-n.x,  -n.y)
  
  
  plot_df <-
    covid_cum_death %>%
    left_join(results2) %>%
    #spread(key="tipo", value="n") %>%
    mutate(faltantes = ifelse(is.na(npred),NA,npred-n)) %>%
    rename(observados=n)
  
  
  #plot_df[plot_df$SEMANA_DEF > maxsemana-1, c("nq25", "nq975", "npred", "faltantes")] <- NA
  
  plot_df %>%
    select(-nq25, -nq975, -npred) %>%
    gather(key="tipo", value="n", -SEMANA_DEF) %>%
    ggplot() +
    geom_col(aes(SEMANA_DEF, n, fill=tipo), position="stack", alpha=.8) +
    geom_errorbar(aes(SEMANA_DEF, ymin=observados +faltantes - nq25, ymax=observados +faltantes + nq975, colour="black"), data=plot_df, size=.1)+
    theme_bw() +
    scale_y_continuous("número de defunciones", breaks=seq(0,8000,1000), limits = c(0,8000)) +
    scale_x_continuous("semana de defunción", breaks = seq(1, 60, 5), limits = c(1,60)) +
    scale_fill_manual(name = "",  values=c("red", "grey10"), labels = c("estimados", "observados")) +
    scale_color_manual(name = "", values=c("black"), labels = c("int. de probabilidad de 95%")) +
    ggtitle(paste("Estimación de defunciones confirmadas semanales por COVID-19", sep=""),
            subtitle = paste("La estimación toma en cuenta únicamente el retraso en el registro de defunciones\nen las bases de datos de la Dirección General de Epidemiología\nActualizado el",as.Date(maxfecha))) +
    theme(legend.position = "bottom",
          axis.title= element_text(size=13),
          axis.text = element_text(size=10),
          legend.title =  element_blank(),
          legend.text = element_text(size = 13),
          plot.title = element_text(size=14),
          plot.subtitle = element_text(size=10)
    )  
  
  ggsave(paste("reportes_def/", mod,"_sem/def/lag_sem_",maxfecha,".png", sep=""),  width = 180, height = 180 * 2/3, units = "mm")
  
  
  
  
}

