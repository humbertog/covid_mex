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
covid_def <- covid_def %>% filter(ID_REGISTRO %in% c(last_db$ID_REGISTRO[last_db$CLASIFICACION_FINAL <= 3]))


# Runs the model for dates from fecha_min_val to fecha_max_val
# To run maodel for only one date set fecha_min_val = fecha_max_val
fecha_min_val <- as.Date("2021-01-01")
fecha_max_val <- as.Date("2021-01-10")
fechas_val <- seq.Date(from=fecha_min_val, to=fecha_max_val, by = "1 day")
mod <- "model32"


fecha_min_fit <- as.Date("2020-04-12")


# Obtains the lags
covid_def_lag <- 
  covid_def %>% 
  filter(FECHA_DEF >= fecha_min_fit) %>%
  filter(FECHA_ACTUALIZACION >= fecha_min_fit) %>%
  mutate(lag = as.integer(FECHA_ACTUALIZACION - FECHA_DEF)) %>%
  select(ID_REGISTRO, FECHA_DEF, FECHA_ACTUALIZACION, lag) 



for (ii in 1:length(fechas_val)) {
  maxfecha <- fechas_val[ii]
  fecha_pred <- maxfecha - 7
  
  print(paste("Starting", maxfecha, "----------------------------------"))
  # Loac MCMC
  load(paste("mcmc_def/", mod, "/",  maxfecha, "-", mod, ".RData", sep=""))
  ps_mod3 <- modelo$BUGSoutput$mean$p
  NN_mod3 <- modelo$BUGSoutput$mean$NN
  NN_q25_mod3 <- modelo$BUGSoutput$summary[paste("NN[", 1:length(NN_mod3), "]", sep=""),"2.5%"]
  NN_q975_mod3 <- modelo$BUGSoutput$summary[paste("NN[", 1:length(NN_mod3), "]", sep=""),"97.5%"]
  
  
  covid_def_lag_2 <- 
    covid_def_lag %>%
    filter(FECHA_ACTUALIZACION <= maxfecha) %>%
    group_by(FECHA_DEF, lag) %>%
    summarise(n=n()) %>%
    group_by()
  
  
  # Adds the missing lags for each FECHA_DEF in covid_def_lag_2
  fb <- sort(unique(covid_def_lag_2$FECHA_DEF))
  lmax <- 1:as.integer(maxfecha - fecha_min_fit)
  for (fi in 1:length(fb)) {
    f <- fb[fi]
    idx <- which(!(lmax %in% covid_def_lag_2$lag[covid_def_lag_2$FECHA_DEF == f]))
    covid_def_lag_2 <- bind_rows(covid_def_lag_2, 
                                 tibble(FECHA_DEF=f, lag=idx, n=0))
  }
  

  # Counts the number of occurrences 
  covid_def_lag_2 <-
    covid_def_lag_2 %>%
    filter( lag>0) %>%
    group_by(FECHA_DEF) %>%
    mutate(N=sum(n)) %>%
    group_by() %>%
    arrange(FECHA_DEF, lag) %>%
    filter(FECHA_DEF < maxfecha) 
  
  
  
  
  plag <- data.frame(lag=1:length(ps_mod3),p=ps_mod3)
  
  
  
  # plot
  plag_plot <- 
    covid_def_lag_2 %>%
    filter(lag != 0) %>%
    group_by(lag) %>%
    summarise(nn =sum(n), nmean = mean(n),num=n()) %>%
    mutate(pobs = nn /(num+1-lag), pobs_mean=nmean / sum(nmean)) %>%
    mutate(pobs = pobs /sum(pobs, na.rm = TRUE) ) %>%
    arrange(lag)
  
  
  lag_q50 <- plag_plot$lag[length(cumsum(plag_plot$pobs)[cumsum(plag_plot$pobs) <= .5])]
  lag_q75 <- plag_plot$lag[length(cumsum(plag_plot$pobs)[cumsum(plag_plot$pobs) <= .75])]
  lag_q95 <- plag_plot$lag[length(cumsum(plag_plot$pobs)[cumsum(plag_plot$pobs) <= .90])]
  
  plag_plot %>% 
    ggplot(aes(lag)) +
    geom_col(aes(y=pobs,fill="observado"), alpha=.7) +
    geom_line(aes(lag, p,colour="estimado"), data=plag, size=1) +
    geom_vline(xintercept = c(lag_q50, lag_q75, lag_q95), colour="blue",linetype="dashed") +
    annotate(geom="text", x=c(lag_q50, lag_q75, lag_q95)+8, y=c(.2, .18,.16), label=c("<-mediana", "<-cuantil 75", "<-cuantil 90"), colour="blue") +
    scale_x_continuous("días transcurridos entre las fechas de defuncíon y registro", breaks = seq(0,120,15), 
                       limits=c(0,120)) +
    scale_y_continuous("probabilidad", 
                       breaks=seq(0,.25,.025), 
                       limits = c(0,.25), 
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
            subtitle = paste("Distribución del número de días transcurridos entre las fechas de defunción y de registro\nActualizado el ", as.Date(maxfecha)))
  
  ggsave(paste("reportes_def/",mod, "/lag/lag_",maxfecha,".png", sep=""),  width = 180, height = 180 * 2/3, units = "mm")
  
  
  
  
  covid_cum_death <- 
    covid_def %>%
    filter(FECHA_ACTUALIZACION <= maxfecha) %>%
    group_by(FECHA_DEF) %>%
    summarise(n=n()) %>%
    group_by()
  
  
  reg_same_day <- 
    covid_def_lag %>%
    filter(FECHA_DEF == FECHA_ACTUALIZACION) %>%
    group_by(FECHA_DEF) %>%
    summarise(n=n())
  
  reg_same_day[1:(length(reg_same_day$FECHA_DEF)-7),]
  
  
  results <-tibble(FECHA_DEF=seq(from=fecha_min_fit, to=maxfecha-1, by="1 day"), n = NN_mod3, nq25=NN_mod3-NN_q25_mod3, nq975=NN_q975_mod3-NN_mod3) 
  
  results2 <- 
    results %>%
    left_join(reg_same_day, by="FECHA_DEF") %>%
    mutate(n.y=ifelse(is.na(n.y),0,n.y)) %>%
    mutate(npred=n.x + n.y) %>%
    select(-n.x,  -n.y)
  
  
  plot_df <-
    covid_cum_death %>%
    left_join(results2) %>%
    #spread(key="tipo", value="n") %>%
    mutate(faltantes = ifelse(is.na(npred),NA,npred-n)) %>%
    rename(observados=n)
  
  
  plot_df[plot_df$FECHA_DEF > fecha_pred,c("nq25", "nq975", "npred", "faltantes")] <- NA
  
  plot_df %>%
    select(-nq25, -nq975, -npred) %>%
    gather(key="tipo", value="n", -FECHA_DEF) %>%
    ggplot() +
    geom_col(aes(FECHA_DEF, n, fill=tipo), position="stack", alpha=.8) +
    geom_errorbar(aes(FECHA_DEF, ymin=observados +faltantes - nq25, ymax=observados +faltantes + nq975, colour="black"), data=plot_df, size=.1)+
    theme_bw() +
    scale_y_continuous("número de defunciones diarias", breaks=seq(0,1500,100), limits = c(0,1500)) +
    scale_x_date("fecha de defucnión", breaks = seq.Date(from=as.Date("2020-04-01"), to=as.Date("2021-07-01"), by="1 month"), 
                 limits=c(as.Date("2020-04-01"), as.Date("2021-07-01")),
                 date_labels = "%m/%y") +
    scale_fill_manual(name = "",  values=c("red", "grey10"), labels = c("estimados", "observados")) +
    scale_color_manual(name = "", values=c("black"), labels = c("int. de probabilidad de 95%")) +
    ggtitle(paste("Estimación de defunciones confirmadas diarias por COVID-19", sep=""),
            subtitle = paste("La estimación toma en cuenta únicamente el retraso en el registro de defunciones\nen las bases de datos de la Dirección General de Epidemiología\nActualizado el",as.Date(maxfecha))) +
    theme(legend.position = "bottom",
          axis.title= element_text(size=13),
          axis.text = element_text(size=10),
          legend.title =  element_blank(),
          legend.text = element_text(size = 13),
          plot.title = element_text(size=14),
          plot.subtitle = element_text(size=10)
    )  
  
  ggsave(paste("reportes_def/", mod,"/def_day/lag_day_",maxfecha,".png", sep=""),  width = 180, height = 180 * 2/3, units = "mm")
  
  
  
  
  plot_cum <-
    plot_df %>%
    select(FECHA_DEF,observados) %>%
    mutate(cum_obs=cumsum(observados)) 
  
  cum_pred <- 
    plot_df %>%
    select(-observados) %>%
    filter(!is.na(npred)) %>%
    mutate(cum_pred= cumsum(npred) + plot_cum$cum_obs[plot_cum$FECHA_DEF == fecha_min_fit],
           cum_up= cumsum(npred + nq975) + plot_cum$cum_obs[plot_cum$FECHA_DEF == fecha_min_fit],
           cum_down= cumsum(npred-nq25) + plot_cum$cum_obs[plot_cum$FECHA_DEF == fecha_min_fit]
    )
  
  
  plot_cum <-
    plot_cum %>%
    left_join(cum_pred) %>%
    select(FECHA_DEF, cum_obs, cum_pred, cum_up,cum_down )
  
  
  plot_cum %>% 
    ggplot() +
    geom_line(aes(FECHA_DEF, cum_obs, colour="black"), size=.5) +
    geom_ribbon(aes(FECHA_DEF,ymin = cum_down, ymax =cum_up), fill = "grey70", alpha=.4) +
    geom_line(aes(FECHA_DEF, cum_pred,colour="red"), size=.5) +
    # geom_line(aes(FECHA_DEF, cum_up), colour="red", linetype="dotted") +
    # geom_line(aes(FECHA_DEF, cum_down), colour="red", linetype="dotted") +
    theme_bw() +
    scale_y_continuous("número de defunciones acumuladas", breaks=seq(0,200000,10000), limits = c(0,200000)) +
    scale_x_date("fecha de defucnión", breaks = seq.Date(from=as.Date("2020-04-01"), to=as.Date("2021-07-01"), by="1 month"), 
                 limits=c(as.Date("2020-04-01"), as.Date("2021-07-01")),
                 date_labels = "%m/%y") +
    scale_color_manual(name = "",  values=c("black", "red"), labels = c("observados", "estimados")) +
    ggtitle(paste("Estimación de defunciones confirmadas acumuladas por COVID-19", sep=""),
            subtitle = paste("La estimación toma en cuenta únicamente el retraso en el registro de defunciones\nen las bases de datos de la Dirección General de Epidemiología\nActualizado el",as.Date(maxfecha))) +
    theme(legend.position = "bottom",
          axis.title= element_text(size=13),
          axis.text = element_text(size=10),
          legend.title =  element_blank(),
          legend.text = element_text(size = 13),
          plot.title = element_text(size=14),
          plot.subtitle = element_text(size=10)
    )  
  ggsave(paste("reportes_def/",mod, "/def_cum/lag_cum_",maxfecha,".png", sep=""),  width = 180, height = 180 * 2/3, units = "mm")
  
}

