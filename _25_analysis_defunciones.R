
library(R2jags)
library(lubridate)
library(tidyverse)
library(RColorBrewer)


source("_00_readData.R")



# Runs the model for dates from fecha_min_val to fecha_max_val
# To run maodel for only one date set fecha_min_val = fecha_max_val

fecha_max_val <- as.Date("2020-06-17")
fecha_min_val <- as.Date("2020-05-01")


mod <- "model33"


fechas_val <- seq.Date(from=fecha_min_val, to=fecha_max_val, by = "1 day")


for (ii in 1:length(fechas_val)) {
  maxfecha <- fechas_val[ii]
  fecha_pred <- maxfecha - 7
  
  print(paste("Starting", maxfecha, "----------------------------------"))
  
  load(paste("mcmc_def/", mod, "/",  maxfecha, "-", mod, ".RData", sep=""))
  
  
  ps_mod3 <- modelo3$BUGSoutput$mean$p
  NN_mod3 <- modelo3$BUGSoutput$mean$NN
  NN_q25_mod3 <- modelo3$BUGSoutput$summary[paste("NN[", 1:length(NN_mod3), "]", sep=""),"2.5%"]
  NN_q975_mod3 <- modelo3$BUGSoutput$summary[paste("NN[", 1:length(NN_mod3), "]", sep=""),"97.5%"]
  
  
  
  
  covid_def_lag <- 
    covid %>% 
    filter(FECHA_ACTUALIZACION <= maxfecha) %>%
    filter(MUERTO == 1, RESULTADO2 == "positivo") %>%
    filter(FECHA_DEF >= "2020-04-12") %>%
    filter(FECHA_ACTUALIZACION >= "2020-04-12") %>%
    group_by(ID_REGISTRO, FECHA_DEF) %>%
    summarise(FECHA_REG = min(FECHA_ACTUALIZACION)) %>%
    mutate(lag = as.integer(FECHA_REG - FECHA_DEF)) %>%
    group_by() 
  
  
  
  covid_def_lag_2 <-
    covid_def_lag %>%
    group_by(FECHA_DEF, lag) %>%
    summarise(n=n()) %>%
    group_by() %>%
    group_by(FECHA_DEF) %>%
    mutate(N=sum(n)) %>%
    group_by() %>%
    arrange(FECHA_DEF, lag)
  
  
  
  plag <- data.frame(lag=1:length(ps_mod3),p=ps_mod3)
  

  
  # plot
  plag_plot <- 
    covid_def_lag_2 %>%
    filter(lag != 0) %>%
    group_by(lag) %>%
    summarise(nn =sum(n), nmean = mean(n)) %>%
    mutate(pobs = nn /sum(nn), pobs_mean=nmean / sum(nmean)) %>%
    arrange(lag)
    
    
  lag_q50 <- plag_plot$lag[length(cumsum(plag_plot$pobs_mean)[cumsum(plag_plot$pobs_mean) <= .5])]
  lag_q75 <- plag_plot$lag[length(cumsum(plag_plot$pobs_mean)[cumsum(plag_plot$pobs_mean) <= .75])]
  lag_q95 <- plag_plot$lag[length(cumsum(plag_plot$pobs_mean)[cumsum(plag_plot$pobs_mean) <= .95])]
  
  plag_plot %>% 
    ggplot(aes(lag)) +
    geom_col(aes(y=pobs_mean,fill="observado"), alpha=.7) +
    geom_line(aes(lag, p,colour="estimado"), data=plag, size=1) +
    geom_vline(xintercept = c(lag_q50, lag_q75, lag_q95), colour="blue",linetype="dashed") +
    annotate(geom="text", x=c(lag_q50, lag_q75, lag_q95)+6, y=c(.2, .18,.16), label=c("<-mediana", "<-cuantil 75", "<-cuantil 95"), colour="blue") +
    scale_x_continuous("días transcurridos entre las fechas de defuncíon y registro", breaks = seq(0,91,7), 
                 limits=c(0,91)) +
    scale_y_continuous("probabilidad", 
                       breaks=seq(0,.20,.05), 
                       limits = c(0,.20), 
                       labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(name = "", values=c("red"), labels = c("estimado")) +
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
  
  ggsave(paste("reportes_def/",mod, "/lag_",maxfecha,".png", sep=""),  width = 180, height = 180 * 2/3, units = "mm")
  

  
  
  covid_cum_death <- 
    covid%>%
    filter(FECHA_ACTUALIZACION == maxfecha) %>%
    filter(MUERTO ==1,  RESULTADO2=="positivo") %>%
    group_by(FECHA_DEF) %>%
    summarise(n=n()) %>%
    group_by()
  
  
  reg_same_day <- 
    covid_def_lag %>%
    filter(FECHA_DEF == FECHA_REG) %>%
    group_by(FECHA_DEF) %>%
    summarise(n=n())
  
  reg_same_day[1:(length(reg_same_day$FECHA_DEF)-7),]
  
  
  
  
  
  results <-tibble(FECHA_DEF=seq(from=as.Date("2020-04-12"), to=fecha_pred, by="1 day"), n = NN_mod3, nq25=NN_mod3-NN_q25_mod3, nq975=NN_q975_mod3-NN_mod3) 
  
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


  plot_df %>%
    select(-nq25, -nq975, -npred) %>%
    gather(key="tipo", value="n", -FECHA_DEF) %>%
    ggplot() +
    geom_col(aes(FECHA_DEF, n, fill=tipo), position="stack", alpha=.8) +
    geom_errorbar(aes(FECHA_DEF, ymin=observados +faltantes - nq25, ymax=observados +faltantes + nq975, colour="black"), data=plot_df)+
    theme_bw() +
    scale_y_continuous("número de defunciones diarias", breaks=seq(0,1000,100), limits = c(0,1000)) +
    scale_x_date("fecha de defucnión", breaks = seq.Date(from=as.Date("2020-03-01"), to=as.Date("2020-07-17"), by="2 weeks"), 
                 limits=c(as.Date("2020-03-01"), as.Date("2020-07-17")),
                 date_labels = "%m-%d") +
    scale_fill_manual(name = "",  values=c("red", "grey10"), labels = c("estimados", "observados")) +
    scale_color_manual(name = "", values=c("black"), labels = c("int. de probabilidad de 95%")) +
    ggtitle(paste("Estimación de defunciones confirmadas diarias por COVID-19", sep=""),
            subtitle = paste("La estimación toma en cuenta únicamente el retraso en el registro de defunciones\nen las bases de datos de la Dirección General de Epidemiología\nActualizado el",as.Date(maxfecha))) +
    theme(legend.position = "bottom",
          axis.title= element_text(size=13),
          axis.text = element_text(size=12),
          legend.title =  element_blank(),
          legend.text = element_text(size = 13),
          plot.title = element_text(size=14),
          plot.subtitle = element_text(size=10)
    )  
  
  ggsave(paste("reportes_def/", mod,"/subregistro_cambio_",maxfecha,".png", sep=""),  width = 180, height = 180 * 2/3, units = "mm")
  
  
  
  
  plot_cum <-
    plot_df %>%
    select(FECHA_DEF,observados) %>%
    mutate(cum_obs=cumsum(observados)) 
  
  cum_pred <- 
    plot_df %>%
    select(-observados) %>%
    filter(!is.na(npred)) %>%
    mutate(cum_pred= cumsum(npred) + plot_cum$cum_obs[plot_cum$FECHA_DEF == "2020-04-12"],
           cum_up= cumsum(npred + nq975) + plot_cum$cum_obs[plot_cum$FECHA_DEF == "2020-04-12"],
           cum_down= cumsum(npred-nq25) + plot_cum$cum_obs[plot_cum$FECHA_DEF == "2020-04-12"]
           )
  

  plot_cum <-
    plot_cum %>%
    left_join(cum_pred) %>%
    select(FECHA_DEF, cum_obs, cum_pred, cum_up,cum_down )
  
  
  plot_cum %>% 
    ggplot() +
    geom_line(aes(FECHA_DEF, cum_obs, colour="black"), size=1) +
    geom_ribbon(aes(FECHA_DEF,ymin = cum_down, ymax =cum_up), fill = "grey70", alpha=.4) +
    geom_line(aes(FECHA_DEF, cum_pred,colour="red"), size=1) +
    # geom_line(aes(FECHA_DEF, cum_up), colour="red", linetype="dotted") +
    # geom_line(aes(FECHA_DEF, cum_down), colour="red", linetype="dotted") +
    theme_bw() +
    scale_y_continuous("número de defunciones acumuladas", breaks=seq(0,40000,5000), limits = c(0,40000)) +
    scale_x_date("fecha de defucnión", breaks = seq.Date(from=as.Date("2020-03-15"), to=as.Date("2020-08-07"), by="2 weeks"), 
                 limits=c(as.Date("2020-03-15"), as.Date("2020-08-07")),
                 date_labels = "%m-%d") +
    scale_color_manual(name = "",  values=c("black", "red"), labels = c("observados", "estimados")) +
    ggtitle(paste("Estimación de defunciones confirmadas acumuladas por COVID-19", sep=""),
            subtitle = paste("La estimación toma en cuenta únicamente el retraso en el registro de defunciones\nen las bases de datos de la Dirección General de Epidemiología\nActualizado el",as.Date(maxfecha))) +
    theme(legend.position = "bottom",
          axis.title= element_text(size=13),
          axis.text = element_text(size=12),
          legend.title =  element_blank(),
          legend.text = element_text(size = 13),
          plot.title = element_text(size=14),
          plot.subtitle = element_text(size=10)
    )  
  ggsave(paste("reportes_def/",mod, "/subregistro_",maxfecha,".png", sep=""),  width = 180, height = 180 * 2/3, units = "mm")
    
}

