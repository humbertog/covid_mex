
library(R2jags)
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(MCMCvis)

source("_00_readData.R")



# Runs the model for dates from fecha_min_val to fecha_max_val
# To run maodel for only one date set fecha_min_val = fecha_max_val

fecha_max_val <- as.Date("2020-06-16")
fecha_min_val <- as.Date("2020-06-16")



max_lag <- Inf
#max_lag <- 28
#max_lag <- 35

fechas_val <- seq.Date(from=fecha_min_val, to=fecha_max_val, by = "1 day")


for (ii in 1:length(fechas_val)) {
  maxfecha <- fechas_val[ii]
  fecha_pred <- maxfecha - 7
  
  print(paste("Starting", maxfecha, "----------------------------------"))
  
  load(paste("mcmc_defunciones/", maxfecha, "-model32.RData", sep=""))
  
  
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
    group_by()
  
  
  
  fb <- sort(unique(covid_def_lag_2$FECHA_DEF))
  lmax <- 1:as.integer(maxfecha - as.Date("2020-04-12"))
  
  for (fi in 1:length(fb)) {
    f <- fb[fi]
    idx <- which(!(lmax %in% covid_def_lag_2$lag[covid_def_lag_2$FECHA_DEF == f]))
    covid_def_lag_2 <- bind_rows(covid_def_lag_2, 
                                 tibble(FECHA_DEF=f, lag=idx, n=0))
  }
  
  
  covid_def_lag_2 <-
    covid_def_lag_2 %>%
    group_by(FECHA_DEF) %>%
    mutate(N=sum(n)) %>%
    group_by() %>%
    arrange(FECHA_DEF, lag)
  
  
  plag <- data.frame(lag=1:length(ps_mod3),p=ps_mod3)
  
  # plot
  covid_def_lag %>%
    filter(lag != 0) %>%
    ggplot(aes(lag)) +
    geom_histogram(aes(y=..density..,fill="observado"), binwidth = 1, center = 1, alpha=.7) +
    geom_line(aes(lag, p,colour="estimado"), data=plag, size=1) +
    xlim(0,90) +
    ylim(0,.2) +
    scale_color_manual(name = "", values=c("red"), labels = c("estimado")) +
    scale_fill_manual(name = "", values=c("black"), labels = c( "observado")) +
    theme_bw() +
    theme(legend.position = c(.2,.8),
          axis.title= element_text(size=12),
          axis.text = element_text(size=11),
          legend.title =  element_text(size = 12),
          legend.text = element_text(size = 12),
          plot.title = element_text(size=13),
          plot.subtitle = element_text(size=10)
    )  +
    ggtitle(paste("Distribución del número de días transcurridos entre la fecha de defunción y la fecha de reporte")) 
  
  ggsave(paste("reportes_defunciones/lag_",maxfecha,".png", sep=""),  width = 250, height = 250 * 2/3, units = "mm")
  
  
  
  # plot
  covid_def_lag %>%
    filter(lag!=0) %>%
    group_by(lag) %>%
    summarise(n=n()) %>%
    group_by() %>%
    mutate(cump = cumsum(n / sum(n))) %>%
    print(n=100)



    # ggplot(aes(lag)) +
    # geom_histogram(aes(y=..density..,fill="observado"), binwidth = 1, center = .5, alpha=.7) +
    # geom_line(aes(lag, p,colour="estimado"), data=plag, size=2) +
    # xlim(0,90) +
    # ylim(0,.2) +
    # scale_color_manual(name = "", values=c("red"), labels = c("estimado")) +
    # scale_fill_manual(name = "", values=c("black"), labels = c( "observado")) +
    # theme_bw() +
    # theme(legend.position = c(.7,.7),
    #       axis.title= element_text(size=11),
    #       axis.text = element_text(size=11),
    #       legend.title =  element_text(size = 11),
    #       legend.text = element_text(size = 11),
    #       plot.title = element_text(size=13),
    #       plot.subtitle = element_text(size=10)
    # ) +
    # ggtitle(paste("Distribución del número de días transcurridos entre la fecha de defunción y la fecha de reporte"))
  #   
  #   ggsave(paste("reportes_defunciones/lag_cum_",maxfecha,".png", sep=""),  width = 250, height = 250 * 2/3, units = "mm")
  # 
  
  covid_max_date <-
    covid%>%
    filter(FECHA_ACTUALIZACION == maxfecha) %>%
    filter(MUERTO ==1,  RESULTADO2=="positivo")
  
  
  covid_cum_death <- 
    covid_max_date %>%
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
    geom_col(aes(FECHA_DEF, n, fill=tipo), position="stack") +
    geom_errorbar(aes(FECHA_DEF, ymin=observados +faltantes - nq25, ymax=observados +faltantes + nq975), data=plot_df)+
    theme_bw() +
    scale_y_continuous("número de defunciones diarias", breaks=seq(0,1000,100), limits = c(0,1000)) +
    scale_x_date("fecha de defucnión", breaks = seq.Date(from=as.Date("2020-03-01"), to=as.Date("2020-07-17"), by="2 weeks"), 
                 limits=c(as.Date("2020-03-01"), as.Date("2020-07-17")),
                 date_labels = "%m-%d") +
    scale_fill_brewer(name = "", labels = c("estimados", "observados"), palette="Set1") +
    ggtitle(paste("Estimación de defunciones confirmadas diarias por fecha de defunción")) +
    theme(legend.position = c(.2,.8),
          axis.title= element_text(size=12),
          axis.text = element_text(size=11),
          legend.title =  element_text(size = 12),
          legend.text = element_text(size = 12),
          plot.title = element_text(size=13),
          plot.subtitle = element_text(size=10)
    ) 
  
  ggsave(paste("reportes_defunciones/subregistro_cambio_",maxfecha,".png", sep=""),  width = 250, height = 250 * 2/3, units = "mm")
  
  
  
  
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
    scale_color_brewer(name = "", labels = c("observados", "estimados"), palette="Set1") +
    ggtitle(paste("Estimación de defunciones acumuladas por COVIDD-19 por fecha de defunción")) +
    theme(legend.position = c(.2,.8),
          axis.title= element_text(size=12),
          axis.text = element_text(size=11),
          legend.title =  element_text(size = 12),
          legend.text = element_text(size = 12),
          plot.title = element_text(size=13),
          plot.subtitle = element_text(size=10)
    ) 
  ggsave(paste("reportes_defunciones/subregistro_",maxfecha,".png", sep=""),  width = 250, height = 250 * 2/3, units = "mm")
    
}

