library(R2jags)
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(vroom)
library(cowplot)

source("_00_initialization.R")

for (ii in 1:length(fechas_val)) {
  maxfecha <- fechas_val[ii]
  wday <- wday(maxfecha, week_start=1)
  # No considera ultima semana, al menos que sea completa 
  # (la base tiene que ser domingo para entrar en el analisis)
  maxsemana <-  unique(covid_def_lag$SEMANA_ACTUALIZACION[covid_def_lag$FECHA_ACTUALIZACION == maxfecha])
  if (wday == 7) {
    maxsemana <- maxsemana 
  } else {
    maxsemana <- maxsemana - 1
  }
  
  
  print(paste("Starting", maxfecha, "----------------------------------"))
  
  
  covid_def_lag_2 <- 
    covid_def_lag %>%
    filter(SEMANA_ACTUALIZACION <= maxsemana) %>%
    filter(SEMANA_DEF >= sem_min_fit) %>%
    group_by(SEMANA_DEF, lag_semana) %>%
    summarise(n=n()) %>%
    group_by() %>%
    filter( lag_semana>0) %>%
    group_by(SEMANA_DEF) %>%
    mutate(N=sum(n)) %>%
    group_by() %>%
    arrange(SEMANA_DEF, lag_semana)
    
  
  covid_def_lag_2 %>% group_by(SEMANA_DEF) %>% summarise(sum(n)) %>% print(n=100)
  
  # Crea los datos para estimacion en Jags:
  semanas <- sort(unique(covid_def_lag_2$SEMANA_DEF))
  lags <- rev(max(semanas) - semanas + 1)
  
  I <- length(semanas)
  Jmax <- length(lags)
  
  Y <- matrix(rep(NA, I * Jmax), nrow = I)
  
  for (i in 1:I) {
    for (j in 1:(Jmax-i+1)) {
      sem <- semanas[i]
      lag <- lags[j]
      
      jd_temp <- covid_def_lag_2 %>% filter(SEMANA_DEF == sem, lag_semana == lag)
      if (length(jd_temp$SEMANA_DEF) == 0) {
        yi <- 0
      } else {
        yi <- jd_temp$n
      }
      Y[i, j] <- yi
    }
  }
  
  
  J <- Jmax:1
  N <- rowSums(Y, na.rm=TRUE)
  
  
  jags.data <- list(Y=Y, N=N, J=J, Jmax=Jmax, I=I)
  

  # Prior
  jags.data$alpha <- colSums(jags.data$Y, na.rm = TRUE) 
  #jags.data$alpha <- c(1000,  rep(.01, Jmax-1))
                        
  
  
  if (mod == "model32" ) {
    est_params <- c("NN", "p")
  } else if(mod == "model33") {
    est_params <- c("NN", "p", "l", "k")
  }else if(mod == "model31") {
    est_params <- c("NN", "p", "beta")
  } else if (mod == "model32_1") {
    est_params <- c("NN", "p")
  } else if (mod == "model34") {
    est_params <- c("NN", "p")
  }
  
  modelo <- do.call(jags.parallel, list(data = jags.data, 
                                        model.file=mod,
                                        parameters.to.save=est_params, 
                                        DIC=FALSE,
                                        n.chains=4, n.iter = 40000, n.burnin=20000,n.thin=100))
  
  
  save(modelo, file = paste("mcmc_def/",mod,"_sem/", maxfecha, "-",mod,".RData", sep=""))
}



