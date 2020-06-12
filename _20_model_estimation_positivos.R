library(R2jags)
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(MCMCvis)

source("_00_readData.R")


covid <- covid %>% filter(RESULTADO2 == "positivo")

covid_all <- covid

# Runs the model for dates from fecha_min_val to fecha_max_val
# To run maodel for only one date set fecha_min_val = fecha_max_val

fecha_max_val <- as.Date("2020-06-10")
fecha_min_val <- as.Date("2020-05-12")



max_lag <- Inf
#max_lag <- 28
#max_lag <- 35

fechas_val <- seq.Date(from=fecha_min_val, to=fecha_max_val, by = "1 day")


for (ii in 1:length(fechas_val)) {
  maxfecha <- fechas_val[ii]
  fecha_pred <- maxfecha - 7
  
  print(paste("Starting", maxfecha, "----------------------------------"))
  
  covid <- 
    covid_all %>%
    filter(FECHA_ACTUALIZACION <= maxfecha) 
  
  covid_fecha_corte <-
    covid %>%
    group_by(FECHA_ACTUALIZACION, RESULTADO2) %>%
    summarise(n=n())  %>%
    group_by()
  
  # ----------------------------------------------------------------
  # Calcula el número de casos nuevos entre una base y otra, tomando como 
  # referencia FECHA_BASE
  
  covid_def <- 
    covid %>% 
    filter(RESULTADO2 == "positivo") %>%
    select(-FECHA_INGRESO,-FECHA_DEF, -RESULTADO)
  
  
  new_cases <- tibble()
  fechas <- sort(unique(covid$FECHA_ACTUALIZACION))
  fechasl <- fechas[-1]
  for (i in 1:length(fechasl)) {
    f2 <- fechasl[i]
    f1 <- f2 - 1
    
    new_f1f2 <- 
      covid_def %>%
      filter(FECHA_ACTUALIZACION == f2) %>%
      filter(!(ID_REGISTRO %in% c(covid_def$ID_REGISTRO[covid_def$FECHA_ACTUALIZACION == f1])))
    
    new_cases <- bind_rows(new_cases, new_f1f2)
    
  }
  
  new_cases_lag_all <- tibble()
  for (i in 1:(length(fechas)-1 )) {
    f <- fechas[i]
    new_cases_t <-
      new_cases %>%
      filter(FECHA_ACTUALIZACION > f, FECHA_SINTOMAS <= f) %>%
      group_by(FECHA_ACTUALIZACION) %>%
      count(.drop=FALSE) %>%
      ungroup() %>%
      mutate(lag = as.integer(FECHA_ACTUALIZACION - as.Date(f))) %>%
      mutate(FECHA_BASE = f) %>%
      ungroup() 
    
    lmax <- 1:max(new_cases_t$lag)
    zerocounts <- !(lmax %in% new_cases_t$lag)
    
    if(sum(zerocounts) > 0) {
      new_cases_t <- new_cases_t %>%
        bind_rows(tibble(FECHA_ACTUALIZACION = f + lmax[zerocounts], n=0, lag = lmax[zerocounts],FECHA_BASE=f ))
      
    }
    new_cases_t <- 
      new_cases_t %>%
      arrange(FECHA_ACTUALIZACION)
    
    
    new_cases_lag_all <- bind_rows(new_cases_lag_all, new_cases_t)
    
  }
  
  
  new_cases_lag <- 
    new_cases_lag_all %>%
    filter(lag <= max_lag) %>%
    filter(FECHA_BASE <= fecha_pred) %>%
    group_by(FECHA_BASE) %>%
    mutate(N = sum(n)) %>%
    arrange(lag) %>%
    group_by(FECHA_BASE) %>%
    mutate(cumn = cumsum(n))%>% 
    group_by() %>%
    mutate(wday = wday(FECHA_ACTUALIZACION, week_start = 1)) %>%
    mutate(wend = ifelse(wday > 5, 1, 0))
  
  
  #new_cases_lag %>% filter(FECHA_BASE=="2020-05-01") %>% print(n=100)
  # ----------------------------------------------------------------
  # Modelos
  
  createJagsData <- function(data_new_cases) {
    fb <- sort(unique(data_new_cases$FECHA_BASE))
    Jmax <- max(data_new_cases$lag)
    I <- length(fb)
    Y <- matrix(rep(NA, I * Jmax), nrow = I)
    J <- rep(NA, I)
    N <- rep(NA, I)
    
    for (i in 1:length(fb)) {
      jd_temp <- 
        data_new_cases %>%
        filter(FECHA_BASE == fb[i]) %>%
        arrange(lag) 
      
      yi <- jd_temp$n 
      ji <- length(yi)
      Y[i, 1:ji] <- yi
      J[i] <- ji
      N[i] <- sum(yi)
    }
    
    jags.data <- list(Y=Y, N=N, J=J, Jmax=Jmax, I=I)
    return(jags.data)
    
  }
  
  jags.data <- createJagsData(new_cases_lag)
  
  t1 <- proc.time()
  modelo1 <- do.call(jags, list(data = jags.data, 
                                model.file="modelo1",
                                parameters.to.save=c("NN", "beta", "p"), 
                                DIC=TRUE,
                                n.chains=3, n.iter = 100000))
  
  modelo2 <- do.call(jags, list(data = jags.data, 
                                model.file="modelo2",
                                parameters.to.save=c("NN", "p"), 
                                DIC=TRUE,
                                n.chains=3, n.iter = 100000))
  t2 <- proc.time()
  t2 - t1
  
  
  save(modelo1, file = paste("mcmc_positivos/", maxfecha, "-model1.RData", sep=""))
  save(modelo2, file = paste("mcmc_positivos/", maxfecha, "-model2.RData", sep=""))
  
}























