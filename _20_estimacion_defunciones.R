library(R2jags)
library(lubridate)
library(tidyverse)
library(RColorBrewer)

source("_00_readData.R")



# Runs the model for dates from fecha_min_val to fecha_max_val
# To run maodel for only one date set fecha_min_val = fecha_max_val

fecha_max_val <- as.Date("2020-06-29")
fecha_min_val <- as.Date("2020-06-29")


fecha_min_fit <- as.Date("2020-04-12")

mod <- "model32"

fechas_val <- seq.Date(from=fecha_min_val, to=fecha_max_val, by = "1 day")


for (ii in 1:length(fechas_val)) {
  maxfecha <- fechas_val[ii]
  fecha_pred <- maxfecha - 7
  
  print(paste("Starting", maxfecha, "----------------------------------"))


  covid_def_lag <- 
    covid %>% 
    filter(FECHA_ACTUALIZACION <= maxfecha) %>%
    filter(MUERTO == 1, RESULTADO2 == "positivo") %>%
    filter(FECHA_DEF >= fecha_min_fit) %>%
    filter(FECHA_ACTUALIZACION >= fecha_min_fit) %>%
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
  lmax <- 1:as.integer(maxfecha - fecha_min_fit)
  
  for (fi in 1:length(fb)) {
    f <- fb[fi]
    idx <- which(!(lmax %in% covid_def_lag_2$lag[covid_def_lag_2$FECHA_DEF == f]))
    covid_def_lag_2 <- bind_rows(covid_def_lag_2, 
                                 tibble(FECHA_DEF=f, lag=idx, n=0))
  }
  
  
  covid_def_lag_2 <-
    covid_def_lag_2 %>%
    filter( lag>0) %>%
    group_by(FECHA_DEF) %>%
    mutate(N=sum(n)) %>%
    group_by() %>%
    arrange(FECHA_DEF, lag) %>%
    #filter(FECHA_DEF <= fecha_pred) 
    filter(FECHA_DEF < maxfecha) 
  
  
  createJagsData <- function(data_new_cases) {
    fb <- sort(unique(data_new_cases$FECHA_DEF))
    Jmax <- max(data_new_cases$lag)
    I <- length(fb)
    Y <- matrix(rep(NA, I * Jmax), nrow = I)
    J <- rep(NA, I)
    N <- rep(NA, I)
    
    for (i in 1:length(fb)) {
      jd_temp <- 
        data_new_cases %>%
        filter(FECHA_DEF == fb[i]) %>%
        arrange(lag) 
      
      yi <- jd_temp$n 
      ji <- as.integer(maxfecha - fecha_min_fit) -i + 1
      Y[i, 1:ji] <- yi[1:ji]
      J[i] <- ji
      N[i] <- sum(yi)
    }
    
    jags.data <- list(Y=Y, N=N, J=J, Jmax=Jmax, I=I)
    return(jags.data)
    
  }
  
  jags.data <- createJagsData(covid_def_lag_2)
  
  jags.data$alpha <- colMeans(jags.data$Y, na.rm = TRUE) 
  #jags.data$alpha <- colMeans(jags.data$Y / rowSums(jags.data$Y, na.rm = TRUE), na.rm=TRUE)
  
  if (mod == "model32" ) {
    est_params <- c("NN", "p")
  } else if(mod == "model33") {
    est_params <- c("NN", "p", "l", "k")
  }else if(mod == "model31") {
    est_params <- c("NN", "p", "beta")
  } else if (mod == "model32_1") {
    est_params <- c("NN", "p")
  }
  
  modelo3 <- do.call(jags, list(data = jags.data, 
                                model.file=mod,
                                parameters.to.save=est_params, 
                                DIC=TRUE,
                                n.chains=3, n.iter = 100000))
  
  
  save(modelo3, file = paste("mcmc_def/",mod,"/", maxfecha, "-",mod,".RData", sep=""))
}



