library(R2jags)
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(vroom)



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
fecha_min_val <- as.Date("2021-01-10")
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
  
  
  covid_def_lag_2 <-
    covid_def_lag_2 %>%
    filter( lag>0) %>%
    group_by(FECHA_DEF) %>%
    mutate(N=sum(n)) %>%
    group_by() %>%
    arrange(FECHA_DEF, lag) %>%
    filter(FECHA_DEF < maxfecha) 
  
  
  jags.data <- createJagsData(covid_def_lag_2)
  
  # This is a good best hyperparameter:
  jags.data$alpha <- colMeans(jags.data$Y, na.rm = TRUE) * 100
  # This too:
  #jags.data$alpha <- colSums(jags.data$Y , na.rm=TRUE) 
  
  #jags.data$alpha <- colMeans(jags.data$Y / rowSums(jags.data$Y, na.rm = TRUE), na.rm=TRUE)
  #jags.data$alpha <- colSums(jags.data$Y , na.rm=TRUE) / seq(jags.data$Jmax,1) 
  
  
  
  if (mod == "model32" ) {
    est_params <- c("NN", "p")
  } else if(mod == "model33") {
    est_params <- c("NN", "p", "l", "k")
  }else if(mod == "model31") {
    est_params <- c("NN", "p", "beta")
  } else if (mod == "model32_1") {
    est_params <- c("NN", "p")
  }
  
  modelo <- do.call(jags.parallel, list(data = jags.data, 
                                         model.file=mod,
                                         parameters.to.save=est_params, 
                                         DIC=TRUE,
                                         n.chains=3, n.iter = 40000, n.burnin=20000,n.thin=50))
  
  
  save(modelo, file = paste("mcmc_def/",mod,"/", maxfecha, "-",mod,".RData", sep=""))
}



