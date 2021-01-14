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
fecha_min_val <- as.Date("2021-01-10")
fecha_max_val <- as.Date("2021-01-10")
fechas_val <- seq.Date(from=fecha_min_val, to=fecha_max_val, by = "1 day")
mod <- "model32"


fecha_min_fit <- as.Date("2020-05-31")


# Obtains the lags
covid_def_lag <- 
  covid_def %>% 
  filter(FECHA_DEF > fecha_min_fit) %>%
  filter(FECHA_ACTUALIZACION >= fecha_min_fit) %>%
  mutate(SEMANA_DEF = as.numeric(cut(FECHA_DEF, seq(fecha_min_fit+1, as.Date("2021-01-31"), by="1 week")))) %>%
  mutate(SEMANA_ACTUALIZACION = as.numeric(cut(FECHA_ACTUALIZACION, seq(fecha_min_fit+1, as.Date("2021-01-31"), by="1 week")))) %>%
  mutate(lag_dia = as.numeric(FECHA_ACTUALIZACION - FECHA_DEF)) %>%
  mutate(lag_semana = SEMANA_ACTUALIZACION - SEMANA_DEF) %>%
  select(ID_REGISTRO, FECHA_DEF, FECHA_ACTUALIZACION, SEMANA_DEF, SEMANA_ACTUALIZACION, lag_dia, lag_semana) %>%
  arrange(lag_semana)


covid_def_lag %>%
  distinct(SEMANA_ACTUALIZACION)


ii <- 1
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


semanas <- 1:(maxsemana - 1)
Jmax <- max(semanas)
I <- max(semanas)
Y <- matrix(rep(NA, I * Jmax), nrow = I)

for (i in 1:I) {
  for (j in 1:(Jmax-i+1)) {
    jd_temp <- covid_def_lag_2 %>% filter(SEMANA_DEF == i, lag_semana == j)
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



#jags.data$alpha <- colMeans(jags.data$Y, na.rm = TRUE) 
jags.data$alpha <- c(1000,  rep(.01, Jmax-1))
#jags.data$alpha <- rep(1/Jmax, Jmax)

modelo_new <- do.call(jags.parallel, list(data = jags.data, 
                                      model.file="model34",
                                      parameters.to.save=c("p", "NN", "g"), 
                                      DIC=FALSE,
                                      n.chains=4, n.iter = 40000, n.burnin=20000,n.thin=100))

plot(modelo_new)

res <- modelo_new$BUGSoutput$mean$p

covid_def_lag_test <-
  covid_def_lag %>%
  group_by(SEMANA_DEF, lag_semana) %>%
  summarise(n=n()) %>%
  group_by() %>%
  filter( lag_semana>0) %>%
  group_by(SEMANA_DEF) %>%
  mutate(N=sum(n)) %>%
  group_by() %>%
  mutate(p = n/N)


datares <- data.frame(x=1:length(res), res=res)
covid_def_lag_test %>%
  ggplot() +
  geom_line(aes(lag_semana, p, group=SEMANA_DEF, colour=SEMANA_DEF)) +
  geom_line(aes(x,res), data=datares, colour="red")


