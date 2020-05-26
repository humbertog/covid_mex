library(R2jags)
library(lubridate)
library(tidyverse)
library(RColorBrewer)

source("_readData.R")

maxfecha <- as.Date("2020-05-24")
fecha_pred <- as.Date("2020-05-21")

covid_fecha_corte <-
  covid %>%
  group_by(FECHA_ACTUALIZACION, RESULTADO2, MUERTO) %>%
  summarise(n=n())  %>%
  group_by()


# Incremento en el número de muertos en días pasados
# La variación es alta, incluso para actualizaciones consecutivas y 
# retrasos de más de dos semanas

# ----------------------------------------------------------------
# Calcula el número de casos nuevos entre una base y otra, tomando como 
# referencia FECHA_BASE
# ----------------------------------------------------------------


covid_def <- 
  covid %>% 
  filter(MUERTO == 1, RESULTADO2 == "positivo") %>%
  select(-FECHA_INGRESO, -FECHA_SINTOMAS, -RESULTADO)


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
    filter(FECHA_ACTUALIZACION > f, FECHA_DEF <= f) %>%
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
  filter(lag <= 28) %>%
  filter(FECHA_BASE <= fecha_pred) %>%
  group_by(FECHA_BASE) %>%
  mutate(N = sum(n)) %>%
  arrange(lag) %>%
  group_by(FECHA_BASE) %>%
  mutate(cumn = cumsum(n))%>% 
  group_by() %>%
  mutate(wday = wday(FECHA_ACTUALIZACION, week_start = 1)) %>%
  mutate(wend = ifelse(wday > 5, 1, 0))
  

# ----------------------------------------------------------------
# Exploratorio
# ----------------------------------------------------------------

# Numero de casos que se incluyen cada base extra
new_cases_lag %>%
  filter(FECHA_BASE<="2020-05-01") %>%
  ggplot() +
  geom_point(aes(lag, n, colour=as.factor(FECHA_BASE))) 


# Numero de casos normalizados
new_cases_lag %>%
  filter(FECHA_BASE<="2020-05-01") %>%
  ggplot() +
  geom_point(aes(lag, n/N, colour=as.factor(FECHA_BASE))) +
  geom_vline(xintercept = 7)

# Numero de casos normalizados acumulados
new_cases_lag %>%
  ggplot() +
  geom_point(aes(lag, cumn/N, colour=as.factor(FECHA_BASE))) 



# ----------------------------------------------------------------
# Modelos
# ----------------------------------------------------------------

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
modelo2 <- do.call(jags.parallel, list(data = jags.data, 
                                              model.file="modelo1",
                                              parameters.to.save=c("NN", "NF", "beta", "beta_w", "p"), 
                                              DIC=TRUE,
                                              n.chains=3, n.iter = 200000))

modelo3 <- do.call(jags.parallel, list(data = jags.data, 
                                       model.file="modelo2",
                                       parameters.to.save=c("NN", "NF", "beta", "beta_w", "p"), 
                                       DIC=TRUE,
                                       n.chains=3, n.iter = 200000))
t2 <- proc.time()
t2 - t1

# DIC
modelo2$BUGSoutput$DIC
modelo3$BUGSoutput$DIC
  

# Proportions and number of missing cases
ps_mod2 <- modelo2$BUGSoutput$mean$p
NN_mod2 <- modelo2$BUGSoutput$mean$NN
NN_q25_mod2 <- modelo2$BUGSoutput$summary[paste("NN[", 1:length(N), "]", sep=""),"2.5%"]
NN_q975_mod2 <- modelo2$BUGSoutput$summary[paste("NN[", 1:length(N), "]", sep=""),"97.5%"]




ps_mod3 <- modelo3$BUGSoutput$mean$p
NN_mod3 <- modelo3$BUGSoutput$mean$NN
NNsd_mod3 <- modelo3$BUGSoutput$sd$NN
NN_q25_mod3 <- modelo3$BUGSoutput$summary[paste("NN[", 1:length(N), "]", sep=""),"2.5%"]
NN_q975_mod3 <- modelo3$BUGSoutput$summary[paste("NN[", 1:length(N), "]", sep=""),"97.5%"]


# % de casos faltantes en cada actualizacion

plag <- bind_rows(tibble(lag=1:length(ps_mod2), plag = ps_mod2, modelo="modelo 1"),
                  tibble(lag=1:length(ps_mod3),plag = ps_mod3, modelo="modelo 2"))

plag %>%
  ggplot() +
  geom_line(aes(lag, plag, colour=modelo)) +
  theme_bw() +
  scale_colour_brewer(palette="Set1") +
  xlab("lag") + ylab("lambda") +
  theme(legend.position = c(.7,.7))

ggsave("lag_lambda.png", width = 160, height = 160 * 2/3, units = "mm")

plag %>%
  group_by(modelo) %>%
  arrange(lag) %>%
  mutate(cump = cumsum(plag)) %>%
  ggplot() +
  theme_bw() +
  geom_line(aes(lag, cump, colour=modelo)) +
  scale_colour_brewer(palette="Set1") +
  xlab("lag") + ylab("lambda") +
  theme(legend.position = "")
ggsave("lag_lambda_cum.png", width = 160, height = 160 * 2/3, units = "mm")


# numero de casos faltantes
fb <- sort(unique(new_cases_lag$FECHA_BASE))
NN <- bind_rows(tibble(FECHA_CORTE=fb, N_missing = NN_mod2, modelo="modelo 1"),
                tibble(FECHA_CORTE=fb,N_missing = NN_mod3, modelo="modelo 2"))

NN %>%
  ggplot() +
  geom_col(aes(FECHA_CORTE, N_missing, fill=modelo), position="dodge")


# 
covid_muertes_cambio <- bind_rows(tibble(FECHA_ACTUALIZACION = fb, RESULTADO2="positivo", MUERTO=1, n = NN_mod2, tipo="No registradas", modelo="modelo 1"),
                                  tibble(FECHA_ACTUALIZACION = fb, RESULTADO2="positivo", MUERTO=1, n = NN_mod3, tipo="No registradas", modelo="modelo 2"))
  
  
# Defunciones confirmadas por covid
covid_muertes_estimadas <- 
  bind_rows(covid_fecha_corte %>%
              filter(MUERTO ==1, RESULTADO2 == "positivo") %>%
              mutate(tipo="Registradas", modelo="modelo 1"),
            covid_fecha_corte %>%
              filter(MUERTO ==1, RESULTADO2 == "positivo") %>%
              mutate(tipo="Registradas", modelo="modelo 2"),
            covid_muertes_cambio
            )
            
  
covid_fecha_def_max <-
  covid_def %>%
  filter(FECHA_ACTUALIZACION == maxfecha) %>%
  group_by(FECHA_DEF) %>%
  summarise(n=n()) %>%
  arrange(FECHA_DEF) %>%
  mutate(cumn = cumsum(n)) 
  

intervalos <-
  covid_muertes_estimadas %>%
  group_by(FECHA_ACTUALIZACION, RESULTADO2, MUERTO, modelo) %>%
  summarise(n=sum(n)) %>%
  group_by() %>%
  arrange(modelo, FECHA_ACTUALIZACION) %>%
  filter(FECHA_ACTUALIZACION <= fecha_pred) %>%
  mutate(lowint=c(NN_mod2 - NN_q25_mod2, NN_mod3-NN_q25_mod3),
         upint =c(NN_q975_mod2 - NN_mod2, NN_q975_mod3-NN_mod3))

covid_muertes_estimadas %>%
  ggplot() +
  geom_col(aes(FECHA_ACTUALIZACION, n, fill=tipo)) +
  geom_line(aes(FECHA_DEF, cumn), data=covid_fecha_def_max, size=1) +
  geom_errorbar(aes(FECHA_ACTUALIZACION, ymin=n-lowint, ymax=n+upint), data=intervalos)+
  theme_bw() +
  scale_fill_brewer(palette="Set1")+
  facet_grid(.~modelo) +
  xlab("fecha") + ylab("número de defunciones") +
  ggtitle("Subregistro en el número acumulado de defunciones en cada fecha de corte.", 
          subtitle = 
          "El subregistro corresponde a cada fecha de corte.
La línea negra es número acumulado de defunciones en la última actualización.
El subregistro por debajo de la línea es el subregistro que se contabilizó en fechas posteriores al corte correspondiente.
El subregistro por encima de la línea es el subregistro estimado y que será contabilizado en fechas posteriores al día de hoy.")

ggsave("subregistro.png", width = 200, height = 200 * 2/3, units = "mm")
ggsave("subregistro.png", width = 400, height = 400 * 2/3, units = "mm")



# ----------------------------------------------------------------
# Tiempo de duplicación
# ----------------------------------------------------------------
fecha_tendencia <- as.Date("2020-05-01")

covid_muertes_estimadas_change <-
  covid_muertes_estimadas %>%
  group_by(FECHA_ACTUALIZACION, modelo) %>%
  summarise(n=sum(n)) %>%
  group_by() %>%
  filter(modelo=="modelo 2") %>%
  filter(FECHA_ACTUALIZACION >= fecha_tendencia, FECHA_ACTUALIZACION <= "2020-05-21") %>%
  mutate(lag= as.numeric(FECHA_ACTUALIZACION -fecha_tendencia ))


covid_muertes_estimadas_obs <- 
  covid_muertes_estimadas %>%
  filter(tipo =="Registradas") %>%
  filter(modelo=="modelo 2") %>%
  filter(FECHA_ACTUALIZACION >= fecha_tendencia, FECHA_ACTUALIZACION <= "2020-05-21")  %>%
  mutate(lag= as.numeric(FECHA_ACTUALIZACION -fecha_tendencia ))

covid_fecha_def_max_change <-
  covid_fecha_def_max %>%
  filter(FECHA_DEF >= fecha_tendencia, FECHA_DEF <= "2020-05-21")  %>%
  mutate(lag= as.numeric(FECHA_DEF -fecha_tendencia ))
  

# Tendencias

# Casos estimados
summary(lm(log(n) ~ lag, data=covid_muertes_estimadas_change))

# Totales fecha de corte
summary(lm(log(n) ~ lag, data=covid_muertes_estimadas_obs))

# Ultima base
summary(lm(log(cumn) ~ lag, data=covid_fecha_def_max_change))



  
  
  
