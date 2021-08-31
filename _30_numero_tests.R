library(tidyverse)
library(vroom)



tests_incrementalDB <- function(folder, last_db=NULL) {
  
  if (!is.null(last_db)) {
    covid_m <- readRDS(last_db)
  } else {
    covid_m <- tibble()
  }
  for(f in list.files(folder,include.dirs = FALSE)) {
    print(f)
    fd <- vroom(paste(folder, f, sep="/"), na=c("9999-99-99"), col_types=cols(PAIS_ORIGEN=col_character(), FECHA_DEF=col_date())) 
    
    
    fd <- 
      fd %>% 
      group_by(FECHA_ACTUALIZACION, ENTIDAD_RES, CLASIFICACION_FINAL) %>% 
      summarise(n=n()) %>% group_by()
      
    covid_m <- bind_rows(covid_m, fd)
    
  }
  saveRDS(covid_m, file=paste("datos/covid_tests_incremental_",max(fd$FECHA_ACTUALIZACION),".rds", sep=""))
  return(covid_m)
}



#folder <- '/Users/humberto/other_projects/covid_mex/datos_csv/temp'
#test <- tests_incrementalDB(folder, "./datos/covid_tests_incremental_2021-02-07.rds")
table(test$CLASIFICACION_FINAL)

test %>% 
  filter(CLASIFICACION_FINAL %in% c(3,7)) %>%
  group_by(FECHA_ACTUALIZACION, CLASIFICACION_FINAL) %>%
  summarise(n=sum(n)) %>%
  group_by(CLASIFICACION_FINAL) %>%
  mutate(diff = n - lag(n, default = 0)) %>%
  filter(!(FECHA_ACTUALIZACION %in% as.Date(c("2020-10-07","2021-01-07","2021-01-08", "2021-01-09" )))) %>%
  ggplot(aes(FECHA_ACTUALIZACION, diff)) +
  geom_line(aes(colour=as.factor(CLASIFICACION_FINAL)))


test_nacional <- 
  test %>% 
  filter(CLASIFICACION_FINAL %in% c(3,7)) %>%
  group_by(FECHA_ACTUALIZACION) %>%
  summarise(n=sum(n)) %>%
  mutate(diff = n - lag(n, default = 0)) %>%
  filter(!(FECHA_ACTUALIZACION %in% as.Date(c("2020-10-07","2021-01-07","2021-01-08", "2021-01-09" ))))


test_nacional_pos <- 
  test %>% 
  filter(CLASIFICACION_FINAL %in% c(3)) %>%
  group_by(FECHA_ACTUALIZACION) %>%
  summarise(n=sum(n)) %>%
  mutate(diff = n - lag(n, default = 0)) %>%
  filter(!(FECHA_ACTUALIZACION %in% as.Date(c("2020-10-07","2021-01-07","2021-01-08", "2021-01-09" ))))


test_nacional <- 
  test_nacional %>%
  left_join(test_nacional_pos, by="FECHA_ACTUALIZACION") %>%
  mutate(pos_rate=diff.y / diff.x)


test_nacional %>%
  ggplot(aes(FECHA_ACTUALIZACION, diff.x)) +
  geom_line() +
  geom_smooth()


test_nacional %>%
  ggplot(aes(FECHA_ACTUALIZACION, pos_rate)) +
  geom_line() +
  geom_smooth()



# Por entidad
test_entidad <- 
  test %>% 
  filter(CLASIFICACION_FINAL %in% c(3,7)) %>%
  group_by(FECHA_ACTUALIZACION, ENTIDAD_RES) %>%
  summarise(n=sum(n)) %>%
  group_by() %>%
  group_by(ENTIDAD_RES) %>%
  mutate(diff = n - lag(n, default = 0)) %>%
  filter(!(FECHA_ACTUALIZACION %in% as.Date(c("2020-10-07","2021-01-07","2021-01-08", "2021-01-09" ))))
  

test_entidad %>%
  ggplot() +
  geom_line(aes(FECHA_ACTUALIZACION, diff, colour=ENTIDAD_RES))

  
  
# df %>%
#   group_by(farm) %>%
#   mutate(volume = cumVol - lag(cumVol, default = 0))
