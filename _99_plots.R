
# -------------------------------------------------------------------
# Numero de muertos por fecha de defuncion segun dia de reporte

covid %>% 
  filter(RESULTADO2 == "positivo", MUERTO==1) %>%
  group_by(FECHA_ACTUALIZACION,FECHA_DEF) %>%
  summarise(n=n()) %>%
  arrange(FECHA_ACTUALIZACION,FECHA_DEF) %>%
  filter(FECHA_ACTUALIZACION %in% as.Date(c("2020-05-07", "2020-05-24"))) %>%
  filter(FECHA_DEF >= "2020-03-01") %>%
  mutate(cumn = cumsum(n)) %>%
  ggplot() +
  geom_line(aes(FECHA_DEF, cumn, colour=as.factor(FECHA_ACTUALIZACION)), size=1) +
  geom_segment(aes(x = as.Date("2020-05-07"), y = 2961, xend = as.Date("2020-05-07"), yend = 4530), 
               arrow = arrow(ends = "both",length = unit(0.2, "cm"))) +
  geom_text(aes(x=as.Date("2020-05-13"), y=3745.5, label="1,569 registros")) +
  xlab("fecha de defunción") +
  ylab("número") +
  theme_bw() +
  scale_fill_brewer(palette="Set1")+
  scale_colour_brewer(palette="Set1")+
  guides(colour=guide_legend(title="Fecha de corte")) +
  ggtitle("Número acumulado de muertes confirmadas por COVID-19") +
  theme(legend.position = c(.2,.7))

ggsave("def_acumuladas_2dates.png", width = 180, height = 180 * 2/3, units = "mm")



covid %>% 
  filter(RESULTADO2 == "positivo", MUERTO==1) %>%
  group_by(FECHA_ACTUALIZACION,FECHA_DEF) %>%
  summarise(n=n()) %>%
  arrange(FECHA_ACTUALIZACION,FECHA_DEF) %>%
  #filter(FECHA_ACTUALIZACION %in% as.Date(c("2020-05-07", "2020-05-24"))) %>%
  filter(FECHA_DEF >= "2020-03-01") %>%
  mutate(cumn = cumsum(n)) %>%
  ggplot() +
  geom_line(aes(FECHA_DEF, cumn,group=FECHA_ACTUALIZACION, colour=FECHA_ACTUALIZACION)) +
  xlab("fecha") +
  ylab("número") +
  theme_bw() +
  guides(colour= guide_colorbar(title="Fecha de corte")) +
  ggtitle("Número acumulado de muertes confirmadas por COVID-19") +
  theme(legend.position = c(.2,.7))

ggsave("def_acumuladas_alldates.png", width = 180, height = 180 * 2/3, units = "mm")


covid_muertes_estimadas %>%
  filter(modelo == "modelo 2") %>%
  ggplot() +
  geom_col(aes(FECHA_ACTUALIZACION, n, fill=tipo), alpha=.8) +
  geom_line(aes(FECHA_DEF, cumn, colour="black"), data=covid_fecha_def_max %>% filter(FECHA_DEF >= "2020-04-01"), size=1) +
  #geom_line(data = covid_muertes_estimadas %>% filter(tipo == "Registradas"), aes(FECHA_ACTUALIZACION, n), size=1, colour="blue") +
  #geom_line(data = covid_muertes_estimadas %>%filter(modelo == "modelo 2",FECHA_ACTUALIZACION<="2020-05-21") %>%group_by(FECHA_ACTUALIZACION) %>% summarise(n=sum(n)), aes(FECHA_ACTUALIZACION, n), size=1, colour="blue") +
  geom_errorbar(aes(FECHA_ACTUALIZACION, ymin=n-lowint, ymax=n+upint), data=intervalos %>% filter(modelo == "modelo 2"))+
  theme_bw() +
  scale_colour_brewer(palette="Set1") +
  scale_fill_brewer(name = "Número de", labels = c("subregistros estimados", "registros a la fecha de corte"),palette="Set1") +
  scale_colour_manual(name = "Base del 24 de mayo", labels=c(""), values="black") +
  xlab("fecha") + ylab("número de defunciones") +
  ggtitle("Subregistro de defunciones por COVID-19 en cada fecha de corte") +
  theme(legend.position = "bottom") 
  

ggsave("def_predicted.png", width = 180, height = 180 * 2/3, units = "mm")


covid_muertes_estimadas %>%
  filter(modelo == "modelo 2") %>%
  ggplot() +
  geom_col(aes(FECHA_ACTUALIZACION, n, fill=tipo), alpha=.2) +
  geom_line(aes(FECHA_DEF, cumn, colour="black"), data=covid_fecha_def_max %>% filter(FECHA_DEF >= "2020-04-01"), size=1) +
  geom_line(data = covid_muertes_estimadas %>% filter(tipo == "Registradas"), aes(FECHA_ACTUALIZACION, n), size=1, colour="darkblue") +
  geom_line(data = covid_muertes_estimadas %>%filter(modelo == "modelo 2",FECHA_ACTUALIZACION<="2020-05-21") %>%group_by(FECHA_ACTUALIZACION) %>% summarise(n=sum(n)), 
            aes(FECHA_ACTUALIZACION, n), size=1, colour="darkred") +
  geom_errorbar(aes(FECHA_ACTUALIZACION, ymin=n-lowint, ymax=n+upint), data=intervalos %>% filter(modelo == "modelo 2"))+
  theme_bw() +
  scale_colour_brewer(palette="Set1") +
  scale_fill_brewer(name = "Número de", labels = c("subregistros estimados", "registros a la fecha de corte"),palette="Set1") +
  scale_colour_manual(name = "Acumulado al 24 de mayo", labels=c(""), values="black") +
  xlab("fecha") + ylab("número de defunciones") +
  ggtitle("Subregistro en el número de defunciones por COVID-19 en cada fecha de corte") +
  theme(legend.position = "bottom") 





