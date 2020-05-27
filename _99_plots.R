
# -------------------------------------------------------------------
# Numero de muertos por fecha de defuncion segun dia de reporte

covid %>% 
  filter(RESULTADO2 == "positivo", MUERTO==1) %>%
  group_by(FECHA_ACTUALIZACION,FECHA_DEF) %>%
  summarise(n=n()) %>%
  arrange(FECHA_ACTUALIZACION,FECHA_DEF) %>%
  filter(FECHA_ACTUALIZACION %in% as.Date(c("2020-05-07", "2020-05-26"))) %>%
  filter(FECHA_DEF >= "2020-03-01") %>%
  mutate(cumn = cumsum(n)) %>%
  ggplot() +
  geom_line(aes(FECHA_DEF, cumn, colour=as.factor(FECHA_ACTUALIZACION)), size=1.5) +
  geom_segment(aes(x = as.Date("2020-05-07"), y = 2961, xend = as.Date("2020-05-07"), yend = 4572), 
               arrow = arrow(ends = "both",length = unit(0.2, "cm"))) +
  geom_text(aes(x=as.Date("2020-05-15"), y=3745.5, label="1,611 registros"), size=5) +
  xlab("fecha de defunción") +
  ylab("fallecimientos acumulados") +
  theme_bw() +
  scale_fill_brewer(palette="Set1")+
  scale_colour_brewer(palette="Set1")+
  guides(colour=guide_legend(title="Fecha de corte")) +
  ggtitle("Número acumulado de muertes confirmadas por COVID-19") +
  theme(legend.position = c(.2,.7),
        axis.title= element_text(size=14),
        axis.text = element_text(size=13),
        legend.text = element_text(size = 13),
        legend.title =  element_text(size = 14)
        )

ggsave("def_acumuladas_2dates.png", width = 180, height = 180 * 2/3, units = "mm")



covid_cum_plots <-
  covid %>% 
  filter(RESULTADO2 == "positivo", MUERTO==1) %>%
  group_by(FECHA_ACTUALIZACION,FECHA_DEF) %>%
  summarise(n=n()) %>%
  arrange(FECHA_ACTUALIZACION,FECHA_DEF) %>%
  #filter(FECHA_ACTUALIZACION %in% as.Date(c("2020-05-07", "2020-05-24"))) %>%
  filter(FECHA_DEF >= "2020-03-01") %>%
  mutate(cumn = cumsum(n)) 

covid_cum_plots %>%
  ggplot() +
  geom_line(aes(FECHA_DEF, cumn,group=FECHA_ACTUALIZACION, colour=FECHA_ACTUALIZACION)) +
  geom_line(aes(FECHA_DEF, cumn), colour="red",subset(covid_cum_plots,FECHA_ACTUALIZACION == "2020-05-07"), size=1.2) +
  geom_line(aes(FECHA_DEF, cumn), colour="darkblue",subset(covid_cum_plots,FECHA_ACTUALIZACION == "2020-05-26"), size=1.2) +
  xlab("fecha de defunción") +
  ylab("fallecimientos acumulados") +
  theme_bw() +
  guides(colour= guide_colorbar(title="Fecha de corte")) +
  ggtitle("Número acumulado de muertes confirmadas por COVID-19") +
  theme(legend.position = c(.2,.7),
        axis.title= element_text(size=14),
        axis.text = element_text(size=13),
        legend.text = element_text(size = 13),
        legend.title =  element_text(size = 14)
  )

ggsave("def_acumuladas_alldates.png", width = 180, height = 180 * 2/3, units = "mm")


covid_muertes_estimadas %>%
  filter(modelo == "modelo 2") %>%
  ggplot() +
  geom_col(aes(FECHA_ACTUALIZACION, n, fill=tipo), alpha=.8) +
  geom_line(aes(FECHA_DEF, cumn, colour="black"), data=covid_fecha_def_max %>% filter(FECHA_DEF >= "2020-04-01"), size=1.6) +
  #geom_line(data = covid_muertes_estimadas %>% filter(tipo == "Registradas"), aes(FECHA_ACTUALIZACION, n), size=1, colour="blue") +
  #geom_line(data = covid_muertes_estimadas %>%filter(modelo == "modelo 2",FECHA_ACTUALIZACION<="2020-05-21") %>%group_by(FECHA_ACTUALIZACION) %>% summarise(n=sum(n)), aes(FECHA_ACTUALIZACION, n), size=1, colour="blue") +
  geom_errorbar(aes(FECHA_ACTUALIZACION, ymin=n-lowint, ymax=n+upint), data=intervalos %>% filter(modelo == "modelo 2"))+
  theme_bw() +
  scale_colour_brewer(palette="Set1") +
  scale_fill_brewer(name = "Número de", labels = c("subregistros estimados", "registros a la fecha de corte"),palette="Set1") +
  scale_colour_manual(name = "Corte 26 de mayo", labels=c(""), values="black") +
  xlab("fecha de corte") + ylab("defunciones acumuladas") +
  ggtitle("Subregistro de defunciones acumuladas en cada fecha de corte") +
  theme(legend.position = "bottom",
        axis.title= element_text(size=14),
        axis.text = element_text(size=13),
        legend.text = element_text(size = 11),
        legend.title =  element_text(size = 12)
  ) 
  

ggsave("def_predicted.png", width = 200, height = 200 * 2/3, units = "mm")


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





