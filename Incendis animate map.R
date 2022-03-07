## CARGA DE LIBRERIAS ----
pacman::p_load( tidyverse, lubridate, stringr, RSocrata, sf, rgeos, rgdal, raster, RColorBrewer, gganimate, transformr, ggdark)

## CARGA DATOS INCENDIOS CATALUNYA ----
incendis11_20 <- as_tibble(read.socrata("https://analisi.transparenciacatalunya.cat/resource/bks7-dkfd.json"))
incendis21 <- as_tibble(read.socrata("https://analisi.transparenciacatalunya.cat/resource/crs7-idxi.json"))
incendis22 <- as_tibble(read.socrata("https://analisi.transparenciacatalunya.cat/resource/9r29-e8ha.json"))

## UNIFICAMOS LOS NOMBRES DE LAS VARIABLES ----
incendis21 <- rename(incendis21, termemunic = terme_municipal, haarbrades = ha_arbrades, hanoarbrad = ha_no_arbrades,
       hanoforest = ha_no_forestals, haforestal = ha_forestals)

incendis <- bind_rows(incendis11_20, incendis21, incendis22)

incendis$codi_municipi <- substr(incendis$codi_municipi, 1, nchar(incendis$codi_municipi)-1)

## CARGA DATOS GEOGRÁFICOS ----
municipis_mapa <- readOGR("Municipis shapefiles/Municipios_IGN.shp")
municipis_mapa<-st_as_sf(municipis_mapa)
municipis_mapa <-municipis_mapa %>% filter(CODNUT2 == "ES51")

## ADECUACIÓN DE LOS DATOS ----
municipis_mapa2 <- inner_join(municipis_mapa, incendis, by=c("CODIGOINE" = "codi_municipi"))

municipis_mapa2 <- municipis_mapa2%>%
  mutate_at(vars(c(haarbrades, hanoarbrad, hanoforest, haforestal)), as.numeric)%>%
  mutate(data_incendi = as.Date(data_incendi))

municipis_mapa2$hacremades <- rowSums(data.frame(municipis_mapa2$hanoforest, municipis_mapa2$haforestal), na.rm=TRUE)

municipis_mapa3 <- municipis_mapa2%>%
  filter(hacremades > 0)%>%
  mutate(any = year(data_incendi))%>%
  mutate(mes = case_when(
    month(data_incendi) == 1 ~ "Gener",
    month(data_incendi) == 2 ~ "Febrer",
    month(data_incendi) == 3 ~ "Març",
    month(data_incendi) == 4 ~ "Abril",
    month(data_incendi) == 5 ~ "Maig",
    month(data_incendi) == 6 ~ "Juny",
    month(data_incendi) == 7 ~ "Juliol",
    month(data_incendi) == 8 ~ "Agost",
    month(data_incendi) == 9 ~ "Setembre",
    month(data_incendi) == 10 ~ "Octubre",
    month(data_incendi) == 11 ~ "Novembre",
    month(data_incendi) == 12 ~ "Desembre"
  ))%>%
  mutate(ha_fill = case_when(
    hacremades <= 1 ~ "1",
    hacremades > 1 & hacremades <= 10 ~"2",
    hacremades > 10 & hacremades <= 50 ~"3",
    hacremades > 50 & hacremades <= 100 ~"4",
    hacremades > 100 & hacremades <= 500 ~"5",
    hacremades > 500 & hacremades <= 1000 ~"6",
    hacremades > 1000 & hacremades <= 2000 ~"7",
    hacremades > 2000 & hacremades <= 5000 ~"8",
    hacremades > 5000 ~ "9"
  ))

mesos <- c("Gener", "Febrer", "Març", "Abril", "Maig", "Juny", "Juliol", "Agost", "Setembre", "Octubre", "Novembre", "Desembre")

municipis_mapa3$mes_any <- paste(municipis_mapa3$mes, municipis_mapa3$any)
municipis_mapa3 <- arrange(municipis_mapa3, factor(mes_any, 
                                                           levels = c(
                                                             paste(mesos,"2011"),
                                                             paste(mesos, "2012"),
                                                             paste(mesos, "2013"),
                                                             paste(mesos, "2014"),
                                                             paste(mesos, "2015"),
                                                             paste(mesos, "2016"),
                                                             paste(mesos, "2017"),
                                                             paste(mesos, "2018"),
                                                             paste(mesos, "2019"),
                                                             paste(mesos, "2020"),
                                                             paste(mesos, "2021"),
                                                             paste(mesos, "2022")
                                                           )))
municipis_mapa3$hacremades <- ifelse(municipis_mapa3$any == "2021", municipis_mapa3$hacremades/100, municipis_mapa3$hacremades)

## GIF MAPA INCENDIOS CATALUNYA 2011-2022 POR DÍA INICIO INCENDIO ----


gganimate::animate(
  ggplot(municipis_mapa) +
    geom_sf(fill = "black", color= "white")+
    geom_sf(data = municipis_mapa3, aes(fill = ha_fill))+
    scale_fill_brewer(palette = "YlOrRd", labels = c("<1", "1-10", "11-50", "51-100", "101-500", "501-1.000", "1.001-2.000","2.001-5.000", "5.000-11.000"))+
    dark_theme_void() +
    theme(
      plot.title = element_text(size = 22, hjust= 0.2),
      plot.subtitle = element_text(size = 15, hjust= 0.05),
      plot.caption = element_text(size = 9, hjust = 0.1, vjust= 2),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.position = c(0.8, 0.2),
      legend.key.size = unit(0.4, "cm"))+
    transition_time(data_incendi)+
    labs(title = "Incendis a Catalunya del 2011 a 2022",
       subtitle = "Dia d'inici de l'incendi: {frame_time}",
       caption = "Font: Dataset 'Incendis forestals a Catalunya. Anys 2011-2020' de Dades Obertes.",
       fill = "Ha cremades"),
  fps = 1)

## GIF MAPA INCENDIOS WRAPPED POR MES Y AÑO ----
municipis_mesos <- aggregate(hacremades ~termemunic + CODIGOINE + mes_any + ha_fill, municipis_mapa3, sum, na.rm = TRUE)
municipis_mesos2 <- inner_join(municipis_mapa, municipis_mesos, by=c("CODIGOINE" = "CODIGOINE"))

gganimate::animate(
  ggplot(municipis_mapa) +
    geom_sf(fill = "black", color= "white")+
    geom_sf(data = municipis_mesos2, aes(fill = ha_fill))+
    scale_fill_brewer(palette = "YlOrRd", labels = c("<1", "1-10", "11-50", "51-100", "101-500", "501-1.000", "1.001-2.000","2.001-5.000", "5.000-11.000"))+
    dark_theme_void() +
    theme(
      plot.title = element_text(size = 20, hjust= 0.2),
      plot.subtitle = element_text(size = 15, hjust= 0.06),
      plot.caption = element_text(size = 9, hjust = 0.1, vjust= 2),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.position = c(0.8, 0.2),
      legend.key.size = unit(0.4, "cm"))+
    transition_manual(frames = factor(mes_any, 
                             levels = c(
                               paste(mesos,"2011"),
                               paste(mesos, "2012"),
                               paste(mesos, "2013"),
                               paste(mesos, "2014"),
                               paste(mesos, "2015"),
                               paste(mesos, "2016"),
                               paste(mesos, "2017"),
                               paste(mesos, "2018"),
                               paste(mesos, "2019"),
                               paste(mesos, "2020"),
                               paste(mesos, "2021"),
                               paste("Gener 2022")
                             ))) +
    labs(title = "Incendis a Catalunya del 2011 al 2022",
         subtitle = "{current_frame}",
         caption = "Font: Datasets 'Incendis forestals a Catalunya' de Dades Obertes. @mireiacamacho75",
         fill = "Ha cremades (agregades)"),
  fps = 1, nframes = length(unique(municipis_mesos2$mes_any)), width = 500, height = 500)



## MAPA POR AÑOS WRAPPED ----

municipis4 <- aggregate(hacremades ~termemunic + CODIGOINE + any + ha_fill, municipis_mapa3, sum, na.rm = TRUE)
municipis4 <- municipis4%>% filter(any != "2022")
municipis_mapa4 <- inner_join(municipis_mapa, municipis4, by=c("CODIGOINE" = "CODIGOINE"))


ggplot(municipis_mapa) +
  geom_sf(fill = "black", color= "white", lwd = 0.03)+
  geom_sf(data = municipis_mapa4, aes(fill = ha_fill))+
  facet_wrap(~any)+
  scale_fill_brewer(palette = "YlOrRd", labels = c("<1", "1-10", "11-50", "51-100", "101-500", "501-1.000", "1.001-2.000","2.001-5.000", "5.000-11.000"))+
  dark_theme_void() +
  theme(
    plot.title = element_text(size = 30, hjust= 0.07),
    plot.subtitle = element_text(size = 15, hjust= 0.05, color="black"),
    plot.caption = element_text(size = 12, vjust= 2),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.position = c(0.85,0.2),
    legend.key.size = unit(0.5, "cm"),
    strip.text.x = element_text(size = 15, color = "white", face = "bold")
    )+
  labs(title = "Incendis a Catalunya del 2011 al 2021",
       subtitle = "Espai de marge",
       caption = "Font: Datasets 'Incendis forestals a Catalunya.' de Dades Obertes. @mireiacamacho75",
       fill = "Ha cremades (agr.)")

## MAPA 2021 POR MESES WRAPPED
municipis5 <- aggregate(hacremades ~termemunic + CODIGOINE + any + mes + ha_fill, municipis_mapa3, sum, na.rm = TRUE)
municipis5 <- municipis5%>% filter(any == "2021")
municipis_mapa2021 <- inner_join(municipis_mapa, municipis5, by=c("CODIGOINE" = "CODIGOINE"))


ggplot(municipis_mapa) +
  geom_sf(fill = "black", color= "white", lwd = 0.03)+
  geom_sf(data = municipis_mapa2021, aes(fill = ha_fill))+
  facet_wrap(~factor(mes,
                     levels = c("Gener", "Febrer", "Març", "Abril", "Maig",
                                "Juny", "Juliol", "Agost", "Setembre", "Octubre", "Novembre", "Desembre")))+
  scale_fill_brewer(palette = "YlOrRd", labels = c("<1", "1-10", "11-50", "51-100", "101-500", "501-1.000", "1.001-2.000","2.001-5.000", "5.000-11.000"))+
  dark_theme_void() +
  theme(
    plot.title = element_text(size = 30, hjust= 0.07),
    plot.subtitle = element_text(size = 15, hjust= 0.05, color="black"),
    plot.caption = element_text(size = 12, vjust= 2, hjust=0.1),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    #legend.position = c(0.85,0.2),
    legend.key.size = unit(0.5, "cm"),
    strip.text.x = element_text(size = 15, color = "white", face = "bold")
  )+
  labs(title = "Incendis a Catalunya el 2021 per mes",
       subtitle = "Espai de marge",
       caption = "Font: Datasets 'Incendis forestals a Catalunya. Any anterior' de Dades Obertes. @mireiacamacho75",
       fill = "Ha cremades (agr.)")
