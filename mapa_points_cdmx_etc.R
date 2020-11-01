#### 30 days maps challenge día 1 PUNTOS -----

Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica


# librerías

pacman::p_load(tidyverse,  ggrepel, ggspatial, sf,
               readxl, janitor, treemapify, gganimate,
               DT, ggthemes, viridis, ggalt, scales, extrafont)


###### DATOS -----

df <- st_read("~/Documents/Archivos/Luis/Documents/Mapp 2do semestre/Cursos M y Recursos R/Meet up Laura Herrera Datos de educación/mgm2014v6_2/mgm2015v6_2.shp")

etc <- read_csv("~/Documents/Archivos/Luis/Documents/Mapp 2do semestre/Cursos M y Recursos R/Meet up Laura Herrera Datos de educación/ETC_nacional.csv", 
                locale = locale(encoding = "ISO-8859-1"))

etc <- clean_names(etc)

snie <- read_csv("~/Documents/Archivos/Luis/Documents/Mapp 2do semestre/Cursos M y Recursos R/Meet up Laura Herrera Datos de educación/snie_16_17_inf.csv")

snie <- clean_names(snie)


##### Limpieza  ------#limpieza de bases de ETC

etc_cdmx <-
  etc %>% 
  select(-turno,  -cve_loc, -nombre_localidad) %>% 
  rename(cct = clave_cct) %>% 
  filter(cve_ent == "9")  %>%
  distinct(cct, .keep_all = TRUE)

  
  unique(snie$tipo_educativo)
  
  
  coor_df <-
    snie %>% 
    filter(clave_de_la_entidad_federativa == "09" & nombre_del_control_publico_o_privado == "PÚBLICO" &  tipo_educativo == "EDUCACIÓN BÁSICA") %>%
    select(cct = clave_del_centro_de_trabajo, 
           tot.alum = alumnos_total,
           x =ubicacion_de_la_escuela_localidad_al_oeste_del_meridiano_de_greenwich_expresada_en_grados,
           y= ubicacion_de_la_escuela_localidad_al_norte_del_ecuador_expresada_en_grados,
           imu = grado_de_marginacion_a_nivel_localidad) %>% 
    distinct(cct, .keep_all = TRUE) %>% #sirve para dejar los cct únicos
    right_join(etc_cdmx, by=c("cct"= "cct")) %>%  #unimos con la base de ETC
    mutate(cve_mun = formatC(cve_mun, width = 3, format = "d", flag = "00"), #completar las claves 
           year = sample(2007:2018, 1663, replace=T)) #creando una variable de año
  
  
  alum_cdmx <- snie %>% 
    clean_names() %>%  #janitor
    filter(clave_de_la_entidad_federativa == "09"  & nombre_del_control_publico_o_privado == "PÚBLICO" &   tipo_educativo == "EDUCACIÓN BÁSICA") %>%
    group_by(cve_mun = clave_del_municipio_o_delegacion) %>% 
    summarise(alum.cdmx = sum(alumnos_total, na.rm = T)) 
  
  
  
  
  concen.alum <- coor_df %>% 
    group_by(cve_mun, alcaldia = nombre_municipio) %>% 
    summarise(alum.etc = sum(tot.alum, na.rm = T))  %>% 
    left_join(alum_cdmx) %>% 
    mutate(pct.etc = (alum.etc/alum.cdmx),
           pct.label = paste(round(pct.etc*100,2), "%"))  
  
  
  basedf <- left_join(df, concen.alum)
  # comprime la base y los convierte en puntos
  basedf_points <- st_point_on_surface(df)
  
  # recuperar las coordenadas de los centroides del mapa
  basedf_coords <- as.data.frame(st_coordinates(basedf_points))
  basedf_coords$NAME <- df$NOM_MUN
  basedf_coords$porcentajes <- basedf$pct.label
  
  
  #### Mapa -----
  df <- 
    df %>% 
    filter(CVE_ENT == "09")  %>% 
    rename(cve_mun = "CVE_MUN")
  
  df %>% 
    ggplot() +
    geom_sf()
  
  base.esc <- 
    coor_df %>% 
    left_join(df)
    
  
  ggplot() +
    geom_sf(data = basedf, fill = "grey98", color = "grey50") +
    
    geom_point(data = base.esc,
               aes(x = x, y = y, color = servicio_alimentacion),
               size = 1, alpha = .7)+
    scale_color_manual(values = c("#0B465F", "#50F2B7"))+
    
    # geom_text_repel(data = basedf_coords, aes(X, Y, label = NAME), 
    #                 colour = "black", size = 2) +
    
    labs(title = "Programa Escuelas de Tiempo Completo", 
         subtitle = "Educación Básica en la CDMX || Datos a 2018",
         caption="Mapa elaborado por @1LuisDavid con datos de la SEP",
         x = " ",
         y = " ",
         color = "¿Cuenta con serivcio\nde alimentación?") +
    theme_void() +
  
    
    theme(plot.background = element_rect(fill = "grey80", colour = "grey80"),
          plot.title = element_text(face = "bold", size = 14, color = "black"),
          plot.subtitle = element_text(color = "black", size = 14),
          plot.caption = element_text(face = "italic", hjust = 1, size = 10, color = "black"),
          legend.title = element_text(color = "black"),
          legend.text = element_text(color = "black"),
          legend.position = c(.05, .05),
          legend.justification = c("left", "bottom"),
          legend.box.just = "left")+
    
    ggsave("Mapa puntos escuelas de tiempo completo.png", width = 8, height =10 , dpi = 200)
  
  
  
  base.esc %>% 
    group_by(servicio_alimentacion) %>% 
    count()
  
  sum(alum_cdmx$alum.cdmx)
  
  