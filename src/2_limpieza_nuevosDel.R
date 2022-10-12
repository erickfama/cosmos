### Limpieza nuevos delitos

# Librerías ----
library(tidyverse)

# Lectura ----
source("src/1_lectura_nuevosDel.R", encoding = "UTF-8")

# Limpieza ----

# Casos de judicial2 que no se encuentran en judicial1
jud3 <- judicial2_raw %>% 
  mutate(index_not_in_jud1 = judicial2_raw$indice %in% judicial1_raw$indice) %>%
  filter(index_not_in_jud1 != TRUE) # Al examinar se encuentra que judicial 1 no contiene del id 1200 al 1281, por lo tanto, se trabajara solo con judicial 2.

# Se elimina judicial1
rm(judicial1_raw, jud3)

# Limpieza de datos de poder judicial
judicial2_clean <- judicial2_raw %>%
  filter(partido == 1) %>% # Partido 1 es Oralidad Aguascalientes, los demas son de otros municipios
  mutate(year_presenta = lubridate::year(fecha_presenta),
         dias_presenta_intermedia = difftime(fecha_audiencia_intermedia, fecha_presenta, units = "days"),
         dias_presenta_abreviado = difftime(fecha_abreviado, fecha_presenta, units = "days"),
         intermedia_abreviado = ifelse(ceiling(dias_presenta_intermedia) == ceiling(dias_presenta_abreviado), 1, 0), # Se redondea hacia arriba con cualquier decimal para evitar incongruencias por las horas
         intermedia_apertura_juicio = difftime(fecha_apertura_juicio, fecha_audiencia_intermedia, units = "days"),
         aperturaJuicio_juicio = difftime(fecha_juicio, fecha_apertura_juicio, units = "days"),
         juicio_concluyeJuicio = difftime(fecha_concluye_juicio, fecha_juicio, units = "days"),
         concluyeJuicio_fecha1 = difftime(fecha1, fecha_concluye_juicio, units = "days"), # Revisar diferencia negativas en esta fecha
         fecha1_fecha2 = difftime(fecha2, fecha1, units = "days"), # Consultar significado fecha1, fecha2 y fecha3
         fecha2_fecha3 = difftime(fecha3, fecha2, units = "days"))

