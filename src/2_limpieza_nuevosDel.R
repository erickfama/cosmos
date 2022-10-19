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

# Numero de delitos por tipo
table(judicial2_raw %>% filter(partido == 1) %>% select(delito))

# Limpieza de datos de poder judicial
judicial2_clean <- judicial2_raw %>%
  filter(partido == 1 & # Partido 1 es Oralidad Aguascalientes, los demas son de otros municipios
         delito  %in% c("VIOLACIÓN", "VIOLACIÓN EQUIPARADA", "VIOLENCIA FAMILIAR")) %>% 
  mutate(across(starts_with("fecha_"), ~ lubridate::ymd(str_remove(.x, "\\s\\d+:\\d+:\\d+")))) %>% # Se eliminan las horas para evitar problemas de diferencias negativas por variaciones en minutos
  mutate(year_presenta = lubridate::year(fecha_presenta),
         dias_presenta_intermedia = difftime(fecha_audiencia_intermedia, fecha_presenta, units = "days"),
         dias_presenta_abreviado = difftime(fecha_abreviado, fecha_presenta, units = "days"),
         dias_intermedia_abreviado = ifelse(dias_presenta_intermedia == dias_presenta_abreviado, 1, 0),
         dias_intermedia_apertura_juicio = difftime(fecha_apertura_juicio, fecha_audiencia_intermedia, units = "days"),
         dias_aperturaJuicio_juicio = difftime(fecha_juicio, fecha_apertura_juicio, units = "days"),
         dias_juicio_concluyeJuicio = difftime(fecha_concluye_juicio, fecha_juicio, units = "days"),
         dias_presenta_apela1 = ifelse(fecha1 >= fecha_presenta & fecha1 < fecha_audiencia_intermedia, difftime(fecha1, fecha_presenta), NA),
         dias_intermedia_apela1 = ifelse(fecha1 >= fecha_audiencia_intermedia & fecha1 < fecha_abreviado, difftime(fecha1, fecha_audiencia_intermedia), NA),
         dias_abreviado_apela1 = ifelse(fecha1 >= fecha_abreviado & fecha1 < fecha_apertura_juicio, difftime(fecha1, fecha_abreviado), NA),
         dias_aperturaJuicio_apela1 = ifelse(fecha1 >= fecha_apertura_juicio & fecha1 < fecha_juicio, difftime(fecha1, fecha_apertura_juicio), NA),
         dias_juicio_apela1 = ifelse(fecha1 >= fecha_juicio & fecha1 < fecha_concluye_juicio, difftime(fecha1, fecha_juicio), NA),
         dias_concluyeJuicio_apela1 = ifelse(fecha1 >= fecha_concluye_juicio, difftime(fecha1, fecha_concluye_juicio, units = "days"), NA), # Revisar diferencia negativas en esta fecha
         dias_apela1_apela2 = difftime(fecha2, fecha1, units = "days"), # Consultar significado fecha1, fecha2 y fecha3
         dias_apela2_apela3 = difftime(fecha3, fecha2, units = "days"),
         dias_presenta_concluyeJuicio = difftime(fecha_concluye_juicio, fecha_presenta, units = "days"))

# Definición de violación ----
# Abuso sexual - Es agresión sexual
# Violación equiparada - Sometimiento con mas de un actor
# Violacion - Sometimiento

# Solo acotar a violacion y violencia familiar

# Recursos de apelacion o revocacion ----
# Apelacion: 11 casos
# Revocacion: 3 casos

# Escritura ----
if(file.exists("./data/3_final/judicial_clean.csv") == FALSE){
  write.csv(judicial2_clean, "./data/3_final/judicial_clean.csv", row.names = FALSE)
}
