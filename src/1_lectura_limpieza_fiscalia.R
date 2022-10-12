### Lectura y limpieza ###

# Librerías ----
library(tidyverse)
library(lubridate)

# Lectura

carp_raw <- readxl::read_xls("./data/1_raw/CARPETAS VIOLACION Y VIOLENCIA FAMILIAR 2021 Y 2022.xls") %>%
  janitor::clean_names()
anexo_raw <- readxl::read_xlsx("./data/1_raw/ANEXO VINCULACIONES U ESP DSJFYG.xlsx") %>%
  janitor::clean_names()

# Limpieza ----

# NAs por variable (datos crudos)
carp_raw %>%
  summarise_all(~ sum(is.na(.)))

# Nuevas variables
carp_clean <- carp_raw %>%
  mutate(id_carpeta = str_extract(carpeta, "(?<=/)([0-9]+)(?=/)"), # 1) identificador de carpeta
         municipio = str_extract(carpeta, "(?<=CI/)(.*)(?=/[0-9]+/[0-9]+-[0-9]+)"), # 2) Se identifica por municipio
         id_exp_jud = str_extract(exp_jud, "^[0-9]+"), # 3) Identificador del expediente judicial
         across(c(fecha, fecha_proceso),  ~ ymd(.x)), # 4) Formato de fecha 
         fecha_sentencia = dmy(fecha_sentencia),
         tiempo_carp_proceso = difftime(fecha_proceso, fecha, units = "days"),
         tiempo_proceso_sentencia = difftime(fecha_sentencia, fecha_proceso, units = "days"),
         tiempo_carp_sentencia = difftime(fecha_sentencia, fecha, units = "days")) %>%
  arrange(id_carpeta) %>%
  select(id_carpeta, everything())

# Repeticiones de carpetas
carp_reps <- carp_clean %>%
  group_by(id_carpeta) %>%
  summarise(repeticiones = n())

sum(carp_reps$repeticiones > 1) # 627 carpetas están repetidas porque se integran por diferentes tipos de denuncias o razones

# Se agrega el número de repeticiones para poder filtrar 
carp_clean <- left_join(carp_clean, carp_reps, by = "id_carpeta")

# Variable para identificar las carpetas que están repetidas porque son casos de violación y violencia familiar
carp_violacion_violencia <- carp_clean %>% 
  group_by(id_carpeta) %>%
  summarise(violacion = ifelse(sum(delito == "Violación") > 0, 1, 0),
            violencia_familiar = ifelse(sum(delito == "Violencia familiar") > 0, 1, 0),
            violacion_violencia = ifelse((violacion + violencia_familiar) == 2, 1, 0))

carp_clean <- left_join(carp_clean, carp_violacion_violencia, by = "id_carpeta")


rm(carp_reps, carp_violacion_violencia, carp_raw)

# Escritura 
if(file.exists("./data/3_final/carp_clean.csv") == FALSE){
  write_csv(carp_clean, "./data/3_final/carp_clean.csv")
}
