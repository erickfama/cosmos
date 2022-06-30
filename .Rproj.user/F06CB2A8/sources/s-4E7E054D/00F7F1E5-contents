### Lectura ###

# librerias ----
library(readr)
library(janitor)
library(magrittr)
library(dplyr)

# Lectura ----

## Sistema tradicional ----
codigos_delitos <- c(1010, 2060, 3040, 4020, 5010, 7010)

conceptos_delitos <- read_csv("./data/1_raw/delitos/diccionario_de_datos_ta_delitos_ehriij2016.csv", skip = 6, col_names = TRUE, col_select = c(6:7), locale = locale(encoding = "latin1")) %>%
  clean_names() %>%
  select(cod_delito = "x0", concepto = "no_aplica") %>%
  filter(cod_delito %in% codigos_delitos) %>%
  mutate(concepto = str_extract(concepto, "\\-\\s[A-Za-z]*\\ó?[a-z]*\\s?[A-Za-z]+\\s?[A-Za-z]+\\s?[A-Za-z]+\\ó?[a-z]*"))

tradicional <- sapply(as.character(2015:2020), function(i){
base_path <- str_replace("./data/1_raw/delitos/sistema_tradicional/delit_ta_ehriij2016.csv", "[0-9]{4}", i)
tradicional <- data.frame(ubicageo = c(),
                          cod_delito = c(),
                          total_procesados = c(),
                          anio = c(),
                          sistema = c())
tradicional <- tradicional %>%
  rbind(read_csv(base_path) %>%
  clean_names() %>%
  select(ubicgeo = starts_with("ubic"), cod_delito = starts_with("delirie"), total_procesados = totalca1) %>%
  mutate(cod_delito = case_when(nchar(as.character(cod_delito)) == 3 ~ as.numeric(as.character(paste(cod_delito, "0", sep = ""))), 
                                nchar(as.character(cod_delito)) > 4 ~ as.numeric(substr(as.character(cod_delito), 1, 4)), 
                                TRUE ~ cod_delito),
         total_procesados = as.numeric(total_procesados)) %>%
  filter(ubicgeo == "01001" & cod_delito %in% codigos_delitos) %>%
  mutate(anio = str_extract(base_path, "[0-9]{4}"),
         sistema = "tradicional"))
}, simplify = FALSE) %>%
  bind_rows() %>%
  left_join(conceptos_delitos, by = "cod_delito")

## Sistema acusatorio oral. Juzgado de control ----
control <- sapply(as.character(2015:2020), function(i){
  base_path <- str_replace("./data/1_raw/delitos/sistema_ao_control/delit_jc_ehriij2015.csv", "[0-9]{4}", i)
  control <- data.frame(ubicageo = c(),
                            cod_delito = c(),
                            total_procesados = c(),
                            anio = c(),
                            sistema = c())
  control <- control %>%
    rbind(read_csv(base_path) %>%
            clean_names() %>%
            select(ubicgeo = starts_with("ubic"), cod_delito = starts_with("delirie"), total_procesados = totalca1) %>%
            mutate(cod_delito = case_when(nchar(as.character(cod_delito)) == 3 ~ as.numeric(as.character(paste(cod_delito, "0", sep = ""))), 
                                          nchar(as.character(cod_delito)) > 4 ~ as.numeric(substr(as.character(cod_delito), 1, 4)), 
                                          TRUE ~ cod_delito),
                   total_procesados = as.numeric(total_procesados)) %>%
            filter(ubicgeo == "01001" & cod_delito %in% codigos_delitos) %>%
            mutate(anio = str_extract(base_path, "[0-9]{4}"),
                   sistema = "control"))
}, simplify = FALSE) %>%
  bind_rows() %>%
  left_join(conceptos_delitos, by = "cod_delito")

## Sistema acusatorio oral. Juzgado de juicio oral ----
oral <- sapply(as.character(2015:2020), function(i){
  base_path <- str_replace("./data/1_raw/delitos/sistema_ao_oral/delit_jo_ehriij2015.csv", "[0-9]{4}", i)
  oral <- data.frame(ubicageo = c(),
                        cod_delito = c(),
                        total_procesados = c(),
                        anio = c(),
                        sistema = c())
  oral <- oral %>%
    rbind(read_csv(base_path) %>%
            clean_names() %>%
            select(ubicgeo = starts_with("ubic"), cod_delito = starts_with("delirie"), total_procesados = totalca1) %>%
            mutate(cod_delito = case_when(nchar(as.character(cod_delito)) == 3 ~ as.numeric(as.character(paste(cod_delito, "0", sep = ""))), 
                                          nchar(as.character(cod_delito)) > 4 ~ as.numeric(substr(as.character(cod_delito), 1, 4)), 
                                          TRUE ~ cod_delito),
                   total_procesados = as.numeric(total_procesados)) %>%
            filter(ubicgeo == "01001" & cod_delito %in% codigos_delitos) %>%
            mutate(anio = str_extract(base_path, "[0-9]{4}"),
                   sistema = "oral"))
}, simplify = FALSE) %>%
  bind_rows() %>%
  left_join(conceptos_delitos, by = "cod_delito")

# Escritura ----

## Sistema tradicional ----
write.csv(tradicional, "./data/2_interim/delitos_tradicional.csv")

## Sistema ao. Control ----
write.csv(control, "./data/2_interim/delitos_control.csv")

## Sistema ao. Oral ----
write.csv(oral, "./data/2_interim/delitos_oral.csv")