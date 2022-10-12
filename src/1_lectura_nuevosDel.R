### Lectura nuevos datos

# Librerías
library(magrittr)

# Lectura ----

# Datos nuevos

## fiscalia
carp_fisc_new_raw <- readxl::read_xlsx("./data/1_raw/ConsultaViolencias2017-2022 CIDE.xlsx") %>%
  janitor::clean_names()

## Poder judicial
judicial1_raw <- readxl::read_xls("./data/1_raw/RESUMEN_FINAL_V01 (1).xls") %>%
  janitor::clean_names()

judicial2_raw <- readxl::read_xls("./data/1_raw/RESUMEN_FINAL_V01 (2).xls") %>%
  janitor::clean_names()

# Datos viejos

## fiscalia
carp_fisc_old <- readr::read_csv("./data/3_final/carp_clean.csv")