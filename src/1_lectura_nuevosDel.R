### Lectura nuevos datos

# Librerías
library(magrittr)

# Lectura
del_raw <- readxl::read_xlsx("./data/1_raw/ConsultaViolencias2017-2022 CIDE.xlsx") %>%
  janitor::clean_names()

judicial1_raw <- readxl::read_xls("./data/1_raw/RESUMEN_FINAL_V01 (1).xls") %>%
  janitor::clean_names()

judicial2_raw <- readxl::read_xls("./data/1_raw/RESUMEN_FINAL_V01 (2).xls") %>%
  janitor::clean_names()
