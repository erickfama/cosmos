### Limpieza nuevos delitos

# Librerías ----
library(tidyverse)

# Lectura ----
source("src/1_lectura_nuevosDel.R", encoding = "UTF-8")

# Limpieza ----
jud3 <- judicial2_raw %>% 
  mutate(index_not_in_jud1 = judicial2_raw$indice %in% judicial1_raw$indice) %>%
  filter(index_not_in_jud1 != TRUE)
