### EDA poder judicial 

# Librerias ----
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
# Lectura ----
judicial_clean <- read.csv("./data/3_final/judicial_clean.csv") %>%
  mutate(across(starts_with("fecha"), ~ ymd(.x)))

# Violencia familiar ----

familiar <- judicial_clean %>% filter(delito == "VIOLENCIA FAMILIAR")


# distribuciones fechas
judicial_clean %>%
  select(delito, where(is.Date)) %>%
  pivot_longer(cols = starts_with("fecha"), names_to = "columna", values_to = "fecha") %>%
  ggplot(aes(fecha)) + 
  geom_histogram() +
  facet_grid(columna ~ .) 

# Distribuciones tiempos

## Medias de cada grupo
dias_mean <- judicial_clean %>%
  select(starts_with("dias")) %>% 
  pivot_longer(cols = starts_with("dias"), names_to = "columna", values_to = "dias") %>%
  filter(!is.na(dias)) %>%
  group_by(columna) %>%
  summarise(dias_grp = mean(dias))

# Gráfica
judicial_clean %>%
  select(starts_with("dias")) %>%
  pivot_longer(cols = starts_with("dias"), names_to = "columna", values_to = "dias") %>%
  filter(!is.na(dias)) %>%
  ggplot(aes(dias)) + 
  geom_histogram() +
  geom_vline(data = dias_mean, aes(xintercept = dias_grp), col = "red") +
  facet_grid(columna ~ .)

summary(judicial2_clean %>% select(starts_with("dias")))
View(judicial_clean %>% select(starts_with("dias")) %>% names() %>% as.data.frame())

## Estadisticas basicas ----

# Fecha mas antigua denuncia
min(familiar$fecha_presenta)

# Fecha mas reciente denuncia
max(familiar$fecha_presenta)

# Año de casos
familiar %>%
  ggplot(aes(x = year_presenta)) + 
  geom_bar(stat = "count") +
  scale_x_continuous(breaks = seq(2016, 2022)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  facet_wrap(~delito)

## Denuncia a abreviado ----
sum(!is.na(familiar$fecha_abreviado))
familiar_abreviado <- familiar %>%
  filter(!is.na(fecha_abreviado) & is.na(fecha_audiencia_intermedia))

### Dias que tardan en llegar a abreviado
summary(familiar_abreviado$dias_presenta_abreviado)

## Llegan a intermedia ----
familiar_presenta_intermedia <- familiar %>%
  filter(!is.na(fecha_audiencia_intermedia))

## Tiempo que tardan en llegar a intermedia
summary(familiar$dias_presenta_intermedia)

## Casos que se van a abreviado en intermedia
sum(!is.na(familiar_presenta_intermedia$fecha_abreviado))

## Llegan a apertura juicio ----
familiar_apertura_juicio <- familiar %>%
  filter(!is.na(fecha_apertura_juicio))

### Tiempo que tardan en llegar a apertura_juicio
summary(familiar_apertura_juicio$dias_intermedia_apertura_juicio)

### casos que se van a abreviado en apertura_juicio
sum(!is.na(familiar_apertura_juicio$fecha_abreviado))

## Llegan a juicio ----
familiar_juicio <- familiar %>%
  filter(!is.na(fecha_juicio))

### Tiempo que tardan en llegar a juicio
summary(familiar_juicio$dias_aperturaJuicio_juicio)

### casos que se van a abreviado en _juicio
sum(!is.na(familiar_juicio$fecha_abreviado))
 
## Concluye juicio ----
familiar_concluye <- familiar %>%
  filter(!is.na(fecha_concluye_juicio)) 

### Tiempo que tarda en concluir el juicio
summary(familiar_concluye$dias_juicio_concluyeJuicio)

## Apelaciones ----
familiar_apela <- familiar %>%
  filter(!is.na(fecha1))

