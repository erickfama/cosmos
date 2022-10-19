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

## Tiempos ley ----
familiar_tley <- familiar %>%
  select(dias_presenta_intermedia, dias_presenta_concluyeJuicio) %>%
  mutate(`Presenta denuncia - Audiencia Intermedia` = as.factor(ifelse(dias_presenta_intermedia > 180, "Excede tiempo de ley", "No excede tiempo de ley")),
         `Presenta denuncia - Concluye Juicio` = as.factor(ifelse(dias_presenta_concluyeJuicio > 730, "Excede tiempo de ley", "No excede tiempo de ley")))

familiar_tley %>%
  select(c("Presenta denuncia - Audiencia Intermedia", "Presenta denuncia - Concluye Juicio")) %>%
  pivot_longer(cols = c("Presenta denuncia - Audiencia Intermedia", "Presenta denuncia - Concluye Juicio"), names_to = "grupo", values_to = "dummies") %>%
  filter(!is.na(dummies)) %>%
  group_by(grupo, dummies) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(dummies, prop, fill = dummies)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  scale_fill_manual(values = c("Excede tiempo de ley" = "#ffa500", "No excede tiempo de ley" = "#005aff"), name = "") + 
  facet_wrap(~grupo) +
  labs(title = "Violencia familiar",
       y = "",
       x = "",
       caption = glue::glue("Nota: De las {nrow(familiar)} denuncias, {nrow(familiar_presenta_intermedia)} llegan a intermedia y {nrow(familiar_concluye)} concluyen juicio.")) + 
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        text = element_text(size = 16))
  ggsave("./figs/EDA_judicial/familiar_Texcede.png", width = 11, height = 6, units = "in", dpi = 600)

# Violacion ----

violacion <- judicial_clean %>% filter(delito == "VIOLACIÓN")

## Estadisticas basicas ----

# Fecha mas antigua denuncia
min(violacion$fecha_presenta)

# Fecha mas reciente denuncia
max(violacion$fecha_presenta)

# Año de casos
violacion %>%
  ggplot(aes(x = year_presenta)) + 
  geom_bar(stat = "count") +
  scale_x_continuous(breaks = seq(2016, 2022)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  facet_wrap(~delito)

## Denuncia a abreviado ----
sum(!is.na(violacion$fecha_abreviado))
violacion_abreviado <- violacion %>%
  filter(!is.na(fecha_abreviado) & is.na(fecha_audiencia_intermedia))

### Dias que tardan en llegar a abreviado
summary(violacion_abreviado$dias_presenta_abreviado)

## Llegan a intermedia ----
violacion_presenta_intermedia <- violacion %>%
  filter(!is.na(fecha_audiencia_intermedia))

## Tiempo que tardan en llegar a intermedia
summary(violacion$dias_presenta_intermedia)

## Casos que se van a abreviado en intermedia
sum(!is.na(violacion_presenta_intermedia$fecha_abreviado))

## Llegan a apertura juicio ----
violacion_apertura_juicio <- violacion %>%
  filter(!is.na(fecha_apertura_juicio))

### Tiempo que tardan en llegar a apertura_juicio
summary(violacion_apertura_juicio$dias_intermedia_apertura_juicio)

### casos que se van a abreviado en apertura_juicio
sum(!is.na(violacion_apertura_juicio$fecha_abreviado))

## Llegan a juicio ----
violacion_juicio <- violacion %>%
  filter(!is.na(fecha_juicio))

### Tiempo que tardan en llegar a juicio
summary(violacion_juicio$dias_aperturaJuicio_juicio)

### casos que se van a abreviado en _juicio
sum(!is.na(violacion_juicio$fecha_abreviado))
 
## Concluye juicio ----
violacion_concluye <- violacion %>%
  filter(!is.na(fecha_concluye_juicio)) 

### Tiempo que tarda en concluir el juicio
summary(violacion_concluye$dias_juicio_concluyeJuicio)

## Apelaciones ----
violacion_apela <- violacion %>%
  filter(!is.na(fecha1))

### Tiempos ley ----
violacion_tley <- violacion %>%
  select(dias_presenta_intermedia, dias_presenta_concluyeJuicio) %>%
  mutate(`Presenta denuncia - Audiencia Intermedia` = as.factor(ifelse(dias_presenta_intermedia > 180, "Excede tiempo de ley", "No excede tiempo de ley")),
         `Presenta denuncia - Concluye Juicio` = as.factor(ifelse(dias_presenta_concluyeJuicio > 730, "Excede tiempo de ley", "No excede tiempo de ley")))

violacion_tley %>%
  select(c("Presenta denuncia - Audiencia Intermedia", "Presenta denuncia - Concluye Juicio")) %>%
  pivot_longer(cols = c("Presenta denuncia - Audiencia Intermedia", "Presenta denuncia - Concluye Juicio"), names_to = "grupo", values_to = "dummies") %>%
  filter(!is.na(dummies)) %>%
  group_by(grupo, dummies) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(dummies, prop, fill = dummies)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  scale_fill_manual(values = c("Excede tiempo de ley" = "#ff6666", "No excede tiempo de ley" = "#6666ff"), name = "") + 
  facet_wrap(~grupo) +
  labs(title = "Violación",
       y = "",
       x = "",
       caption = glue::glue("Nota: De las {nrow(violacion)} denuncias, {nrow(violacion_presenta_intermedia)} llegan a intermedia y {nrow(violacion_concluye)} concluyen juicio.")) + 
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        text = element_text(size = 16))
  ggsave("./figs/EDA_judicial/violacion_Texcede.png", width = 11, height = 6, units = "in", dpi = 600)

# Notas tiempos que dicta la ley entre fechas

## Presenta -  intermedia son de 1 a 6 meses (30-180 dias)

## Presenta - concluye juicio hasta 2 años (730 dias)