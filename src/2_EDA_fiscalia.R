### EDA fiscalia ###

# Librerias ----
library(tidyverse)
library(lubridate)
library(ggpp)
library(ggtext)
library(ggx)
library(gganimate)
library(hcandersenr)
library(tidytext)
library(wordcloud2)

# Lectura ----
carp_clean <- read_csv("./data/3_final/carp_clean.csv") %>%
  filter(municipio == "AGS") %>%
  mutate(delito = ifelse(delito == "Violencia Familiar equiparada", "Violencia familiar", delito),
         across(starts_with("fecha"), ~ as_date(.x)),
         vinculacion_proceso = ifelse(is.na(fecha_proceso), 0, 1),
         sentencia = ifelse(is.na(fecha_sentencia), 0, 1))

# EDA ----

## Colores 
col_vioL_vioF <- c("Violación" = "#ff6666", "Violencia familiar" = "#6666ff")
col_proc_sent <- c("Proceso" = "#00c08a", "Sentencia" = "#ffb840")
col_anios <- c("2020" = "#0084ff", "2021" = "#ffc300", "2022" = "#fa3c4c")

## Formato
th <- theme(plot.title = element_text(size = 14, hjust = 0.5),
            plot.subtitle = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
)

th_col <- theme(plot.title = element_markdown(size = 14, hjust = 0.5),
                plot.subtitle = element_markdown(size = 12),
                axis.text.x = element_text(size = 12),
                axis.title.x = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                axis.title.y = element_text(size = 12),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 13),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()
)

## Total de carpetas por año y delito ----

p1_tot_carp_anio <- carp_clean %>%
  mutate(anio_carpeta = year(fecha),
         anio_carpeta = factor(anio_carpeta, levels = c(2020, 2021, 2022))) %>%
  group_by(anio_carpeta, delito) %>%
  summarise(freq = n()) %>%
  mutate(total = case_when(delito == "Violación" ~ sum(freq),
                           delito == "Violencia familiar" ~ sum(freq)),
         porc = freq/total) %>%
  ggplot(aes(anio_carpeta, freq, fill = anio_carpeta, label = freq)) +
  geom_col(width = 0.5) +
  geom_text(vjust = -0.3, 
            position = position_dodge(width = 0.5)) +
  scale_y_continuous(limits = c(0, 5655), breaks = scales::pretty_breaks(n = 5)) +
  scale_fill_manual(values = c("2020" = "#0084ff", "2021" = "#ffc300", "2022" = "#fa3c4c")) +
  facet_grid(~delito, scales = "free_x", space = "free_x") +
  labs(title = "Total de carpetas de investigación por año (2020 a 2022)",
       subtitle = glue::glue("Total de carpetas de violación: {sum(carp_clean$delito == 'Violación')}\n Total de carpetas de violencia familiar: {sum(carp_clean$delito == 'Violencia familiar')}"),
       x = "",
       y = "",
       caption = "Elaboración propia.") +
  theme_bw() + 
  theme(legend.position = "none") +
  th

ggsave(plot = p1_tot_carp_anio, "./figs/1_total_carpetas.png", width = 10, height = 6, units = "in", dpi = 600)

## Tendencias ----

### Violencia familiar
  
fit_violencia <- with(carp_clean %>%
              filter(delito == "Violencia familiar") %>%
              group_by(fecha) %>%
              count(delito), 
            ksmooth(fecha, n, kernel = "normal", bandwidth = 30))

p2_tend_viole <- carp_clean %>%
  filter(delito == "Violencia familiar") %>%
  group_by(fecha) %>%
  count(delito) %>%
  ungroup() %>%
  mutate(smooth = fit_violencia$y) %>%
  ggplot(aes(fecha, n)) +
  geom_point(size = 1, alpha = 0.3) + 
  geom_line(aes(fecha, smooth), color = "#6666ff", size = 1.2) + 
  scale_x_date(date_breaks = "2 month", labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  facet_grid(~delito) + 
  theme_bw() + 
  theme(legend.position = "none") +
  th +
  labs(title = "Tendencia mensual de carpetas de investigación (2020 a 2022)",
       subtitle = glue::glue("Total de carpetas de violencia familiar: {sum(carp_clean$delito == 'Violencia familiar')}"),
       x = "",
       y = "Cantidad de carpetas abiertas",
       caption = "Elaboración propia.")

ggsave(plot = p2_tend_viole, "./figs/2_tendencia_violencia.png", width = 10, height = 6, units = "in", dpi = 600)

### Violacion

fit_violacion <- with(carp_clean %>%
              filter(delito == "Violación") %>%
              group_by(fecha) %>%
              count(delito), 
            ksmooth(fecha, n, kernel = "normal", bandwidth = 30))

p3_tend_viola <- carp_clean %>%
  filter(delito == "Violación") %>%
  group_by(fecha) %>%
  count(delito) %>%
  ungroup() %>%
  mutate(smooth = fit_violacion$y) %>%
  ggplot(aes(fecha, n)) +
  geom_point(size = 1.5, alpha = 0.3) + 
  geom_line(aes(fecha, smooth), color = "#ad1439", size = 1.2) + 
  scale_x_date(date_breaks = "2 month", labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  facet_grid(~delito) + 
  theme_bw() + 
  theme(legend.position = "none") +
  th +
  labs(title = "Tendencia mensual de carpetas de investigación (2020 a 2022)",
       subtitle = glue::glue("Total de carpetas de violación: {sum(carp_clean$delito == 'Violación')}"),
       x = "",
       y = "Cantidad de carpetas abiertas",
       caption = "Elaboración propia.")

ggsave(plot = p3_tend_viola, "./figs/3_tendencia_violacion.png", width = 10, height = 6, units = "in", dpi = 600)
  
## NAs en fechas ----

col_proc_sent <- c("Proceso" = "#00c08a", "Sentencia" = "#ffb840")

### Fecha de proceso 

p4_tot_carp_proceso <- carp_clean %>% 
  mutate(NA_count_proceso = ifelse(is.na(fecha_proceso), "No vinculada a proceso", "Vinculada a proceso"),
         NA_proceso = ifelse(is.na(fecha_proceso), 1, 0),
         delito = ifelse(delito == "Violencia Familiar equiparada", "Violencia familiar", delito)) %>%
  group_by(delito, NA_count_proceso) %>%
  summarise(freq = n()) %>%
  mutate(total = case_when(delito == "Violación" ~ sum(freq),
                          delito == "Violencia familiar" ~ sum(freq)),
         porc = freq/total) %>%
  ggplot(aes(NA_count_proceso, porc, fill = NA_count_proceso, label = scales::percent(porc))) + 
  geom_col(width = 0.5) +
  geom_text(nudge_y = 0.03) + 
  scale_x_discrete(labels = scales::wrap_format(15)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 4)) +
  scale_fill_manual(values = c("No vinculada a proceso" = "#fa3c4c", "Vinculada a proceso" = "#0084ff")) +
  facet_wrap(vars(delito)) + 
  labs(title = "Carpetas de investigación <b><span style='color:#c08a00;'>vinculadas a proceso</span></b> (2020 a 2022)",
       subtitle = glue::glue("Total de carpetas de violación: {sum(carp_clean$delito == 'Violación')}\n Total de carpetas de violencia familiar: {sum(carp_clean$delito == 'Violencia familiar')}"),
       x = "",
       y = "",
       caption = "Elaboración propia.") +
  theme_bw() + 
  theme(plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, size = 14),
        legend.position = "none") +
  th_col

ggsave(plot = p4_tot_carp_proceso, "./figs/4_total_carpetas_vinculadas.png", width = 10, height = 6, units = "in", dpi = 600)

### Fecha de sentencia respecto al total de carpetas

p5_tot_carp_sentencia <- carp_clean %>% 
  mutate(NA_count_sentencia = ifelse(is.na(fecha_sentencia), "Sin sentencia", "Con sentencia"),
         NA_count_sentencia = factor(NA_count_sentencia, levels = c("Sin sentencia", "Con sentencia"))) %>%
  group_by(delito, NA_count_sentencia) %>%
  summarise(freq = n()) %>%
  mutate(total = case_when(delito == "Violación" ~ sum(freq),
                           delito == "Violencia familiar" ~ sum(freq)),
         porc = freq/total) %>%
  ggplot(aes(NA_count_sentencia, porc, fill = NA_count_sentencia, label = scales::percent(porc))) + 
  geom_col(width = 0.5) +
  geom_text(nudge_y = 0.03) + 
  scale_y_continuous(limits = c(0, 1.05), labels = scales::percent_format(), breaks = seq(0, 1, 0.1)) +
  scale_fill_manual(values = c("Sin sentencia" = "#fa3c4c", "Con sentencia" = "#0084ff")) +
  facet_wrap(vars(delito)) + 
  labs(title = "Carpetas de investigación con <b><span style='color:#8a00c0;'>sentencia</span></b> (2020 a 2022)",
       subtitle = glue::glue("Total de carpetas de violación: {sum(carp_clean$delito == 'Violación')}\n Total de carpetas de violencia familiar: {sum(carp_clean$delito == 'Violencia familiar')}"),
       x = "",
       y = "",
       caption = "Elaboración propia.") +
  theme_bw() + 
  theme(plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, size = 14),
        legend.position = "none") +
  th_col

ggsave(plot = p5_tot_carp_sentencia, "./figs/5_total_carpetas_sentencia_respectoTotal.png", width = 10, height = 6, units = "in", dpi = 600)

### Fecha de sentencia respecto al total de carpetas vinculadas

p5.1_tot_carp_sentencia <- carp_clean %>% 
  filter(!is.na(fecha_proceso)) %>%
  mutate(NA_count_sentencia = ifelse(is.na(fecha_sentencia), "Sin sentencia", "Con sentencia"),
         NA_count_sentencia = factor(NA_count_sentencia, levels = c("Sin sentencia", "Con sentencia"))) %>%
  group_by(delito, NA_count_sentencia) %>%
  summarise(freq = n()) %>%
  mutate(total = case_when(delito == "Violación" ~ sum(freq),
                           delito == "Violencia familiar" ~ sum(freq)),
         porc = freq/total) %>%
  ggplot(aes(NA_count_sentencia, porc, fill = NA_count_sentencia, label = scales::percent(porc))) + 
  geom_col(width = 0.5) +
  geom_text(nudge_y = 0.03) + 
  scale_y_continuous(limits = c(0, 1.05), labels = scales::percent_format(), breaks = seq(0, 1, 0.1)) +
  scale_fill_manual(values = c("Sin sentencia" = "#fa3c4c", "Con sentencia" = "#0084ff")) +
  facet_wrap(vars(delito)) + 
  labs(title = "Carpetas de investigación con <b><span style='color:#8a00c0;'>sentencia</span></b> (2020 a 2022)",
       subtitle = glue::glue("Total de carpetas de violación vinculadas: {sum(carp_clean$delito == 'Violación' & is.na(carp_clean$fecha_proceso) == FALSE)}\n Total de carpetas de violencia familiar vinculadas: {sum(carp_clean$delito == 'Violencia familiar' & is.na(carp_clean$fecha_proceso) == FALSE)}"),
       x = "",
       y = "",
       caption = "Elaboración propia.") +
  theme_bw() + 
  theme(plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, size = 14),
        legend.position = "none") +
  th_col

ggsave(plot = p5.1_tot_carp_sentencia, "./figs/5_1_total_carpetas_sentencia_respectoVinculadas.png", width = 10, height = 6, units = "in", dpi = 600)

## Tiempos entre fechas ----
col_proc_sent <- c("Abre" = "#008d65", "Proceso" = "#c08a00", "Sentencia" = "#8a00c0")

### Tiempo transcurrido para la vinculacion a proceso

p6_tiempo_abre_vinc <- carp_clean %>%
  mutate(anio_carpeta = year(fecha),
         anio_carpeta = factor(anio_carpeta, levels = c(2020, 2021, 2022)),
         NA_count_proceso = ifelse(is.na(fecha_proceso), "No vinculada a proceso", "Vinculada a proceso"),
         proceso_mismo_anio = case_when(tiempo_carp_proceso < 365 ~ "Menos de un año",
                                        tiempo_carp_proceso >= 365 & tiempo_carp_proceso < 730 ~ "1 a 2 años", 
                                        tiempo_carp_proceso >= 730 ~ "2 años o más"),
         proceso_mismo_anio = factor(proceso_mismo_anio, levels = c("Menos de un año", "1 a 2 años", "2 años o más"))) %>%
  group_by(anio_carpeta, delito, NA_count_proceso, proceso_mismo_anio) %>%
  summarise(freq = n()) %>%
  filter(!is.na(proceso_mismo_anio)) %>%
  mutate(total = case_when(delito == "Violación" ~ sum(freq),
                           delito == "Violencia familiar" ~ sum(freq)),
         porc = freq/total) %>%
  ggplot(aes(anio_carpeta, porc, fill = proceso_mismo_anio, label = scales::percent(porc))) + 
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) + 
  geom_text(position = position_dodge2nudge(width = 0.9, preserve = "single", y = 0.03)) +
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_fill_manual(name = "Tiempo transcurrido", values = c("Menos de un año" = "#8ac040", "1 a 2 años" = "#fcd008", "2 años o más" = "#fe864c")) + 
  facet_grid(~delito, scales = "free_x", space = "free_x") +
  labs(title = "<span style='font-size:14pt'>Tiempo transcurrido desde que se 
       <b><span style='color:#008d65;'>1) abre la carpeta</span></b> hasta que se
       <b><span style='color:#c08a00;'>2) vincula a proceso</span></b>
       </span>",
       subtitle = glue::glue("Total de carpetas de violación vinculadas: {sum(carp_clean$delito == 'Violación' & is.na(carp_clean$fecha_proceso) == FALSE)}\n Total de carpetas de violencia familiar vinculadas: {sum(carp_clean$delito == 'Violencia familiar' & is.na(carp_clean$fecha_proceso) == FALSE)}"),
       x = "Año en que fue abierta la carpeta de investigación",
       y = "",
       caption = "Elaboración propia.") +
  theme_bw() + 
  theme(plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, size = 14)) +
  th_col

ggsave(plot = p6_tiempo_abre_vinc, "./figs/6_tiempo_abre_vinculacion.png", width = 10, height = 6, units = "in", dpi = 600)

### Tiempo transcurrido para la sentencia desde que se vincula a proceso

p7_tiempo_vinc_sentencia <- carp_clean %>%
  mutate(anio_carpeta = year(fecha_proceso),
         anio_carpeta = factor(anio_carpeta, levels = c(2020, 2021, 2022)),
         NA_count_sentencia = ifelse(is.na(fecha_sentencia), "Sin sentencia", "Con sentencia"),
         NA_count_sentencia = factor(NA_count_sentencia, levels = c("Con sentencia", "Sin sentencia")),
         proceso_mismo_anio = case_when(tiempo_proceso_sentencia < 365 ~ "Menos de un año",
                                        tiempo_proceso_sentencia >= 365 & tiempo_proceso_sentencia < 730 ~ "1 a 2 años", 
                                        tiempo_proceso_sentencia >= 730 ~ "2 años o más"),
         proceso_mismo_anio = factor(proceso_mismo_anio, levels = c("Menos de un año", "1 a 2 años", "2 años o más"))) %>%
  group_by(anio_carpeta, delito, NA_count_sentencia, proceso_mismo_anio) %>%
  summarise(freq = n()) %>%
  filter(!is.na(proceso_mismo_anio)) %>%
  mutate(total = case_when(delito == "Violación" ~ sum(freq),
                           delito == "Violencia familiar" ~ sum(freq)),
         porc = freq/total) %>%
  ggplot(aes(anio_carpeta, porc, fill = proceso_mismo_anio, label = scales::percent(porc))) + 
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) + 
  geom_text(position = position_dodge2nudge(width = 0.9, preserve = "single", y = 0.03)) +
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_fill_manual(name = "Tiempo transcurrido", values = c("Menos de un año" = "#8ac040", "1 a 2 años" = "#fcd008", "2 años o más" = "#fe864c")) + 
  facet_grid(~delito, scales = "free_x", space = "free_x") +
  labs(title = "<span style='font-size:14pt'>Tiempo transcurrido desde que se 
       <b><span style='color:#c08a00;'>2) vincula a proceso</span></b> hasta que se da
       <b><span style='color:#8a00c0;'>3) sentencia</span></b>
       </span>",
       subtitle = glue::glue("Total de carpetas de violación con sentencia: {sum(carp_clean$delito == 'Violación' & is.na(carp_clean$fecha_sentencia) == FALSE)}\n Total de carpetas de violencia familiar con sentencia: {sum(carp_clean$delito == 'Violencia familiar' & is.na(carp_clean$fecha_sentencia) == FALSE)}"),
       x = "Año en que se vinculó a proceso la carpeta de investigación",
       y = "",
       caption = "Elaboración propia.") +
  theme_bw() + 
  theme(plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, size = 14)) +
  th_col

ggsave(plot = p7_tiempo_vinc_sentencia, "./figs/7_tiempo_vinculacion_sentencia.png", width = 10, height = 6, units = "in", dpi = 600)


### Tiempo transcurrido para la sentencia desde que se abre la carpeta

p8_tiempo_abre_sentencia <- carp_clean %>%
  mutate(anio_carpeta = year(fecha),
         anio_carpeta = factor(anio_carpeta, levels = c(2020, 2021, 2022)),
         NA_count_sentencia = ifelse(is.na(fecha_sentencia), "Sin sentencia", "Con sentencia"),
         NA_count_sentencia = factor(NA_count_sentencia, levels = c("Con sentencia", "Sin sentencia")),
         proceso_mismo_anio = case_when(tiempo_carp_sentencia < 365 ~ "Menos de un año",
                                        tiempo_carp_sentencia >= 365 & tiempo_carp_sentencia < 730 ~ "1 a 2 años", 
                                        tiempo_carp_sentencia >= 730 ~ "2 años o más"),
         proceso_mismo_anio = factor(proceso_mismo_anio, levels = c("Menos de un año", "1 a 2 años", "2 años o más"))) %>%
  group_by(anio_carpeta, delito, NA_count_sentencia, proceso_mismo_anio) %>%
  summarise(freq = n()) %>%
  filter(!is.na(proceso_mismo_anio)) %>%
  mutate(total = case_when(delito == "Violación" ~ sum(freq),
                           delito == "Violencia familiar" ~ sum(freq)),
         porc = freq/total) %>%
  ggplot(aes(anio_carpeta, porc, fill = proceso_mismo_anio, label = scales::percent(porc))) + 
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) + 
  geom_text(position = position_dodge2nudge(width = 0.9, preserve = "single", y = 0.03)) +
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_fill_manual(name = "Tiempo transcurrido", values = c("Menos de un año" = "#8ac040", "1 a 2 años" = "#fcd008", "2 años o más" = "#fe864c")) + 
  facet_grid(~delito, scales = "free_x", space = "free_x") +
  labs(title = "<span style='font-size:14pt'>Tiempo transcurrido desde que se 
       <b><span style='color:#008d65;'>1) abre la carpeta</span></b> hasta que se da
       <b><span style='color:#8a00c0;'>3) sentencia</span></b>
       </span>",
       subtitle = glue::glue("Total de carpetas de violación con sentencia: {sum(carp_clean$delito == 'Violación' & is.na(carp_clean$fecha_sentencia) == FALSE)}\n Total de carpetas de violencia familiar con sentencia: {sum(carp_clean$delito == 'Violencia familiar' & is.na(carp_clean$fecha_sentencia) == FALSE)}"),
       x = "Año en que fue abierta la carpeta de investigación",
       y = "",
       caption = "Elaboración propia.") +
  theme_bw() + 
  theme(plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, size = 14)) +
  th_col

ggsave(plot = p8_tiempo_abre_sentencia, "./figs/8_tiempo_abre_sentencia.png", width = 10, height = 6, units = "in", dpi = 600)

## Síntesis ----

# Lectura de stop words 
stop_words_esp <- read_csv("./data/1_raw/stop_words.csv")

# Palabras a tokens
sintesis_raw <- carp_clean %>%
  unnest_tokens(input = sintesis, output = word, token = "words") %>%
  select(id_carpeta, delito, fecha, fecha_proceso, fecha_sentencia, vinculacion_proceso, sentencia, word)
  

# Remover stop words
sintesis_clean <- sintesis_raw %>%
  filter(!(word %in% stop_words_esp$stop_words)) %>%
  filter(!grepl("[0-9]+|violencia|familiar|p.r|q.r.r|qrr|of|asi|pr|ademas|señala", word))

### Analisis violacion vinculadas ----

violacion_vinc <- sintesis_clean %>%
  filter(delito == "Violación" & vinculacion_proceso == 1) %>%
  count(word) %>%
  slice_max(n = 20, order_by = n, with_ties = "FALSE")

# Nube de palabras
wordcloud2(violacion_vinc, 
           color = colorRampPalette(colors=c("#6666ff", "#c1c1ff"))(nrow(violacion_vinc)), 
           backgroundColor = "black", size = 2, fontFamily = "Calibri", shape = "pentagon")

# Grafica
p9_violacion_vinc <- violacion_vinc %>%
  mutate(detenido = ifelse(word == "detenido", "#14ad88", "#ad1439")) %>%
  ggplot(aes(reorder(word, n), n, label = n, fill = detenido)) +
  geom_col(width = 0.7, color = "grey") +
  geom_text(vjust = 0.3, hjust = -0.2) +
  scale_y_continuous(limits = c(0, 150)) +
  scale_fill_identity() +
  coord_flip() +
  labs(title = "Palabras clave más repetidas en las síntesis de las carpetas de investigación",
       subtitle = "<b><span style='color:#ad1439;'>Violación</span></b> | <b><span style='color:#0084ff;'>Vinculadas</span></b>",
       x = "",
       y = "Repeticiones",
       caption = "Elaboración propia") +
  theme_bw() +
  theme() +
  th_col

ggsave(plot = p9_violacion_vinc, "./figs/9_palabras_violacion_vinc.png", width = 10, height = 6, units = "in", dpi = 600)
  
### Analisis violacion no vinculadas ----
violacion_NOvinc <- sintesis_clean %>%
  filter(delito == "Violación" & vinculacion_proceso == 0) %>%
  count(word) %>%
  slice_max(n = 20, order_by = n, with_ties = "FALSE")

# Nube de palabras
wordcloud2(violacion_NOvinc, 
           color = colorRampPalette(colors=c("#6666ff", "#c1c1ff"))(nrow(violacion_NOvinc)), 
           backgroundColor = "black", size = 10, fontFamily = "Calibri", shape = "pentagon")

# Grafica
p10_violacion_NOvinc <- violacion_NOvinc %>%
  mutate(detenido = ifelse(word == "detenido", "#14ad88", "#ad1439")) %>%
  ggplot(aes(reorder(word, n), n, label = n, fill = detenido)) +
  geom_col(width = 0.7, color = "grey") +
  geom_text(vjust = 0.3, hjust = -0.2) +
  scale_y_continuous(limits = c(0, 450)) +
  scale_fill_identity() +
  coord_flip() +
  labs(title = "Palabras clave más repetidas en las síntesis de las carpetas de investigación",
       subtitle = "<b><span style='color:#ad1439;'>Violación</span></b> | <b><span style='color:#ff6666;'>No vinculadas</span></b>",
       x = "",
       y = "Repeticiones",
       caption = "Elaboración propia") +
  theme_bw() +
  theme() +
  th_col

ggsave(plot = p10_violacion_NOvinc, "./figs/10_palabras_violacion_no_vinc.png", width = 10, height = 6, units = "in", dpi = 600)

### Analisis violencia familiar vinculadas ----
violencia_vinc <- sintesis_clean %>%
  filter(delito == "Violencia familiar" & vinculacion_proceso == 1) %>%
  count(word) %>%
  slice_max(n = 20, order_by = n, with_ties = "FALSE")

# Nube de palabras
wordcloud2(violencia_vinc, 
           color = colorRampPalette(colors=c("#6666ff", "#0084ff"))(nrow(violencia_vinc)), 
           minSize = "Violencia familiar | Vinculadas",
           backgroundColor = "black", size = 1, fontFamily = "Calibri", shape = "pentagon")

# Grafica
p11_violencia_vinc <- violencia_vinc %>%
  mutate(detenido = ifelse(word == "detenido", "#ffff66", "#6666ff")) %>%
  ggplot(aes(reorder(word, n), n, label = n, fill = detenido)) +
  geom_col(width = 0.7, color = "grey") +
  geom_text(vjust = 0.3, hjust = -0.2) +
  scale_y_continuous(limits = c(0, 300)) +
  scale_fill_identity() +
  coord_flip() +
  labs(title = "Palabras clave más repetidas en las síntesis de las carpetas de investigación",
       subtitle = "<b><span style='color:#6666ff;'>Violencia familiar</span></b> | <b><span style='color:#0084ff;'>Vinculadas</span></b>",
       x = "",
       y = "Repeticiones",
       caption = "Elaboración propia") +
  theme_bw() +
  theme() +
  th_col

ggsave(plot = p11_violencia_vinc, "./figs/11_palabras_violencia_vinc.png", width = 10, height = 6, units = "in", dpi = 600)

### Analisis violencia familiar no vinculadas ----
violencia_NOvinc <- sintesis_clean %>%
  filter(delito == "Violencia familiar" & vinculacion_proceso == 0) %>%
  count(word) %>%
  slice_max(n = 20, order_by = n, with_ties = "FALSE")

# Nube de palabras
wordcloud2(violencia_NOvinc, 
           color = colorRampPalette(colors = c("#ff6666", "#6666ff"))(nrow(violencia_NOvinc)), 
           backgroundColor = "black", size = 2, fontFamily = "Calibri", shape = "pentagon")

# Grafica
p12_violencia_NOvinc <- violencia_NOvinc %>%
  mutate(detenido = ifelse(word == "detenido", "#ffff66", "#6666ff")) %>%
  ggplot(aes(reorder(word, n), n, label = n, fill = detenido)) +
  geom_col(width = 0.7, color = "grey") +
  geom_text(vjust = 0.3, hjust = -0.2) +
  scale_y_continuous(limits = c(0, 4000)) +
  scale_fill_identity() +
  coord_flip() +
  labs(title = "Palabras clave más repetidas en las síntesis de las carpetas de investigación",
       subtitle = "<b><span style='color:#6666ff;'>Violencia familiar</span></b> | <b><span style='color:#ff6666;'>No vinculadas</span></b>",
       x = "",
       y = "Repeticiones",
       caption = "Elaboración propia") +
  theme_bw() +
  theme() +
  th_col

ggsave(plot = p12_violencia_NOvinc, "./figs/12_palabras_violencia_no_vinc.png", width = 10, height = 6, units = "in", dpi = 600)

