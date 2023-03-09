### Graficos para presentacion

# Librerias
library(tidyverse)
library(RColorBrewer)

# Lectura 
calidad_agua <- readxl::read_excel("./data/1_raw/agua_limpia_data/Calidad del Agua Superficial 2012-2020.xlsx") %>%
  janitor::clean_names()

# Grafica de densidad de la variable de focalizacion ----
p <- prog_eval_clean %>% # Se verifica que hay continuidad
  ggplot(aes(im_2010)) +
  geom_density(col = "red") +
  labs(title = "Continuidad en la variable de focalización",
       x = "Z = Índice de marginación", 
       y = "",
       caption = "Elaboración propia.") +
  theme_light() +
  theme(title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )

ggsave(plot = p, "./figs/continuidad.png", width = 10, height = 6, units = "in", dpi = 600)

# 0) Calidad del agua ----
p_0 <- calidad_agua %>%
  filter(!is.na(calidad_coli_fec)) %>%
  group_by(calidad_coli_fec) %>%
  summarise(frec = n()) %>%
  mutate(prop = frec/sum(frec)) %>%
  ggplot(aes(prop, reorder(calidad_coli_fec, -prop), fill = calidad_coli_fec)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(labels = scales::percent_format(), breaks = scales::pretty_breaks(), limits = c(0, 1)) +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(title = "Calidad del agua del período 2012-2020", 
       subtitle = "Indicador Coliformes Fecales",
       x = "",
       y = "", 
       fill = "",
       caption = "Elaboración propia con datos de la Red Nacional de Medición de la Calidad del Agua.") +
  theme_light() +
  theme(title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )

ggsave(plot = p_0, "./figs/1_dist_grupos.png", width = 10, height = 6, units = "in", dpi = 600)

# 1) Umbral variable de participacion ----
prog_eval_clean %>%
  ggplot(aes(im_2010, D_i, color = as.factor(D_i))) +
  geom_point(alpha = 0.4, size = 2) +
  geom_vline(xintercept = -z, color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = c(-1.75, -1.32, -1.07, -0.81, 0.7, 3.92), limits = c(-2, 4)) +
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
  scale_color_discrete(labels = c("0 - Control", "1 - Tratamiento")) +
  labs(title = "Distribución de los grupos de control y tratamiento",
       subtitle = "n = 22,666",
       x = "Índice de marginalidad",
       y = "Participación en el tratamiento",
       caption = "Elaboración propia con datos de CONAPO y PROAGUA.",
       color = "Participación") +
  theme_light()  +
  theme(title = element_text(size = 14),
        axis.text.x = element_text(size = 12,angle = 90),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        )

  ggsave(plot = p_1, "./figs/1_dist_grupos.png", width = 10, height = 6, units = "in", dpi = 600)

# 2) Ventana 
prog_eval_clean %>%
  ggplot(aes(im_2010, D_i, color = as.factor(D_i))) +
  geom_point(alpha = 0.4, size = 2) +
  geom_vline(xintercept = -z, color = "red", linetype = "dashed") +
  geom_vline(xintercept = -z - h, color = "purple", size = 1) +
  geom_vline(xintercept = -z + h, color = "purple", size = 1) + 
  scale_x_continuous(breaks = c(-1.75, round(-z - h, 2), round(-z + h, 2), 3.92), limits = c(-2, 4)) +
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
  scale_color_discrete(labels = c("0 - Control", "1 - Tratamiento")) +
  labs(title = "Ventana cercana al umbral",
       subtitle = glue::glue("Criterio para establecer ventana: Z \u00B1 {round(h, 3)}, n = {nrow(prog_eval_tresh)}"),
       x = "Índice de marginalidad",
       y = "Participación en el tratamiento",
       caption = "Elaboración propia con datos de CONAPO y PROAGUA.",
       color = "Participación") +
  theme_light() +
  theme(title = element_text(size = 14),
        axis.text.x = element_text(size = 12,angle = 90, vjust = 0.2),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave(plot = p_2, "./figs/2_ventana.png", width = 10, height = 6, units = "in", dpi = 600)

# 3) Probabilidad de participacion

p_3 <- prog_eval_tresh %>%
  ggplot(aes(im_2010_centered, D_i)) +
  geom_point(color = "blue", alpha = 0.3, size = 2) +
  geom_smooth(aes(im_2010_centered, probs_2probit, color = as.factor(com_alto_muyAlto)), method = "lm", size = 1, se = FALSE) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  scale_x_continuous(breaks = c(-h, 0, h), limits = c(-h, h)) + 
  scale_color_manual(values = c("#EA4005", "#03CD00"), labels = c("0 - Control", "1 - Tratamiento")) +
  labs(title = "Probabilidad de participación",
       subtitle = glue::glue("n = {nrow(prog_eval_tresh)}"),
       x = "Índice de marginalidad centrado en el umbral (-0.81)",
       y = "P(D)",
       caption = "Elaboración propia con datos de CONAPO y PROAGUA.",
       color = "Participación") +
  theme_light() +
  theme(title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggsave(plot = p_3, "./figs/3_ppart.png", width = 10, height = 6, units = "in", dpi = 600)

# 4) Salto variable de resultados

p_4 <- prog_eval_tresh %>%
  ggplot(aes(im_2010_centered, gastro_tasa_2017)) +
  geom_point(color = "#0588E8", size = 2) +
  geom_smooth(aes(color = as.factor(com_alto_muyAlto)), method = "lm", size = 1, se = FALSE) +
  scale_color_manual(values = c("#EA4005", "#03CD00"), labels = c("0 - Control", "1 - Tratamiento")) +
  scale_x_continuous(breaks = c(-h, 0, h), limits = c(-h, h)) + 
  labs(title = "Efecto en la variable de resultados",
       subtitle = glue::glue("n = {nrow(prog_eval_tresh)}"),
       x = "Índice de marginalidad centrado en el umbral (-0.81)",
       y = "(Y) Tasa de mortalidad infantil en % para 2017",
       caption = "Elaboración propia con datos de CONAPO y la Secretaría de Salud.",
       color = "Participación") +
  theme_light() +
  theme(title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggsave(plot = p_4, "./figs/4_salto_y.png", width = 10, height = 6, units = "in", dpi = 600)

# 5) Salto variable de resultados 
p_5 <- prog_eval_tresh %>%
  filter(gastro_0 == 0) %>%
  ggplot(aes(im_2010_centered, gastro_tasa_2017)) +
  geom_point(color = "#0588E8", size = 2) +
  geom_smooth(aes(color = as.factor(com_alto_muyAlto)), method = "lm", size = 1, se = FALSE) +
  scale_color_manual(values = c("#EA4005", "#03CD00"), labels = c("0 - Control", "1 - Tratamiento")) +
  scale_x_continuous(breaks = c(-h, 0, h), limits = c(-h, h)) + 
  labs(title = "Efecto en la variable de resultados",
       subtitle = "Eliminando las localidades que no tuvieron muertes en 2016 y 2017",
       x = "Índice de marginalidad centrado en el umbral (-0.81)",
       y = "(Y) Tasa de mortalidad infantil en % para 2017",
       caption = "Elaboración propia con datos de CONAPO y la Secretaría de Salud.",
       color = "Participación") +
  theme_light() +
  theme(title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggsave(plot = p_5, "./figs/5_salto_y_filt.png", width = 10, height = 6, units = "in", dpi = 600)

# 6) Bootstrap
p_6 <- as.data.frame(bootstrap_t) %>%
  select(t_vals = 1) %>%
  ggplot(aes(t_vals)) +
  geom_density(col = "red") +
  scale_x_continuous(breaks = c(round(min(bootstrap_t), 2), round(mean(bootstrap_t), 2), round(max(bootstrap_t), 2))) +
  labs(title = "Variación del estimador del impacto",
       subtitle = "10,000 repeticiones Bootstrap",
x = "Valores del estimador",
y = "",
caption = "Elaboración propia.") +
  theme_light() +
  theme(title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave(plot = p_6, "./figs/6_bootstrap.png", width = 10, height = 6, units = "in", dpi = 600)


