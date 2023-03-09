################################
### Aguascalientes ###
################################

library(tidyverse)
library(lubridate)
library(extrafont)
library(ggx)
library(gganimate)


# library(archive)

# Data from SEGOB

dl <- tempfile(fileext = ".zip")
download.file("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", dl)
df <- read.csv(unzip(dl), header = T)

df <- df %>%
  select(-ORIGEN, -SECTOR, -ENTIDAD_UM, -ENTIDAD_NAC, -HABLA_LENGUA_INDIG, -UCI) %>%
  janitor::clean_names()

df$FECHA_SINTOMAS <- as.Date(df$FECHA_SINTOMAS, format = "%Y-%m-%d")

# Plot AGS

ags <- df %>%
  filter(entidad_res == 1) %>%
  janitor::clean_names()

p <- ags %>%
  filter(clasificacion_final == 3) %>%
  group_by(fecha_ingreso) %>%
  count(clasificacion_final) %>%
  ggplot(aes(x = as_date(fecha_ingreso), y = n, color = n, fill = n)) +
  geom_col() +
  # geom_smooth(se = F, colour = "#7b4397", alpha = 1) +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(breaks = seq(0, 400, 50)) +
  scale_color_gradient(low = "#ffc371", high = "#dc2430") +
  scale_fill_gradient(low = "#ffc371", high = "#dc2430") +
  labs(title = "Casos positivos SARS-COV-2 en Aguascalientes",
       subtitle = "Total de casos confirmados: {frame_time}",
       x = "", y = "Número de casos",
       caption = glue::glue("Fecha de actualización: {unique(ags$fecha_actualizacion)}\nElaborado por Erick Fajardo")) +
  ggthemes::theme_few() +
  theme(text = element_text(family = "Consolas", colour = "white"),
        axis.text = element_text(colour = "#adadad"),
        panel.background = element_rect(fill = '#262626',
                                        colour = '#262626',
                                        size = 0.5, linetype = "solid"),
        plot.background = element_rect(colour = NA, fill = '#262626'),
        plot.title = element_text(face = "bold", colour = "white", size = 14), 
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 11),
        legend.position = "none") +
  transition_time(cumsum(n)) +
  shadow_mark(past = T, future = F, alpha = 0.5)
  
animate(p, nframes = 120, renderer = gifski_renderer("./gifs/animate_test.gif"),
        height = 700, width = 1000)

