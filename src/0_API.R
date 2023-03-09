### API ###

# Librerias ----
library(httr)
library(jsonlite)
library(rjson)
library(tidyverse)

# Consulta ----

# 1) Ruta 

inegiR::inegi_series(serie = "620000", token = TOKEN, geography = "01001", as_tt = TRUE)

TOKEN <- "b1065104-ce58-527f-8ee0-5ba3e9e4d008"
url <- "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/6200001281,6200001321,6200030164,6200030717,6200030859,6200031004,6200031006/es/07000001/false/BISE/2.0/[Aquí va tu Token]?type=json"
url_api <- str_replace(string = url, pattern = "\\[Aquí va tu Token\\]", replacement = TOKEN)

# 2) Datos crudos en json

# Respuesta
res <- GET(url_api)

# Datos json
datos_json <- content(res, as = "text") %>%
  fromJSON() %>%
  pluck("Series")

datos <- datos[[1]]$OBSERVATIONS[[1]]
unlist(datos)


# EJEMPLO


url = paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/1003000001/es/0700/false/BISE/2.0/",
             TOKEN,"?type=json")

res=GET(url)


#Obtenemos la respuesta de la API y la pasamos a JSON
vivienda <-content(res, as = 'text')%>%
  fromJSON()%>%
  #Se extrae elemento "Series"
  pluck("Series")

vivienda <- vivienda[[1]]$OBSERVATIONS

vivienda <- data.frame(time_period = vivienda$TIME_PERIOD,
                      obs_value = vivienda$OBS_VALUE)
  

#Se extraen el elemento "OBSERVATIONS" que es aquel que contiene los datos
  pluck("OBSERVATIONS")%>%
  #Se transforma a dataframe y se limpian los nombres
  as.data.frame()%>%
  janitor::clean_names()%>%
  #Se seleccionan las variables requeridas
  select(time_period, obs_value)%>%
  #Se transforman a numérico y renombran
  mutate_all(as.numeric)%>%
  rename(year=1,
         vivi=2
  )%>%
  #Dato de vivienda en millones
  mutate(vivi=vivi/1000000)
