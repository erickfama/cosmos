### Descarga bases Censo de Procuración de Justicia Estatal 2021-2019 ###

# Librerías ----
library(tidyverse)
library(robotstxt)
library(rvest)
library(RSelenium)

# Inicio etapa de investigación ----

# Verifying if besoccer accepts automated extraction
url_base <- "https://www.inegi.org.mx/"
url_base_anio <- "https://www.inegi.org.mx/programas/cnpje/NNN/"

links <- c(url_base, url_base_anio)

paths_allowed(links)

# Pages and HTML extraction
years <- as.character(2019:2021)
br_links <- str_replace(url_base_anio, "NNN", years)

htmls <- br_links %>%
  lapply(read_html)

# Getting informations (sample)  
for(html in htmls) {
  matches <- htmls %>%
    html_nodes("#Datos_abiertos > div > div.table-responsive > tr:nth-child(10) > td:nth-child(3) > div.a.aLink.href") %>%
    html_text()
  total_matches <- matches
}
web <- "https://www.inegi.org.mx/programas/cnpje/2021/#Datos_abiertos"
web <- "https://www.inegi.org.mx/componentes/pestana/js/pestana.min.js"

web %>%
  read_html() %>%
  html_nodes("#pestanasSite") %>%
  html_text()
document.querySelector("#divTablaDescargaArchivos_DMDA")
document.querySelector("#contM")
web_soccer <- "https://www.besoccer.com/competition/scores/serie_a_brazil/2021"

web_soccer %>% 
  read_html() %>% 
  html_nodes("h1") %>% 
  html_text()

# R Selenium
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_341/")
driver <- rsDriver(browser = c("chrome"))
remote_driver <- driver[["client"]]
remote_driver$open()

remote_driver$navigate("https://www.latlong.net/convert-address-to-lat-long.html")


