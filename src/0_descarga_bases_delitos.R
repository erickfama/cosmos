### Descarga de bases ###

# librerias ----
library(dplyr)
library(stringr)

# NOTA: Los datos se descargan de https://www.inegi.org.mx/programas/justiciapenal/#Datos_abiertos

# Estructura de carpetas ----
if(dir.exists("./data") == FALSE){
  dir.create("./data")
  dir.create("./data/1_raw")
  dir.create("./data/1_raw/delitos")
  dir.create("./data/1_raw/delitos/sistema_tradicional")
  dir.create("./data/1_raw/delitos/sistema_ao_control")
  dir.create("./data/1_raw/delitos/sistema_ao_oral")
  dir.create("./data/2_interim")
  dir.create("./data/3_final")
}

# Urls ----

# Sistema tradicional
url_tradicional <- "https://www.inegi.org.mx/contenidos/programas/justiciapenal/2015/datosabiertos/ta_delitos_ehriij2015_csv.zip"

# Sistema acusatorio oral. Juzgado de control
url_control <- "https://www.inegi.org.mx/contenidos/programas/justiciapenal/2015/datosabiertos/jc_delitos_ehriij2015_csv.zip"

# Sistema acusatorio oral. Juzgado de juicio oral 
url_oral <- "https://www.inegi.org.mx/contenidos/programas/justiciapenal/2015/datosabiertos/jo_delitos_ehriij2015_csv.zip"

# Se crean los url para los anios de 2015 a 2020
urls <- data.frame(urls_tradicional = sapply(as.character(2015:2020), function(i){
  str_replace_all(url_tradicional, "2015", i)
}),
urls_control = sapply(as.character(2015:2020), function(i){
  str_replace_all(url_control, "2015", i)
}),
urls_oral = sapply(as.character(2015:2020), function(i){
  str_replace_all(url_oral, "2015", i)
})
)

# Descarga ----

## Paths ----

# Sistema tradicional
path_tradicional <- "./data/1_raw/delitos/sistema_tradicional/"

# Sistema acusatorio oral. Juzgado de control
path_control <- "./data/1_raw/delitos/sistema_ao_control/"

# Sistema acusatorio oral. Juzgado de juicio oral 
path_oral <- "./data/1_raw/delitos/sistema_ao_oral/"

## Descarga de archivos ----

# Sistema tradicional
sapply(1:nrow(urls), function(i){
  file_name <- paste(path_tradicional, str_extract(urls$urls_tradicional[i], pattern = "ta_[a-z]*_[a-z]*[0-9]*_csv.zip$"), sep = "")
  download.file(urls$urls_tradicional[i], file_name)
})

# Sistema acusatorio oral. Juzgado de control
sapply(1:nrow(urls), function(i){
  file_name <- paste(path_control, str_extract(urls$urls_control[i], pattern = "jc_[a-z]*_[a-z]*[0-9]*_csv.zip$"), sep = "")
  download.file(urls$urls_control[i], file_name)
})

# Sistema acusatorio oral. Juzgado de juicio oral 
sapply(1:nrow(urls), function(i){
  file_name <- paste(path_oral, str_extract(urls$urls_oral[i], pattern = "jo_[a-z]*_[a-z]*[0-9]*_csv.zip$"), sep = "")
  download.file(urls$urls_oral[i], file_name)
})

# Extraccion ----

## Sistema tradicional ----
zip_paths <- sapply(2015:2020, function(i){
  str_replace_all("./data/1_raw/delitos/sistema_tradicional/ta_delitos_ehriij2015_csv.zip", "[0-9]{4}", as.character(i))
})
file_names <- sapply(1:nrow(urls), function(i){
  str_replace(paste("conjunto_de_datos/delit_", str_replace(str_extract(urls$urls_tradicional[i], pattern = "ta_[a-z]*_[a-z]*[0-9]*_csv"), "_csv", ".csv"), sep = ""), "delitos_", "")
})

for(i in 1:nrow(urls)){
  unzip(zip_paths[i], files = file_names[i], exdir = "./data/1_raw/delitos/sistema_tradicional", junkpaths = TRUE)
}

## Sistema ao. Control ----
zip_paths <- sapply(2015:2020, function(i){
  str_replace_all("./data/1_raw/delitos/sistema_ao_control/jc_delitos_ehriij2015_csv.zip", "[0-9]{4}", as.character(i))
})
file_names <- sapply(1:nrow(urls), function(i){
  str_replace(paste("conjunto_de_datos/delit_", str_replace(str_extract(urls$urls_control[i], pattern = "jc_[a-z]*_[a-z]*[0-9]*_csv"), "_csv", ".csv"), sep = ""), "delitos_", "")
})

for(i in 1:nrow(urls)){
  unzip(zip_paths[i], files = file_names[i], exdir = "./data/1_raw/delitos/sistema_ao_control", junkpaths = TRUE)
}


## Sistema ao. Oral ----
zip_paths <- sapply(2015:2020, function(i){
  str_replace_all("./data/1_raw/delitos/sistema_ao_oral/jo_delitos_ehriij2015_csv.zip", "[0-9]{4}", as.character(i))
})
file_names <- sapply(1:nrow(urls), function(i){
  str_replace(paste("conjunto_de_datos/delit_", str_replace(str_extract(urls$urls_oral[i], pattern = "jo_[a-z]*_[a-z]*[0-9]*_csv"), "_csv", ".csv"), sep = ""), "delitos_", "")
})

for(i in 1:nrow(urls)){
  unzip(zip_paths[i], files = file_names[i], exdir = "./data/1_raw/delitos/sistema_ao_oral", junkpaths = TRUE)
}
