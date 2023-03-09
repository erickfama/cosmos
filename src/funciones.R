### Funciones ###

# Crear urls de diferentes anios
url_year <- function(url_base, year_base, years){
    data.frame(urls = sapply(as.character(years), function(i){
      str_replace_all(url_base, year_base, i)
    }))
}
