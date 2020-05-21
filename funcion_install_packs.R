##### INSTALACIÓN Y CARGA AUTOMÁTICA DE PAQUETES EN R #####
paquetes <- function(p){
  for(i in 1:length(p)){
    if (p[i] %in% installed.packages()) do.call(require, list(p[i]))
    else do.call(install.packages, list(p[i])); do.call(require, list(p[i]))
  }
}

jeje <- c("shiny","ggplot2","flexdashboard","leaflet","leaflet.extras","lubridate","stringr","htmltools")
paquetes(jeje)
