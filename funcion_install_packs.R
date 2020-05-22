##### INSTALACIÓN Y CARGA AUTOMÁTICA DE PAQUETES EN R #####
paquetes <- function(p){
  new <- p[!(p %in% installed.packages()[,"Package"])]
  if (length(new)) install.packages(new)
  
  do.call(require, as.list(p))
  }
}
