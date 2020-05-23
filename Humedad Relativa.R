### Cálculo de humedad relativa según datos de temperatura ###
# Referencia:
# (Allen et al., 2006)
# Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (2006). Evapotranspiración del cultivo: guías para la determinación de los requerimientos de agua de los cultivos. Roma: FAO, 298.

eo=function(T){0.6108*exp((17.27*T)/(T+237.3))} # Presión de saturación de vapor
hr= function(tmin,tmed){eo(tmin)*100/eo(tmed)} # Humedad Relativa (%) según Presión real de vapor

hr(10.0 ,19.2) # Ejémplo de cálculo de Humedad relativa para día promedio

# T = Temperatura
# tmin = temperatura mínima
# tmed = temperatura media
# tmax = temperatura máxima

# La versión en español contiene una corrección a la temperatura por día, 
# la fórmula que se presenta aquí corresponde con esa versión actualizada

### términos en inglés ###
# Relative humidity = Humedad Relativa
# Steam saturation pressure = Presión de saturación de vapor
# dew temperature = temperatura de rocío
# Actual steam pressure = Presión real de vapor
