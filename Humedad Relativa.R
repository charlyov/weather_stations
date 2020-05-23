### Cálculo de humedad relativa según datos de temperatura ###
# Referencia:
# (Allen et al., 2006)
# Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (2006). Evapotranspiración del cultivo: guías para la determinación de los requerimientos de agua de los cultivos. Roma: FAO, 298.

eo=function(T){0.6108*exp((17.27*T)/(T+237.3))} # Presión de saturación de vapor
hr= function(tmin,tmed){eo(tmin)*100/eo(tmed)} # Humedad Relativa (%) según Presión real de vapor

hr(10.0 ,19.2) # Humedad relativa para día de incendios
hr(10, mean(c(10,27.3))) # Humedad relativa para todas las estaciones (resúmenes diarios)


### términos en inglés ###
# Relative humidity
# Steam saturation pressure = Presión de saturación de vapor
# dew temperature = temperatura de rocío
# Actual steam pressure = Presión real de vapor