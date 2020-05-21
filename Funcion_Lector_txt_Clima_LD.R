# setwd("/media/cstierra1/E82A-5E2E/NN_R")
# setwd("/home/cstierra1/Python_Learning/NN_R")
setwd(getwd())
setwd("G:/Mi unidad/TESIS MAESTRÍA/Tesis maestría inputs/Datos Climaticos/prueba")
# c(15197,15035,15322, 15378, 16036)


################################################################
################################################################
### Function read.smn
### Developed by A. Gomez-Tagle Ch.
### INIRENA, UMSNH 06.2018
### This function reads directly the SMN daily data file format *.txt
### and returns a structured data.frame with timeline $time
###
### the object generated is a data.frame with the following columns:
### time: a POSIXlt format timeline
### Pmm: daily precipitation depth in mm
### Emm: daily evaportaion depth mm (from evaporation pan measurements)
### Tmax: daily maximum temperature in ºC
### Tmin. daily minimmum temperature in ºC



read.smn <- function (file){
	dat <- read.table(file, header=FALSE, skip=21, na.strings="Nulo",nrows=length(readLines(file))-22)

	colnames(dat) <- c("fecha", "Pmm", "Emm", "Tmax", "Tmin")

	dat$time <- as.POSIXct(dat$fecha, format="%d/%m/%Y")

	dat <- dat [,c(6,2:5)]

	return (dat)
}

## Prueba

# dat1 <- read.smn("16253.txt")
# plot(dat1$Pmm ~ dat1$time, type="h", col=4)
# plot(d16008$Pmm ~ d16008$time, type="h", col=4, xlim=as.POSIXct(strptime(c(1960,2018),"%Y")))


##########################
dir()   #obtiene los nombres de los archivos del directorio de trabajo

length(dir()) # tamaño del directorio, es decir, número de archivos

files = dir()

# 16001.txt

nombres <-  paste("d", substr(files,1,5),sep="") # creación de la lista de objetos que estarán dentro del objeto lista para que se asigne cada archivo con cada objeto de la lista
nombres 

lista <-  as.list(nombres) # crea una lista con las dimenciones de los nombres
names(lista) = nombres # Para que los nombres estén asignados a cada hoja de la lista, siendo coincidentes con la misma secuencia de nombres que se utilizó para generar la lista

for(i in 1: length(dir())){
	print(files[i])
	lista[[i]] = read.smn(files[i])
} ### genera una lista con i hojas, donde cada hoja tiene el data.frame de los archivos smn ya convertidos con el primer paquete, pero antes la instrucción es que ponga en la consola el archivo en el que va (como una ayuda para saber cómo va el proceso)

#################### Ejemplos de visualización con la lista ya hecha

lista[[1]]

str(lista) # para saber la estructura cada uno de los data.frames dentro de la lista


lista$d16001

lista[[1]]

lista$d16001

plot(lista$d16001$time, lista$d16001$Pmm, col=4, type="h")

################## extracción de las fechas para un data.frame

lista$d16001$time.lt = as.POSIXlt(lista$d16001$time) #Nueva columna del data.frame d16001 con las fechas, pero ahora con formato POSIXlt, ya que cuando las crea les asigna un formato POSIXct

paste(
	lista$d16001$time.lt$year+1900
	,
	lista$d16001$time.lt$mon+1,sep="-"
	) ## Ejemplo de cómo se pueden extraer datos de fecha fácilmente utilizando el formato POSIXlt

paste(lista$d16001$time.lt$year + 1900,
	lista$d16001$time.lt$mon + 1,
	lista$d16001$time.lt$mday + 1,
	sep="-") ## Ejemplo de cómo se pueden extraer datos de fecha fácilmente utilizando el formato POSIXlt
	

lista$d16001$year.mon = paste(
		lista$d16001$time.lt$year+1900
		,
		lista$d16001$time.lt$mon+1
, sep="-") # creación de una nueva columna con los textos asignados

lista$d16001$year.mon

# Precipitacion


sc <-  which(is.na(lista$d16001$Pmm) == FALSE) # Creación de la máscara con sólo los datos que no tienen NA
sc

lista$d16001$Pmm[sc] # Aplicación de la máscara para dejar sólo los datos que no son NA

tapply(lista$d16001$Pmm[sc], lista$d16001$year.mon[sc], sum) # creación de un resumen con la máscara y Aplicación de la máscara para dejar sólo los datos que no son NA

##### Ejercicio para ver cómo usar el which.min y el which.max
which.min(lista[[1]]$time)


lista[[1]]$time[which.min(lista[[1]]$time)]
lista[[1]]$time[which.max(lista[[1]]$time)]
####################

## Aqui vamos a poner vector de fechas minimas y maximas

# Fecha minima
f.min = NA

# Fecha maxima 
f.max = NA

### iteración para tener un vector con los valores mínimos de los data.frame de toda la lista
for(i in 1:length(lista)){
	print(names(lista)[i])
	print(lista[[i]]$time[which.min(lista[[i]]$time)])
	print(lista[[i]]$time[which.max(lista[[i]]$time)])
	
	f.min[i] = lista[[i]]$time[which.min(lista[[i]]$time)]
	f.max[i] = lista[[i]]$time[which.max(lista[[i]]$time)]
}

# vector de minimos
f.min

# vector de maximos
f.max

as.POSIXlt(f.min[which.min(f.min)], origin="1970-01-01") # Cambio de formato para visualizar la fecha de UNIX a formato POSIX

as.POSIXlt(f.max[which.max(f.max)], origin="1970-01-01") # Cambio de formato para visualizar la fecha de UNIX a formato POSIX



###### Generación del vector de tiempos únicos TiempoSIX #######

	print(names(lista)[1])
	TiempoX = lista[[1]]$time # Genera la lista de la iteración cero
for(i in 2:length(lista)){
	print(names(lista)[i]) # Para saber en qué nombre de data.frame está haciendo el proceso
	TiempoX = append(TiempoX,lista[[i]]$time, after = length(lista[[i-1]]$time)) # Anexa los nuevos valores a lo que ya tenía el vector previamente
	print(length(TiempoX))
}


TiempoX <- sort(unique(TiempoX)) #, fromLast= T) # Para quedarse con las fechas únicas y tener el listado total de fechas a asignar
TiempoSIX <- as.POSIXlt(TiempoX, origin="1970-01-01") # Cambio de formato para visualizar la fecha de UNIX a formato POSIX

print(as.POSIXlt(c(
	TiempoX[which.max(TiempoX)],
	TiempoX[which.min(TiempoX)]
	)
		, origin="1970-01-01"))

Time.y.m <- paste(
		TiempoSIX$year+1900
		,
		TiempoSIX$mon+1
, sep="-") # creacíon de la columna de tiempos con mes y año
Time.y.m <- unique(Time.y.m) # Datos únicos para tener el vector listo

####### Unión de las bases de datos para tener los datos de precipitación Tiempo/Estación ########

str(Time.y.m)
length(Time.y.m)

### Ejemplos del uso de match ###
match(Time.y.m, paste(
	lista[[i]]$time.lt$year+1900
			,
			lista[[i]]$time.lt$mon+1
			, sep="-") )

#	Dummy <- data.frame(Dummy,(match(Time.y.m %in% rownames(Pmm_mask), Pmm_mask)))
#	Dummy <- data.frame(Dummy,(match(Time.y.m, Pmm_mask)))

# match(paste(
#	lista[[i]]$time.lt$year+1900
#			,
#			lista[[i]]$time.lt$mon+1
#			, sep="-"), Time.y.m )

# merge(OBJ_x, OBJ_y, by.x = "campoX", by.y = "campoY", all.x = rellena con los que no coincidan)

merge(data.frame(Time.y.m),Pmm_mask, by.x = "Time.y.m", by.y = "TimeYM", all.x = T)

####FIN DEL EJEMPLO#####

Time.y.m <- paste(TiempoSIX$year+1900,TiempoSIX$mon+1, sep="-") # creacíon de la columna de tiempos con mes y año
Time.y.m <- unique(Time.y.m) # Datos únicos para tener el vector listo
#Dummy <- data.frame(Time.y.m)
#NROW(Dummy) = length(Time.y.m)
#rownames(Dummy) <- Time.y.m
i=1
print(names(lista)[i])
	lista[[i]]$time.lt = as.POSIXlt(lista[[i]]$time)
	lista[[i]]$year.mon <- paste(
			lista[[i]]$time.lt$year+1900
			,
			lista[[i]]$time.lt$mon+1
			, sep="-")
	
	maskita <- which(is.na(lista[[i]]$Pmm) == FALSE)
	Pmm_mask <- data.frame(tapply(lista[[i]]$Pmm[maskita], lista[[i]]$year.mon[maskita],sum))
	colnames(Pmm_mask) <- names(lista[i])
	Pmm_mask$TimeYM <- rownames(Pmm_mask)
	Dummy <- data.frame(merge(data.frame(Time.y.m),Pmm_mask, by.x = "Time.y.m", by.y = "TimeYM", all.x = T))
	print(dim(Dummy))
for(i in 2:length(lista)){
	print(names(lista)[i])
	lista[[i]]$time.lt = as.POSIXlt(lista[[i]]$time)
	lista[[i]]$year.mon <- paste(
			lista[[i]]$time.lt$year+1900
			,
			lista[[i]]$time.lt$mon+1
			, sep="-")
	
	maskita <- which(is.na(lista[[i]]$Pmm) == FALSE)
	Pmm_mask <- data.frame(tapply(lista[[i]]$Pmm[maskita], lista[[i]]$year.mon[maskita],sum))
	colnames(Pmm_mask) <- names(lista[i])
	Pmm_mask$TimeYM <- rownames(Pmm_mask)
	
	Dummy <- data.frame(Dummy,merge(data.frame(Time.y.m),Pmm_mask, by.x = "Time.y.m", by.y = "TimeYM", all.x = T))
	Dummy$Time.y.m.1 <- NULL
	print(dim(Dummy))
	}
	colnames(Dummy) = c("Time.y.m",names(lista[1:i]))
	write.table(Dummy,file="Prec.csv",sep=",")

# rm(Dummy)
# rm(Pmm_mask)
#	rm(maskita)
	rm(Dumm)
	
### Caso 2 ### tratando de simplificar el código
	Time.y.m <- paste(TiempoSIX$year+1900,TiempoSIX$mon+1, sep="-") # creacíon de la columna de tiempos con mes y año
	Time.y.m <- unique(Time.y.m) # Datos únicos para tener el vector listo
	Dumm <- array(NA,c(length(Time.y.m),length(lista)+1))
	Dumm[,1] <- Time.y.m

for(i in 1:length(lista)){
  lista[[i]]$time.lt = as.POSIXlt(lista[[i]]$time)
  lista[[i]]$year.mon <- paste(
    lista[[i]]$time.lt$year+1900
    ,
    lista[[i]]$time.lt$mon+1
    , sep="-")
  
  maskita <- which(is.na(lista[[i]]$Pmm) == FALSE)
  Pmm_mask <- data.frame(tapply(lista[[i]]$Pmm[maskita], lista[[i]]$year.mon[maskita],sum))
  colnames(Pmm_mask) <- names(lista[i])
  Pmm_mask$TimeYM <- rownames(Pmm_mask)
  
  Dumm[,i+1] <- merge(data.frame(Time.y.m),Pmm_mask, by.x = "Time.y.m", by.y = "TimeYM", all.x = T)[,2]
  print(names(lista)[i])
}
colnames(Dumm) = c("Time.y.m",names(lista[1:length(lista)]))
write.table(Dumm,file="Prec.csv",sep=",",row.names = F)


#### TMAX AGRUPADO TODO ####
Time.y.m <- paste(TiempoSIX$year+1900,TiempoSIX$mon+1, sep="-") # creacíon de la columna de tiempos con mes y año
Time.y.m <- unique(Time.y.m) # Datos únicos para tener el vector listo
Dumm <- array(NA,c(length(Time.y.m),length(lista)+1))
Dumm[,1] <- Time.y.m

for(i in 1:length(lista)){
  lista[[i]]$time.lt = as.POSIXlt(lista[[i]]$time)
  lista[[i]]$year.mon <- paste(
    lista[[i]]$time.lt$year+1900
    ,
    lista[[i]]$time.lt$mon+1
    , sep="-")
  
  maskita <- which(is.na(lista[[i]]$Tmax) == FALSE)
  Tmax_mask <- data.frame(tapply(lista[[i]]$Tmax[maskita], lista[[i]]$year.mon[maskita],mean))
  colnames(Tmax_mask) <- names(lista[i])
  Tmax_mask$TimeYM <- rownames(Tmax_mask)
  
  Dumm[,i+1] <- merge(data.frame(Time.y.m),Tmax_mask, by.x = "Time.y.m", by.y = "TimeYM", all.x = T)[,2]
  print(names(lista)[i])
}
colnames(Dumm) = c("Time.y.m",names(lista[1:length(lista)]))
write.table(Dumm,file="Tmax.csv",sep=",",row.names = F)


#### TMIN AGRUPADO TODO ####
Time.y.m <- paste(TiempoSIX$year+1900,TiempoSIX$mon+1, sep="-") # creacíon de la columna de tiempos con mes y año
Time.y.m <- unique(Time.y.m) # Datos únicos para tener el vector listo
Dumm <- array(NA,c(length(Time.y.m),length(lista)+1))
Dumm[,1] <- Time.y.m

for(i in 1:length(lista)){
  lista[[i]]$time.lt = as.POSIXlt(lista[[i]]$time)
  lista[[i]]$year.mon <- paste(
    lista[[i]]$time.lt$year+1900
    ,
    lista[[i]]$time.lt$mon+1
    , sep="-")
  
  maskita <- which(is.na(lista[[i]]$Tmin) == FALSE)
  Tmin_mask <- data.frame(tapply(lista[[i]]$Tmin[maskita], lista[[i]]$year.mon[maskita],mean))
  colnames(Tmin_mask) <- names(lista[i])
  Tmin_mask$TimeYM <- rownames(Tmin_mask)
  
  Dumm[,i+1] <- merge(data.frame(Time.y.m),Tmin_mask, by.x = "Time.y.m", by.y = "TimeYM", all.x = T)[,2]
  print(names(lista)[i])
}
colnames(Dumm) = c("Time.y.m",names(lista[1:length(lista)]))
write.table(Dumm,file="Tmin.csv",sep=",",row.names = F)


##función para facilitar cosas##

exp.smn.todos <- function(datos) {
  datos <- ifelse ("Prec" %in% datos||"prec" %in% datos||"PMM" %in% datos||"PREC" %in% datos, "Pmm",
            ifelse("TMin"%in% datos||"Tempmin"%in% datos||"TempMin"%in% datos||"TMIN"%in% datos,"Tmin",
              ifelse("TMax"%in% datos||"Tempmax"%in% datos||"TempMax"%in% datos||"TMAX"%in% datos, "Tmax", datos)))
  op <- ifelse ("Pmm" %in% datos,"sum","mean")
Time.y.m <- paste(TiempoSIX$year+1900,TiempoSIX$mon+1, sep="-") # creacíon de la columna de tiempos con mes y año
Time.y.m <- unique(Time.y.m) # Datos únicos para tener el vector listo
Dumm <- array(NA,c(length(Time.y.m),length(lista)+1))
Dumm[,1] <- Time.y.m

for(i in 1:length(lista)){
  lista[[i]]$time.lt = as.POSIXlt(lista[[i]]$time)
  lista[[i]]$year.mon <- paste(
    lista[[i]]$time.lt$year+1900
    ,
    lista[[i]]$time.lt$mon+1
    , sep="-")
  
  maskita <- which(is.na(lista[[i]][datos]) == FALSE)
  mask <- data.frame(tapply(lista[[i]][maskita,datos], lista[[i]]$year.mon[maskita] ,op))
  colnames(mask) <- names(lista[i])
  mask$TimeYM <- rownames(mask)
  
  Dumm[,i+1] <- round(merge(data.frame(Time.y.m),mask, by.x = "Time.y.m", by.y = "TimeYM", all.x = T)[,2], digits=2)
  print(names(lista)[i])
}
colnames(Dumm) = c("Time.y.m",names(lista[1:length(lista)]))
write.table(Dumm,file=paste(datos,".csv",sep=""),sep=",",row.names = F)
paste("Tabla ", datos, ".csv", " guardada correctamente", sep = "")
}

exp.smn.todos("Prec")
