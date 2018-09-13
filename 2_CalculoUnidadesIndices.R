#**Manejo de librerías**#
#Instalación de ,ibrerías necesarias, 
#sólo se debe hacer una única vez, se instala para el sistema
install.packages("ncdf4")

#Cargar las librerías necesarias para correr el progerama
#Se debe hacer siempre que se abra R
library(ncdf4)#Librería para manejo de archivos .nc4

#**Realizar calculos para las variables**#

#Cargar unarchivo descargado cualquiera para estraer la latitud y longitud
openFileNC4 <- nc_open('E:/Tesis2018-2/DataNasaCMM/Rainf_f_tavgTair_f_inst194801.nc4')

#Definir las variables, latitud(lat) y longitud(lon)
lat <- ncdf4::ncvar_get(openFileNC4, varid="lat")
lon <- ncdf4::ncvar_get(openFileNC4, varid="lon")

#Cerrar el archivo .nc4 que fue abierto
nc_close(openFileNC4)

#**Cambiar las unidades de las variables a grados centigrados(temp) y milimetros(prec)**#
#Rango de periodos a cambiar las unidades
years <- 1948:2017
months <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

#Ciclos para recorrer archivo por archivo y modificar las unidades
for (year in years) {
  for (month in months) {
    #Dirección donde todos los archivos están almacenados. 
    #Advertencia, esto es una copia del originalporque los archivos serán sobreescritos
    #Se debe tener permisos de escritura en la carpeta 
    dir = "E:/Tesis2018-2/DataNasaCMM/"
    full_path = paste0(dir, 'Rainf_f_tavgTair_f_inst', year, month, ".nc4")
    
    #Abrir cada archivo .nc4
    openFileNC4 <- nc_open(full_path, write=TRUE)
    
    #Extraer la matriz de la variable de temperatura del archivo
    Tair_f_inst <- ncdf4::ncvar_get(openFileNC4, varid="Tair_f_inst")
    
    #Matriz con la temperatura en kelvin que se guardará en la matriz
    #de temperatura que posteriormente tendrá los datos en grados centígrados
    Tair_c = Tair_f_inst
    
    #Extraer la matriz de la variable de precipitación del archivo
    Rainf_f_tavg <- ncdf4::ncvar_get(openFileNC4, varid="Rainf_f_tavg")
    
    #Matriz con la precipitación en kg/m-2 s-1 que se guardará en la matriz
    #de precipitación que posteriormente tendrá los datos en grados centígrados
    Rainf_mm = Rainf_f_tavg
    
    #Ciclos para acceder a cada pixel (celda) de las matrices tanto de temperatura
    #como de precipitación para modificar su valor.
    for(i in 1:length(lon)){
      for(j in 1:length(lat)){
        
        #Operaciones para cambiar las unidades
        Tair_c[i,j]=  Tair_f_inst[i,j] - 273.15
        Rainf_mm[i,j]=  Rainf_f_tavg[i,j] * 2592000
        
      }
    }
    
    #Guardar archivo con información de la temperatura en grados Centígrados
    ncvar_put( openFileNC4, "Tair_f_inst", Tair_c, start=NA, count=NA )
    
    #Guardar archivo con información de la precipitación en milímegtros
    ncvar_put( openFileNC4, "Rainf_f_tavg", Rainf_mm, start=NA, count=NA )
    
    #Cerrar el archivo .nc4 que fue abierto
    nc_close(openFileNC4)
    
  }
}

#**Calcular el promedio multianual**#
#Rango de años
years <- 1948:2017

#Arreglo de todos los meses
months <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

#n=filas, m=columnas, k=numero de matrices
k = 12; n=length(lon); m = length(lat)

#TJ=Matriz de promedio multianual Tempetatura
TJ <- array(0, c(n,m,k))

#PJ=Matriz de Promedio multianual Precipitacion
PJ <- array(0, c(n,m,k))

#Ciclo para calcular el promedio multianual aplicando la formula
for (year in years) {
  for (month in months) {
    
    #Dirección donde todos los archivos se encuetras almacenados
    dir = "E:/Tesis2018-2/DataNasaCMM/"
    full_path = paste0(dir, 'Rainf_f_tavgTair_f_inst', year, month, ".nc4")
    
    #Abrir cada archivo .nc4
    openFileNC4 <- nc_open(full_path)
    
    #Obtener variable Temperatura
    Tair_f_inst <- ncdf4::ncvar_get(openFileNC4, varid="Tair_f_inst")
    
    #Obtener variable Precipitación
    Rainf_f_tavg <- ncdf4::ncvar_get(openFileNC4, varid="Rainf_f_tavg")
    
    #Ciclo donde se realiza la operación del promedio multianual pixel a pixel
    for(i in 1:length(lon)){
      for(j in 1:length(lat)){
        
        #Sumar cada pixel en cada mes y año
        TJ[i, j, strtoi(month, 10)] = TJ[i, j, strtoi(month, 10)] + Tair_f_inst[i, j]
        PJ[i, j, strtoi(month, 10)] = PJ[i, j, strtoi(month, 10)] + Rainf_f_tavg[i, j]
        
      }
    }
    
    #Cerrar de forma segura el archivo .nc4 previamente abierto
    nc_close(openFileNC4)
    
  }
}

#Obtener el promedio de TJ y PJ
TJ = TJ / length(years)
PJ = PJ / length(years)


#Guardar promedios multinaules en archivos.nc4 
monthQty = 12
for (i in 1:monthQty){
    #Se debe tener permisos de escritura y haber una copia de los archivos
    dir = "E:/Tesis2018-2/DataPromMultianual/"
    full_path = paste0(dir, 'PMAMonth', i, ".nc4")
    
    #Se abre cada archivo .nc4 que será sobreescrito
    openFileNC4ForW <- nc_open(full_path, write=TRUE)
    #Guardar los cambios del archivo con la información del indice puntual de temperatura
    ncvar_put( openFileNC4ForW, "Tair_f_inst", TJ[,,i], start=NA, count=NA )
    
    #Guardar los cambios del archivo con la información del indice puntual de precipitación
    ncvar_put( openFileNC4ForW, "Rainf_f_tavg", PJ[,,i], start=NA, count=NA )
    
    #Cerrar los archivos .nc4 que fueron abiertos
    nc_close(openFileNC4)
    nc_close(openFileNC4ForW)
}
    


#**Calculo del indice puntual de temperatura y precipitación**#
#rango de años
years <- 2007:2017

#Arreglo de meses
months <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

#Ciclo para abrir archivo por archivo y calcular el indice puntual
for (year in years) {
  for (month in months) {
    
    #Dirección donde todos los archivos están almacenados. 
    dir = "E:/Tesis2018-2/DataNasaCMM/"
    full_path = paste0(dir, 'Rainf_f_tavgTair_f_inst', year, month, ".nc4")
    
    #Abrir cada archivo .nc4 de la carpeta que los contiene todos
    openFileNC4 <- nc_open(full_path)
    
    #Se debe tener permisos de escritura y haber una copia de los archivos
    dir = "E:/Tesis2018-2/DataIndPuntual/"
    full_path = paste0(dir, 'Rainf_f_tavgTair_f_inst', year, month, ".nc4")
    
    #Se abre cada archivo .nc4 que será sobreescrito
    openFileNC4ForW <- nc_open(full_path, write=TRUE)
    
    Tair_f_inst <- ncdf4::ncvar_get(openFileNC4, varid="Tair_f_inst")
    
    #Matriz con el Indice puntual temperatura que será modificado
    Tair_puntual_index = Tair_f_inst
    
    Rainf_f_tavg <- ncdf4::ncvar_get(openFileNC4, varid="Rainf_f_tavg")
    
    #Matriz con el Indice puntual de precipitación que será modificado
    Rainf_puntual_index = Rainf_f_tavg
    
    #Ciclo para recorrer cada pixel y realizar operación de indice puntual
    for(i in 1:length(lon)){
      for(j in 1:length(lat)){
        
        #Operación de indice puntual de temperatura
        Tair_puntual_index[i,j]=  Tair_f_inst[i,j] - TJ[i,j, strtoi(month, 10)]
        
        #Operación de indice puntual de precipitación
        Rainf_puntual_index[i,j]=  Rainf_f_tavg[i,j] / PJ[i,j, strtoi(month, 10)]
        
      }
    }
    
    #Guardar los cambios del archivo con la información del indice puntual de temperatura
    ncvar_put( openFileNC4ForW, "Tair_f_inst", Tair_puntual_index, start=NA, count=NA )
    
    #Guardar los cambios del archivo con la información del indice puntual de precipitación
    ncvar_put( openFileNC4ForW, "Rainf_f_tavg", Rainf_puntual_index, start=NA, count=NA )
    
    #Cerrar los archivos .nc4 que fueron abiertos
    nc_close(openFileNC4)
    nc_close(openFileNC4ForW)
    
  }
}

#**Calcular indices acumulados: Temperatura y Precipitación
#Arreglo de 11 eventos: El niño, La niña, Neutro.
n_events = 11

#Matriz de 11 filas 2 columnas
events <- array('', c(n_events,2))

#Inicio y fin de cada evento
events[1,1] = '200608'; events[1,2] = '200702'
events[2,1] = '200703'; events[2,2] = '200705'
events[3,1] = '200706'; events[3,2] = '200807'
events[4,1] = '200808'; events[4,2] = '200905'
events[5,1] = '200906'; events[5,2] = '201005'
events[6,1] = '201006'; events[6,2] = '201106'
events[7,1] = '201107'; events[7,2] = '201204'
events[8,1] = '201205'; events[8,2] = '201501'
events[9,1] = '201502'; events[9,2] = '201605'
events[10,1] = '201606'; events[10,2] = '201606'
events[11,1] = '201607'; events[11,2] = '201701'

#Ciclo para recorrer cada evento
for(event in 1:n_events){
  #Fecha de inicio de cada evento con el formato de fecha
  d1 < as.Date(paste0(events[event,1], "01"), "%Y%m%d")
  
  #Fecha de finalización de cada evento con el formato de fecha
  d2 <- as.Date(paste0(events[event,2], "01"), "%Y%m%d")
  
  #Arreglo que contiene la secuencia de todos los meses en el periodo
  all_months <- format(seq(d1,d2,by="month"), "%Y%m")
  
  #n=numero de filas, m=numero de columnas
  n=length(lon); m = length(lat)
  
  #Partes de las ecuaciones para calcular T=temperatura, P=precipitacion
  sum1T <- array(0, c(n,m))
  sum2T <- array(0, c(n,m))
  sum1P <- array(0, c(n,m))
  sum2P <- array(0, c(n,m))
  
  #Ciclo para recorre mes a mes cada evento
  for (ymonth in all_months) {
    
    #Directorio de los archivos de datos
    dir = "E:/Tesis2018-2/DataNasaCMM/"
    full_path = paste0(dir, 'Rainf_f_tavgTair_f_inst', ymonth, ".nc4")
    
    #Abrir cada archivo .nc4
    openFileNC4 <- nc_open(full_path)
    
    #Matriz de variable Temperatura
    Tair_f_inst <- ncdf4::ncvar_get(openFileNC4, varid="Tair_f_inst")
    
    #Matriz de variable Precipitación
    Rainf_f_tavg <- ncdf4::ncvar_get(openFileNC4, varid="Rainf_f_tavg")
    
    #Ciclo que recorre pixel a pixel de la matriz de temperatura y precipitación
    #y realizar el cálculo del indice acumulado
    for(i in 1:length(lon)){
      for(j in 1:length(lat)){
        
        #Calcular las sumatorias para las partes de la ecuacion
        sum1T[i,j] = sum1T[i,j] + Tair_f_inst[i,j];
        sum2T[i,j]= sum2T[i,j] + TJ[i,j,strtoi(substr(ymonth, 5, 6), 10)]
        sum1P[i,j] = sum1P[i,j] + Rainf_f_tavg[i,j];
        sum2P[i,j]= sum2P[i,j] + PJ[i,j,strtoi(substr(ymonth, 5, 6), 10)]
      }
    }
    #Cerrar el archivo abierto previamente
    nc_close(openFileNC4)
    
  }
  
  #Cada parte de la sumatoria de temperature se divide por n(numero de meses)
  sum1T = sum1T / length(all_months)
  sum2T = sum2T / length(all_months)
  
  #Archivo de eventos previamente creado (11 archivos tipo .nc4) para guardar 11 eventos
  dir = "E:/Tesis2018-2/DataIndAcumulado/"
  full_path = paste0(dir, 'evento', event, ".nc4")
  openFileNC4 <- nc_open(full_path, write = TRUE)
  
  #Obtener variable de temperatura para utilizar
  Tair_f_inst <- ncdf4::ncvar_get(openFileNC4, varid="Tair_f_inst")
  
  #Obtener variable de precipitación para utilizar
  Rainf_f_tavg <- ncdf4::ncvar_get(openFileNC4, varid="Rainf_f_tavg")
  
  #Dar una inicialización a las matrices de variables temperatura y precipitacion
  resT = Tair_f_inst;
  resP = Rainf_f_tavg;
  
  for(i in 1:length(lon)){
    for(j in 1:length(lat)){
      #Resultado de la ecuación de indice acumulado de temperatura 
      #e indice acumulado de precipitacion
      resT[i,j] = sum1T[i,j] - sum2T[i,j]
      resP[i,j] = sum1P[i,j] / sum2P[i,j]
      
    }
  }
  #Modificar archivos con el indices acumulados ya calculado
  ncvar_put( openFileNC4, "Tair_f_inst", resT, start=NA, count=NA )
  ncvar_put( openFileNC4, "Rainf_f_tavg", resP, start=NA, count=NA )
  
  #cerrar archivos abiertos
  nc_close(openFileNC4)
  
}
