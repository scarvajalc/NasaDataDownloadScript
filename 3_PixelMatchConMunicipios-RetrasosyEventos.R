#**Manejo de librerías**#
#Instalación de ,ibrerías necesarias, 
#sólo se debe hacer una única vez, se instala para el sistema
install.packages("rasterVis")
install.packages("maptools")
install.packages("maps")

#Cargar las librerías necesarias para correr el progerama
#Se debe hacer siempre que se abra R
library(raster)
library(ncdf4)
library(maptools)



#**Extracción de los valores de los pixeles de los archivos nc4 a los poligonos de los municipios - Datos obtenidos por el sensor e indices Acumulados**#
#Nota: estos cálculos pueden tardar varias horas, para el periodo definido 2007 -2016 el procesamiento de este paso duró aproximadamente 16 horas


#Definición del periodo en el cual se van a relizar los calculos. Debe existir un archivo .nc4 por cada mes
years <- 2007:2016
months <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

#Ruta donde se encuentra el archivo .shp que contiene los poligonos de los municipios
munShapePath <- "D:/Maestria/Trabajo de Grado/NASA/script/mapa municipios/Municipios wgs84_Disolv.shp"
#Lectura del archivo .shp
munShape <- readShapePoly(munShapePath)
#Data frame donde se almacenan los atributos del archivo shape. En este caso usaremos el atributo Codigo_DAN para obtener el código Dane del municipio
munShape.df <- as(munShape, "data.frame")
#Contador de mes. En este caso son 120 meses
month_number <- 1
#Tabla final donde se guardaran todos los valores de cada municipio. Se inicializa con un valor basura que luego sera reemplazado
data_table <- 0

#Algoritmo para realizar la extracción de los valores de los pixeles en cada municipio
#Ciclos que nos permiten iterar sobre cada uno de los archivos correspondientes a un mes en el periodo definido
for (year in years) {
  for (month in months) {
    #Ruta donde se encuentran todos los archivos .nc4 correspondientes a los datos obtenidos por el sensor ya convertidos a las unidade deseadas
    dir = "D:/Maestria/Trabajo de Grado/NASA/script/Datos/DataNasaCMM/"
    #Ruta donde se encuentran todos los archivos .nc4 que contienen el Indice Puntual calculado (Alteraciones) previamente en el anterior script
    dirPI = "D:/Maestria/Trabajo de Grado/NASA/script/Datos/DataIndPuntual/"
    full_pathPI = paste0(dirPI, 'Rainf_f_tavgTair_f_inst', year, month, ".nc4")
    #Se abre el archivo .nc4 como un raster
    #Por cada una de las variables se debe crear un raster diferente
    #Raster de la temperatura obtenida por el sensor
    rasterTemp <- raster(full_path, varname="Tair_f_inst")
    #Raster de la precipitación obtenida por el sensor
    rasterPrec <- raster(full_path, varname="Rainf_f_tavg")
    #Raster del indice puntual de la temperatura
    rasterTempPI <- raster(full_pathPI, varname="Tair_f_inst")
    #Raster del indice puntual de la precipitación
    rasterPrecPI <- raster(full_pathPI, varname="Rainf_f_tavg")
    
    #Se hace uso de la función extract con cada uno de los raster y el archivo .shp abierto previamente (munShape) para la extracción de los valores de los pixeles en cada municipio
    #La función extract recibe un raster y un archivo con los poligonos de los municipios (argumentos 1 y 2), los demas argumentos son parametros que no se usan para nuestros calculos a excepción del parametro normalizeWeights y weights
    #Parametro weights: Permite incluir a los calculos, pixeles que no estan contenidos totalmente en los poligonos. Con este parametro activo la funcion retorna una matriz por cada poligono que contiene los pixeles parcialmente incluidos en el poligono, sus valores, y el porcentaje del pixel que se encuentra en dicho poligono
    #Parametro normalizeWeights: Permite saber que porcentaje del poligono es ocupado por cada pixel.  De esta forma podemos calcular un promedio ponderado para obtener el valor final de la variable en cada municipio
    dataTemp <- extract(rasterTemp, munShape, fun=NULL, na.rm=FALSE, weights=TRUE, normalizeWeights=TRUE, cellnumbers=TRUE, small=FALSE, df=FALSE,factors=TRUE, sp=FALSE)
    dataPrec <- extract(rasterPrec, munShape, fun=NULL, na.rm=FALSE, weights=TRUE, normalizeWeights=TRUE, cellnumbers=TRUE, small=FALSE, df=FALSE, factors=TRUE, sp=FALSE)
    dataTempPI <- extract(rasterTempPI, munShape, fun=NULL, na.rm=FALSE, weights=TRUE, normalizeWeights=TRUE, cellnumbers=TRUE, small=FALSE, df=FALSE,factors=TRUE, sp=FALSE)
    dataPrecPI <- extract(rasterPrecPI, munShape, fun=NULL, na.rm=FALSE, weights=TRUE, normalizeWeights=TRUE, cellnumbers=TRUE, small=FALSE, df=FALSE, factors=TRUE, sp=FALSE)
    
    #Variable que contiene el total de municipios
    qtyMun <- 1122
    #Matriz auxiliar donde vamos a almacenar los valores finales de todas las variables y los datos del municipio por cada periodo. Más adelante esta matriz se unirá a la matriz principal que contendrá los resultados para todos los periodos
    data = array(0, c(qtyMun,11))
    

    #Ciclo para de acuerdo a las matrices obtenidas con la función extract obtener el promedio ponderado de los pixeles en los municipios y así obtener los valores finales de cada variable en el municipio    
    for(i in 1:qtyMun){
      #Variables para el cálculo de la precipitación
      #Vector que contiene los valores de los pixeles encontrados por la función extract en un municipio
      valuesP <- dataPrec[[i]][, 2]
      #Vector que contiene los pesos de los pixeles encontrados por la función extract en un municipio (Porcentaje del poligono que ocupa cada pixel)
      weightsP <- dataPrec[[i]][, 3]
      #Variable donde se almacenará el valor final luego de calcular el promedio ponderado
      precMun <- 0
      
      #Se crean las mismas variables para temperatura, indice puntual temperatura e indice puntual precipitación
      
      valuesT <- dataTemp[[i]][, 2]
      weightsT <- dataTemp[[i]][, 3]
      tempMun <- 0
      
      valuesPPI <- dataPrecPI[[i]][, 2]
      weightsPPI <- dataPrecPI[[i]][, 3]
      precMunPI <- 0
      
      valuesTPI <- dataTempPI[[i]][, 2]
      weightsTPI <- dataTempPI[[i]][, 3]
      tempMunPI <- 0
      
      #Algoritmo que calcula el promedio ponderado de acuerdo a los  vectores creado anteriormente
      #Como algunos pixeles del raster no contienen valores, es decir que el valor es "NA", en caso de que otros pixeles en el municipio si tengan algún valor se reemplazan los "NA" por el promedio de los demas valores. De esta forma no se afectan los cálculos
      for(j in 1:length(valuesT)){
        
        if(is.na(valuesT[j])){
          tempMun = tempMun + (mean(valuesT, na.rm=TRUE) *weightsT[j])
          precMun = precMun + (mean(valuesP, na.rm=TRUE) *weightsP[j])
          tempMunPI = tempMunPI + (mean(valuesTPI, na.rm=TRUE) *weightsTPI[j])
          precMunPI = precMunPI + (mean(valuesPPI, na.rm=TRUE) *weightsPPI[j])
        }else{      
          tempMun = tempMun + (valuesT[j]*weightsT[j])
          precMun = precMun + (valuesP[j]*weightsP[j])
          tempMunPI = tempMunPI + (valuesTPI[j]*weightsTPI[j])
          precMunPI = precMunPI + (valuesPPI[j]*weightsPPI[j])
        }
        
      }
      #Vector con los nombres de cada una de las columnas que va a tener el archivo
      columns <- c("ID", "Codigo DANE", "Año", "Mes", "Fecha","Número de mes", "Codigo DANE-mes","Temperatura", "Precipitación", "Alt Temp", "Alt Prec" )
      #Asignación del vector de nombres a la matriz
      colnames(data) <- columns
      #Se obtiene el código dane del municipio con el dataFrame que contiene los atributos del archivo .shp
      DANE_code = munShape.df[i,"Codigo_DAN"]
      #Se obtiene la fecha en el formato deseado
      date <- format(as.Date(paste0(year,month, "01"), "%Y%m%d"), "%Y%m")

      #Se guardan en la matriz auxiliar los campos requeridos.
      data[i,1] = i
      data[i,2] = DANE_code
      data[i,3] = year
      data[i,4] = month
      data[i,5] = date
      data[i,6] = month_number
      #Campo CodigoDane - número de mes
      data[i,7] = paste0(DANE_code, "-", month_number )
      data[i,8] = tempMun
      data[i,9] = precMun
      data[i,10] = tempMunPI
      data[i,11] = precMunPI

      
    }
    
    #Como ya se mencionó, data_table contendrá la tabla final de los municipios, incialmente se había creado con un valor basura por lo que en la primera iteración se debe reemplazar este valor basura por los calculos realizados para el primer periodo
    if(month_number == 1){
        data_table = data
    #La tabla con los datos de cada mes que se vaya calculando (data) se une a la tabla principal (data_table) con la función rbind
    }else{
        data_table = rbind(data_table, data)
    }
    
    #Se suma uno al contador de meses debido a que ya finalizó el procesamiento del mes
    month_number = month_number + 1
    
  }
}

#Si se quieren ver los resultados obtenidos hasta el momento se puede usar la siguiente linea para convertir la matriz a un archivo .csv que se peude visualizar en Excel
#write.csv(data_table, file = "Datos_Brutos_Alteraciones.csv")



#Ruta donde se encuentra el archivo .shp que contiene los poligonos de los municipios
munShapePath <- "D:/Maestria/Trabajo de Grado/NASA/script/mapa municipios/Municipios wgs84_Disolv.shp"
#Lectura del archivo .shp
munShape <- readShapePoly(munShapePath)
#Data frame donde se almacenan los atributos del archivo shape. En este caso usaremos el atributo Codigo_DAN para obtener el código Dane del municipio
munShape.df <- as(munShape, "data.frame")
#Tabla final donde se guardaran todos los valores de cada municipio. Se inicializa con un valor basura que luego sera reemplazado
data_table_prom <- 0

#Contador de mes
month_number <- 1

months <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

for (month in months){
    print(month)
    #Ruta donde se encuentran todos los archivos .nc4 promedio multianual
    dir = "D:/Maestria/Trabajo de Grado/NASA/script/Datos/DataPromMultianual/"
    full_path = paste0(dir, 'PMAMonth', month, ".nc4")
    #Se abre el archivo .nc4 como un raster
    #Por cada una de las variables se debe crear un raster diferente
    #Raster del promedio de  la temperatura
    rasterTemp <- raster(full_path, varname="Tair_f_inst")
    #Raster del promedio de  la precipitación
    rasterPrec <- raster(full_path, varname="Rainf_f_tavg")

    
    #Se hace uso de la función extract con cada uno de los raster y el archivo .shp abierto previamente (munShape) para la extracción de los valores de los pixeles en cada municipio
    #La función extract recibe un raster y un archivo con los poligonos de los municipios (argumentos 1 y 2), los demas argumentos son parametros que no se usan para nuestros calculos a excepción del parametro normalizeWeights y weights
    #Parametro weights: Permite incluir a los calculos, pixeles que no estan contenidos totalmente en los poligonos. Con este parametro activo la funcion retorna una matriz por cada poligono que contiene los pixeles parcialmente incluidos en el poligono, sus valores, y el porcentaje del pixel que se encuentra en dicho poligono
    #Parametro normalizeWeights: Permite saber que porcentaje del poligono es ocupado por cada pixel.  De esta forma podemos calcular un promedio ponderado para obtener el valor final de la variable en cada municipio
    dataTemp <- extract(rasterTemp, munShape, fun=NULL, na.rm=FALSE, weights=TRUE, normalizeWeights=TRUE, cellnumbers=TRUE, small=FALSE, df=FALSE,factors=TRUE, sp=FALSE)
    dataPrec <- extract(rasterPrec, munShape, fun=NULL, na.rm=FALSE, weights=TRUE, normalizeWeights=TRUE, cellnumbers=TRUE, small=FALSE, df=FALSE, factors=TRUE, sp=FALSE)

    
    #Variable que contiene el total de municipios
    qtyMun <- 1122
    #Matriz auxiliar donde vamos a almacenar los valores finales de todas las variables y los datos del municipio por cada periodo. Más adelante esta matriz se unirá a la matriz principal que contendrá los resultados para todos los periodos
    data = array(0, c(qtyMun,5))
    
    
    #Ciclo para de acuerdo a las matrices obtenidas con la función extract obtener el promedio ponderado de los pixeles en los municipios y así obtener los valores finales de cada variable en el municipio    
    for(i in 1:qtyMun){
        #Variables para el cálculo de la precipitación
        #Vector que contiene los valores de los pixeles encontrados por la función extract en un municipio
        valuesP <- dataPrec[[i]][, 2]
        #Vector que contiene los pesos de los pixeles encontrados por la función extract en un municipio (Porcentaje del poligono que ocupa cada pixel)
        weightsP <- dataPrec[[i]][, 3]
        #Variable donde se almacenará el valor final luego de calcular el promedio ponderado
        precMun <- 0
        
        #Se crean las mismas variables para temperatura
        
        valuesT <- dataTemp[[i]][, 2]
        weightsT <- dataTemp[[i]][, 3]
        tempMun <- 0

        #Algoritmo que calcula el promedio ponderado de acuerdo a los  vectores creado anteriormente
        #Como algunos pixeles del raster no contienen valores, es decir que el valor es "NA", en caso de que otros pixeles en el municipio si tengan algún valor se reemplazan los "NA" por el promedio de los demas valores. De esta forma no se afectan los cálculos
        for(j in 1:length(valuesT)){
            
            if(is.na(valuesT[j])){
                tempMun = tempMun + (mean(valuesT, na.rm=TRUE) *weightsT[j])
                precMun = precMun + (mean(valuesP, na.rm=TRUE) *weightsP[j])
            }else{      
                tempMun = tempMun + (valuesT[j]*weightsT[j])
                precMun = precMun + (valuesP[j]*weightsP[j])
                
            }
            
        }
        #Vector con los nombres de cada una de las columnas que va a tener el archivo
        columns <- c("ID", "Codigo DANE", "Mes","Prom_Temperatura", "Prom_Precipitación")
        #Asignación del vector de nombres a la matriz
        colnames(data) <- columns
        #Se obtiene el código dane del municipio con el dataFrame que contiene los atributos del archivo .shp
        DANE_code = munShape.df[i,"Codigo_DAN"]
        
        
        #Se guardan en la matriz auxiliar los campos requeridos.
        data[i,1] = i
        data[i,2] = DANE_code
        data[i,3] = month
        data[i,4] = tempMun
        data[i,5] = precMun
  
        
        
    }
    
    #Como ya se mencionó, data_table contendrá la tabla final de los municipios, incialmente se había creado con un valor basura por lo que en la primera iteración se debe reemplazar este valor basura por los calculos realizados para el primer periodo
    if(month_number == 1){
        data_table_prom = data
        #La tabla con los datos de cada mes que se vaya calculando (data) se une a la tabla principal (data_table) con la función rbind
    }else{
        data_table_prom = rbind(data_table_prom, data)
    }
    #Se suma uno al contador de meses debido a que ya finalizó el procesamiento del mes
    month_number = month_number + 1
    
}



#**Extracción de los valores de los pixeles de los archivos nc4 a los poligonos de los municipios - Eventos niño, niña y neutro (Indices acumulados)**#

#Número del total de eventos
n_events = 11

#Ruta donde se encuentra el archivo .shp que contiene los poligonos de los municipios
munShapePath <- "D:/Maestria/Trabajo de Grado/NASA/script/mapa municipios/Municipios wgs84_Disolv.shp"
#Lectura del archivo .shp
munShape <- readShapePoly(munShapePath)
#Data frame donde se almacenan los atributos del archivo shape. En este caso usaremos el atributo Codigo_DAN para obtener el código Dane del municipio
munShape.df <- as(munShape, "data.frame")

#Cantidad de municipios
qtyMun <- 1122
#Matriz donde se guardarán los calculos finales
events_data = array(0, c(qtyMun,23))
#Vector que contiene el nombre de las columnas de la matriz, la convención en este caso es "E#EventoVariable" donde T es temperatur y P es precipitación
columns <- c("Codigo DANE", "E1T", "E1P", "E2T", "E2P", "E3T", "E3P","E4T", "E4P", "E5T", "E5P", "E6T", "E6P", "E7T", "E7P","E8T", "E8P", "E9T", "E9P", "E10T", "E10P", "E11T", "E11P")         
#Asignación del vector de nombres a la matriz
colnames(events_data) <- columns

#Algoritmo para realizar la extracción de los valores de los pixeles en cada municipio
#Ciclo que recorre cada uno de los eventos. Importante recordar que debe exisitir un archivo .nc4 con los cálculos correspondientes por cada evento
for(event in 1:n_events){
    #Ruta donde se encuentran todos los archivos .nc4 correspondientes a el indice acumulado calculado por cada evento
    dir = "D:/Maestria/Trabajo de Grado/NASA/script/Datos/DataIndAcumulado/"
    full_path = paste0(dir, 'evento', event, ".nc4")
    
    #Se abre el archivo .nc4 como un raster por cada variable
    rasterTemp <- raster(full_path, varname="Tair_f_inst")
    rasterPrec <- raster(full_path, varname="Rainf_f_tavg")
    
    #se realiza la extracción de los valores por cada pixel con la función extract. Ver arriba la extracción del indice puntual y datos del sensor para mas detalle
    dataTemp = extract(rasterTemp, munShape, fun=NULL, na.rm=FALSE, weights=TRUE, normalizeWeights=TRUE, cellnumbers=TRUE, small=FALSE, df=FALSE,factors=TRUE, sp=FALSE)
    dataPrec = extract(rasterPrec, munShape, fun=NULL, na.rm=FALSE, weights=TRUE, normalizeWeights=TRUE, cellnumbers=TRUE, small=FALSE, df=FALSE,factors=TRUE, sp=FALSE)
    
    #Ver arriba la extracción del indice puntual y datos del sensor para mas detalle
    ##Ciclo para de acuerdo a las matrices obtenidas con la función extract obtener el promedio ponderado de los pixeles en los municipios y así obtener los valores finales de cada variable en el municipio    
    for(i in 1:qtyMun){
        
        #Variables para el cálculo de la precipitación
        #Vector que contiene los valores de los pixeles encontrados por la función extract en un municipio
        valuesP <- dataPrec[[i]][, 2]
        #Vector que contiene los pesos de los pixeles encontrados por la función extract en un municipio (Porcentaje del poligono que ocupa cada pixel)
        weightsP <- dataPrec[[i]][, 3]
        #Variable donde se almacenará el valor final luego de calcular el promedio ponderado
        precMun <- 0
        #Se crean las mismas variables para temperatura
        valuesT <- dataTemp[[i]][, 2]
        weightsT <- dataTemp[[i]][, 3]
        tempMun <- 0

        #Algoritmo que calcula el promedio ponderado de acuerdo a los  vectores creado anteriormente
        #Como algunos pixeles del raster no contienen valores, es decir que el valor es "NA", en caso de que otros pixeles en el municipio si tengan algún valor se reemplazan los "NA" por el promedio de los demas valores. De esta forma no se afectan los cálculos
        for(j in 1:length(valuesT)){
            
            if(is.na(valuesT[j])){
                tempMun = tempMun + (mean(valuesT, na.rm=TRUE) *weightsT[j])
                precMun = precMun + (mean(valuesP, na.rm=TRUE) *weightsP[j])
            }else{      
                tempMun = tempMun + (valuesT[j]*weightsT[j])
                precMun = precMun + (valuesP[j]*weightsP[j])
            }
            
        }
        
        #Se obtiene el código dane del municipio con el dataFrame que contiene los atributos del archivo .shp
        DANE_code = munShape.df[i,"Codigo_DAN"]
        
        #Se guardan en la matriz final los campos requeridos en la columna correspondiente (event*2 para temperatura y event*2 +1 para precipitación)
        events_data[i,1] = DANE_code
        events_data[i,(event  * 2)] = tempMun
        events_data[i,(event * 2) + 1] = precMun
        
        
    }
}
#Si se quieren ver los resultados obtenidos hasta el momento se puede usar la siguiente linea para convertir la matriz a un archivo .csv que se peude visualizar en Excel
#write.csv(events_data, file = "Eventos.csv")

#Ya que estos cálculos llevan mucho tiempo puede ser util guardar las matrices obtenidas en archivos .rds que conservan las propiedades del objeto en R, de esta forma se puede acceder a los datos en cualquier momento en formato R sin tener que volver a realizar los  cálculos
#Descomentar las siguientes líneas para guardar las dos tablas generadas en archivos .rds
#saveRDS(events_data, "events_data.rds")
#saveRDS(data_table, "data_table.rds")
#Para restaurar los objetos gurdados en R
#data_table <- readRDS("final_table.rds")
#events_data <- readRDS("events_data.rds")

#**Re ordenamiento de la matriz por municipio**#

#Se reordena la matriz por la columna Codigo DANE para así ordenar por municipio ya que actualmente se encuentra ordenada por periodo
data_table = data_table[order(data_table[,"Codigo DANE"]),]


#**Adición de las columnas de restrasos por cada una de las 4 variables**#


#Variable que almacena el total de corrimientos que se van a realizar por cada variable
total_lags <- 6
#Total de meses del estudio
periods <- 120
#Cantidad de municipios
qty_mun <- 1122

#Indices de las columnas (variables) a las que se le va a relizar el retraso En este caso las varibales temperatura, precipitación, alt temperatura y alt precipitación se encuentran en las columnas 8,9,10 y 11
lag_columns = c(8,9,10,11)
#El algoritmo que se va a aplicar para la creación de las columnas adicionales requiere ordenar los indices de forma descendente
lag_columns = sort(lag_columns,  decreasing = TRUE)

#Creación de las  n columnas (n=total_lags) adicionales en la matriz en su posición correspondiente
for(i in lag_columns){
    if(i == dim(data_table)[2]){
        data_table = cbind(data_table[,1:i], array(0, c(dim(data_table)[1],total_lags))) 
    }else{
        data_table = cbind(data_table[,1:i], array(0, c(dim(data_table)[1],total_lags)), data_table[,(i+1):dim(data_table)[2]])     
    }
    
}


#El algoritmo que se va a aplicar para realizar los corrimientos(retrasos) requiere ordenar los indices de forma ascendente
lag_columns = sort(lag_columns,  decreasing = FALSE)

#Algoritmo para llenar las columnas de los retrasos con los valores correspondientes teniendo en cuenta el número de periodos para no mezclar la información de un municipio con otro
for(k in 1:length(lag_columns)){
    original_col = lag_columns[k] + (total_lags * (k - 1))
    global_pos <- 1
    for(i in 1:qty_mun){
        for(lag in 1:total_lags){
            aux = global_pos
            for(p in 1:periods){
                if(p <= lag){
                    data_table[global_pos + p - 1, original_col + lag ] = ""
                }else{
                    data_table[global_pos + p - 1, original_col + lag ] = data_table[aux, original_col]
                    aux = aux + 1
                }
            }    
        }
        global_pos = global_pos + periods
    }
}


#**Incorporación  de los datos de los eventos (events_data) a la tabla principal (data_table)**#

#Se agregan las 22 columnas donde se almacenarán los datos de los eventos
data_table = cbind(data_table, array("", c(dim(data_table)[1],22)))

#Total de eventos
n_events = 11
#Arreglo de eventos que contiene la fecha de inicio y final de cada uno. Deben coincidir con los eventos ya definidos en todos los scripts anteriores
events <- array('', c(n_events,2))
events[1,1] = '200701'; events[1,2] = '200702'
events[2,1] = '200703'; events[2,2] = '200705'
events[3,1] = '200706'; events[3,2] = '200807'
events[4,1] = '200808'; events[4,2] = '200905'
events[5,1] = '200906'; events[5,2] = '201005'
events[6,1] = '201006'; events[6,2] = '201106'
events[7,1] = '201107'; events[7,2] = '201204'
events[8,1] = '201205'; events[8,2] = '201501'
events[9,1] = '201502'; events[9,2] = '201605'
events[10,1] = '201606'; events[10,2] = '201606'
events[11,1] = '201607'; events[11,2] = '201612'

#Total de meses del estudio
periods <- 120
#Número de la columna donde se guardará la información del primer evento
starting_events_col <- 36

#Algoritmo para incluir la información de cada evento en la tabla principal según la fecha del evento y el dato del municipio correspondiente
for(event in 1:n_events){
    d1 <- as.Date(paste0(events[event,1], "01"), "%Y%m%d")
    d2 <- as.Date(paste0(events[event,2], "01"), "%Y%m%d")
    all_months <- format(seq(d1,d2,by="month"), "%Y%m")
    for(i in 1:qty_mun){
        for(j in 1:(qty_mun * periods)){
            #events_data y data_table deben tener una columna llamada "Codigo DANE" (esta columna se crearon en la extracción de los pixeles)
            if(events_data[i , "Codigo DANE"] == data_table[j, "Codigo DANE"]){
                for(k in j:(j+(periods - 1))){
                    #data_table debe tener una columna llamada Fecha con el formado YYYYMM (esta columna se creó en la extracción de los pixeles)
                    if(data_table[k, "Fecha"] %in% all_months){
                        data_table[k,starting_events_col + 2*(event - 1)] = events_data[i, 2 + 2*(event - 1)]
                        data_table[k,starting_events_col + 2*(event - 1) + 1] = events_data[i, 2 + 2*(event - 1) + 1]
                    }   
                }
                break
            }
        }
    }
}


#**Incorporación  de los datos de promedio mutlianual en la tabla principal (data_table)**#

#Se agregan las 2 columnas donde se almacenarán el promedio multianual de la precipitacion y la temperatura
data_table = cbind(data_table, array("", c(dim(data_table)[1],2)))
#Total de meses del estudio
periods <- 120
#Cantidad de municipios
qty_mun <- 1122

#Algoritmo para incluir promedios multianuales en la tabla
for(j in 1:(qty_mun * periods)){
    for(i in 1:(qty_mun*12)){
        #events_data y data_table deben tener una columna llamada "Codigo DANE" (esta columna se crearon en la extracción de los pixeles)
        if(data_table_prom[i , "Codigo DANE"] == data_table[j, "Codigo DANE"] && data_table_prom[i , "Mes"] == data_table[j, "Mes"]){
            #promedio temp
            data_table[j,(dim(data_table)[2]) - 1] = data_table_prom[i, 4]
            
            #promedio prec
            data_table[j,dim(data_table)[2]] = data_table_prom[i, 5]
            break;
        }
        
    }
}



#**Corregir formato de la columna Codigo DANE-mes**#
qtyMun <- 1122
periods <- 120

for(i in 1:(qtyMun*periods)){
    cod = data_table[i,"Codigo DANE"]
    nMonth = as.integer(data_table[i, "Número de mes"])
    stringMonth = ""
    if(nMonth < 10){
        stringMonth = paste0("00", nMonth)
    }else{
        if(nMonth < 100){
            stringMonth = paste0("0", nMonth)
        }else{
            stringMonth = nMonth
        }
    }
    data_table[i, "Codigo DANE-mes"] = paste0(cod,stringMonth)
}

#Vector con el nombre de todas las columnas incluyendo eventos y retrasos
#NOTA: Los nombres de las columnas deben ser puestos de forma manual en orden como se ve en el siguiente vector
#Debe cambiar los nombres si cambia de datos o si cambia el # de corrimientos o la fecha de los eventos
columns <- c("ID",	"Codigo DANE",	"Año",	"Mes",	"Fecha",	"Número de mes",	"Codigo DANE-mes",	"Temperatura",	"Temperatura1",	"Temperatura2",	"Temperatura3",	"Temperatura4",	"Temperatura5",	"Temperatura6",	"Precipitación",	"Precipitación1",	"Precipitación2",	"Precipitación3",	"Precipitación4",	"Precipitación5",	"Precipitación6",	"AltTemp",	"AltTemp1",	"AltTemp2",	"AltTemp3",	"AltTemp4",	"AltTemp5",	"AltTemp6",	"AltPrec",	"AltPrec1",	"AltPrec2",	"AltPrec3",	"AltPrec4",	"AltPrec5",	"AltPrec6",	"altTempNiño-ene07-feb07",	"altPrecNiño-ene07-feb07",	"altTempNeutro-mar07-may07",	"altPrecNeutro-mar07-may07",	"altTempNiña-jun07-jul08",	"altPrecNiña-jun07-jul08",	"altTempNeutro-ago08-may09",	"altPrecNeutro-ago08-may09",	"altTempNiño-jun09-may10",	"altPrecNiño-jun09-may10",	"altTempNiña-jun10-jun11",	"altPrecNiña-jun10-jun11",	"altTempNiña-jul11-abr12",	"altPrecNiña-jul11-abr12",	"altTempNeutro-may12-ene15",	"altPrecNeutro-may12-ene15",	"altTempNiño-feb15-may16",	"altPrecNiño-feb15-may16",	"altTempNeuto-jun16-jun16",	"altPrecNeuto-jun16-jun16",	"altTempNiñajul16-dic16","altPrecNiñajul16-dic16", "PromedioTemp", "PromedioPrec")
#Asignación del vector de nombres a la matriz
colnames(data_table) <- columns

#Se guarda la matriz en un objeto .rds para conservar los datos obtenidos en formato R
saveRDS(data_table, "final_data.rds")

#Escritura de la matriz final en un archivo .csv
write.csv(data_table, file = "final_data_table.csv")


