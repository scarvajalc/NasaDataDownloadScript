#**Credenciales para la autenticación**#
#Previamente se debe tener usuario y contraseña registrados en:
#https://urs.earthdata.nasa.gov/
NASA_user <- 'santiagocc'
NASA_pw <- 'Unal1234'

#**Manejo de librerías**#
#Instalación de ,ibrerías necesarias, 
#sólo se debe hacer una única vez, se instala para el sistema
install.packages("ncdf4")

#Cargar las librerías necesarias para correr el progerama
#Se debe hacer siempre que se abra R
library(ncdf4)#Librería para manejo de archivos .nc4

#**Descargar los datos de la Nasa**#

#Descargar datos desde 1948 a 1999 (version 2.0)
path_base <- 'http://hydro1.gesdisc.eosdis.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fdata%2FGLDAS%2FGLDAS_NOAH025_M.2.0%2F'
path_base2 <- '%2FGLDAS_NOAH025_M.A'

concatenateParams <- '%2C'

#Región espacial para definir las coordenadas
refineSpatialRegion <- c('-4.638', '-82.222', '16.808', '-66.313')

path_base4 <- '&LABEL=GLDAS_NOAH025_M.A'

path_base5 <- '.020.nc4.SUB.nc4&SHORTNAME=GLDAS_NOAH025_M&SERVICE=L34RS_LDAS&VERSION=1.02&DATASET_VERSION=2.0&VARIABLES='

#Variables que se van a descargar
variables <- c("Rainf_f_tavg","Tair_f_inst")

#Directorio donde se va a guardar los archivos descargados
save_directory_path <- 'C:/Users/Daniel/Documents/Unal/Maestria_2018_2/Tesis/DatosEspacialesR/DataNasa/'

#Rango de los datos por año
years <- 1948:1999

#Rango de los datos por mes
months <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

#Ciclos para descargar archivo por archivo mes a mes, año a año.
for (year in years) {
  for (month in months) {
    url <- paste(path_base, year, path_base2, year, month, "", sep = "")
    
    #Nombre con el que se guardará el archivo en el pc
    file_name_disk <- paste(variables[c(1)], variables[c(2)], year, month, ".nc4", sep = "")
    
    file_data <- paste(path_base3, refineSpatialRegion[c(1)], concatenateParams, refineSpatialRegion[c(2)], concatenateParams, refineSpatialRegion[c(3)], 
                       concatenateParams, refineSpatialRegion[c(4)], path_base4, year, month, path_base5, variables[c(1)], concatenateParams, variables[c(2)], sep = "")
    
    #url final que será descargada
    url_file <- paste(url, file_data, sep = "")
    
    #Función con el método wget para descargar el archivo de la Nasa
    download.file(url = url_file,
                  destfile = paste(save_directory_path, file_name_disk, sep = ""),
                  method = 'wget',
                  extra = paste('--load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --user=', NASA_user, ' --password=', NASA_pw, ' --content-disposition', sep = ""))
  }
}


#Descargar datos desde 2000 a 2018 (version 2.1)
path_base <- 'http://hydro1.gesdisc.eosdis.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fdata%2FGLDAS%2FGLDAS_NOAH025_M.2.1%2F'
path_base2 <- '%2FGLDAS_NOAH025_M.A'
path_base3 <- '.021.nc4&FORMAT=bmM0Lw&BBOX='

concatenateParams <- '%2C'

#Región espacial para definir las coordenadas
refineSpatialRegion <- c('-4.638', '-82.222', '16.808', '-66.313')

path_base4 <- '&LABEL=GLDAS_NOAH025_M.A'

path_base5 <- '.021.nc4.SUB.nc4&SHORTNAME=GLDAS_NOAH025_M&SERVICE=L34RS_LDAS&VERSION=1.02&DATASET_VERSION=2.1&VARIABLES='

#Variables que se van a descargar
variables <- c("Rainf_f_tavg","Tair_f_inst")

#Directorio donde se va a guardar los archivos descargados
save_directory_path <- 'C:/Users/Daniel/Documents/Unal/Maestria_2018_2/Tesis/DatosEspacialesR/DataNasa/'

#Rango de los datos por año
years <- 2000:2017

#Rango de los datos por mes
months <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

#Ciclos para descargar archivo por archivo mes a mes, año a año.
for (year in years) {
  for (month in months) {
    url <- paste(path_base, year, path_base2, year, month, "", sep = "")
    
    #Nombre con el que se guardará el archivo en el pc
    file_name_disk <- paste(variables[c(1)], variables[c(2)], year, month, ".nc4", sep = "")
    
    file_data <- paste(path_base3, refineSpatialRegion[c(1)], concatenateParams, refineSpatialRegion[c(2)], concatenateParams, refineSpatialRegion[c(3)],
                       concatenateParams, refineSpatialRegion[c(4)],path_base4, year, month, path_base5, variables[c(1)], concatenateParams, variables[c(2)], sep = "")
    
    #url final que será descargada
    url_file <- paste(url, file_data, sep = "")
    
    #Función con el método wget para descargar el archivo de la Nasa
    download.file(url = url_file,
                  destfile = paste(save_directory_path, file_name_disk, sep = ""),
                  method = 'wget',
                  extra = paste('--load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --user=', NASA_user, ' --password=', NASA_pw, ' --content-disposition', sep = ""))
    
  }
}
