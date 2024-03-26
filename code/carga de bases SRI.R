# Carga de datos de cierres SRI

# Preliminares ------------------------------------------------------------

# Carga de librerías

library(readxl)
library(dplyr)
library(tidyr)
library(readr)

# Descarga de datos -------------------------------------------------------

# Descargar los datos de la página web del Banco de Datos Abiertos

# URLs de descarga (del Banco de Datos Abiertos del Gobierno del Ecuador)

url <- c("https://www.sri.gob.ec/o/sri-portlet-biblioteca-alfresco-internet/descargar/9b140d4c-235f-42c8-89a1-981b81639591/sri_cierres_2018.csv",
         "https://www.sri.gob.ec/o/sri-portlet-biblioteca-alfresco-internet/descargar/f3c2665f-9033-4134-8b30-d042616afe8b/sri_cierres_2017.csv")

# Definición de direcciones dentro del repositorio

dir <- file.path("data/sri", basename(url))

# Descargar mediante la función download.file y mapply

mapply(download.file, url, dir)

# Carga de datos ----------------------------------------------------------

# Utilizar readr para cargar los datos

sri_2018 <- read_delim("data/sri/sri_cierres_2018.csv",
                       delim = ";",
                       locale = locale(encoding = "ISO-8859-1"))
