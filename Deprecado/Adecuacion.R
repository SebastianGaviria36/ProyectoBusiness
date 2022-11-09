#Packages space
library(readxl)
library(tidyverse)
library(lubridate)

#Loading dataset
datosModelo <- read_excel("datosModelo.xlsx", 
                          col_types = c("text", "text", "text", 
                                        "numeric", "text", "numeric", "date", 
                                        "date", "date", "text", "text", "text", 
                                        "numeric", "text", "text", "text", 
                                        "text", "text", "text", "text", "numeric", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "numeric", 
                                        "text", "text", "text", "numeric"))  %>%
  as.data.frame()

#Searching for wrong plates
for (i in 1:nrow(datosModelo)){
  placa <- strsplit(datosModelo[i,5], split = "")[[1]]
  if (length(placa) != 6){
    print(datosModelo[i,5])
  }
} #Hay 16 placas erradas

#Exploring KILOMETRAJE and DIAS
sum(is.na(datosModelo$KILOMETRAJE)) #Faltan 90 registros

sum(is.na(datosModelo$DIAS)) #Faltan 86 registros

plot(datosModelo$DIAS,datosModelo$KILOMETRAJE) #Se puede imputar (ambas) con linearmodel

#Searching in TALLER
unique(datosModelo$TALLER) #Se deben adecuar los niveles

#Reviewing levels in DESCRIPCION_PARTE
unique(datosModelo$DESCRIPCION_PARTE) #Se deben adecuar los niveles

#CANTIDAD_REF corresponds to DESCRIPTION_PARTE?

#Vector con las filas donde falta la cantidad de partes

#There are NA´s in VERSION?
sum(is.na(datosModelo$VERSION)) #Se deben imputar 12 observaciones

#Exploring TIPO_DE_SERVICIO
unique(datosModelo$TIPO_DE_SERVICIO) #Corregir nivel "correctivo"

#Is EMPRESA complete?
View(datosModelo[is.na(datosModelo$EMPRESA),c("PLACA","EMPRESA")])
#Acá puede ser que los faltantes sean vehículos sin empresa o podría imputarse con 
#la placa

#Exploring DESCRIPCION_SERVICIO
unique(datosModelo$DESCRIPCION_SERVICIO) #Se deben revisar los niveles 

#Exploring DESCRIPCION_INGRESO
unique(datosModelo$DESCRIPCION_INGRESO) #Demasiados niveles, revisar y corregir

#Exploring ESTADO
unique(datosModelo$ESTADO) #Se pueden unificar niveles

#Resumir NEED_REPUESTO, REPUESTO_SAP, REPUESTO_NACIONAL etc en una sola variable

#Exploring DEPARTAMENTO_PROPIETARIO
unique(datosModelo$DEPARTAMENTO_PROPIETARIO) #Revisar NA´s 

#Renting variable
unique(datosModelo$RENTING) #Imputar NA´s

#--------------------------------IMPUTATION PROCEDURE--------------------------------------
#PLACA
datosModelo$PLACA[660] <- "JKW267"

#kILOMETRAJE
modkilometraje <- lm(KILOMETRAJE ~ DIAS, data = datosModelo)

datosModelo$KILOMETRAJE[is.na(datosModelo$KILOMETRAJE)] <- predict(modkilometraje, 
        data.frame(DIAS = datosModelo$DIAS[is.na(datosModelo$KILOMETRAJE)]))

#DIAS
moddias <- lm(DIAS ~ KILOMETRAJE, data = datosModelo)

datosModelo$DIAS[is.na(datosModelo$DIAS)] <- predict(moddias,
        data.frame(KILOMETRAJE = datosModelo$KILOMETRAJE[is.na(datosModelo$DIAS)]))


#TALLER
datosModelo$TALLER[datosModelo$TALLER == "Surtiretenes BGTA"] <- "SURTIRETENES Y RODAMIENTOS"
datosModelo$TALLER[datosModelo$TALLER == "SURTIRETENES Y RODAMIENTOS LTD"] <- "SURTIRETENES Y RODAMIENTOS"
datosModelo$TALLER[datosModelo$TALLER == "Surtiretenes CALI"] <- "SURTIRETENES Y RODAMIENTOS"
datosModelo$TALLER[datosModelo$TALLER == "SURTIRETENES Y RODAMIENTOS LTDA CAL"] <- "SURTIRETENES Y RODAMIENTOS"

datosModelo$TALLER[datosModelo$TALLER == "ETM"] <- "EQUIPOS TECNI METALICOS"
datosModelo$TALLER[datosModelo$TALLER == "EQUIPOS TECNI METALICOS SAS"] <- "EQUIPOS TECNI METALICOS"

datosModelo$TALLER[datosModelo$TALLER == "Imalbestos"] <- "IMALBESTOS"
datosModelo$TALLER[datosModelo$TALLER == "DISTRIBUIDORA IMALBESTOS SAS"] <- "IMALBESTOS"

datosModelo$TALLER[datosModelo$TALLER == "TPO REPUESTOS SAS"] <- "TPO REPUESTOS"
datosModelo$TALLER[datosModelo$TALLER == "TPO CALI"] <- "TPO REPUESTOS"


#DESCRIPCION_PARTE
unique(datosModelo$DESCRIPCION_PARTE)

descripcion_parte <- read_rds("descripcion_parte.Rds")

where.is <- function(string){
  idx <- c()
  for (i in 1:nrow(datosModelo)){
    if (!is.na(datosModelo$DESCRIPCION_PARTE[i]) & 
        datosModelo$DESCRIPCION_PARTE[i] == string){
      idx <- append(idx,i)
    }
  }
  idx
}

for (i in 1:nrow(descripcion_parte)){
  if (!is.na(descripcion_parte$X3[i])){
    datosModelo$DESCRIPCION_PARTE[where.is(descripcion_parte$X3[i])] <-
      descripcion_parte$X1[i]
  }
  if (!is.na(descripcion_parte$X2[i])){
    datosModelo$DESCRIPCION_PARTE[where.is(descripcion_parte$X2[i])] <-
      descripcion_parte$X1[i]
  }
}

#CANTIDAD_REF
faltantes <- c()
for (i in 1:nrow(datosModelo)){
  if (!is.na(datosModelo$DESCRIPCION_PARTE[i])){
    if(is.na(datosModelo$CANTIDAD_REF[i])){
      faltantes <- append(faltantes,i)
    }
  }
}

cantidad_ref <- table(datosModelo$DESCRIPCION_PARTE,
                      datosModelo$CANTIDAD_REF)

for (faltante in faltantes){
  nombre <- datosModelo$DESCRIPCION_PARTE[faltante]
  datosModelo$CANTIDAD_REF[faltante] <- which.max(cantidad_ref[nombre,])
}

#VERSION
#LA VERSION MAS FRECUENTE EN CADA AÑO ES V3, POR ESO:

datosModelo$VERSION[is.na(datosModelo$VERSION)] <- "V3"

#TIPO_DE_SERVICIO
datosModelo$TIPO_DE_SERVICIO[datosModelo$TIPO_DE_SERVICIO=="correctivo"] <-
  "Correctivo"

#EMPRESA
placaempresa <- datosModelo[is.na(datosModelo$EMPRESA),c("PLACA","EMPRESA")]
placas <- unique(placaempresa$PLACA)

where.is.2 <- function(placa){
  idx <- c()
  for (i in 1:nrow(datosModelo)){
    if (datosModelo$PLACA[i]==placa){
      idx <- append(idx,i)
    }
  }
  idx
}


for (i in 1:length(placas)){
  empresa <- unique(na.omit(datosModelo$EMPRESA[datosModelo$PLACA==placas[i]]))
  if (!is.na(empresa[1])){
    datosModelo$EMPRESA[where.is.2(placas[i])] <- empresa
  }
}
  
#DESCRIPCION_SERVICIO
descripcion_servicio <- readRDS("descripcion_servicio.Rds")
datosModelo$DESCRIPCION_SERVICIO <- descripcion_servicio

datosModelo$DESCRIPCION_SERVICIO[datosModelo$DESCRIPCION_SERVICIO=="REVISION KM"] <- "MANTENIMIENTO PREVENTIVO"
datosModelo$DESCRIPCION_SERVICIO[datosModelo$DESCRIPCION_SERVICIO=="MANTENIMIENTO"] <- "MANTENIMIENTO PREVENTIVO"
datosModelo$DESCRIPCION_SERVICIO[datosModelo$DESCRIPCION_SERVICIO=="MATENIMIENTO REV 20.000KM"] <- "MANTENIMIENTO REV 20.000KM"
datosModelo$DESCRIPCION_SERVICIO[datosModelo$DESCRIPCION_SERVICIO=="MANTENIMIENTO REV 25.0000KM"] <- "MANTENIMIENTO REV 25.000KM"
datosModelo$DESCRIPCION_SERVICIO[datosModelo$DESCRIPCION_SERVICIO=="MANTENIMIENTO REV 2500KM Y 5000KM"] <- "MANTENIMIENTO REV 2.500KM Y 5.000KM"
datosModelo$DESCRIPCION_SERVICIO[datosModelo$DESCRIPCION_SERVICIO=="MANTENIMIENTO REV10.000KM"] <- "MANTENIMIENTO REV 10.000KM"
datosModelo$DESCRIPCION_SERVICIO[datosModelo$DESCRIPCION_SERVICIO=="MANTENIMIENTO REV 2.000KM"] <- "MANTENIMIENTO REV 2.500KM"

#ESTADO
datosModelo$ESTADO[datosModelo$ESTADO=="MANTENIMIENTO"] <- "MANTENIMIENTO PREVENTIVO"
datosModelo$ESTADO[datosModelo$ESTADO=="CORRIGE / REPARA"] <- "REPARO"
datosModelo$ESTADO[datosModelo$ESTADO=="REVISION KM"] <- "MANTENIMIENTO PREVENTIVO"
datosModelo$ESTADO[datosModelo$ESTADO=="ENGRASA"] <- "LUBRICAN"
datosModelo$ESTADO[datosModelo$ESTADO=="LIMPIEZA Y MANTENIMIENTO"] <- "MANTENIMIENTO PREVENTIVO"
datosModelo$ESTADO[datosModelo$ESTADO=="CONECTAR Y ASEGURAR"] <- "AJUSTA"
datosModelo$ESTADO[datosModelo$ESTADO=="AJUSTE Y ENGRASA"] <- "LUBRICAN"
datosModelo$ESTADO[datosModelo$ESTADO=="NECESITA CAMBIO CLIENTE NO AUTORIZA"] <- "CLIENTE NO AUTORIZA CAMBIO"
datosModelo$ESTADO[datosModelo$ESTADO=="LIMPIEZA Y REVISION"] <- "LIMPIEZA"
datosModelo$ESTADO[datosModelo$ESTADO=="LIMPIEZA Y AJUSTE"] <- "LIMPIEZA"
datosModelo$ESTADO[datosModelo$ESTADO=="REPARA Y AJUSTA"] <- "REPARO"
datosModelo$ESTADO[datosModelo$ESTADO=="TORQUEA"] <- "AJUSTA"

#DEPARTAMENTO_PROPIETARIO
for (i in 1:2033){
  if (is.na(datosModelo$DEPARTAMENTO_PROPIETARIO[i])){
    if (datosModelo$CIUDAD_PROPIETARIO[i] %in% c("MEDELLIN","ITAGUI","LA ESTRELLA")){
      datosModelo$DEPARTAMENTO_PROPIETARIO[i] <- "ANTIOQUIA"
    }
    if (!is.na(datosModelo$CIUDAD_PROPIETARIO[i]) & 
        datosModelo$CIUDAD_PROPIETARIO[i]=="CALI"){
      datosModelo$DEPARTAMENTO_PROPIETARIO[i] <- "VALLE DEL CAUCA"
    }
    if (!is.na(datosModelo$CIUDAD_PROPIETARIO[i]) &
        datosModelo$CIUDAD_PROPIETARIO[i]=="BOGOTA DC"){
      datosModelo$DEPARTAMENTO_PROPIETARIO[i] <- "CUNDINAMARCA"
    }
  }
  if (!is.na(datosModelo$CIUDAD_PROPIETARIO[i]) &
      datosModelo$CIUDAD_PROPIETARIO[i]=="BOGOTA DC"){
    datosModelo$DEPARTAMENTO_PROPIETARIO[i] <- "CUNDINAMARCA"}
}

#RENTING
datosModelo$RENTING[datosModelo$PLACA=="KVW139"] <- "SI"

#UNIFICANDO VARIABLES REPUESTO

datosModelo$TIPO_REPUESTO[is.na(datosModelo$TIPO_REPUESTO)] <- "NO"

datosModelo <- datosModelo[,-c(21:25)]

openxlsx::write.xlsx(datosModelo, "datosModelo2.xlsx")
