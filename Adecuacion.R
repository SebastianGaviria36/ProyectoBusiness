#Packages space
library(readxl)
library(tidyverse)

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
faltantes <- c()
for (i in 1:nrow(datosModelo)){
  if (!is.na(datosModelo$DESCRIPCION_PARTE[i])){
    if(is.na(datosModelo$CANTIDAD_REF[i])){
      faltantes <- append(faltantes,i)
    }
  }
}

faltantes #Vector con las filas donde falta la cantidad de partes

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
unique(datosModelo$DESCRIPCION_INGRESO) #Se pueden unificar niveles

#Resumir NEED_REPUESTO, REPUESTO_SAP, REPUESTO_NACIONAL etc en una sola variable

#Exploring DEPARTAMENTO_PROPIETARIO
unique(datosModelo$DEPARTAMENTO_PROPIETARIO) #Revisar NA´s 

#Renting variable
unique(datosModelo$RENTING) #Imputar NA´s









