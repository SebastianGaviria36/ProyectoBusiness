library(readxl)
library(tidyverse)
datosModelo2 <- read_excel("datosModelo2.xlsx") %>% as.data.frame()


for (i in 1:2033){
  
  partes <- unique(datosModelo2$DESCRIPCION_PARTE)
  
  if(datosModelo2$ESTADO[i] == "CAMBIO" & 
     is.na(datosModelo2$DESCRIPCION_PARTE[i])){
    
    print(partes)
    print(datosModelo2$DESCRIPCION_INGRESO[i])
    rempindex <- readline("reemplazar por: ")
    
    if (is.na(as.integer(rempindex))){
      datosModelo2$DESCRIPCION_PARTE[i] <- rempindex
    }
    else if (is.integer(as.integer(rempindex))) {
      datosModelo2$DESCRIPCION_PARTE[i] <- partes[as.integer(rempindex)]
    }
  }
}
