library(readxl)
library(tidyverse)
datosModelo2 <- read_excel("datosModelo2.xlsx") %>% as.data.frame()


for (i in 1:2033){
  
  partes <- sort(unique(datosModelo2$DESCRIPCION_PARTE))
  
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

datosModelo2$DESCRIPCION_PARTE[datosModelo2$DESCRIPCION_PARTE == "BRAZO LIMPIABRISAS"] <- "LIMPIABRISAS"
datosModelo2$DESCRIPCION_PARTE[datosModelo2$DESCRIPCION_PARTE == "RADIO"] <- "DISPOSITIVO MULTIMEDIA CAMION"
datosModelo2$DESCRIPCION_PARTE[datosModelo2$DESCRIPCION_PARTE == "CALAPIE IZQUIERDO"] <- "ESTRIBO DE DOS ESCALONES IZQUIERDO"
datosModelo2$DESCRIPCION_PARTE[datosModelo2$DESCRIPCION_PARTE == "FUSBILE"] <- "FUSIBLE"
datosModelo2$DESCRIPCION_PARTE[datosModelo2$DESCRIPCION_PARTE == "LUZ TRASERA DERECHA"] <- "LUZ TRASERA"
datosModelo2$DESCRIPCION_PARTE[datosModelo2$DESCRIPCION_PARTE == "LUZ TRASERA IZQUIERDA"] <- "LUZ TRASERA"
datosModelo2$DESCRIPCION_PARTE[datosModelo2$DESCRIPCION_PARTE == "MANIJA EXTERIOR DERECHA PUERTA"] <- "MANIJA EXTERIOR"
datosModelo2$DESCRIPCION_PARTE[datosModelo2$DESCRIPCION_PARTE == "RATCHE AUTOMATICO FRENO TRAS IZQ"] <- "RATCHET TRASERO IZQUIERDO"
datosModelo2$DESCRIPCION_PARTE[datosModelo2$DESCRIPCION_PARTE == "SENSOR DE ALARMA DE PRESION DE AIRE"] <- "SENSOR DE PRESION"
datosModelo2$DESCRIPCION_PARTE[datosModelo2$DESCRIPCION_PARTE == "SOPORTE ESPEJO RETROVISOR EXTERNO DER"] <- "ESPEJO RETROVISOR"
datosModelo2$DESCRIPCION_PARTE[datosModelo2$DESCRIPCION_PARTE == "SOPORTE ESPEJO RETROVISOR EXTERNO IZQ"] <- "ESPEJO RETROVISOR"
datosModelo2$DESCRIPCION_PARTE[datosModelo2$DESCRIPCION_PARTE == "STQ ESPEJO RETROVISOR BAJO FRONTAL"] <- "ESPEJO RETROVISOR"
datosModelo2$DESCRIPCION_PARTE[datosModelo2$DESCRIPCION_PARTE == "TUBERIA DE AIRE"] <- "MANGUERA COMPRESOR"


openxlsx::write.xlsx(datosModelo2, "datosModelo2.xlsx")
