#REQUIRED PACKAGES-----------------------
library(openxlsx)
library(tidyverse)
library(readxl)
library(lubridate)
#REQUIRED PACKAGES-----------------------

datafoo <- function(datasap,datatall,datamod){
  #strings with the names of the excel files
  sap <- read_excel(datasap)
  #tall <- read_excel(datatall)
  mod <- read_excel(datamod)
  splitdate <- max(mod$FECHA_INGRESO)
  
  #SAP TREATMENT-------------------------------------------
  colnames(sap) <- paste("v", 1:ncol(sap), sep = "")
  
  sap <- sap %>%
    filter(v8 == "E-CARGO 4.0T",
           v4 > splitdate,
           v13 %in% c(NA,25,26)) %>%
    select(marca = "STARK",
           modelo = v8, 
           chasis = v15,
           motor = v20,
           placa = v16,
           kilometraje = v55,
           fecha_ingreso = v4,
           fecha_alistamiento = v77,
           fecha_aom = v77,
           taller = v28,
           codigo_referencia_mobility = v5,
           descripcion_parte = v17,
           cantidad_ref = v7,
           version = v15,
           tipo_de_servicio = "Correctivo",
           empresa = v15,
           tipo = , #(garantía o no) Preguntar si se relaciona con TP 
           )
  View(sap)
  #SAP TREATMENT-------------------------------------------
  
}

datafoo(datasap = "rep4w.XLSX", datamod = "datosModelo2.xlsx")
