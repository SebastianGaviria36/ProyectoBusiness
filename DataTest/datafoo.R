datafoo <- function(datasap,datatall,datamod){
  
  #REQUIRED PACKAGES---------------------------------------
  library(openxlsx)
  library(tidyverse)
  library(readxl)
  library(lubridate)
  
  #strings with the names of the excel files---------------
  sap <- read_excel(datasap)
  #tall <- read_excel(datatall)
  mod <- read_excel(datamod) 

  
  
  splitdate <- max(mod$FECHA_INGRESO) #Split date
  
  #SAP TREATMENT-------------------------------------------
  colnames(sap) <- paste("v", 1:ncol(sap), sep = "")
  
  sap <- sap %>%
    filter(v8 == "E-CARGO 4.0T",
           v49 > splitdate,
           v13 %in% c(NA,25,26))
  n <- nrow(sap)
  sap <- sap %>%
    mutate(marca = rep("STARK",n),
           kilometraje = as.numeric(v55),
           tipo_de_servicio = rep("Correctivo",n),
           tipo = rep("GARANTÍAS",n), 
           descripcion_servicio = rep("CORRECTIVO",n),
           estado = rep("CAMBIO",n),
           tipo_repuesto = rep("SAP",n),
           linea = rep("STQ1079L02Y1NBEV",n),
           dias = interval(ymd(v77),ymd(v49)) %/% days(1)) %>%
    select(marca = marca,
           modelo = v8, 
           chasis = v15,
           motor = v20,
           placa = v16,
           kilometraje = kilometraje,
           fecha_ingreso = v49,
           fecha_alistamiento = v77,
           fecha_aom = v77,
           taller = v28,
           codigo_referencia_mobility = v5,
           descripcion_parte = v17,
           cantidad_ref = v7,
           version = v15,
           tipo_de_servicio = tipo_de_servicio,
           empresa = v15,
           tipo = tipo,
           descripcion_servicio = descripcion_servicio,
           descripcion_ingreso = v18,
           estado = estado,
           tipo_repuesto = tipo_repuesto,
           nota = v59,
           departamento = v12,
           ciudad = v12,
           linea = linea,
           modelo_año = v48,
           ciudad_propietario = v15,
           departamento_propietario = v15,
           renting = v15,
           dias = dias) %>% as.data.frame()
  
  for (i in 1:n){
    
    sap[i,14] <- sort(unique(mod$VERSION[mod$CHASIS==sap[i,3]]))[1]
    sap[i,16] <- sort(unique(mod$EMPRESA[mod$CHASIS==sap[i,3]]))[1]
    sap[i,23] <- sort(unique(mod$DEPARTAMENTO[mod$CIUDAD==sap[i,24]]))[1]
    sap[i,27] <- sort(unique(mod$CIUDAD_PROPIETARIO[mod$CHASIS==sap[i,3]]))[1]
    sap[i,28] <- sort(unique(mod$DEPARTAMENTO_PROPIETARIO[mod$CHASIS==sap[i,3]]))[1]
    sap[i,29] <- sort(unique(mod$RENTING[mod$CHASIS==sap[i,3]]))[1]
    
    if (is.na(sap[i,5])){
      sap[i,5] <- sort(unique(mod$PLACA[mod$CHASIS==sap[i,3]]))[1]
    }
    
    talleres <- unique(mod$TALLER)
    tallersap <- sap[i,10]
    
    if (tallersap %in% talleres){
      sap[i,10] <- tallersap
    }
    else{
      print(data.frame(Taller=talleres))
      print(data.frame(TALLERSAP=tallersap))
      tallerindex <- readline("Reemplazar por: ")
      
      if (is.na(as.integer(tallerindex))){
        sap[i,10] <- tallerindex
      }
      
      else{
        sap[i,10] <- talleres[as.integer(tallerindex)]
      }
    }
    
    fallas <- unique(mod$DESCRIPCION_PARTE)
    fallasap <- sap[i,12]
    
    if (fallasap %in% fallas){
      sap[i,12] <- fallasap
    }
    
    else{
      print(data.frame(FALLAS=fallas))
      print(data.frame(FALLASAP=fallasap))
      fallaindex <- readline("Reemplazar por: ")
      
      if (is.na(as.integer(fallaindex))){
        sap[i, c(12,19)] <- fallaindex
      }
      
      else {
        sap[i,c(12,19)] <- fallas[as.integer(fallaindex)] 
      }
    }
  }
  names(sap) <- toupper(names(sap))
  View(sap)
  View(rbind(mod,sap))

  #TALLER TREATMENT----------------------------------------

}

datafoo(datasap = "rep4w.XLSX", datamod = "datosModelo2.xlsx")
