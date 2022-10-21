datafoo <- function(datasap,datamod){
  
  #REQUIRED PACKAGES---------------------------------------
  library(openxlsx)
  library(tidyverse)
  library(readxl)
  library(lubridate)
  
  #strings with the names of the excel files---------------
  sap <- read_excel(datasap)
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
  tall_link <- function(){
    #tall_bar <- read_excel("DataTest/Control Taller - Barranquilla.xlsx")
    
    file.rename("DataTest/Control Taller - Medellín (2).xlsx",
                "DataTest/Control Taller - Medellin (2).xlsx")
    file.rename("DataTest/Control Taller - Bogotá-.xlsx",
                "DataTest/Control Taller - Bogota-.xlsx")
    
    tall_buc <- read_excel("DataTest/Control Taller - Bucaramanga.xlsx", 
                           sheet = "Entregados",
                           col_types = c("text", "text", "text", 
                                         "text", "text", "text", "numeric", 
                                         "text", "date", "text", "text", "text", 
                                         "text", "text", "numeric", "numeric", 
                                         "text", "text", "text", "text", "date", 
                                         "text", "date", "text", "numeric", 
                                         "numeric", "numeric", "numeric")) %>%
      as.data.frame()
    tall_cal <- read_excel("DataTest/Control Taller - Cali.xlsx", 
                           sheet = "Entregados",
                           col_types = c("text", "text", "text", 
                                         "text", "text", "text", "numeric", 
                                         "text", "date", "text", "text", "text", 
                                         "text", "text", "numeric", "numeric", 
                                         "text", "text", "text", "text", "date", 
                                         "text", "date", "text", "numeric", 
                                         "numeric", "numeric", "numeric")) %>%
      as.data.frame()
    tall_med <- read_excel("DataTest/Control Taller - Medellin (2).xlsx", 
                           sheet = "Entregados",
                           col_types = c("text", "text", "text", 
                                         "text", "text", "text", "numeric", 
                                         "text", "date", "text", "text", "text", 
                                         "text", "text", "numeric", "numeric", 
                                         "text", "text", "text", "text", "date", 
                                         "text", "date", "text", "numeric", 
                                         "numeric", "numeric", "numeric")) %>%
      as.data.frame()
    tall_bog <- read_excel("DataTest/Control Taller - Bogota-.xlsx", 
                           sheet = "Entregados",
                           col_types = c("text", "text", "text", 
                                         "text", "text", "text", "numeric", 
                                         "text", "date", "text", "text", "text", 
                                         "text", "text", "numeric", "numeric", 
                                         "text", "text", "text", "text", "date", 
                                         "text", "date", "text", "numeric", 
                                         "numeric", "numeric", "numeric")) %>%
      as.data.frame()
    tall_nac <- read_excel("DataTest/Control Taller - NACIONALES.xlsx", 
                           sheet = "Entregados",
                           col_types = c("text", "text", "text", 
                                         "text", "text", "text", "numeric", 
                                         "text", "date", "text", "text", "text", 
                                         "text", "text", "numeric", "numeric", 
                                         "text", "text", "text", "text", "date", 
                                         "text", "date", "text", "numeric", 
                                         "numeric", "numeric", "numeric")) %>%
      as.data.frame()
    
    tall_linked <- rbind(tall_bog, tall_buc, tall_cal, tall_med, tall_nac)
    tall_linked

  }
  
  datatall <- tall_link()
  tall <- datatall %>%
    filter(Vehiculo == "4.0T E-CARGO",
           `Fecha Ingreso` > splitdate)
  
  n <- nrow(tall)
  tall <- tall %>%
    mutate(marca = rep("STARK", n),
           modelo = rep("E-CARGO 4.0T", n),
           chasis = toupper(Placa),
           motor = toupper(Placa),
           placa = toupper(Placa),
           kilometraje = Km,
           fecha_ingreso = as.Date(`Fecha Ingreso`),
           fecha_alistamiento = toupper(Placa),
           fecha_aom = toupper(Placa),
           taller = CSA,
           codigo_referencia_mobility = rep(NA,n),
           descripcion_parte = rep(NA,n),
           cantidad_ref = rep(NA,n),
           version = toupper(Placa),
           tipo_de_servicio = `Tipo de Servicio`,
           empresa = Empresa,
           tipo = ifelse(toupper(Garantia) == "NO","NO","GARANTÍAS"),
           descripcion_servicio = `Motivo de ingreso`,
           descripcion_ingreso = `Motivo de ingreso`,
           estado = Comentario,
           tipo_repuesto = rep("NO APLICA", n),
           nota = rep(NA, n),
           departamento = toupper(Placa),
           ciudad = toupper(Ciudad),
           linea = rep("STQ1079L02Y1NBEV",n),
           modelo_año = toupper(Placa),
           ciudad_propietario = toupper(Placa),
           departamento_propietario = toupper(Placa),
           renting = ifelse(toupper(Renting) == "NO","NO","SI"),
           dias = rep(NA,n)) %>%
    select(marca = marca,
           modelo = modelo,
           chasis = chasis,
           motor = motor,
           placa = placa,
           kilometraje = kilometraje,
           fecha_ingreso = fecha_ingreso,
           fecha_alistamiento = fecha_alistamiento,
           fecha_aom = fecha_aom,
           taller = taller,
           codigo_referencia_mobility = codigo_referencia_mobility,
           descripcion_parte = descripcion_parte,
           cantidad_ref = cantidad_ref,
           version = version,
           tipo_de_servicio = tipo_de_servicio,
           empresa = empresa,
           tipo = tipo,
           descripcion_servicio = descripcion_servicio,
           descripcion_ingreso = descripcion_ingreso,
           estado = estado,
           tipo_repuesto = tipo_repuesto,
           nota = nota,
           departamento = departamento,
           ciudad = ciudad,
           linea = linea,
           modelo_año = modelo_año,
           ciudad_propietario = ciudad_propietario,
           departamento_propietario = departamento_propietario,
           renting = renting,
           dias = dias)
  for (i in 1:n){
    matched <- match(tall$placa[i],datosModelo2$PLACA)
    tall$chasis[i] <- datosModelo2$CHASIS[matched]
    tall$motor[i] <- datosModelo2$MOTOR[matched]
    tall$fecha_alistamiento[i] <- as.character(datosModelo2$FECHA_ALISTAMIENTO[matched])
    tall$fecha_aom[i] <- as.character(datosModelo2$FECHA_AOM[matched])
    tall$version[i] <- datosModelo2$VERSION[matched]
    tall$departamento[i] <- datosModelo2$DEPARTAMENTO[matched]
    tall$modelo_año[i] <- datosModelo2$MODELO_AÑO[matched]
    tall$ciudad_propietario[i] <- datosModelo2$CIUDAD_PROPIETARIO[matched]
    tall$departamento_propietario[i] <- datosModelo2$DEPARTAMENTO_PROPIETARIO[matched]
    tall$dias[i] <- interval(ymd(tall$fecha_aom[i]),
                             ymd(tall$fecha_ingreso[i])) %/% days(1)
  }
View(tall)
}

datafoo(datasap = "DataTest/rep4w.XLSX", 
        datamod = "DataTest/datosModelo2.xlsx")
