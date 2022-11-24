tall_link <- function(){
  library(dplyr)
  library(readxl)
  library(openxlsx)
  tall_bar <- read_excel("Control Taller - Barranquilla.xlsx",
                         sheet = "Entregados",
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "numeric", 
                                       "text", "date", "text", "text", "text", 
                                       "text", "text", "numeric", "numeric", 
                                       "text", "text", "text", "text", "date", 
                                       "text", "date", "text", "numeric", 
                                       "numeric", "numeric", "numeric")) %>%
    as.data.frame()
  
  file.rename("Control Taller - Medellín (2).xlsx",
              "Control Taller - Medellin (2).xlsx")
  file.rename("Control Taller - Bogotá-.xlsx",
              "Control Taller - Bogota-.xlsx")
  
  tall_buc <- read_excel("Control Taller - Bucaramanga.xlsx", 
                         sheet = "Entregados",
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "numeric", 
                                       "text", "date", "text", "text", "text", 
                                       "text", "text", "numeric", "numeric", 
                                       "text", "text", "text", "text", "date", 
                                       "text", "date", "text", "numeric", 
                                       "numeric", "numeric", "numeric")) %>%
    as.data.frame()
  tall_cal <- read_excel("Control Taller - Cali.xlsx", 
                         sheet = "Entregados",
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "numeric", 
                                       "text", "date", "text", "text", "text", 
                                       "text", "text", "numeric", "numeric", 
                                       "text", "text", "text", "text", "date", 
                                       "text", "date", "text", "numeric", 
                                       "numeric", "numeric", "numeric")) %>%
    as.data.frame()
  tall_med <- read_excel("Control Taller - Medellin (2).xlsx", 
                         sheet = "Entregados",
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "numeric", 
                                       "text", "date", "text", "text", "text", 
                                       "text", "text", "numeric", "numeric", 
                                       "text", "text", "text", "text", "date", 
                                       "text", "date", "text", "numeric", 
                                       "numeric", "numeric", "numeric")) %>%
    as.data.frame()
  tall_bog <- read_excel("Control Taller - Bogota-.xlsx", 
                         sheet = "Entregados",
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "numeric", 
                                       "text", "date", "text", "text", "text", 
                                       "text", "text", "numeric", "numeric", 
                                       "text", "text", "text", "text", "date", 
                                       "text", "date", "text", "numeric", 
                                       "numeric", "numeric", "numeric")) %>%
    as.data.frame()
  tall_nac <- read_excel("Control Taller - NACIONALES.xlsx", 
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
  write.xlsx(tall_linked, "Tablero_Taller_General.xlsx")
}
tall_link()
