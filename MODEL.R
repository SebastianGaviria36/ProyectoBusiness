#-------------------LOADING DATA--------------------------
library(readxl)
data <- read_excel("datosModelo2.xlsx", 
                           col_types = c("text", "text", "text", 
                                         "text", "text", "numeric", "date", 
                                         "date", "date", "text", "text", "text", 
                                         "numeric", "text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "text", "text", "numeric"))
#------------------MODELLING-------------------------------
