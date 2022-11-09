#-------------------PACKAGES AREA-------------------------
library(readxl)
library(tidyverse)
library(gamlss)
library(survival)
library(plotly)
#-------------------LOADING DATA---------------------------
data <- read_excel("datosModelo2.xlsx", 
                    col_types = c("text", "text", "text", 
                                  "text", "text", "numeric", "date", 
                                  "date", "date", "text", "text", "text", 
                                  "numeric", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "numeric"))
#-------------------DATA ADECUATION------------------------
data$KMRAN <- rep(NA,nrow(data)) #New variable w km range
for (i in 1:nrow(data)){
  if (is.na(data$KILOMETRAJE[i])){
    data$KMRAN[i] <- NA
    next
  }
  if (data$KILOMETRAJE[i] >= 0 & data$KILOMETRAJE[i] <= 5000){
    data$KMRAN[i] <- "0-5000"
  }
  if (data$KILOMETRAJE[i] >= 5001 & data$KILOMETRAJE[i] <= 10000){
    data$KMRAN[i] <- "5001-10000"
  }
  if (data$KILOMETRAJE[i] >= 10001 & data$KILOMETRAJE[i] <= 15000){
    data$KMRAN[i] <- "10001-15000"
  }
  if (data$KILOMETRAJE[i] >= 15001 & data$KILOMETRAJE[i] <= 20000){
    data$KMRAN[i] <- "15001-20000"
  }
  if (data$KILOMETRAJE[i] >= 20001 & data$KILOMETRAJE[i] <= 25000){
    data$KMRAN[i] <- "20001-25000"
  }
  if (data$KILOMETRAJE[i] >= 25001 & data$KILOMETRAJE[i] <= 30000){
    data$KMRAN[i] <- "25001-30000"
  }
  if (data$KILOMETRAJE[i] >= 30001 & data$KILOMETRAJE[i] <= 35000){
    data$KMRAN[i] <- "30001-35000"
  }
  if (data$KILOMETRAJE[i] >= 35001 & data$KILOMETRAJE[i] <= 40000){
    data$KMRAN[i] <- "35001-40000"
  }
  if (data$KILOMETRAJE[i] >= 40001 & data$KILOMETRAJE[i] <= 45000){
    data$KMRAN[i] <- "40001-45000"
  }
  if (data$KILOMETRAJE[i] >= 45001 & data$KILOMETRAJE[i] <= 50000){
    data$KMRAN[i] <- "45001-50000"
  }
  if (data$KILOMETRAJE[i] >= 50001 & data$KILOMETRAJE[i] <= 55000){
    data$KMRAN[i] <- "50001-55000"
  }
  if (data$KILOMETRAJE[i] >= 55001 & data$KILOMETRAJE[i] <= 60000){
    data$KMRAN[i] <- "55001-60000"
  }
  if (data$KILOMETRAJE[i] >= 60001 & data$KILOMETRAJE[i] <= 65000){
    data$KMRAN[i] <- "60001-65000"
  }
  if (data$KILOMETRAJE[i] >= 65001 & data$KILOMETRAJE[i] <= 70000){
    data$KMRAN[i] <- "65001-70000"
  }
  if (data$KILOMETRAJE[i] >= 70001 & data$KILOMETRAJE[i] <= 75000){
    data$KMRAN[i] <- "70001-75000"
  }
}
#------------------AUX FUNCTIONS---------------------------
rmse <- function(real,pred){
  sqrt(mean((real-pred)^2))
}
#------------------MODELLING-------------------------------

##-----------------EXPONENTIAL SURVIVAL MODEL--------------
censind <- ifelse(is.na(data$DESCRIPCION_PARTE),0,1)

for (i in 1:nrow(data)){
  if (!is.na(data$KILOMETRAJE[i]) & data$KILOMETRAJE[i]==0){
    data$KILOMETRAJE[i]<-1
  }
}

mod1 <- survreg(Surv(KILOMETRAJE,censind) ~ DESCRIPCION_PARTE +
                  EMPRESA + DEPARTAMENTO, data = data, dist = "exponential")

topredmod1 <- data %>% dplyr::select(KILOMETRAJE,
                       DESCRIPCION_PARTE, 
                       EMPRESA, 
                       DEPARTAMENTO) %>% 
  na.omit()

predictedmod1 <- predict(mod1, newdata = topredmod1)
rmse(topredmod1$KILOMETRAJE, predictedmod1) 

pmod1 <- data.frame(KILOMETRAJE = c(topredmod1$KILOMETRAJE,
                                    predictedmod1),
                    VALOR = c(rep("Real",866),rep("Predicho",866)),
                    X = rep(1:866,2)) %>%
  ggplot(aes(X,KILOMETRAJE, group = VALOR, color = VALOR)) +
  geom_line() +
  labs(title = "Ajuste Modelo Exponencial", y = "Kilometraje",
       x = "", color = "Valor") +
  theme(axis.text.x = element_blank())
ggplotly(pmod1)
##-----------------LINEAR MODEL----------------------------
mod2 <- lm(KILOMETRAJE ~ DESCRIPCION_PARTE +
             EMPRESA + DEPARTAMENTO, data = data)

topredmod2 <- data %>% dplyr::select(KILOMETRAJE,
                                     DESCRIPCION_PARTE, 
                                     EMPRESA, 
                                     DEPARTAMENTO) %>% 
  na.omit()

predictedmod2 <- predict(mod2, newdata = topredmod2)
rmse(topredmod2$KILOMETRAJE, predictedmod2) 

pmod2 <- data.frame(KILOMETRAJE = c(topredmod2$KILOMETRAJE,
                                    predictedmod2),
                    VALOR = c(rep("Real",866),rep("Predicho",866)),
                    X = rep(1:866,2)) %>%
  ggplot(aes(X,KILOMETRAJE, group = VALOR, color = VALOR)) +
  geom_line() +
  labs(title = "Ajuste Modelo Lineal", y = "Kilometraje",
       x = "", color = "Valor") +
  theme(axis.text.x = element_blank())
ggplotly(pmod2)
