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
resultsframe <- function(Modelname, rmse, aic){
  newob <- data.frame(Modelo = Modelname, RMSE = rmse, AIC = aic)
  rbind(modelsperform, newob)
}

#------------------GRAPHIC TEMPLATE------------------------
pmod1 <- data.frame(KILOMETRAJE = c(topred1$KILOMETRAJE,
                                    predictedmod1),
                    VALOR = c(rep("Real",866),rep("Predicho",866)),
                    X = rep(1:866,2)) %>%
  ggplot(aes(X,KILOMETRAJE, group = VALOR, color = VALOR)) +
  geom_line() +
  labs(title = "Ajuste Modelo Exponencial", y = "Kilometraje",
       x = "", color = "Valor") +
  theme(axis.text.x = element_blank())
ggplotly(pmod1)
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

topred1 <- data %>% dplyr::select(KILOMETRAJE,
                       DESCRIPCION_PARTE, 
                       EMPRESA, 
                       DEPARTAMENTO) %>% na.omit() #datos para predecir
                                                   #sobre estos regresores

predictedmod1 <- predict(mod1, newdata = topred1)
modelsperform <- data.frame(Modelo = "Surv Exponential",
                            RMSE = rmse(topred1$KILOMETRAJE, predictedmod1),
                            AIC = AIC(mod1))

##-----------------WEIBULL SURVIVAL MODEL------------------
mod2 <- survreg(Surv(KILOMETRAJE,censind) ~ DESCRIPCION_PARTE +
                  EMPRESA + DEPARTAMENTO, data = data, dist = "weibull")

predictedmod2 <- predict(mod2, newdata = topred1)
modelsperform <- resultsframe("Surv Weibull", 
                              rmse(topred1$KILOMETRAJE, predictedmod2),
                              AIC(mod2))

##-----------------GAUSSIAN SURVIVAL MODEL-----------------
mod3 <- survreg(Surv(KILOMETRAJE,censind) ~ DESCRIPCION_PARTE +
                  EMPRESA + DEPARTAMENTO, data = data, dist = "gaussian")

predictedmod3 <- predict(mod3, newdata = topred1)
modelsperform <- resultsframe("Surv Gaussian", 
                              rmse(topred1$KILOMETRAJE, predictedmod3),
                              AIC(mod3))

##-----------------LOGISTIC SURVIVAL MODEL-----------------
mod4 <- survreg(Surv(KILOMETRAJE,censind) ~ DESCRIPCION_PARTE +
                  EMPRESA + DEPARTAMENTO, data = data, dist = "logistic")

predictedmod4 <- predict(mod4, newdata = topred1)
modelsperform <- resultsframe("Surv Logistic", 
                              rmse(topred1$KILOMETRAJE, predictedmod4),
                              AIC(mod4))

##-----------------LOGNORMAL SURVIVAL MODEL----------------
mod5 <- survreg(Surv(KILOMETRAJE,censind) ~ DESCRIPCION_PARTE +
                  EMPRESA + DEPARTAMENTO, data = data, dist = "lognormal")

predictedmod5 <- predict(mod5, newdata = topred1)
modelsperform <- resultsframe("Surv Lognormal", 
                              rmse(topred1$KILOMETRAJE, predictedmod5),
                              AIC(mod5))

##-----------------LOGLOGISTIC SURVIVAL MODEL--------------
mod6 <- survreg(Surv(KILOMETRAJE,censind) ~ DESCRIPCION_PARTE +
                  EMPRESA + DEPARTAMENTO, data = data, dist = "loglogistic")

predictedmod6 <- predict(mod6, newdata = topred1)
modelsperform <- resultsframe("Surv Loglogistic", 
                              rmse(topred1$KILOMETRAJE, predictedmod6),
                              AIC(mod6))

##-----------------LINEAR MODEL----------------------------
mod7 <- lm(KILOMETRAJE ~
             EMPRESA + DEPARTAMENTO, data = data)

predictedmod7 <- predict(mod7, newdata = topred1)
modelsperform <- resultsframe("RLM", 
                              rmse(topred1$KILOMETRAJE, predictedmod7),
                              AIC(mod7))
##-----------------GLM POISSON MODEL-----------------------
mod8 <- data %>% mutate(KILOMETRAJE = round(KILOMETRAJE)) %>%
glm(KILOMETRAJE ~ DESCRIPCION_PARTE +
             EMPRESA + DEPARTAMENTO, data = ., family = poisson())

predictedmod8 <- predict(mod8, newdata = topred1)

modelsperform <- resultsframe("GLM POIS", 
                              rmse(topred1$KILOMETRAJE, predictedmod8),
                              AIC(mod8))
##-----------------GLM GAMMA MODEL-------------------------
mod9 <- glm(KILOMETRAJE ~ DESCRIPCION_PARTE +
        EMPRESA + DEPARTAMENTO, data = data, family = Gamma())

predictedmod9 <- predict(mod9, newdata = topred1)

modelsperform <- resultsframe("GLM GAMMA", 
                              rmse(topred1$KILOMETRAJE, predictedmod9),
                              AIC(mod9))
##-----------------GLM QUASI MODEL-------------------------
mod10 <- glm(KILOMETRAJE ~ DESCRIPCION_PARTE +
              EMPRESA + DEPARTAMENTO, data = data, family = quasipoisson())

predictedmod10 <- predict(mod10, newdata = topred1)

modelsperform <- resultsframe("GLM QUASIPOIS", 
                              rmse(topred1$KILOMETRAJE, predictedmod10),
                              AIC(mod10))
##-----------------GAMLSS BCT MODEL------------------------
mod11 <- gamlss(KILOMETRAJE ~ DESCRIPCION_PARTE +
                EMPRESA + DEPARTAMENTO, family = BCT(), data = topred1,
                control = gamlss.control(n.cyc = 260))
predictedmod11 <- predict(mod11, newdata = topred1)
modelsperform <- resultsframe("GAMLSS BCT", 
                              rmse(topred1$KILOMETRAJE, predictedmod11),
                              AIC(mod11))

#Todos los modelos planteados hasta este punto siguen la estructura:
#KILOMETRAJE ~ PREDICTORES, lo cual es acertado si se busca conocer el
#kilometraje de falla usual de los vehículos bajo ciertas condiciones. Sin
#embargo, este paradigma supone un inconveniente de cara al entrenamiento de 
#los modelos: Como se supo en el análisis descriptivo de los datos, es
#fundamental involucrar la variable DESCRIPCION_PARTE en la estructura de los
#objetos matemáticos y dada su gran dimensionalidad, incluirla como regresor
#genera singularidades y deficiencias estadísticas que reducen drásticamente 
#el desempeño predictivo (Véase el problema de rangos deficientes y 
#saturación del modelo).

#En retrospectiva, este modelo busca ser ensamblado con otro mismo que dado el
#kilometraje predicho, pueda predecir a su vez la probabilidad de falla en cada
#concepto. Esta práctica se conoce como ensamble de modelos y en este caso
#aunque es plausible, dados los inconvenientes encontrados, se debe atacar el 
#problema desde otro frente.

#Por este motivo, se prosigue el proceso de modelación desde el paradigma de la
#clasificación, donde la estructura del modelo tentativa puede ser
#P(DESCRIPCION_PARTE) ~ CLASIFICADORES + KILOMETRAJE/RANGO_KILOMETRAJE.

data$DESCRIPCION_PARTE[is.na(data$DESCRIPCION_PARTE)] <- "SIN FALLO"
library(nnet)
##----------------MULTINOMIAL LOGISTIC MODEL---------------
modclas1 <- multinom(DESCRIPCION_PARTE ~  EMPRESA + DEPARTAMENTO + KMRAN,
                     MaxNWts = 12000,
                     data = data %>% filter(ESTADO == "CAMBIO"), #¿filtrar?
                     maxit = 10000)
