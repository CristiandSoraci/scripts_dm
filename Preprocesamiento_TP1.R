#Arbol elemental con libreria  rpart
require("data.table")
require("xgboost")
require("Matrix")
require("ggplot2")
require("tidyverse")
require("dplyr")

#Aqui se debe poner la carpeta de la computadora local
setwd("C:/Users/lssop/Google Drive/UBA2C/DMEyF/datasetsOri")  #Establezco el Working Directory

#Cargo los 2 datasets

ds_set  <- fread("paquete_premium_202009.csv")

ds_nov  <- fread("paquete_premium_202011.csv")

ds_set[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", 1, 0)]

# Sacamos la clase ternaria
ds_set[, clase_ternaria:= NULL]

ds_fe_train=ds_set %>% select(-c("ccajas_transacciones", "Master_mpagominimo" ))


### HASTA ACA TRASNFORMACIONES COMUNES A TODOS, NADA DE FE #####
## 1ER DS Basico sin ninguna transformacion 

setwd("C:/Users/lssop/Google Drive/UBA2C/DMEyF/dmeyf/feature_eng")

fwrite(ds_fe_train, file="ds_train.csv", sep="," ) 

## Lo mismo para el SET de prueba (NOV)

ds_nov[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", 1, 0)]

# Sacamos la clase ternaria
ds_nov[, clase_ternaria:= NULL]

ds_fe_test=ds_nov %>% select(-c("ccajas_transacciones", "Master_mpagominimo" ))

setwd("C:/Users/lssop/Google Drive/UBA2C/DMEyF/dmeyf/feature_eng")

fwrite(ds_fe_test, file="ds_test.csv", sep="," ) 

## FEATURE ENGINEERING ####################################################

ds_fe_train <- ds_fe_train %>% 
  mutate(m_trx_prom_tarj = (Master_mconsumototal + Visa_mconsumototal)/(Master_cconsumos + Visa_cconsumos))

ds_fe_train <- ds_fe_train %>% 
  mutate(actividad = (1+chomebanking_transacciones)*(1+tmobile_app)*(1+cpayroll_trx)*
           (1+ctarjeta_debito_transacciones)*(1+ctrx_quarter))

ds_fe_train <- ds_fe_train %>% 
  mutate(volumen = (1+mcuentas_saldo)*(1+(mpasivos_margen + mactivos_margen + mrentabilidad_annual))*
           (1+cliente_antiguedad)*
           (1+ctarjeta_visa + ctarjeta_master)*(1+mtarjeta_visa_consumo))

glimpse(ds_fe_train)

setwd("C:/Users/lssop/Google Drive/UBA2C/DMEyF/dmeyf/feature_eng")

fwrite(ds_fe_train, file="ds_fe_train.csv", sep="," )

## Generar el mismo tratamiento para el test

ds_nov[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", 1, 0)]

# Sacamos la clase ternaria
ds_nov[, clase_ternaria:= NULL]

ds_fe_test=ds_nov %>% select(-c("ccajas_transacciones", "Master_mpagominimo" ))

ds_fe_test <- ds_fe_test %>% 
  mutate(m_trx_prom_tarj = (Master_mconsumototal + Visa_mconsumototal)/(Master_cconsumos + Visa_cconsumos))

ds_fe_test <- ds_fe_test %>% 
  mutate(actividad = (1+chomebanking_transacciones)*(1+tmobile_app)*(1+cpayroll_trx)*
           (1+ctarjeta_debito_transacciones)*(1+ctrx_quarter))

ds_fe_test <- ds_fe_test %>% 
  mutate(volumen = (1+mcuentas_saldo)*(1+(mpasivos_margen + mactivos_margen + mrentabilidad_annual))*
           (1+cliente_antiguedad)*
           (1+ctarjeta_visa + ctarjeta_master)*(1+mtarjeta_visa_consumo))


setwd("C:/Users/lssop/Google Drive/UBA2C/DMEyF/dmeyf/feature_eng")

fwrite(ds_fe_test, file="ds_fe_test.csv", sep="," )

## TRATAMIENTO DE NAs -- VER SI VALE LA PENA ##########

## Voy a imputar los valores de los descuentos por clientes desde NOV a SET

faltantes1=ds_nov %>% select(numero_de_cliente, ccajeros_propios_descuentos, mcajeros_propios_descuentos, ctarjeta_visa_descuentos
                        
                        ,mtarjeta_visa_descuentos, ctarjeta_master_descuentos, mtarjeta_master_descuentos)

## borro esas columnas de mi ds ###

## las agrego con un left join ##

ds_sin_na = ds_set %>%  select(-ccajeros_propios_descuentos, 
                           -mcajeros_propios_descuentos, -ctarjeta_visa_descuentos
                           ,-mtarjeta_visa_descuentos, -ctarjeta_master_descuentos, 
                           -mtarjeta_master_descuentos) %>%
            
              left_join(., faltantes1, by = "numero_de_cliente")

ds_sin_na=ds_sin_na[,-c("ccajas_transacciones", "Master_mpagominimo" )]

fwrite(ds_sin_na, file="ds_sin_na.csv", sep="," )

## FEATURE ENGINEERING #############

ds_fe = fread("ds_sin_na.csv")

ds_fe <- ds_fe %>% 
  mutate(m_trx_prom_tarj = (Master_mconsumototal + Visa_mconsumototal)/(Master_cconsumos + Visa_cconsumos))

ds_fe <- ds_fe %>% 
  mutate(actividad = (1+chomebanking_transacciones)*(1+tmobile_app)*(1+cpayroll_trx)*
           (1+ctarjeta_debito_transacciones)*(1+ctrx_quarter))

ds_fe <- ds_fe %>% 
  mutate(volumen = (1+mcuentas_saldo)*(1+(mpasivos_margen + mactivos_margen + mrentabilidad_annual))*
           (1+cliente_antiguedad)*
           (1+ctarjeta_visa + ctarjeta_master)*(1+mtarjeta_visa_consumo))

ds_fe[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", 1, 0)]

# Sacamos la clase ternaria
ds_fe[, clase_ternaria:= NULL]

fwrite(ds_fe, file="ds_fe_train.csv", sep="," )

# Aplico el mismo tratamiento al set de test

ds_nov <- ds_nov %>% 
  mutate(m_trx_prom_tarj = (Master_mconsumototal + Visa_mconsumototal)/(Master_cconsumos + Visa_cconsumos))

ds_nov <- ds_nov %>% 
  mutate(actividad = (1+chomebanking_transacciones)*(1+tmobile_app)*(1+cpayroll_trx)*
           (1+ctarjeta_debito_transacciones)*(1+ctrx_quarter))

ds_nov <- ds_nov %>% 
  mutate(volumen = (1+mcuentas_saldo)*(1+(mpasivos_margen + mactivos_margen + mrentabilidad_annual))*
           (1+cliente_antiguedad)*
           (1+ctarjeta_visa + ctarjeta_master)*(1+mtarjeta_visa_consumo))

ds_nov <- ds_nov %>% select(-"ccajas_transacciones",-"Master_mpagominimo" )

ds_nov[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", 1, 0)]

# Sacamos la clase ternaria
ds_nov[, clase_ternaria:= NULL]

fwrite(ds_nov, file="ds_fe_test.csv", sep="," )

## FIN PREPROCESAMIENTO ###

## Aplico el modelo con los hiperparametros elegidos ##

ds=ds_fe

ds[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", 1, 0)]

#ds = ds %>% select(-clase_ternaria)

# Sacamos la clase ternaria
#ds[, clase_ternaria:= NULL]

semillas = c(454823, 454843, 454847, 454849,454859,454889)

set.seed(semillas[2])


inTraining <- caret::createDataPartition(ds$clase_binaria, p = 0.70, list = FALSE)

train  <-  ds[  inTraining, ]
test   <-  ds[ -inTraining, ]

train_matrix <- xgb.DMatrix(data.matrix(train), label = train$clase_binaria)

test_matrix <- xgb.DMatrix(data.matrix(test), label = test$clase_binaria)

#### Pruebo los mejores hyperparametros

xgb_params <- list("booster"="gbtree",   "objective" = "binary:logistic",  # outputs probabilities
                   "eval_metric" = "auc")
#                   "num_class" = nc)

model <- xgboost(params = xgb_params,
                 data = train_matrix,
                 nrounds = 100,
                 min_child_weight=17,
                 #                subsample = 0.4548,
                 eta = 0.05,
                 gamma = 0,
                 max_depth = 6)

## cargo un modelo

model <- xgb.load('xgb.model_10.5')

## Chequeo performance en train y test

pred_train <- predict(model,newdata =  train_matrix)#aplico el modelo

pred_test  <- predict(model, newdata = test_matrix) #aplico el modelo

ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == 1, 48750, -1250 ))
  )
}

ganancia(pred_train, train$clase_binaria)

ganancia(pred_train, train$clase_binaria)/length(train)

ganancia(pred_test, test$clase_binaria)

ganancia(pred_test, test$clase_binaria)/length(test)

### Guardo el ultimo modelo segun ganancia en test  #######

xgb.save(model, 'xgb.model_10.5')

## IMPORTANCIA DE VARIABLES

xgb.importance(colnames(dtrain), model = modelo_xgb_1)
