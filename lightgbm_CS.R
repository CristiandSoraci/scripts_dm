#Este LightGBM fue construido  para destronar a quienes desde el inicio utilizaron XGBoost y  LightGBM
#install.packages("lightgbm", repos = "https://cran.r-project.org")

#mientras sus compañeros luchaban por correr un rpart

#Con los pibes NO

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("lightgbm")

setwd("C:/Users/lssop/Google Drive/UBA2C/DMEyF/datasetsOri")   #establezco la carpeta donde voy a trabajar

#cargo el dataset
#dataset  <- fread("paquete_premium_202009.csv")

#genero el formato requerido por LightGBM
dtrain  <- lgb.Dataset( data=  data.matrix(ds_fe_Train),
                        label= dataset[ , clase_binaria])

#Solo uso DOS hiperparametros,  max_bin  y min_data_in_leaf
#Dadme un punto de apoyo y movere el mundo, Arquimedes
modelo  <- lightgbm( data= dtrain,
                     params= list( objective= "binary",
                                   max_bin= 15,
                                   min_data_in_leaf= 4000,
                                   learning_rate= 0.05 )  )


#aplico el modelo a los datos nuevos, dapply
prediccion  <- predict( modelo,  data.matrix(ds_fe_test))

#la probabilidad de corte ya no es 0.025,  sino que 0.031
entrega  <- as.data.table( list( "numero_de_cliente"= ds_fe_test[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion > 0.031) ) ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file= "k104_004.csv",
        sep=  "," )

