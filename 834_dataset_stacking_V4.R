#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#4 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd("~/buckets/b1/datasets")

version  <- "v003"  #cambiar cada vez, asi se tiene versionado del dataset

dataset  <- fread("dataset_epic_simple_v007.csv.gz")
#dataset  <- copy(  dataset[  , c("numero_de_cliente","foto_mes","clase_ternaria"),  with=FALSE] )

gc()


#leo TODOS los archivos que estan en la carpeta  modelitos
#y hago el join con  dataset  <numero_de_cliente, foto_mes, clase_ternaria>

archivos  <- list.files( pattern="modelitos.csv.gz", path="~/buckets/b1/modelitos/" )
for( archivo  in archivos )
{
  darchivo  <- fread( paste0("~/buckets/b1/modelitos/", archivo ) )
  dataset  <- merge( dataset, darchivo, by=c("numero_de_cliente","foto_mes") )
}

gc()

fwrite( dataset,
        file=paste0( "dataset_stacking_", version, ".csv.gz"),
        sep="," )

