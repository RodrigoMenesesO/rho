###################################### NEURAL NETWORKS ######################################################

### RECURSOS

list.of.packages <- c("devtools","bigrquery","neuralnet","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(devtools)
require(bigrquery)
require(neuralnet)
require(dplyr)

### CONEXIONES Y CONSULTAS

project<-'proyecto-r-227519' # Reemplazar por tu propio proyecto de Google Cloud Platform

sql<-"SELECT * FROM `bigquery-public-data.iowa_liquor_sales.sales` limit 10000" # Identifica los nombres en la consola de BigQuery
sales<-query_exec(sql,project = project,use_legacy_sql = FALSE)

########## CASO 1 - NUEVAS TIENDAS

# Dear, amiguito.
# 
# El cliente JuanPregunton due�o de una cadena comercial de licores quiere 
# generar una inteligencia artificial capaz de decidir
# si sus nuevas tiendas llegar�an a la meta de ventas estipulada por la 
# compa��a, el equipo de retail analytics ha definido que cada
# tienda podr� afrontar sus obligaciones siempre que este por encima del 
# segundo cuartil (0.5) de la distribuci�n de las ventas totales 
# semanales que se encuentran en los datos de retail, puedes identificar 
# las ventas como "sale_dollars." El cliente desea que dicha IA 
# se base en el volumen vendido por litros y en el costo estatal de la botella
# en cada tienda, identificadas como "volume_sold_liters"
# y "state_bottle_cost".
#
# Los datos de retail constituyen una semana de observaciones de retail.
#
# FYI: Puede encontrar la informaci�n sobre las nuevas tiendas en new_stores.csv, 
# donde se ha estimado el agregado de costo por botella
# y venta en litros que se supone tendr�n dichas tiendas.
# 
# ATTE: Rodrigo.

# 1. Quedarse con las variables importantes: tienda, volumen de venta en litros, 
# costo estatal de la botella y venta en dolares

colnames(datos)

datos<-data.frame(tienda=sales$store_name,venta_litros=sales$volume_sold_liters,
                  costo_botella=sales$state_bottle_cost,venta_dolares=sales$sale_dollars)
head(datos)

# 2. Identificar NA's o missings y removerlos 

sum(is.na(datos$tienda) | is.null(datos$tienda))
sum(is.na(datos$venta_litros) | is.null(datos$venta_litros))
sum(is.na(datos$costo_botella) | is.null(datos$costo_botella))
sum(is.na(datos$venta_dolares) | is.null(datos$venta_dolares)) # No hay ninguno

# 3. Agregar ventas de las tiendas

agregado_tienda<-aggregate(datos[,2:4],by=list(datos$tienda),sum)
agregado_tienda<-rename(agregado_tienda,"tienda"="Group.1")

# 4. Se obtiene el segundo cuartil

meta_ventas<-quantile(agregado_tienda$venta_dolares,0.5)

# 5. Indicadora de meta

alcanza_meta<-ifelse(agregado_tienda$venta_dolares>=meta_ventas,1,0)

agregado_tienda$alcanza_meta<-alcanza_meta

# 6. Ajuste de un perceptr�n multicapa de dos neuronas con objetivo de clasificaci�n a trav�s de la funci�n
# de activaci�n sigmoidea con funci�n de p�rdida MSE y ajustando con Fast Resilient Backpropagation (RProp+).

red<-neuralnet(alcanza_meta~venta_litros+costo_botella,data = agregado_tienda,
               hidden = 3,act.fct = "logistic",linear.output = FALSE)

# 7. Graficaci�n del perceptr�n

plot(red)

# 8. Se utiliza la red neuronal para determinar si las nuevas tiendas alcanzar�n las metas.

nuevas_tiendas<-read.csv(choose.files(caption = "Informaci�n de nuevas tiendas"))

predecir<-neuralnet::compute(red,nuevas_tiendas[,2:3])

resultados<-ifelse(predecir$net.result>=0.5,1,0)

nuevas_tiendas$alcanza_meta<-resultados

# 9. Conclusi�n: con base en el pron�stico del perceptr�n, solo las tiendas 3 y 4
# alcanzar�an las metas de venta.


