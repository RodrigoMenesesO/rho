
list.of.packages <- c("ggplot2","cluster","mclust","factoextra","dendextend","ggpubr","tidyverse","kableExtra",
                      "AnalyzeTS","dplyr","gridExtra","devtools","bigrquery")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(ggplot2)
require(cluster)
require(mclust)
require(factoextra)
require(dendextend)
require(ggpubr)
require(tidyverse)
require(kableExtra)
require(AnalyzeTS)
require(dplyr)
require(gridExtra)
require(devtools)
require(bigrquery)

## K - Medoids 

##  Ajuste a través de Partitioning Around Medoids

# 1. Se selecciona aleatoriamente centros (medoids semilla)

# 2. Matriz de distancias a todas las observaciones

# 3. Asignar cada observación a su medoid más cercano

# 4. Para cada cluster, se comprueba si alguna otra observación
# logra disminuir la suma de distancias tomado como medoid

# 5. Si algún medoid ha cambiado en el paso 4, regresar al paso
# 3, de lo contrario se termina el algoritmo

# K-medoids es útil cuando se sospecha de la presencia de outliers
# En cuyo caso también es adecuado usar como regla de asociación
# la distancia l1 gen (Manhattan) la cual es menos sensible que la 
# euclidea gen.

# Aplicar este método cuando haya presencia de outliers y no se 
# puedan corregir, pues requiere mucho más poder computacional 
# que k-means.

## Ejemplo: 

##################################################

# Conexión: se hace uso de los viajes en taxi de Chicago (2017) de Google Bigquery

project<-'proyecto-r-227519' # Selecciona tu proyecto de Google

sql<-'SELECT taxi_id, trip_seconds, trip_miles, trip_total FROM `bigquery-public-data.chicago_taxi_trips.taxi_trips`'

taxis<-query_exec(sql,project=project,use_legacy_sql = FALSE)

# Se hará uso de las variables de tiempo, distancia y costo total

# Se reescalan los datos para que la aplicación del método sea mejor.

taxis[,2:4]<-scale(taxis[,2:4])

# Desde este punto se suguiere la existencia de outliers al tratarse de 
# viajes que pueden ser pequeños o muy largos.

# Se hace la medición de la reducción de la suma intra-clusters conforme
# se aumenta el número de clusters para encontrar el valor ideal de K.
# Recuerde que se aplicará la distancia l1 generalizada.

## Observe el desbordamiento superior debido a la gran necesidad computacional

fviz_nbclust(x = taxis[,2:4], FUNcluster = pam, method = "wss", k.max = 15,
             diss = dist(taxis[,2:4], method = "manhattan"))

# Se agregan los datos por taxi

taxis_agregado<-aggregate(taxis[,2:4],by=list(taxis[,1]),sum)

# Se encuentran algunos NA's por lo cual es necesario eliminar las 
# observaciones de algunos taxis, al ser una población tan grande
# no afectará significativamente, en caso de ser una pequeña
# debería tomarse otro camino.

taxis_agregado<-taxis_agregado[which(complete.cases(taxis_agregado$trip_seconds) &
                                      complete.cases(taxis_agregado$trip_miles) &
                                      complete.cases(taxis_agregado$trip_total)),]

# Se aplica nuevamente la función fviZ_nbclust para determinar la k óptima
# sin embargo, dado el volumen de datos, la necesidad computacional sigue siendo muy elevada
# por lo que es necesario realizar un muestreo aleatorio.

muestra<-sample(1:nrow(taxis_agregado),500)
muestra_taxis_agregado<-taxis_agregado[muestra,]

fviz_nbclust(x = muestra_taxis_agregado[,2:4], FUNcluster = pam, method = "wss", k.max = 10,
             diss = dist(muestra_taxis_agregado[,2:4], method = "manhattan"))

# Se puede observar que a partir del cluster 4 la suma ya no se reduce significativamente
# por lo tanto k=4

# Se realiza el agrupamiento en clusters

grupo<-pam(muestra_taxis_agregado[,2:4],k=4,metric="manhattan")

# El objeto contiene los medoids, la asignación de cada observación a un cluster
# y la función objetivo que corresponde a la suma de distancias inicial (build) y
# final (swap)

## Finalmente se grafica

p<-fviz_cluster(object = grupo, data = muestra_taxis_agregado[,2:4], ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Clusters k-medoids")

## En clusters donde la dimensionalidad de los datos es de dos, es posible graficar los medoids, en este
## caso no es tan fácil dado que para graficar se aplican componentes principales y los medoids no forman
## parte de la información al realizarse el cálculo, por lo cual realmente será innecesario