
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

##  Ajuste a trav�s de Partitioning Around Medoids

# 1. Se selecciona aleatoriamente centros (medoids semilla)

# 2. Matriz de distancias a todas las observaciones

# 3. Asignar cada observaci�n a su medoid m�s cercano

# 4. Para cada cluster, se comprueba si alguna otra observaci�n
# logra disminuir la suma de distancias tomado como medoid

# 5. Si alg�n medoid ha cambiado en el paso 4, regresar al paso
# 3, de lo contrario se termina el algoritmo

# K-medoids es �til cuando se sospecha de la presencia de outliers
# En cuyo caso tambi�n es adecuado usar como regla de asociaci�n
# la distancia l1 gen (Manhattan) la cual es menos sensible que la 
# euclidea gen.

# Aplicar este m�todo cuando haya presencia de outliers y no se 
# puedan corregir, pues requiere mucho m�s poder computacional 
# que k-means.

## Ejemplo: 

##################################################

# Conexi�n: se hace uso de los viajes en taxi de Chicago (2017) de Google Bigquery

project<-'proyecto-r-227519' # Selecciona tu proyecto de Google

sql<-'SELECT taxi_id, trip_seconds, trip_miles, trip_total FROM `bigquery-public-data.chicago_taxi_trips.taxi_trips`'

taxis<-query_exec(sql,project=project,use_legacy_sql = FALSE)

# Se har� uso de las variables de tiempo, distancia y costo total

# Se reescalan los datos para que la aplicaci�n del m�todo sea mejor.

taxis[,2:4]<-scale(taxis[,2:4])

# Desde este punto se suguiere la existencia de outliers al tratarse de 
# viajes que pueden ser peque�os o muy largos.

# Se hace la medici�n de la reducci�n de la suma intra-clusters conforme
# se aumenta el n�mero de clusters para encontrar el valor ideal de K.
# Recuerde que se aplicar� la distancia l1 generalizada.

## Observe el desbordamiento superior debido a la gran necesidad computacional

fviz_nbclust(x = taxis[,2:4], FUNcluster = pam, method = "wss", k.max = 15,
             diss = dist(taxis[,2:4], method = "manhattan"))

# Se agregan los datos por taxi

taxis_agregado<-aggregate(taxis[,2:4],by=list(taxis[,1]),sum)

# Se encuentran algunos NA's por lo cual es necesario eliminar las 
# observaciones de algunos taxis, al ser una poblaci�n tan grande
# no afectar� significativamente, en caso de ser una peque�a
# deber�a tomarse otro camino.

taxis_agregado<-taxis_agregado[which(complete.cases(taxis_agregado$trip_seconds) &
                                      complete.cases(taxis_agregado$trip_miles) &
                                      complete.cases(taxis_agregado$trip_total)),]

# Se aplica nuevamente la funci�n fviZ_nbclust para determinar la k �ptima
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

# El objeto contiene los medoids, la asignaci�n de cada observaci�n a un cluster
# y la funci�n objetivo que corresponde a la suma de distancias inicial (build) y
# final (swap)

## Finalmente se grafica

p<-fviz_cluster(object = grupo, data = muestra_taxis_agregado[,2:4], ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Clusters k-medoids")

## En clusters donde la dimensionalidad de los datos es de dos, es posible graficar los medoids, en este
## caso no es tan f�cil dado que para graficar se aplican componentes principales y los medoids no forman
## parte de la informaci�n al realizarse el c�lculo, por lo cual realmente ser� innecesario