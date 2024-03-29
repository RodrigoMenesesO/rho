
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


# Clustering LARge Applications combina la idea de kmedoids con 
# el muestreo, para reducir la necesidad computacional

# B�sicamente, CLARA parte el conjunto de observaciones en un conjunto de muestras definidas por
# el usuario y a dichas muestras les aplica el algoritmo k-medoids, utilizando los 
# medoids agrupa las observaciones de todo el conjunto de datos y calcula las distancias intra-clusters

# El algoritmo se repite m�ltiples veces para reducir el sesgo del muestreo y finalmente se seleccionan
# los medoids que hayan conseguido la menor suma de distancia intra clusters.

#### EJEMPLO: se utilizar� CLARA con el set completo de datos de taxis de Chicago, agregado por taxis.

# Conexi�n con BigQuery

project<-'proyecto-r-227519' # Selecciona tu proyecto de Google.

sql<-'SELECT taxi_id, trip_seconds, trip_miles, trip_total FROM `bigquery-public-data.chicago_taxi_trips.taxi_trips`'

taxis<-query_exec(sql,project=project,use_legacy_sql = FALSE)

# Datos agregados por taxi

taxis_agregado<-aggregate(taxis[,2:4],by=list(taxis[,1]),sum)

# Se encuentran algunos NA's por lo cual es necesario eliminar las 
# observaciones de algunos taxis, al ser una poblaci�n tan grande
# no afectar� significativamente, en caso de ser una peque�a
# deber�a tomarse otro camino.

taxis_agregado<-taxis_agregado[which(complete.cases(taxis_agregado$trip_seconds) &
                                       complete.cases(taxis_agregado$trip_miles) &
                                       complete.cases(taxis_agregado$trip_total)),]

# Se utiliza fviz_nbclust para obtener el n�mero de clusters necesarios

fviz_nbclust(taxis_agregado[,2:4],FUNcluster = clara,method = "wss",
             diss = get_dist(taxis_agregado[,2:4],method = "manhattan"),k.max = 10)

# Se observa un valor bastante optimo en k=6, por lo que se utiliza dicho n�mero de clusters

# Se aplica CLARA, es importante precisar que la funci�n clara estandariza los datos con el par�metro
# stand, por lo que no es necesario estandarizarlos previamente, pamlike aplica el algoritmo PAM
# dentro de las muestras.

grupos<-clara(taxis_agregado[,2:4],k=6,metric = "manhattan", stand = TRUE,
              samples = 50, pamLike = TRUE)

# Del objeto pueden observarse los medoids, la funci�n objetivo (se busca minimizar
# las distancias de la m�trica l1 generalizada)

# Finalmente se pueden graficar los clusters mediante proyecciones de componentes 
# principales

fviz_cluster(object = grupos, ellipse.type = "t", geom = "point",
             pointsize = 2.5) +
  theme_bw() +
  labs(title = "CLARA") +
  theme(legend.position = "none")

# Donde se aprecia el efecto de la proyecci�n de los datos debida a la transformaci�n
# lineal que sufren debido a los componentes principales

