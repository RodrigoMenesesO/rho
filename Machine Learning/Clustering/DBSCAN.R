
list.of.packages <- c("ggplot2","cluster","mclust","factoextra","dendextend","ggpubr","tidyverse","kableExtra",
                      "AnalyzeTS","dplyr","gridExtra","devtools","bigrquery","fpc","dbscan")
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
require(fpc)
require(dbscan)

#### DENSITY-BASED SPATIAL CLUSTERING FOR APPLICATIONS WITH NOISE (DBSCAN)

# Permite formar clusters a partir de la densidad de observaciones en las 
# vecindades de algún punto inicial.

# La separación de los clusters se genera a través de zonas vacías (sin
# observaciones) o con ruido (solo algunas observaciones distantes sin 
# mucha similitud).

## DEFINICIONES

# Requiere dos parámetros dados por el usuario:

## EPSILON: tamaño de la vecindad

## Minimum points (minPts): número de puntos mínimos 
# en la vecindad como criterio de selección para
# las siguientes definiciones

# Cada observación del conjunto de datos puede
# ser clasificada con los parámetros anteriores

## Punto interior: observación que tiene en su vecindad
# un número mayor o igual al minPts

## Punto frontera: Observación que en su vecindad tiene
# menor número de observaciones que minPts pero
# está contenido en la vecindad de un Core point

## Ruido: toda observación que no sea punto interior
# así como tampoco sea punto frontera

## ENLACES

## Directamente alcanzable: sean dos observaciones A y B
# se dice que A es directamente alcanzable desde B si
# A pertenece a la vecindad de B y B es un punto interior.

## Alcanzable: sean dos observaciones A y B, se dice que 
# A es alcanzable desde B si existe un conjunto de puntos 
# interiores cuyas vecindades se intersectan y corren 
# desde B hasta A.

## Densamente conectadas: sean A, B y C tres observaciones, 
# entonces se dice que A y B están densamente conectadas
# si C es un punto interior y A,B son alcanzables desde C.

## Algoritmo DBSCAN

# 1. Categorizar cada observación como punto interior, frontera
# o ruido

# 2. Se selecciona alguna observación que sea punto interior, encontrar 
# todas las observaciones con las que esté densamente conectadas
# y formar un cluster

# 3. Se repite 2 hasta que todos los puntos interiores sean visitados
# (algunos algoritmos visitarán todos los puntos sin importar su class)

# 4. Aquellas observaciones que no entren como puntos interiores y
# que no hayan podido ser densamente conectadas con las observaciones
# de algún cluster se les marca como ruido.

#### Teorema:

# Todas las observaciones que forman parte de un mismo cluster construido a partir
# de DBSCAN están densamente conectadas. Por lo tanto, si una observación es 
# densamente alcanzable desde cualquier observación de un cluster, entonces dicha
# observación también pertenece al cluster.

# Prueba: por definición.

######## EJEMPLO

# Se utiliza el conjunto de datos de taxis de Chicago, usando tiempo, distancia y costo total

# Conexión con BigQuery

project<-'proyecto-r-227519' # Selecciona tu proyecto de Google.

sql<-'SELECT taxi_id, trip_seconds, trip_miles, trip_total FROM `bigquery-public-data.chicago_taxi_trips.taxi_trips`'

taxis<-query_exec(sql,project=project,use_legacy_sql = FALSE)

# Datos agregados por taxi

taxis_agregado<-aggregate(taxis[,2:4],by=list(taxis[,1]),sum)

# Se encuentran algunos NA's por lo cual es necesario eliminar las 
# observaciones de algunos taxis, al ser una población tan grande
# no afectará significativamente, en caso de ser una pequeña
# debería tomarse otro camino.

taxis_agregado<-taxis_agregado[which(complete.cases(taxis_agregado$trip_seconds) &
                                       complete.cases(taxis_agregado$trip_miles) &
                                       complete.cases(taxis_agregado$trip_total)),]

# Se toma una muestra aleatoria de 3000 observaciones y se reescalan

muestra<-sample(1:nrow(taxis_agregado),3000)
muestra_taxis_agregado<-taxis_agregado[muestra,]

muestra_taxis_agregado[,2:4]<-scale(muestra_taxis_agregado[,2:4])

# Selección del valor óptimo de epsilon



# Se encuentra el valor óptimo de épsilon

# Al ser una muestra pequeña se usan como minPts el valor de 5

dbscan::kNNdistplot(muestra_taxis_agregado[,2:4],5)

# Se encuentra un punto de inflexión alrededor del valor 2, por lo que se toma como 
# tamaño de la vecindad

dbscan_c<-fpc::dbscan(muestra_taxis_agregado[,2:4],eps = 2,MinPts = 5)

head(dbscan_c)

# Se visualizan los datos

fviz_cluster(object = dbscan_c, data = muestra_taxis_agregado[,2:4], stand = FALSE,
             geom = "point", ellipse = FALSE, show.clust.cent = FALSE,
             pallete = "jco") +
  theme_bw() +
  theme(legend.position = "bottom")

# Se aprecia un comportamiento similar al observable cuando se aplicó
# HC, puesto que los datos tienen áreas de gran densidad, en el caso de HC
# esto llevó a los algoritmos a desbalancear los clusters, en este caso, 
# todo lo que no pertenence a dicho cluster grande se considera ruido.

# Aunque la representación se ha realizado a partir de una transformación
# lineal por componentes principales, es notorio como incluso las proyecciones
# muestran esas grandes densidades en la región espacial de los datos.


