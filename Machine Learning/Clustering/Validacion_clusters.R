
list.of.packages <- c("ggplot2","cluster","mclust","factoextra","dendextend","ggpubr","tidyverse","kableExtra",
                      "AnalyzeTS","dplyr","gridExtra","devtools","bigrquery","fpc","dbscan", "clustertend",
                      "clValid")
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


### Validación de clusters

# Validar la existencia y veracidad de los clusters es importante para
# no generar agrupaciones que realmente no existen, es por ello que es
# necesario hacer uso de pruebas estadísticas para no caer en 
# grupos inexistentes (tenga en mente que cualquier método de clustering
# casi seguramente siempre generará clusters aunque estos no sean reales,
# finalmente es el objetivo de los algoritmos)

########### COMPARACIÓN DE LOS DATOS CONTRA UN HIPERCUBO UNIFORME

# A través del estadístico de Hopkins se puede probar si existe una
# tendencia de los datos a estar distribuidos cercanamente como se 
# distribuye un hipercubo uniforme (cubo o cuadrado según las dimensiones)

require(clustertend)

#### EJEMPLO: VALIDAR TAXIS

# Se utiliza el conjunto de datos de taxis de Chicago, usando tiempo, distancia y costo total

# Conexión con BigQuery

project<-'proyecto-r-227519' # Selecciona tu proyecto de Google

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

muestra<-sample(1:nrow(taxis_agregado),300)
muestra_taxis_agregado<-taxis_agregado[muestra,]

muestra_taxis_agregado[,2:4]<-scale(muestra_taxis_agregado[,2:4])

# Se valida si los datos se asemejan a un cubo uniforme

hop<-hopkins(muestra_taxis_agregado[,2:4],n=nrow(muestra_taxis_agregado)-1)

# Valores cercanos a 0.5 indican la semejanza con un cubo uniforme, en ese caso
# los grupos que se puedan generar a partir del clustering serían inválidos.

hop

# Dado el valor del estadístico existe una fuerte posibilidad de que realmente
# hayan grupos, pues no hay una semejanza con un cubo uniforme

### VISUAL ASSESSMENT OF CLUSTER TENDENCY (VAT)

# Permite mediante visualización identificar si realmente existen grupos
# apartir de las matrices de disimiliridades

# 1. Se calcula la matriz de distancias

# 2. Se ordena la matriz de tal forma que 
# las observaciones similares se encuentren
# juntas

# 3. Se muestra gráficamente la matriz de distancias
# de forma ordenada con un gradiente de color para
# el valor de las distancias, si realmente existen 
# agrupaciones subyacentes inexploradas, se podrán
# observar patrones cuadrados.

distancias<-dist(muestra_taxis_agregado[,2:4],method = "euclidian")

fviz_dist(dist.obj = distancias, show_labels = FALSE) +
  labs(title = "Distancias") + theme(legend.position = "bottom")

# Se puede apreciar la existencia de un conjunto de datos fuertemente 
# dominante, lo que podría sugerir que es inútil el uso de clustering
# para el conjunto de datos.


############# SELECCIÓN DE CLUSTERS

muestra<-sample(1:nrow(taxis_agregado),100)
muestra_taxis_agregado<-taxis_agregado[muestra,]

muestra_taxis_agregado[,2:4]<-scale(muestra_taxis_agregado[,2:4])

# Elbow: reducir la suma de distancias intra-clusters (wss)

fviz_nbclust(x = muestra_taxis_agregado[,2:4], FUNcluster = kmeans, method = "wss", k.max = 15) +
  labs(title = "Número óptimo de clusters")

# Se selecciona el número de clusters a partir del cual ya no se reduce significativamente
# la varianza intra grupos

# Silueta: cuantificar cuan buena es la asignación de cada observación
# conforme a su similitud con las demás en el cluster frente a las de
# otros clusters

fviz_nbclust(x = muestra_taxis_agregado[,2:4], FUNcluster = kmeans, method = "silhouette", k.max = 15) +
  labs(title = "Número óptimo de clusters")

# Se selecciona el valor que maximice dicha medición de similaridad

# GAP: estadístico que busca alejar la estructura de los clusters
# de la que se conseguiría a través de una distribución uniforme, 
# similar al estadístico de Hopkins en su fundamento.

fviz_nbclust(x = muestra_taxis_agregado[,2:4], FUNcluster = kmeans, method = "gap_stat", nboot = 500,
             k.max = 15, verbose = FALSE, nstart = 50) +
  labs(title = "Número óptimo de clusters")

# En este caso se elige el valor de K que maximiza el GAP
# de tal forma que la estructura de los clusters sea lo más distinta
# a una distribución uniforme

########################### CALIDAD DE LOS CLUSTERS

## VALIDACIÓN INTERNA: Se emplea información contenida en el proceso
# del clustering para evaluar la bondad de las agrupaciones

# Se utiliza cuán homogeneos y cuán separados están los clusters. Es decir,
# cuan pequeña es la distancia interna de las observaciones en los clusters
# y cuan alejados están entre ellos.

# Silhouette width: cuantifica la similitud de las observaciones de los clusters
# con las demás que pertenecen al mismo cluster y se contrasta estas similitudes
# contra las de otros clusters.

c_kmeans <- eclust(x = muestra_taxis_agregado[,2:4], FUNcluster = "kmeans", k = 2, seed = 123,
                      hc_metric = "euclidean", nstart = 50, graph = FALSE)

fviz_silhouette(sil.obj = c_kmeans, print.summary = TRUE, palette = "jco",
                ggtheme = theme_classic()) 

# Se puede concluir que efectivamente es ineficiente el uso de más
# de un cluster, realmente todas las observaciones son muy parecidas.
# Como se ha apreciado anteriormente.

# Indice de Dunn: análisis del peor caso, permite sistemáticamente
# deshacerse de clusters malos.

indices<-cluster.stats(d=dist(muestra_taxis_agregado[,2:4],method = "euclidian"),
                       clustering = c_kmeans$cluster)

indices$dunn

# Existe entonces un cluster muy malo, por lo que se reduce a un solo cluster 
# el análisis, es evidente que tomar más de un cluster no resulta ciertamente
# favorable.

# Estabilidad: nada recomendado en bigdata

## VALIDACIÓN EXTERNA: set de validaciones o información externa
# para medir la bondad del ajuste.

# Aplicar supervised

## SIGNIFICANCIA DE LOS CLUSTERS: calcular la probabilidad de que los
# clusters sean generados de manera aleatoria.

# Aplicar bootstraping

######################## HEATMAPS

# Los mapas de calor son representaciones matriciales en las cuales
# para cada posición de la matriz, en vez de mostrarse su valor numérico
# se muestra un gradiente de color.

heatmap(x = as.matrix(muestra_taxis_agregado[,2:4]), scale = "none",
        distfun = function(x){dist(x, method = "euclidean")},
        hclustfun = function(x){hclust(x, method = "average")},
        cexRow = 0.7)

################# SELECCIÓN DEL MEJOR MÉTODO DE CLUSTERING

library(clValid)

comparacion <- clValid(obj = muestra_taxis_agregado[,2:4], nClust = 2:6,
                       clMethods = c("hierarchical", "kmeans", "pam"),
                       validation = c("stability", "internal"))

# Se observa que el ganador es el cluster jerárquico con corte a dos clusters.

