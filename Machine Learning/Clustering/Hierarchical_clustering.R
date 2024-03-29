
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

# Clustering jer�rquico

# Presenta una alternativa al clustering particional, pues
# el usuario no necesita proveer el n�mero de clusters por 
# generar.

# Existen dos algoritmos principales dentro de este tipo
# de clustering: aglomerativo y mediante divisiones.

# El algoritmo aglomerativo define cada observaci�n como un
# cluster semilla y mediante distancias une los clusters y
# los fusiona para generar uno solo, esto es similar a como
# un arbol nace de sus semillas que cada vez se acercan m�s entre
# ellas hasta formar la base del �rbol.

# Este proceso lo hace iterativamente hasta un l�mite (sin el
# la estructura final ser�a un solo cluster conteniendo todas
# las observaciones)

# Sin embargo, despu�s de cierto n�mero de iteraciones pueden
# ya no compararse observaciones, si no, clusters, por lo que es 
# importante comprender c�mo se define la similitud entre clusters.

# Dicha similitud se hace a trav�s de algoritmos llamados enlaces.

### Enlace Complete o Maximum: se calculan todas las distancias (con la
# distancia base definida) entre ambos clusters y la m�xima de ellas se
# toma como distancia entre clusters.

### Enlace single o minimum: similar a la anterior pero se selecciona
# la menor de las distancias

### Average: se calculan todas las distancias entre las observaciones de
# los clusters y se selecciona el promedio de las distancias.

### Centroid: se calcula el centroide de ambos clusters y se selecciona
# la distancia entre ellos. 

### Ward: Se basa en seleccionar como cluster con el cual fusionarse a 
# aquel que genere el valor �ptimo de la funci�n objetivo, por ejemplo
# se puede buscar minimizar la varianza intraclusters tras la fusi�n, por
# lo cual se hacen todas las combinaciones posibles por pares.

# Otro algoritmo es Divisive (por divisiones), cuyo nombre completo es
# DIvisive ANAlysis Clustering (DIANA), este algoritmo comienza con un
# gran cluster que se divide en m�s peque�os.

# El criterio de divisi�n son distancias nuevamente, el primer cluster
# es el dado por el di�metro m�s grande entre pares de observaciones, 
# se selecciona la observaci�n m�s dispar a todas l�s dem�s dentro
# de dicho cluster (mayor promedio de distancias respecto a las dem�s
# observaciones), esta observaci�n se selecciona y se forma el nuevo
# cluster a partir de ella dependiendo de si las observaciones
# est�n m�s pr�ximas al cluster o a lo dem�s de la partici�n.

# Dividiendo las observaciones de dicha forma, se forman dos clusters.

# Este proceso se hace de manera iterativa hasta generar una cierta
# cantidad de clusters.

# Es valioso para el uso del clustering jer�rquico la aplicaci�n 
# de dendogramas.

############## EJEMPLO AGLOMERATIVO

# Se hace uso de los datos de taxis de Chicago y sus variables, tiempo, distancia y costo total

# Conexi�n con BigQuery

project<-'proyecto-r-227519' # Selecciona tu proyecto de Google

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

# Se selecciona una muestra de 300 observaciones para ejemplificar.

muestra<-sample(1:nrow(taxis_agregado),300,replace = FALSE)

muestra_taxis_agregado<-taxis_agregado[muestra,]
muestra_taxis_agregado[2:4]<-scale(muestra_taxis_agregado)

# Se calcula la matriz de distancias

md<-dist(muestra_taxis_agregado[,2:4], method="manhattan")

hc_completo<-hclust(d=md,method = "complete")
hc_average<-hclust(d=md,method="average")
hc_single<-hclust(d=md,method = "single")

plot(hc_completo,ylab="",xlab="",cex=0.1,main = "HC Completo")
plot(hc_average,ylab="",xlab="",cex=0.1,main = "HC Average")
plot(hc_single,ylab="",xlab="",cex=0.1,main = "HC Single")

# Se selecciona el mejor linkage respecto a la matriz de distancias

cor(md,cophenetic(hc_completo))
cor(md,cophenetic(hc_average))
cor(md,cophenetic(hc_single))

# Se selecciona como linkage al promedio de las distancias

# Se corta el dendograma a trav�s de la observaci�n de las distancias
# verticales del dendograma, en este caso se seleccionan 3 clusters, por lo
# que se corta alrededor del 10

cut_HC_average<-cutree(hc_average,3)

# Se observan el n�mero de observaciones por clusters

table(cut_HC_average)

# Se observa el desbalance del tama�o de los clusters debido a un 
# error al seleccionar las m�tricas, podr�a optarse por usar
# maximo y minimo, aunque quiz� los datos en s� sean tan abrumadoramente
# similares respecto a alguna de las variables y el efecto del sesgo
# sea inneludible a trav�s de HC

### EJEMPLO DIANA

md<-dist(muestra_taxis_agregado[,2:4],"euclidian")
HC_diana<-diana(md,diss = TRUE,stand = FALSE) # Ya se reescal�

fviz_dend(x = HC_diana, cex = 0.5) +
  labs(title = "Diana",
       subtitle = "Distancia eucl�dea")

# Recuerde que no se hace uso de ninguna funci�n enlace

# Por inspecci�n se decide cortar el dendograma en 4 clusters

cut_diana<-cutree(HC_diana,4)

# Se observa la distribuci�n de las observaciones a trav�s de 
# los clusters

table(cut_diana)

# Se continua percibiendo la existencia de una caracteristica
# muy dominante que sesga las reglas de asociaci�n y determina
# muchas observaciones en un cluster, se concluye nuevamente
# la posibilidad de que los datos en s� conduzcan a esa tendencia.

# Finalmente se puntualiza que al conjunto de datos agregados
# por taxi se le ha aplicado un muestreo aleatorio simple
# con lo cual es posible que tambi�n halla una influencia debida al
# sesgo de muestreo.

################################ HIERARCHICAL KMEANS ###############################

# Es una combinaci�n del clustering jer�rquico y k-means

# Resulta de aplicar clustering jer�rquico y posteriormente k means tomando como
# base los centroides de los clusters adecuaddos generados por HC para usarlos como
# centroides semilla de k-means.

# Recordando que en aglomerativo el mejor enlace fue el promedio de las distancias
# entre observaciones de clusters, se utiliza dicho enlace, adem�s el n�mero
# de clusters que se encontraron adecuados fueron 3.

HC_kmeans<-hkmeans(muestra_taxis_agregado[,2:4],hc.metric = "euclidian",
                   hc.method = "average",k=3)

fviz_cluster(object = HC_kmeans, pallete = "jco", repel = TRUE) +
  theme_bw() + labs(title = "Hierarchical k-means")



