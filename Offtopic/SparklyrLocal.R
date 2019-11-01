list.of.packages <- c("ellipsis","backports","rstudioapi","sparklyr","devtools","nycflights13","Lahman","dplyr",
                      "ggplot2","DBI","dbplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(rstudioapi)
require(sparklyr)
require(devtools)
require(Lahman)
require(nycflights13)
require(dplyr)
require(ggplot2)
require(DBI)
require(dbplyr)
 
#devtools::install_github("rstudio/sparklyr")

# CONEXIÓN A SPARK
  conf<-list()
  conf$`sparklyr.cores.local` <- 4
  conf$`sparklyr.shell.driver-memory` <- "8G"
  conf$spark.memory.fraction <- 0.9
  
  sc <- spark_connect(master = "local",
                      config = conf)
  
## Se cargan algunas bases: iris, vuelos y bateos.

iris_tbl <- copy_to(sc, iris)
flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
batting_tbl <- copy_to(sc, Lahman::Batting, "batting")
src_tbls(sc)

flights_tbl %>% filter(dep_delay == 2)

### Algunos ejemplos 

# Aviones agrupados por el numero de cola, se agrega el numero de retrasos,
# la media de la distancia recorrida en vuelo y la media del tiempo de retraso en
# horas.

retrasos <- flights_tbl %>%
  group_by(tailnum) %>%
  summarise(count=n(),dist=mean(distance),delay=mean(arr_delay)) %>%
  filter(count>20, dist<2000,!is.na(delay)) %>%
  collect()

# Se grafica

ggplot(retrasos, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)+theme_bw()

#

colnames(flights)

## Distancia promedio recorrida dia por los aviones

dist_by_month<-flights %>%
  group_by(month) %>%
  summarise(promedio=sum(distance))

ggplot(dist_by_month,aes(x=as.factor(month),y=promedio))+
  geom_point()+theme_bw()+geom_line(aes(group=1))+labs(x="Mes",y="Distancia agregada",title = "Distancia total por mes")

## Almacenar en Spark y eliminar de R (ventas)

sales1<-sales

sales_tbl<-copy_to(sc,sales1)

remove(sales1)

ventas_por_tienda<-sales_tbl %>%
  group_by(store_name) %>%
  summarise(ventas=sum(sale_dollars),volumenl=sum(volume_sold_liters),botellas=sum(bottles_sold))

ggplot(ventas_por_tienda,aes(volumenl,ventas))+geom_point()+
  geom_smooth(col="red")+theme_bw()+labs(x="Volumen vendido por tienda (litros)", y="Venta en dolares")

### FUNCIONES VENTANA

batting_tbl %>%
  select(playerID, yearID, teamID, G, AB:H) %>%
  arrange(playerID, yearID, teamID) %>%
  group_by(playerID) %>%
  filter(min_rank(desc(H)) <= 2 & H > 0)

###### USO DE SQL

iris_sql<-dbGetQuery(sc,"SELECT * FROM iris LIMIT 10")
iris_sql

preview_vuelos<-dbGetQuery(sc,"SELECT * FROM flights LIMIT 10")

##### Guardar información de Spark en la memoria de R

iris_copy<-collect(iris)

##### SQL TRANSLATE 

dbplyr::translate_sql(if(x>5) "big" else "little")
dbplyr::translate_sql(case_when(x))

### MUESTRAS

sample_n(flights,10)
sample_frac(flights,0.1)

### MEMORIA PERSISTENTE A TRAVÉS DE UN PARQUET EN UN HDFS

spark_write_parquet(flights_tbl, "hdfs://hdfs.company.org:9000/hdfs-path/flights")

# Para leerla

spark_read_parquet(sc,"flights_tbl","hdfs://hdfs.company.org:9000/hdfs-path/flights")



