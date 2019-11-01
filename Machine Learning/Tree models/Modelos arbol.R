list.of.packages <- c("devtools","bigrquery","ggplot2","randomForest","mlbench","caret","e1071",
                      "tidyverse","ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(devtools)
require(bigrquery)
require(ggplot2)
require(randomForest)
library(mlbench)
library(caret)
library(e1071)
library(tidyverse)
library(ggpubr)

### ARBOLES DE REGRESIÓN Y CLASIFICACIÓN

# Se excluyen modelos simples que no impliquen
# bagging o boosting.

######## ALGORITMOS DE ENSAMBLADO

# Error: varianza+sesgo+error puramente aleatorio

# Bagging: multiples arboles en paralelo
# con la máxima varianza posible que
# contribuyen con sus estimaciones individuales
# a un valor promedio que se toma como
# estimación final.
# A través del bootstrapping sobre el conjunto
# de entrenamiento se busca no tocar el sesgo del
# conjunto de árboles de reg/clas pero reducir
# sistematicamente la varianza para con ello
# reducir el error del algoritmo.

##################### RANDOM FOREST

# Bagging decorrelacionado donde todos los predictores son considerados
# y se evita dominancia paramétrica.

# Se toman m predictores aleatoriamente de todos los considerados
# y a partir de ellos se aplica bagging, el valor de m es inversamente
# proporcional a lo correlacionados que puedan estar los arboles individuales
# debido a dominancia paramétrica.

# Este algoritmo de ensamble no sufre problemas de overfitting

######### EJEMPLO

# Se hará uso del retail de licor de Iowa (Iowa Licor Sales).

project<-'proyecto-r-227519'# Usar proyecto propio.

sql<-"SELECT * FROM `bigquery-public-data.iowa_liquor_sales.sales`"

sales<-query_exec(sql,project = project,use_legacy_sql = FALSE)

agregado_retail_full<-aggregate(sales[,c("sale_dollars","bottles_sold","state_bottle_cost")],
                                by=list(sales$store_name),sum)

### Conjunto de entrenamiento (75%)

muestra<-sample(1:nrow(agregado_retail_full),round(0.75*nrow(agregado_retail_full),0))

agregado_retail<-agregado_retail_full[muestra,]

### Conjunto de prueba (25%)

complemento<-(1:nrow(agregado_retail_full))[!1:nrow(agregado_retail_full) %in% muestra]

agregado_retail_train<-agregado_retail_full[complemento,]


# Optimización de hiperparámetros: <<<<<<<<<<<<<<<<<<<<<

# Numero de parámetros a tomar (m)

tuning_rf_mtry <- function(df, y, ntree = 500){
  # Esta función devuelve el out-of-bag-MSE de un modelo RandomForest en función
  # del número de predictores evaluados (mtry)
  
  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   ntree = número de árboles creados en el modelo randomForest
  require(dplyr)
  max_predictores <- ncol(df) - 1
  n_predictores   <- rep(NA, max_predictores)
  oob_mse         <- rep(NA, max_predictores)
  for (i in 1:max_predictores) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = i, ntree = ntree)
    n_predictores[i] <- i
    oob_mse[i] <- tail(modelo_rf$mse, n = 1)
  }
  results <- data_frame(n_predictores, oob_mse)
  return(results)
}

mtry<-tuning_rf_mtry(df=agregado_retail[,2:4],y="sale_dollars")

ggplot(data = mtry, aes(x = n_predictores, y = oob_mse)) +
  scale_x_continuous(breaks = mtry$n_predictores) +
  geom_line() +
  geom_point() +
  geom_point(data = mtry %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs mtry",
       x = "nº predictores empleados") +
  theme_bw()

# Se deben emplear ambas variables

# Numero minimo de observaciones por nodo terminal

tuning_rf_nodesize <- function(df, y, size = NULL, ntree = 500){
  # Esta función devuelve el out-of-bag-MSE de un modelo randomForest en función
  # del tamaño mínimo de los nodos terminales (nodesize).
  
  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   sizes = tamaños evaluados
  #   ntree = número de árboles creados en el modelo randomForest
  
  require(dplyr)
  if (is.null(size)){
    size <- seq(from = 1, to = nrow(df), by = 5)
  }
  oob_mse <- rep(NA, length(size))
  for (i in seq_along(size)) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = 5, ntree = ntree,
                              nodesize = i)
    oob_mse[i] <- tail(modelo_rf$mse, n = 1)
  }
  results <- data_frame(size, oob_mse)
  return(results)
}

nodesize<-tuning_rf_nodesize(df=agregado_retail[,2:4],y="sale_dollars",size = c(1:20))

ggplot(data = nodesize, aes(x = size, y = oob_mse)) +
  scale_x_continuous(breaks = nodesize$size) +
  geom_line() +
  geom_point() +
  geom_point(data = nodesize %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs nodesize",
       x = "nº observaciones en nodos terminales") +
  theme_bw()

# Se toma como mínimo una observación

# Numero de arboles

modelo_randomforest <- randomForest(sale_dollars ~ ., data = agregado_retail[,2:4],
                                    mtry = 2 , ntree = 500, nodesize = 3,
                                      importance = TRUE)

oob_mse <- data.frame(oob_mse = modelo_randomforest$mse,
                      arboles = seq_along(modelo_randomforest$mse))
ggplot(data = oob_mse, aes(x = arboles, y = oob_mse )) +
  geom_line() +
  labs(title = "Evolución del out-of-bag-error vs número árboles",
       x = "nº árboles") +
  theme_bw()

# Alrededor de los 400 se estabiliza

### MODELO

modelo_randomforest <- randomForest(sale_dollars ~ ., data = agregado_retail[,2:4],
                                    mtry = 2 , ntree = 400, nodesize = 1,
                                    importance = TRUE)
## Predecir

predicciones<-predict(modelo_randomforest,newdata = agregado_retail_train)

## MSE

test_mse <- mean((predicciones - agregado_retail_train$sale_dollars)^2)

## Importancia de los predictores

importancia_pred <- as.data.frame(importance(modelo_randomforest, scale = TRUE))
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
p1 <- ggplot(data = importancia_pred, aes(x = reorder(variable, `%IncMSE`),
                                          y = `%IncMSE`,
                                          fill = `%IncMSE`)) +
  labs(x = "variable", title = "Reducción de MSE") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia_pred, aes(x = reorder(variable, IncNodePurity),
                                          y = IncNodePurity,
                                          fill = IncNodePurity)) +
  labs(x = "variable", title = "Reducción de pureza") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggarrange(p1, p2)

# Boosting: ajusta secuencialmente modelos/alg sencillos
# llamados week learners de forma que cada modelo/alg
# subsecuente aprende del anterior y al final al igual
# que en el bagging se toma el valor promedio de las 
# predicciones dadas por todos ellos.

#AdaBoosting; Gradient Boosting; Stochastic Gradient Boosting


