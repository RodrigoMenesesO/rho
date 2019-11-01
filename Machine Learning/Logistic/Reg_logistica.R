list.of.packages <- c("ggplot2","devtools","bigrquery","vcd","ROCR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(ggplot2)
require(devtools)
require(bigrquery)
require(vcd)
require(ROCR)

############### REGRESION LOGISTICA

# Modelo supervisado para obtener las probabilidades del tipo P(Y=k|X=x).
# Donde Y es una variable de clasificación (binaria o multinomial) y 
# X es una matriz de observaciones de n variables independientes.

# Las probabilidades son obtenidas a través de una codificación que realiza
# una función de enlace, normalmente la función que se emplea como enlace es
# la función sigmoidea.

### EJEMPLO

# Se utilizan los datos de Iowa Licor Sales obtenidos de google big query

project<-'proyecto-r-227519' # Usar proyecto propio de Google.

sql<-"SELECT * FROM `bigquery-public-data.iowa_liquor_sales.sales`"

sales<-query_exec(sql,project = project,use_legacy_sql = FALSE)

# Se genera una variable binaria que inspeccione si por tienda se ha superado el segundo cuartil
# del total de ventas por tienda o no.

agregado_retail_full<-aggregate(sales[,c("sale_dollars","bottles_sold","state_bottle_cost")],
                           by=list(sales$store_name),sum)
## 2do cuartil y definición de la meta

sq<-quantile(agregado_retail_full$sale_dollars,0.5)

meta_alcanzada<-c()
for(i in 1:nrow(agregado_retail_full)){
  meta_alcanzada[i]<-ifelse(agregado_retail_full$sale_dollars[i]>=sq,1,0)
}

agregado_retail_full$meta_alcanzada<-as.factor(meta_alcanzada)


### Conjunto de entrenamiento

muestra<-sample(1:nrow(agregado_retail_full),round(0.75*nrow(agregado_retail_full),0))

agregado_retail<-agregado_retail_full[muestra,]

### Conjunto de prueba

complemento<-(1:nrow(agregado_retail_full))[!1:nrow(agregado_retail_full) %in% muestra]

agregado_retail_train<-agregado_retail_full[complemento,]

## Se genera el modelo logístico, se estudiará si se alcanza la meta respecto a venta de botellas y al 
## costo estatal por botella.

modelo_log<-glm(as.factor(meta_alcanzada)~state_bottle_cost+bottles_sold,data = agregado_retail,family = "binomial")

confint(modelo_log)

## Evaluación del modelo

######## Likelihood ratio

dif_residuos<-modelo_log$null.deviance-modelo_log$deviance

df<-modelo_log$df.null-modelo_log$df.residual

p_value<-pchisq(dif_residuos,df=df,lower.tail = FALSE)

  p_value

summary(modelo_log)

## El modelo en conjunto y cada predictor son significativos.

## Predicciones (threshold: 0.5)

predicciones<-ifelse(modelo_log$fitted.values>0.5,1,0)
confusionm<-table(modelo_log$model$`as.factor(meta_alcanzada)`,predicciones,dnn=c("Valores reales","Predicciones"))
confusionm

mosaic(confusionm,shade = T,colorize=T,gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

## Predicciones correctas:

sum(diag(confusionm))/sum(confusionm)

# Realiza un 95% de predicciones correctas.

######### Se prueba el modelo con la información de entrenamiento
  
  predicciones<-predict.glm(modelo_log,newdata=agregado_retail_train[,3:4],type = "response")
  predicciones<-ifelse(predicciones>0.5,1,0)
  table(predicciones)

### Se analiza el rendimiento del modelo con relación a las predicciones.
  
pred<-prediction(predicciones,agregado_retail_train$meta_alcanzada)
rendimiento<-performance(pred,"tpr","fpr")

auc<-performance(pred,"auc")@y.values[[1]]

pd<-data.frame(fpr=unlist(rendimiento@x.values), tpr=unlist(rendimiento@y.values))

p<-ggplot(pd, aes(x=fpr, y=tpr))+ geom_line(colour="red")+
  labs(x="False Positive Rate",y="True Positive Rate")+
  ggtitle("Curva ROC para la regresión logística")+
  theme(plot.title=element_text(size=10))+
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")+
  annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
           label=paste("AUC =", round(auc, 2)))

## Se observa un gran rendimiento validado por la curva ROC de costo-beneficio

a<-table(agregado_retail_train$meta_alcanzada,predicciones)
a

sum(diag(a))/sum(a)
