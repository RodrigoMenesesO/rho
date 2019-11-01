list.of.packages <- c("devtools","bigrquery")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(devtools)
require(rlang)
require(bigrquery)

project<-'proyecto-r-227519' # Tu proyecto de Google

sql<-"SELECT * FROM `bigquery-public-data.iowa_liquor_sales.sales`" # Consulta SQL

sales<-query_exec(sql,project = project,use_legacy_sql = FALSE) # Conexión

