list.of.packages <- c("curl","zip","googleCloudStorageR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Creación del bucket y asignación de claves de autenticación

Sys.setenv("GCS_DEFAULT_BUCKET" = "rhobucket",
           "GCS_AUTH_FILE" = choose.files(caption = "Cuenta de autenticación de GCS"))

require(googleCloudStorageR)

## EXPLORAR EL BUCKET

  # Buckets en el proyecto
  
  gcs_list_buckets("40831583190")

# Se selecciona el rhobucket
  
  gcs_get_bucket("rhobucket")

# Se enlistan los objetos existentes en el bucket
  
  gcs_list_objects("rhobucket")  

# Se observa que no existen objetos en el bucket.
  
## SUBIR OBJETOS
  
# Cuando son menos de 5MB
  
  gcs_upload(iris,type = "text/csv")
  
# Mayores a 5MB
  
  big_file<-"retail.csv"
  write.csv(retail, file = big_file) 

  upload<-gcs_upload(big_file)  
  
# ACTUALIZAR PERMISOS

# Todo usuario:
  
  gcs_update_object_acl("iris.csv",entity_type = "allUsers")

# Un usuario específico de google como dueño
  
  gcs_update_object_acl("iris.csv",entity = "roy.orellana.act@gmail.com",role="OWNER")

# Un grupo de google
  
  gcs_update_object_acl("iris.csv",entity = "grupo@actuarios.com",entity_type = "group") # NOT RUN
  
# Un dominio poseedor del objeto
  
  gcs_update_object_acl("iris.csv",entity = "actuarios.com",entity_type = "domain",role = "OWNER")

# ELIMINAR OBJETOS, BUCKETS

  gcs_delete_object("iris.csv")
  # gcs_delete_bucket()

# PERMISOS ACTUALES DE LOS OBJETOS Y ROLES
  
  gcs_get_object_acl("iris.csv",entity = "roy.orellana.act@gmail.com")

  gcs_get_object_acl("iris.csv",entity_type = "allUsers")  

# SESIONES DE R
  require(lubridate)
  
  gcs_save_image(file = paste0(".Rdata",today())) # Guardar el .RData en la nube
  gcs_load() # Correr el .RData.
  
# GUARDAR OBJETOS ESPECIFICOS DEL ENVIROMENT
  
  hola<-"holamundo"
  gcs_save("hola",file = "hola.RData")
  
  remove(hola)
  hola
  
  gcs_load("hola.RData")
  hola
  
  gcs_delete_object("hola.RData")
  
# CREAR LINKS DE DESCARGA 
  
  url<-gcs_download_url("iris.csv")
  url  
  