###-libs----------------------------------------------------------------------
list.of.packages <- c("ggplot2", "rvest","magrittr","stringr","tm","SnowballC",
                    "wordcloud","RColorBrewer","rowr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(ggplot2)
require(rvest)
require(magrittr)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
require(rowr)
library(RColorBrewer)

################## PARÁMETROS ##############################################################

web.page<-c('https://www.ittux.edu.mx/index.php/carreras-ittux/licenciatura-administracion')


titulo.oferta<-c("licenciatura","curso","maestria","maestría","doctorado","seminario","diplomado",
                 "certificacion","certificación","programa")
tipo.oferta<-web.page
web.site<-c("sitio web","web","página web")
encargado<-c("titular","encargado","coordinador","coordinación","coordinacion","reponsable","maestro",
             "imparte")
vigencia<-c("vigencia")
direccion<-c("telefono","contacto",'direccion',"estado","cp")
institucion<-c("escuela","instituto","institucion","facultad","centro","colegio")
tipo.institucion<-c("publica","pública","privada")
adscripcion<-c("departamento")
#
socios<-c("socios","colaboradores","colaborador","financiador","financiadora","patrocinio",
          "patrocinador")
web.institución<-c("sitio web","web","página web")
nivel<-c("pregrado","licenciatura","tecnico","técnico","actualización","actualizacion",
         "educación continua","educacion continua","diplomado")
duracion<-c("duracion","duración","periodo","años","meses","mes","año")
objetivo<-c("objetivo","plan","mision","visión","vision","misión")
pob.objetivo<-c("perfil","población","poblacion","ingreso")
c.egreso<-c("egreso","perfil","conocimientos","adquiridos")
h.egreso<-c("egreso","habilidades","destrezas")
campo.t<-c("campo","trabajo","laboral","area")
reconocimiento<-c("reconocimiento","diploma","certificación","certificacion","titulo",
                  "título","grado","constancia","participación","participacion")
#
unidad.entrenamiento<-c("horas","dedicadas","dedicación","créditos","creditos",
                        "entrenamiento")
numero.entrenamiento<-c("numero","creditos")
fechas<-c("calendario","periodicidad","frecuencia","inicio","finalización","finalizacion")
temporalidad<-c("tiempo","completo","parcial","fin de semana","nocturno","vespertino",
                "matutino")
idioma<-c("idioma","idiomas","lengua","lenguas","dialecto","dialectos")
admision<-c("criterios","admisión","admision","acceso","nuevo ingreso")
examen<-c("examen","admisión","admision","cupo","limite","edad")
costos<-c("costo","pago","mensualidad","anualidad","pago","pagos","financiamiento")
facilidades<-c("becas","financiamiento","apoyo","subsidio")
politicas.ingreso<-c("discapacidad","minoria","minoría","etnias","bajos recursos",
                     "indigenas","indígenas")
#
tipo.programa<-c("presencial","semipresencial","virtual","mixto")
plataforma<-c("plataforma","canal","medio")
asignaturas<-c("teoria","conceptos","practica","metodología","metodologia","análisis",
               "analisis","negociación","negociacion","solucion de problemas")
practica.final<-c("practica","final","proyecto","exposicion","tesis","tesina")
evaluacion<-c("evaluacion","certificado")
innovacion<-c("innovacion","innovación","vanguardia","novedoso","nuevo")
suster<-c("desarrollo sustentable","desarrollo rural","sustentabilidad","sustentable",
          "identidad cultural","identidad")
m1<-c("modulo 1","parte 1","sección 1","seccion 1")
m2<-c("modulo 2","parte 2","sección 2","seccion 2")
m3<-c("modulo 3","parte 3","sección 3","seccion 3")
m4<-c("modulo 4","parte 4","sección 4","seccion 4")
plan<-c("plan de estudios","curricula","currícula")
estructura.plan<-c("plan de estudios particular","retícula","reticula")
certificacion<-c("certificacion externa","certificación externa","certificado extra",
                 "extras","extra")
g.innovacion<-c("innovacion","innovación")
observaciones<-c("observaciones")
buenas.practicas<-c("buenas practicas","buenas prácticas","entrevista","profundizar",
                    "profundice")


###################################### INICIO #################################################

page<-read_html(web.page)

############### FUNCIONES ##############################################################

############## COLAPSA EL TEXTO EN UN SOLO VECTOR ############################

collapse<-function(text){
  for(i in 1:length(text)){
    if(i==1){t<-text[i]
    }else{
      t<-paste(t,text[i])
    }
  }
  return(t)
}

############## LIMPIA EL VECTOR COLAPSADO ##########################################

clean<-function(text){
  t<-tolower(text)
  t<-gsub('[[:digit:]]+', '', t)
  t<-gsub("[[:punct:]]", " ", t)
}

############## CONVIERTE EN ESPACIO ###################################################

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

#########################################################################################

############# DESCOMPOSICIÓN DE LA PÁGINA WEB ###########################################

################# h1

page %>%
  html_nodes("h1")

h1<-page %>%
  html_nodes("h1") %>%
  html_text()

################# h2

page %>%
  html_nodes("h2")

h2<-page %>%
  html_nodes("h2") %>%
  html_text()

################# p nodes

page %>%
  html_nodes("p")

p<-page %>%
  html_nodes("p") %>%
  html_text()

################ ul nodes

ul<-page %>%
  html_nodes("ul") %>%
  html_text()

################ li nodes

li<-page %>%
  html_nodes("li")%>%
  html_text()

################ ol nodes
ol<-page %>%
  html_nodes("ol")%>%
  html_text()

################ div nodes
div<-page %>%
  html_nodes("div")%>%
  html_text()

######################### Limpieza ########################################################

text_h1<-h1%>%
  str_replace_all(pattern = "\n", replacement = " ") %>%
  str_replace_all(pattern = "[\\^]", replacement = " ") %>%
  str_replace_all(pattern = "\"", replacement = " ") %>%
  str_replace_all(pattern = "\\s+", replacement = " ") %>%
  str_replace_all(pattern = '"', replacement = " ") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_trim(side = "both")

text_h2<-h2%>%
  str_replace_all(pattern = "\n", replacement = " ") %>%
  str_replace_all(pattern = "[\\^]", replacement = " ") %>%
  str_replace_all(pattern = "\"", replacement = " ") %>%
  str_replace_all(pattern = "\\s+", replacement = " ") %>%
  str_replace_all(pattern = '"', replacement = " ") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_trim(side = "both")

text_p<-p%>%
  str_replace_all(pattern = "\n", replacement = " ") %>%
  str_replace_all(pattern = "[\\^]", replacement = " ") %>%
  str_replace_all(pattern = "\"", replacement = " ") %>%
  str_replace_all(pattern = "\\s+", replacement = " ") %>%
  str_replace_all(pattern = '"', replacement = " ") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_trim(side = "both")

text_ul<-ul%>%
  str_replace_all(pattern = "\n", replacement = " ") %>%
  str_replace_all(pattern = "[\\^]", replacement = " ") %>%
  str_replace_all(pattern = "\"", replacement = " ") %>%
  str_replace_all(pattern = "\\s+", replacement = " ") %>%
  str_replace_all(pattern = '"', replacement = " ") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_trim(side = "both")

text_li<-li%>%
  str_replace_all(pattern = "\n", replacement = " ") %>%
  str_replace_all(pattern = "[\\^]", replacement = " ") %>%
  str_replace_all(pattern = "\"", replacement = " ") %>%
  str_replace_all(pattern = "\\s+", replacement = " ") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_replace_all(pattern = '"', replacement = " ") %>%
  str_trim(side = "both")

text_ol<-ol%>%
  str_replace_all(pattern = "\n", replacement = " ") %>%
  str_replace_all(pattern = "[\\^]", replacement = " ") %>%
  str_replace_all(pattern = "\"", replacement = " ") %>%
  str_replace_all(pattern = "\\s+", replacement = " ") %>%
  str_replace_all(pattern = '"', replacement = " ") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_trim(side = "both")

text_div<-div%>%
  str_replace_all(pattern = "\n", replacement = " ") %>%
  str_replace_all(pattern = "[\\^]", replacement = " ") %>%
  str_replace_all(pattern = "\"", replacement = " ") %>%
  str_replace_all(pattern = "\\s+", replacement = " ") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_replace_all(pattern = '"', replacement = " ") %>%
  str_trim(side = "both")

################################################# DIV ######################################
tdiv<-as.character(tolower(text_div))
tol<-as.character(tolower(text_ol))
tli<-as.character(tolower(text_li))
tul<-as.character(tolower(text_ul))
tp<-as.character(tolower(text_p))
th2<-as.character(tolower(text_h2))
th1<-as.character(tolower(text_h1))

resultado<-c()
for (i in 1:length(titulo.oferta)) {
  resultado1<-as.character(tdiv[(grep(titulo.oferta[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
anexo<-data.frame(Titulo.oferta=resultado)

resultado<-c()
for (i in 1:length(titulo.oferta)) {
  resultado1<-as.character(tdiv[(grep(titulo.oferta[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a1<-resultado

resultado<-c()
for (i in 1:length(tipo.oferta)) {
  resultado1<-as.character(tdiv[(grep(tipo.oferta[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a2<-resultado

resultado<-c()
for (i in 1:length(web.site)) {
  resultado1<-as.character(tdiv[(grep(web.site[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a3<-resultado

resultado<-c()
for (i in 1:length(encargado)) {
  resultado1<-as.character(tdiv[(grep(encargado[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a4<-resultado

resultado<-c()
for (i in 1:length(vigencia)) {
  resultado1<-as.character(tdiv[(grep(vigencia[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a5<-resultado

resultado<-c()
for (i in 1:length(direccion)) {
  resultado1<-as.character(tdiv[(grep(direccion[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a6<-resultado

resultado<-c()
for (i in 1:length(institucion)) {
  resultado1<-as.character(tdiv[(grep(institucion[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a7<-resultado

resultado<-c()
for (i in 1:length(tipo.institucion)) {
  resultado1<-as.character(tdiv[(grep(tipo.institucion[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a8<-resultado 

resultado<-c()
for (i in 1:length(adscripcion)) {
  resultado1<-as.character(tdiv[(grep(adscripcion[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a9<-resultado

resultado<-c()
for (i in 1:length(socios)) {
  resultado1<-as.character(tdiv[(grep(socios[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a10<-resultado

resultado<-c()
for (i in 1:length(web.institución)) {
  resultado1<-as.character(tdiv[(grep(web.institución[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a11<-resultado

resultado<-c()
for (i in 1:length(nivel)) {
  resultado1<-as.character(tdiv[(grep(nivel[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a12<-resultado

resultado<-c()
for (i in 1:length(duracion)) {
  resultado1<-as.character(tdiv[(grep(duracion[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a13<-resultado

resultado<-c()
for (i in 1:length(objetivo)) {
  resultado1<-as.character(tdiv[(grep(objetivo[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a14<-resultado

resultado<-c()
for (i in 1:length(pob.objetivo)) {
  resultado1<-as.character(tdiv[(grep(pob.objetivo[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a15<-resultado

resultado<-c()
for (i in 1:length(c.egreso)) {
  resultado1<-as.character(tdiv[(grep(c.egreso[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a16<-resultado

resultado<-c()
for (i in 1:length(h.egreso)) {
  resultado1<-as.character(tdiv[(grep(h.egreso[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a17<-resultado

resultado<-c()
for (i in 1:length(campo.t)) {
  resultado1<-as.character(tdiv[(grep(campo.t[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a18<-resultado

resultado<-c()
for (i in 1:length(reconocimiento)) {
  resultado1<-as.character(tdiv[(grep(reconocimiento[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a19<-resultado

resultado<-c()
for (i in 1:length(unidad.entrenamiento)) {
  resultado1<-as.character(tdiv[(grep(unidad.entrenamiento[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a20<-resultado

resultado<-c()
for (i in 1:length(numero.entrenamiento)) {
  resultado1<-as.character(tdiv[(grep(numero.entrenamiento[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a21<-resultado

resultado<-c()
for (i in 1:length(fechas)) {
  resultado1<-as.character(tdiv[(grep(fechas[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a22<-resultado

resultado<-c()
for (i in 1:length(temporalidad)) {
  resultado1<-as.character(tdiv[(grep(temporalidad[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a23<-resultado

resultado<-c()
for (i in 1:length(idioma)) {
  resultado1<-as.character(tdiv[(grep(idioma[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a24<-resultado

resultado<-c()
for (i in 1:length(admision)) {
  resultado1<-as.character(tdiv[(grep(admision[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a25<-resultado

resultado<-c()
for (i in 1:length(examen)) {
  resultado1<-as.character(tdiv[(grep(examen[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a26<-resultado

resultado<-c()
for (i in 1:length(costos)) {
  resultado1<-as.character(tdiv[(grep(costos[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a27<-resultado

resultado<-c()
for (i in 1:length(facilidades)) {
  resultado1<-as.character(tdiv[(grep(facilidades[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a28<-resultado

resultado<-c()
for (i in 1:length(politicas.ingreso)) {
  resultado1<-as.character(tdiv[(grep(politicas.ingreso[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a29<-resultado

resultado<-c()
for (i in 1:length(tipo.programa)) {
  resultado1<-as.character(tdiv[(grep(tipo.programa[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a30<-resultado

resultado<-c()
for (i in 1:length(plataforma)) {
  resultado1<-as.character(tdiv[(grep(plataforma[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a31<-resultado

resultado<-c()
for (i in 1:length(asignaturas)) {
  resultado1<-as.character(tdiv[(grep(asignaturas[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a32<-resultado

resultado<-c()
for (i in 1:length(practica.final)) {
  resultado1<-as.character(tdiv[(grep(practica.final[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a33<-resultado

resultado<-c()
for (i in 1:length(evaluacion)) {
  resultado1<-as.character(tdiv[(grep(evaluacion[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a34<-resultado

resultado<-c()
for (i in 1:length(innovacion)) {
  resultado1<-as.character(tdiv[(grep(innovacion[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a35<-resultado

resultado<-c()
for (i in 1:length(suster)) {
  resultado1<-as.character(tdiv[(grep(suster[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a36<-resultado

resultado<-c()
for (i in 1:length(m1)) {
  resultado1<-as.character(tdiv[(grep(m1[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a37<-resultado

resultado<-c()
for (i in 1:length(m2)) {
  resultado1<-as.character(tdiv[(grep(m2[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a38<-resultado

resultado<-c()
for (i in 1:length(m3)) {
  resultado1<-as.character(tdiv[(grep(m3[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a39<-resultado

resultado<-c()
for (i in 1:length(m4)) {
  resultado1<-as.character(tdiv[(grep(m4[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a40<-resultado

resultado<-c()
for (i in 1:length(plan)) {
  resultado1<-as.character(tdiv[(grep(plan[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a41<-resultado

resultado<-c()
for (i in 1:length(estructura.plan)) {
  resultado1<-as.character(tdiv[(grep(estructura.plan[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a42<-resultado

resultado<-c()
for (i in 1:length(certificacion)) {
  resultado1<-as.character(tdiv[(grep(certificacion[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a43<-resultado

resultado<-c()
for (i in 1:length(g.innovacion)) {
  resultado1<-as.character(tdiv[(grep(g.innovacion[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a44<-resultado

resultado<-c()
for (i in 1:length(observaciones)) {
  resultado1<-as.character(tdiv[(grep(observaciones[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a45<-resultado

resultado<-c()
for (i in 1:length(buenas.practicas)) {
  resultado1<-as.character(tdiv[(grep(buenas.practicas[i],strsplit(tdiv,"loquesea")))])
  resultado1<-resultado1[which(nchar(as.character(resultado1))<500)]
  resultado1<-unique(resultado1)
  resultado<-c(resultado,resultado1)
  resultado<-unique(resultado)
  resultado<-resultado[order(nchar(resultado), resultado)]
}
a46<-resultado

anexo<-cbind.fill(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,
                  a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,
                  a27,a28,a29,a30,a31,a32,a33,a34,a35,a36,a37,a38,a39,a40,
                  a41,a42,a43,a44,a45,a46,fill = NA)

colnames(anexo)<-c("Titulo oferta","Tipo oferta","Web site","Encargado/coordinador",
                   "Vigencia","Nombre institución","Tipo de institución",
                   "Naturaleza institucion","Entidad adscrita","Eventuales socios",
                   "Website institucion","Nivel","Duracion","Objetivo del programa",
                   "Poblacio a quien va dirigido","Perfil de egreso:conocimiento",
                   "Perfil de egreso:habilidades","Campo de trabajo","Reconocimiento/cert",
                   "Unidad entrenamiento","Numero de creditos de entrenamiento",
                   "Periodicidad","Temporalidad","Idioma","Requisitos acceso",
                   "Examen acceso","Costos","Facilidades economicas","Politicas ingreso",
                   "Tipo programa","Tipo plataforma","Orientacion asignaturas",
                   "Practica final","Evaluacion aprobacion","Aspectos innovadores metodos",
                   "Contenidos relativos a desarrolo territorial","Modulo1","Modulo2",
                   "Modulo3","Modulo4","Plan de estudios","Plan de modulos particulares",
                   "Certificacion extra","Innovación","Observaciones",
                   "Propuesta seguimiento")

write.csv(anexo,file = "Relleno_para_anexo.csv",row.names = FALSE)
