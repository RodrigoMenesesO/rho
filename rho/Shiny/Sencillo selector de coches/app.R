list.of.packages <- c("shiny","shinydashboard","intrval")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinydashboard)
require(intrval)

cars<-read.csv(file = "cars.csv")

colapsar<-function(lista){
  vector<-c()
  for(i in 1:length(lista)){
    vector<-paste(vector,lista[i])
  }
  return(vector)
}

a<-colapsar(as.vector(cars$Color))

b<-strsplit(a," ")
b<-b[[1]]

b<-gsub('"',"",b)
d<-levels(as.factor(b))

header<-dashboardHeader(title = "Selector de coches")
sidebar<-dashboardSidebar(disable = TRUE,
  sidebarMenu(
    menuItem("Selecciona",tabName = "select",icon = icon("car",lib = "font-awesome"))
  )
)
body<-dashboardBody(
tabItems(
tabItem(tabName = "select",
    box(h2("Caracteristicas"),h2(icon("car")),align="center",width = 12,fluidRow(
      box(selectInput("rend","Consumo de combustible",choices = c("BAJO","ALTO")),
          width = 4,height = 122),
      box(selectInput("col","Color",choices = d,selected = "BLANCO"),
      height = 122,width = 4),
      box(selectInput("int","Interiores",choices = c("BASICO","INTERMEDIO","AVANZADO")),
          width = 4,height = 122)),
    fluidRow(
      box(sliderInput("price","Rango de precio",min=225000,max=992000,
                      value=c(250000,500000)),width = 4),
      box(selectInput("pot","Potencia",choices = c("BASICA","INTERMEDIA","AVANZADA")),width = 4),
      box(selectInput("cil","Cilindraje",choices = 2:6),width = 4)))
    ,
    box(h2("Resultados"),icon("sunglasses"),align="center",
        fluidRow(box(textOutput("seleccionado"),align="center",width = 12)),width = 12)
)
))



ui<-fillPage(dashboardPage(header = header,sidebar = sidebar,body = body))

server<-function(input,output){
  selector<-reactiveValues(set=NULL)
  
  observeEvent(input$rend,{
    if(input$rend=="BAJO"){
      selector$set[1]<-"BAJO"
    } else{
      selector$set[1]<-"ALTO"
    }
    
    if(input$int=="BASICO"){
      selector$set[2]<-"Interior.basico"
    } else if(input$int=="INTERMEDIO"){
      selector$set[2]<-"Interior.intermedio"
    } else{
      selector$set[2]<-"Interior.avanzado"
    }
    
    if(input$pot=="BASICA"){
      selector$set[3]<-"Potencia.basica"
    } else if(input$pot=="INTERMEDIA"){
      selector$set[3]<-"Potencia.intermedia"
    } else{
      selector$set[3]<-"Potencia.avanzada"
    }
    
    if(input$int=="BASICO" & input$pot=="BASICA"){
      selector$set[4]<-"Precio.basico"
    } else if(input$int=="BASICO" & input$pot=="INTERMEDIA"){
      selector$set[4]<-"Precio.intermedio"
    } else if(input$int=="BASICO" & input$pot=="AVANZADA"){
      selector$set[4]<-"Precio.avanzado"
    } else if(input$int=="INTERMEDIO" & input$pot=="BASICA"){
      selector$set[4]<-"Precio.intermedio"
    } else if(input$int=="AVANZADO" & input$pot=="BASICA"){
      selector$set[4]<-"Precio.avanzado"
    } else if(input$int=="INTERMEDIO" & input$pot=="INTERMEDIA"){
      selector$set[4]<-"Precio.intermedio"
    } else if(input$int=="AVANZADO" & input$pot=="AVANZADA"){
      selector$set[4]<-"Precio.avanzado"
    }else if(input$int=="INTERMEDIO" & input$pot=="AVANZADA"){
      selector$set[4]<-"Precio.avanzado"
    }else if(input$int=="AVANZADO" & input$pot=="INTERMEDIA"){
      selector$set[4]<-"Precio.avanzado"
    }
    
  })
  
  observeEvent(input$pot,{
    if(input$rend=="BAJO"){
      selector$set[1]<-"BAJO"
    } else{
      selector$set[1]<-"ALTO"
    }
    
    if(input$int=="BASICO"){
      selector$set[2]<-"Interior.basico"
    } else if(input$int=="INTERMEDIO"){
      selector$set[2]<-"Interior.intermedio"
    } else{
      selector$set[2]<-"Interior.avanzado"
    }
    
    if(input$pot=="BASICA"){
      selector$set[3]<-"Potencia.basica"
    } else if(input$pot=="INTERMEDIA"){
      selector$set[3]<-"Potencia.intermedia"
    } else{
      selector$set[3]<-"Potencia.avanzada"
    }
    
    if(input$int=="BASICO" & input$pot=="BASICA"){
      selector$set[4]<-"Precio.basico"
    } else if(input$int=="BASICO" & input$pot=="INTERMEDIA"){
      selector$set[4]<-"Precio.intermedio"
    } else if(input$int=="BASICO" & input$pot=="AVANZADA"){
      selector$set[4]<-"Precio.avanzado"
    } else if(input$int=="INTERMEDIO" & input$pot=="BASICA"){
      selector$set[4]<-"Precio.intermedio"
    } else if(input$int=="AVANZADO" & input$pot=="BASICA"){
      selector$set[4]<-"Precio.avanzado"
    } else if(input$int=="INTERMEDIO" & input$pot=="INTERMEDIA"){
      selector$set[4]<-"Precio.intermedio"
    } else if(input$int=="AVANZADO" & input$pot=="AVANZADA"){
      selector$set[4]<-"Precio.avanzado"
    }else if(input$int=="INTERMEDIO" & input$pot=="AVANZADA"){
      selector$set[4]<-"Precio.avanzado"
    }else if(input$int=="AVANZADO" & input$pot=="INTERMEDIA"){
      selector$set[4]<-"Precio.avanzado"
    }
    
  })
  
  observeEvent(input$int,{
    if(input$rend=="BAJO"){
      selector$set[1]<-"BAJO"
    } else{
      selector$set[1]<-"ALTO"
    }
    
    if(input$int=="BASICO"){
      selector$set[2]<-"Interior.basico"
    } else if(input$int=="INTERMEDIO"){
      selector$set[2]<-"Interior.intermedio"
    } else{
      selector$set[2]<-"Interior.avanzado"
    }
    
    if(input$pot=="BASICA"){
      selector$set[3]<-"Potencia.basica"
    } else if(input$pot=="INTERMEDIA"){
      selector$set[3]<-"Potencia.intermedia"
    } else{
      selector$set[3]<-"Potencia.avanzada"
    }
    
    if(input$int=="BASICO" & input$pot=="BASICA"){
      selector$set[4]<-"Precio.basico"
    } else if(input$int=="BASICO" & input$pot=="INTERMEDIA"){
      selector$set[4]<-"Precio.intermedio"
    } else if(input$int=="BASICO" & input$pot=="AVANZADA"){
      selector$set[4]<-"Precio.avanzado"
    } else if(input$int=="INTERMEDIO" & input$pot=="BASICA"){
      selector$set[4]<-"Precio.intermedio"
    } else if(input$int=="AVANZADO" & input$pot=="BASICA"){
      selector$set[4]<-"Precio.avanzado"
    } else if(input$int=="INTERMEDIO" & input$pot=="INTERMEDIA"){
      selector$set[4]<-"Precio.intermedio"
    } else if(input$int=="AVANZADO" & input$pot=="AVANZADA"){
      selector$set[4]<-"Precio.avanzado"
    }else if(input$int=="INTERMEDIO" & input$pot=="AVANZADA"){
      selector$set[4]<-"Precio.avanzado"
    }else if(input$int=="AVANZADO" & input$pot=="INTERMEDIA"){
      selector$set[4]<-"Precio.avanzado"
    }
    
  })
  
  observeEvent(input$rend,{
    output$seleccionado<-renderText({
      original<-cars
      db<-cars
      count<-c(1,2,14,13,12)
      db<-db[which(db$Consumo==selector$set[1]),count]
      db<-data.frame(db,Potencia=original[which(original$Consumo==selector$set[1]),selector$set[3]],
                     Interiores=original[which(original$Consumo==selector$set[1]),selector$set[2]],
                     Precio=original[which(original$Consumo==selector$set[1]),selector$set[4]])
      db<-db[which(db$Precio %()% input$price & db$Cilindrada==input$cil),]
      if(!is.na(db[1,2])){
      paste("Te espera un ",db[1,2]," el cual forma parte de los coches ",db[1,1],
            " con una grandiosa potencia de ",db[1,"Potencia"]," caballos de fuerza de interiores en ",
            db[1,"Interiores"]," en un color ",input$col, " con un consumo ",db[1,"Consumo"]," de combustible, al increible precio de $",
            db[1,"Precio"]," aproximadamente para el modelo 2019",sep = "")
      } else{
        paste("No hemos podido encontrar un coche para ti, lo sentimos, intenta con otras opciones")
      }
      })
  })
    
  observeEvent(input$pot,{
    output$seleccionado<-renderText({
      original<-cars
      db<-cars
      count<-c(1,2,14,13,12)
      db<-db[which(db$Consumo==selector$set[1]),count]
      db<-data.frame(db,Potencia=original[which(original$Consumo==selector$set[1]),selector$set[3]],
                     Interiores=original[which(original$Consumo==selector$set[1]),selector$set[2]],
                     Precio=original[which(original$Consumo==selector$set[1]),selector$set[4]])
      db<-db[which(db$Precio %()% input$price & db$Cilindrada==input$cil),]
      if(!is.na(db[1,2])){
        paste("Te espera un ",db[1,2]," el cual forma parte de los coches ",db[1,1],
              " con una grandiosa potencia de ",db[1,"Potencia"]," caballos de fuerza de interiores en ",
              db[1,"Interiores"]," en un color ",input$col, " con un consumo ",db[1,"Consumo"]," de combustible, al increible precio de $",
              db[1,"Precio"]," aproximadamente para el modelo 2019",sep = "")
      } else{
        paste("No hemos podido encontrar un coche para ti, lo sentimos, intenta con otras opciones")
      }
    })
  })
  
  observeEvent(input$int,{
    output$seleccionado<-renderText({
      original<-cars
      db<-cars
      count<-c(1,2,14,13,12)
      db<-db[which(db$Consumo==selector$set[1]),count]
      db<-data.frame(db,Potencia=original[which(original$Consumo==selector$set[1]),selector$set[3]],
                     Interiores=original[which(original$Consumo==selector$set[1]),selector$set[2]],
                     Precio=original[which(original$Consumo==selector$set[1]),selector$set[4]])
      db<-db[which(db$Precio %()% input$price & db$Cilindrada==input$cil),]
      if(!is.na(db[1,2])){
        paste("Te espera un ",db[1,2]," el cual forma parte de los coches ",db[1,1],
              " con una grandiosa potencia de ",db[1,"Potencia"]," caballos de fuerza de interiores en ",
              db[1,"Interiores"]," en un color ",input$col, " con un consumo ",db[1,"Consumo"]," de combustible, al increible precio de $",
              db[1,"Precio"]," aproximadamente para el modelo 2019",sep = "")
      } else{
        paste("No hemos podido encontrar un coche para ti, lo sentimos, intenta con otras opciones")
      }
    })
  })
  
}

shinyApp(ui,server)