###-libs----------------------------------------------------------------------
list.of.packages <- c("ggplot2", "shiny","shinydashboard","nortest","sde")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(nortest)
require(sde)


header<-dashboardHeader(title = "GBM")
sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Bidimensional",tabName = "bi",icon = icon("chart-bar",lib = "font-awesome"))
    #menuItem("Tridimensional",tabName = "tri",icon = icon("chart-bar",lib = "font-awesome"))
  )
)
body<-dashboardBody(
  tabItems(
    tabItem(tabName = "bi",
            fluidRow(
              box(plotOutput("gbm.bid",height = 500)),
              box(sliderInput("deriva1","Coeficiente de tendencia (deriva)",min=0.1,max=2,value=1)),
              box(sliderInput("difusion1","Coeficiente de difusion",min=0.1,max=2,value=1)),
              box(sliderInput("capital1","Capital inicial",min=1,max=100,value=5)),
              box(sliderInput("tiempo1","Tiempo final",min=0.1 ,max=3,value=0.5)),
              box(sliderInput("trayectorias1","Numero de trayectorias",min=1,max=50,value=10)),
              box(sliderInput("splits1","Numero de quiebres",min=1,max=100,value=50))
            )
    ),
    
    tabItem(tabName = "tri",
            fluidRow(
              box(plotOutput("NADA AUN...")
    )
  ))
  ))



ui<-dashboardPage(header = header,sidebar = sidebar,body = body)
server<-function(input,output){
  
  Mov.Brown<-function(mu,sigma,P0,T,nt,n){
    dt=T/n; t=seq(0,T,by=dt)
    X=matrix(rep(0,length(t)*nt), nrow=nt)
    for (i in 1:nt) {X[i,]= GBM(x=P0,r=mu,sigma=sigma,T=T,N=n)}
    ymax=max(X); ymin=min(X)
    plot(t,X[1,],t='l',ylim=c(ymin, ymax), col=1,
         ylab="P(t)",xlab="t")
    for(i in 2:nt){lines(t,X[i,], t='l',ylim=c(ymin, ymax),col=i)}
  }
  
  observeEvent(input$deriva1,{ 
    output$gbm.bid<-renderPlot({Mov.Brown(mu=input$deriva1, sigma=input$difusion1, 
P0=input$capital1, T = input$tiempo1, nt=input$trayectorias1, n=input$splits1)})
  })
  
  observeEvent(input$difusion1,{ 
    output$gbm.bid<-renderPlot({Mov.Brown(mu=input$deriva1, sigma=input$difusion1, 
P0=input$capital1, T = input$tiempo1, nt=input$trayectorias1, n=input$splits1)})
  })
  
  observeEvent(input$capital1,{ 
    output$gbm.bid<-renderPlot({Mov.Brown(mu=input$deriva1, sigma=input$difusion1, 
   P0=input$capital1, T = input$tiempo1, nt=input$trayectorias1, n=input$splits1)})
  })
  
  observeEvent(input$tiempo1,{ 
    output$gbm.bid<-renderPlot({Mov.Brown(mu=input$deriva1, sigma=input$difusion1, 
          P0=input$capital1, T = input$tiempo1, nt=input$trayectorias1, n=input$splits1)})
  })
  
  observeEvent(input$trayectorias1,{ 
    output$gbm.bid<-renderPlot({Mov.Brown(mu=input$deriva1, sigma=input$difusion1, 
    P0=input$capital1, T = input$tiempo1, nt=input$trayectorias1, n=input$splits1)})
  })
  
  observeEvent(input$splits1,{ 
    output$gbm.bid<-renderPlot({Mov.Brown(mu=input$deriva1, sigma=input$difusion1, 
    P0=input$capital1, T = input$tiempo1, nt=input$trayectorias1, n=input$splits1)})
  })
  
}

shinyApp(ui,server = server)
