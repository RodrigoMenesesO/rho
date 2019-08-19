# 6. Caminata aleatoria al estilo de la ruina del jugador, n es el numero de juegos o pasos,
# los contadores son los puntos de los jugadores o posiciones iniciales de la caminata,
# y prob.x es la probabilidad de que gane el jugador x siendo su complemento la probabilidad de que
# gane el jugador y

caminata<-function(n=10,contador.x=10, contador.y=10, prob.x=0.5){
  cont.mem.x<-contador.x #Reservamos memoria para contar los puntos de x
  cont.mem.y<-contador.y #Lo mismo para y
  v.aleatorio.x<-rbinom(n, 1, p=prob.x) #Obtención de las n observaciones aleatorias
  v.aleatorio.x[v.aleatorio.x==0]<- -1 #Reemplazamos los ceros de las observaciones por -1
  v.aleatorio.y<-v.aleatorio.x*(-1) #Cuando x gana, y...
  suma.acum.x<-cumsum(v.aleatorio.x)+contador.x #Puntuación de x
  suma.acum.y<-cumsum(v.aleatorio.y)+contador.y #Puntuación de y
  
  data<-cumsum(v.aleatorio.x)+contador.x #Reservamos memoria para la puntuación de x
  
  #Condicion de victoria (límite de la caminata)
  if( any( which(data>=contador.x+contador.y) ) | any( which(data<=0) ) ){
    limite<-1+min( which(data>=contador.x+contador.y), which(data<=0) )
  } 
  j<-n
  
  for(i in 1:n){
    if(data[i]==contador.x+contador.y){
      cat("El jugador Y ganó en",i,"juegos","\n")
      j<-i
      break
    }
  }
  for(i in 1:j){
    if(data[i]==0){
      cat("El jugador X ganó en",i,"juegos","\n")
      j<-i
      break
    }
  }
  plot(data[1:j],type = "b",ylab = "Saltos",xlab="Juegos",
       main="Simulacion de una caminata aleatoria")
  return(data[1:j])
}

caminata(100)
