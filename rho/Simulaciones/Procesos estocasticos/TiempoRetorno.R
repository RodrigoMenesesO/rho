# Tiempo de retorno y tiempo promedio de retorno del estado i

# Describe el tiempo y tiempo promedio en el cual, partiendo del estado i, la cadena regresa al estado i.

# Funcion para generar la cadena de Markov

source("./MarkovChain.R")

# Genere una matriz de transicion

P<-matrix(c(0.1,0.4,0.9,0.6),ncol=2)

# Genere una cadena de Markov

cm1<-CadenaMarkov(P) # 100 pasos

# Funcion de tiempo promedio de retorno

retorno<-function(cm){
  i<-1
  j<-1
  tr<-rep(1,length(levels(as.factor(cm))))
  s.cm<-rep(1,length(levels(as.factor(cm))))
  while (i<=length(cm)) {
    
    a<-diff(which(cm==j))==i
    i<-i+1
    if(length(a)==0){
      next
    }
    if (any(a)==TRUE){
      cat("El tiempo de retorno del estado ", j,"es ", i-1,"\n")
      tr[j]<-i-1
      s.cm[j]<-length(which(diff(which(cm==j))==tr[j]))
      i<-1
      j<-j+1
    }else{
      stop("La cadena de Markov tiene al menos un tiempo de retorno no finito")
    }
    if (j==length(levels(as.factor(cm)))+1){
      next
    }
    tmr<-mean(s.cm)
  }
  return(list(Tiempo.medio.retorno=tmr,Tiempo.retorno=tr))
}

# Calcule el tiempo y tiempo promedio de retorno por cada estado

retorno(cm1)