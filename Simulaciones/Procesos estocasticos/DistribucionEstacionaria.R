# Obtencion de la distribucion estacionaria de una cadena de markov

# Considere para sus cálculos que toda Cadena de Markov irreducible sobre un espacio de estados finitos es 
# positiva recurrente.

# P es una matriz de transicion 

# Por ejemplo: 

P<-matrix(c(0.1,0.4,0.9,0.6),ncol=2)

# Distribucion estacionaria:

dist.est<-function(P){
  Pt<-t(P)
  matriz<-rbind((Pt-diag(1,ncol(P))),rep(1,ncol(P)))
  if(det(P)==0){
    stop("No existe distribucion estacionaria")
  }
  b<-matrix(c(rep(0,ncol(P)),1),byrow = 1)
  solution<-qr.solve(matriz,b)
  solution
}

# Intente:

dist.est(P)