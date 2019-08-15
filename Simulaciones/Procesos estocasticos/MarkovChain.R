# A function to define a Markov Chain

# P is a transition matrix and n is the number of steps

# For example P<-matrix(c(1,0,0,1),ncol=2)

CadenaMarkov <- function(P, n = 100, mu0 = rep(1, nrow(P))/nrow(P)) {
  if(length(colnames(P)) == 0) estados <- 1:ncol(P) else estados <- colnames(P)
  func.inic <- cumsum(mu0)
  func.act <- t(apply(P, 1, cumsum))
  U <- runif(n) # utiliza uniformes [0,1]
  X <- numeric(n)*NA
  j = 1; while(U[1] > func.inic[j]) j = j + 1; #Valor inicial de la cadena
  X[1] <- estados[j]
  # restantes valores de la cadena
  for (i in 2:n) {
    j = 1; while(U[i] > func.act[X[i - 1], j]) j = j + 1;
    X[i] <- estados[j]
  }
  return(X)
}