# Simulacion de un arbol binomial donde:

#  S es el valor inicial
#  u es la tasa de utilidad
#  d es la tasa de deficit
#  N es el numero de periodos.

arbol_binomial = function(S, u, d, N) {
  tree = matrix(0, nrow=N+1, ncol=N+1)
  
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i,j] = S * u^(j-1) * d^((i-1)-(j-1))
    }
  }
  return(tree)
}

# Por ejemplo

arbol_binomial(S=1,N=10,u=0.5,d=0.2)

