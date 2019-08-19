# Funcion para obtener el precio de una opcion binaria (call o put), donde:

#  tree es un arbol binomial
#  delta_t es la tasa de capitalizacion
#  r es la tasa libre de riesgo
#  u es la tasa de utilidad
#  d es la tasa de deficit
#  X es el precio objetivo 
#  type es el tipo de opcion; call por defecto, put si no se especifica

Opcion.Binomial<- function(tree, u, d, delta_t, r, X, type="call") {
  q <- (exp(r*delta_t) - d)/(u-d)
  option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  if(type == 'put') {
    option_tree[nrow(option_tree),] = pmax(X - tree[nrow(tree),], 0)
  } else {
    option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),] - X, 0)
  }
  
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] = ((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
    }
  }
  return(option_tree)
}

# Ejemplo:

source("../Procesos estocasticos/ArbolBinomial.R")

arbol<-arbol_binomial(100,0.8,0.5,4)

opcion<-Opcion.Binomial(arbol,0.8,0.5,3,0.07,40,type = "call")

opcion