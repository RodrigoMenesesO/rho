# Simulacion grafica del movimiento browniano usando la funcion GBM de la paqueteria sde.

# Donde 
#  P0 es el valor inicial
#  mu es la tasa de interes
#  sigma es la volatilidad
#  T el tiempo final
#  nt el numero de trayectorias simuladas
#  n el numero de intervalos en los cuales dividir el intervalo [P0,T] es decir el numero de quiebres

require(sde)
Mov.Brown<-function(mu,sigma,P0,T,nt,n){
  dt=T/n; t=seq(0,T,by=dt)
  X=matrix(rep(0,length(t)*nt), nrow=nt)
  for (i in 1:nt) {X[i,]= GBM(x=P0,r=mu,sigma=sigma,T=T,N=n)}
  ymax=max(X); ymin=min(X)
  plot(t,X[1,],t='l',ylim=c(ymin, ymax), col=1,
       ylab="P(t)",xlab="t")
  for(i in 2:nt){lines(t,X[i,], t='l',ylim=c(ymin, ymax),col=i)}
}

# Ejemplo:

Mov.Brown(mu=1, sigma=1, P0=1, T = 0.1, nt=100, n=40)