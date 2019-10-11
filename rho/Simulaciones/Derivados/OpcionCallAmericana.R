# Funcion para obtener el valor de una opcion call americana, donde:

#El usuario añade su valor inicial S, número de periodos N, tasa de utilidad u y tasa de
#déficit d, el precio objetivo K, la capitalización t y la tasa libre de riesgo r.
    
    # S es el valor inicial
    # K es el precio objetivo
    # r es la tasa libre de riesgo
    # u es la tasa de utilidad
    # d es la tasa de deficit
    # t es la capitalizacion
    # N es el numero de periodos

Opcion.Call.Americana<-function (S, K, r, u, d, t, N) 
{
  R <- exp(r * (t/N))
  Rinv <- 1/R
  p_up <- (R - d)/(u - d)
  p_down <- 1 - p_up
  prices <- c(rep(0, N + 1))
  prices[1] <- S * (d^N)
  uu <- u %*% u
  for (i in 2:(N + 1)) {
    prices[i] <- c(uu * prices[i - 1])
  }
  call_prices <- pmax(0, (prices - K))
  for (step in N:1) {
    for (i in 1:(step + 1)) {
      call_prices[i] <- c((p_up * call_prices[i + 1] + 
                             p_down * call_prices[i]) * Rinv)
      prices[i] <- c(d * prices[i + 1])
      call_prices[i] <- max(call_prices[i], prices[i] - 
                              K)
    }
  }
  return(call_prices[1])
}
