# Librerias

list.of.packages <- c("actuar")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(actuar)

# Simulacion del proceso Cramer-Lundberg

# u como capital inicial
# pr como la cantidad de primas que ingresan por unidad de tiempo
# lambda_p como la intensidad de las reclamaciones 
# lambda_n como el parametro del monto de las reclamaciones lambda_n
# param_n1 como una lista con el parametro de la exponencial "rate"
# param_n2 como una lista de los argumentos para la función rpareto1 siendo "shape" el parametro de la
# distribucion pareto uniparametrica y "min" el limite inferior del soporte de la distribucion.
# eps como la probabilidad de salto

PP.C.L <- function(u = 10, pr = 1,lambda_p = 1,f_p = rexp,param_p = list(rate = 1),
                   lambda_n = 1,f_n1 = rexp, param_n1 = list(rate = 1 / 2), f_n2 = rpareto1,
                   param_n2 = list(shape = 3 / 2, min = 2 / 3), eps = 0.1,maximo_numero_saltos = 10000,
                   maximo_lapso = 7500) {
  
  
  # Funcion de utilidad
  last <- function(x) ifelse(length(x) > 0, yes = x[length(x)], no = 0)
  
  # Sentencia de elemento de lista extra con valor sin significado en cada lista
  param_p[["n"]] <- 1
  param_n1[["n"]] <- 1
  param_n2[["n"]] <- 1
  
  # Variable de iteracion
  jumps_number <- 0
  
  path <- matrix(NA, nrow = 1, ncol = 2)
  colnames(path) <- c("time", "X")
  path[1, ] <- c(0, u)
  
  # Funcion que añade el caso desfavorable
  add_jump_n <- function() {
    
    jump_value <- ifelse(test = rbinom(n = 1, size = 1, prob = eps) == 1,
                         yes = do.call(f_n2, param_n2),
                         no = do.call(f_n1, param_n1))
    jumps_n <<-  c(jumps_n, jump_value)
    
    jump_time <- current_time_n
    time_n <<- c(time_n, current_time_n)
    
    time_to_jump <- jump_time - path[nrow(path), 1]
    
    path <<- rbind(
      path,
      c(jump_time,
        path[nrow(path), 2] + time_to_jump * pr)
    )
    
    path <<- rbind(
      path,
      c(path[nrow(path), 1],
        path[nrow(path), 2] - jump_value)
    )
    
    jumps_number <<- jumps_number + 1
  }
  
  # Funcion que añade el caso favorable
  add_jump_p <- function(jump_time, jump_value) {
    
    jump_value <- do.call(f_p, param_p)
    jumps_p <<-  c(jumps_p, jump_value)
    
    jump_time <- current_time_p
    time_p <<- c(time_p, current_time_p)
    
    time_to_jump <- jump_time - path[nrow(path), 1]
    path <<- rbind(
      path,
      c(jump_time,
        path[nrow(path), 2] + time_to_jump * pr)
    )
    
    path <<- rbind(
      path,
      c(path[nrow(path), 1],
        path[nrow(path), 2] + jump_value)
    )
    
    jumps_number <<- jumps_number + 1
  }
  
  # Checa si se encuentra en ruina
  is_ruined <- function() path[nrow(path), 2] < 0
  
  # Checa si el maximo numero de saltos se ha alcanzado, con el fin de evitar
  #desbordamiento
  is_maximo_numero_saltos_attained <- function() jumps_number >= maximo_numero_saltos
  
  # Checa si el patron ha alcanzado el máximo plazo
  is_maximo_lapso_attained <- function() path[nrow(path), 1] >= maximo_lapso
  
  jumps_n <- numeric() # Tamaño de los saltos negativos
  jumps_p <- numeric() # Tamaño de los saltos positivos
  
  time_n <- numeric() # Tiempo en los saltos negativos
  time_p <- numeric() # Tiempo en los saltos positivos
  
  current_time_n <- rexp(1, lambda_n)
  current_time_p <- rexp(1, lambda_p)
  
  repeat{
    
    if(current_time_p > current_time_n) {
      
      add_jump_n()
      
      if(is_ruined()) break
      if(is_maximo_numero_saltos_attained()) break
      if(is_maximo_lapso_attained()) break #
      
      repeat {
        
        current_time_n <- last(time_n) + rexp(1, lambda_n)
        if(current_time_p < current_time_n) break
        
        add_jump_n()
        
        if(is_ruined()) break
        if(is_maximo_numero_saltos_attained()) break
        if(is_maximo_lapso_attained()) break #
      }
      
      if(is_ruined()) break
      if(is_maximo_numero_saltos_attained()) break
      if(is_maximo_lapso_attained()) break #
      
      
    } else {
      
      
      add_jump_p()
      
      if(is_maximo_numero_saltos_attained()) break
      if(is_maximo_lapso_attained()) break #
      
      repeat {
        current_time_p <- last(time_p) + rexp(1, lambda_p)
        if(current_time_p > current_time_n) break
        
        add_jump_p()
        
        if(is_maximo_numero_saltos_attained()) break
        if(is_maximo_lapso_attained()) break #
      }
      
      if(is_maximo_numero_saltos_attained()) break
      if(is_maximo_lapso_attained()) break #
      
    }
  }
  
  if(!is_ruined())
    path <- rbind(
      path,
      c(maximo_lapso,
        path[nrow(path), 2] + (maximo_lapso - path[nrow(path), 1]) * pr)
    )
  
  
  
  if(jumps_number >= maximo_numero_saltos)
    warning("Se ha alcanzado el maximo numero de saltos")
  
  rval <- list(
    path = path,
    jumps_p = jumps_p,
    time_p = time_p,
    jumps_n = jumps_n,
    time_n = time_n,
    jumps_number = jumps_number,
    is_ruined = is_ruined(),
    is_maximo_numero_saltos_attained = is_maximo_numero_saltos_attained(),
    is_maximo_lapso_attained = is_maximo_lapso_attained(),
    u = u,
    pr = pr,
    lambda_p = lambda_p,
    f_p = f_p,
    param_p = param_p,
    lambda_n = lambda_n,
    f_n1 = f_n1,
    param_n1 = param_n1,
    f_n2 = f_n2,
    param_n2 = param_n2,
    eps = eps,
    maximo_numero_saltos = maximo_numero_saltos,
    maximo_lapso = maximo_lapso
  )
  
  class(rval) <- "process"
  
  return(rval)
}

# Simulacion con valores por defecto

PP.C.L(pr=2)
