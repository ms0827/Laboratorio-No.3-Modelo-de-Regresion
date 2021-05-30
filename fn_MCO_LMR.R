
#data= cars
mco_lmr <- function(data) {
  
  #dataset
  #data <-read.csv(ABC, sep=",")
  
  #Valores de los estimadores del modelo Bo, B1
  x = data[,1]
  y = data[,2]
  x_mean = sum(x/nrow(data))
  y_mean = sum(y/nrow(data))
  
  #Suma de los cuadrados de X
  Sxx = sum((x - mean(x))^2)
  
  #Suma de los cuadrados de Y
  Syy = sum((y - mean(y))^2)
  
  #Suma de los productos cruzados XY
  Sxy = sum((x - mean(x)) * (y - mean(y)))
  
  #Pendiente de la recta de regresion
  beta1 = Sxy / Sxx
  
  #Intercepto de la recta de regresion
  beta0 = y_mean - beta1*x_mean
  
  #Modelo estimado
  y_hat = beta0 + beta1 * x
  
  #Valores de los residuos
  e = y - y_hat
  n = length(e)
  s2_e = sum(e^2)/(n - 2)
  
  #Suma de los erros al cuadrado
  se = sqrt(s2_e)
  
  #Suma de cuadrados del error
  SSE = sum((y - y_hat)^2)
  
  #Suma de cuadrados total
  SST = sum((y - mean(y))^2)
  
  #Suma de cuadrados de la regresion
  SSReg = sum((y_hat - mean(y))^2)

  #Coeficiente de determinacion R2
  R2 = SSReg / SST
  
  #Coeficiente de correlacion r
  R = Sxy/sqrt(Sxx*Syy)
  R
  
  my_lista <- list(b0=beta0, b1=beta1, r2=R2, r=R, residuos=e)
  return(my_lista)
  
}

  
  
  