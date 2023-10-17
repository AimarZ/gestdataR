#' Discretizar con puntos de corte
#'
#' @description
#' Esta función discretiza los valores de un vector dados los puntos de corte
#'
#' @param x Vector numérico a discretizar
#' @param cut.points Vector de puntos de corte
#'
#' @return Vector tipo factor con los valores de \code{x} discretizados en intérvalos
#' @export
#'
#' @examples
#' x <- c(3.5,7.4,1.2,2.0,3.9,5.3,6.1,6.9)
#' discretize(x, c(2,4,5.5))
discretize <- function(x, cut.points){
  if(!is.numeric(x)){
    stop("Sólo se admiten vectores numéricos")
  }
  if (NA %in% cut.points || any(duplicated(cut.points))){
    stop("Vector o num.bins no válido")
  }
  return (cut(x, breaks=c(-Inf,cut.points,Inf)))
}

# Discretización EW -----------------------------------------------------------

#' Discretización de "equal width" para vectores
#'
#' @description Esta función discretiza todos los valores numéricos
#' de un vector siguiendo el algoritmo "equal width"
#'
#' @param x Vector numérico sobre el que aplicar el algoritmo
#' @param num.bins Número de intervalos con los que discretizar
#' @param return.cut.points Si se deben devolver los puntos de corte (\code{TRUE}) o no (\code{FALSE} por default)
#'
#' @return Si \code{return.cut.points==TRUE} devuelve una lista con 2 elementos:
#'  1) Vector tipo factor del mismo tamaño que \code{x}, con los valores numéricos discretizados en intérvalos
#'  2) Puntos de corte de los intérvalos
#'  Si \code{return.cut.points==FALSE}, devolverá un vector tipo factor
#' @export
#'
#' @examples
#' x <- rnorm(8,3,5)
#' discretizeEW_vector(x,5)
discretizeEW_vector <- function(x, num.bins, return.cut.points=FALSE){
  if(!is.numeric(x)){
    stop("Sólo se admiten vectores numéricos")
  }
  minimo = min(x)
  maximo = max(x)
  tamaño = (maximo-minimo)/num.bins
  puntos.corte = rep(minimo,num.bins-1) + (1:(num.bins-1)) * tamaño
  vector.resultante = discretize(x,puntos.corte)
  if (return.cut.points){
    return (list(vector.resultante, puntos.corte))
  } else{
    return (vector.resultante)
  }
}

# Esta función discretiza los valores de las columnas numéricas de una matriz
# o dataframe usando el algoritmo "equal width"
discretizeEW_tabla = function(x, num.bins){
  # Prepara el resultado con la misma estructura que x pero rellenado de NA
  result = x
  result[] = NA
  # Discretiza sólo las columnas numéricas
  bool_numeric = numeric.columns(x)
  if (any(bool_numeric)){
    result[,bool_numeric] = apply(x[,bool_numeric], num.bins, FUN= discretizeEW_vector, MARGIN = 2)
  }
  result[,!bool_numeric] = x[,!bool_numeric]
  names(result) = names(x)
  return(result)
}

#' Discretización de "equal width" para arrays
#'
#' @description Esta función discretiza todos los valores numéricos
#' de un vector o una tabla siguiendo el algoritmo "equal width"
#'
#' @param x Vector numérico, matriz numérica o un dataframe sobre el que aplicar el algoritmo
#' @param num.bins Número de intervalos con los que discretizar
#'
#' @return Array del mismo tamaño que \code{x}, con los valores numéricos discretizados en intérvalos
#' @export
#'
#' @examples
#' x <- matrix(rnorm(30,5,10),ncol=3)
#' discretizeEW(x,5)
discretizeEW = function(x, num.bins){
  if (num.bins<1){
    stop("El párametro num.bins tiene que ser mayor que 0")
  }
  if (is.vector(x)){
    return(discretizeEW_vector(x,num.bins))
  }
  else if (is.data.frame(x) || is.matrix(x)){
    return(discretizeEW_tabla(x,num.bins))
  }
  else{
    stop("Tipo del párametro x incorrecto")
  }
}


# Discretización EF -----------------------------------------------------------

#' Discretización de "equal frequency" para vectores
#'
#' @description Esta función discretiza todos los valores numéricos
#' de un vector siguiendo el algoritmo "equal frequency"
#'
#' @param x Vector numérico sobre el que aplicar el algoritmo
#' @param num.bins Número de intervalos con los que discretizar
#' @param return.cut.points Si se deben devolver los puntos de corte (\code{TRUE}) o no (\code{FALSE} por default)
#'
#' @return Si \code{return.cut.points==TRUE} devuelve una lista con 2 elementos:
#'  1) Vector tipo factor del mismo tamaño que \code{x}, con los valores numéricos discretizados en intérvalos
#'  2) Puntos de corte de los intérvalos
#'  Si \code{return.cut.points==FALSE}, devolverá un vector tipo factor
#' @export
#'
#' @examples
#' x <- rnorm(8,3,5)
#' discretizeEF_vector(x,5)
discretizeEF_vector <- function(x, num.bins, return.cut.points = FALSE){
  if(!is.numeric(x)){
    stop("Sólo se admiten vectores numéricos")
  }
  # Ya que necesitamos discretizar por la frecuencia, podemos utilizar quantiles
  puntos.corte = quantile(x, probs = seq(0, 1, 1/num.bins)[2:num.bins], names = FALSE)
  vector.resultante = discretize(x,puntos.corte)
  if (return.cut.points){
    return (list(vector.resultante, puntos.corte))
  } else{
    return (vector.resultante)
  }
}
# Esta función discretiza los valores de las columnas numéricas de una matriz
# o dataframe usando el algoritmo "equal width"
discretizeEF_tabla = function(x, num.bins){
  # Prepara el resultado con la misma estructura que x pero rellenado de NA
  result = x
  result[] = NA
  # Discretiza sólo las columnas numéricas
  bool_numeric = numeric.columns(x)
  if (any(bool_numeric)){
    result[,bool_numeric] = apply(x[,bool_numeric], num.bins, FUN= discretizeEF_vector, MARGIN = 2)
  }
  result[,!bool_numeric] = x[,!bool_numeric]
  return(result)
}

#' Discretización de "equal frequency" para arrays
#'
#' @description Esta función discretiza todos los valores numéricos
#' de un vector o una tabla siguiendo el algoritmo "equal frequency"
#'
#' @param x Vector numérico, matriz numérica o un dataframe sobre el que aplicar el algoritmo
#' @param num.bins Número de intervalos con los que discretizar
#'
#' @return Array del mismo tamaño que \code{x}, con los valores numéricos discretizados en intérvalos
#' @export
#'
#' @examples
#' x <- matrix(rnorm(30,5,10),ncol=3)
#' discretizeEF(x,5)
discretizeEF = function(x, num.bins){
  if (num.bins<2){
    stop("El párametro num.bins tiene que ser mayor que 1")
  }
  if (is.vector(x)){
    return(discretizeEF_vector(x,num.bins))
  }
  else if (is.data.frame(x) || is.matrix(x)){
    return(discretizeEF_tabla(x,num.bins))
  }
  else{
    stop("Tipo del párametro x incorrecto")
  }
}


# Entropy-based binning ---------------------------------------------------



#' Discretización por entropía para vectores
#'
#' @description Esta función discretiza los valores de un vector con el objetivo de maximizar la información mutua
#' respecto a otra variable.
#'
#' @param X Vector numérico a discretizar
#' @param Y Vector respecto al cual se quiere maximizar la información mutua
#' @param num.bins Número de intervalos con los que discretizar
#' @param tries Cuantos intentos de discretización se harán con distintos puntos de corte
#' @param random.cut Valor boolear que especifica si los puntos de corte se calcularán de manera aleatoria o no
#' @param offset En el caso de que no haya aleatoriedad, en cada intento se incrementarán los puntos de corte por este valor numérico. Por defecto es None, y el offset será la desviación estándar de los datos dividido por el numéro de intentos
#'
#' @return Lista con 3 valores:
#'     1- pandas.core.arrays.categorical.Categorical con los valores de x discretizados en intérvalos
#'     2- Lista de puntos de corte calculados
#'     3- El valor de información mutua máxima obtenida
#' @export
#'
#' @examples
#' X = c(23, 41, 32, 58, 26)
#' Y = c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' entropy.binning(X,Y,num.bins=3,tries=10,random.cut=TRUE)
entropy.binning = function(X,Y,num.bins,tries=5, random.cut = FALSE,offset=NA){
  if (!is.vector(X) || !is.vector(Y)){
    stop("X e Y tienen que ser un vector")
  }
  if (length(X)!=length(Y)){
    stop("X e Y no tienen el mismo tamaño")
  }
  if (!is.binary(Y)){
    stop("La variable objetivo Y no es de tipo binario")
  }
  if (!is.numeric(X)){
    stop("La variable de características X no es de tipo numérico")
  }

  # Se calculan los valores mínimo y máximo de X
  minimo = min(X)
  maximo = max(X)

  best = -1
  result = NA

  # Si random_cut es False, se calculan los puntos de corte de manera equiespaciada
  if (!random.cut){
    tamaño = (maximo-minimo)/num.bins
    if (is.na(offset)){
      offset = sd(X)/tries
    }
    lower.bound = minimo - offset*((tries-1) %/% 2)
  }

  # Se realizan los intentos de discretización
  for (i in 1:tries){
    # Si random_cut es True, se calculan los puntos de corte de manera aleatoria
    if (random.cut){
      puntos.corte = sort(runif(num.bins-1,minimo,maximo))
    } else{
      # Se calculan los puntos de corte equiespaciados
      puntos.corte = rep(lower.bound,num.bins-1) + (1:(num.bins-1)) * tamaño
      lower.bound  = lower.bound + offset
    }
    # Se discretiza X y se calcula la información mutua
    X.discr = discretize(X,puntos.corte)
    mutinfo = mutual.information.vectors(X.discr,Y)[1]
    # Se guarda el resultado si es el mejor hasta el momento
    if (mutinfo>best){
      best = mutinfo
      result = list(X.discr,puntos.corte,best)
    }
  }
  return (result)
}

