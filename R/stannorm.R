# Normalizar --------------------------------------------------------------

# Esta función normaliza los valores de un vector numérico
normalize_vector = function(vec){
  if (!is.numeric(vec)){
    stop("Los valores no son numéricos")
  }
  return((vec-min(vec))/max(vec-min(vec)))
}

# Esta función normaliza los valores de las columnas numéricas de un dataframe
# o matriz
normalize_tabla = function(x){
  result = x
  result[] = NA
  bool_numeric = numeric.columns(x)
  if (any(bool_numeric)){
    result[,bool_numeric] = apply(x[,bool_numeric], FUN=normalize_vector, MARGIN=2)
  }
  # Deja los valores categóricos intactos
  result[,!bool_numeric] = x[,!bool_numeric]
  return (result)
}

#' Normalizar arrays
#'
#' @description Esta función normaliza los valores de un vector y las columnas de
#' una matriz o dataframe para que esten en un rango [0,1]
#'
#' @param x Vector, matriz o dataframe que normalizar
#'
#' @return Array normalizado de la misma dimensión que \code{x}
#' @export
#'
#' @examples
#' x <- matrix(rnorm(30,5,10),ncol=3)
#' normalize(x)
normalize = function(x){
  if (is.vector(x)){
    return(normalize_vector(x))
  }
  else if (is.data.frame(x) || is.matrix(x)){
    return(normalize_tabla(x))
  }
  else{
    stop("Tipo del párametro x incorrecto")
  }
}

# Estandarizar ------------------------------------------------------------

# Esta función estandariza los valores de un vector numérico
standarize_vector = function(vec){
  if (!is.numeric(vec)){
    stop("Los valores no son numéricos")
  }
  return((vec-mean(vec))/sd(vec))
}

# Esta función estandariza los valores de las columnas numéricas de un dataframe
# o matriz
standarize_tabla = function(x){
  result = x
  result[] = NA
  bool_numeric = numeric.columns(x)
  if (any(bool_numeric)){
    result[,bool_numeric] = apply(x[,bool_numeric], FUN=standarize_vector, MARGIN=2)
  }
  # Deja los valores categóricos intactos
  result[,!bool_numeric] = x[,!bool_numeric]
  return (result)
}

#' Estandarizar arrays
#'
#' @description Esta función estandariza los valores de un vector y las columnas de
#' una matriz o dataframe para que la media sea 0 y la desviación estándar 1
#'
#' @param x Vector, matriz o dataframe que estandarizar
#'
#' @return Array estandarizado de la misma dimensión que \code{x}
#' @export
#'
#' @examples
#' x <- matrix(rnorm(30,5,10),ncol=3)
#' standarize(x)
standarize = function(x){
  if (is.vector(x)){
    return(standarize_vector(x))
  }
  else if (is.data.frame(x) || is.matrix(x)){
    return(standarize_tabla(x))
  }
  else{
    stop("Tipo del párametro x incorrecto")
  }
}
