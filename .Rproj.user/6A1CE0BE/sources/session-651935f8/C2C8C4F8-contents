
# Numeric columns ---------------------------------------------------------

#' Buscar columnas numéricas
#'
#' @description
#' Esta función busca columnas numéricas en una matriz o dataframe
#'
#'
#' @param x Matriz o dataframe que analizar
#'
#' @return Un vector lógico que especifica que columnas son numéricas (TRUE)
#' y cuales no (FALSE)
#' @export
#'
#' @examples
#' Name <- c("Jon", "Bill", "Maria", "Ben", "Tina")
#' Age <- c(23, 41, 32, 58, 26)
#' Weight <- c(156,98,75,112,74)
#' Height <- c(1.75,1.68,1.56,1.86,1.76)
#' x <- data.frame(Name, Age, Weight, Height)
#' numeric.columns(x)
numeric.columns = function(x){
  if (is.matrix(x)){
    bool_numeric = rep(is.numeric(x),dim(x)[2])
  }
  else if (is.data.frame(x)) {
    bool_numeric = sapply(x,FUN=is.numeric)
  }
  else{
    stop("x tiene que ser una matriz o un dataframe")
  }
  return (bool_numeric)
}

# is.binary ---------------------------------------------------------------


#' Revisar si un vector es binario
#'
#' @description Esta función revisa si un vector de entrada es binario o no
#'
#' @param x Vector que revisar
#'
#' @return \code{TRUE} si \code{x} es binario, \code{FALSE} en el caso contrario
#' @export
#'
#' @examples
#' X <- c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE)
#' is.binary(X)
#' @examples
#' X <- c(3.5,7.4,1.2,2.0,3.9,5.3,6.1,6.9)
#' is.binary(X)
is.binary = function(x){
  return (is.logical(x) || (is.numeric(x) && all(x %in% 0:1) ))
}


# Entropía condicional ----------------------------------------------------


#' Entropía condicional entre vectores
#'
#' @description Esta función calcula la entropía condicional entre 2 vectores
#'
#' @param X Primer vector (condicionado)
#' @param Y Segundo vector (condicionante)
#'
#' @return Valor numérico de la entropía condicional
#' @export
#'
#' @examples
#' Name <- c("Jon", "Bill", "Maria", "Ben", "Tina")
#' Age <- c(23, 41, 32, 58, 26)
#' conditional.entropy(Name,Age)
conditional.entropy = function(X,Y){
  # Si es un dataframe, coge las columnas como vectores
  if (is.data.frame(X)){
    X = X[,1]
    Y = Y[,1]
  }
  # Recoge que valores puede coger cada columna
  ux = unique(X)
  uy = unique(Y)
  # Calcula todas las combinaciones de valores posibles
  combs = expand.grid(ux,uy, stringsAsFactors = FALSE)
  # Para cada combinación, calcula la entropía condicional parcial y suma todas
  return(sum(apply(combs, X, Y, FUN=conditional.entropy.part, MARGIN=1)))
}

# Función de ayuda para calcular la entropía condicional parcial dadas
# 2 columnas (X,Y) y la combinación de 2 valores que se analiza
conditional.entropy.part = function(comb,X,Y){
  a = comb[1]
  b = comb[2]
  # Mira que posiciones tienen los valores que se definen en comb
  condX = (X==a)
  condY = (Y==b)
  both = condX & condY
  # Si no hay ninguna 'row' que cumple las 2 condiciones, la entropía condicional es 0
  suma = sum(both)
  if (suma==0){
    return(0)
  }
  # Aplica la fórmula de entropía condicional
  prob.both = suma/length(X)
  prob.cond = suma/sum(condY)
  return (prob.both*log2(1/prob.cond))

}
