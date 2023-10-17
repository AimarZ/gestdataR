# Correlación -------------------------------------------------------------

#' Correlación de Pearson entre columnas
#'
#' @description Esta función calcula los coeficientes de correlación de Pearson
#' de entre todos los pares de columnas numéricas de un array
#'
#' @param x Matriz o dataframe sobre el cual calcular las correlaciones
#'
#' @return Vector numérico de las correlaciones entre las columnas numéricas de \code{x}
#' @export
#'
#' @examples
#' Name <- c("Jon", "Bill", "Maria", "Ben", "Tina")
#' Age <- c(23, 41, 32, 58, 26)
#' Weight <- c(156,98,75,112,74)
#' Height <- c(1.75,1.68,1.56,1.86,1.76)
#' x <- data.frame(Name, Age, Weight, Height)
#' correlacion(x)
correlacion <- function(x){
  # Mira que columnas son numéricas
  bool_numeric = numeric.columns(x)
  num.numeric = sum(bool_numeric)
  if (num.numeric<2){
    stop("Se necesitan al menos 2 variables numéricas")
  }
  # Para cada combinación de 2 columnas numéricas, calcula la correlación entre ellas
  combs = combn(1:num.numeric, 2)
  res = apply(combs, x, bool_numeric, MARGIN=2, FUN = coeficiente.correlacion)
  if (is.null(dim(res))){
    corrs = res
  } else{
    corrs = as.numeric(res[1,])
  }
  names(corrs) = res[2,]
  return(corrs)
}

#Calcula el coeficiente de correlación r dada un array x, 2 índices de columna
#en comb, y cuales son las columnas numéricas especificadas en bool_numeric
coeficiente.correlacion <- function(comb, x, bool_numeric){
  i = comb[1]
  j = comb[2]
  a = x[, bool_numeric][,i]
  b= x[, bool_numeric][,j]
  # El coeficiente de Pearson se calcula dividiendo la covarianza por
  # las desviaciones estándar
  covar = cov(a, b, method = "pearson")
  cor.coef = covar/sd(a)/sd(b)
  orig.names = names(x)[bool_numeric]
  if (is.null(orig.names)){
    new.name = paste(paste("V",i,sep=""),paste("V",j,sep=""),sep="-")
  }else{
    new.name = paste(orig.names[i],orig.names[j],sep="-")
  }
  # Devuelve el coeficiente y el nombre que recibirá en una lista
  return(c(cor.coef, new.name))
}

# Información mutua -------------------------------------------------------

#

#' Información mutua entre 2 vectores
#'
#' @description  Esta función calcula la información mutua entre 2 vectores
#'
#' @param X Vector 1 sobre el cual calcular la información mutua
#' @param Y Vector 2 sobre el cual calcular la información mutua
#'
#' @return Valor numérico que corresponde a la información mutua entre los 2 vectores de entrada
#' @export
#'
#' @examples
#' Name <- c("Jon", "Bill", "Maria", "Ben", "Tina")
#' Age <- c(23, 41, 32, 58, 26)
#' Weight <- c(156,98,75,112,74)
#' Height <- c(1.75,1.68,1.56,1.86,1.76)
#' x <- data.frame(Name, Age, Weight, Height)
#' mutual.information(x)
mutual.information.vectors = function(X,Y){
  return(entropy(X) - conditional.entropy(X,Y))
}

#' Información mutua entre columnas
#'
#' @description Esta función calcula la información mutua entre las columnas
#' de una matriz o dataframe
#'
#' @param x Matriz o dataframe sobre el cual calcular la información mutua
#' @param only.categorical Si calcular la información mutua entre todas las columnas (\code{FALSE} por default) o sólo con las categóricas (\code{TRUE})
#'
#' @return Vector numérico de los valores de información mutua entre las columnas de \code{x}
#' @export
#'
#' @examples
#' Name <- c("Jon", "Bill", "Maria", "Ben", "Tina")
#' Age <- c(23, 41, 32, 58, 26)
#' Weight <- c(156,98,75,112,74)
#' Height <- c(1.75,1.68,1.56,1.86,1.76)
#' x <- data.frame(Name, Age, Weight, Height)
#' mutual.information(x)
mutual.information = function(x, only.categorical=FALSE){
  # range = que columnas se utilizarán para calcular la información mutua
  range = 1:dim(x)[2]
  if (only.categorical){
    bool_numeric = numeric.columns(x)
    # Si solo se quieren utilizar las columnas categóricas, se quitan los
    # índices que corresponden a las numéricas de la variable range
    range = range[!bool_numeric]
  }
  if (length(range)<2){
    stop("Se necesitan al menos 2 columnas")
  }
  # Calcula todas las combinaciones de 2 entre las columnas
  combs = combn(range, 2)
  # Calcula la información mutua para cada combinación
  mutinfo = apply(combs,x, MARGIN=2, FUN = function(comb,x){return(mutual.information.vectors(x[,comb[1]], x[,comb[2]]))})

  if (is.null(names(x))){
    nombres = paste("V",1:dim(x)[2],sep="")
  } else{
    nombres = names(x)
  }
  names(mutinfo) = apply(combs,nombres,MARGIN=2,FUN=function(comb,nombres){return(paste(nombres[comb[1]],nombres[comb[2]],sep="-"))})
  return(mutinfo)
}

# Correlación + Información mutua -----------------------------------------


#' Matriz de correlación e información mutua
#'
#' @description Esta función calcula la información mutua entre las columnas
#' categóricas de una matriz o dataframe, y la correlación entre las numéricas
#'
#' @param x Matriz o dataframe sobre el cual calcular la información mutua y las correlaciones
#'
#' @return Matriz numérica de los valores de información mutua y correlación entre las columnas de \code{x}
#' @export
#'
#' @examples
#' Name <- c("Jon", "Bill", "Maria", "Ben", "Tina")
#' Age <- c(23, 41, 32, 58, 26)
#' Weight <- c(156,98,75,112,74)
#' Height <- c(1.75,1.68,1.56,1.86,1.76)
#' x <- data.frame(Name, Age, Weight, Height)
#' mutual.correlation(x)
mutual.correlation = function(x){
  bool_numeric = numeric.columns(x)
  nombres = names(x)
  if (is.null(nombres)){
    nombres = paste("V",1:dim(x)[2],sep="")
  }
  # Prepara la estructura de la matriz de correlación/info.mutua resultado
  result = matrix(nrow = dim(x)[2], ncol= dim(x)[2], dimnames = replicate(2, nombres, simplify = FALSE))
  # Revisa cuantas columnas numéricas y categóricas hay.
  # Si no hay suficientes numéricas, sólo se tendrá que calcular la información mutua.
  # Si no hay suficientes categóricas, sólo se tendrá que calcular la correlación.
  no.numerics = FALSE
  if (sum(bool_numeric)<2){
    no.numerics=TRUE
    metrics = c()
  }
  else{
    metrics = correlacion(x)
  }
  if (sum(!bool_numeric)<2){
    if (no.numerics==TRUE){
      stop("No hay suficientes columnas numéricas ni categóricas")
    }
  }
  else{
    metrics = c(metrics, mutual.information(x,only.categorical = TRUE))
  }
  # Rellena la matriz de resultado teniendo en cuenta los nombres
  nombres = names(metrics)
  sapply(1:length(metrics), FUN=function(i){
    split = strsplit(nombres[i], "-")[[1]]
    result[split[1],split[2]] <<- metrics[i]
    result[split[2],split[1]] <<- metrics[i] })
  # La diagonal se compondrá de las entropías para las categóricas,
  # y un coeficiente de correlación=1 para las numéricas
  diag(result) = entropy(x)
  diag(result)[bool_numeric] = 1
  return (result)
}


# Correlation plot --------------------------------------------------------

library(ggcorrplot)


#' Gráfico de correlaciones
#'
#' @description Esta función crea un gráfico de tipo mapa de calor para visualizar una matriz de correlaciones dada
#'
#' @param x Matriz de correlaciones
#'
#' @return Diagrama de mapa de calor
#' @export
#'
#' @examples
#' Age <- c(23, 41, 32, 58, 26)
#' Weight <- c(156,98,75,112,74)
#' Height <- c(1.75,1.68,1.56,1.86,1.76)
#' x <- data.frame(Age, Weight, Height)
#' corr.matrix = mutual.correlation(x)
#' corr.plot(corr.matrix)
corr.plot = function(x){
  if(!requireNamespace("ggcorrplot")){
    stop("La función 'corr.plot' requiere la instalación del paquete 'ggcorrplot'.
    Instala este paquete y ejecuta la función de nuevo")
  }
  return(ggcorrplot::ggcorrplot(x, lab = TRUE,tl.srt = 70, digits = 3, show.diag = TRUE, colors = c("white","#dddddd","red")))
}

