# Varianza ----------------------------------------------------------------

# Esta función calcula la varianza de un vector numérico
varianza_vector <- function(x){
  if (!is.numeric(x)){
    stop("Vector no numérico")
  }
  return(sum((x-mean(x))**2) / (length(x)-1))
}

# Esta función calcula la varianza de las columnas numéricas de una matriz o dataframe
varianza_tabla <- function(x){
  result = rep(NA,dim(x)[2])
  bool_numeric = numeric.columns(x)
  if (any(bool_numeric)){
    result[bool_numeric] = apply(x[,bool_numeric], FUN=varianza_vector, MARGIN=2)
  }
  names(result) = names(x)
  return (result)
}

#' Varianza de arrays
#'
#' @description Esta función calcula la varianza de un vector numérico o de
#' cada columna numérica de un matriz o dataframe
#'
#' @param x Array de valores sobre los cuales calcular la varianza
#'
#' @return En el caso de que \code{x} sea un vector, devolverá el valor de varianza.
#' Si no, devolverá un vector que recoja las varianzas de las columnas numéricas de \code{x}
#' @export
#'
#' @examples
#' x <- matrix(rnorm(30,5,10),ncol=3)
#' varianza(x)
varianza = function(x){
  if (is.vector(x)){
    return(varianza_vector(x))
  }
  else if (is.data.frame(x) || is.matrix(x)){
    if (any(dim(x)==0)){
      stop("Alguna dimensión es 0")
    }
    return(varianza_tabla(x))
  }
  else{
    stop("Tipo del párametro x incorrecto")
  }
}


# Entropy -----------------------------------------------------------------

# Esta función calcula la entropía de un vector
entropy_vector <- function(x){
  probs = table(x)/length(x)
  logits = -probs* log2(probs)
  entropia = sum(logits, na.rm = TRUE)
  return (entropia)
}

# Esta función calcula la entropía para cada columna de una matriz o dataframe
# Si only.categorical es TRUE, lo hace sólo para las categóricas.
entropy_tabla = function(x, only.categorical = FALSE){
  result = rep(NA,dim(x)[2])
  names(result) = names(x)
  if (only.categorical==TRUE){
    bool_numeric = numeric.columns(x)
  }else{
    bool_numeric = rep(FALSE,dim(x)[2])
  }
  bool_categoric = !bool_numeric
  if (any(bool_categoric)){
    result[bool_categoric] = apply(x[,bool_categoric], FUN=entropy_vector, MARGIN=2)
  }
  return (result)
}

#' Entropía de un array
#'
#' @description Esta función calcula la entropía de un vector o de cada una de
#' las columnas de una matriz o dataframe
#'
#' @param x Vector, matriz o dataframe sobre el que calcular la entropía
#' @param only.categorical Si calcular la información mutua entre todas las columnas (\code{FALSE} por default) o sólo con las categóricas (\code{TRUE})
#'
#' @return En el caso de que \code{x} sea un vector, devolverá el valor de entropía.
#' Si no, devolverá un vector que recoja las entropías de las columnas de \code{x}
#' @export
#'
#' @examples
#' Name <- c("Jon", "Bill", "Maria", "Ben", "Tina")
#' Age <- c(23, 41, 32, 58, 26)
#' Weight <- c(156,98,75,112,74)
#' Height <- c(1.75,1.68,1.56,1.86,1.76)
#' x <- data.frame(Name, Age, Weight, Height)
#' entropy(x)
entropy = function(x, only.categorical=FALSE){
  if (is.vector(x) || is.factor(x)){
    return(entropy_vector(x))
  }
  else if (is.data.frame(x) || is.matrix(x)){
    if (any(dim(x)==0)){
      stop("Alguna dimensión es 0")
    }
    return(entropy_tabla(x,only.categorical))
  }
  else{
    stop("Tipo del párametro x incorrecto")
  }
}


# ROC ---------------------------------------------------------------


#' Curva ROC
#'
#' @description Esta función calcula los puntos de la curva ROC para
#' la predicción de una variable binaria y visualiza el gráfico
#'
#' @param x Todos los datos recogidos en una matriz numérica o dataframe
#' @param attribute.variable Índice o nombre de la columna que corresponde a los atributos
#' numéricos que se utilizan para hacer la predicción
#' @param predict.variable Índice o nombre de la columna binaria que se quiere predecir
#'
#' @return Lista con 2 elementos:
#'  1) Coordenadas del eje X de la curva ROC (valores FPR)
#'  2) Coordenadas del eje Y de la curva ROC (valores TPR)
#' @export
#'
#' @examples
#' X <- c(3.5,7.4,1.2,2.0,3.9,5.3,6.1,6.9)
#' Y <- c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE)
#' df <- data.frame(X,Y)
#' ROC(df,1,2)
ROC = function(x,attribute.variable,predict.variable){
  if (!(is.data.frame(x) || is.matrix(x))){
    stop("x debe ser una matriz o un dataframe")
  }
  # Intenta utilizar el índice de la variable objetivo para indexar
  tryCatch(
    {
      binary.variable = x[,predict.variable]
    },
    error=function(e) {
      stop("El índice o nombre de la variable objetivo no es correcto")
  })
  if (!is.binary(binary.variable)){
    stop("La variable objetivo no es de tipo binario")
  }
  # Intenta utilizar el índice de la variable atributo para indexar
  tryCatch(
    {
      x[,attribute.variable]
    },
    error=function(e) {
      stop("El índice o nombre de la variable de características no es correcto")
    })
  if (!is.numeric(x[,attribute.variable])){
    stop("La variable de características no es de tipo numérico")
  }
  # Ordena el array teniendo en cuenta los atributos
  x <- x[order(x[,attribute.variable]),]
  binary.variable = x[,predict.variable]
  # aux = min(variable de atributo)-1
  aux = x[1,attribute.variable]-1
  coords_x = c()
  coords_y = c()
  # El numero de puntos será igual al número de 'rows'+1
  for (i in 1:(dim(x)[1]+1)){
    # Haz una predicción suponiendo un punto de corte
    punto.corte = c(aux,x[,attribute.variable])[i]
    predictions = x[,attribute.variable]>punto.corte
    # Mira si la predicción y el objetivo son iguales
    equal_comp = predictions == binary.variable
    # Calcula las métricas
    TP = sum(equal_comp[predictions])
    TN = sum(equal_comp[!predictions])
    FP = sum(!equal_comp[predictions])
    FN = sum(!equal_comp[!predictions])
    TPR = TP/(TP+FN)
    FPR = FP/(FP+TN)
    # Añade el TPR y FPR a las coordenadas
    coords_x = c(coords_x,FPR)
    coords_y = c(coords_y,TPR)
  }
  plot(coords_x,coords_y, type="b", ylab="sensitivity", xlab="1- specifity")
  return(list(coords_x,coords_y))
}

# AUC ---------------------------------------------------------------

#' Valor AUC
#'
#' @description Esta función calcula el área de debajo de la curva ROC, es decir,
#' el valor AUC (Area Under the Curve)
#'
#' @param coords.list Lista de 2 elementos: coordenadas X e Y de los puntos que conforman la curva ROC
#'
#' @return Valor que corresponde al AUC
#' @export
#'
#' @examples
#' X <- c(3.5,7.4,1.2,2.0,3.9,5.3,6.1,6.9)
#' Y <- c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE)
#' df <- data.frame(X,Y)
#' coords.list = ROC(df,"X","Y")
#' AUC(coords.list)
AUC <- function(coords.list) {
  x= coords.list[[1]]
  y = coords.list[[2]]
  # Calcula la integral dados los puntos (x1,y1), (x2,y2) ...
  delta.x <- diff(x)
  mean.y <- rowMeans(cbind(y[-1], y[-length(y)]))
  return(abs(sum(delta.x * mean.y)))
}


# Métricas ----------------------------------------------------------------


#' Métricas de tablas
#'
#' @description Esta función calcula las varianzas de las columnas numéricas y
#' las entropías de las categóricas de una matriz o dataframe. Si se especifican
#' una variable predictora y una columna objetivo, también calcula la curva ROC
#' y el valor AUC.
#'
#' @param x Matriz numérica o dataframe que contiene los datos
#' @param attribute.variable Índice o nombre de la columna que corresponde a los atributos
#' que se utilizan para hacer la predicción (\code{NA} por defecto)
#' @param predict.variable Índice o nombre de la columna binaria que se quiere predecir
#' (\code{NA} por defecto)
#'
#' @return Vector numérico que contiene todas las métricas
#' @export
#'
#' @examples
#' Name <- c("Jon", "Bill", "Maria", "Ben", "Tina")
#' Age <- c(23, 41, 32, 58, 26)
#' Weight <- c(156,98,75,112,74)
#' Height <- c(1.75,1.68,1.56,1.86,1.76)
#' Working <- c(FALSE, TRUE, FALSE, TRUE, TRUE)
#' x <- data.frame(Name, Age, Weight, Height, Working)
#' metricas(x,"Age","Working")
metricas = function(x,attribute.variable=NA,predict.variable=NA){
  if (!(is.data.frame(x) || is.matrix(x))){
    stop("x debe ser una matriz o un dataframe")
  }
  nombres = c(names(x))
  # Calcula la varianza de las columnas numéricas
  result = varianza(x)
  # Las columnas categóricas tendrán el valor NA. Hay que localizarlas
  mask.na = is.na(result)
  if (any(mask.na)){
    # Los valores NA hay que reemplazarlos por la entropía
    result[mask.na] = entropy(x[,mask.na])
  }
  if (!is.na(predict.variable) && !is.na(attribute.variable)){
    coords.list = ROC(x,attribute.variable,predict.variable)
    result = c(result, AUC(coords.list))
    nombres = c(names(x),"AUC")
  }
  names(result) = nombres
  return(result)
}


# Filtrado de variables ---------------------------------------------------

#' Filtrado de variables
#'
#' @description Esta función elimina las columnas de una matriz o dataframe
#' que no cumplan una condición dada respectiva a su varianza o entropía.
#'
#' @param x Matriz o dataframe que se quiere filtrar
#' @param condition Condición que debe cumplir la métrica de una columna
#' para que se conserve
#' @param use.entropy Si se quiere utilizar la entropía como métrica de filtrado
#' (\code{TRUE} por defecto)
#' @param use.varianza Si se quiere utilizar la varianza como métrica de filtrado
#' (\code{FALSE} por defecto)
#'
#' @return Matriz o dataframe con las columnas filtradas
#' @export
#'
#' @examples
#' Name <- c("Jon", "Bill", "Maria", "Ben", "Tina")
#' Age <- c(23, 41, 32, 58, 26)
#' Weight <- c(156,98,75,112,74)
#' Height <- c(1.75,1.68,1.56,1.86,1.76)
#' Working <- c(FALSE, TRUE, FALSE, TRUE, TRUE)
#' x <- data.frame(Name, Age, Weight, Height, Working)
#' filtrar(x, condition="< 2",use.varianza = TRUE, use.entropy = TRUE)
filtrar = function(x, condition, use.entropy = TRUE, use.varianza = FALSE){
  if (!is.character(condition)){
    stop("La condición tiene que ser de tipo 'character' ")
  }
  if (!(is.data.frame(x) || is.matrix(x))){
    stop("x debe ser una matriz o un dataframe")
  }
  if((use.entropy || use.varianza) == FALSE){
    stop("No se ha especificado ninguna métrica")
  }
  if ((use.entropy && use.varianza) == TRUE){
    metrics = metricas(x)
  } else if (use.entropy == TRUE){
    metrics = entropy(x, only.categorical=TRUE)
  } else{
    metrics = varianza(x)
  }
  # Las columnas que se mantendrán. Por ahora, todas
  keep.columns = rep(TRUE, dim(x)[2])
  mask_not_na = !is.na(metrics)
  # Trata de evaluar la condición dada
  tryCatch(
    {
      # Crea un bloque de código en formato string en base a la condición
      statement = paste("for (i in seq_along(keep.columns[mask_not_na])){\n",
                        "elem = metrics[mask_not_na][i]\n",
                        "keep.columns[mask_not_na][i] <- elem ", condition, "}\n",
                        sep="")

      #statement = paste("keep.columns[mask_not_na] <- metrics[mask_not_na]", condition)
      # Ejecuta el string como si fuera una orden
      eval(parse(text=statement))
    },
    error=function(e) {
      stop("La condición especificada no es válida")
    })
  return (x[,keep.columns])
}
