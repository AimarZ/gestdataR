# boxplot -----------------------------------------------------------------

#' Boxplot
#'
#' @description
#' Esta función visualiza los valores numéricos de un dataframe en un gráfico de tipo boxplot.
#'
#'
#' @param x Dataframe a visualizar
#' @param together Valor boolear que especifica si se quieren ver todas las variables en un mismo boxplot o no
#'
#' @export
#'
#' @examples
#' Name <- c("Jon", "Bill", "Maria", "Ben", "Tina")
#' Age <- c(23, 41, 32, 58, 26)
#' Weight <- c(156,98,75,112,74)
#' Height <- c(1.75,1.68,1.56,1.86,1.76)
#' Hair = c("Long", "Short","Long", "Short","Long")
#' Independent = c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' x <- data.frame(Name, Age, Weight, Height, Hair, Independent)
#' boxplot_dataset(x,together=FALSE)
boxplot_dataset = function(x, together = TRUE){
  x= x[numeric.columns(x)]
  if(together){
    boxplot(x)
  }else{
    for (name in colnames(x)){
      boxplot(x[name],main=paste(name))
      if (colnames(x)[length(x)] != name){
        readline("Presiona Enter para ver el siguiente gráfico")
      }
    }
  }
}




# pie plot ---------------------------------------------------------------------

#' Pie plot
#'
#' @description
#'Esta función visualiza las variables de un dataframe distintos gráficos tipo pieplot ("tarta").
#'
#'
#' @param x Dataframe a visualizar
#' @param only_categorical Valor boolear que especifica si sólo se quieren visualizar sólo las variables categóricas o no
#'
#' @export
#'
#' @examples
#' Name <- c("Jon", "Bill", "Maria", "Ben", "Tina")
#' Age <- c(23, 41, 32, 58, 26)
#' Weight <- c(156,98,75,112,74)
#' Height <- c(1.75,1.68,1.56,1.86,1.76)
#' Hair = c("Long", "Short","Long", "Short","Long")
#' Independent = c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' x <- data.frame(Name, Age, Weight, Height, Hair, Independent)
#' pieplot(x,only.categorical=TRUE)
pieplot = function(x, only.categorical=TRUE){
  if (only.categorical){
    x = x[!numeric.columns(x)]
  }
  for (name in colnames(x)){
    pie(table(x[name]),labels=names(table(x[name])),main=paste(name,"variable distribution"))
    if (colnames(x)[length(x)] != name){
      readline("Presiona Enter para ver el siguiente gráfico")
    }
  }
}
