% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlmutua.R
\name{corr.plot}
\alias{corr.plot}
\title{Gráfico de correlaciones}
\usage{
corr.plot(x)
}
\arguments{
\item{x}{Matriz de correlaciones}
}
\value{
Diagrama de mapa de calor
}
\description{
Esta función crea un gráfico de tipo mapa de calor para visualizar una matriz de correlaciones dada
}
\examples{
Age <- c(23, 41, 32, 58, 26)
Weight <- c(156,98,75,112,74)
Height <- c(1.75,1.68,1.56,1.86,1.76)
x <- data.frame(Age, Weight, Height)
corr.matrix = mutual.correlation(x)
corr.plot(corr.matrix)
}
