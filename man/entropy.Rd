% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{entropy}
\alias{entropy}
\title{Entropía de un array}
\usage{
entropy(x, only.categorical = FALSE)
}
\arguments{
\item{x}{Vector, matriz o dataframe sobre el que calcular la entropía}

\item{only.categorical}{Si calcular la información mutua entre todas las columnas (\code{FALSE} por default) o sólo con las categóricas (\code{TRUE})}
}
\value{
En el caso de que \code{x} sea un vector, devolverá el valor de entropía.
Si no, devolverá un vector que recoja las entropías de las columnas de \code{x}
}
\description{
Esta función calcula la entropía de un vector o de cada una de
las columnas de una matriz o dataframe
}
\examples{
Name <- c("Jon", "Bill", "Maria", "Ben", "Tina")
Age <- c(23, 41, 32, 58, 26)
Weight <- c(156,98,75,112,74)
Height <- c(1.75,1.68,1.56,1.86,1.76)
x <- data.frame(Name, Age, Weight, Height)
entropy(x)
}
