% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{remove_rows,Dataset,numeric-method}
\alias{remove_rows,Dataset,numeric-method}
\title{Quitar varias filas}
\usage{
\S4method{remove_rows}{Dataset,numeric}(object, v)
}
\arguments{
\item{object}{El Dataset que se quiere modificar}

\item{v}{vector de índices de las filas a quitar}
}
\value{
Un nuevo objeto de clase Dataset con las filas quitadas
}
\description{
Método para quitar varias filas del Dataset
}
\examples{
dataset = new("Dataset")
dataset = add_col(dataset,c(1,2,3))
dataset = add_col(dataset,c("Jon","Bill","James"))
dataset = remove_rows(dataset,c(2,1))
dataset

}
