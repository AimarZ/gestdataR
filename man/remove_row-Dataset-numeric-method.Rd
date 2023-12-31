% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{remove_row,Dataset,numeric-method}
\alias{remove_row,Dataset,numeric-method}
\title{Quitar una fila}
\usage{
\S4method{remove_row}{Dataset,numeric}(object, i)
}
\arguments{
\item{object}{El Dataset que se quiere modificar}

\item{i}{índice de la fila a quitar}
}
\value{
Un nuevo objeto de clase Dataset con la fila quitada
}
\description{
Método para quitar una fila del Dataset
}
\examples{
dataset = new("Dataset")
dataset = add_col(dataset,c(1,2,3))
dataset = add_col(dataset,c("Jon","Bill","James"))
dataset = remove_row(dataset,2)
dataset

}
