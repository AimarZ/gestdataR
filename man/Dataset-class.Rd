% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\docType{class}
\name{Dataset-class}
\alias{Dataset-class}
\title{Una clase S4 para representar una tabla de datos (Dataset)}
\description{
La clase Dataset sirve para almacenar datos en una estructura de tabla. Su funcionamiento es similar al de un data.frame de R, pero más limitado.
El Dataset está formado por una lista de columnas, cada una de las cuales es una lista de valores. Todos los valores de una columna tienen que ser del mismo tipo.
La clase Dataset sólo admite los tipos de datos int, float, bool y str.""".
}
\section{Slots}{

\describe{
\item{\code{data}}{Lista de columnas que forman el Dataset}

\item{\code{dtypes}}{Vector de caracteres que indica el tipo de cada columna}

\item{\code{shape}}{Vector de dos elementos que indica el número de filas y columnas del Dataset}

\item{\code{colnames}}{Vector de caracteres que indica el nombre de cada columna}

\item{\code{allowed_types}}{Vector de caracteres que indica los tipos de datos que se pueden almacenar en el Dataset}
}}

