#' Una clase S4 para representar una tabla de datos (Dataset)
#'
#' @description
#' La clase Dataset sirve para almacenar datos en una estructura de tabla. Su funcionamiento es similar al de un data.frame de R, pero más limitado.
#' El Dataset está formado por una lista de columnas, cada una de las cuales es una lista de valores. Todos los valores de una columna tienen que ser del mismo tipo.
#' La clase Dataset sólo admite los tipos de datos int, float, bool y str.""".
#'
#'
#' @slot data Lista de columnas que forman el Dataset
#' @slot dtypes Vector de caracteres que indica el tipo de cada columna
#' @slot shape Vector de dos elementos que indica el número de filas y columnas del Dataset
#' @slot colnames Vector de caracteres que indica el nombre de cada columna
#' @slot allowed_types Vector de caracteres que indica los tipos de datos que se pueden almacenar en el Dataset
#' @export
setClass(
  "Dataset",
  representation(
    data = "list",
    dtypes = "character",
    shape = "numeric",
    allowed_types = "character"
  ),
  prototype(
    data = list(),
    shape = c(0,0),
    allowed_types = c("integer", "numeric", "logical", "character")
  )
)

#' Constructor avanzado de la clase Dataset
#'
#' @description Esta función crea un objeto de clase \code{\linkS4class{Dataset}} a partir de una tabla de datos (matriz o data.frame)
#'
#' @param x Tabla de datos (matriz o data frame)
#' @return Un objeto de clase \code{\linkS4class{Dataset}} que representa la tabla de datos pasada como argumento
dataset_from_table <- function(x) {
  object <- new("Dataset")
  names = colnames(x)
  if (is.matrix(x)) {
    x = as.data.frame(x)
  }
  object <- add_cols(object,as.list(x),names=names)
  return(object)
}

# to_dataframe -----------------------------------------------------------------

setGeneric(name="to_dataframe",
           def=function(object)
           {
             standardGeneric("to_dataframe")
           }
)

#' Dataset a data.frame
#'
#' @description Método para convertir un Dataset a un data.frame
#' @param object El Dataset que se quiere convertir
#' @return Un data.frame con los mismos datos que el Dataset de entrada
#'
#' @examples
#' dataset = new("Dataset")
#' dataset = add_col(dataset,c(1,2,3))
#' dataset = add_col(dataset,c("Jon","Bill","James"))
#' dataset = add_col(dataset,c(7,8,9))
#' df = to_dataframe(dataset)
#' df
#'
setMethod(f="to_dataframe",
          signature(object = "Dataset"),
          definition=function(object) {
            nombres = names(object@data)
            df = data.frame(row.names = 1:object@shape[1])
            for(i in seq_along(object@data)){
              df[nombres[i]] = object@data[[i]]
            }
            return(df)
          })

# Función para definir el en que formato se imprime un objeto de clase Dataset
setMethod(f = "show",
          signature = "Dataset",
          definition = function(object){
            cat(paste("Dataset of shape: ", "[",object@shape[1], ",",object@shape[2], "]",sep=""),"\n")
            cat("Data by columns:","\n")
            colnames = names(object@data)
            for (i in seq_along(object@data)) {
              cat(paste(colnames[i], " = [", paste(object@data[[i]],collapse=" ",sep=" "), "] , dtype = ", object@dtypes[i], "\n",sep=""))
            }
          })

# add_col -----------------------------------------------------------------

setGeneric(name="add_col",
           def=function(object,data,name=NULL)
           {
             standardGeneric("add_col")
           }
)

#' Añadir una columna
#'
#' @description Método para añadir una columna al Dataset
#' @param object El Dataset que se quiere modificar
#' @param data Vector de valores de la columna a añadir. Todos los valores tienen que ser del mismo tipo.
#' @param name Nombre de la columna, opcional
#' @return Un nuevo objeto de clase Dataset con la columna añadida
#'
#' @examples
#' dataset = new("Dataset")
#' dataset = add_col(dataset,c(1,2,3))
#' dataset = add_col(dataset,c("Jon","Bill","James"))
#' dataset = add_col(dataset,c(7,8,9))
#' dataset
#'
setMethod(f="add_col",
          signature(object = "Dataset"),
          definition=function(object, data, name = NULL) {
            if (!class(data) %in% object@allowed_types) {
              stop("Sólo se admiten valores de tipo:", paste(object@allowed_types, collapse = ","))
            }
            if (length(data) < 1) {
              stop("data es una lista vacía. No hay nada para añadir")
            }
            if (object@shape[1] != 0 && object@shape[1] != length(data)) {
              stop(paste("La columna tiene que tener", object@shape[1], "valores"))
            }

            index = object@shape[2] + 1
            # Si no se especifica nombre, se le asigna uno por defecto
            if (is.null(name)) {
              name = paste("V", index, sep = "")
            }
            object@data[[name]] = data
            object@shape[2] = object@shape[2] + 1
            object@shape[1] = length(data)
            object@dtypes = c(object@dtypes, class(data))
            return(object)
          })

# add_cols ----------------------------------------------------------------

setGeneric(
  "add_cols",
  function(object, data, names = NULL) {
    standardGeneric("add_cols")
  }
)

#' Añadir varias columnas
#'
#' @description Método para añadir varias columnas al Dataset
#' @param object El Dataset que se quiere modificar
#' @param data Lista de vectores que contienen los valores de las columnas a añadir
#' @param names Vector de nombres de las columnas, opcional
#' @return Un nuevo objeto de clase Dataset con la columnas añadida
#'
#' @examples
#' dataset = new("Dataset")
#' dataset = add_cols(dataset,list(c(1,2,3),c("Jon","Bill","James"),c(7,8,9))
#' dataset
#'
setMethod(
  "add_cols",
  signature(object = "Dataset", data = "list"),
  function(object, data, names = NULL) {
    if (length(data) < 1) {
      stop("data es una lista vacía. No hay nada para añadir")
    }
    for (i in seq_along(data)) {
      if (is.null(names)){
        name = NULL
      } else {
        name = names[i]
      }
      object = add_col(object,data[[i]], name)
    }
    return(object)
  }
)

# add_row -----------------------------------------------------------------

setGeneric(
  "add_row",
  function(object, data) {
    standardGeneric("add_row")
  }
)

#' Añadir una fila
#'
#' @description Método para añadir una fila al Dataset
#' @param object El Dataset que se quiere modificar
#' @param data Vector de valores de la fila a añadir.
#'
#' @return Un nuevo objeto de clase Dataset con la fila añadida
#'
#' @examples
#' dataset = new("Dataset")
#' dataset = add_row(dataset,c(1,"Bill",5))
#' dataset = add_row(dataset,c(2,"Jon",6))
#' dataset
#'
setMethod(
  "add_row",
  signature(object = "Dataset", data = "list"),
  function(object, data) {
    if (length(data) < 1) {
      stop("data es una lista vacía. No hay nada para añadir")
    }
    if (object@shape[2] != 0 && object@shape[2] != length(data)) {
      stop(paste("La fila debe tener", object@shape[2], "valores"))
    }
    for (i in seq_along(data)) {
      if (!class(data[[i]]) %in% object@allowed_types) {
        stop("Sólo se admiten valores de tipo:", paste(object@allowed_types, collapse = ", "))
      }
      if (length(data[[i]]) >1){
        stop("Esta función solo sirve para añadir una sola fila")
      }
      if (length(object@dtypes) > 0 && class(data[[i]]) != object@dtypes[i]) {
        stop(paste("El valor para la columna", i, "debe ser de tipo", object@dtypes[i]))
      }
    }
    for (i in seq_along(data)) {
      # Si no hay columnas, se añade la primera
      if (length(object@dtypes) == 0) {
        object@data[[i]] = data[[i]]
      } else { # Si ya hay columnas, se añade a la última
        object@data[[i]] = c(object@data[[i]], data[[i]])
      }
    }
    # Si no hay nombres, se les asigna uno por defecto
    if (is.null(names(object@data))) {
      new_names = c()
      dtypes = c()
      for (i in seq_along(data)) {
        new_names = c(new_names,paste("V", i, sep = ""))
        dtypes = c(dtypes, class(data[[i]]))

      }
      names(object@data) = new_names
      object@dtypes = dtypes
    }
    object@shape[2] = length(data)
    object@shape[1] = object@shape[1] + 1
    return(object)
  }
)

# add_rows ----------------------------------------------------------------


setGeneric(
  "add_rows",
  function(object, data) {
    standardGeneric("add_rows")
  }
)

#' Añadir varias filas
#'
#' @description Método para añadir varias filas al Dataset
#' @param object El Dataset que se quiere modificar
#' @param data Lista de listas que contienen los valores de las filas a añadir
#'
#' @return Un nuevo objeto de clase Dataset con las filas añadidas
#'
#' @examples
#' dataset = new("Dataset")
#' dataset = add_rows(dataset,list(c(1,"Bill",5),c(2,"Jon",6)))
#' dataset
#'
setMethod(
  "add_rows",
  signature(object = "Dataset", data = "list"),
  function(object, data) {
    if (length(data) < 1) {
      stop("data es una lista vacía. No hay nada para añadir")
    }
    for (i in seq_along(data)) {
      object = add_row(object,data[[i]])
    }
    return(object)
  }
)


# remove_col --------------------------------------------------------------

setGeneric(
  "remove_col",
  function(object, i) {
    standardGeneric("remove_col")
  }
)

#' Quitar una columna
#'
#' @description Método para quitar una columna del Dataset
#' @param object El Dataset que se quiere modificar
#' @param i índice o nombre de la columna a quitar
#' @return Un nuevo objeto de clase Dataset con la columna quitada
#'
#' @examples
#' dataset = new("Dataset")
#' dataset = add_col(dataset,c(1,2,3),name="nums")
#' dataset = add_col(dataset,c("Jon","Bill","James"),name="names")
#' dataset = remove_col(dataset,1)
#' dataset
#' dataset = remove_col(dataset,"names")
#' dataset
setMethod(
  "remove_col",
  signature(object = "Dataset"),
  function(object, i) {
    # Si se pasa un nombre, se convierte a índice
    if (is.character(i)){
      i = match(i,names(object@data))
    }
    object@data = object@data[-i]
    object@dtypes = object@dtypes[-i]
    object@shape[2] = object@shape[2] - 1
    return(object)
  }
)


# remove_cols -------------------------------------------------------------



setGeneric(
  "remove_cols",
  function(object, l) {
    standardGeneric("remove_cols")
  }
)

#' Quitar varias columnas
#'
#' @description Método para quitar varias columnas del Dataset
#' @param object El Dataset que se quiere modificar
#' @param l lista de índices o nombres de las columnas a quitar
#' @return Un nuevo objeto de clase Dataset con las columnas quitadas
#'
#' @examples
#' dataset = new("Dataset")
#' dataset = add_col(dataset,c(1,2,3),name="nums")
#' dataset = add_col(dataset,c("Jon","Bill","James"),name="names")
#' dataset = remove_cols(dataset,list(1,"names"))
#' dataset
setMethod(
  "remove_cols",
  signature(object = "Dataset", l = "list"),
  function(object, l) {
    # Para que funcione bien, hay que borrar las columnas de mayor a menor
    # Por tanto, se ordena la lista de mayor a menor,
    # teniendo en cuenta que i puede ser un nombre o un número
    sorted_indexes=c(rbind(1:length(object@data), names(object@data)))
    l_sorted = l[order(match(l,sorted_indexes),decreasing = TRUE)]
    for (i in l_sorted) {
      object = remove_col(object,i)
    }
    return(object)
  }
)

# remove_row --------------------------------------------------------------


setGeneric(
  "remove_row",
  function(object, i) {
    standardGeneric("remove_row")
  }
)

#' Quitar una fila
#'
#' @description Método para quitar una fila del Dataset
#' @param object El Dataset que se quiere modificar
#' @param i índice de la fila a quitar
#' @return Un nuevo objeto de clase Dataset con la fila quitada
#'
#' @examples
#' dataset = new("Dataset")
#' dataset = add_col(dataset,c(1,2,3))
#' dataset = add_col(dataset,c("Jon","Bill","James"))
#' dataset = remove_row(dataset,2)
#' dataset
#'
setMethod(
  "remove_row",
  signature(object = "Dataset", i = "numeric"),
  function(object, i) {
    object@shape[1] = object@shape[1] - 1
    for (j in seq_along(object@data)) {
      object@data[[j]] = object@data[[j]][-i]
    }
    return(object)
  }
)

# remove_rows -------------------------------------------------------------


setGeneric(
  "remove_rows",
  function(object, v) {
    standardGeneric("remove_rows")
  }
)

#' Quitar varias filas
#'
#' @description Método para quitar varias filas del Dataset
#' @param object El Dataset que se quiere modificar
#' @param v vector de índices de las filas a quitar
#' @return Un nuevo objeto de clase Dataset con las filas quitadas
#'
#' @examples
#' dataset = new("Dataset")
#' dataset = add_col(dataset,c(1,2,3))
#' dataset = add_col(dataset,c("Jon","Bill","James"))
#' dataset = remove_rows(dataset,c(2,1))
#' dataset
#'
setMethod(
  "remove_rows",
  signature(object = "Dataset", v = "numeric"),
  function(object, v) {
    # Para que funcione bien, hay que borrar las filas de mayor a menor
    v_sorted <- sort(v,decreasing=TRUE)
    for (i in v_sorted) {
      object = remove_row(object,i)
    }
    return(object)
  }
)

# length ------------------------------------------------------------------

setGeneric(
  "get_length",
  function(object) {
    standardGeneric("get_length")
  }
)

#' Tamaño del Dataset
#'
#' @description Método para obtener el tamaño del Dataset
#' @param object El Dataset que se quiere medir
#' @return Un integer que indica el número de columnas del Dataset
#'
#' @examples
#' dataset = new("Dataset")
#' dataset = add_col(dataset,c(1,2,3))
#' dataset = add_col(dataset,c("Jon","Bill","James"))
#' get_length(dataset)
setMethod(
  "get_length",
  signature(object = "Dataset"),
  function(object) {
    return(length(object@data))
  }
)

# rename_cols ------------------------------------------------------------------

setGeneric(
  "rename_cols",
  function(object,new_names) {
    standardGeneric("rename_cols")
  }
)

#' Renombrar columnas del Dataset
#'
#' @description Método para renombrar las columnas de un Dataset
#'
#' @param object El Dataset que se quiere modificar
#' @param new_names Vector de carácteres que recoge los nuevos nombres
#' @return Un nuevo objeto de clase Dataset con las columnas renombradas
#'
#' @examples
#' dataset = new("Dataset")
#' dataset = add_col(dataset,c(1,2,3))
#' dataset = add_col(dataset,c("Jon","Bill","James"))
#' dataset = rename_cols(dataset,c("Index","Name"))
#' dataset
setMethod(
  "rename_cols",
  signature(object = "Dataset",new_names = "character"),
  function(object,new_names) {
    names(object@data) = new_names
    return(object)
  }
)


# Indexing ----------------------------------------------------------------

setGeneric(
  "getitem",
  function(object,i,j=NULL) {
    standardGeneric("getitem")
  }
)

#' Indexar un Dataset
#'
#' @description Método para indexar un Dataset. El primer índice indica la columna y el segundo la fila.
#' Si no se especifica el segundo índice, se devuelve la columna completa.
#' @param object El Dataset que se quiere indexar
#' @param i índice o nombre de la columna a indexar
#' @param j índice de la fila a indexar, opcional
#' @return Un vector con los valores de la columna o un valor concreto si se especifica el segundo índice
#'
#' @examples
#' dataset = new("Dataset")
#' dataset = add_col(dataset,c(1,2,3))
#' dataset = add_col(dataset,c("Jon","Bill","James"))
#' getitem(dataset,"V1")
#' getitem(dataset,"V1",3)
#' getitem(dataset,1,1)
#'
setMethod(
  "getitem",
  signature(object = "Dataset"),
  function(object, i, j=NULL) {
    if (is.null(j)){
      return(object@data[[i]])
    }
    return(object@data[[i]][j])
  }
)

