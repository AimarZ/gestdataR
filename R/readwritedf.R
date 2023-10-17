#' Guardado de datos en fichero
#'
#' @description
#' Esta función escribe y guarda una matriz o dataframe de entrada en el
#' fichero especificado. El formato es el siguiente (los símbolos <> indican que
#' la información es opcional):
#'
#' tipodeobjeto,númerofilas,númerocolumnas<,nombre>
#' <#Rownames: nombrefila1,nombrefila2,nombrefila3...>
#' <#Colnames: nombrecolumna1,nombrecolumna2,nombrecolumna3...>
#' datos,de,columna,1,separados,por,coma
#' datos,de,columna,2,separados,por,coma
#' datos,de,columna,3,separados,por,coma
#' ...
#'
#'
#'
#' @param x Matriz o dataframe a guardar. También puede ser una lista, donde
#' el primer elemento especifica el nombre y el segundo el dataset a guardar
#' @param file.path Dirección y nombre del fichero en donde guardar los datos
#'
#' @export
#'
#' @examples
#' Name = c("Jon", "Bill", "Maria", "Ben", "Tina")
#' Age = c(23, 41, 32, 58, 26)
#' Weight = c(156,98,75,112,74)
#' Height = c(1.75,1.68,1.56,1.86,1.76)
#' x = data.frame(Name, Age, Weight, Height)
#' write.dataframe(list("MiDataset",x),"custom_data.txt")
write.dataframe = function (x, file.path) {
  # Si es una lista, hay que desglosarla
  if (is.list(x) && !is.data.frame(x)){
    if (class(x[[1]]) == "character") {
      name = paste(",",x[[1]], sep="")
      } else {
      stop("El primer elemento de la lista debe ser un string")
    }
    x = x[[2]]
  }
  else{
    name = ""
  }
  if (is.data.frame(x)){
    obj.type = "data.frame"
  }
  else if (is.matrix(x)){
    obj.type = "matrix"
  }
  else{
    stop("Sólo se admiten matrices y dataframes para guardar")
  }
  if (0 %in% dim(x)){
    stop("Alguna dimensión de x es 0")
  }
  # Abrir el fichero
  con = file(description = file.path, open="w")
  # Escribe la primera línea siguiendo el formato
  writeLines(paste(obj.type, dim(x)[1], paste(dim(x)[2],name,sep=""), sep=","), con=con)
  # Si el dataset tiene nombres de filas, hay que escribirlos
  if (!is.null(rownames(x))){
    writeLines(paste("#Rownames: ", paste(rownames(x), collapse=","),
                     sep=""), con=con)
  }
  # Si el dataset tiene nombres de columnas, hay que escribirlos
  if (!is.null(colnames(x))){
    writeLines(paste("#Colnames: ", paste(colnames(x), collapse=","),
                     sep=""), con=con)
  }
  # Escribe las columnas del dataset, una en cada línea y con los valores
  # separados por coma
  lines = apply(x, MARGIN=2,
                FUN=function(col) {
                  return(paste(col, collapse=","))
                })
  writeLines(lines, con=con)
  close(con)
}

#' Lectura de datos de un fichero
#'
#' @description
#' Esta función lee un dataset desde un fichero especificado que haya sido
#' creado con el método gemelo \code{write.dataframe}
#'
#'
#' @param file.path Dirección y nombre del fichero a leer
#'
#' @return Matriz o dataframe construido a partir de los datos leídos en el
#' fichero. Si el dataset está nombrado, devolverá una lista donde el primer
#' elemento especificará el nombre y el segundo el dataset
#' @export
#'
#' @examples
#' Name = c("Jon", "Bill", "Maria", "Ben", "Tina")
#' Age = c(23, 41, 32, 58, 26)
#' Weight = c(156,98,75,112,74)
#' Height = c(1.75,1.68,1.56,1.86,1.76)
#' x = data.frame(Name, Age, Weight, Height)
#' write.dataframe(list("MiDataset",x),"custom_data.txt")
#' read.dataframe("custom_data.txt")
read.dataframe = function(file.path) {
  # Lee las líneas del fichero
  con = file(description=file.path, open="r")
  lines = readLines(con=con)
  close(con)
  # Separa la primera línea por comas
  line1.info = strsplit(lines[1],",")[[1]]
  if (length(line1.info)<3){
    stop("La primera línea tiene que tener uno de los formatos:\n",
         "tipo,númerodefilas,númerodecolumnas\n",
         "tipo,númerodefilas,númerodecolumnas,nombre")
  }
  # Descifra el tipo de objeto que se está leyendo
  obj.type = line1.info[1]
  if (obj.type != "matrix" && obj.type != "data.frame"){
    stop("El objeto a leer tiene que ser una matriz o un dataframe")
  }
  # Consigue las dimensiones del dataset
  num.rows = as.numeric(line1.info[2])
  num.columns = as.numeric(line1.info[3])
  # Descubre si se ha escrito el nombre del dataset. En tal caso, guardalo
  var.name = NULL
  if (length(line1.info)>3){
    var.name = line1.info[4]
  }
  nexti = 2
  nextline = lines[nexti]
  #Mira si se han especificado los nombres de las filas
  row.names = NULL
  if (substr(nextline, 1, 11) == "#Rownames: "){
    row.names = strsplit(substr(nextline,12, nchar(nextline)),",")[[1]]
    if (length(row.names)!=num.rows){
      stop("La dimensión leída no corresponde con el número de filas")
    }
    nexti = 3
    nextline = lines[nexti]
  }
  #Mira si se han especificado los nombres de las columnas
  column.names = NULL
  if (substr(nextline, 1, 11) == "#Colnames: "){
    column.names = strsplit(substr(nextline,12, nchar(nextline)),",")[[1]]
    if (length(column.names)!=num.columns){
      stop("La dimensión leída no corresponde con el número de columnas")
    }
    nexti = 4
    nextline = lines[nexti]
  }
  # Crea una matriz con las especificaciones leídas
  x = matrix(nrow=num.rows,ncol=num.columns, dimnames = list(row.names,column.names))
  # Si el objeto que se está leyendo es un data.frame, hay que convertirlo
  if (obj.type == "data.frame"){
    x = data.frame(x)
  }
  # Consigue los valores del dataset

  # all.numeric servirá para saber si todos los valores leídos han sido numéricos
  all.numeric = TRUE
  #Por cada línea restante, añádela a la variable x
  lines = lines[-(1:nexti-1)]
  ignore = sapply(1:length(lines),lines,
            FUN=function(i,lines) {
              line = lines[i]
              column = unlist(strsplit(line, ","))
              numeric.column = suppressWarnings(as.numeric(column))
              if (any(is.na(numeric.column))){
                all.numeric = FALSE
                # Si la columna es numérica y el dataset es un dataframe,
                # hay que convertir la columna a numérica
              } else if (obj.type=="data.frame"){
                column = numeric.column
              }
              x[,i] <<- column
            })
  # Si todos los valores de la matriz han sido numéricos, hay que convertirlo a numérico
  if (all.numeric & obj.type=="matrix"){
    x = as.numeric(x)
  }

  # Si se ha leído el nombre del dataset, hay que devolver una lista
  if (!is.null(var.name)) {
    object = list(Name=var.name, dataset=x)
  } else {
    object = x
  }
  return(object)
}


#' Guardado de dataframe en fichero CSV
#'
#' @description
#' Esta función escribe y guarda los datos de un dataframe de entrada en el
#' fichero tipo CSV especificado. El formato es el siguiente:
#'
#'  ,nombrecolumna1,nombrecolumna2,nombrecolumna3...
#'  nombrefila1,datos,de,fila,1,separados,por,coma
#'  nombrefila2,datos,de,fila,2,separados,por,coma
#'  nombrefila3,datos,de,fila,3,separados,por,coma
#'  ...
#'
#'
#' @param x Dataframe a guardar.
#' @param file.path Dirección y nombre del fichero de tipo CSV en donde guardar los datos
#'
#' @export
#'
#' @examples
#' data = data.frame(
#' Nombre = c("Alice", "Bob", "Charlie"),
#' Edad = c(25, 30, 35),
#' Ciudad = c("Nueva York", "Los Ángeles", "Chicago")
#' )
#' rownames(data) = c("Primero","Segundo","Tercero")
#' dataframe2csv(data, "ejemplo.csv")
dataframe2csv = function(x, file.path) {
  if (substring(file.path, nchar(file.path) - 3, nchar(file.path)) != ".csv") {
    stop("El archivo no es de tipo .csv")
  }

  if (!is.data.frame(x)) {
    stop("Solo se admiten data frames para guardar")
  }

  if (nrow(x) == 0 || ncol(x) == 0) {
    stop("Alguna dimensión de x es 0")
  }

  # Abre el archivo CSV para escritura
  con = file(file.path, "w")

  # Escribe los nombres de las columnas
  writeLines(paste(",",paste(names(x), collapse = ","),sep=""), con)

  # Escribe cada fila de datos con su nombre
  names = rownames(x)
  if (is.null(names)){
    names = 1:(dim(a)[0])
  }
  for (i in 1:nrow(x)) {
    row = paste(names[i],paste(x[i, ], collapse = ","),sep=",")
    writeLines(row, con)
  }

  # Cierra el archivo
  close(con)
}


#' Lectura de datos de un fichero csv
#'
#' @description
#' Esta función lee un dataset desde un fichero de formato CSV
#'
#'
#' @param file.path Dirección y nombre del fichero .csv a leer
#'
#' @return Dataframe construido a partir de los datos leídos en el fichero.
#'
#' @export
#'
#' @examples
#' data = data.frame(
#' Nombre = c("Alice", "Bob", "Charlie"),
#' Edad = c(25, 30, 35),
#' Ciudad = c("Nueva York", "Los Ángeles", "Chicago")
#' )
#' rownames(data) = c("Primero","Segundo","Tercero")
#' dataframe2csv(data, "ejemplo.csv")
#' csv2dataframe("ejemplo.csv")
csv2dataframe = function(file.path) {
  if (substring(file.path, nchar(file.path) - 3, nchar(file.path)) != ".csv") {
    stop("El archivo no es de tipo .csv")
  }

  # Abre el archivo CSV para lectura
  con = file(file.path, "r")

  # Lee la primera línea que contiene los nombres de las columnas
  col_names = unlist(strsplit(readLines(con, n = 1), split = ","))[-1]

  # Inicializa una lista para almacenar los datos
  data_list = list()

  row_names = c()
  # Lee las líneas de datos y las almacena en la lista
  line = readLines(con, n = 1)
  while (length(line) > 0) {
    line_data = unlist(strsplit(line, split = ","))
    row_names= c(row_names,line_data[1])
    row_data = line_data[-1]
    data_list = c(data_list, list(row_data))
    line = readLines(con, n = 1)
  }

  # Cierra el archivo
  close(con)

  # Convierte la lista de datos en un dataframe
  data = as.data.frame(do.call(rbind, data_list), stringsAsFactors = FALSE)
  colnames(data) = col_names
  rownames(data) = row_names

  return(data)
}

