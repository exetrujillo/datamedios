#' Parser de Fuentes
#'
#' Esta funcion toma un string que contiene nombres de fuentes separados por comas
#' y devuelve una lista con cada fuente como un elemento separado, sin espacios en blanco adicionales.
#'
#' @param cadena Un string que contiene nombres de fuentes separados por comas.
#' @return Una lista de strings, cada uno representando una fuente sin espacios en blanco adicionales.
#' @examples
#' parserFuentes("bbcl, emol, mediosregionales, ")
#' parserFuentes(" emol-todas, bbcl")
#' @export

parserFuentes <- function(cadena) {
  # Validacion: Verificar que el argumento no sea nulo
  if (is.null(cadena)) {
    stop("Error: El argumento 'cadena' no puede ser nulo.")
  }

  # Validacion: Verificar que el argumento sea un string
  if (!is.character(cadena) || length(cadena) != 1) {
    stop("Error: El argumento 'cadena' debe ser un string.")
  }

  # Validacion: Manejar el caso de una cadena vacia
  if (nchar(cadena) == 0) {
    return(list())
  }

  # Separar la cadena por comas
  elementos <- unlist(strsplit(cadena, split = ","))

  # Eliminar espacios en blanco adicionales de cada elemento
  elementos_limpios <- trimws(elementos)

  # Filtrar elementos no vacios
  elementos_no_vacios <- elementos_limpios[elementos_limpios != ""]

  # Definir los valores permitidos
  valores_permitidos <- c("bbcl", "emol", "guioteca", "mediosregionales", "emol-todas")

  # Filtrar elementos que no estan en los valores permitidos
  elementos_validos <- elementos_no_vacios[elementos_no_vacios %in% valores_permitidos]

  # Si 'emol-todas' esta presente, eliminar 'emol', 'guioteca' y 'mediosregionales'
  if ("emol-todas" %in% elementos_validos) {
    elementos_validos <- elementos_validos[!elementos_validos %in% c("emol", "guioteca", "mediosregionales")]
  }

  # Retornar como lista
  return(as.list(elementos_validos))
}
