#' Inicializa una solicitud a emol.com y retorna maximo 10 noticias
#'
#' Esta funcion auxiliar llama a emol.com utilizando una frase de busqueda.
#' Entrega como maximo 10 resultados. Se debe llamar desde otras funciones solo
#' con una fuente a la vez, es decir, sin llamar a emol-todas.
#'
#' @param search_query Una frase de busqueda (obligatoria).
#' @param page La pagina de busqueda para iterar, es un int
#' @param fuentes Es un string que deberia tener solo fuentes de emol posibles separadas por comas.
#' @return Un dataframe con el caso de la busqueda, incluyendo solo columnas especificas.
#' @examples
#' primer_caso <- iteracion_emol("Boric", fuentes="emol-todas")
#' @export

iteracion_emol <- function(search_query, page=0, fuentes="emol-todas") {
  # Validamos el parametro
  if (missing(search_query) || !is.character(search_query)) {
    stop("Debe proporcionar una frase de busqueda como texto.")
  }
  # Hay que validar el formato del string fuente

  # Parametros de configuracion y adicionales
  batch_size <- 10
  data_initial <- list()
  patronBusqueda <- ""

  if(fuentes=="emol-todas"){
    patronBusqueda <- "emol,mediosregionales,guioteca"
  } else {
    fuentesParsed <- parserFuentes(fuentes)
    if(length(fuentesParsed) > 0){
      patronBusqueda <- paste0(fuentesParsed, collapse = ",")
    } else {
      message("Error al parsear las fuentes")
    }
  }


  # URL iteracion
  url_iteracion <- paste0(
    "https://newsapi.ecn.cl/NewsApi/emol/buscador/",
    patronBusqueda,
    "?q=",URLencode(search_query),
    "&size=10&from=", (page*batch_size)
  )

  # Solicitud iteracion
  response_iteracion <- httr::GET(url_iteracion)

  if (response_iteracion$status_code == 200) {
    data_iteracion <- httr::content(response_iteracion, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)

    if (!is.null(data_iteracion$hits$hits) && length(data_iteracion$hits$hits) > 0) {
      datos_iteracion <- as.data.frame(data_iteracion$hits$hits)

      columnas_deseadas <- c(
        "_id",
        "_source.titulo",
        "_source.texto",
        "_source.permalink",
        "_source.tablas.tablaMedios",
        "_source.imagen",
        "_source.autor",
        "_source.bajada",
        "_source.seccion",
        "_source.subSeccion",
        "_source.fechaPublicacion",
        "_source.fechaModificacion"
      )

      # Filtrar solo las columnas que existen en el dataframe
      columnas_disponibles <- columnas_deseadas[columnas_deseadas %in% names(datos_iteracion)]

      # Si no hay columnas disponibles, mostrar advertencia
      if(length(columnas_disponibles) == 0) {
        warning("Ninguna de las columnas especificadas esta disponible en los datos.")
        return(datos_iteracion)
      }

      # Retornamos el dataframe con solo las columnas seleccionadas
      return(datos_iteracion[, columnas_disponibles])
    } else {
      stop("No se encontraron notas en la respuesta de la iteracion.")
    }
  } else {
    stop("Error al realizar la solicitud inicial. Codigo de estado: ", response_iteracion$status_code)
  }
}
