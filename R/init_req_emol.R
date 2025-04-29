#' Inicializa una solicitud a emol.com y retorna el primer caso de busqueda
#'
#' Esta funcion permite realizar una consulta inicial a emol.com utilizando una frase de busqueda.
#'
#' @param search_query Una frase de busqueda (obligatoria).
#' @param fuentes Un string donde se ponen las fuentes de emol a consultar
#' @return Un dataframe con el primer caso de la busqueda.
#' @export
#' @examples
#' \dontrun{
#' primer_caso <- init_req_emol("Boric", fuentes="emol")
#' }

init_req_emol <- function(search_query, fuentes="emol-todas") {
  # Validamos el parametro
  if (missing(search_query) || !is.character(search_query)) {
    stop("Debe proporcionar una frase de busqueda como texto.")
  }
  # Hay que validar el formato del string fuente

  # Parametros de configuracion y adicionales
  total_articles <- 0  # Basado en `data_initial$hits$total`
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
      message("Error al parsear")
    }
  }


  # URL inicial
  url_initial <- paste0(
    "https://newsapi.ecn.cl/NewsApi/emol/buscador/",
    patronBusqueda,
    "?q=",URLencode(search_query),
    "&size=10&from=0"
  )

  # Solicitud inicial
  response_initial <- httr::GET(url_initial)
  if (response_initial$status_code == 200) {
    data_initial <- httr::content(response_initial, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)

    total_articles = data_initial$hits$total
    message(paste("Total de resultados disponibles en", patronBusqueda, "para la busqueda:", total_articles))

    if (!is.null(data_initial$hits$hits) && length(data_initial$hits$hits) > 0) {
      # Extraemos el primer caso
      # first_case <- as.data.frame(data_initial$hits$hits[1, ])
      # Extraemos máximo los 10 primeros casos
      first_case <- as.data.frame(data_initial$hits$hits)

      # Agregamos la columna "total"
      first_case$total <- total_articles

      # Retornamos el primer caso
      return(first_case)
    } else {
      stop("No se encontraron notas en la respuesta inicial.")
    }
  } else {
    stop("Error al realizar la solicitud inicial. Codigo de estado: ", response_initial$status_code)
  }
}
