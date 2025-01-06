#' Agregar datos unicos a una tabla MySQL
#'
#' Esta funcion agrega datos a una tabla MySQL utilizando una API que espera datos en formato JSON.
#'
#' @param data Un data frame con los datos a insertar.
#'
#' @examples
#' \dontrun{
#' # Agregar datos unicos
#' noticias <- extraer_noticias_max_res("tesla", max_results=10, subir_a_bd = FALSE)
#' agregar_datos_unicos(noticias)
#' }
#'
#' @export
#'
agregar_datos_unicos <- function(data) {
  # Validamos entrada
  if (!is.data.frame(data)) {
    stop("El argumento 'data' debe ser un data frame")
  }
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("El data frame no puede estar vacio")
  }
  if (!"search_query" %in% colnames(data)) {
    stop("El data frame debe contener una columna llamada 'search_query'.")
  }

  # Extraemos y removemos search_query y operamos otras columnas que harían fallar la API
  search_query <- data$search_query[1]
  data$search_query <- NULL
  data$post_content_clean <- NULL # En caso de que el data frame venga con esta columna
  data$parrafos_filtrados <- NULL # O que tenga esta columna

  # Convertimos data a lista
  data_list <- as.list(data)

  # URLs de las APIs
  url1 <- "http://librosycodigo.ddns.net:3123/write_news"
  url2 <- "http://librosycodigo.ddns.net:3123/write_search_query"

  # Funcion para enviar datos a la API
  enviar_datos <- function(url, data) {
    response <- httr::POST(url, body = data, encode = "json")
    if (httr::http_status(response)$category != "Success") {
      stop(paste("Error al enviar datos a", url, "- Codigo:", response$status_code))
    }
    return(response)
  }

  # Enviamos datos a write_news
  enviar_datos(url1, data_list)

  # Enviamos datos a write_search_queries
  data_list <- append(data_list, list("search_query" = search_query))
  enviar_datos(url2, data_list)

  message("Datos agregados exitosamente.")
}
