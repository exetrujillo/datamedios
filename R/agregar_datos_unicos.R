#' Agregar datos únicos a una tabla MySQL
#'
#' Esta función agrega datos a una tabla MySQL utilizando una API que espera datos en formato de lista.
#'
#' @param data Un data frame con los datos a insertar.
#'
#' @examples
#' \dontrun{
#' # Agregar datos únicos
#' agregar_datos_unicos(noticias)
#' }
#'
#' @export
#'
agregar_datos_unicos <- function(data) {
  # Validar entrada
  if (!is.data.frame(data)) {
    stop("El argumento 'data' debe ser un data frame")
  }
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("El data frame no puede estar vacío")
  }

  # Convertir data a lista
  data_list <- as.list(data)

  # URLs de las APIs
  url1 <- "http://librosycodigo.ddns.net:3123/write_news"
  url2 <- "http://librosycodigo.ddns.net:3123/write_search_query"

  # Enviar datos a write_news
  response1 <- httr::POST(url1, body = data_list, encode = "json")
  if (response1$status_code != 200) {
    stop(paste("Error al enviar datos a", url1, "- Código:", response1$status_code))
  }

  # Enviar datos a write_search_query
  response2 <- httr::POST(url2, body = data_list, encode = "json")
  if (response2$status_code != 200) {
    stop(paste("Error al enviar datos a", url2, "- Código:", response2$status_code))
  }

  message("Datos agregados exitosamente a ambas tablas.")
}
