#' Inicializa una solicitud a BioBio.cl y retorna el primer caso de busqueda
#'
#' Esta funcion permite realizar una consulta inicial a BioBio.cl utilizando una frase de busqueda.
#'
#' @param search_query Una frase de busqueda (obligatoria).
#' @return Un dataframe con el primer caso de la busqueda.
#' @export
#' @examples
#' \dontrun{
#' primer_caso <- init_req_bbcl("inteligencia artificial")
#' }

init_req_bbcl <- function(search_query) {
  # Validamos el parametro
  if (missing(search_query) || !is.character(search_query)) {
    stop("Debe proporcionar una frase de busqueda como texto.")
  }

  # Encabezados para la solicitud
  headers <- c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:131.0) Gecko/20100101 Firefox/131.0",
    `Accept` = "application/json, text/plain, */*",
    `Referer` = paste0("https://www.biobiochile.cl/buscador.shtml?s=", URLencode(search_query)),
    `Content-Type` = "application/json; charset=UTF-8"
  )

  # URL inicial
  url_initial <- paste0(
    "https://www.biobiochile.cl/lista/api/buscador?offset=0",
    "&search=", URLencode(search_query),
    "&intervalo=&orden=ultimas"
  )

  # Solicitud inicial
  response_initial <- httr::GET(url_initial, httr::add_headers(.headers = headers))
  if (response_initial$status_code == 200) {
    data_initial <- httr::content(response_initial, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)

    if (!is.null(data_initial$notas) && length(data_initial$notas) > 0) {
      # Extraemos el primer caso
      first_case <- as.data.frame(data_initial$notas[1, ])

      # Agregamos la columna "total"
      first_case$total <- data_initial$total

      # Retornamos el primer caso
      return(first_case)
    } else {
      stop("No se encontraron notas en la respuesta inicial.")
    }
  } else {
    stop("Error al realizar la solicitud inicial. Codigo de estado: ", response_initial$status_code)
  }
}
