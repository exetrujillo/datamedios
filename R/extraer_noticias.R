#' Extracción de noticias desde la API de BíoBío.cl
#'
#' Esta función permite realizar una extracción automatizada de noticias desde la API de BíoBío.cl.
#'
#' @param search_query Una frase de búsqueda (obligatoria).
#' @param max_results Número máximo de resultados a extraer (opcional, por defecto todos).
#' @return Un dataframe con las noticias extraídas.
#' @examples
#' noticias <- extraer_noticias("inteligencia artificial", max_results = 100)
#' @export
extraer_noticias <- function(search_query, max_results = NULL) {
  # Validamos los parámetros
  if (missing(search_query) || !is.character(search_query)) {
    stop("Debe proporcionar una frase de búsqueda como texto.")
  }

  # Inicializamos variables
  offset <- 0
  total_results <- 0
  all_data <- data.frame(
    ID = character(),
    post_title = character(), #titulo de la nota
    post_content = character(), # contenido de la nota
    post_excerpt = character(), # resumen breve de la nota
    post_URL = character(), # url completa de la nota
    post_categories = c(), # categorías asociadas a la nota (lista de objetos con ID, nombre y slug)
    post_tags = c(), # etiquetas asociadas a la nota (lista de objetos con ID, nombre y slug)
    year = integer(), # año de publicación
    month = integer(), # mes de publicación
    day = integer(), # día de publicación
    post_category_primary.name = character(), #categoría primaria, en biobio son bastante arbitrarias
    post_category_secondary.name = character(), #categoría secundaria, en biobio son bastante arbitrarias
    post_image.URL = character(), # URL de la imagen destacada.
    post_image.alt = character(), # descripción alt de la imagen
    post_image.caption = character(), # pie de foto de la imagen.
    author.display_name = character(), # nombre del autor de la nota.
    raw_post_date = as.Date(character()), # fecha raw, en formato ("YYYY-MM-DD")
    resumen_de_ia = character(), # resumen de la nota hecha por ia, si es que aplica
    stringsAsFactors = FALSE
  )

  # Encabezados para la solicitud
  headers <- c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:131.0) Gecko/20100101 Firefox/131.0",
    `Accept` = "application/json, text/plain, */*",
    `Referer` = paste0("https://www.biobiochile.cl/buscador.shtml?s=", URLencode(search_query)),
    `Content-Type` = "application/json; charset=UTF-8"
  )

  # URL inicial
  url_initial <- paste0(
    "https://www.biobiochile.cl/lista/api/buscador?offset=", offset,
    "&search=", URLencode(search_query),
    "&intervalo=&orden=ultimas"
  )

  # Solicitud inicial para obtener el total de resultados
  response_initial <- httr::GET(url_initial, httr::add_headers(.headers = headers))
  if (response_initial$status_code == 200) {
    data_initial <- httr::content(response_initial, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)
    if (!is.null(data_initial$total)) {
      total_results <- as.numeric(data_initial$total)
      print(paste0("Total de resultados posibles:",total_results))
    } else {
      stop("No se encontró el parámetro 'total' en la respuesta.")
    }
  } else {
    stop("Error al realizar la solicitud inicial. Código de estado: ", response_initial$status_code)
  }

  # Limitamos los resultados si max_results está definido
  if (!is.null(max_results)) {
    total_results <- min(total_results, max_results)
  }

  columnas_deseadas <- as.data.frame(all_data, stringsAsFactors = FALSE)

  # Iteración para obtener todos los datos
  while (offset < total_results) {
    url <- paste0(
      "https://www.biobiochile.cl/lista/api/buscador?offset=", offset,
      "&search=", URLencode(search_query),
      "&intervalo=&orden=ultimas"
    )
    offset <- offset + 20

    response <- tryCatch(
      { httr::GET(url, httr::add_headers(.headers = headers)) },
      error = function(e) {
        message("Error en la conexión: ", e)
        return(NULL)
      }
    )
    if (is.null(response)) next

    response_data <- httr::content(response, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)

    if (!is.null(response_data$notas)) {
      notas <- response_data$notas

      # Aseguramos que todas las columnas deseadas estén presentes en notas
      for (col in names(columnas_deseadas)) {
        if (!col %in% names(notas)) {
          notas[[col]] <- NA
        }

        # Forzamos el tipo de la columna según lo especificado en columnas_deseadas
        notas[[col]] <- tryCatch({
          if (class(columnas_deseadas[[col]]) == "Date") {
            as.Date(notas[[col]]) # Conversión específica para fechas
          } else {
            as(notas[[col]], class(columnas_deseadas[[col]]))
          }
        }, error = function(e) {
          warning(paste("No se pudo convertir la columna", col, "al tipo", class(columnas_deseadas[[col]])))
          notas[[col]] # Devuelve la columna original en caso de error
        })
      }

      # Nos aseguramos de que el orden de las columnas sea consistente
      notas <- notas[names(columnas_deseadas)]

      # Combinamos con los datos acumulados
      all_data <- dplyr::bind_rows(all_data, notas)
    }
  }

  # Retornamos los datos procesados
  return(all_data)
}
