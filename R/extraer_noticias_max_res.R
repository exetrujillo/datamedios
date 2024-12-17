#' Extracción de noticias desde la API de BíoBío.cl
#'
#' Esta función permite realizar una extracción automatizada de noticias desde la API de BíoBío.cl.
#'
#' @param search_query Una frase de búsqueda (obligatoria).
#' @param max_results Número máximo de resultados a extraer (opcional, por defecto todos).
#' @return Un dataframe con las noticias extraídas.
#' @examples
#' noticias <- extraer_noticias_max_res("inteligencia artificial", max_results = 100)
#' @export
extraer_noticias_max_res <- function(search_query, max_results = NULL) {
  # Validamos los parámetros
  if (missing(search_query) || !is.character(search_query)) {
    stop("Debe proporcionar una frase de búsqueda válida como texto.")
  }
  if (!is.null(max_results) && (!is.numeric(max_results) || max_results <= 0)) {
    stop("max_results debe ser un número entero positivo o NULL.")
  }

  # Inicializamos variables
  encoded_query <- URLencode(search_query)
  all_data <- data.frame()

  # Definir las columnas deseadas
  columnas_deseadas <- c(
    "ID", "post_title", "post_content", "post_excerpt", "post_URL",
    "post_categories", "post_tags", "year", "month", "day",
    "post_category_primary.name", "post_category_secondary.name",
    "post_image.URL", "post_image.alt", "post_image.caption",
    "author.display_name", "raw_post_date", "resumen_de_ia"
  )

  # Obtenemos la respuesta inicial
  respuesta_inicial <- init_req_bbcl(search_query)
  fecha_mas_reciente <- lubridate::ymd_hms(respuesta_inicial$raw_post_date[1])
  total_results <- as.integer(respuesta_inicial$total)
  print(paste0("Total de resultados posibles: ", total_results))
  print(paste0("Noticia más reciente disponible es de la fecha: ", fecha_mas_reciente))

  # Determinamos el número de resultados a extraer
  if (is.null(max_results) || max_results > total_results) {
    max_results <- total_results
  }

  print(paste0("Se extraerán un máximo de ", max_results, " resultados."))

  # Iteramos para obtener todas las noticias necesarias
  offset <- 0
  while (nrow(all_data) < max_results) {
    url <- paste0(
      "https://www.biobiochile.cl/lista/api/buscador?offset=", offset,
      "&search=", encoded_query,
      "&intervalo=&orden=ultimas"
    )

    response <- httr::GET(url, httr::add_headers(.headers = c(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:131.0) Gecko/20100101 Firefox/131.0",
      `Accept` = "application/json, text/plain, */*",
      `Content-Type` = "application/json; charset=UTF-8"
    )))

    if (response$status_code != 200) {
      warning("Error al realizar la solicitud. Código de estado: ", response$status_code)
      break
    }

    data <- httr::content(response, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)

    if (is.null(data$notas) || length(data$notas) == 0) {
      warning("No se encontraron más notas para extraer.")
      break
    }

    noticias <- as.data.frame(data$notas)
    noticias <- noticias[, columnas_deseadas, drop = FALSE]

    # Si alguna columna falta en 'noticias', puedes añadirla con valores NA
    missing_columns <- setdiff(columnas_deseadas, colnames(noticias))
    for (col in missing_columns) {
      noticias[[col]] <- NA
    }

    # Asegurarte de que las columnas estén en el mismo orden
    noticias <- noticias[, columnas_deseadas, drop = FALSE]
    noticias$raw_post_date <- as.Date(noticias$raw_post_date)

    all_data <- rbind(all_data, noticias)

    if (nrow(all_data) >= max_results) {
      all_data <- all_data[1:max_results, ]
      break
    }

    offset <- offset + 20
  }

  return(all_data)
}
