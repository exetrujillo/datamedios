#' Extracción de noticias desde la API de BíoBío.cl por rango de fechas
#'
#' Esta función permite realizar una extracción automatizada de noticias desde la API de BíoBío.cl utilizando un rango de fechas.
#'
#' @param search_query Una frase de búsqueda (obligatoria).
#' @param fecha_inicio Fecha de inicio del rango de búsqueda en formato "YYYY-MM-DD" (obligatoria).
#' @param fecha_fin Fecha de fin del rango de búsqueda en formato "YYYY-MM-DD" (obligatoria).
#' @return Un dataframe con las noticias extraídas.
#' @examples
#' noticias <- extraer_noticias_fecha("inteligencia artificial", "2023-01-01", "2023-12-31")
#' @export

extraer_noticias_fecha <- function(search_query, fecha_inicio, fecha_fin) {
  # Validamos los parámetros
  if (missing(search_query) || !is.character(search_query)) {
    stop("Debe proporcionar una frase de búsqueda como texto.")
  }
  if (missing(fecha_inicio) || missing(fecha_fin) || !lubridate::is.Date(lubridate::ymd(fecha_inicio)) || !lubridate::is.Date(lubridate::ymd(fecha_fin))) {
    stop("Debe proporcionar fechas de inicio y fin válidas en formato 'YYYY-MM-DD'.")
  }
  if (lubridate::ymd(fecha_inicio) > lubridate::ymd(fecha_fin)) {
    stop("La fecha de inicio debe ser anterior o igual a la fecha de fin.")
  }

  # Inicializamos variables
  offset <- 0
  total_results <- 0
  all_data <- data.frame(
    ID = character(),
    post_title = character(),
    post_content = character(),
    post_excerpt = character(),
    post_URL = character(),
    post_categories = character(),
    post_tags = character(),
    year = integer(),
    month = integer(),
    day = integer(),
    post_category_primary.name = character(),
    post_category_secondary.name = character(),
    post_image.URL = character(),
    post_image.alt = character(),
    post_image.caption = character(),
    author.display_name = character(),
    raw_post_date = as.Date(character()),
    resumen_de_ia = character(),
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
      if (!is.null(data_initial$notas) && length(data_initial$notas) > 0) {
        fecha_mas_reciente <- lubridate::ymd_hms(data_initial$notas$raw_post_date[1])
        print(paste0("Total de resultados posibles: ", total_results))
        print(paste0("Noticia más reciente disponible es de la fecha: ", fecha_mas_reciente))
      } else {
        stop("No se encontraron notas en la respuesta inicial.")
      }
    } else {
      stop("No se encontró el parámetro 'total' en la respuesta.")
    }
  } else {
    stop("Error al realizar la solicitud inicial. Código de estado: ", response_initial$status_code)
  }

  ## Bucle para iterar sobre las páginas de resultados
  repeat {
    # URL de solicitud
    url <- paste0(
      "https://www.biobiochile.cl/lista/api/buscador?offset=", offset,
      "&search=", URLencode(search_query),
      "&intervalo=&orden=ultimas"
    )

    # Solicitud a la API
    response <- httr::GET(url, httr::add_headers(.headers = headers))
    if (response$status_code != 200) {
      stop("Error en la solicitud: código de estado ", response$status_code)
    }

    # Parseo de la respuesta
    data <- httr::content(response, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)

    # Salimos del bucle si no hay más datos
    if (is.null(data$notas) || length(data$notas) == 0) break

    # Convertimos las fechas de las noticias
    data$notas$raw_post_date <- as.Date(data$notas$raw_post_date)

    # Filtramos las noticias dentro del rango de fechas
    noticias_filtradas <- data$notas[data$notas$raw_post_date >= as.Date(fecha_inicio) &
                                       data$notas$raw_post_date <= as.Date(fecha_fin), ]

    # Procesar post_categories y post_tags como JSON
    if (!is.null(noticias_filtradas$post_categories)) {
      noticias_filtradas$post_categories <- sapply(noticias_filtradas$post_categories, jsonlite::toJSON, auto_unbox = TRUE)
    }

    if (!is.null(noticias_filtradas$post_tags)) {
      noticias_filtradas$post_tags <- sapply(noticias_filtradas$post_tags, jsonlite::toJSON, auto_unbox = TRUE)
    }

    # Verificamos si hay noticias filtradas
    if (nrow(noticias_filtradas) > 0) {
      # Aseguramos que solo las columnas necesarias estén presentes
      # Seleccionamos solo las columnas que existen en all_data
      noticias_filtradas <- noticias_filtradas[, intersect(names(noticias_filtradas), names(all_data))]

      # Si hay columnas faltantes en noticias_filtradas, las agregamos como NA
      columnas_faltantes <- setdiff(names(all_data), names(noticias_filtradas))
      if (length(columnas_faltantes) > 0) {
        for (col in columnas_faltantes) {
          noticias_filtradas[[col]] <- NA  # Agregamos columnas faltantes como NA
        }
      }

      # Reordenamos las columnas de noticias_filtradas para que coincidan con el orden de all_data
      noticias_filtradas <- noticias_filtradas[names(all_data)]

      # Agregamos noticias filtradas al dataframe all_data
      all_data <- rbind(all_data, noticias_filtradas)

    } else {
      fecha_reciente <- max(data$notas$raw_post_date)
      if (fecha_reciente < as.Date(fecha_inicio)) {
        print("No hay más noticias dentro del rango de fechas. Terminando la búsqueda.")
        break  # Salimos del bucle si la fecha más reciente es anterior a fecha_inicio
      }
    }

    # Incrementamos offset
    offset <- offset + 20

    # Salimos si ya hemos procesado todos los resultados disponibles
    if (offset >= total_results) break
  }

  print(paste0("Total de noticias encontradas en el rango de fechas: ", nrow(all_data)))

  return(all_data)
}

