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
    # Verificar las columnas de noticias
    missing_columns <- setdiff(colnames(all_data), colnames(noticias))

    # Si faltan columnas, añadirlas con valores NA
    for (col in missing_columns) {
      noticias[[col]] <- NA
    }

    # Asegurarse de que las columnas estén en el orden correcto
    noticias <- noticias[, colnames(all_data), drop = FALSE]
    # Añadir las noticias a all_data
    all_data <- rbind(all_data, noticias)

    # Controlar el número de resultados
    if (nrow(all_data) >= max_results) {
      all_data <- all_data[1:max_results, ]
      break
    }

    offset <- offset + 20
  }

  all_data$raw_post_date <- as.Date(all_data$raw_post_date)
  return(all_data)
}
