#' Extraccion de noticias de BioBio.cl por rango de fechas
#'
#' Esta funcion permite realizar una extraccion automatizada de noticias de BioBio.cl utilizando un rango de fechas.
#'
#' @param search_query Una frase de busqueda (obligatoria).
#' @param fecha_inicio Fecha de inicio del rango de busqueda en formato "YYYY-MM-DD" (obligatoria).
#' @param fecha_fin Fecha de fin del rango de busqueda en formato "YYYY-MM-DD" (obligatoria).
#' @return Un dataframe con las noticias extraidas.
#' @examples
#' \dontrun{
#' noticias <- extraer_noticias_fecha_bbcl("inteligencia artificial", "2025-01-01",
#' "2025-02-24")
#' }
#' @export

extraer_noticias_fecha_bbcl <- function(search_query, fecha_inicio, fecha_fin) {

  # Inicializamos variables
  offset <- 0
  total_results <- 0
  all_data <- data.frame(
    ID = character(),
    post_title = character(),
    post_content = character(),
    post_content_clean = character(),
    post_URL = character(),
    post_categories = character(), # este
    post_tags = character(),       # y este los tenemos que unificar posteriormente
    post_image.URL = character(),
    author.display_name = character(),
    raw_post_date = as.Date(character()),
    resumen_de_ia = character(),
    search_query = character(),
    medio = character(),
    stringsAsFactors = FALSE
  )

  # Encabezados para la solicitud
  headers <- c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:131.0) Gecko/20100101 Firefox/131.0",
    `Accept` = "application/json, text/plain, */*",
    `Referer` = paste0("https://www.biobiochile.cl/buscador.shtml?s=", URLencode(search_query)),
    `Content-Type` = "application/json; charset=UTF-8"
  )

  # Obtenemos la respuesta inicial
  respuesta_inicial <- init_req_bbcl(search_query)
  fecha_mas_reciente <- lubridate::ymd_hms(respuesta_inicial$raw_post_date[1])
  total_results <- as.integer(respuesta_inicial$total)
  if(total_results > 0){
    message(paste0("Total de resultados posibles: ", total_results))
    message(paste0("Noticia mas reciente disponible es de la fecha: ", fecha_mas_reciente))
  }else{
    stop("No se encontraron noticias con la search query especificada.")
  }

  ## Bucle para iterar sobre las paginas de resultados
  repeat {
    # URL de solicitud
    url <- paste0(
      "https://www.biobiochile.cl/lista/api/buscador?offset=", offset,
      "&search=", utils::URLencode(search_query),
      "&intervalo=&orden=ultimas"
    )

    # Solicitud a la API
    response <- httr::GET(url, httr::add_headers(.headers = headers))
    if (response$status_code != 200) {
      stop("Error en la solicitud: codigo de estado ", response$status_code)
    }

    # Parseo de la respuesta
    data <- httr::content(response, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)

    # Salimos del bucle si no hay mas datos
    if (is.null(data$notas) || length(data$notas) == 0) break

    # Convertimos las fechas de las noticias
    data$notas$raw_post_date <- as.Date(data$notas$raw_post_date)

    # Filtramos las noticias dentro del rango de fechas
    noticias_filtradas <- data$notas[data$notas$raw_post_date >= as.Date(fecha_inicio) &
                                       data$notas$raw_post_date <= as.Date(fecha_fin), ]

    # Verificamos si hay noticias filtradas
    if (nrow(noticias_filtradas) > 0) {
      # Aseguramos que solo las columnas necesarias esten presentes
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
        message("No hay mas noticias dentro del rango de fechas. Terminando la busqueda.")
        break  # Salimos del bucle si la fecha mas reciente es anterior a fecha_inicio
      }
    }

    # Incrementamos offset
    offset <- offset + 20

    # Salimos si ya hemos procesado todos los resultados disponibles
    if (offset >= total_results) break
  }

  all_data$search_query <- tolower(search_query)

  all_data$post_image.URL <- paste0("https://media.biobiochile.cl/wp-content/uploads/", as.character(all_data$post_image.URL))

  message(paste0("Total de noticias encontradas en el rango de fechas: ", nrow(all_data)))

  # Creamos columna temas y eliminamos las que almacenaban data frames
  # Creamos la nueva columna "temas" como una lista combinada
  all_data$temas <- lapply(seq_len(nrow(all_data)), function(i) {
    # Extraer los slugs de post_categories
    slugs_categorias <- all_data$post_categories[[i]]$slug

    # Extraer los slugs de post_tags
    slugs_tags <- all_data$post_tags[[i]]$slug

    # Combinar ambos en un solo vector
    temas_combinados <- c(slugs_categorias, slugs_tags)

    # Devolver la lista combinada
    temas_combinados
  })

  # Eliminar columnas originales
  all_data$post_categories <- NULL
  all_data$post_tags <- NULL

  # Definir contenido de la columa medio
  all_data$medio <- "bbcl"

  ###############################

  # Redefinir nombres de columnas

  colnames(all_data) <- colnames(all_data) %>%
    dplyr::recode(
      post_title = "titulo",
      post_content = "contenido",
      post_URL = "url",
      `author.display_name` = "autor",
      raw_post_date = "fecha",
      resumen_de_ia = "resumen",
      post_content_clean = "contenido_limpio",
      `post_image.URL` = "url_imagen"
    )

  ###############################


  return(all_data)
}
