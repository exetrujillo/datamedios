# dev/extraer_noticias_wp.R

# --- Dependencias (las ponemos aquí para acordarnos) ---
# library(httr)
# library(jsonlite)
# library(dplyr)

#' Extrae noticias de un sitio basado en WordPress
#'
#' Esta función se conecta al endpoint de la API REST de WordPress (`/wp-json/wp/v2/posts`)
#' de un sitio web para descargar noticias.
#'
#' @param base_url La URL base del sitio web (ej. "https://www.ciperchile.cl").
#' @param max_results El número máximo de noticias a extraer. Por defecto, 100.
#' @param per_page Cuántos resultados pedir por cada llamada a la API. No exceder 100.
#' @param fields Campos específicos a solicitar de la API. NULL para todos por defecto.
#' @param user_agent Un User-Agent personalizado para ser cortés con el servidor.
#' @return Un dataframe con las noticias extraídas, estandarizado al formato de datamedios.
#' @export
#' @examples
#' \donttest{
#' # Extraer las últimas 10 noticias de CIPER Chile
#' # noticias_ciper <- extraer_noticias_wp("https://www.ciperchile.cl", max_results = 10)
#' }
extraer_noticias_wp <- function(base_url,
                                max_results = 100,
                                per_page = 100,
                                fields = NULL,
                                user_agent = "datamedios R package scraper (https://github.com/exetrujillo/datamedios)",
                                max_retries = 3,
                                retry_delay = 5) {
  # 1. Validaciones y setup
  if (!is.character(base_url) || nchar(base_url) == 0) stop("El argumento 'base_url' debe ser una URL válida como texto.")
  base_url <- sub("/$", "", base_url)
  api_endpoint <- paste0(base_url, "/wp-json/wp/v2/posts")

  # 2. Inicializar variables
  all_posts <- list()
  current_page <- 1
  more_pages_available <- TRUE
  if (per_page > 100) {
    warning("El máximo 'per_page' para la API de WordPress es 100. Se usará 100.")
    per_page <- 100
  }

  message(paste("Iniciando extracción desde:", api_endpoint))

  # 3. Bucle principal para paginación
  while (more_pages_available && length(all_posts) < max_results) {
    query_params <- list(
      per_page = per_page,
      page = current_page,
      `_fields` = if (!is.null(fields)) paste(fields, collapse = ",") else NULL
    )

    cat(sprintf("\rPidiendo página %d... (Total artículos: %d)", current_page, length(all_posts)))


    response <- NULL
    for (attempt in 1:max_retries) {
      tryCatch(
        {
          Sys.sleep(1) # Delay antes de cada intento
          response <- httr::GET(url = api_endpoint, query = query_params, httr::user_agent(user_agent), httr::timeout(60))

          # Si el estado es 200 (éxito) o un error no recuperable (ej. 404), salimos del bucle de reintentos.
          if (httr::status_code(response) %in% c(200, 400, 404)) {
            break
          }

          # Si es un error de servidor (5xx), esperamos y reintentamos.
          if (httr::status_code(response) >= 500 && httr::status_code(response) < 600) {
            cat(sprintf(
              "\n  -> Error de servidor (%d) en intento %d/%d. Reintentando en %d segundos...",
              httr::status_code(response), attempt, max_retries, retry_delay
            ))
            Sys.sleep(retry_delay)
          } else {
            # Otro error inesperado, salimos del bucle de reintentos
            break
          }
        },
        error = function(e) {
          # Error de conexión, no de HTTP
          cat(sprintf(
            "\n  -> Error de conexión en intento %d/%d: %s. Reintentando en %d segundos...",
            attempt, max_retries, e$message, retry_delay
          ))
          Sys.sleep(retry_delay)
        }
      ) # Fin del tryCatch
    } # Fin del bucle for de reintentos

    # Ahora procesamos la respuesta final después de los reintentos
    if (is.null(response) || httr::status_code(response) != 200) {
      if (!is.null(response)) {
        warning(sprintf("\nNo se pudo obtener la página %d después de %d intentos (estado final: %d). Finalizando.", current_page, max_retries, httr::status_code(response)))
      } else {
        warning(sprintf("\nNo se pudo obtener la página %d después de %d intentos (error de conexión). Finalizando.", current_page, max_retries))
      }
      more_pages_available <- FALSE
      next # Salta al siguiente ciclo del while (que terminará la ejecución)
    }

    # Si llegamos aquí, la respuesta es 200 OK
    json_text <- httr::content(response, "text", encoding = "UTF-8")
    posts_data <- jsonlite::fromJSON(json_text, flatten = TRUE)

    if (length(posts_data) == 0 || NROW(posts_data) == 0) {
      more_pages_available <- FALSE
    } else {
      all_posts <- c(all_posts, split(posts_data, seq(nrow(posts_data))))
      if (NROW(posts_data) < per_page) {
        more_pages_available <- FALSE
      } else {
        current_page <- current_page + 1
      }
    }
  }

  cat(sprintf("\nExtracción finalizada. Se obtuvieron %d artículos en total.\n", length(all_posts)))

  # Estandarizar y combinar
  if (length(all_posts) == 0) {
    return(data.frame())
  }

  if (length(all_posts) > max_results) all_posts <- all_posts[1:max_results]

  final_df <- dplyr::bind_rows(all_posts)

  column_map <- c(
    "id" = "ID", "link" = "url", "date" = "fecha", "title.rendered" = "titulo",
    "content.rendered" = "contenido", "excerpt.rendered" = "resumen", "author" = "autor_id"
  )
  existing_cols <- names(column_map)[names(column_map) %in% names(final_df)]

  output_df_base <- data.frame(
    ID = NA, titulo = NA, contenido = NA, contenido_limpio = NA_character_,
    url = NA, autor_id = NA, fecha = NA, resumen = NA,
    search_query = NA_character_, medio = NA, url_imagen = NA_character_,
    stringsAsFactors = FALSE
  )

  renamed_df <- final_df %>%
    dplyr::select(dplyr::all_of(existing_cols)) %>%
    dplyr::rename_with(~ column_map[.], .cols = dplyr::all_of(existing_cols))

  if (nrow(renamed_df) > 0) {
    output_df <- dplyr::bind_rows(output_df_base, renamed_df)
    output_df <- output_df[!is.na(output_df$ID), ]
    output_df$medio <- gsub("^(https://www\\.|https://|http://www\\.|http://)", "", base_url)

    final_cols <- names(output_df_base)
    output_df <- output_df[, final_cols[final_cols %in% names(output_df)]]
  } else {
    output_df <- data.frame(matrix(ncol = length(names(output_df_base)), nrow = 0, dimnames = list(NULL, names(output_df_base))))
  }

  message("Columnas estandarizadas al formato de datamedios.")
  return(output_df)
}
