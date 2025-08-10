#' Extrae noticias de Ciper Chile por rango de fechas
#'
#' Esta funcion se conecta a la API de Ciper para descargar noticias dentro de un rango de fechas.
#'
#' @param search_query El termino de busqueda (opcional).
#' @param fecha_inicio Fecha de inicio del rango de busqueda en formato "YYYY-MM-DD" (obligatoria).
#' @param fecha_fin Fecha de fin del rango de busqueda en formato "YYYY-MM-DD" (obligatoria).
#' @return Un dataframe con las noticias extraidas, estandarizado al formato de datamedios.
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
extraer_noticias_fecha_ciper <- function(search_query = NULL, fecha_inicio, fecha_fin) {
  base_url <- "https://www.ciperchile.cl"
  api_endpoint <- paste0(base_url, "/wp-json/wp/v2/posts")
  user_agent <- "datamedios R package scraper (https://github.com/exetrujillo/datamedios)"

  all_posts <- list()
  current_page <- 1
  total_pages <- Inf
  max_retries <- 3
  retry_delay <- 5

  # Convertir fechas a formato ISO8601
  after_date <- paste0(fecha_inicio, "T00:00:00")
  before_date <- paste0(fecha_fin, "T23:59:59")

  message(paste("Iniciando extraccion desde Ciper para la busqueda:", search_query, "entre", fecha_inicio, "y", fecha_fin))

  pb <- NULL

  while (current_page <= total_pages) {
    query_params <- list(
      search = if (!is.null(search_query) && nzchar(search_query)) search_query else NULL,
      after = after_date,
      before = before_date,
      per_page = 100,
      page = current_page
    )

    response <- NULL
    for (attempt in 1:max_retries) {
      response <- httr::GET(url = api_endpoint, query = query_params, httr::user_agent(user_agent), httr::timeout(60))
      if (httr::status_code(response) < 500) {
        break
      }
      message(paste("\nError 500 en el servidor, reintentando en", retry_delay, "segundos..."))
      Sys.sleep(retry_delay)
    }

    if (httr::status_code(response) != 200) {
      warning(paste("\nError en la solicitud a la API de Ciper. Codigo:", httr::status_code(response)))
      break
    }

    if (current_page == 1) {
      headers <- httr::headers(response)
      total_posts <- as.integer(headers$`x-wp-total`)
      total_pages <- as.integer(headers$`x-wp-totalpages`)
      message(paste("Total de resultados encontrados en Ciper:", total_posts))
    }
    
    if (current_page == 1 && total_pages > 0) {
      pb <- utils::txtProgressBar(min = 0, max = total_pages, style = 3)
    }

    posts_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), flatten = TRUE)

    if (length(posts_data) == 0 || NROW(posts_data) == 0) {
      message("No hay noticias en el rango de fechas seleccionado.")
      break
    }

    all_posts <- c(all_posts, split(posts_data, seq(nrow(posts_data))))

    if (!is.null(pb)) {
        utils::setTxtProgressBar(pb, current_page)
    }

    current_page <- current_page + 1
    Sys.sleep(0.5)
  }
  
  if (!is.null(pb)) close(pb)

  if (length(all_posts) == 0) {
    return(data.frame())
  }

  final_df <- dplyr::bind_rows(all_posts)

  column_map <- c(
    "id" = "ID", "link" = "url", "date" = "fecha", "title.rendered" = "titulo",
    "content.rendered" = "contenido", "excerpt.rendered" = "resumen"
  )
  existing_cols <- names(column_map)[names(column_map) %in% names(final_df)]

  output_df <- final_df %>%
    dplyr::select(dplyr::all_of(existing_cols)) %>%
    dplyr::rename_with(~ column_map[.], .cols = dplyr::all_of(existing_cols))

  output_df$search_query <- if (!is.null(search_query)) tolower(search_query) else NA_character_
  output_df$medio <- "ciper"
  output_df$contenido_limpio <- NA_character_
  output_df$url_imagen <- NA_character_
  output_df$autor <- NA_character_
  output_df$temas <- I(lapply(1:nrow(output_df), function(x) c()))

  output_df <- scrape_body_ciper(output_df)

  output_df$contenido <- output_df$contenido_limpio

  output_df <- output_df %>% 
    dplyr::mutate(ID = paste0(ID, "-c"))

  return(output_df)
}
