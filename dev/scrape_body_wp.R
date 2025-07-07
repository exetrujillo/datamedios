# --- Dependencias ---
# library(rvest)
# library(stringr)
# library(dplyr)

#' Scrapea el cuerpo de las noticias desde una lista de URLs
#'
#' Esta función itera sobre un dataframe de noticias, visita la URL de cada una,
#' y extrae el texto principal usando un selector CSS específico.
#'
#' @param news_df Un dataframe que debe contener una columna 'url'.
#' @param content_selector Un string con el selector CSS que identifica el texto principal de las noticias.
#' @param user_agent Un User-Agent personalizado para ser cortés con el servidor.
#' @return El dataframe de entrada con la columna 'contenido_limpio' rellenada con el texto scrapeado.

scrape_body_from_urls <- function(news_df,
                                  content_selector,
                                  user_agent = "datamedios R package scraper (https://github.com/exetrujillo/datamedios)") {
  # 1. Validación de entradas
  if (!is.data.frame(news_df) || !"url" %in% names(news_df)) {
    stop("El 'news_df' debe ser un dataframe con una columna 'url'.")
  }
  if (!is.character(content_selector) || nchar(content_selector) == 0) {
    stop("Debes proporcionar un 'content_selector' como un string CSS.")
  }

  message(paste("Iniciando scrapeo del cuerpo de", nrow(news_df), "artículos."))

  # Inicializar la columna si no existe
  if (!"contenido_limpio" %in% names(news_df)) {
    news_df$contenido_limpio <- NA_character_
  }

  # 2. Bucle para iterar sobre cada URL
  for (i in 1:nrow(news_df)) {
    current_url <- news_df$url[i]

    # Implementación de lógica de reanudación (si la celda ya tiene contenido, la saltamos)
    # No estoy feliz con esta solución, parece ser lenta
    if (!is.na(news_df$contenido_limpio[i]) && nchar(news_df$contenido_limpio[i]) > 0) {
      cat(sprintf("\rArtículo %d/%d ya procesado. Saltando...", i, nrow(news_df)))
      next
    }

    cat(sprintf("\rProcesando artículo %d/%d...", i, nrow(news_df)))

    # Delay para ser buena onda con el server
    Sys.sleep(1)

    extracted_text <- NA_character_ # Default a NA

    tryCatch(
      {
        page_html <- rvest::read_html(current_url)

        content_nodes <- rvest::html_nodes(page_html, content_selector)

        if (length(content_nodes) > 0) {
          text_pieces <- rvest::html_text(content_nodes)
          combined_text <- paste(text_pieces, collapse = "\n\n")
          extracted_text <- stringr::str_squish(combined_text)
        } else {
          # Se deja una nota si no se encontró el selector de cuerpo
          extracted_text <- "SELECTOR_NOT_FOUND"
        }
      },
      error = function(e) {
        # Se deja una nota si la url falló
        extracted_text <- paste("HTTP_ERROR:", e$message)
      }
    )

    # Asignar el resultado al dataframe
    news_df$contenido_limpio[i] <- extracted_text

    # Guardar progreso cada 50 artículos
    if (i %% 50 == 0) {
      cat(sprintf("\nGuardando progreso en 'ciper_completo_progreso.rds' en la fila %d...", i))
      saveRDS(news_df, "ciper_completo_progreso.rds")
    }
  }

  cat("\n¡Scrapeo del cuerpo de las noticias completado!\n")
  return(news_df)
}
