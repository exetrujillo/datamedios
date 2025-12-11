#' Scrapea el cuerpo de las noticias desde una lista de URLs de Ciper
#' 
#' Esta funcion itera sobre un dataframe de noticias, visita la URL de cada una,
#' y extrae el texto principal usando un selector CSS especifico para Ciper.
#' Admite ejecucion en paralelo para acelerar el proceso.
#'
#' @param news_df Un dataframe que debe contener una columna ''url''.
#' @param parallel Booleano que indica si se debe usar procesamiento en paralelo. Por defecto es TRUE.
#' @return El dataframe de entrada con la columna ''contenido_limpio'' rellenada con el texto scrapeado.
#' @keywords internal
#' @import pbapply
scrape_body_ciper <- function(news_df, parallel = TRUE) {
  # --- Validacion de entradas ---
  if (!is.data.frame(news_df) || !"url" %in% names(news_df)) {
    stop("El \'news_df\' debe ser un dataframe con una columna \'url\".")
  }
  if (nrow(news_df) == 0) {
    message("No hay articulos para scrapear.")
    return(news_df)
  }

  # --- Inicializar columna de contenido ---
  if (!"contenido_limpio" %in% names(news_df)) {
    news_df$contenido_limpio <- NA_character_
  }

  # --- Logica de Scrapeo ---
  if (parallel && requireNamespace("parallel", quietly = TRUE) && requireNamespace("pbapply", quietly = TRUE)) {
    # --- EJECUCION EN PARALELO ---
    num_cores <- parallel::detectCores() - 1
    if (num_cores < 1) num_cores <- 1

    num_cores <- min(num_cores, 2)# CRAN exige 2 cores maximo

    cl <- parallel::makeCluster(num_cores)

    # Exportar paquetes y variables necesarios a los workers
    parallel::clusterEvalQ(cl, {
      library(httr)
      library(rvest)
      library(stringr)
    })

    urls_to_process <- news_df$url
    message(paste("Iniciando scrapeo para", length(urls_to_process), "articulos..."))

    # Funcion que se ejecutara en cada worker
    scrape_single_url <- function(url) {
      content_selector <- "div.col-lg-9 p.texto-nota"
      user_agent <- "datamedios R package scraper (https://github.com/exetrujillo/datamedios)"
      max_retries <- 3
      retry_delay <- 5

      for (attempt in 1:max_retries) {
        response <- tryCatch(
          httr::GET(url, httr::user_agent(user_agent), httr::timeout(30)),
          error = function(e) e
        )

        if (!inherits(response, "error") && httr::status_code(response) < 500) {
          break
        }
        Sys.sleep(retry_delay)
      }

      if (inherits(response, "error")) {
        return(paste("REQUEST_ERROR:", response$message))
      }

      if (httr::status_code(response) == 200) {
        page_html <- rvest::read_html(response)
        content_nodes <- rvest::html_nodes(page_html, content_selector)
        if (length(content_nodes) > 0) {
          text_pieces <- rvest::html_text(content_nodes)
          return(stringr::str_squish(paste(text_pieces, collapse = "\n\n")))
        } else {
          return("")
        }
      } else {
        return(paste("HTTP_ERROR:", httr::status_code(response)))
      }
    }

    # Ejecutar en paralelo con barra de progreso
    results_list <- pbapply::pblapply(cl = cl, X = urls_to_process, FUN = scrape_single_url)

    # Detener el cluster
    parallel::stopCluster(cl)

    news_df$contenido_limpio <- unlist(results_list)
    message("Scrapeo en paralelo completado.")


  } else {
    # --- EJECUCION SECUENCIAL (con barra de progreso) ---
    message(paste("Iniciando scrapeo secuencial para", nrow(news_df), "articulos..."))
    pb <- utils::txtProgressBar(min = 0, max = nrow(news_df), style = 3)

    for (i in 1:nrow(news_df)) {
      if (!is.na(news_df$contenido_limpio[i]) && nzchar(news_df$contenido_limpio[i])) {
        utils::setTxtProgressBar(pb, i)
        next
      }

      # La logica de scrape_single_url se repite aqui para el modo secuencial
      current_url <- news_df$url[i]
      content_selector <- "div.col-lg-9 p.texto-nota"
      user_agent <- "datamedios R package scraper (https://github.com/exetrujillo/datamedios)"
      max_retries <- 3
      retry_delay <- 5

      response <- NULL
      for (attempt in 1:max_retries) {
        response <- tryCatch(
          httr::GET(current_url, httr::user_agent(user_agent), httr::timeout(30)),
          error = function(e) e
        )
        if (!inherits(response, "error") && httr::status_code(response) < 500) break
        Sys.sleep(retry_delay)
      }

      extracted_text <- if (inherits(response, "error")) {
        paste("REQUEST_ERROR:", response$message)
      } else if (httr::status_code(response) == 200) {
        page_html <- rvest::read_html(response)
        content_nodes <- rvest::html_nodes(page_html, content_selector)
        if (length(content_nodes) > 0) {
          stringr::str_squish(paste(rvest::html_text(content_nodes), collapse = "\n\n"))
        } else { "" }
      } else { paste("HTTP_ERROR:", httr::status_code(response)) }

      news_df$contenido_limpio[i] <- extracted_text
      utils::setTxtProgressBar(pb, i)
      Sys.sleep(0.3) # Pequeña pausa para no ser agresivo
    }
    close(pb)
    message("\nScrapeo secuencial completado.")
  }

  return(news_df)
}