#' Funcion para limpiar notas de contenido HTML
#'
#' Esta funcion permite limpiar por completo las notas eliminando codigos y secciones irrelevantes.
#' Verifica que el input sea un data frame con una columna llamada `contenido`.
#' @param datos Data frame donde estan almacenadas las notas
#' @param sinonimos Un vector de character
#' @return Un dataframe con el contenido limpio en la columna contenido_limpio
#' @examples
#'
#' \donttest{
#' datos <- extraer_noticias_max_res("inteligencia artificial", max_results= 150, subir_a_bd = FALSE)
#' datos_proc <- limpieza_notas(datos, sinonimos = c("IA", "AI"))
#' }
#' @export

limpieza_notas <- function(datos, sinonimos = c()) {
  # Validacion inicial
  if (!is.data.frame(datos)) {
    stop("El argumento 'datos' debe ser un data frame.")
  }
  if (!"contenido" %in% colnames(datos)) {
    stop("El data frame debe contener una columna llamada 'contenido'.")
  }
  if (!is.null(sinonimos) && !is.character(sinonimos)) {
    stop("'sinonimos' debe ser un vector de palabras.")
  }

  # Sobrescribir/crear contenido_limpio a partir de contenido
  datos$contenido_limpio <- datos$contenido

  # Iteramos sobre cada fila del data frame
  for (i in seq_len(nrow(datos))) {
    current_content <- datos$contenido_limpio[[i]]

    is_html_like <- FALSE

    if (!is.na(current_content) && is.character(current_content) && nzchar(current_content)) {
      is_html_like <- stringr::str_detect(current_content, "<\\s*(div|p|span|a|body|html|br|img|strong|em|h[1-6])[^>]*>")
    }

    if (is_html_like) {
      parsed_html <- tryCatch({
        rvest::read_html(current_content)
      }, error = function(e) {
        return(NULL)
      })

      if (!is.null(parsed_html)) {
        parsed_html %>%
          rvest::html_nodes("div.lee-tambien-bbcl") %>%
          xml2::xml_remove()
        parsed_html %>%
          rvest::html_nodes("blockquote.instagram-media") %>%
          xml2::xml_remove()
        parsed_html %>%
          rvest::html_nodes("blockquote.twitter-tweet") %>%
          xml2::xml_remove()

        processed_text <- rvest::html_text2(parsed_html)
      } else {
        processed_text <- current_content
      }
    } else {
      processed_text <- current_content
    }

    if (!is.na(processed_text)) {
      processed_text <- stringr::str_replace_all(
        processed_text,
        stringr::regex("Lee tambi\u00e9n.*?(<\\/div>)?", dotall = TRUE, ignore_case = TRUE),
        ""
      )
      datos$contenido_limpio[[i]] <- stringr::str_squish(processed_text)
    } else {
      datos$contenido_limpio[[i]] <- NA
    }
  }

  main_query <- ""
  if ("search_query" %in% colnames(datos) && nrow(datos) > 0 && !is.na(datos$search_query[[1]])) {
    main_query <- datos$search_query[[1]]
  }

  escape_regex <- function(string) {
    if(is.na(string)) return(NA_character_)
    stringr::str_replace_all(string, "([.\\\\+*?\\[\\^\\]$(){}=!<>|:])", "\\\\\\1")
  }

  terms_to_match <- c()
  if (main_query != "" && !is.na(main_query)) {
    terms_to_match <- c(terms_to_match, escape_regex(main_query))
  }
  if (length(sinonimos) > 0) {
    valid_sinonimos <- sinonimos[!is.na(sinonimos)]
    if(length(valid_sinonimos) > 0) {
      terms_to_match <- c(terms_to_match, sapply(valid_sinonimos, escape_regex))
    }
  }

  if (length(terms_to_match) > 0) {
    pattern <- paste0("(?i)\\b(", paste(terms_to_match, collapse = "|"), ")\\b")
    matches_pattern <- stringr::str_detect(datos$contenido_limpio, pattern)
    indices_no_match <- which(matches_pattern == FALSE)


    if(length(indices_no_match) > 0 && length(indices_no_match) < nrow(datos)) {
      datos <- datos[-indices_no_match, , drop = FALSE]
    } else if (length(indices_no_match) == nrow(datos) && nrow(datos) > 0) {
      original_colnames <- colnames(datos)
      datos <- datos[0, , drop = FALSE]
      colnames(datos) <- original_colnames
    }
  }

  if (nrow(datos) == 0) {
    return(datos)
  }

  return(datos)
}
