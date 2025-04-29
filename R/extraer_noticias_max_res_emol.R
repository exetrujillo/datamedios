#' Extraccion de noticias de Emol.com
#'
#' Esta funcion permite extraer noticias de las fuentes de Emol, tanto de las
#' noticias no pagas de emol, como de guioteca y los medios regionales de El Mercurio
#'
#' @param search_query Una frase de busqueda (obligatoria).
#' @param max_results Numero maximo de resultados a extraer (opcional, por defecto todos).
#' @param fuente Fuente de emol para iterar (obligatoria).
#' @return Un dataframe con las noticias extraidas.
#' @examples
#' \dontrun{
#' noticias <- extraer_noticias_max_res_emol("inteligencia artificial", "2025-01-01",
#' "2025-02-24", fuente="mediosregionales")
#' }
#' @export
extraer_noticias_max_res_emol <- function(search_query, max_results=NULL, fuente){
  # Validamos los parametros
  if (missing(search_query) || !is.character(search_query)) {
    stop("Debe proporcionar una frase de busqueda valida como texto.")
  }
  if (!is.null(max_results) && (!is.numeric(max_results) || max_results <= 0)) {
    stop("max_results debe ser un numero entero positivo o NULL.")
  }

  # Funcion auxiliar para crear dataframe vacio
  create_empty_df <- function() {
    data.frame(
      ID = character(),
      titulo = character(),
      contenido = character(),
      contenido_limpio = character(),
      url = character(),
      url_imagen = character(),
      autor = character(),
      fecha = character(),
      temas = I(list()),
      resumen = character(),
      search_query = character(),
      medio = character(),
      stringsAsFactors = FALSE
    )
  }

  # Obtener la respuesta inicial
  respuesta_inicial <- tryCatch({
    init_req_emol(search_query, fuentes = fuente)
  }, error = function(e) {
    message("Error al obtener la respuesta inicial: ", e$message)
    return(NULL)
  })

  # Verificar si hay resultados en la respuesta inicial
  if (is.null(respuesta_inicial) || nrow(respuesta_inicial) == 0) {
    message(paste("No se encontraron resultados iniciales para la fuente:", fuente))
    return(create_empty_df())
  }

  # Calcular total de resultados
  total_results <- tryCatch({
    as.numeric(respuesta_inicial$total[1])
  }, error = function(e) {
    message("No se pudo convertir total de resultados: ", e$message)
    NA_real_
  })

  # Verificar total de resultados
  if (is.na(total_results) || total_results <= 0) {
    message("No se pudo determinar el numero total de resultados.")
    return(create_empty_df())
  }

  # Determinar numero maximo de resultados a extraer
  if (is.null(max_results) || max_results > total_results) {
    max_results <- total_results
  }

  # Inicializar variables para el bucle
  all_data <- create_empty_df()
  offset <- 0

  repeat {
    # Obtener los datos de la pagina actual
    datos_pagina <- tryCatch({
      iteracion_emol(search_query, page = offset, fuentes = fuente)
    }, error = function(e) {
      message("Error al obtener la pagina ", offset, " de la fuente ", fuente,": ", e$message)
      return(NULL)
    })

    # Verificar si hay datos
    if (is.null(datos_pagina) || nrow(datos_pagina) == 0) {
      message("No hay mas resultados o se produjo un error. Finalizando extraccion.")
      break
    }

    # Agregar los datos al dataframe final
    all_data <- tryCatch({
      rbind(all_data, datos_pagina)
    }, error = function(e) {
      message("Error al combinar datos: ", e$message)
      all_data
    })

    # Verificar si alcanzamos el numero maximo de resultados
    if (nrow(all_data) >= max_results) {
      all_data <- all_data[1:max_results, ]
      break
    }

    # Incrementar el offset para la siguiente iteracion
    offset <- offset + 1

    # Opcional: agregar un retraso para no sobrecargar la API
    # Sys.sleep(0.5)
  }

  #### PROCESAMIENTO DF FINAL ####

  # Verificar si hay datos para procesar
  if (is.null(all_data) || nrow(all_data) == 0) {
    message(paste("No se encontraron datos para la fuente:", fuente, "en el rango de fechas especificado."))
    return(create_empty_df())
  }

  # Verificar que el dataframe tenga todas las columnas necesarias
  required_columns <- c("_id", "_source.titulo", "_source.texto", "_source.permalink")
  missing_columns <- required_columns[!required_columns %in% names(all_data)]

  if (length(missing_columns) > 0) {
    message(paste("Faltan columnas necesarias en los datos:", paste(missing_columns, collapse=", ")))
    return(create_empty_df())
  }

  # Agregar columnas adicionales de metadatos
  all_data$search_query <- tolower(search_query)
  all_data$medio <- fuente

  # Determinar la columna de fecha a utilizar
  fecha_source <- if (fuente == "guioteca") {
    "_source.fechaModificacion"
  } else if ("_source.fechaPublicacion" %in% names(all_data)) {
    "_source.fechaPublicacion"
  } else {
    "_source.fechaModificacion"
  }

  # SOLO PARA TESTING, COMENTAR EN PRODUCCION
  # processed_data <- all_data

  # Procesar los datos utilizando el enfoque rowwise
  processed_data <- tryCatch({
    all_data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        ID = paste0(`_id`, "-e"),
        titulo = `_source.titulo`,
        contenido = `_source.texto`,
        contenido_limpio = NA,
        url = `_source.permalink`,
        url_imagen = if (fuente %in% c("emol", "mediosregionales")) {
          x <- `_source.tablas.tablaMedios`
          if (is.data.frame(x) && "Url" %in% names(x)) as.character(x[1, "Url"]) else NA
        } else {
          `_source.imagen`
        },
        autor = if (fuente == "guioteca") NA_character_ else `_source.autor`,
        fecha = as.character(as.Date(dplyr::coalesce(!!rlang::sym(fecha_source)))),
        # Extraer el resumen como un valor escalar
        resumen = {
          bajada <- `_source.bajada`
          if (is.null(bajada)) {
            NA_character_
          } else if (is.data.frame(bajada)) {
            as.character(bajada[1,1])
          } else if (is.list(bajada)) {
            if (length(bajada) > 0) as.character(bajada[[1]]) else NA_character_
          } else {
            as.character(bajada)
          }
        }
      ) %>%
      dplyr::mutate(
        temas = list({
          if (fuente == "mediosregionales") {
            # Si la fuente es "mediosregionales", usar seccion y subSeccion
            t <- c(`_source.seccion`, `_source.subSeccion`)
          } else {
            # Caso normal: usar seccion y temas
            t <- `_source.seccion`
            if ("_source.temas" %in% colnames(all_data)) {
              aux <- `_source.temas`
              if (!is.null(aux) && length(aux) > 0 && "nombre" %in% names(aux)) {
                t <- c(t, as.character(aux$nombre))
              }
            }
          }
          t <- tolower(t)
          t <- gsub("\\s+", "-", t)
          t
        })
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(ID, titulo, contenido, contenido_limpio, url, url_imagen, autor, fecha, temas, resumen, search_query, medio)
  }, error = function(e) {
    message("Error al procesar los datos: ", e$message)
    return(create_empty_df())
  })

  return(processed_data)
}
